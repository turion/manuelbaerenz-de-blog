<!---
```haskell
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Version3 where

-- base
import Control.Monad (forM_)

-- wai
import Network.Wai

-- lucid
import Lucid

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-warp
import LiveCoding.Warp

-- manuelbaerenz-de-blog
import Version0 (LiveWebApp, anchors)
import Version2 ()
```
-->

So let's do that now!

```haskell
-- time
import Data.Time

data State = State
  { counter    :: Integer
  , lastAccess :: Maybe UTCTime
  }
  deriving Data

initialState :: State
initialState = State 0 Nothing
```

A value of `Nothing` denotes no visitors so far.

The step function of the web app is now not pure anymore,
since it has access to the current time.
We can still reuse the pure `mutate` function,
but we will also perform an `IO` action to get the current time.

```haskell
counterApp :: LiveWebApp
counterApp = Cell { cellState = initialState, cellStep }
  where
    cellStep counter request = do
      lastAccess <- Just <$> getCurrentTime
      let counter' = mutate request counter
      return (response counter', counter' { lastAccess })
```

<!---
```haskell
mutate :: Request -> State -> State
mutate request State { counter, lastAccess } = State
  { counter = case pathInfo request of
      ["inc"] -> counter + 1
      ["dec"] -> counter - 1
      _       -> counter
  , lastAccess
  }
```
-->

We add one line to the view of the page:

```haskell
content :: State -> Html ()
content State { counter, lastAccess } = do
  h3_ $ "Counter: " <> toHtml (show counter)
  forM_ lastAccess $ p_ . ("Last access: " <>) . toHtml . show
  anchors
```

<!---
```haskell
pageView :: State -> Html ()
pageView counter = doctypehtml_ $ do
  head_ $ do
    title_ "Counter"
    -- Saves us from clicking the reload button on the browser
    meta_ [httpEquiv_ "refresh", content_ "1; url=/"]
  body_ $ content counter

response :: State -> Response
response = responseLBS status200 [(hContentType, "text/html")] . renderBS . pageView
```
-->

If the `lastAccess` field contains a value, then we display it.

_And that's it!_

The server keeps the counter state and adds the last access time
(after we've initialised it, of course).

![Gif]

#### But... how?!

I admit, `essence-of-live-coding` does some magic under the hood.
Once you understand it, it ceases to be magic,
and turns into a neat, practical trick.

The magic lies in the `Data` constraint.
Whenever a type is an instance of `Data`
(and you can derive it automatically),
you gain a dynamic view onto it that reflects the structure of the type.
Given a value of `State` (which _is_ an instance of `Data`, we derived it),
you can inspect what constructor was used,
what fields it has,
what values the fields have,
and so on.
Finally, you can safely cast values of potentially different types.

`essence-of-live-coding` uses this functionality to compare the _old live state_
(such as `State { counter = 23 }`)
with the _new initial state_
(such as `State { counter = 0, lastAccess = Nothing }`).
It ticks of this list:

* Are the constructors the same? _Yes_.
* Is the field `counter` (in the old state)
  found in the constructor of the new state? _Yes_.
* Does the value assigned to `counter` (in this case, `23`)
  typecheck with the field `counter` in the new state? _Yes_.

Then it copies the old value into the new initial state.
There was no data for `lastAccess` that could have been copied,
so it is simply initialised.

The library handles a lot more cases
(for example nested records, or casting into a newtype),
and can be extended with custom state migrations.
For details, have a look at [the article](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCoding.pdf) describing the initial implementation.

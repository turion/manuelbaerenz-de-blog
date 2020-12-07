<!---
```haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Version0 where

-- wai
import Network.Wai

-- lucid
import Lucid
```
-->

Now, we are going to chose particular types for them,
in order to write a web application.
`m` is going to be plain old `IO`.
(There are a lot of other cool choices for `m` that don't involve `IO` at all,
such as `Either`,
but let's save those for a future blog post!)
And in a coarse first approximation,
a web application eats HTTP requests and spits out HTTP responses.
There we go:

```haskell
-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-warp
import LiveCoding.Warp

type LiveWebApp = Cell IO Request Response
```

The types `Request` and `Response` come from the [`wai`](https://hackage.haskell.org/package/wai) package.

"We still haven't seen how to do live coding!",
you might say.
We'll get there soon.
Let's write a web app,
then we'll see how to live code with it.

#### The first web application

Let us start with a counter.
The internal state is an integer,
which should be displayed,
and there should be a link to increment the counter.

At the end it is supposed to look like this:

![]

The view of the page, given the internal state,
can be written using [`lucid`](http://hackage.haskell.org/package/lucid),
a clean and lightweight HTML DSL.

```haskell
type State = Integer

pageView :: State -> Html ()
pageView counter = doctypehtml_ $ do
  head_ $ do
    title_ "Counter"
    -- Saves us from clicking the reload button on the browser
    meta_ [httpEquiv_ "refresh", content_ "1; url=/"]
  body_ $ content counter

content :: State -> Html ()
content counter = do
  h3_ $ "Counter: " <> toHtml (show counter)
  anchors

anchors :: Html ()
anchors = p_ $ a_ [href_ "/inc"] "Increment"
```

We have to parse the request in order to find out whether to increment the counter state.

```haskell
mutate :: Request -> Integer -> Integer
mutate request counter = case pathInfo request of
  ["inc"] -> counter + 1
  _       -> counter
```

Let's stitch everything together:

```haskell
response :: Integer -> Response
response = responseLBS status200 [(hContentType, "text/html")] . renderBS . pageView

counterApp :: LiveWebApp
counterApp = Cell { cellState, cellStep }
  where
    cellState = 0
    cellStep counter request
      = let counter' = mutate request counter
        in return (response counter', counter')
```

The app starts at count 0.
When it receives a request, the counter is updated,
and the new page returned.

#### How to run it all

The library `essence-of-live-coding` contains all the necessary tools to connect such a web application into an executable live program:

```haskell
liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram = liveCell $ runWarpC_ 8080 counterApp
```

The [`HandlingStateT`](https://hackage.haskell.org/package/essence-of-live-coding-0.2.4/docs/LiveCoding-Handle.html#t:HandlingStateT) monad is of no further importance here.
In this case, it simply manages the `warp` backend.

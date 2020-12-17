<!---
```haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Version1 where

-- wai
import Network.Wai

-- lucid
import Lucid

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-warp
import LiveCoding.Warp

-- manuelbaerenz-de-blog
import Version0
```
-->

### Change the program...

Suppose we also want to be able to _decrement_ the counter.
Let's build the UI part:

```haskell
anchors :: Html ()
anchors = do
  p_ $ a_ [href_ "/inc"] "Increment"
  p_ $ a_ [href_ "/dec"] "Decrement"
```

The state change also has to be updated:

```haskell
mutate :: Request -> Integer -> Integer
mutate request counter = case pathInfo request of
  ["inc"] -> counter + 1
  ["dec"] -> counter - 1
  _       -> counter
```

We can stop the currently running server and restart the new version,
which works as intended.
But this is not very satisfying, for two reasons:

* We manually have to restart the server every time we change the code.
* We lose the state of the server each time we restart:
  The counter is back at 0.

Let's solve these two problems one at a time.

First, instead of `cabal repl`, we can use [`ghcid`](https://github.com/ndmitchell/ghcid) to automatically reload.
This is a simple daemon that watches your project and performs customizable commands like `:reload` whenever the files change.
In principle, any daemon (say, [`steeloverseer`](https://hackage-origin.haskell.org/package/steeloverseer),
or even a language server plugin)
could be used, but `ghcid` is particularly easy to set up.

Second, and this is the crucial bit,
`essence-of-live-coding` allows us to keep the state of the webserver while we're changing its implementation!
To leverage this capability, no edits to the code are necessary.
We simply need to configure `ghci` and `ghcid` slightly.
For a project like this, it suffices to copy the template `.ghci` and `.ghcid` files from https://github.com/turion/essence-of-live-coding/tree/master/templates into the project directory.

With both of these technicalities out of the way, let's live code!

![Live coding change GIF]

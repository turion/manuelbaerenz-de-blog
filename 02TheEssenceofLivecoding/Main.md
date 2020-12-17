<!---
```haskell
-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-warp
import LiveCoding.Warp

-- manuelbaerenz-de-blog
import Version0
```
-->


## Live coding a simple web application

In the newt two blog posts,
you will learn how to _live code_ a simple web application.
And by live coding, I don't only mean "it restarts when you change the code".
[That's been done.](https://binarin.ru/post/auto-reload-threepenny-gui/)
I mean "it restarts when you change the code _and keeps its state_ from the previous run".

The motto is:

> __Change the program, keep the state.__

I invite you to take a look at my previous post to learn what exactly I mean by "state".
(Spoiler: It's about Model-View-Controller and state machines.)

### Cells

The web application we will build is a simple counter application.
It is going to accept HTTP requests,
and depending on the request it will increment (or decrement) an internal counter,
and it will respond with a simple website that displays the state of the counter
(and also offers you manipulate the counter again).

To begin, I will drop a not-so-scary concept on you that goes by scary names such as
"Mealy machine with side effects", "synchronous monadic stream function", "resumption", or "transducer".
I call it a `Cell`, because we will be doing live coding,
and cells are the basic building blocks of everything live.

Cells consume input, produce output, and "do" something repeatedly.
They have a notion of internal state,
and it is this state that we want to preserve when the live coding framework reloads.
The definition is quoted directly from the library [`essence-of-live-coding`](https://hackage.haskell.org/package/essence-of-live-coding-0.2.4/docs/LiveCoding-Cell.html#t:Cell):

```{.haskell .ignore}
data Cell m a b = forall s . Data s => Cell
  { cellState :: s
  , cellStep  :: s -> a -> m (b, s)
  }
```

A cell is parametrised by three types:

* A monad `m` which specifies the side effects that the cell can have when we execute it.
  (Here: The ability to serve on a network.)
* A type `a` of inputs. One value of this type is _consumed_ at every step.
  (Here: A HTTP request.)
* A type `b` of outputs. One value of this type is _produced_ at every step.
  (Here: An HTML page.)

Then the cell consists of three pieces of data:

* An internal state type `s` of your choice.
  (Here: An integer for the counter.)
* A current state `cellState` of that type `s`. This will change at every step.
* Finally, a step function `cellStep` which performs side effects in `m` to:
  * consume an `a`,
  * update the state of the program,
  * produce a `b`.

Generalising from our example,
the types `m`, `a`, `b`, and `s` can be anything we want them to be.
Well, nearly.
`m` has to represent side effects, so it should better be a `Monad`, or so.
And you might have noticed the constraint `Data s`.
But more on that later.

<!---
If you're reading this from github, look into the subdirectory.
-->

```{include=TheEssenceofLivecodingandModel-View-Controller/Version0.md}
```

The library gives us a standard main function:

<!---
TODO
Update to next release
```haskell
liveMain = foreground . runIO
```
-->

```haskell
main :: IO ()
main = liveMain liveProgram
```

We can execute this program with `cabal run`,
and it does indeed what we expect:

![Gif showing app]

To see which external libraries, language extensions and imports you need,
have a look [at the source code](https://github.com/turion/manuelbaerenz-de-blog/tree/master/02TheEssenceofLivecoding).

```{include=02TheEssenceofLivecoding/Version0.md}
```

```{include=02TheEssenceofLivecoding/Version1.md}
```

```{include=02TheEssenceofLivecoding/Version2.md}
```

```{include=02TheEssenceofLivecoding/Version3.md}
```

### Outlook

* Livecoding and Functional Reactive Programming
* Client-side with GHCJS
* A deeper look into the entrails of `essence-of-live-coding`
* Control flow state

### To do

* Split into
  1. Webapp introduction
  2. Make changes
* When a piece of code didn't change, briefly repeat what it does
* When a code change comes, say what was before and what is now
* Resolve all TODOs (they are small)
* Make all GIFs

#### Write

* cabal setup
* Mention how it's not so nice that the matching on the urls is with a literal string.
  Improve in future blog post, or maybe right here as a further iteration.
* Insert GIF of first counter
* Insert GIF of counter + vscodium + ghcid
* Possibly show more relevant imports
* This is literate Haskell  you can run it

#### Surrounding

* Doku dort aufbessern wo ich Funktionen benutze
  * LiveCoding.Cell
    * Hide ArrM
    * Hide all the stuff merely for the article
    * Document everything in this module

#### Deploy

* Maybe I can put the different versions in separate files and include them?
  https://github.com/owickstrom/pandoc-include-code
* After release, update haddock link to my lib in this blog post
* When blogs are sequential, link to previous blog
* Make blogs into separate repo
  * Rename blog files and executable names
  * Link to github

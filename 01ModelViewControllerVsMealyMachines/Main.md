<!---
```haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- base
import Control.Concurrent

-- wai
import Network.Wai

-- warp
import Network.Wai.Handler.Warp

-- http-types
import Network.HTTP.Types.Status

-- bytestring
import Data.ByteString.Lazy.Char8 as ByteString

-- gloss
import Graphics.Gloss.Interface.Pure.Game
```
-->

## Live coding a web application, Part 0

The following blog posts will lead you through the development of a simple web application
using the live coding framework [`essence-of-live-coding`](https://github.com/turion/essence-of-live-coding) in Haskell.
This "zeroth" post is mostly about some theoretical background involving state machines,
but no worries, we will get our hands dirty already here!
At the end, you will have a little example web application _and_ a graphics application running.

Every blog post is written in literate Haskell,
and can be compiled and run.
Simply check out [the repository](https://github.com/turion/manuelbaerenz-de-blog/).

## Model-View-Controller vs. transducers & resumptions, or: Moore vs. Mealy

In programming, there is a recurring, vague concept of "internal state", "input" and "output".
It returns in many forms and facets.
While the precise meaning is often unclear,
we can capture what we mean exactly in a strongly typed language like Haskell.
Let's do that, for two manifestations of that concept.

Also, let's write a simple web application,
and a simple graphical application,
using these paradigms.

### Why bother?

Most programs can in principle be described by a state machine.
They consume some kind of input, be it as files, network, standard input, user interaction,
and produce some kind of output.
They keep their internal state in the memory,
and the behavior depends strongly on its state.
For example, a web application accepts a certain kind of requests,
keeps a certain state (the "model") in memory or database,
and delivers a certain kind of responses.

Also, for most programs this is a really impractical description,
and we should be horrified if we were asked to write anything bigger than a simple web application by monolithically stating what its complete input, state, and output is.
(Keep this in mind: I'm going to address the issue of modularity in a later blog post.)
But it is a compellingly simple characterisation that helps us understand how programs work.

And, spoiler alert, it will help us tremendously when we want to do _live coding_.
And we _do_ want to do live coding,
and we will do live coding.
Stay tuned for a later blog post.

### Mealy machines

The type of input values is called `i`,
similarly the output values are of type `o`.
We don't want to fix particular types right now,
and instead define what a Mealy machine is in general,
using [parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism).
That's why `i` and `o` are type variables.
We can then later use these concepts with any concrete type.

We will represent the internal state as a single type as well,
call it `s`.
This state will contain _all_ of the internal details of our program.
That's a lot.
Given a value of `s`, we want to be able to say exactly how our program can behave.

When our program receives input and produces output, its internal state will change.
Most programming languages allow you to manipulate state by offering some kind of mutability.
Not so in Haskell, and some other functional languages.
Instead of mutating a state variable,
we will simply consume the old state and produce the new state as a result.
A state change is done by a function of type `s -> s`.

But we also want to consume inputs of type `i`, and produce outputs of type `o`.
Let's add them then to the type signature by creating tuples:
`(i, s) -> (o, s)`.
And because it is weird to tuple input and state together,
we can [uncurry](https://en.wikipedia.org/wiki/Currying) them,
and arrive at: `i -> s -> (o, s)`.
It's time to wrap this up to a datatype definition.

```haskell
data Mealy s i o = Mealy
  { state :: s
  , step  :: i -> s -> (o, s)
  }
```

Let's unwrap what we've got.
A [Mealy machine](https://en.wikipedia.org/wiki/Mealy_machine) is parametrised by:

* A type of _states_ `s`,
* a type of _inputs_ `i`,
* a type of _outputs_ `o`.

It contains as data a "current" state, simply called `state`.
And when you feed it an input value, you can transform its current state to an output value and the new state.
This is done by the `step` function.

Mealy machines are known since the '50s,
and are ubiquitous in computer science.
The types `s`, `i` and `o` were originally assumed to be finite,
but for our purposes we can drop that condition.
Different names such as "transducers" and "resumptions" have been given to these state machines,
but the basic concept stays the same.

What do we do with a Mealy machine?
We feed it input and get output.
In the process of doing that,
the machine - or rather its internal state - will change.
So we have to return a new machine as well.

```haskell
stepMealy :: Mealy s i o -> i -> (o, Mealy s i o)
stepMealy Mealy { state, step } i =
  let (o, state') = step i state
  in (o, Mealy { state = state', step })
```

Unwrap the machine, apply the step function, wrap it up again.

But we don't have to feed it input value by input value.
We can feed it a whole stream!
Or rather a list.

```haskell
listMealy :: Mealy s i o -> [i] -> [o]
listMealy mealy [] = []
listMealy mealy (i : is) =
  let (o, mealy') = stepMealy mealy i
  in o : listMealy mealy' is
```

An empty list of inputs gives no output.
But assume we have input.
Then for each value `i`,
we do one step,
and then call `listMealy` again until we're done.

### A simple web application

Let us fix a particular input and output now by looking at a popular web application library in Haskell,
the [Web Application Interface `wai`](https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:Application):

```haskell
type WebApp s = Mealy s Request Response
```

`Request` represents HTTP requests,
and `Response` represents, well, HTTP responses.

You could write a simple web application that displays the number of visitors it had so far.
Let's encode that as an `Integer`.

We'll have to throw in a few types and libraries that are largely orthogonal to the concept of state machines,
but they are needed to make a real world application out of all the theory.

```haskell
myApp :: WebApp Integer
myApp = Mealy { state = 0, step }
  where
    step _request nVisitors
      = (mkResponse $ response nVisitors, nVisitors + 1)
    response 0
      = "You're the first :)"
    response nVisitors
      = "There have been " <> ByteString.pack (show nVisitors) <> " visitors!"
    mkResponse
      = responseLBS status200 [("Content-Type", "text/plain")]
```

How do we run it?
In fact, this is really simple using [`warp`](https://hackage.haskell.org/package/warp-3.3.13/docs/Network-Wai-Handler-Warp.html),
and [`MVar`s](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Concurrent-MVar.html):

```haskell
runWebApp :: WebApp s -> IO ()
runWebApp app = do
  var <- newMVar app
  run 8080 $ \request responder -> do
    app <- takeMVar var
    let (response, app') = stepMealy app request
    putMVar var app'
    responder response

mainMealy :: IO ()
mainMealy = runWebApp myApp
```

<!---
```haskell
main = mainMealy
```
-->

Note that by using an `MVar`,
we have basically made the web application single threaded.
It is guaranteed that the response is generated at the internal state directly after the request was processed.

It works just as expected:

```
$ curl localhost:8080
You're the first :)
$ curl localhost:8080
There have been 1 visitors!
$ curl localhost:8080
There have been 2 visitors!
```

## Model-View-Controller

MVC is one of the oldest design patterns for interactive applications,
reaching back to the '70s.
It comes in endless variations, implementations and facets,
and thus there is probably no "true" or "correct" version of it.
Let us nevertheless try and work out a precise notion,
aware that it will differ from many existing implementations.

Similarly to Mealy machines,
an MVC is parametrised by a type of internal state, input, and output, respectively.
They are called "model", "controller", and "view",
but for comparison we will keep calling them `s`, `i`, and `o`.
The basic idea now is that a user can manipulate the model by sending input
(say, clicking a button, filling a form, ...),
and view the updated model (through, say, a website or GUI).

Again, an input value will manipulate the state.
But here is the crucial difference:
Output is derived _only_ from the state.
In particular, output can be inferred without knowledge of the input.
Or, put differently: _Output and input can happen at different times_.

Let's make all these words precise.

```haskell
data Moore s i o = Moore
  { model      :: s
  , manipulate :: i -> s -> s
  , view       :: s -> o
  }
```

The definition is nearly the same as `Mealy`,
but the input and output steps have been separated.

But what is `Moore`, you ask?
"_Who_ is `Moore`?", I retort.
"The inventor of Model-View-Controller?"
In fact not.
We have reproduced a concept known as well since the '50s,
called ["Moore machines"](https://en.wikipedia.org/wiki/Moore_machine),
after their eponymous inventor.

### Two kinds of state machines? That is too complicated!

Let us reduce complexity then,
and embed Moore machines into Mealy machines.
If we have the power of receiving input and emitting output,
we can as well do it at the same time.

```haskell
mooreToMealy
  :: Moore s i o
  -> Mealy s i o
mooreToMealy Moore { model, manipulate, view }
  = Mealy { state = model, step }
  where
    step i s = let s' = manipulate i s in (view s', s')
```

Manipulate the state with the input and view it immediately to get an output value.

Is that it?
Are we done?
Is every Moore machine really just a Mealy machine?
Are Mealy machines more general?

They are the same only when used synchronously,
that is, one output value directly following one input value.
In that case, they have [equal computational power](https://en.wikipedia.org/wiki/Moore_machine#Relationship_with_Mealy_machines).

There is even a sort-of inverse to `mooreToMealy`,
which makes a wonky Moore machine out of a Mealy machine.
Let's think about this for a moment and ponder why this shouldn't work at first glance.
A Mealy machine can inspect the current input when producing an output,
which is something the Moore machine can't do.
With a little crutch, we can work around this, and simply record the last output in the state.

```haskell
mealyToMoore
  :: Mealy     s  i o
  -> Moore (o, s) i o
mealyToMoore Mealy { state, step }
  = Moore { model, manipulate, view }
  where
    model = (error "You have perform one step first!", state)
    manipulate i (_, s) = step i s
    view (o, _) = o
```

The disadvantages are obvious:
We increased the size of the internal state,
and we introduced the possibility of failure through incorrect usage.

But by leaving Mealy machines for Moore machines,
we gain a huge advantage:
_Moore machines can be used asynchronously._

### UIs are asynchronous

...because users don't necessarily consume the view at the same time they manipulate the model.
To illustrate this,
let's shift gears completely and do some graphics programming,
using the [`gloss`](http://hackage.haskell.org/package/gloss) library.
It provides a simple interface to 2d interactive vector graphics.
The main loop can be called with [this mouthful of a function](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/src/Graphics.Gloss.Interface.Pure.Game.html#play) quoted from the library:

```{.haskell .ignore}
-- | Play a game in a window. Like `simulate`, but you manage your own input events.
play    :: Display              -- ^ Display mode.
        -> Color                -- ^ Background color.
        -> Int                  -- ^ Number of simulation steps to take for each second of real time.
        -> world                -- ^ The initial world.
        -> (world -> Picture)   -- ^ A function to convert the world a picture.
        -> (Event -> world -> world)
                -- ^ A function to handle input events.
        -> (Float -> world -> world)
                -- ^ A function to step the world one iteration.
                --   It is passed the period of time (in seconds) needing to be advanced.
        -> IO ()
```

What is all this?
After digesting the whole type signature,
we notice that the parameters can be split up in a few groups.
The first three are just settings that could have been grouped in a triplet:

```haskell
type GlossSettings = (Display, Color, Int)

defaultGlossSettings
  = (InWindow "MealyVsMoore" (600, 800) (20, 20), dark green, 30)
```

And the remaining arguments are just Model-View-Controller,
or in other words, they describe a Moore machine.
`world` is the model, `world -> Picture` is the view,
and the next two functions are controllers which can be grouped together as a single function.
All in all, we can wrap the parameters up as a particular Moore machine:

```haskell
type GlossApp s = Moore s (Either Event Float) Picture

runGlossApp :: GlossSettings -> GlossApp s -> IO ()
runGlossApp (display, color, nSteps) Moore { model, manipulate, view } = play
  display
  color
  nSteps
  model
  view
  (manipulate . Left)
  (manipulate . Right)
```

The point here is that events such as mouse clicks,
and model updates from a physical simulation,
can arrive independently of when the picture is rendered to the screen.
This separates concerns and prevents accidental synchronisation.
The rendering loop can simply render the current world whenever it wants to,
without being blocked on input or a clock.
That's great!

As a simple example, let's make a `GlossApp` that paints a white circle in the position of the last mouse click.

```haskell
myGlossApp :: GlossApp (Maybe (Float, Float))
myGlossApp = Moore { model = Nothing, manipulate, view }
  where
    -- If there was a mouse click, record it
    manipulate (Left (EventKey (MouseButton LeftButton) Down _ pos)) _ = Just pos
    -- Else, don't change the state
    manipulate _ state = state

    -- No mouse click yet, paint a blank picture
    view Nothing = Blank
    -- The last mouse click was at (x, y), paint a circle there
    view (Just (x, y)) = translate x y $ color white $ thickCircle 10 20

mainMoore :: IO ()
mainMoore = runGlossApp defaultGlossSettings myGlossApp
```

It will look something like this:

![myGlossApp](images/myGlossApp.gif)

## Where to go from here?

Extracting the state as a single, explicit value has an immense advantage:
It allows us to do _live coding_ on it.
How that works is the topic of the next few blog posts.
The rough idea is that we've separated the program's _state_ from its _behaviour_,
and when live coding we want to keep the state as good as we can when we change the program.

Having a single state variable also has a huge disadvantage:
The code is not modular at all.
Every detail of the program has to go in this single machine description.
Reusing "one half" of your program elsewhere is not possible,
because there is no natural subdivision of a state machine.
Spoiler alert:
Mealy machines can be composed easily,
while Moore machines require more coordination.
The composition of Mealy machines will lead us naturally to _synchronous Functional Reactive Programming (FRP)_
(as embodied for example by [Yampa](hackage.haskell.org/package/Yampa) and [Dunai](http://hackage.haskell.org/package/dunai)),
while the composition of Moore machines will lead us to _clocked, asynchronous FRP_
(as embodied by [`rhine`](https://github.com/turion/rhine)).

But now I'm getting a handful of blog posts ahead of myself,
and I should return to the immediate goals:
In the next two posts, we will write a more interactive web application,
and I'll show you how to do real live coding!

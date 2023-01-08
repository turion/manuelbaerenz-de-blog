# Introduction

There are plenty of "simple prob. monad from scratch", this is an advanced thing.
Much has been talked about how to represent probabilities,
but it's a different story how to do useful things with them, and the choices of how to represent them have a huge influence on that.

[ ] This blog post follows the version xyz of library foo

# Hierarchy

* Functor = Push distributions forward by function
* Applicatives = Joint distributions
* Monad = Hierarchical models

## Sort these in

* Arrow
* MonadChoice
* MonadZero
* MonadPlus

# Kinds of probability type operators

In principle three kinds of type operators.
Like with embedding any kind of computation in an existing language,
one has the choice of deep embeddings (embed the semantics of the computation you want to represent in the semantics of the target language)
and shallow embeddings (embed the syntax as data and the computation rules as functions).
The advantage of the semantic embedding is usually expressiveness, disadvantages arising when the semantics don't match exactly.
(Haskell does pretty well here because a function is very close to a mathematical function.)
The advantage of shallow embeddings is introspection, which simplifies a lot of computation.
The disadvantage is restriction to the chosen model.

[ ] Check that this is the standard terminology for deep/shallow

On this continuum, there are three main approaches, from deep to shallow:

1. The definition (Kolgomorov)
2. Sampling
3. Synthetic

## Definition

In the original Kolmogorov definition, a probability measure on a type `a` is something that assigns a number to (certain well-behaved) subsets of `a`.
One ought to have a function `evalProb :: m a -> Subset a -> real` which returns the probability of `a` occurring.
Extremely, one could define `type Prob a = Subset a -> real`.

The type of `a` will dictate which `Subset a` are implementable.
One needs a membership function `elem :: Subset a -> a -> Bool` for it to make sense.

If `a` is real, one would use `[Interval a]`, if `a` is discrete, `[a]`.
In general, `Subset a = a -> Bool` is possible, since this is the final encoding of the membership function.
(This idea will return later when we talk about conditioning.
[ ] Make sure it does
)

But it's hard to actually compute with this definition.
1. It puts way to much burden on the implementor.
We suddenly have to invent this `Subset a` type, and if we make a complicated choice like `a -> Bool` we have no idea how to get a good number out of `evalProb`.
2. The encoding is far from being tight: Nothing in the data structures ensures that subsets behave properly, or that probabilities sum to 1 and so on.

### Monad hierarchy

It's not a functor in the obvious sense already because `Subset a` is not always a contravariant functor.
If we choose `Subset a = a -> Bool`, it is, and then `Prob a` is a functor.
But if we take anything computable like `[a]` or `[Interval a]`, we have to sum or integrate the probability over the preimage.
For summing, we'd need `Eq a`, and integrating (or infinite sums) exactly is not computable in general.

## Sampling

[ ] Find in the literature whether this has a name already.

Basically `newtype RandomT m a = RandomT (StateT r m a)` such that `r` models a sample space.
This mimics the definition of a probability distribution as a function from a sample space to an event space.

One expects a function `splitGen :: r -> (r, r)` to exist for `r`, and implements the central interface:

```haskell
gen = randomT $ do
  r <- get
  let (r', r'') = splitGen r
  put r''
  return r'
```

Every time one needs a source of randomness, one uses `gen`.
This way, one can create a stream of `a`s.
So sampling is really easy.

### Choice of `r`

Standard is the interval from 0 to 1.
Novel is Sam Staton et al.'s infinitely branching infinite tree which is useful for lazy calculations.

### How to run sampling

To execute this monad, we
Essential that we have a `random :: m r`.
Given that, we can implement `runRandom :: ReaderT r m a -> m a`.

### Haskell type classes

This automatically a `Functor`: Sample a value and apply a function to it.
It also borrows `Applicative` and `Monad` from `StateT`.

Footnote:

There is a `Monad` shim by using a `splitGen :: r -> (r, r)` in the definition of `(>>=)` by passing the split generators to each argument.
But it doesn't obey the laws.
It's not obvious to observe the broken laws if one interprets all the values as strictly random,
but if one wants to reproduce the randomly generated things, one sees that e.g. `return () >> m /= m`.

### Statistics

The definition of a statistic is a function `m a -> b` where `m` is some randomness monad.
[ ] Is this a good definition?

This is a weakness of the sampling approach.
One cannot compute statistics of a distribution exactly.
One can approximate it by sampling from the stream and computing the statistics over that.
Basically, Monte Carlo.

## Synthetic: Representations of probability distribution families (syntactic embeddings)

One chooses a family of p.d.s and implements their parameters as datatypes, and then tries to implement all the useful capabilities on that family.

### Choices

#### Discrete

The most popular choice is the discrete probability distribution, because it is easy to implement and has the most capabilities.
This is also the choice that most people talk about when they introduce probability monads.

So this is `DiscreteT real m a = m [(real, a)]`.
Note that if you're able to observe the order of the tuples, it's not a monad transformer.
Really, one wants a kind of set instead of a list, but then it's not so obvious how to make it a functor.
[ ] Is there a SetT transformer?
[ ] How about Coyoneda + Set? ProgramT + Set?
Maybe it's nice if one only has `sample :: DiscreteT real m a -> m a` and `runDiscreteT :: Ord a => DiscreteT real m a -> m [(real, a)]` which returns the values ordered by `real` and `a`, or by `a`, made unique.

#### Normal

```haskell
data Normal real a where
  Normal :: real -> real -> real

data Normal a where
  Normal :: Floating a => a -> a -> a
```

This can only sample one distribution with one type.
One can apply linear transformations to the output.

By using Coyoneda or/and free monads, one can recover a monad.

#### Other distributions

They follow a similar implementation pattern like `Normal`, often the kinds of transformations one can apply are even more restrictive.

There is a typical combination of discrete & another distribution, which is called mixture model.

Exponential family is also possible.

### Capabilities

#### Type classes

Discrete is a functor.
(`a` might not be unique on the output, but this is not a problem for sampling.)
Most other distributions can be functors if one can restrict the kinds of functions one can apply, e.g. by the existential type construction.
Exponential family should be fairly general.
[ ] Implement exponential family to substantiate this claim
But unfortunately, the Haskell type class hierarchy is too coarse, and one would need to make it finer to restrict to exactly the allowed kinds of functions (e.g. only linear).

Applicative is already not so clear.
Discrete is Applicative.
One needs to extend most other families with a construction like this:

```haskell
data Normals a where
  Normals :: Vec n (Normal a) -> (Vec n a -> b) -> b
```
Or simply:
```haskell
data Normals a where
  Normals :: Vec n (Normal a) -> Vec n a
```
And add the Coyoneda separately.
Note that this is not a mixture model because there is not a choice between the different normals,
instead they are all sampled.

[ ] Monad

#### Statistics

Statistics are easy to implement & O(1) to run if the distribution is known.
This is the strength of this approach.

Caveat, if you make the list too long for discrete or mixture models, you suffer from linear complexity in the length of the list.

# Capabilities

## Monad hierarchy

## Sampling

### Recovering/evaluating probability

How well the probability density (original definition of probability) is computable from the sampling or the synthetic viewpoint depends on the choice.
For explicit distributions, this can be `O(1)` for a singular set.
But when applying Coyoneda or free monads, you already lose this property.
For discrete distributions or mixture models, require `Ord a` to upgrade to `O(log(n))` in the length of the list, or `O(n)` otherwise.
In general, if one can sample from `a`, you can do Monte Carlo, and use the membership function, but this is only an approximation.

## Conditioning

Conditioning comes in different flavours.
The most flexible somewhat harder to implement is `score :: real -> m ()`.
Then there is also `mzero :: m a`, which is like `score 0`.
The point of these is mainly when one has `Alternative` or `MonadPlus`,
because then one has `score x <|> score y = score (x + y)`.

One uses these to implement part of Bayes' rule.
One computes a likelihood and gives it to `score`.
Summing over all possible result values implements Bayesian inference.
This can be done either because one uses `Alternative` (or `Selective`) to branch,
or `Monad`.

But this breaks the abstraction of the probability monad.
It only works if one has a probability evaluation function,
and there is no straightforward way in general to go from the probability type to that function.
So, one often has to do this separately of the other capabilities.

For discrete distributions, this is very easy to implement.
Likewise for mixture models.
For semantic embeddings and explicit distributions, not so much.

A further issue here is that the total probability mass changes.
So to compute a probability, one needs to normalize, which is often expensive.
[ ] In what cases is it not a problem? Sampling?

## Inference

## Marginalisation

Random _monads_ are actually hierarchical models + marginalisation.
Both are in one go.

So one has: `(p(theta), p(x | theta)) |-> p(x)`

One can recover purely hierarchical models by saying `x = (x', theta)`,
thus forwarding the parameters as well.

In principle, `Functor` already gives marginalisation in the special case of `fmap fst`.

## Bayes theorem

To do inference, one generally wants to implement Bayes' rule somehow.
Conditioning offers a simple way to do this, but it's actually too powerful.
Bayes' rule in a type signature is in general:

```haskell
bayes :: Eq b => m a -> (a -> m b) -> b -> m a
```
But this is often not straightforward to implement,
chiefly because the function in the second argument is not introspectable,
so one cannot use it to create a new distribution.
All that one can do with this function is evaluate it.
[ ] Does https://acatalepsie.fr/posts/overloading-lambda or https://hackage.haskell.org/package/overloaded-0.3.1/docs/Overloaded-Categories.html help?
So a typical implementation in terms of `Alternative` (which in turn may be implemented in terms of `score`) may look like:

```haskell
bayes ma f b = do
  a <- ma
  b' <- f a
  guard $ b == b'
  return a
```
But this is not a feasible computation in most cases.
For discrete distributions, this is fine.
The semantic thingies don't have `guard` though.
Also, continuous distributions don't work here because they can't deal with `guard` in a sensible way,
because the probability for a single value is 0.

A workaround is importance sampling, where we have a function `softEq :: b -> b -> real`,
and we do:

```haskell
bayes ma f b = do
  a <- ma
  b' <- f a
  score $ b `softEq` b'
  return a
```
But now the inference is not exact anymore.

A different general implementation that may be exact in terms of `score` and `evalProb` may be:

```haskell
bayes ma f b = do
  a <- ma
  score $ evalProb (f a) $ singleton b
  return a
```

This can often not be implemented because `evalProb` is hard to implement.

All the above approaches have the issue related to normalization.
Even if one only samples from the posterior, one must take care that the total probability mass has not decreased too much.
This is problematic when repeating the bayes step many times, then the mass decreases exponentially.

## Conjugate probabilities

For some specific distributions, exact Bayesian inference is known.
But if the likelihood is expressed as a function `a -> m b`,
one doesn't know what conditional distribution this is.

[ ] Maybe it is possible to understand this better with a HOAS approach like https://acatalepsie.fr/posts/overloading-lambda
[ ] Or with operational because then we can control the type of `a`?

So in a sense, the embedding is too deep for this kind of reflection!

This gets easier when we have more shallow embedding.
One can define some types of arrows:

```haskell
data Normal a b where
  Normal :: StdDev -> Normal real real
```
Since the normal distribution is its own conjugate, one can define:

[ ] This doesn't make sense, there is no input of type ()
```haskell
bayes :: Normal () a -> Normal a b -> Normal b a
```
[ ] Calculate this and make sure it makes sense

[ ] Or is it:
```haskell
bayes :: Normal () a -> Normal a b -> b -> Normal () a
```

[ ] Is this still useful after defining the free arrow over this?

More generally, if one has conjugate families, one might define:

```haskell
class Conjugate p l where
  bayes :: p () a -> l a b -> p b a
```

Even more generally, one would be interested in conditionally conjugate families:

```haskell
class Conjugate p l where
  bayes :: p x a -> l a b -> p (x, b) a
```

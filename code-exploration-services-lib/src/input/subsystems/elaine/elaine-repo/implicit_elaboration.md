# Implicit elaboration resolution

- Elaborations have a nice quality: they all elaborate into Free monads and
  hence they can be composed easily.
- The order of elaborations does not matter.
- Hence we can do a combined elaboration consisting of elaborations `e_1`, ...,
  `e_n` as `elab (e_1 + ... + e_n)`.
- However, this still means that you usually write the same set of elaborations
  over and over.
- So what if we could run all the correct elaborations implicitly based on the
  elaborations we have in scope?
- In that case we need a module system to distinguish in and out of scope items.

```haskell
module effects where
    heffect Catch
    | catch: A !! Catch -> A !! Catch -> A !! Catch

    effect Throw
    | throw: A -> A ! Throw

module impl where
    import effects

    elaborate eCatch: a !! Catch -> a ! Throw,f
        ...
    
    handler hThrow: a ! Throw -> Maybe a
        ...

module main where
    import effects
    import impl

    -- If catch appears in the computation, elab knows to use eCatch from impl
    -- to elaborate.
    -- It also knows that the resulting effect row requires the throw effect.
    hThrow (elab (....))
```

- This is the easy case, but it quickly gets more complicated.
- For example, what if there are multiple elaborations for a single heffect in
  scope? And what if they elaborate to separate effects?
- Also, how do we make this check as local as possible, so that we can type
  check each module separately (if at all possible).
- What if the effects we elaborate into are not in scope?
- Can we do the entire analysis of this statically, so that we can
  "monomorphize" before the program gets run.
- Interesting side effect of this system: higher order effects and their
  elaborations are entirely defined at compile-time. Does this enable more
  optimization? I.e. can we elaborate everything before running the program. I
  think so.
- There are also questions about how the module system works in general.
- The resolution system has a lot of similarities with typeclasses. In
  particular, one could imagine the following typeclass using the Haskell
  library:

```haskell
class Elab (h :: HFunctor) (f :: Functor)
    elab :: h (Free f) -> Free f

instance (Throw < f) => Elab Catch f
    elab (Catch m1 m2 k) = hup (handle hThrow) m1 >>= (maybe (m2 >>= k) k)
```

- What's interesting in particular is how to simplify this whole thing to do
  just what we need, but not be more general and even give us some nice things.

## Questions

- How should the lambda resolve?
- Double effects with different type parameters. Should they be allowed? How
  should they resolve?
- Can higher order effects be in scope multiple times with different parameters.
- Can higher order effects appear multiple times in an effect row?
  - With the same parameters it is useless, because they both elaborate to the
    same thing.
- Three types of "sameness":
  - Different effects: e.g. Except & Lambda are different
  - Same effects: e.g. Except & Except are the same
  - Different parameters: Lambda CBV & Lambda CBN
- Different instances might need different elaborations. TODO: create examples
  - Reader with ask and local parametrized with different types
- What is the elaboration type? Either an actual type or only a constraint
- Resolve elaborations at definition site or call site (should probably be
  definition), should there be an option to defer to call site?

## Examples of monads and effects

TODO Study

- <https://wiki.haskell.org/All_About_Monads#A_Catalog_of_Standard_Monads>
- <https://en.wikipedia.org/wiki/Monad_(functional_programming)#More_examples>
- <https://hackage.haskell.org/package/mtl>

### Abort

```hs
data Maybe a
  = Just a
  | Nothing

effect Abort
  ctl abort() : a
```

### State

A standard state effect

```hs
effect State<a>
  ctl get() : a
  ctl put(v: a) : ()
```

### LocalState

A state that resets the changes after the computation. It's a sort of
combination of the State and Reader effects, but it can elaborate into State.

> This is not defined anywhere else

```hs
heffect LocalState<a>
  ctl get() : a
  ctl put(v: a) : a
  ctl local(c: e b) : b
```

### Reader

A reader effect with a local operation, which makes it higher-order.

> Question: Should `f` be required to be pure?

```
heffect Reader<a>
  ctl ask() : a
  ctl local(f: a -> a, c: e b) : b
```

### Writer

Just log values away and don't do much with them.

```
heffect Writer<a>
  ctl tell(v: a) : ()
```

### Continuation

Async bayyybe. Actually I don't know how to convert this

Let's mark it TODO.

## TODO: Make examples where elaboration succeeds and fails

- Also focus on edge cases

## Next week

- Begin language spec
- Begin syntax & semantics definitions
- Create first version of typing judgments (for elaborations don't get hung up
  in other details)
- Make examples

## Type inference

- Long term: start with explicit types and effects
- Hindley-Milner style inference
- Infer effects from operations
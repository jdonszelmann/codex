Ok, so the design has changed significantly, which changes the elaboration resolution. The problem is that now elaborations are runtime values, so how do you figure out which one to use. 

Option 1 is to do the resolution entirely at runtime, meaning that whenever we find an `elab`, we check the context for bindings to elaborations and apply those. That should work.

Option 2 is to have a single elaborations context to which all elaboration bindings are automatically added, which is applied when an `elab` is encountered. That's kinda nice.

Option 3 is to do a type-directed approach, where the we do option 1, but at compile-time. That is, we insert the right elaborations statically based on the type information. The advantage is that we can provide static information on which elaborations are used. The downside is that we need to have full typing information, which means a full type checker. Still I like this option best.

If we have a type checker, this last option is easy. When we encounter an `elab`, we check the typing context for elaborations and type check the sub-expression. We then match those up to get some list of bindings `e1`, ..., `eN`. Assume we have some built-in function `combine`, which combines 2 elaborations, we then rewrite `elab` to `elab[combine(e1, combine(e2, ...))]`. This is equivalent to `elab[e1] elab[e2] ...`

I've thought about how we could do the same thing for handlers, but that's not desireable because handlers might have different behaviour based on the order in which they are applied. Elaborations don't have this problem, so we can use an implicit order.

I might want to support nested modules and have a default root modules. That would be nice for many examples in the thesis too. I could even have some facsimile of Rust's `::` syntax.


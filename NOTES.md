# nil and false handling

One benefit of the `Choice` wrapper is that we can choose `nil` or `false`
values explicitly without having to worry about how `when` and `if` responds.

BUT! we have another issue, encoded in a test; when `false` or `nil` values come
into a primitive distribution, how do we disambiguate a lack of constraints from
an explicit `nil` update?

One idea is to guard against passing in `nil` when you don't mean it! I think
that's too easy to mess up.

Another is to add an explicit `(Nil.)` wrapper that means "update this primitive
to `nil`"...

BUT I guess if we have `Choice` wrappers then the "update"gets passed a
constraint. Okay...

But then what do we do if it does NOT get passed a constraint? wrap it
internally?

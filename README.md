# [One argument](https://github.com/tfeb/oa)

An attempt at a language for Racket which supports one-argument λ and
function application with only one argument, for playing with trivial
λ-calculus things.

This is very much prototype code: it's intended entirely as something
to play with λ-calculus and emphatically not for any serious use.
Signifiantly I am not sure if the normal-order languages are
semantically correct: they certainly were not and may still not be.

`raco pkg install` will install it.  There are some examples in
`examples`.

## The languages
There are four languages, all available via `#lang`.

### `oa/normal/pure` (*aka* `oa/normal`)
This is a normal-order language in which λ takes just one argument,
which is not in parenthesis.  So the identity function is `(λ x x)`,
for instance.

### `oa/normal/fancy`
This is a normal-order language which supports 'fancy' λ:

- `(λ (x) ...)` is `(λ x ...)`;
- `(λ (x y) ...)` is `(λ x (λ y ...)` and so on.

In addition function application is fancified: `(f x y)` is `((f x)
y)` and so on.

### `oa/applicative/pure` (*aka* `oa/applicative`)
This is an applicative-order language with the same syntax as
`oa/normal/pure`.

### `oa/applicative/fancy`
This is an applicative-order language with the same syntax as
`oa/normal/fancy`.

## Additional syntax
All the languages support a simple version of `define`: `(define
identity (λ x x))` defines `identity` to have the value `(λ x x)` for
instance.  There is no fancy syntax for `define`.

All the languages support a tiny subset of `rackunit`:

- `test-case` is `rackunit`'s `test-case`; `check-equiv?` and
- `check-not-equiv?` are like `check-eqv?` and `check-not-eqv?` but
   they will suitably force evaluation of anything not evaluated yet.

Additionally there are no changes to the reader, so it will happily
read numbers and so on.  The only *operation* is λ however.

## Hacks
The printer is modified to try to print things which have been given
names with `define` in a useful way: it prints their names and the
canonical source code if they are functions.  This is a bit ad-hoc.
As an example of this:

In `oa/normal/pure`:

```
> (λ x x)
#<λ>
> (define identity (λ x x))
> identity
{identity}: (λ x x)
> (define another identity)
> another
{another identity}: (λ x x)
> identity
{another identity}: (λ x x)
```

In `oa/normal/fancy`:

```
> (λ (x y) x)
#<λ>
> (define true (λ (x y) x))
> true
{true}: (λ x (λ y x))
```

There are two possibly-useful environment variables:

- `OA_HOLD_DEBUGGING`, if defined, will cause it to tell you when
  forms are being 'held' (equivalent to `delay`) and 'released'
  (equivalent to `force`).
- `OA_NO_STASH_PRINTING`, if defined, will turn off all the attempted
  printing cleverness.

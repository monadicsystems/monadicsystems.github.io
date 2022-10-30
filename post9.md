The Okapi DSL has some interesting algebraic properties.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PseudoAlgebra where

import Control.Applicative
import Okapi

foo = do
  methodGET
  pathPart "foo"
  pathEnd
  write @Text "foo"

bar = do
  methodGET
  pathPart "bar"
  pathEnd
  write @Text "bar"

baz = do
  methodGET
  pathPart "baz"
  pathEnd
  write @Text "baz"

-- x === x + 0 === 0 + x

ex1 = foo

ex2 = foo <|> next

ex3 = next <|> foo

foo' = do
  pathPart "foo"
  pathEnd
  write @Text "foo"

bar' = do
  pathPart "bar"
  pathEnd
  write @Text "bar"

baz' = do
  pathPart "baz"
  pathEnd
  write @Text "baz"

-- a * x + a * y + a * z === a * (x + y + z) === (x + y + z) * a

ex4 = foo <|> bar <|> baz

ex5 = do
  methodGET
  foo' <|> bar' <|> baz'

ex6 = do
  foo' <|> bar' <|> baz'
  methodGET

-- 0 === a * 0 === 0 * a

ex6 = next

ex7 = do
  foo
  next

ex8 = do
  next
  foo
  
-- 1 === a * 1 === 1 * a

ex9 = pure ()

ex10 = do
  foo
  pure ()

ex11 = do
  pure ()
  foo
```

> **Warning** These properties hold assuming you're using the pure variant of `ServerT`.
> Putting `IO` in the stack, for example, will make these properties untrue.

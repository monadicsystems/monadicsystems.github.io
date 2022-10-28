---
title: Web Framework Mimicry
image: /logo-bg.png
summary: How to mimic other web frameworks with Okapi
---

# Web Framework Mimicry

> mimicry - the close **external resemblance** of an animal or plant (or part of one) to another animal, plant, or inanimate object.

In this post I want to explore how we can use the Okapi to "mimic" other web frameworks.

## Updates

Okapi has changed a bit since my last post.
If you've been reading my previous blog posts, this is for you.
If this is your first time reading about Okapi, now is a great time to jump in.

Okapi is a monadic DSL for decribing web servers. Okapi exports a variety of simple *HTTP request parsers* that can be composed using `do` notation and other operators to create more complicated parsers.

Here's an example web server from the official [Okapi documentation](https://www.okapi.wiki/).

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Combinators
import Data.Text

main :: IO ()
main = run id do
  methodGET -- (1)        
  pathSeg "greet" -- (2)
  maybeName <- optional $ pathParam <|> queryParam "name" -- (3)
  pathEnd -- (4)
  let greeting :: Text = case maybeName of -- (5)
        Nothing   -> "Hello there."
        Just name -> "Hello, " <> name <> "."
  write greeting -- (6)
```

1. Checks that the request's method is `GET`
2. Checks that the first path segment of the URL is equal to `greet`
3. Binds a path parameter or a query parameter to `maybeName`, if it exists. Otherwise, `Nothing` is bound to `maybeName`. This line contains two useful            functions:
     a. The `optional` parser combinator from the `parser-combinators` library. The parser it is applied to will return `Nothing` instead of failing if the             parser fails
     b. The `<|>` operator from the `Alternative` typeclass. If the parser on the left side of the operator fails, the parser on the right side of the operator         is tried
5. A let expression that assigns the correct value to `greeting` based on the value of `maybeName`
6. Write `greeting` to the reponse body


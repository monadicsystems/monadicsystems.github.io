---
title: Web Framework Mimicry
image: /logo-bg.png
summary: How to mimic other web frameworks with Okapi
---

# Web Framework Mimicry

> mimicry - the close **external resemblance** of an animal or plant (**or part of one**) to another animal, plant, or inanimate object.

In this post I want to explore how we can use the Okapi to "mimic" other web frameworks.

## What's Okapi?

> **Note** You can skip this section if you're already familiar with Okapi.

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
  pathPart "greet" -- (2)
  maybeName <- optional $ pathParam <|> queryParam "name" -- (3)
  pathEnd -- (4)
  let greeting :: Text = case maybeName of -- (5)
        Nothing   -> "Hello there."
        Just name -> "Hello, " <> name <> "."
  write greeting -- (6)
```

1. Checks that the request's method is `GET`
2. Checks that the first part of the URL path is equal to `greet`
3. Binds a path parameter or a query parameter to `maybeName`, if it exists. Otherwise, `Nothing` is bound to `maybeName`. This line contains two useful            functions:
     * The `optional` parser combinator from the `parser-combinators` library. The parser it is applied to will return `Nothing` instead of failing if the              parser fails
     * The `<|>` operator from the `Alternative` typeclass. If the parser on the left side of the operator fails, the parser on the right side of the operator          is tried
4. Checks that the request has no more remaining path segments
5. A let expression that assigns the correct value to `greeting` based on the value of `maybeName`
6. Write `greeting` to the response body


As you can see, a lot can be accomplished with primitive functions like `pathParam`, `methodGET`, `queryParam`, etc.
Since Okapi is a monadic DSL, it is intuitive to use, easy to compose, and even has some basic algebraic properties!
Due to its' simplicity, I consider Okapi to be "low-level" compared to other web frameworks. What do I mean by this?
Well, Okapi is just a thin abstraction built on top of WAI. It lacks a lot of the built-in features that other frameworks have right out of the box.
The core of Okapi only provides the types and parsers need to manipulate HTTP requests and responses.

## Mimicking Sinatra-like Frameworks

This refers to frameworks like Scotty, Spock, Flask, etc.

## Mimicking Yesod

I need more type safety

## Mimicking Giraffe

## Mimicking Servant

Can Okapi mimic servant? No. Let's just integrate!

## Conclusion

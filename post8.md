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
4. Checks that the request has no more remaining path parts
5. A let expression that assigns the correct value to `greeting` based on the value of `maybeName`
6. Write `greeting` to the response body

Here's another example of simple server that returns `ping` on a `GET` request to `/pong`, and `pong` on `GET` request to `ping`. 

```haskell
main = run id $ ping <|> pong

ping = do
  methodGET
  pathPart "ping"
  pathEnd
  write @Text "pong"
  
pong = do
  methodGET
  pathPart "pong"
  pathEnd
  write @Text "ping"
```

We can use a rule similar to the distributive property of multiplication to simplify our ping-pong server.

```haskell
main = run id do
  methodGET
  ping <|> pong
  
ping = do
  pathPart "ping"
  pathEnd
  write @Text "pong"
  
pong = do
  pathPart "pong"
  pathEnd
  write @Text "ping"
```

As you can see, a lot can be accomplished with primitive functions like `pathParam`, `methodGET`, `queryParam`, etc.
Since Okapi is a monadic DSL, it is intuitive to use, easy to compose, and we can even apply our knowledge of algebraic properties when using it.
Due to its' simplicity, I consider Okapi to be lower level compared to other web frameworks. What do I mean by this?
Well, Okapi is just a thin abstraction built on top of WAI. It lacks a lot of the built-in features that other frameworks have right out of the box.
The core of Okapi only provides the types and parsers need to manipulate HTTP requests and responses. This gives us a lot of flexibility to build on top of it.

## Mimicking Method-Path-Handler Style Frameworks

This refers to frameworks like Scotty, Spock, Laravel, Sinatra, Express, etc. where you declare your server's endpoints by specifying a request method, a path pattern, and then a handler function. I'm just going to call this convention *Method-Path-Handler style*.

Let's see what a server that greets the user looks like in these Method-Path-Handler style frameworks.

> **Note**
> I'm excluding imports and the code needed to actually execute the servers.
> I just want to focus on the endpoint definitions.

First, Ruby's popular micro web framework, Sinatra.

```ruby
get '/greeting/:name' do
  "Hello #{params['name']}"
end
```

This is Scotty, a Haskell framework inspired by Sinatra.

```haskell
greeting = get "/greeting/:name" do
  name <- param "name"
  text $ "Hello " <> name
```

Here's another Haskell web framework inspired by Sinatra, Spock.

```haskell
greeting = get ("greeting" <//> ":name") do
  name <- param' "name"
  text $ "Hello " <> name
```

Here's what it looks like in the Node.js framework, Express.

```js
app.get('/greeting/:name', (req, res) => {
  res.send(`Hello ${req.params.name}`)
})
```

Finally, Laravel, a web framework for PHP.

```php
Route::get('/greeting/{name}', function ($name) {
    return 'Hello '.$name;
});
```

We can see a pattern here. These Method-Path-Handler style frameworks share these 3 traits.

1. The use of higher-order functions, such as `get`, `post`, etc. that represent the HTTP method that the endpoint accepts.
2. The higher-order functions representing HTTP methods take a pattern as the first argument, usually represented as a string. This pattern represents
   the URL path that the endpoint will respond to. Path parameters are also defined in this pattern.
3. The higher-order functions take a function representing the handler as the second argument. This handler is executed if the request uses the correct
   method and matches the URL path pattern.

Let's try and *mimic* this style of defining endpoints using Okapi. First, let's define the endpoint we saw in the above examples with Okapi so we can compare it to the Method-Path-Handler style.

```haskell
greeting = do
  methodGET
  pathPart "greeting"
  name <- pathParam
  pathEnd
  write @Text $ "Hello " <> name
```

This is the most conventional way to implement the endpoint in Okapi. Let's work our way to Method-Path-Handler style from here.

First, let's define those higher-order functions that represent the HTTP method that the endpoint accepts.

```haskell
get :: MonadServer m => m a -> (a -> m ()) -> m ()
get p handler = methodGET >> (p >>= handler)
```

With this function we could now define the greeting endpoint like so.

```haskell
greeting = get p $ \name -> write @Text $ "Hello " <> name
  where
    p = do
      pathPart "greeting"
      name <- pathParam
      pathEnd
      pure name
```

This is good, but we can do better. The the first function that we pass into `get`, which represents the pattern for the URL path, can modify the response body if it wants.

```haskell
greeting = get p $ \name -> write @Text $ "Hello " <> name
  where
    p = do
      pathPart "greeting"
      name <- pathParam
      pathEnd
      write @Text $ "I don't mean what I say. " -- Uh oh!
      pure name
```

The compiler won't catch this. Bob, for example, would recieve this response if he hit our endpoint.

```haskell
I don't mean what I say. Hello Bob
```

We can prevent this with a better type signature for `get`.

```haskell
get :: MonadServer m => (forall n. MonadRequest n => n a) -> (a -> m ()) -> m ()
```

This means that the function we pass in for our path pattern can only affect the request, not the response. If we try to affect the response, we will get a compile time error.

```haskell
greeting = get p $ \name -> write @Text $ "Hello " <> name
  where
    p = do
      pathPart "greeting"
      name <- pathParam
      pathEnd
      write @Text $ "I don't mean what I say. " -- Not allowed to use this here. GHC says NO!
      pure name
```

Better, but still not that close to what we see in Method-Path-Handler style frameworks. We can get closer.
The main issue with our current implementation is that we have to code pattern for the path ourselves.
The fact that we can do this is actually great because this gives us the flexibility to implement patterns that might not be possible with just a DSL for specifying the pattern automatically, but our goal isn't to be flexible here. We're trying to mimic these popular Method-Path-Handler style frameworks.

To get even closer to our target, we will use quasiquotation. With quasiquotes we can parse a route pattern DSL and automatically generate the function that is passed into `get`.

```haskell
greeting = get [p|/greeting/:Text|] \name -> write $ "Hello " <> name
```

We can use that exact quasiquote in conventional Okapi style too.

```haskell
main = run id do
  methodGET
  name <- [p|/greeting/:Text|]
  write @Text $ "Hello " <> name
```

The function generated by the quasiquote is just a parser like the rest of the Okapi parsers, so we can use it with parser combinators like `<|>`.

```haskell
main = run id do
  methodGET
  name <- [p|/greeting/:Text|] <|> [p|/my/name/is/:Text|]
  write @Text $ "Hello " <> name
```

Anyways, let's compare the final result with Sinatra, the poster boy of Method-Path-Handler style frameworks.

Here's the greeting endpoint implemented using Sinatra.

```ruby
get '/greeting/:name' do
  "Hello #{params['name']}"
end
```

Here's the same greeting endpoint implemented using the core Okapi library, plus a quasiquoter for generating the path parser.

```haskell
greeting = get [p|/greeting/:Text|] \name ->
  write $ "Hello " <> name
```

I'd say that we successfully mimicked Method-Path-Handler style web frameworks with Okapi.

## Mimicking Yesod

Yesod. Probably the most-used web framework in the Haskell ecosystem, and for good reasons. You can think of it as Ruby on Rails, but replace Ruby with
Haskell. Yesod is a large beast compared to Okapi and has many features, so we will not attempt to mimic Yesod in its entirety.

One aspect of Yesod that makes it really useful is its [type-safe routing](). By type-safe routing I mean a routing system in which types are used to guarantee consistency between code that is generating URLs, and code that is routing URLs to the appropriate handlers. Let's see how this is accomplished in Yesod, and then let's try to mimic this useful feature in Okapi.

Here's a good example from the official [Yesod Web Framework Book]() that I've modified slightly.

```haskell
data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
/calculator CalcR GET
/add/#Int/#Int AddR POST
/sub/#Int/#Int SubR POST
/sq/#Int SqR POST
|]

getCalcR :: Handler Html
getCalcR = defaultLayout
  [whamlet|
    <h1>Calculator
    <h2>Add
    
    <h2>Subtract
    
    <h2>Square
    
  |]

postAddR :: Int -> Int -> Handler Html
postAddR x y = defaultLayout
  [whamlet|
    <h1>Answer: #{x + y}
  |]

postSubR :: Int -> Int -> Handler Html
postSubR x y = defaultLayout
  [whamlet|
    <h1>Answer: #{x - y}
  |]

postSqR :: Int -> Handler Html
postSqR x = defaultLayout
  [whamlet|
    <h1>Answer: #{x * x}
  |]

main :: IO ()
main = warp 3000 App
```

```haskell
pattern CalcR :: (Method, Path)
pattern CalcR = (GET, ["calculator"])

pattern AddR :: Int -> Int -> (Method, Path)
pattern AddR x y = (POST, ["add", PathParam x, PathParam y])

pattern SubR :: Int -> Int -> (Method, Path)
pattern SubR x y = (POST, ["sub", PathParam x, PathParam y])

pattern SqR :: Int -> (Method, Path)
pattern SqR x = (POST, ["sq", PathParam x])

main :: IO ()
main = run id $ route \case
  CalcR -> do
  AddR x y -> do
  SubR x y -> do
  SqR x -> do
  _ -> next
```

## Mimicking Servant

Can Okapi mimic servant? No. Let's just integrate!

## Conclusion

, but if we wanted, we could also define it like this.

```haskell
main = do
  pathPart "greeting"
  name <- pathParam
  write @Text $ "Hello " <> name
  pathEnd
  methodGET
```

Even though this looks way out of order, it has the exact same behavior as the previous implementation! How? Well, you can think of each statement in the
`do` block as a check against the request. As long as all checks are satisified by the request made to the server, a response is returned. `methodGET` checks that the request is a `GET` request. `pathPart "greeting"` checks that first path part is equal to `"greeting"`. `pathParam` checks that there is another path part and parses it, in this case, as `Text`. `pathEnd` checks that there are no more path segments. `write` appends data to the response body that will be returned if all the checks in the `do` block succeed.

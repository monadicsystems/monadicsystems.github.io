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
Since Okapi is a monadic DSL, it is intuitive to use, easy to compose, and we can even apply algebraic properties to our server descriptions.
Due to its' simplicity, I consider Okapi to be "low-level" compared to other web frameworks. What do I mean by this?
Well, Okapi is just a thin abstraction built on top of WAI. It lacks a lot of the built-in features that other frameworks have right out of the box.
The core of Okapi only provides the types and parsers need to manipulate HTTP requests and responses.

## Method-Path-Then-Handler Style Frameworks

This refers to frameworks like Scotty, Spock, Laravel, Sinatra, Express, Flask, etc. where you declare your server's endpoints by specifying a request method, a path pattern, and then a handler function. I'm just going to call this *Method-Path-Then-Handler* style.

We'll create a server that greets a user when a `GET` request is made to `/greeting/<name>`, where `<name>` is path parameter representing the user's name.

This is what that looks like in Scotty.

```haskell
main =
  scotty 3000 $
    get "/greeting/:name" do
        name <- param "name"
        text $ "Hello " <> name
```

Another Haskell web framework, Spock.

```haskell
main =
  runSpock 3000 $ spockT id $
    get ("hello" <//> ":name") do
      name <- param' "name"
      text $ "Hello " <> name
```

The popular Python micro web framework, Flask.

```python
app = Flask(__name__)

@app.route("/greeting/<name>")
def greeting():
    return f'Hello {name}'
```

Ruby's popular micro web framework, Sinatra.

```ruby
get '/greeting/:name' do
  "Hello #{params['name']}"
end
```

Express from JavaScript land.

```js
const app = express()

app.get('/greeting/:name', (req, res) => {
  res.send(`Hello ${req.params.name}`)
})

app.listen(3000)
```

And finally Laravel, a web framework for PHP.

```php
Route::get('/greeting/{name}', function ($name) {
    return 'Hello '.$name;
});
```

## Mimicking Yesod

I need more type safety

## Mimicking Giraffe

## Mimicking Servant

Can Okapi mimic servant? No. Let's just integrate!

## Conclusion

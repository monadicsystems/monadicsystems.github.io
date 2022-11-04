---
title: Web Framework Mimicry
image: /logo-bg.png
summary: How to mimic other web frameworks with Okapi
---

# Web Framework Mimicry

> mimicry - the close **external resemblance** of an animal or plant (**or part of one**) to another animal, plant, or inanimate object.

In this post I want to explore how we can use [Okapi]() to "mimic" other web frameworks.

## What's Okapi?

> **Note**
> You can skip this section if you're already familiar with Okapi.

Okapi is a monadic DSL for decribing web servers. Okapi provides simple *HTTP request parsers* that can be combined with each other using `do` notation and parser combinators to create more complicated parsers.

Here's an example web server from the official [Okapi documentation](https://www.okapi.wiki/) that greets the user.

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

3. Binds a path parameter or a query parameter to `maybeName`, if it exists. Otherwise, `Nothing` is bound to `maybeName`. This line contains two useful            parser combinators:
     * The `optional` parser combinator from the `parser-combinators` library. This transforms the result of a parser into `Nothing` if it fails, or                    `Just value` if it succeeds
     * The `<|>` operator from the `Alternative` typeclass. If the parser on the left side of the operator fails, the parser on the right side of the operator          is tried

4. Checks that the request has no more remaining path parts

5. A let expression that assigns the correct value to `greeting` based on the value of `maybeName`

6. Write `greeting` to the response body

Here's another example of simple server that returns `ping` on a `GET` request to `/pong`, and `pong` on `GET` request to `/ping`.
Notice how we use the `<|>` operator again to combine endpoint definitions.

```haskell
main = run id do
  ping <|> pong

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

Besides the one we just used, Okapi has other algebraic properties that we can use to reason about and simplify our server definitions.

## Mimicking Method-Path-Handler Style Frameworks

Compared to other web frameworks, Okapi is relatively "low-level". This gives developers the flexibility to build abstractions on top of Okapi that suit their specific needs and preferences. We will explore this aspect of the library by using it to implement conventions and patterns that exist in other web frameworks. 

First, we'll start by mimicking what I call *Method-Path-Handler style frameworks*. I mean frameworks like Scotty, Spock, Laravel, Sinatra, Express, etc. where you declare your server's endpoints by specifying a request method, a path pattern, and then a handler function.

Let's see what a server that greets the user looks like in these kind of frameworks.

> **Note**
> I'm excluding imports and the code needed to actually execute the servers.
> I just want to focus on the code needed to define an endpoint.

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

We can see a pattern here. Method-Path-Handler style frameworks share these 3 traits.

1. The use of higher-order functions, such as `get`, `post`, etc. to represent the HTTP method that the endpoint accepts.

2. The higher-order functions representing HTTP methods take a pattern as the first argument, usually represented as a string. This pattern represents
   the URL path that the endpoint will respond to. Path parameters are also defined in this pattern.

3. The higher-order functions take a function representing the handler as the second argument. This handler is executed if the request uses the correct
   method and matches the URL path pattern.

Let's try and *mimic* this style of defining endpoints using Okapi. The most conventional way to implement the endpoint in plain Okapi is the following. 

```haskell
greeting = do
  methodGET
  pathPart "greeting"
  name <- pathParam @Text
  pathEnd
  write $ "Hello " <> name
```

Let's work our way towards Method-Path-Handler style from here.

First, let's define a higher-order function representing the HTTP method that the endpoint accepts.

```haskell
get :: MonadServer m => m a -> (a -> m ()) -> m ()
get getParams handler = do
  methodGET
  params <- p
  handler params
```

With this function we can now define the greeting endpoint like so.

```haskell
greeting = get getParams $ \name -> write @Text $ "Hello " <> name
  where
    getParams = do
      pathPart "greeting"
      name <- pathParam
      pathEnd
      pure name
```

`getParams` represents the path pattern that we see in Method-Path-Handler style frameworks. It's job is to extract data from the path of the request and return it so the handler can use it.

This is good, but we can do better. The the `getParams` parser that we pass into `get`, which represents the pattern for the URL path, can modify the response body if it wants. For example, we could do the following with `getParams`.

```haskell
greeting = get p $ \name -> write @Text $ "Hello " <> name
  where
    p = do
      pathPart "greeting"
      name <- pathParam
      pathEnd
      write @Text "I don't mean what I say. " -- Uh oh!
      pure name
```

The `getParams` parser is only supposed to extract data from the HTTP request, but in the above example we're modifying the HTTP response by mistake in the definition of `getParams`. The compiler won't catch this mistake. Bob, for example, would recieve this response if he hit this faulty endpoint.

```haskell
I don't mean what I say. Hello Bob
```

We can prevent this with a better type signature for `get`.

```haskell
get :: MonadServer m => (forall n. MonadRequest n => n a) -> (a -> m ()) -> m ()
```

This means that the parser representing our path pattern can only affect the request, not the response. If the implementation of the first argument to `get` affects the response in any way, we'll get a compile time error.

```haskell
greeting = get getParams $ \name -> write @Text $ "Hello " <> name
  where
    getParams = do
      pathPart "greeting"
      name <- pathParam
      pathEnd
      write @Text "I don't mean what I say. " -- Not allowed to use this here. GHC says NO!
      pure name
```

Better, but still not that close to what we see in Method-Path-Handler style frameworks. We can get closer.
The main issue with our current implementation is that we have to implement the pattern for the path manually.

The fact that we can do this is actually great because this gives us the flexibility to implement patterns that might not be possible to describe with a DSL, but our goal isn't to be flexible here. Our goal is to mimic Method-Path-Handler style frameworks.

To get even closer to our target, we will use quasiquotation. With quasiquotes we can parse a domain specific language that represents the path pattern, and generate the path parser that is passed into `get`.

```haskell
greeting = get [p|/greeting/:Text|] \name -> write $ "Hello " <> name
```

We can use the `p` quasiquote in conventional Okapi style too.

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

You can also define query parameters, optional parameters, etc. with the `p` quasiquoter. See the implementation [here]() if you're interested. I'll probably write more about this quasiquoter in a future blog post.

Anyways, let's compare the final result with Sinatra, the poster boy of Method-Path-Handler style frameworks.

Here's the greeting endpoint implemented using Sinatra.

```ruby
get '/greeting/:name' do
  "Hello #{params['name']}"
end
```

Here's the same greeting endpoint implemented using the core Okapi library, plus the `p` quasiquote we implemented.

```haskell
greeting = get [p|/greeting/:Text|] \name ->
  write $ "Hello " <> name
```

I'd say that we successfully mimicked Method-Path-Handler style web frameworks with Okapi.

## Mimicking Yesod

Yesod. Probably the most-used web framework in the Haskell ecosystem, and for good reasons. You can think of it as Ruby on Rails, but replace Ruby with
Haskell. Yesod is a large beast compared to Okapi and has many features, so we will not attempt to mimic Yesod in its entirety.

One aspect of Yesod that makes it really useful is its [*type-safe routing*](). By type-safe routing I mean a routing system in which types are used to guarantee consistency between the code that is generating URLs, and the code that is routing URLs to their appropriate handlers. This is especially useful if we have a server-side rendered web application, and we need to link one page of our web application to another.

Let's see how this is accomplished in Yesod, and then let's try to mimic this useful feature in Okapi. Here's a simple example from the official [Yesod Web Framework Book]() that I've modified to create a basic website that just squares numbers and shows the results.

```haskell
data App = App
instance Yesod App

-- (1)
mkYesod "App" [parseRoutes|
/ HomeR GET
/square/#Int SqR GET
|]

getHomeR :: Handler Html -- (2)
getHomeR = defaultLayout
  [whamlet|
    <h1>Welcome!
    <a href=@{SqR 0}>Squares starting at 0
  |]

getSqR :: Int -> Handler Html
getSqR n = do
  let
    prev = n - 1
    next = n + 1  
  defaultLayout -- (3)
    [whamlet|
      <b>The square of #{n} is #{n * n}.
      <a href=@{SqR prev}>What's the square of #{prev}?
      <a href=@{SqR next}>What's the square of #{next}?
      <a href=@{HomeR}>Go Home
    |]

main :: IO ()
main = warp 3000 App
```

>**Note**
> Feel free to skip these annotations if you're already familiar with how Yesod generates routes

1. This `parseRoutes` quasiquoter parses the Yesod routing DSL and generates the code for routing requests to the correct handler functions.
   You can read the official Yesod documentation to learn more about how the DSL works, but I'll give a brief summary here. To define an endpoint
   you must first provide a path pattern that may or may not have path path parameters, such as `/` or `/square/#Int`, where `#Int` represents a path parameter    of type `Int`. Then you must assign an upper case identifier to the route definition, such as `HomeR` or `SqR`. The convention is that the route name ends in    `R`, but this is not necessary. Finally, you may assign one or more HTTP methods to the route. Identifiers like `GET`, `POST`, etc. are used. This will          restrict what kinds of HTTP requests the route will accept.

2. The `getHomeR` handler corresponds to the route `/ HomeR GET`. `getHomeR` MUST be named `getHomeR`. This is not a convention. To name a handler function        correctly for a route definition the name of the handler function must start with the HTTP method that the route accepts, in lowercase letters, followed by      the name of the route definition. So `/ HomeR GET` corresponds to `getHomeR` and `/square/#Int SqR GET` corresponds to `getSqR`. Another thing to note is        that the type of the handler function must correctly correspond to the route definition. For example, `getSqR` takes an `Int` as an argument because its
   corresponding route definition has a path parameter of type `Int`.

3. Here we're using the `hamlet` DSL to generate the HTML that's returned to the client. We can embed values in our HTML using `#{}`, and embed our                application's routes in the template using `@{}`. I want you to focus on the use of `@{}` throughout this template. Inside of the `@{}` declarations you'll      see the identifiers that we assigned to our route definitions earlier. Insteading of manually typing the value of our hrefs and potentially making a mistake
   that isn't caught at compile-time, we can use `@{HomeR}` or `@{SqR 0}` to automatically and correctly generate the values of our hrefs. Since the `SqR` route    defintion contains a path parameter of type `Int`, we must pass a value of type `Int` into the `SqR` constructor. For example, `@{SqR 1}` generates
   the URL `/square/1`.

The main point that I'm trying to get across is that Yesod provides type safety to the developer. The more mistakes the developer can identify at compile-time, before our code is executed, the better. Yesod protects the developer from the possibility of mistyping a URL. As long as the developer uses `@{}` to generate links in their templates, they are guaranteed to take the user to a valid location.

How can we mimic this useful feature in Okapi?
Okapi partially achieves the level of safety using *bidirectional patterns*. As the name implies, bidirectional patterns can be used to
to both pattern match and construct values. Let's explore how this can be used to our advantadge.

```haskell
-- (1)
pattern HomeR :: (Method, Path)
pattern HomeR = (GET, [])

pattern SqR :: Int -> (Method, Path)
pattern SqR n = (GET, ["square", PathParam n])

-- (2)
route :: MonadServer m => ((Method, Path) -> m ()) -> m ()
route handler = do
  requestMethod <- method
  requestPath   <- path
  handler (requestMethod, requestPath)

main :: IO ()
main = run id $ route \case -- (3)
  HomeR -> write $ renderHtml $
    [hamlet|
      <h1>Welcome!
      <a href=@{SqR 0}>Squares starting at 0
    |] render
  SqR n -> do
    let
      prev = n - 1
      next = n + 1
    write $ renderHtml $
      [hamlet|
        <b>The square of #{n} is #{n * n}.
        <a href=@{SqR prev}>What's the square of #{prev}?
        <a href=@{SqR next}>What's the square of #{next}?
        <a href=@{HomeR}>Go Home
      |] render
  _ -> next
  
-- (4)
render :: Render (Method, Path)
render (_, requestPath) _ = renderPath requestPath
```

1. Instead of using a custom DSL to define our routes, we use the pattern synonyms to define them.
   In this case our pattern synonyms represent a tuple of `Method` and `Path`. `Path` is just a type synonym for `[Text]`.
   If our route contains path parameters, we use the `PathParam` type synonym to express that in our `Path` pattern.
   The definitions are pretty self-explanatory. Also note the type signatures of the pattern synonyms.

2. The `route` function is a higher-order function that is parameterized by a handler function of the type `MonadServer m => (Method, Path) -> m ()`. `route`      extracts the HTTP method and URL path of the request, and passes these as a tuple to the handler function.
   
3. For the handler passed into the `route` function we just use a simple lambda function. The `-XLambadCase` language extension allows us to begin pattern          matching on the lambda's argument without binding it to an identifier. In this case, we're pattern matching on values of type `(Method, Path)` using the
   pattern synonyms we defined earlier. If the request doesn't match any of our pattern synonyms and we reach the last case in the case statement, we call          `next`, which tries another parser if there is one. In this case we're not using the `route` function with `<|>` to combine it with another parser, so it        will just return a `404 Not Found` error. If there is a match, the correct response modifiers are executed. You'll notice that I'm using the `hamlet`            templating language. Usually I use `lucid` to generate HTML when using Okapi, but to keep this example similar to the Yesod example, I'm using `hamlet`.
   It also shows that you can use Okapi with the templating language of your choice.
   
4. The `render` function is used to tell the Hamlet template how to render values of type `(Method, Path)` as URLs. Here, we just ignore the method part of the
   tuple and use the `renderPath` provided by Okapi to render the request path as an URL.
   
Looks good! Obviously, Yesod does a lot more than just provide type-safe routing, but I think it's great that we can at least mimic some of Yesod's functionality with Okapi and minimal additional complexity. We just used higher-order functions and pattern synonyms in addtion to Okapi's core parsers to implement this feature.

## Mimicking Servant

Servant. My first Haskell web framework, and one of the most practical uses of type-level programming that I know of.
In Servant, you define your API endpoints as types. Here's an example from the official Servant documentation.

```haskell
type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]
              -- describes an endpoint reachable at:
              -- /users/list-all/now
```

This type definition represents an API that returns a list of `User` as `JSON` when a `GET` request is made to `/users/list-all/now`.

Cool, right? This isn't just for show though. By using types to define API endpoints, we not only get type-safe routing as we defined it in the previous section, but also automatic client-side code generation, automatic API documentation generation, and stronger formal guarantees about the API at compile-time. Why is the fact that we have stronger formal gurantees about our API at compile time important? What does this mean? Well, it means we know more about the behavior of the API before we even execute it. Before we run the program we know what type of requests our endpoints will accept, what type of data they will return, and even what type of errors they may throw. Knowing all of this information at compile time is useful because we get practical features like automatic client-side code generation and API documentation generation for free.

So, can we mimic this level of type-safety in Okapi? The short answer is, no. A mentor of mine mentioned that it maybe possible to achieve more type-safety with Okapi by using [*indexed monads*](). I won't go into detail about what indexed monads are here, but It's an interesting topic and I suggest you read about them if you're already fairly comfortable with monads. Anyways, even if we did make Okapi an indexed monadic DSL it would require us to change the core library quite drastically which is something we're trying to avoid here. I am open to someone creating a `okapi-indexed` library though.

So, are there any other options? Instead of trying to mimic the power of Servant, let's form a symbiotic relationship with Servant instead.

> symbiosis - interaction between two different organisms living in close physical association, **typically to the advantage of both**.

Since Okapi and Servant share a common ancestor, WAI, we can use them with each other without much friction. [There's a detailed tutorial on how to do this
in the official Okapi documentation]().

If you have a long-standing Servant project and want to try Okapi without rewriting all your code, you can.
If you try Okapi and find out you don't like it or need more type-safety, you can gradually migrate to Servant.

One way of using Okapi that could potentially be useful is using Okapi as a quick prototyping tool and then gradually migrating your API
to Servant to "harden" it over time.

## Conclusion

We discovered how flexible Okapi is and how we can customize it in various ways to suit our specific needs and/or preferences.
Okapi is still very much a WIP! At this stage I'm just experimenting with ideas, but would like to have 1.0 release ready for next year if the community thinks it would be worth the effort.
There are still many things that need to be done in order for Okapi to be production ready. I can only work on this project part-time, so if you're
looking for a fun, cool, modern side-project to work on, checkout it out on GitHub.

One thing I would like to do in the near future is build a realword application, preferably a SaaS application, with Okapi to see how it fares in comparison to other web frameworks. I'd also like to get some benchmarks going.

Stay tuned for my next post where we will attempt to recreate the Elm Architecture on the server in pure Haskell using Okapi + Lucid + Htmx.

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

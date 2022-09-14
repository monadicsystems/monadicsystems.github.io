---
title: Type Safe Named Routes Using Patterns: Part II
image: /logo-bg.png
summary: A blog post about type safe named routes in Okapi
---

# Type Safe Named Routes Using Patterns: Part II

In Part I, we pondered the motivation for type safe named routes and how they work in other frameworks like Yesod and Servant.
Now we can talk about how type safe named routes are implemented in Okapi.

Before we see examples of how type safe named routes work in Okapi, we must first understand Haskell's `-XPatternSynonyms` language extension.

### -XPatternSynonyms

Patterns in Haskell are used to **construct** and **destructure** data.
If you use Haskell, you define patterns all the time. For example, when you define a data type like

```haskell
data Foo = Foo Text Int
```

you're creating a pattern that can be used to create values of that type (often called *data construtors*),

```haskell
myFoo :: Foo
myFoo = Foo "hello" 54
```

and for deconstructing values of that type (often called *pattern matching*).

```haskell
getTextFromFoo :: Foo -> Text
getTextFromFoo (Foo text _) = text
```

There's also a way to define patterns without defining a data type.
To do this we must use Haskell's `-XPatternSynonyms` language extension. When this language extension is turned on we gain the ability to define custom patterns for existing types. The syntax looks like this:

```haskell
pattern FooText :: Foo -> Text
pattern FooText text <- Foo text _
```

Pattern declarations are similar to function declarations, but they are prefixed with the `pattern` keyword
and must have an uppercase identifier, like data constructors. They can even have type signatures! You'll also notice that instead
of an `=` sign preceding the body of the declaration, a `<-` is used instead. This reverse arrow is used when defining what's
called a **unidirectional pattern**. Unidirectional patterns are unidirectional because they can only be used for deconstructing values, not constructing them.
For example, let's redefine the function `getTextFromFoo` using the unidirectional pattern we defined:

```haskell
getTextFromFoo :: Foo -> Text
getTextFromFoo (FooText text) = text
```

If we try to use the same pattern to construct a value of type `Foo`, we will get a compiler error:

```haskell
myFoo :: Foo
myFoo = FooText "hello" -- THIS WON'T WORK BECAUSE FooText IS UNIDRECTIONAL! ONE WAY! NOT BOTH!
```

This makes sense because if we look at our `FooText` pattern declaration, there's no information about
what should stand in for the second parameter of the `Foo` constructor. To remedy this we can explicitly define the second parameter to the
`Foo` constructor in our `FooText` pattern declaration using another syntax:

```haskell
pattern FooText :: Foo -> Text
pattern FooText text <- Foo text _ -- How we DECONSTRUCT values of type Foo
  where
    FooText text = Foo text 9000   -- How we CONSTRUCT values of type Foo
```

Patterns declared using this syntax are called **explicit bidirectional patterns** because the programmer is explicitly defining how the pattern
constructs and deconstructs a value. Now we can use `FooText` not only as a deconstructor, but a constructor as well:

```haskell
anotherFoo :: Foo
anotherFoo = FooText "YEAH"

-- >>> anotherFoo == Foo "YEAH" 9000
-- True
-- >>> anotherFoo == Foo "YEAH" 69
-- False
```

Suppose we needed a way to construct and deconstruct values of type `Foo`, but with `Foo`'s parameters flipped. We could do this using the explicit bidirectional pattern syntax,

```haskell
pattern FlippedFoo :: Int -> Text -> Foo
pattern FlippedFoo int text <- Foo text int
  where
    FlippedFoo int text = Foo text int
```

but can also use another syntax to express the same idea in a more concise way.

```haskell
pattern FlippedFoo :: Int -> Text -> Foo
pattern FlippedFoo int text = Foo text int
```

Patterns declared in this manner are called **implicit bidirectional patterns**. The body of implicit bidirectional pattern declarations is prefixed an `=` sign, just like function declarations, instead of the `<-` used in unidirectional and explicit bidirectional patterns. This can only be done if all pattern variables are used in the body of the pattern declaration.

Now we can use `FlippedFoo` just like `Foo`, except the parameters are in reverse order.

```haskell
-- >>> FlippedFoo 54 "Hello" == Foo "Hello" 54
-- True
-- >>> FlippedFoo 9000 "YEAH" == Foo "YEAH" 9000
-- True
```

Nice!

## How Okapi Uses Patterns For Type Safe Named Routes

In Okapi, type safe named routes are implemented using the language extensions we defined above. It works becuase we can use the same "identifier", a bidirectional pattern, to deconstruct and construct an HTTP request.

Okapi exports a function `route` with the type signature:

```haskell
route ::
  MonadOkapi m =>
  m a ->
  -- ^ Parser
  (a -> Handler m) ->
  -- ^ Dispatcher
  Handler m
```

`route` takes a *parser* that's used to parse data from the HTTP request, and a *dispatcher* that's used to choose the correct handler based on the data parsed by the parser.

The simplest way to use `route` is to give it the `path` parser (the parser that extracts the request's path information), and a function that matches the path parsed from the request to the appropriate handler.

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Okapi

main :: IO ()
main = run id $ route path $ \case
  ["home"] -> do
    methodGET
    return ok
  ["introduce", name] -> do
    methodGET
    return $ redirect 302 $ "/greet/" <> name
  ["greet", name] -> do
    methodGET
    return $ setPlaintext ("Hello " <> name) $ ok
  ["greet"] -> do
    methodGET
    maybeName <- optional $ queryParam "name"
    let greeting = case maybeName of
      Nothing   -> "Hello, Stranger."
      Just name -> "Hello, " <> name <> "."
    return $ setPlaintext greeting $ ok
  _ -> next
```

The `-XLambdaCase` language extension is perfect here because we can define functions just used for pattern matching in a more concise way. It allows us to skip the boilerplate code required to bind the lambda argument, and then pattern match on it with a `case` statement. We can simply use `\case` instead.

This is cool, but we can do better. Our handler for `GET /introduce/{name}` (the second case of the case statement) returns a redirect to another location on our server, but to create the URL we use string concatenation. As we mentioned in Part I of this series, making URLs via string concatentation is prone to developer error, so let's use bidirectional patterns to make sure the redirect is guaranteed to take the user to a valid location.

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Okapi

pattern HomeRoute = ["home"]

pattern IntroduceRoute name = ["introduce", name]

pattern GreetNameRoute name = ["greet", name]

pattern GreetOptionalNameRoute = ["greet"]

main :: IO ()
main = run id $ route path $ \case
  HomeRoute -> do
    methodGET
    return ok
  IntroduceRoute name -> do
    methodPOST
    redirect 302 $ renderPath $ GreetRoute name
  GreetRoute name -> do
    methodGET
    return $ setJSON ("Hello " <> name) $ ok
  GreetOptionalNameRoute -> do
    methodGET
    maybeName <- optional $ queryParam "name"
    let greeting = case maybeName of
      Nothing   -> "Hello, Stranger."
      Just name -> "Hello, " <> name <> "."
    return $ setJSON greeting $ ok
  _ -> next
```

Now, we pattern match on the request path using pattern synonyms. We also use the same patterns to construct the URLs to those handlers. We use the `renderPath` function exported by Okapi to automatically generate the URL for our pattern synonyms. No more string concatenation.

What do we do if the path parameters are a type other than `Text`? The path parameter we assign to the `name` pattern variable in the above examples is of the `Text` type. What if requirements changed and we needed to identify people using an identifier of type `Int`? Okapi exports the bidirectional pattern `PathParam` that's perfect for situations when we want to match on a path parameter that's of a type other than `Text`, as long as the type implements the `ToHttpApiData` and `FromHttpApiData` type classes.

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Okapi

pattern HomeRoute = ["home"]

pattern IntroduceRoute pid = ["introduce", PathParam pid]

pattern GreetNameRoute pid = ["greet", PathParam pid]

pattern GreetOptionalNameRoute = ["greet"]

main :: IO ()
main = run id $ route path $ \case
  HomeRoute -> do
    methodGET
    return ok
  IntroduceRoute pid -> do
    methodPOST
    redirect 302 $ renderPath $ GreetRoute pid
  GreetRoute pid -> do
    methodGET
    return $ setJSON pid $ ok
  GreetOptionalNameRoute -> do
    methodGET
    maybePid <- optional $ queryParam "pid"
    let returnPid = case maybePid of
      Nothing  -> -1
      Just pid -> pid
    return $ setJSON returnPid $ ok
  _ -> next
```

Awesome!

We've only covered the simplest use case of type safe named routes in Okapi. What if we want to pattern match on properties of the request other than
the path?

Imagine we had a cooking recipe app with two types of forms, one for querying recipes and another for submitting new recipes. The HTML for the two forms would look like this.

```html
<form action="/recipe" method="post">
  ...
</form>

<form action="/recipe" method="get">
  ...
</form>
```

If we could pattern match on the request method and request path, we could safely generate type safe form attributes for our HTML forms too. Just like how we generated the URL for our redirect in the previous example.

```html
<form {renderFormAttributes PostRecipeRoute}>
  ...
</form>

<form {renderFormAttributes QueryRecipesRoute}>
  ...
</form>
```

This would ensure that all form actions in our HTML are valid.

[To be continued...](/post4)

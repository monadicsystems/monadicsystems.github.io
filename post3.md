---
title: Type Safe Named Routes Using Patterns: Part II
image: /logo-bg.png
summary: A blog post about type safe named routes in Okapi
---

# Type Safe Named Routes Using Patterns: Part II

In Part I, we pondered the motivation for type safe named routes and how they work in other frameworks like Yesod and Servant.
Now we can talk about how type safe named routes are implemented in Okapi.

## Patterns On Steroids

Before we see examples of how type safe named routes work in Okapi, we must first understand two language extensions
that make Haskell's patterns more powerful: `-XPatternSynonyms` and `-XViewPatterns`.

### -XPatternSynonyms

Bidirectional patterns are a feature in Haskell that can be used to **construct** and **destructure** data.
If you use Haskell, you define bidirectional patterns all the time. For example, when you define a data type like

```haskell
data Foo = Foo Text Int
```

you're creating a constructor that can be used to create values of that type

```haskell
myFoo :: Foo
myFoo = Foo "hello" 54
```

and a destructor (usually called a pattern) that's used for pattern matching on that type

```haskell
getTextFromFoo :: Foo -> Text
getTextFromFoo (Foo text _) = text
```

There's also a way to define patterns without defining a data type.
To do this we must use Haskell's `PatternSynonyms` extension. When this language extension is turned on, we gain the ability to define custom patterns for existing types. The syntax looks like this:

```haskell
pattern FooText :: Foo -> Text
pattern FooText text <- Foo text _
```

As you can see, pattern declarations are similar to function declarations, but they are prefixed with the `pattern` keyword
and must have an uppercase identifier, like data constructors. They can even have type signatures! You'll also notice that instead
of a `=` preceding the body of the declaration, a `<-` is used instead. This reverse arrow is used when defining what's
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
what should stand in for the second parameter of the `Foo` constructor. To remedy this, we can explicitly define the second parameter to the
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

Suppose we needed a way to construct and deconstruct values of type `Foo`, but with `Foo`'s parameters flipped.
We can do this using another syntax for pattern declarations:

```haskell
pattern FlippedFoo :: Int -> Text -> Foo
pattern FlippedFoo int text = Foo text int
```

Patterns declared in this manner are called **implicit bidirectional patterns**. Implicit bidirectional pattern declarations have a RHS prefixed with `=` instead of `<-`. This can only be done if all pattern variables on the LHS are used on the RHS. We can use `FlippedFoo` just like `Foo`,
except the parameters are in reverse order:

```haskell
-- >>> FlippedFoo 54 "Hello" == Foo "Hello" 54
-- True
-- >>> FlippedFoo 9000 "YEAH" == Foo "YEAH" 9000
-- True
```

Nice!

### -XViewPatterns

Yes, there's more! We can make patterns even more powerful in Haskell using the `-XViewPatterns` language extenstion.
When this language extension is turned on, we can pattern match on the *projection of a value* and not just the value itself.
This useful for pattern matching on data structures, like values of the `Map` type from the `containers` package.

Suppose we have a map representing the favorite colors of random people

```haskell
import qualified Data.Map as Map

data Color = Red | Blue | Yellow

favoriteColors :: Map.Map Text Color
favoriteColors = Map.fromList [("Bob", Blue), ("Alice", Yellow), ("Larry", Yellow)]
```

and we want to see if a specific person's favorite color is `Blue` using view patterns.
First, we need to define what I call a **projection function**. The projection function is what we'll use in the view pattern to project a property of the data structure that we want to pattern match on. We'll call this view function `projectolor`:

```haskell
projectColor :: Text -> Map.Map Text Color -> (Maybe Color, Map.Map Text Color)
projectColor name colorMap = case Map.lookup name colorMap of
  Nothing    -> (Nothing, colorMap)
  Just color -> (Just color, Map.delete (name, color) colorMap)
```

It takes a name of type `Text`, a `Map` from `Text` to `Color`, and returns either one of these tuples:

1. A `Color` value wrapped with `Just` if it's found in the `Map`, and a new `Map` with the entry that was found deleted from it
2. `Nothing`, and the original `Map` that was passed into the view function

The return type of the projection function is important because that's what we'll be pattern matching on in our view pattern.

Now let's use `projectColor` in a view pattern.

```haskell  
favoriteColorIsBlue :: Text -> Map.Map Text Color -> Bool
favoriteColorIsBlue name (viewColor name -> (Just Blue, _)) = True
favoriteColorIsBlue _ _ = False
```

You'll notice on the first line of our function declaration, in place of the second parameter, there's an application of our `projectColor` function followed by a `->`. That's a view pattern. We're applying our projection function `projectColor name` to the argument and pattern matching on the result by using `->`.

Notice how function currying is being used to our advantadge here. The type of `projectColor` is `Text -> Map Text Color -> (Maybe Color, Map Text Color)`, but the type of `projectColor name` is `Map Text Color -> (Maybe Color, Map Text Color)` which allows us to use it in this view pattern. Also notice how we can pass in any variables on the RHS into the view function that's used in our view pattern. In this case, it's `name`.

The pattern after the `->` is for matching on the tuple result of the projection function. In this case we just want to match on `Just Blue` and ignore the new `Map Text Color` value.

You may be wondering, "Why put all that logic into a pattern and not just use a regular function?". Well, this is a contrived example so I agree with you.
The benefit of using view patterns is that you can create abstractions that you wouldn't be able to otherwise. In some cases, a solution using view patterns can be more concise and allow more code reuse. For example, let's say we wanted to create a function that takes the names of two people, a map of everyone's favorite color, and returns whether or not the two people would get along based on their color preferences:

```haskell
{-
People who like Blue get along with other people that like Blue or Yellow
People who like Yellow get along with everybody
People who like Red can only get along with people who like Yellow
-}
getsAlong :: Text -> Text -> Map.Map Text Color -> Bool
getsAlong person1 person2 (viewColor person1 -> (Just Yellow, viewColor person2 -> (Just _, _))) = True
getsAlong person1 person2 (viewColor person1 -> (Just Red, viewColor person2 -> (Just Yellow, _))) = True
getsAlong person1 person2 (viewColor person1 -> (Just Blue, viewColor person2 -> (Just Blue, _))) = True
getsAlong person1 person2 (viewColor person1 -> (Just Blue, viewColor person2 -> (Just Yellow, _))) = True
getsAlong _ _ = False
```

Notice how we can nest view patterns. In this case, we're looking for two people in a map to see if they would get along so we use a view pattern on the map passed into the function to see what color the first person likes, and then we use the same view pattern again on the map result from the first view pattern and 
see what color the second likes. Remember how our projection function `viewColor` deletes an entry in the map if it was found? This is where it comes in handy: when we want to project values out of a data structure multiple times.

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

`route` takes a *parser* that's used to parse data from the HTTP request, and a *dispatcher* that's used to choose the correct handler based on the data parsed by the parser function passed in as the first parameter.

The simplest way to use `route` is to give it the `path` parser, and a function that matches the path parsed from the request (a list of `Text`) to the appropriate handler.

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
    redirect 302 $ "/greet/" <> name
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

The `LambdaCase` extension is perfect here because we can define functions that only do pattern matching in a more concise way. It allows us to skip the boilerplate code required to bind the lambda argument and pattern match on it with a `case` statement. We can just use `\case`.

This is cool, but we can do better by using type safe named routes. One of our handlers returns a redirect to another handler on our server, so let's use a bidirectional pattern to make sure the redirect is guaranteed to redirect the user to a valid location.

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

We pattern match on the request path using the custom patterns we defined. We also use the same patterns to construct the URLs to those handlers. Since our custom patterns construct and deconstruct the `Path` type, we use the `renderPath` function when generating the URL for our custom patterns.

What do we do if the path parameters are a type other than `Text`? In the above example, the `name` path parameter has the type `Text`. What if requirements changed and we needed to identify people using an identifier of type `Int`? Okapi exports the pattern `PathParam` that's perfect for situations when we want to match on a path parameter that's of a type other than `Text`:

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Okapi

pattern HomeRoute = ["home"]

pattern IntroduceRoute pid = ["introduce", PathParam pid]
-- ^ Uses @PathParam@ pattern to match on values of any type that implement the @ToHttpApiData@ and @FromHttpApiData@ type classes.

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

Sweet. The above example is the simplest case of type safe named routes in Okapi. What if we want to pattern match on properties of the request other than
the path? This would be useful in cases where we have forms, like in a todo app for example. If we can pattern match on the request method and request path, we can create type safe form actions to put in our HTML.

[To be continued...](/post4)

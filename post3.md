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
To do this we must use Haskell's `-XPatternSynonyms` language extension. When this language extension is turned on, we gain the ability to define custom patterns for existing types. The syntax looks like this:

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

### -XViewPatterns

Yes, there's more! We can make patterns even more powerful in Haskell using the `-XViewPatterns` language extenstion.
When this language extension is turned on, we can pattern match on the *projection of a value* and not just the value itself.
This useful for pattern matching on data structures, like values of the `Map` type from the `containers` package.

Suppose we have a map representing the favorite colors of random people,

```haskell
import qualified Data.Map

data Color = Red | Blue | Yellow

favoriteColors :: Map Text Color
favoriteColors = fromList [("Bob", Blue), ("Alice", Yellow), ("Larry", Yellow)]
```

and we want to see if a specific person's favorite color is `Blue`. We could just use a traditional function, but let's try to solve this using view patterns.

First, we need to define what I call a **projection function**. The projection function is what we'll use in the view pattern to project a property of the data structure that we want to pattern match on. We'll call this view function `projectColor`:

```haskell
projectColor :: Text -> Map Text Color -> (Maybe Color, Map.Map Text Color)
projectColor name colorMap = case Map.lookup name colorMap of
  Nothing    -> (Nothing, colorMap)                          
  Just color -> (Just color, Map.delete (name, color) colorMap)
```

It takes a name of type `Text`, a `Map` from `Text` to `Color`, and returns either one of these tuples:

1. `Nothing`, and the original `Map` that was passed into the view function
2. A `Color` value wrapped with `Just` if it's found in the `Map`, and a new `Map` with the entry that was found deleted from it

The return type of the projection function is important because that's what we'll be pattern matching on in our view pattern.

Now let's use `projectColor` in a view pattern.

```haskell  
favoriteColorIsBlue :: Text -> Map Text Color -> Bool
favoriteColorIsBlue name (projectColor name -> (Just Blue, _)) = True
favoriteColorIsBlue _ _ = False
```

You'll notice on the first line of our function declaration, in place of the second pattern variable, there's an application of our `projectColor` function followed by a `->`. After the `->` there's a tuple pattern, matching on the result of `projectColor`. That's a view pattern. We're applying our projection function `projectColor name` to the function's second argument of type `Map Text Color`, and pattern matching on the resulting tuple by using `->`.

Notice how function currying is being used to our advantadge here. The type of `projectColor` is `Text -> Map Text Color -> (Maybe Color, Map Text Color)`, but the type of `projectColor name` is `Map Text Color -> (Maybe Color, Map Text Color)`. This fact allows us to use `projectColor name` in the view pattern to match on a projection of `Map Text Color`, in this case a tuple.

Also, take note of how we can partially apply our projection function to other function parameters, in this case `name`.

The pattern after the `->` is for matching on the tuple result of the projection function. In this case we just want to match on `Just Blue`, and don't care about the new `Map Text Color` value that's produced.

You may be wondering, "Why put all that logic into a pattern and not just use a regular function?". Well, this is a contrived example so I agree with you.
The benefit of using view patterns is that you can create abstractions that you wouldn't be able to create otherwise. In some cases, a solution using view patterns can be more concise and allow for more code reuse.

For example, let's say we wanted to create a function that takes the names of two people, a map of everyone's favorite color, and returns whether or not the two people would get along based on their color preferences.

```haskell
{-
People who like Blue get along with other people that like Blue or Yellow
People who like Yellow get along with everybody
People who like Red can only get along with people who like Yellow
-}
getsAlong :: Text -> Text -> Map.Map Text Color -> Bool
getsAlong person1 person2 (projectColor person1 -> (Just Yellow, viewColor person2 -> (Just _, _))) = True
getsAlong person1 person2 (projectColor person1 -> (Just Red, viewColor person2 -> (Just Yellow, _))) = True
getsAlong person1 person2 (projectColor person1 -> (Just Blue, viewColor person2 -> (Just Blue, _))) = True
getsAlong person1 person2 (projectColor person1 -> (Just Blue, viewColor person2 -> (Just Yellow, _))) = True
getsAlong _ _ = False
```

Notice how we can nest view patterns. In this case, we're looking for two people in a map to see if they would get along. We use a view pattern on the map passed into the function to see what color the first person likes. Then we use the same view pattern again on the new map returned by the first view pattern and 
see what color the second person likes. Remember how our projection function `projectColor` deletes an entry in the map if it was found? This is where it comes in handy: when we want to project values out of a data structure multiple times.

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

```haskell
<form action="/recipe" method="post">
  ...
</form>

<form action="/recipe" method="get">
  ...
</form>
```

If we could pattern match on the request method and request path, we could safely generate type safe form attributes for our HTML forms too. Just like how we generated the URL for our redirect in the previous example.

```haskell
<form {renderFormAttributes PostRecipeRoute}>
  ...
</form>

<form {renderFormAttributes QueryRecipesRoute}>
  ...
</form>
```

[To be continued...](/post4)

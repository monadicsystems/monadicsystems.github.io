---
title: Type Safe Named Routes Using Patterns: Part III
image: /logo-bg.png
summary: A blog post about type safe named routes in Okapi
---

# Type Safe Named Routes Using Patterns: Part III

In Part I we pondered the motivation for type safe named routes, and how they work in other frameworks like Yesod and Servant.

In Part II we covered pattern synonyms, and how we can use them to implement type safe named routes in Okapi.

In the final post of this series, we'll look at how to handle more complicated cases where we need to dispatch
on other request information besides the path.

## Type Safe Form Attributes

At the end of Part II, we left off with this interesting thought.

> Imagine we had a cooking recipe app with two types of forms, one for querying recipes and another for submitting new recipes.
> The HTML for the two forms would look like this.
> ```html
> <form action="/recipe" method="post">
>  ...
> </form>
>
> <form action="/recipe" method="get">
>  ...
> </form>
> ```
>
> If we could pattern match on the request method and request path, we could safely generate type safe form attributes for our HTML forms too. Just like how we > generated the URL for our redirect in the previous example.
>
> ```html
> {% raw %}<form {renderFormAttributes PostRecipeRoute}>
>   ...
> </form>
>
> <form {renderFormAttributes QueryRecipesRoute}>
>   ...
> </form>{% endraw %}
> ```
>
> This would ensure that all form actions in our HTML are valid.

To implement this thought let's model what a recipe is, and declare our pattern synonyms for the routes.
To help with pattern matching on the request method Okapi exports bidirectional patterns for all request methods, like `GET` and `POST`.

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Okapi

data Recipe = Recipe
  { recipeName :: Text
  , recipeIngredients :: [Text]
  , recipeDetail :: Text
  , recipeCookTime :: Int -- ^ Cook time of the recipe in minutes
  }

pattern HomeRoute = (GET, [""]) -- ^ @[""]@ represents the trailing slash e.g. www.example.com/

pattern QueryRecipesRoute = (GET, ["recipes"])

pattern PostRecipesRoute = (POST, ["recipes"])
```

Now that we've defined our route patterns, let's implement our rendering functions. Now, we will have two rendering functions: one for generating URLs and one for generating HTML form actions.

```haskell
renderURL :: (Method, Path) -> Text
renderURL (_, p) = renderPath p

renderFormAttrs :: (Method, Path) -> Text
renderFormAttrs (m, p) = renderAction p <> " " <> renderMethod m
  where
    renderAction p = "action=\"" <> renderPath p <> "\""
    renderMethod = \case
      POST -> "method=\"" <> "post" <> "\""
      _    -> "method=\"" <> "get" <> "\"" -- ^ method="get" is the default method for forms
```

Sweet! You might be thinking,

> Hmmmm, I thought the appeal of type safe named routes was that we remove the possibility of developer error.
> In this case, we're implementing these rendering functions ourselves.
> Couldn't the developer make a mistake here that isn't caught by the compiler?
> What if the string concatenation in the implementation of `renderFormAction` isn't correct?

This is true, the developer can make mistakes when implementing custom rendering functions and they may not be caught by the compiler.
Still, I believe this is better than plain string concatenation because it's much easier to test and find potential bugs in the implementation.
We can use basic unit tests to see that the rendering functions generate the correct string.
It's a lot harder to test if we have string concatenation directly in the HTML template. We can only manually test the web application, or look at the generated HTML to see if the strings were concatenated correctly.

Also, consider the case where we have to use these generated URLs and form actions in multiple places.
With the custom rendering functions, we just have to implement them once, test them if we see the need, and be done with it. If we do find bugs in the URLs/form actions in the future, we just have to reimplement the rendering function that's causing it.
If we were to use string concatenation directly in our HTML template, in multiple places, we'd risk introducing a bug each time we need generate a URL/form action. We'd also have to update the same code in multiple places if we were to make any change to our URL structure. This is no good.

Now that we have our route patterns and rendering functions, let's implement the parser and dispatcher function that we're passing to `route`.
Remember, the type of the `route` function is `MonadOkapi m => m a -> (a -> Handler m) -> Handler m` where the first argument is the parser and the second
argument is the dispatcher.

For the parser, we want to return the request method and request path because that what we've defined our pattern synonyms to match on;
a value of type `(Method, Path)`.

```haskell
methodAndPath :: MonadOkapi m => m (Method, Path)
methodAndPath = do
  m <- method
  p <- path
  return (m, p)
```

Brilliant. Now we can define the dispatcher function using `-XLambdaCase`, and feed that in to the `route` function directly to bring everything together.

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Okapi

data Recipe = Recipe
  { recipeName :: Text
  , recipeIngredients :: [Text]
  , recipeDetail :: Text
  , recipeCookTime :: Int -- ^ Cook time of the recipe in minutes
  }

pattern HomeRoute = (GET, [""]) -- ^ @[""]@ represents the trailing slash e.g. www.example.com/

pattern QueryRecipesRoute = (GET, ["recipes"])

pattern PostRecipesRoute = (POST, ["recipes"])

renderURL :: (Method, Path) -> Text
renderURL (_, p) = renderPath p

renderFormAttrs :: (Method, Path) -> Text
renderFormAttrs (m, p) = renderAction p <> " " <> renderMethod m
  where
    renderAction p = "action=\"" <> renderPath p <> "\""
    renderMethod = \case
      POST -> "method=\"" <> "post" <> "\""
      _    -> "method=\"" <> "get" <> "\"" -- ^ method="get" is the default method for forms
      
methodAndPath :: MonadOkapi m => m (Method, Path)
methodAndPath = do
  m <- method
  p <- path
  return (m, p)

main :: IO ()
main = run id $ route methodAndPath $ \case
  HomeRoute -> do
    let html =
      [qq|
          <h1>Recipes Home</h1>
          <hr>
          <h2>Query Recipes</h2>
          <form {renderFormAttrs QueryRecipesRoute}>
            ...
          </form>
          <hr>
          <h2>Create New Recipes</h2>
          <form {renderFormAttrs PostRecipesRoute}>
            ...
          </form>
      |]
    return $ setHTML html $ ok
  QueryRecipesRoute -> do
    maybeTodo <- liftIO $ queryTodo tID
    case maybeTodo of
      Nothing   -> return notFound
      Just todo -> return $ setJSON todo $ ok
  PostRecipesRoute -> do
    todoForm <- bodyForm @TodoForm
    maybeNewTodo <- liftIO $ insertTodoForm todoForm
    case maybeNewTodo of
      Nothing      -> return notFound
      Just newTodo -> return $ setJSON newTodo $ ok
  _ -> next
```

For the HTML templating in this example I'm using the `interpolatedstring-perl6` package.
This package makes it really easy to interpolate Haskell expressions in multiline strings.

### -XViewPatterns

Yes, there's more! We can make patterns even more powerful in Haskell using the `-XViewPatterns` language extenstion.
When this language extension is turned on we can pattern match on the *projection of a value* and not just the value itself.
This is useful for pattern matching on data structures, like values of the `Map` type from the `containers` package.

Suppose we have a map representing the favorite colors of random people,

```haskell
import qualified Data.Map

data Color = Red | Blue | Yellow

favoriteColors :: Map Text Color
favoriteColors = fromList [("Bob", Blue), ("Alice", Yellow), ("Larry", Yellow)]
```

and we want to see if a specific person's favorite color is `Blue`. We could just use a traditional function, but let's try to solve this using view patterns.

First, we need to define what I call a **projection function**. The projection function is what we'll use in the view pattern to project a property of the data structure that we want to pattern match on. We'll call this projection function `projectColor`.

```haskell
projectColor :: Text -> Map Text Color -> (Maybe Color, Map.Map Text Color)
projectColor name colorMap = case Map.lookup name colorMap of
  Nothing    -> (Nothing, colorMap)                          
  Just color -> (Just color, Map.delete (name, color) colorMap)
```

It takes a name of type `Text`, a `Map` from `Text` to `Color`, and returns either one of these tuples:

1. `Nothing`, and the original `Map` that was passed into the projection function
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
getsAlong person1 person2 (projectColor person1 -> (Just Yellow, projectColor person2 -> (Just _, _))) = True
getsAlong person1 person2 (projectColor person1 -> (Just Red, projectColor person2 -> (Just Yellow, _))) = True
getsAlong person1 person2 (projectColor person1 -> (Just Blue, projectColor person2 -> (Just Blue, _))) = True
getsAlong person1 person2 (projectColor person1 -> (Just Blue, projectColor person2 -> (Just Yellow, _))) = True
getsAlong _ _ = False
```

Notice how we can nest view patterns. In this case, we're looking for two people in a map to see if they would get along. We use a view pattern on the map passed into the function to see what color the first person likes. Then we use the same view pattern again on the new map returned by the first view pattern and 
see what color the second person likes. Remember how our projection function `projectColor` deletes an entry in the map if it was found? This is where it comes in handy: when we want to project different values out of a data structure multiple times.

## Conclusion

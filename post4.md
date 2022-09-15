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

## Conclusion

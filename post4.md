---
title: Type Safe Named Routes Using Patterns: Part III
image: /logo-bg.png
summary: A blog post about type safe named routes in Okapi
---

# Type Safe Named Routes Using Patterns: Part III

In Part I, we pondered the motivation for type safe named routes and how they work in other frameworks like Yesod and Servant.
Now we can talk about how type safe named routes are implemented in Okapi.

In Part II, we covered pattern synonyms and view patterns, and how we can use them to implement type safe named routes in Okapi.

In the final post of this series, we'll look at how to handle more complicated cases where we need to dispatch
on other request information besides the path.

## Safe Redirects

First we have to define the parser to pass to the `route` function:

```haskell
methodAndPath :: MonadOkapi m => m (Method, Path)
methodAndPath = do
  m <- method
  p <- path
  return (m, p)
```

It's a parser that returns a tuple of `Method` and `Path`. Now, we need to pass the `route` function a lambda that matches on values of type `(Method, Path)` and returns the correct handler. Let's use pattern synonyms again. To help match on the `Method`, Okapi exports pattern synonyms for each of the request methods, like `GET` and `POST`:

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Okapi

data Todo = Todo
  { todoID :: Int
  , todoText :: Text
  }
  
newtype TodoForm = TodoForm { todoFormText :: Text }

pattern GetTodos = (GET, ["todos"])

pattern GetTodo tID = (GET, ["todos", PathParam tID])

pattern PostTodo = (POST, ["todos"])

pattern PatchTodo tID = (PATCH, ["todos", PathParam tID])

pattern DeleteTodo tID = (DELETE, ["todos", PathParam tID])

methodAndPath :: MonadOkapi m => m (Method, Path)
methodAndPath = do
  m <- method
  p <- path
  return (m, p)
  
renderRedirect :: (Method, Path) -> Text
renderRedirect (_, p) = renderPath p

renderFormAction :: (Method, Path) -> Text
renderFormAction (m, p) = undefined

main :: IO ()
main = run id $ route methodAndPath $ \case
  GetTodos -> do
    todos <- liftIO $ queryAllTodos
    return $ setJSON todos $ ok
  GetTodo tID -> do
    maybeTodo <- liftIO $ queryTodo tID
    case maybeTodo of
      Nothing   -> return notFound
      Just todo -> return $ setJSON todo $ ok
  PostTodo -> do
    todoForm <- bodyForm @TodoForm
    maybeNewTodo <- liftIO $ insertTodoForm todoForm
    case maybeNewTodo of
      Nothing      -> return notFound
      Just newTodo -> return $ setJSON newTodo $ ok
  PatchTodo tID -> do
    todoForm <- bodyForm @TodoForm
    maybePatchedTodo <- liftIO $ updateTodo tID todoForm
    case maybePatchedTodo of
      Nothing      -> return notFound
      Just patchedTodo -> return $ setJSON patchedTodo $ ok
  DeleteTodo tID -> do
    maybeDeleted <- liftIO $ deleteTodo tID
    case maybeDeleted of
      Nothing -> return notFound
      Just _  -> redirect $ renderRedirect GetTodos
  _ -> next
```

## Conclusion

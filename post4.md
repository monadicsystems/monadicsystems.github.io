---
title: Named Routes in Okapi: Part III
image: /logo-bg.png
summary: A blog post about type safe named routes in Okapi
---

# Named Routes in Okapi: Part III

In Part I we pondered the motivation for named routes, and how they work in other frameworks like Yesod and Servant.

In Part II we covered pattern synonyms, and how we can use them to implement named routes in Okapi.

In the final post of this series, we'll look at how to handle more complicated cases where we need to dispatch
on other request information besides the path.

## Safe Form Attributes

At the end of Part II, we left off with this interesting thought.

> Imagine we had an online car dealership with two types of forms. One for querying the available cars for sale,
> and another for submitting a car you'd like to sell. The HTML for the two forms would look like this.
>
> ```html
> <form action="/cars" method="get">
>  ...
> </form>
>
> <form action="/cars" method="post">
>   ...
> </form>
> ```
>
> If we could pattern match on the request method and request path, we could safely generate form attributes for our HTML forms too.
> Just like how we safely generated the URLs for our redirects in the previous examples.
>
> ```html
> <form {renderFormAttrs QueryCarsRoute}>
>   ...
> </form>
>
> <form {renderFormAttrs PostCarsRoute}>
>   ...
> </form>
> ```

To implement this, let's model what a car is in our application and declare pattern synonyms for the routes.
To help with pattern matching on the request method Okapi exports pattern synonyms for all request methods, like `GET` and `POST`.

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Okapi
import ... -- other various imports

data Make = Toyota | Ford | Honda | Mercedes | BMW
  deriving (Eq, Show)

instance ToHttpApiData Make where
  toQueryParam = \case
    Toyota -> "toyota"
    Ford -> "ford"
    Honda -> "honda"
    Mercedes -> "mercedes"
    BMW -> "bmw"

instance FromHttpApiData Make where
  parseQueryParam = \case
    "toyota"   -> Right Toyota
    "ford"     -> Right Ford
    "honda"    -> Right Honda
    "mercedes" -> Right Mercedes
    "bmw"      -> Right BMW
    _          -> Left "Couldn't parse car make"

data Car = Car
  { carMake  :: Make
  , carYear  :: Int
  , carMiles :: Int
  , carPrice :: Float
  } deriving (Eq, Show, Generic, FromForm)

pattern HomeRoute = (GET, [])

pattern QueryCarsRoute = (GET, ["cars"])

pattern PostCarsRoute = (POST, ["cars"])

pattern PostSuccessRoute = (GET, ["cars", "post", "success"])

pattern PostFailureRoute = (GET, ["cars", "post", "failure"])
```

Now that we've defined our data types, instances, and route patterns, let's implement our rendering functions. Now, we will need two rendering functions. One for generating URLs and one for generating HTML form attributes.

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

> Hmmmm, I thought the appeal of named routes was that we remove the possibility of developer error.
> In this case, we're implementing these rendering functions ourselves.
> Couldn't the developer make a mistake here that isn't caught by the compiler?
> What if the string concatenation in the implementation of `renderFormAttrs` isn't correct?

This is true, the developer can make mistakes when implementing custom rendering functions and they may not be caught by the compiler.
Still, I believe this is better than plain string concatenation because it's much easier to test and find potential bugs in the implementation.
We can use basic unit tests to see that the rendering functions generate the correct string.
It's a lot harder to test if we have string concatenation directly in the HTML template.
We can only manually test the web application, or look at the generated HTML to see if the strings were concatenated correctly.

Also, consider the case where we have to use these generated URLs and form actions in multiple places.
With the custom rendering functions, we just have to implement them once, test them if needed, and be done with it. If we do find bugs in the URLs/form attributes in the future, we just have to reimplement the rendering function that's causing it.
If we were to use string concatenation directly in our HTML template, in multiple places, we'd risk introducing a bug each time we need to generate URLs/form attributes. We'd also have to update the same code in multiple places if we were to make any change to our URL structure. This is not good.

Now that we have our route patterns and rendering functions, let's implement the parser and dispatcher functions that we're going to pass to `route`.
Remember, the type of the `route` function is `MonadOkapi m => m a -> (a -> Handler m) -> Handler m` where the first argument is the parser and the second
argument is the dispatcher.

For the parser we want to return the request method and request path, because that's what we've defined our pattern synonyms to match on:
values of type `(Method, Path)`.

```haskell
methodAndPathParser :: MonadOkapi m => m (Method, Path)
methodAndPathParser = do
  m <- method
  p <- path
  return (m, p)
```

Our dispatcher will have 6 routes. 5 for each pattern we defined earlier, and 1 for when the incoming request doesn't match any of the patterns.

1. HomeRoute

   This is where the user will find the two forms. One for querying current cars on sale, and one for putting a car up for sale.

2. QueryCarsRoute

   This is the endpoint that accepts the form to query current cars on sale, and returns a table of cars that match the user's criteria.
   
3. PostCarsRoute

   This is the endpoint that accepts the form to put a car up for sale.
   Depending on that validity of the form, the user is redirected to a success or failure page.

4. PostSuccessRoute

   The user is redirected to this page if the post form is submitted succesfully.

5. PostFailureRoute

   The user is redirected to this page if the contents of the post form are invalid.
   
6. _

   The dispatcher fails with `next`, and the user gets a default 404 response if the request doesn't match any of our patterns.

Brilliant. Let's define the dispatcher function using `-XLambdaCase`, and feed that in to the `route` function to bring everything together.

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Okapi
import ... -- other various imports

data Make = Toyota | Ford | Honda | Mercedes | BMW
  deriving (Eq, Show)

instance ToHttpApiData Make where
  toQueryParam = \case
    Toyota -> "toyota"
    Ford -> "ford"
    Honda -> "honda"
    Mercedes -> "mercedes"
    BMW -> "bmw"

instance FromHttpApiData Make where
  parseQueryParam = \case
    "toyota"   -> Right Toyota
    "ford"     -> Right Ford
    "honda"    -> Right Honda
    "mercedes" -> Right Mercedes
    "bmw"      -> Right BMW
    _          -> Left "Couldn't parse car make"

data Car = Car
  { carMake  :: Make
  , carYear  :: Int
  , carMiles :: Int
  , carPrice :: Float
  } deriving (Eq, Show, Generic, FromForm)

pattern HomeRoute = (GET, [])

pattern QueryCarsRoute = (GET, ["cars"])

pattern PostCarsRoute = (POST, ["cars"])

pattern PostSuccessRoute = (GET, ["cars", "post", "success"])

pattern PostFailureRoute = (GET, ["cars", "post", "failure"])

renderURL :: (Method, Path) -> Text
renderURL (_, p) = renderPath p

renderFormAttrs :: (Method, Path) -> Text
renderFormAttrs (m, p) = renderAction p <> " " <> renderMethod m
  where
    renderAction p = "action=\"" <> renderPath p <> "\""
    renderMethod = \case
      POST -> "method=\"" <> "post" <> "\""
      _    -> "method=\"" <> "get" <> "\"" -- ^ method="get" is the default method for forms

methodAndPathParser :: MonadOkapi m => m (Method, Path)
methodAndPathParser = do
  m <- method
  p <- path
  return (m, p)

methodAndPathDispatcher :: (MonadOkapi m, MonadIO m) => IORef [Car] -> (Method, Path) -> m Response
methodAndPathDispatcher database = \case
  HomeRoute -> do
    let html =
          [qq|
            <h1>Welcome to the online car dealership!</h1>
            <hr>
            <h2>Query Cars</h2>
            <form {renderFormAttrs QueryCarsRoute}>
              <label for="make">Car Make: </label>
              <select name="make" id="make" multiple>
                <option value={toQueryParam Toyota}>Toyota</option>
                <option value={toQueryParam Ford}>Ford</option>
                <option value={toQueryParam Honda}>Honda</option>
                <option value={toQueryParam Mercedes}>Mercedes</option>
                <option value={toQueryParam BMW}>BMW</option>
              </select>
              <br>
              <label for="year">Car Latest Year: </label>
              <input type="range" id="year" name="year" min="1985" step="1" max="2022" value="2022" oninput="this.nextElementSibling.value = this.value">
              <output>2022</output>
              <br>
              <label for="miles">Car Max Miles: </label>
              <input type="range" id="miles" name="miles" min="0" step="50000" max="500000" value="500000" oninput="this.nextElementSibling.value = this.value">
              <output>500000</output>
              <br>
              <label for="price">Car Max Price: </label>
              <input type="range" id="price" name="price" min="0" step="1000" max="200000" value="200000" oninput="this.nextElementSibling.value = this.value">
              <output>200000</output>
              <br>
              <input type="submit" value="Submit">
            </form>
            <hr>
            <h2>Put Your Car Up For Sale</h2>
            <form {renderFormAttrs PostCarsRoute}>
              <label for="carMake">Car Make: </label>
              <select name="carMake" id="carMake">
                <option value={toQueryParam Toyota}>Toyota</option>
                <option value={toQueryParam Ford}>Ford</option>
                <option value={toQueryParam Honda}>Honda</option>
                <option value={toQueryParam Mercedes}>Mercedes</option>
                <option value={toQueryParam BMW}>BMW</option>
              </select>
              <br>
              <label for="carYear">Car Year: </label>
              <select name="carYear" id="carYear">
                {Data.ByteString.concat $ Prelude.map makeYearOption [1985..2022]}
              </select>
              <br>
              <label for="carMiles">Car Miles: </label>
              <input type="range" id="carMiles" name="carMiles" min="0" step="50000" max="500000" value="200000" oninput="this.nextElementSibling.value = this.value">
              <output>200000</output>
              <br>
              <label for="carPrice">Car Price: </label>
              <input type="range" id="carPrice" name="carPrice" min="0" step="1000" max="200000" value="20000" oninput="this.nextElementSibling.value = this.value">
              <output>20000</output>
              <br>
              <input type="submit" value="Submit">
            </form>
          |]
    return $ setHTML html $ ok
  QueryCarsRoute -> do
    maybeMakes <- optional $ queryList @Make "make"
    latestYear <- queryParam @Int "year"
    maxMiles <- queryParam @Int "miles"
    maxPrice <- queryParam @Float "price"

    carsThatMatchQuery <- liftIO $ do
      let makes = case maybeMakes of
            Nothing -> []
            Just (m :| ms) -> m : ms
      availableCars <- readIORef database
      return $ filterCars makes maxMiles maxPrice availableCars

    let html =
          if Prelude.null carsThatMatchQuery
            then
              [qq|
                <h1>No results match your query.</h1>
                <a href="{renderURL HomeRoute}">Go back</a>
              |]
            else
              [qq|
                <table>
                  <tr>
                    <th>Make</th>
                    <th>Year</th>
                    <th>Miles</th>
                    <th>Price</th>
                  </tr>
                  {Data.ByteString.concat $ Prelude.map makeCarTableRow carsThatMatchQuery}
                </table>
                <a href="{renderURL HomeRoute}">Go back</a>
              |]

    return $ setHTML html $ ok
  PostCarsRoute -> do
    maybeCarForSale <- optional $ bodyForm @Car
    case maybeCarForSale of
      Nothing -> return $ redirect 302 $ renderURL PostFailureRoute 
      Just carForSale -> do
        liftIO $ modifyIORef database (carForSale :)
        return $ redirect 302 $ renderURL PostSuccessRoute
  PostSuccessRoute -> do
    let html =
          [qq|
            <h1>
              Your car is now up for sale!
            </h1>
            <a href="{renderURL HomeRoute}">Go back</a>
          |]
    return $ setHTML html $ ok
  PostFailureRoute -> do
    let html =
          [qq|
            <h1>
              We can't put your car up for sale.
              Make sure you entered valid data.
            </h1>
            <a href="{renderURL HomeRoute}">Go back</a>
          |]
    return $ setHTML html $ ok
  _ -> Okapi.next

main :: IO ()
main = do
  database <- newIORef []
  run id $ route methodAndPathParser $ methodAndPathDispatcher database
  
-- HELPERS BELOW

makeYearOption :: Int -> ByteString
makeYearOption year = [qq|<option value={toQueryParam year}>{toQueryParam year}</option>|]

makeCarTableRow :: Car -> ByteString
makeCarTableRow Car{..} =
  [qq|
    <tr>
      <td>{carMake}</td>
      <td>{show carYear}</td>
      <td>{show carMiles}</td>
      <td>${show carPrice}</td>
    </tr>
  |]

filterCars :: [Make] -> Int -> Float -> [Car] -> [Car]
filterCars makes maxMiles maxPrice cars =
  [ car
  | car <- cars
  , carMiles car <= maxMiles
  , carPrice car <= maxPrice
  , if Prelude.null makes then True else carMake car `Prelude.elem` makes
  ]
```

For the HTML templating in this example I'm using the `interpolatedstring-perl6` package.
This package makes it really easy to interpolate Haskell expressions in multiline strings.

For the "database" we're using an `IORef [Car]` that's created in `main` and passed to our `methodAndPathDispatcher` function.

## Conclusion

This is how named routes are done in Okapi. I like using patterns for named routes in Okapi because it's more flexible, easier to understand, and doesn't take as long to compile compared to using Template Haskell and type-level programming. Although Okapi's named routes don't provide the same level of guarantees that Servant's named routes do, I think they are good enough for most use cases and are more flexible than Servant's named routes.

I'm still in the process of seeing what else can be done with patterns. We can actually pattern match on more than just the request path and request method.
With view patterns we can also pattern match on the request query, request headers, and request body, and even perform validation on these parts of the request! Right now Okapi's `Request` data type doesn't provide request information like the request host and request port, but if it did it'd be possible to also match
on those request properties just like anything else. This would be useful for virtual hosting. This may be added in the future.

If you found this series interesting, checkout Okapi's [documentation](https://www.okapi.wiki/) for more information on how to use it in your own projects.
Everything is still very much a work in progress. If you have any ideas, concerns, or criticisms to contribute feel free to [create an issue](https://github.com/monadicsystems/okapi/issues).

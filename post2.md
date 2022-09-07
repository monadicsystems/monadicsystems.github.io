---
title: Type Safe Named Routes Using Bidrectional Patterns
author: Rashad Gover
image: /logo-bg.png
summary: A blog post about how type-safe named routes are implemented in Okapi
render_with_liquid: false
---

# Type Safe Named Routes Using Bidirectional Patterns

A while back, I made a [post on reddit](https://www.reddit.com/r/haskell/comments/trzi5u/new_serverside_framework_based_on_monadic_parsing/)
introducing the [Okapi web framework]() to the Haskell community. There was good discussion to be had and I got lots of useful, constructive feedback.
There was one comment on the post by u/n00bomb that interested me:

> How to do "Named Routes"?

At the time, my response was:

> Hmmm, I'm not sure what you mean by "Named Routes"? Are you referring to Servant named routes?
> In that case, I'm not sure that applies to Okapi. Can you give a pseudo-code example of that?
> 
> Edit: Ah I see what you mean. As it is right now, named routes aren't available in Okapi, but they would be nice to have in the future.
> I'm trying to think of a mechanism that I can use to implement them...I'll get back to you when I figure it out.

I'm here today to report back on the solution I've chosen to solve this problem in Okapi.

## Routes and Handlers

**Routes** are definitions that a web framework uses to match incoming requests with the correct handlers.
**Handlers** are functions that perform actions and determine what response is returned to the client.
Frameworks use various language features for defining routes and their handlers.

For example, Python's Flask uses function decorators to define routes and the function definitions they're applied to as handlers:

```python
@app.route('/product/<name>')
def get_product(name):
  return "The product is " + str(name)
```

PHP's Laravel uses the `Route` class and its methods to define routes, and callback functions to define handlers:

```php
Route::get('/product/{name}', function ($name) {
    return 'The product is '.$name;
});
```

Okapi uses a special monad to define both routes and handlers:

```haskell
newtype Name a = Name { unName :: Text }

getProduct = do
  methodGET
  pathParam @Text `equals` "product"
  (Name name) <- pathParam @(Name Product) -- This could just be Text, but we're using Haskell so...
  pathEnd
  ok
    & setPlaintext ("The product is " <> name)
    & return
```

An interesting thing to note about Okapi is its lack of distinction between *route* and *handler*. If the developer wants, they can
separate the two constructs like so:

```haskell
getProduct = getProductRoute >>= getProductHandler

getProductRoute = do
  methodGET
  pathParam @Text `equals` "product"
  name <- pathParam @(Name Product)
  pathEnd
  return name
  
getProductHandler (Name name) = ok & setPlaintext ("The product is " <> name) & return
```

## Named Routes

Now that we briefly covered what routes and handlers are, let's uncover what u/n00bomb was talking about when they mentioned *named routes*.
I found the description of named routes in [this Laravel tutorial](https://www.javatpoint.com/named-routes-in-laravel#:~:text=Named%20routes%20is%20an%20important,a%20nickname%20to%20the%20route.) to be a good summary of the concept:

> Named routes is an important feature in the Laravel framework.
> It allows you to refer to the routes when generating URLs or redirects to the specific routes.
> In short, we can say that the naming route is the way of providing a nickname to the route.

To get an idea for why named routes are beneficial, let's imagine we are web devlopers for a local pet store:
 
- We have a handler for the route `/petstore/reptile/snake` that returns a page of all our snakes on sale
- We have a special sale for snakes this week, so the manager wants a hyperlink to the `/petstore/reptile/snake` page on our homepage
- We add the hyperlink `<a href="/petstore/reptile/snek">CUTE SNAKES FOR SALE</a>` to our homepage HTML template
- We deploy the new version of the website and wait for the money from selling large amounts of pet snakes to roll in

Unfortunately, we find out next week that the hyperlink we added was misspelled and snake sales were lower than expected.
The manager of the pet store isn't happy.

This is where named routes come in. They push the burden of making sure URLs are spelled correctly on to the computer. We do this by assigning
our route definitions to identifiers that can be used in our HTML templates or redirects. When we refer to the identifier in our code, the correct URL for
the route it was assigned is automatically generated. If we misspell the named route's identifier, the computer will be able let us know with an error like
`Couldn't find variable misspelledVariableName`.

Here's an example of named routes in Laravel:

```php
Route::get('/petstore/reptile/snake', function () {
    // handler logic
})->name('snakesforsale');
```

Now we can use the `snakesforsale` identifier in our templates (Laravel uses Blade templates) like so:

```html
...
<a href="{{ route('snakesforsale') }}">CUTE SNAKES FOR SALE</a>
...
```

Named routes are also nice becasuse we can update the definition of a route without updating all of the places in the codebase where we refer to that route.
The definition may change, but as long as the route's identifier is the same we can rest assured that we have no broken links on our website (internal facing ones at least).

## Type Safe Named Routes

Even better than named routes are *type safe named routes*.

Type-safe URLs (or URIs) are URLs that can be constructed safely because they contain type information. Constructing URLs with string concatenation is error prone because the dynamic parts of the URL aren’t specifically typed, they are all of type String, and they are more prone to human error because typos within the String aren’t caught by the compiler. Let’s say our server handles the endpoint `/todo/:id`, where `:id` is a path parameter representing a todo ID number. If we want to link to the `/todo/:id` endpoint from one of our pages we need to construct the URL and add it to our HTML. With a plain string concatenation method, we could use a function like


```
renderTodoIDURL :: Text -> Text
renderTodoIDURL id = “/todo/” <> id
```

Which would be bad because we would be free to do something that doesn’t make sense, like `renderTodoIDURL “dog”` or `renderTodoIDURL “onehundred”`. To fix this we could do something like

```
renderTodoIDURL :: Int -> Text
renderTodoIDURL id = “/todo/” <> show id
```
Where the Text is replaced with an Int. Now we remove the possibility of doing anything like `renderTodoIDURL “abc”`. Even better would be replacing the Int with the newtype TodoID or Natural.

We’ve made the URL generation function more type safe which is great, but what happens if we update our handler to handle the endpoint `/task/:id` instead of `/todo/:id`? Well in order for our URL generation to work properly we would have to update the function `renderTodoIDURL` to

```
renderTodoIDURL :: Int -> Text -- We should change the name too, but let’s skip that for now
renderTodoIDURL id = “/task/” <> show id
```

Notice how if we change our handler, we have to make a change to the URL rendering function too. We have to make changes in at least two places. This is another downside to this method. If the handler and URL rendering function are defined close to each other this might not be bad, but we’d have to rely on developers to keep them close. Another downside is that we get no help from the compiler if we make this change. We could’ve updated the handler and forgot to update the URL rendering function, and deployed our app!
How Haskell Web Frameworks solve this issue

Various Haskell web frameworks solve this issue to various degrees using various methods. The two main language features that Haskell web frameworks use to solve the type-safe URL problem are:

Type-level programming
Template Haskell

How Okapi solves the problem

Okapi proposes using an underused language feature for solving this problem: bidrectional patterns.

Bidirectional patterns are patterns that can be used to deconstruct and construct data in Haskell.

Okapi provides a function called `route :: (Path -> Handler m) -> Handler m` that is meant to be used with the LambdaCase language extension to match a path to a `Handler m`. An example of it being used:

```
myServer = route $ \case
  TodoRoute -> …
  TodoByIDRoute todoID -> …
  _ -> skip
```

The same patterns that we use to pattern match on the path can be used to construct URLs as well.

Since the pattern is the deconstructor and constructor, if we update the pattern we’re updating the handler and the URL generator at the same time.

If the patterns are implicitly bidirectional, you only have to make a change in one place to update the deconstructor and constructor. Nice!

There are cases where you might have to make a pattern explicitly bidirectional. This means the deconstructor and constructor have different definitions, but still the same type. Updates to one may require a change in the other, so you will updating two places at most when you make a change like with our original handler and URL generation function. The good thing is that they are defined right next to each other and their placement in the source code is enforced by the compiler. Type mismatches will be caught by the compiler too. Value mismatches can slip through though if the constructor and deconstructor have the same type.

Dispatching on other parts of the Request

By default, Okapi exposes a route function that takes a function that dispatches the correct handler based on the path. Users of Okapi can create a custom route function that allows them to dispatch on any part of the request, like the method, query parameters, and/or headers. For example, let’s create a route function that matches against the request method, path, and query parameters.

Another upside to this approach to the type-safe URL problem is that it is extensible unlike other approaches. Most frameworks only allow matching on the request path. This all you need most of the time, but if you need the extra functionality for whatever reason, Okapi has it. Depending on the needs of the developer, they can dispatch on the different parts of the request they care about, and create patterns for them.




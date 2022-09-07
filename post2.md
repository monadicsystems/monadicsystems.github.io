---
title: Type Safe Named Routes Using Bidrectional Patterns
image: /logo-bg.png
summary: A blog post about how type-safe named routes are implemented in Okapi
---

# Type Safe Named Routes Using Bidirectional Patterns

A while back, I made a [post on reddit](https://www.reddit.com/r/haskell/comments/trzi5u/new_serverside_framework_based_on_monadic_parsing/)
introducing the [Okapi web framework]() to the Haskell community. There was good discussion to be had and I got lots of useful, constructive feedback.
There was one comment on the post by u/n00bomb that caught my attention:

> How to do "Named Routes"?

At the time, my response was:

> Hmmm, I'm not sure what you mean by "Named Routes"? Are you referring to Servant named routes?
> In that case, I'm not sure that applies to Okapi. Can you give a pseudo-code example of that?
> 
> Edit: Ah I see what you mean. As it is right now, named routes aren't available in Okapi, but they would be nice to have in the future.
> I'm trying to think of a mechanism that I can use to implement them...I'll get back to you when I figure it out.

I'm here today to report back on a solution I found to solve this problem in Okapi.

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

An interesting thing to note about Okapi is its lack of distinction between route and handler. If the developer wants, they can
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

Now that we briefly covered what routes and handlers are, let's uncover what u/n00bomb was talking about when they mentioned **named routes**.
I found this description of named routes in [this Laravel tutorial](https://www.javatpoint.com/named-routes-in-laravel#:~:text=Named%20routes%20is%20an%20important,a%20nickname%20to%20the%20route.) to be a good summary of the concept:

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
{% raw %}<a href="{{ route('snakesforsale') }}">CUTE SNAKES FOR SALE</a>{% endraw %}
...
```

Named routes are also useful becasuse we can update the definition of a route without updating all of the places in the codebase where we refer to that route.
The definition may change, but as long as the route's identifier is the same we can rest assured that we have no broken links on our website (internal facing ones at least).

## Type Safe Named Routes

In the examples up until this point, we've looked at simple routes that didn't have any path parameters, like `/petstore/reptile/snake`.
The question now is, how do we use named routes to handle URLs with path parameters? For example, let's say we need to implement a router for URLs of the form `/user/<uid>/profile` where `<uid>` is a path parameter representing a user's unique identifier. In Laravel, we could define our route for such a URL as follows:

```php
Route::get('/user/{uid}/profile', function ($uid) {
    // handler logic
})->name('profile');
```

To generate the URL for this named route, we use the `route` function just as we used it in the previous example, except now we also need to pass the function a map representing the URLs parameters:

```php
$linkToUser1Profile = route('profile', ['uid' => 1]);
```

There a couple of issues with this though:

- We can't guarantee that `uid` is an integer.
  What if the developer types `route('profile', ['uid' => 'lol'])` or `route('profile', ['uid' => 9.9])`?
  The developer won't realize their mistake until the URL is tested or deployed and found by a user.

- The developer can pass in extra parameters or not enough parameters, e.g. `route('profile', ['uid' => 1, 'foo' => 'bar', 'baz' => 420])`.
  In Laravel, this generates the URL `/user/1/profile?foo=bar&baz=420`. This may cause some problems down the road.

Some web frameworks in statically typed languages offer a solution to these issues that we'll call **type safe named routes**.
Type safe named routes are similar to named routes, except they offer an extra layer of safety that catches developer errors at the best time: compile time.

Although you can find implementations of type safe named routes in other statically typed languages, I'm most familiar with how web frameworks in Haskell do it.
For the most part, Haskell web frameworks utilize one of these two techniques to implement type safe named routes:

1. Metaprogramming via Template Haskell e.g. [Yesod](), [wai-routes](), and [Happstack]() via the [web-routes-boomerang package]()
2. Type-level programming e.g. [Servant]() and [Spock]()

Okapi proposes a third technique: bidirectional patterns.

## Type Safe Named Routes in Okapi

Before we see examples of how type safe named routes work in Okapi, we must first understand two language extensions:
`-XPatternSynonyms` and `-XViewPatterns`.

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
First, we need to define what I call a **view function**. The view function is what we'll use in the view pattern to project a property of the data structure that we want to pattern match on. We'll call this view function `viewColor`:

```haskell
viewColor :: Text -> Map.Map Text Color -> (Maybe Color, Map.Map Text Color)
viewColor name colorMap = case Map.lookup name colorMap of
  Nothing    -> (Nothing, colorMap)
  Just color -> (Just color, Map.delete (name, color) colorMap)
```

It takes a name of type `Text`, a `Map` from `Text` to `Color`, and returns either one of these tuples:

1. A `Color` value wrapped with `Just` if it's found in the `Map`, and a new `Map` with the entry that was found deleted from it
2. `Nothing`, and the original `Map` that was passed into the view function

The return type of the view function is important because that's what we'll be pattern matching on in our view pattern.

Now let's use our view function in a view pattern.

```haskell  
favoriteColorIsBlue :: Text -> Map.Map Text Color -> Bool
favoriteColorIsBlue name (viewColor name -> (Just Blue, _)) = True
favoriteColorIsBlue _ _ = False
```

You'll notice on the first line of our function declaration, in place of the second parameter, there's a call to our `viewColor name` function followed by a `->`. That's a view pattern. We're applying our view function `viewColor name` to the argument and pattern matching on the result tuple by using `->`. Note how function currying is being used to our advantadge here. The type of `viewColor` is `Text -> Map Text Color -> (Maybe Color, Map Text Color)`, but the type of `viewColor name` is `Map Text Color -> (Maybe Color, Map Text Color)` which allows us to use it in this view pattern. Also note how we can pass in any variables on the RHS into the view function that's used in our view pattern. The pattern after the `->` is for matching on the tuple result of the view function. In this case we just want to match on `Just Blue` and ignore the new `Map Text Color` value.

You may be wondering, "Why put all that logic into a pattern and not just use a regular function?". Well, this is a contreived example so I agree with you.
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

### How Okapi Does It

In Okapi, type safe named routes are implemented using bidrec

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

## Conclusion




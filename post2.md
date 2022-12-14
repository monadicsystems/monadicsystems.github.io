---
title: Named Routes in Okapi: Part I
image: /logo-bg.png
summary: A blog post about type safe named routes in Okapi
---

# Named Routes in Okapi: Part I

A while back, I made a [post on Reddit](https://www.reddit.com/r/haskell/comments/trzi5u/new_serverside_framework_based_on_monadic_parsing/)
introducing [Okapi](https://www.okapi.wiki/) to the Haskell community. There was good discussion in the thread and I got lots of useful, constructive feedback.
There was one comment on the post by u/n00bomb that caught my attention:

> How to do "Named Routes"?

At the time, my response was:

> Hmmm, I'm not sure what you mean by "Named Routes"? Are you referring to Servant named routes?
> In that case, I'm not sure that applies to Okapi. Can you give a pseudo-code example of that?
> 
> Edit: Ah I see what you mean. As it is right now, named routes aren't available in Okapi, but they would be nice to have in the future.
> I'm trying to think of a mechanism that I can use to implement them...I'll get back to you when I figure it out.

I think I've found a solution that I like so I'll be covering it from motivation, to implementation, to practical use cases in a series of three essays:

1. Part I (the one you're currently reading)

   I cover the motivation for named routes and briefly give an idea of how they're implemented in some other web frameworks.

2. [Part II](/post3)

   I go over the `-XPatternSynonyms` language extension and how it's used to implement type safe named routes in Okapi.

3. [Part III](/post4)

   An example of a simple web application built using Okapi's named routes.

Enjoy!

## Routes and Handlers

Generally speaking, **routes** are definitions that a web framework uses to match incoming requests with the correct handlers.
**Handlers** are functions that perform actions and determine what response is returned to the client.
Frameworks use various language features for defining routes and their handlers.

For example, Python's Flask uses function decorators to define routes, and the function definitions they're applied to as handlers.

```python
@app.route('/product/<name>')
def get_product(name):
  return "The product is " + str(name)
```

PHP's Laravel uses the `Route` class and its methods to define routes, and callback functions to define handlers.

```php
Route::get('/product/{name}', function ($name) {
    return 'The product is '.$name;
});
```

Okapi uses a special monad to define both routes and handlers.

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

An interesting thing to note about Okapi is its lack of distinction between route and handler. If the developer wants, they can still
separate the two constructs.

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

To get an idea for why named routes are beneficial, let's imagine we are web developers for a local pet store:
 
- We have a handler for the route `/petstore/reptile/snake` that returns a page of all our snakes on sale
- We have a special sale for snakes this week, so our manager wants a hyperlink to the `/petstore/reptile/snake` page on our homepage
- We add the hyperlink `<a href="/petstore/reptile/snek">SNAKES FOR SALE</a>` to our homepage HTML template
- We deploy the new version of the website and wait for the money from selling large amounts of pet snakes to roll in

Unfortunately, we find out next week that the hyperlink we added was misspelled and snake sales were lower than expected.
The manager of the pet store isn't happy.

This is where named routes come in. They push the burden of making sure URLs are spelled correctly on to the computer. We do this by assigning
our route definitions to identifiers that can be used in our HTML templates or redirect statements.
When we refer to the identifier in our code, the correct URL for the route it was assigned is automatically generated.
If we misspell the named route's identifier, the computer will be able let us know with an error like `Couldn't find variable misspelledVariableName`.

Here's an example of a named route in Laravel.

```php
Route::get('/petstore/reptile/snake', function () {
    // handler logic
})->name('snakesforsale');
```

Now we can use the `snakesforsale` identifier in our templates.

```html
...
{% raw %}<a href="{{ route('snakesforsale') }}">CUTE SNAKES FOR SALE</a>{% endraw %}
...
```

Named routes are also useful becasuse we can update the definition of a route without updating all of the places in the codebase where we refer to that route.
The definition may change, but as long as the route's identifier is the same we can rest assured that we have no broken links on our website (internal facing ones at least).

## Type Safe Named Routes

In the examples up until this point, we've looked at simple routes that didn't have any path parameters, like `/petstore/reptile/snake`.
The question now is, how do we use named routes to handle URLs with path parameters? For example, let's say we need to implement a router for URLs of the form `/user/<uid>/profile` where `<uid>` is a path parameter representing a user's unique identifier. In Laravel, we can define the route for such a URL.

```php
Route::get('/user/{uid}/profile', function ($uid) {
    // handler logic
})->name('profile');
```

To generate the URL for this named route, we use the `route` function just as we used it in the previous example, except now we also need to pass the function a map representing the URLs parameters.

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

Although you can find implementations of type safe named routes in many statically typed languages, I'm most familiar with them from my experience with Haskell web frameworks like Yesod and Servant. From my knowledge, Haskell web frameworks utilize one of these two techniques to implement type safe named routes:

1. Metaprogramming via Template Haskell e.g. [Yesod](https://www.yesodweb.com/blog/2010/05/really-type-safe-urls), [wai-routes](https://hackage.haskell.org/package/wai-routes), and [Happstack via the web-routes-boomerang package](https://www.happstack.com/docs/crashcourse/index.html#web-routes-boomerang)
2. Type-level programming e.g. [Servant](https://kseo.github.io/posts/2017-01-20-how-servant%27s-type-safe-links-work.html) and [Spock](https://www.spock.li/2015/04/19/type-safe_routing.html)

I recommend checking out the links for each framework listed above if you're unfamiliar with how they work, and want to learn more about how named routes are implemented in each respective framework.
I won't cover these frameworks myself in this essay for fear of missrepresenting them. Besides, the documentation and blog posts I listed above already do a great job.

If you're familiar with the web frameworks listed above, and Haskell in general, you know that Template Haskell and type-level programming
are considered to be some of Haskell's more complex features. On top of that, the use of Template Haskell and/or type-level programming increases
compile times by a noticeable amount. Especially on larger projects. This is undesireable because it decreases productivity, and web developers want a fast feedback loop when making constant changes to a web application.

Does Haskell have other simpler features that can be used to implement type safe named routes in Okapi?
After some experimentation, I've come to the conclusion that **pattern synonyms** may be the answer.

[To be continued...](/post3)

---
title: Type Safe Named Routes Using Patterns: Part IV
image: /logo-bg.png
summary: A blog post about type safe named routes in Okapi
---

# Type Safe Named Routes Using Patterns: Part IV

In Part II we used type safe named routes in Okapi to generate redirects that were guaranteed to take the the user to a valid location on the server.

In Part III we used type safe named routes in Okapi to generate redirects and form actions too. 

In Part IV we're going to implement even more complicated type safe named routes that can generate redirects with query parameters and form actions.

The question is, how do we pattern match on a request query that is a list of key-value pairs? The answer to this is the `-XViewPatterns` language
extension.

## -XViewPatterns

On top of `-XPatternSynonyms`, we can make patterns even more powerful in Haskell using the `-XViewPatterns` language extenstion.
When this language extension is turned on we can pattern match on the *projection of a value* and not just the value itself.
This is useful for pattern matching on data structures, like a map.

Suppose we have a map representing the favorite colors of random people,

```haskell
data Color = Red | Blue | Yellow

favoriteColors :: [(Text, Color)]
favoriteColors = [("Bob", Blue), ("Alice", Yellow), ("Larry", Yellow)]
```

and we want to see if a specific person's favorite color is `Blue`. We could just use a traditional function, but let's try to solve this using view patterns.

First, we need to define what I call a **projection function**. The projection function is what we'll use in the view pattern to project a property of the data structure that we want to pattern match on. We'll call this projection function `projectColor`.

```haskell
projectColor :: Text -> [(Text, Color)] -> (Maybe Color, Map.Map Text Color)
projectColor name colorMap = case lookup name colorMap of
  Nothing    -> (Nothing, colorMap)                          
  Just color -> (Just color, delete (name, color) colorMap)
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

Notice how we can nest view patterns. In this case, we're looking for two people in a map to see if they would get along.
We use a view pattern on the map passed into the function to see what color the first person likes.
Then we use the same view pattern again on the new map returned by the first view pattern and see what color the second person likes.
Remember how our projection function `projectColor` deletes an entry in the map if it was found?
This is where it comes in handy: when we want to project different values out of a data structure multiple times.

## Type Safe Todo App



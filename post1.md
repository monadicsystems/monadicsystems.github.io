# Template Fragments with Lucid

Recently, the creator of [htmx]() has been conducting the [template fragments hype train]() and
calling for programmers to expose whether or not [template fragments](https://htmx.org/essays/template-fragments/)
are possible in their templating language of choice.
My templating language of choice is [lucid](); a monadic DSL for rendering HTML in Haskell.

## Lucid in a nutshell

Lucid provides us with an `Html` monad that we can use to sequence and nest HTML tags, like so:

```haskell
myHtml :: Html ()
myHtml = do
  h1_ [class_ "text-xl"] "Bio"
  div_ [class_ "info-box"] $ do
    h2_ [] "Name"
    p_ [] "Rashad"
    h2_ [] "Location"
    p_ [] "Earth"
    h2_ [] "Likes"
    ul_ [] $ do
      li_ [] "Haskell"
      li_ [] "htmx"
      li_ [] "The color green"
```

It's pretty straight forward.
You have a tag function, like `h1_`, that you apply to a list of attributes and an inner HTML value of type `Html ()`.
If two tags have the same indentation level in the same `do` block, they will be rendered as siblings.
If a tag is within the `do` block of the another tag's inner HTML value, it will be rendered as a child of the other tag.

The above isn't a template though, because everything is statically defined. We're just writing HTML using a fancy syntax.
Let's parameterize our `myHtml` value by turning it into a function:

```haskell
data Person = Person
  { name :: Text
  , location :: Text
  , likes :: [Text]
  }

personHtml :: Person -> Html ()
personHtml p = do
  h1_ [class_ "text-xl"] "Bio"
  div_ [class_ "info-box"] $ do
    h2_ [] "Name"
    p_ [] $ toHtml p.name
    h2_ [] "Location"
    p_ [] $ toHtml p.location
    h2_ [] "Likes"
    ul_ [] $ mapM_ (li_ [] . toHtml) p.likes -- mapM_ is like the map function, but it works in a monadic context
```

You probably noticed that in the templating function I'm using the `toHtml` function, but in the static version of the template I was able to use a string literal like `"The color green"` without using `toHtml`. This is because of how Haskell infers the types of string literals when the `OverloadedStrings` language extension is enabled. When the `OverloadedStrings` extension is on, GHC infers the string literal `"The color green"` to be of type `Html ()` automatically. GHC can't do this in the `personHtml` template function because the fields of the `Person` record are defined as being of type `Text`. We could define the `Person` type as `data Person = Person { name :: Html (), location :: Html (), likes :: [Html ()]}` and remove the need to use `toHtml` in our template, but that would be unweildy to other parts of the program that need to manipulate those fields. It's more practical if the fields are of type `Text`, and we convert them to `Html ()` when it's necessary.

Awesome! Now we have an HTML template that we can apply to any value of the type `Person`.

```haskell
myHtml :: Html ()
myHtml = personHtml $ Person
  { name = "Rashad"
  , location = "Earth"
  , likes = ["Haskell", "htmx", "The color green"]
  }

bobHtml :: Html ()
bobHtml = personHtml $ Person
  { name = "Bob"
  , location = "Antarctica"
  , likes = ["The blues", "A good hamburger", "Swimming"]
  }
```

```haskell
data Contact = Contact
  { id :: Int
  , email :: Text
  , archived :: Bool
  }

contactDetail :: Contact -> Html ()
contactDetail contact = do
  html_ [] $ do
    body_ [] $ do
      div_ [hxTarget_ "this"] $ do
        if contact.archived
          then do
            button_ [hxPatch_ "/contacts/" <> show contact.id <> "/unarchive"] "Unarchive"
          else do
            button_ [hxPatch_ "/contacts/" <> show contact.id] "Archive"
      h3_ [] "Contact"
      p_ [] $ toHtml contact.email
```

## Lucid templates are "data-driven"

## Lucid template fragments

## Conclusion

Template fragments are a natural occurence in Lucid. I encourage others to 


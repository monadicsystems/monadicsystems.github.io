---
title: Lucid Template Fragments
author: Rashad Gover
image: /logo.png
summary: A blog post about HTML template fragments in Haskell
---


# Lucid Template Fragments

Recently, the creator of [htmx](https://htmx.org/) has been conducting the [template fragments hype train](https://twitter.com/htmx_org/status/1565005004234186753?s=20&t=3NrFYdZUx0aPv_oxkSvq5Q) and
calling for programmers to expose whether or not template fragments
are possible in their templating language of choice.
If you're not familiar with what template fragments are, read this [essay](https://htmx.org/essays/template-fragments/).
In short, they are fragments of an HTML template that can be used on their own without creating new, individual files for them. 
My templating language of choice is [lucid](https://hackage.haskell.org/package/lucid): a monadic DSL for rendering HTML in Haskell.
Let's go over how lucid works and then see how we can apply the template fragments pattern with it.

## Lucid Crash Course

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
If two tags have the same indentation level in a `do` block, they will be rendered as siblings.
If a tag is within the `do` block of the another tag's inner HTML value, it will be rendered as a child of the other tag.

The above isn't a template though, because everything is statically defined. We're just writing HTML using a fancy syntax.
Let's parameterize our `myHtml` value by turning it into a function. We'll call the template function `personHtml`:

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

> **Note**
> 
> You probably noticed in the templating function that we're using the `toHtml` function, but in the static version of the template we were able to use a
> string literal like `"The color green"` without using `toHtml`. This is because of how Haskell infers the types of string literals when
> the `OverloadedStrings` language extension is enabled. When the `OverloadedStrings` extension is on, GHC (the standard Haskell compiler) infers
> the string literal `"The color green"` to be of type `Html ()` automatically. GHC can't do this in the `personHtml` template function because the fields of
> the `Person` record are defined as being of type `Text`. We could define the `Person` type as
> 
> ```haskell
> data Person = Person
>   { name :: Html ()
>   , location :: Html ()
>   , likes :: [Html ()]
>   }
> ```
> 
> and remove the need to use `toHtml` in our template, but that would be unweildy to other parts of the program that need to manipulate those fields.
> It's more practical if the fields are of type `Text`, and we convert them to `Html ()` when it's necessary.

Awesome! Now we have an HTML template that we can apply to any value of the type `Person`:

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

## Template Fragments with Lucid

Let's use what we learned and implement the template fragments pattern. I'm going to use the example used in the original [essay](https://htmx.org/essays/template-fragments/) so we can compare and contrast between the approaches that these two templating libraries take.

First, let's translate the first chill template used in the essay into lucid:

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
          then button_ [hxPatch_ $ "/contacts/" <> toText contact.id <> "/unarchive"] "Unarchive"
          else button_ [hxPatch_ $ "/contacts/" <> toText contact.id] "Archive"
      h3_ [] "Contact"
      p_ [] $ toHtml contact.email
```

Our goal is to turn the button in the `contactDetail` template into its own template so that we can render it by itself if needed.
To do this, we can simply factor out the HTML we want to reuse into its own function, and use it like any other template function:

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
      div_ [hxTarget_ "this"] $ contactArchiveUI contact
      h3_ [] "Contact"
      p_ [] $ toHtml contact.email

contactArchiveUI :: Contact -> Html ()
contactArchiveUI contact = 
  if contact.archived
    then button_ [hxPatch_ $ "/contacts/" <> toText contact.id <> "/unarchive"] "Unarchive"
    else button_ [hxPatch_ $ "/contacts/" <> toText contact.id] "Archive"
```

Now we can do the following:

```haskell
someContact :: Contact
someContact = Contact
  { id = 101
  , email = "someemail@some.com"
  , archived = True
  }

allTheDetails :: Html ()
allTheDetails = contactDetail someContact

justTheButton :: Html ()
justTheButton = contactArchiveUI someContact
```

There we go! Quite simple.

## Conclusion

Lucid does support the concept of template fragments. You have to factor out the HTML you want to reuse into its own function and explicitly define its parameters, which is slightly less convinient than the chill templates example in the original essay. The chill template in the original essay doesn't require you to factor out anything from the base template, but only annotate the fragment with an identifier that you use to refer to it. I also found it interesting that the chill template fragment seems to "inherit" the parameters passed into the base template.

# Template Fragments with Lucid

Recently, the creator of htmx Carson Gross has been conducting the template fragments hype train and
calling for programmers to expose if template fragments are possible in their templating language of choice.
My templating language of choice is Lucid; a monadic DSL for rendering HTML in Haskell.

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
You have a tag function, like `h1_`, that you apply to a list of attributes and an inner HTML value.
If two tags have the same indentation level, they will be rendered next to eachother.
If one tag is in the `do` block of the another tag, it is rendered within the other tag.

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


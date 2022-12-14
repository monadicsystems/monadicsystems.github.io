---
title: Haskell Server Pages With Okapi
image: /logo-bg.png
summary: How to implement and use Hasekll Server Pages with Okapi
---

# Haskell Server Pages With Okapi

Recently, I've been playing around with [redbean](https://redbean.dev/). Redbean is a highly portable web server written in Lua with support for [Lua Server Pages](https://redbean.dev/#lua). I really like the idea of Lua Server Pages because of their simplicity and ease of use. It reminded me of PHP.

I wanted to see if Haskell Server Pages (HSPs) could be implemented with [Okapi](https://www.okapi.wiki/). What would they look like, how would they function, and how would they compare to server pages in other languages?

## Haskell Server Pages in the Past

The paper *[Haskell Server Pages - Functional Programming and the Battle for the Middle Tier](https://www.researchgate.net/publication/2381809_Haskell_Server_Pages_-_Functional_Programming_and_the_Battle_for_the_Middle_Tier)* shows an implementation of HSPs with a lot of cool features, such as HTML tag literals, pattern matching on HTML tags, and type safe HTML generation.

It was published in 2001, but I'm not sure how much it has been used since then. Regardless, many of the concepts dicussed in the paper are very interesting.

Haskell has changed a lot since 2001, so I figured it was still worth it to try and implement a modern version of HSPs using Okapi.

## Haskell Server Pages With Okapi

Okapi provides a monadic DSL for describing web servers. Here's an example of a simple web server that greets the user.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Combinators
import Data.Text
import Okapi

main :: IO ()
main = run id do
  methodGET                               -- Ensure the request has the GET method
  pathParam @Text `is` "greet"            -- Ensure the first path segment is "greet"
  maybeName <- optional $ pathParam @Text -- Bind an optional path parameter to an identifier
  pathEnd                                 -- Ensure there are no more path segments
  
  -- Use the write function to append data to the response body
  case maybeName of
    Nothing   -> write "Hello, Stranger."
    Just name -> write $ "Cool name, " <> toLBS name <> ". Nice to meet ya!" 
```

`methodGET`, `pathParam`, and `pathEnd` are **parsers**, but instead of parsing text they parse HTTP requests.
Just like textual parsers, you can sequence these parsers using `do` notation and modify their behavior using parser combinators like `optional`.

If you've been following the development of Okapi, you might've noticed that the Okapi DSL looks slightly different compared to previous blog posts and examples. On top of making various changes to Okapi's API to make the library simpler and more ergonomic, I've changed how responses work in Okapi. These changes have been made on a separate branch from `main` called [`hsp`](https://github.com/monadicsystems/okapi/tree/hsp). All changes mentioned in this blog post can be found on that branch. Updates to the [documentation](https://www.okapi.wiki/) that reflect these changes are coming very soon.

### How Responding in Okapi Changed

Before, Okapi's `run` function had the type signature

```haskell
run
  :: Monad m
  => (forall a. m a -> IO a)
  -> OkapiT m Response -- Needs to return a response
  -> IO ()
```

, meaning it only accepted parsers that returned a value of type `Response`.

Now, Okapi's `run` function has the type signature

```haskell
run
  :: Monad m
  => (forall a. m a -> IO a)
  -> OkapiT m () -- Just returns ()
  -> IO ()
```

, so it takes parsers that don't return anything except the `()` value.

Why is that? Well, the `Response` now resides in the state of the parser and it can be manipulated using functions like `write`, `overwrite`, `setHeaders`, `setHTML`, `setJSON`, etc. The greet example you saw above used the `write` function to append data to the response body.

Before, you would have to explicitly return a value of type `Response`, like so.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Combinators
import Data.Text
import Okapi

main :: IO ()
main = run id do
  methodGET
  pathParam @Text `is` "greet"
  maybeName <- optional $ pathParam @Text
  pathEnd

  -- We have to return a response explicitly here
  case maybeName of
    Nothing   -> return $ setPlaintext "Hello, Stranger." $ ok
    Just name -> return $ setPlaintext "Cool name, " <> name <> ". Nice to meet ya!" $ ok
```

### Using `write`

By making the response a part of the state, the code for creating responses is a lot more flexible. For example, we now have a `write` function that's used for appending bytes to whatever is the current state of the response body. This is what we need for HSPs to work and look good.

Here's an example of a server definition that utilizes various response modifiers, including `write`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Combinators
import Data.Text
import Okapi
import Lucid

main :: IO ()
main = run id do
  methodGET
  pathParam @Text `is` "random"
  name <- queryParam @Text "name"
  pathEnd

  setStatus 200                           -- Set response status to 200
  setHeader ("Content-Type", "text/html") -- Set response header "Content-Type" to "text/html"

  -- A "Hello, world!" header
  write "<h1>"
  write "Hello, world!"
  write "</h1>"

  -- Write an ordered list from 1 to 5
  write "<ol>"
  forM_ [1..5] \num -> do
    write "<li>"
    write $ toLBS (num :: Int)
    write "</li>"
  write "</ol>"

  let writeCongrats = do
        write "<h2>"
        write "Congratulations for having your name!"
        write "</h2>"

  case name of
    "James" -> writeCongrats
    "Janet" -> writeCongrats
    "Alice" -> writeCongrats
    "Larry" -> writeCongrats
    _ -> pure ()

  -- Use Lucid too!
  write $ renderBS do
    h1_ [] "You can also write blocks of Lucid to generate HTML!"
    div_ [class_ "info"] do
      p_ [] "Lucid is very useful"
      a_ [href_ "https://hackage.haskell.org/package/lucid"] "Learn more about Lucid here"
```

Pushing the response into the state allows us to generate parts of the response using directives that we all know and love, like `case` expressions, `if_then_else_` expressions, `let` expressions, and even `forM_` loops. You may be thinking that this looks very imperative, and you would be right. Haskell is the best imperative programming language after all!

### Using Template Haskell for HSPs

The `main` procedure defined above is a series of statements that pretty much looks like a server page. If only we could put this series of statements in a seperate file, then we would have a Haskell Server Page. Luckily, we can use Template Haskell for this. The process to take these lists of statements declared in other files, and generate a server from them, consists of these steps:

1. At compile time, look for files with the `.hsp` extension in a directory specified by the developer.
2. Parse the `.hsp` files, indent them, and place the statements within a `do` block to create a large `do` expression.
3. Combine the parsers generated from each `.hsp` file using the `<|>` combinator, and generate path parsers where necessary according to the structure
   of the directory provided by the developer.

You can use HSPs by using the `hsp` quasiquoter exported by `Okapi.HSP`. You'll also need to turn on the `-XQuasiQuotes` language extension.
The `hsp` quasiquoter parses the name of the directory that holds your `.hsp` files and generates the correct code.

The structure of the directory provided to the `hsp` quasiquoter has an effect on the parser that is generated. It is similar to how `Next.js` works. Routing is based on the structure of the directory.

Here's an example of the structure of an [HSPs directory from the Okapi GitHub repo](https://github.com/monadicsystems/okapi/tree/hsp/my_hsp_files).

```
my_hsp_files/
?????? bar/
???  ?????? [age].hsp
?????? calc/
|  ?????? add/
???  |  ?????? [x]/
???  ???  |  ?????? [y].hsp
?????? greeting/
???  ?????? [name].hsp
bar.hsp
greeting.hsp
```

File and directory names surrounded in square brackets, like `[x]` and `[y].hsp` in the directory tree above, are treated as path parameters. For example, the file path `my_hsp_files/calc/add/[x]/[y].hsp` corresponds to the route `my_hsp_files/calc/add/<x>/<y>`, where `<x>` and `<y>` are path parameters. You can refer to path parameter names in square brackets from within your HSP files if:

- The HSP file is a descendant of a directory with a name in square brackets
- The HSP file itself is named with a name in square brackets

For example, here's the contents of the `my_hsp_files/calc/add/[x]/[y].hsp` file.

```haskell
methodGET
write $ toLBS $ x + (y :: Int)
```

We simply add the two path parameters `x` and `y`, and `write` the result to the response body.
A type annotation is needed on at least one of the parameters in this case, so that the compiler knows the type of the parameters.
Otherwise, the compiler will complain that it doesn't know the type of the `x` and `y` values.

Now that we know how the structure of the HSPs directory affects routing, let's generate a functional server from a directory full of `.hsp` files using the `hsp` quasiquoter.
The [`Main.hs` module defined in the `examples/hsp-test` folder of the Okapi GitHub repo](https://github.com/monadicsystems/okapi/blob/hsp/examples/hsp-test/Main.hs) does just that using the `my_hsp_files` directory shown above as the source directory.

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Okapi
import Okapi.HSP (hsp)
import qualified Data.Text as Text
import qualified Control.Monad.Combinators as Combinators
import Lucid

main :: IO ()
main = Okapi.run id [hsp|my_hsp_files|]
```

Magic!

To test it out for yourself clone the `hsp` branch of the Okapi GitHub repository to your machine, and run the following commands:

```
cd okapi
cabal v2-repl okapi:hsp-test-exe
```

Once everything compiles and the `ghci` prompt appears, type `main` into the prompt and hit 'Enter'. This will start a server on `localhost:3000`.

You'll need to have `ghc` and `cabal` installed for this to work. I reccommend you use `ghcup` if you don't have these installed already.

> **Note**
> The above example may or may not work on Windows. Will probably work for MacOS. I ran it on Ubuntu.
> All functions, data types, etc. from external libraries used in your HSP files need to be in scope in order to avoid compile time errors.
> For example, in one of the HSP files we use `renderBS` from the `lucid` library, so `Lucid` is imported into `Main.hs` to bring it into scope.

### Concerns with HSPs

The current implementation is more of a proof-of-concept and it is nowhere near the best that it could be. Some current downsides with HSPs are that syntax highlighting is lacking, error messages aren't the best if there are syntax errors in your HSP file, and documentation in lacking. The current implementation is very bare bones just to showcase the idea. If it catches on, perhaps it would be possible to add syntax highlighting and other helpers that we get from the Haskell Language Server for HSPs.

## Conclusion

This post just scratches the surface of what can be done with HSPs. Feel free to ask questions and/or provide suggestions. I'm looking for contributors. One concern I have with the current implementation of HSPs, and Okapi in general, is the performance. I haven't worried about performance as I'm mostly focused on the ergonomics of the API, but I will definitely be optimizing its performance as time goes on.

In the future, it may be possible to serve an entire web application from a single zip executable containing HSPs. Just like redbean, but for Haskell. This is something I'm currently working on with the help of [Cosmopolitan](https://justine.lol/cosmopolitan/) and [redbean](https://redbean.dev/) contributors. If this idea interests you and you'd like to help, please send me a DM.

<!--

In the new version of Okapi, if no functions to set or add anything to the response are called, a `200 OK` response is returned by default if the parser succeeds.

```haskell
main = run id do
  methodGET
  pathParam `is` "home"
  pathEnd
```

The parser defined above returns a `200 OK` response when a `GET` request is made to the `/home` endpoint. If `GET /home/foo`, `GET /nothome`, `POST /home`, or any other request besides `GET /home` is made the parser will return a `404 Not Found` (the default provided by the `run` function). Basically, if the parser fails the default response `404 Not Found` is returned because this is how `run` is defined. You can provide a custom default response by using the `serve` function instead of `run`.

```haskell
main = serve id (redirect 308 "/home") do
  methodGET
  pathParam `is` "home"
  pathEnd
```

By using `serve`, we can return a redirect to the correct location if the parser fails instead of returning a `404 Not Found` error.
-->

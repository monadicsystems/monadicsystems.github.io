# Creating Haskell Server Pages With Okapi

I've been looking at [redbean](), a highly portable web server written in Lua, and I really liked the idea of Lua Server Pages because of its simplicity. It reminded me of PHP Server Pages and I wanted to see if the same could be accomplished in Haskell. After doing some research, I found that most popular programming languages have a server pages implementation. If you don't know what server pages are, they are basically some programming language + HTML combined together in a single file. From the perspective of an MVC architecture, server pages encode both the view and controller. What do server pages look like in Haskell and how do they compare to server page implementations in other languages? 

## Haskell Server Pages in the Past

It turns out Haskell Server Pages are already a thing, or were a thing in the past. I'm not sure if anyone still uses them.
I found this paper on Haskell Server Pages written in 2001 by x, y, and z. It shows an implementation of HSP with a lot of cool features, like
HTML tag literals, pattern matching on HTML tags, and type safe HTML generation. I'm not even sure if this was released to the public, but if anyone has used this implementation of HSP before, I'd like to hear your experience.

## Haskell Server Pages With Okapi

In order to implement Server Pages With Okapi, I needed to make a slight change in how responses work in Okapi. These changes have been made on a separate branch from `main` called `hsp`. All changes mentioned in this blog post can be found on that branch.

### Responding Explicitly vs Implicitly

Before, an Okapi parser needed to return a value of type `Response` in order to be run. Now if you look at the implementation on the `hsp` branch you'll see that Okapi's `run` function takes a parser that returns `()`. Why is that? Well, the `Response` now resides in the state of the parser and can be manipulated using functions like `setHeaders`, `setHTML`, `setJSON`, etc. Here's an example of a simple ping server using the new `hsp` branch of Okapi.

```haskell
ping = do
  methodGET
  pathParam @Text `is` "ping"
  pathEnd
  setPlaintext "pong"
  
main = run id ping -- /ping -> 200 text/plain "pong"
```

Before, you would have to explicitly return a value of type `Response`.

```haskell
ping = do
  methodGET
  pathParam `is` "ping"
  pathEnd
  let response = setPlainText "pong" $ ok
  return response
  
main = run id ping -- /ping -> 200 text/plain "pong"
```

In the `hsp` version of Okapi, if no functions to set or add anything to the response are called, a `200 OK` response is returned automatically if the parser succeeds.

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

### Using `write`

By making the response a part of Okapi's state, the code for creating responses is a lot more flexible. For example, we now can have a `write` method that appends bytes to whatever is the current state of the response body. This is what we needed for server pages to work and look good.

```haskell
main = run id do
  methodGET                    -- Check that request uses GET HTTP method
  pathParam @Text `is` "greet" -- Check that path parameter is "greet"
  name <- pathParam @Text      -- Bind path parameter after /greet/ path segment
  pathEnd                      -- Check that there are no more path segments

  setStatus 200                           -- Set response status to 200
  setHeader ("Content-Type", "text/html") -- Set response header "Content-Type" to "text/html"

  -- A "Hello, world!" header
  write "<h1>"
  write "Hello, world!"
  write "</h1>"

  -- Write an unordered list from 1 to 5
  write "<ul>"
  for_ [1..5] \num -> do
    write "<li>"
    write $ toText num
    write "</li>"
  write "</ul>"

  let writeCongrats = do
    write "<h2>"
    write "Congratulations for having your name!"
    write "</h2>"

  case name :: Text of
    "James" -> writeCongrats
    "Janet" -> writeCongrats
    "Alice" -> writeCongrats
    "Larry" -> writeCongrats
    _ -> pure ()

  -- Use Lucid too!
  writeLucid do
    h1_ [] "You can also write blocks of Lucid to generate HTML!"
    div_ [class_ "info"] do
      p_ [] "Lucid is very useful"
      a_ [href_ "https://lucid-info.com"] "Learn more about Lucid here"
```

We can generate parts of the response using directives that we all know and love, like `case` statements, `if_then_else_` statements, `let` statements, and even `for_` loops. You may be thinking that this looks very imperative, and you would be right. Haskell is the best imperative programming language after all!

### Using Template Haskell for HSPs

The parser we defined above is a series of statements that pretty much looks like a server page. If only we could put this series of statements in another file, then we would have Haskell Server Pages. Luckily, we can use Template Haskell for this. The process consists of these steps:

1. At compile time, look for files with the `.hsp` extension in the directory specified by the developer. Use the current directory by default.
2. Parse the `.hsp` files, indent them, and place the statements within a `do` block to create a large `do` expression.
3. Combine the parsers generated from each `.hsp` file using the `<|>` combinator and path parsers where necessary according to their file path relative to the directory provided by the developer.

You can use HSPs by using the `hsp` quasiquoter function exported by `Okapi.HSP`. You'll also need to turn on the `-XQuasiQuotes` language extension.
The `hsp` quasiquoter parses the name of the directory that holds your `.hsp` files and generates the correct parser.

The structure of the directory given to the `hsp` quasiquoter has an effect on the parser that is generated. The quasiquoter will generate the correct path parsers for each `.hsp` file based on its file path relative to the directory provided by the developer. This is similar to how `Next.js` works. Routing is based on the structure of the directory.

As an example, let's turn that long parser we defined above for the `greet/<name>` endpoint into a HSP. Let's store our HSP files in the `my_hsp_files` directory.

```
my_hsp_files/
├─ greet/
|  ├─ [name].hsp
```

Next, in the `[name].hsp` file we define our HSP.

```haskell
setStatus 200                           -- Set response status to 200
setHeader ("Content-Type", "text/html") -- Set response header "Content-Type" to "text/html"

-- A "Hello, world!" header
write "<h1>"
write "Hello, world!"
write "</h1>"

-- Write an unordered list from 1 to 5
write "<ul>"
for_ [1..5] \num -> do
  write "<li>"
  write $ toText num
  write "</li>"
write "</ul>"

let writeCongrats = do
  write "<h2>"
  write "Congratulations for having your name!"
  write "</h2>"

case name :: Text of
  "James" -> writeCongrats
  "Janet" -> writeCongrats
  "Alice" -> writeCongrats
  "Larry" -> writeCongrats
  _ -> pure ()

-- Use Lucid too!
writeLucid do
  h1_ [] "You can also write blocks of Lucid to generate HTML!"
  div_ [class_ "info"] do
    p_ [] "Lucid is very useful"
    a_ [href_ "https://lucid-info.com"] "Learn more about Lucid here"
```

Finally, we just need to run the `hsp` quasi quoter.

```haskell
import Okapi.HSP

main = run id [hsp|/my_hsp_files|]
```

Magic!

### Concerns with HSPs

I thought up the current implementation really quickly and it is nowhere near the best that it could be. Some current downsides with HSPs are that syntax highlighting is lacking. Also, error messages aren't the best if there are syntax errors in your HSP file. The current implementation is very bare bones just to showcase the idea. If it catches on, perhaps it would be possible to add syntax highlighting and other helpers that we get from the Haskell Language Server for HSPs.

## Conclusion

This post just scratches the surface of what can be done with HSPs. In the future I hope to write more about this. In the meantime, feel free to ask questions and/or provide suggestions. I'm looking for contributors. One concern I have with the current implementation of HSPs is the performance. I haven't worried about performance as I'm mostly focused on the ergonomics of the API, but I will definitely be optimizing a lot of these ideas as time goes on.

In the future, it may be possible to serve an entire web application from a single zip executable containing HSPs. Just like redbean, but for Haskell. This is something I'm currently working on with the help of [Cosmopolitan]() and [redbean]() contributors. If this idea interests you and you'd like to help, please send me a DM.

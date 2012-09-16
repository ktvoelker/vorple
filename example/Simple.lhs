This is an example application built with Vorple. An Int is stored in each session,
and the client can send two kinds of requests: one to get the current value, and
another to set the value while returning the existing value.

> module Main where

You'll hear about these imports later.

> import Control.Monad.Identity (Identity())
> import Network.Wai.Handler.Warp (run)
> import Web.Vorple

We need types for requests, session state, and responses.

> data Request = Get | Put Int
> type Session = Int
> type Response = Int

Since the request type is a new one of our own invention, it doesn't yet have a
FromJSON instance. We can derive one automatically by calling this Template Haskell
function from Data.Aeson.TH:

> $(deriveJSON id ''Request)

Now we need a request handler. It's basically just a function from Request to
Response, with a monad along the way. Which monad is it? It's the
Vorple () Session Identity monad. Let's break that down:

* Vorple: a monad transformer that stacks a bunch of useful monads on top of whatever
underlying monad your application in needs.

* (): the type of the environment exposed by Vorple's built-in MonadReader
instance, which we aren't using here.

* Session: the type of a session state, which we can access through Vorple's built-in
MonadState instance.

* Identity: a monad that doesn't really do anything. Vorple needs an underlying
monad, but in this case, we don't, so Identity is a good choice.

> handler :: Request -> Vorple () Session Identity Response
> handler Get = get
> handler (Put n) = do
>   prev <- get
>   put n
>   return prev

Finally, we can use the vorple function to turn our handler into a standard WAI
Application which we can run with Warp.

> main :: IO ()
> main = run 4242 $ vorple defaultOptions () 0 handler


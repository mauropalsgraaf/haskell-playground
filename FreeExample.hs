{-# LANGUAGE DeriveFunctor #-}

module FreeExample where

import Control.Monad.Free

-- Console Algebra

data ConsoleF next
  = PrintLine String next
  | ReadLine (String -> next)
  deriving (Functor)

type Console = Free ConsoleF

-- Smart constructors for Console Algebra

printLine :: String -> Console ()
printLine x =
  liftF $ PrintLine x ()

readLine :: Console String
readLine =
  liftF $ ReadLine id

-- Interpreter

unsafeRunConsole :: Console a -> IO a
unsafeRunConsole (Pure a) = return a
unsafeRunConsole (Free (PrintLine x t)) = putStrLn x >> unsafeRunConsole t
unsafeRunConsole (Free (ReadLine f)) = getLine >>= unsafeRunConsole . f

-- Http Algebra

data HttpF next
  = Send String next
  deriving (Functor)

type Http = Free HttpF

-- Smart constructors for Http Algebra

send :: String -> Http ()
send url =
  liftF $ Send url ()

-- Interpreter

unsafeRunHttp :: Http a -> IO a
unsafeRunHttp (Pure a) = return a
run (Free (Send url t)) = putStrLn ("HttpRequest being send to url: " ++ url) >> unsafeRunHttp t

data AppF a = L (Http a) | R (Console a)

type App = Free AppF

unsafeRunApp ::  App a -> IO a
unsafeRunApp (Pure a) = return a
unsafeRunApp (Free (L http)) = unsafeRunHttp http
unsafeRunApp (Free (R console)) = unsafeRunConsole console

program = do
  x <- readLine
  _ <- printLine x
  return ()

Small configuration library written in Haskell

[![Build Status](https://travis-ci.org/YoEight/deiko-config.png?branch=master)](https://travis-ci.org/YoEight/deiko-config)

##Overview

Uses [Typesafe-config](https://github.com/typesafehub/config)'s format: HOCON

HOCON stands for Human-Optimized Config Object Notation. It's basically a JSON superset

Here's an example:

```
# This is a comment

foo.bar = ${toto}

toto = false

rawString = """
            This is a multi-
            lines String
            """

another.string = "I'm a String"

one.more.string = one more string

nested {
   list: [ one
         , 1
         , "both"]
   
   homing = { 
     pass: { b: feez } { a: "Prop"}
   }

   another: [1,2,3] [4,5,6]
}
```

More information about the format can be found on [Typesafe-config project page](https://github.com/typesafehub/config)

##Example

```haskell
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (runErrorT)
import Text.Deiko.Config

data Foo = Foo { fooPort :: Int, fooAddr :: String }

main = do
  res <- runErrorT loadFooProps
  either (error . configMsg) handle res
 
  where
    loadFooProps = do
      config <- loadFile "conf/app.conf"
      port   <- getInt "foo.port" config
      addr   <- getString "foo.addr" config
      liftIO $ makeSomethingUseful (Foo port addr)
      
makeSomethingUseful :: Foo -> IO Bar
makeSomethingUseful = ...

handleBar :: Bar -> IO ()
handleBar = ...

```


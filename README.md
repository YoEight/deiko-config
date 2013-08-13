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

More information about the format can be found on [Typesafe-config project page](https://github.com/typesafehub/config).

The library provides good error messages and comes with a bottom-up type inferencer in order to catch more configuration errors.

Here some use-cases:

1) `foo = ["bar", { baz : 42 }]`

You'll have:

```
Expected String (line: 1, col: 8) but having Object (line: 1, col: 15)
```

reason: List has only one inner type

2) `foo = ["bar"] [{ baz : 42 }]`

You'll have:

```
Expected List[String] (line: 1, col: 7) but having List[Object] (line: 1, col: 15)
```

reason: You can't merge Lists of different types

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


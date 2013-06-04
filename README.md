Small configuration library written in Haskell

##Overview

Uses the same format of [Typesafe-config](https://github.com/typesafehub/config): HOCON

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

##Installation

You need 2 tools named Alex and Happy (like Lex and Yacc but written in Haskell). 

```
$ cabal update
$ cabal install alex
$ cabal install happy
```

Then

```
# In project's home directory
$ alex Text/Deiko/Config/Lexer.x
$ happy Text/Deiko/Config/Parser.y
$ cabal-dev configure
$ cabal-dev install
```

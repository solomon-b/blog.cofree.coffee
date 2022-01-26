---
author: Solomon Bothwell
title: "Happy and Alex Part 2: MVP"
---

In my last post I gave a brief introduction to Happy and parser
generators. In this post I will continue the story with Happy\'s
counterpart Alex.

We will write an MVP parser for an Untyped Lambda Calculus. In a
subsequent post we will improve the design with proper error handling
and spans. Like last time, all code is available in the [git
repo](https://github.com/solomon-b/Dont-Worry-Be-Happy).

Alex, like Happy, is a DSL used to construct Haskell modules. From the
Alex docs:

> Alex takes a description of tokens based on regular expressions and
> generates a Haskell module containing code for scanning text
> efficiently.

At it\'s simplest alex takes in a set of token definitions and then
outputs a scanner function `String -> [Token]`, where `Token` is some
provided lexeme type.

# Alex File Syntax

Here is a complete Alex file taken from the
[documentation](https://www.haskell.org/alex/doc/html/introduction.html):

``` example
{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  let                            { \s -> Let }
  in                             { \s -> In }
  $digit+                        { \s -> Int (read s) }
  [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*  { \s -> Var s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    Let
  | In
  | Sym Char
  | Var String
  | Int Int
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
```

The elements of an Alex file are:

1.  Haskell code wrapped in curly brackets.
2.  Wrapper declarations.
3.  [Macro
    Definitions](https://www.haskell.org/alex/doc/html/alex-files.html#macrodefs).
4.  [Rules](https://www.haskell.org/alex/doc/html/alex-files.html#rules).

The structure of an Alex file is (in order):

1.  Some Haskell code (optional).
2.  Wrapper declaration (optional).
3.  Macro Definitions (optional).
4.  Rules.
5.  Additional Haskell code (optional).

Rules are the only required element, but in practice you will use a
little bit of everything.

## Wrapper Declarations

Wrappers are predefined bits of code that extend the functionality of
Alex. For example, there is a wrapper to add position tracking and
another to add monadic effects. The docs give an [overview of
wrappers](https://www.haskell.org/alex/doc/html/wrappers.html), but it
is best to just [look at the source
code](https://github.com/simonmar/alex/blob/master/data/AlexWrappers.hs).

In this post, we will use `basic` wrapper. It gives you
`alexScanTokens :: String -> [token]` which we will use as the entry
point to our lexer.

## Rules

Rules are the only required element in your Alex file and are the meat
and potatos of the lexer. A rules block is declared with `id |-`
followed by a series of rule statements.

A rule statement consists of a of a regex pattern (or macro definition)
and either a chunk of Haskell code wrapped in curly brackets or a
semicolon:

``` haskell
$digit+ { \s -> Int (read s) }
```

Rules are translated into Haskell functions whose type is determined by
our wrapper implementation. To be more precise, rule function\'s type is
the `a` param in the
[AlexReturn](https://github.com/simonmar/alex/blob/ab87af1803a5e2f2c09b09eb024dc6ec9f44b0e3/data/AlexTemplate.hs#L107-L111)
type. Our `basic` wrapper implements
[alexScanTokens](https://github.com/simonmar/alex/blob/master/data/AlexWrappers.hs#L332-L338)
which recursively consumes the `String` (or `ByteString`) input and
constructs a list of tokens.

If we were to forgo the `basic` wrapper and write out own custom scanner
then we would be abe to dictate the type signature of our rule
functions.

In the example above and in the arithmetic language parser from the last
post the signature was `String -> Token`. We can see the function
definitions in [our arithmetic lexer from last
time](https://github.com/ssbothwell/Dont-Worry-Be-Happy/blob/main/app/arith/Lexer.hs#L1029-L1035).

Regex matching respects the longest match rule which references the
regex match which consumes the greatest number of characters.

When a match is found, then an `AlexReturn` value is constructed. If the
rule contains a chunk of Haskell code then that chunk of code is
returned inside the `AlexToken` data constructor. If the rule has a
semicolon rather then a chunk of Haskell code to return, then an
`AlexReturn` value with the
[AlexSkip](https://github.com/simonmar/alex/blob/ab87af1803a5e2f2c09b09eb024dc6ec9f44b0e3/data/AlexTemplate.hs#L110)
data constructor is used.

The `AlexReturn` term is then consumed by `alexScanTokens` function, in
the case of the `basic` wrapper, or by whatever scanner function we
chose to define.

### Contexts

Rules can also have a `right` and `left` context which are added before
or after your regex respectively.. Contexts allow you to match on the
characters before and after your lexeme.

`Left Context` allows you to match on the beginning of a line via the
`^` character:

``` haskell
^ $alpha [$alpha]* { \s -> Identifier s }
```

This rule will only match a string of alpha characters immediately
following a newline char.

`Right context` is more powerful and has three forms:

1.  `$` : The rule will only match if it is immediately *preceding* a
    newline char.

2.  / *regex* : This rule will only match if its regex match is
    immediately followed by the additional provided regex.

3.  / { ... } : This rule applies a Haskell predicate function on the
    rule to determine if it matches.

    The predicate function\'s type must be:

    ``` haskell
    { ... } :: user       -- predicate state
        -> AlexInput  -- input stream before the token
        -> Int        -- length of the token
        -> AlexInput  -- input stream after the token
        -> Bool       -- True <=> accept the token
    ```

### Start Codes

Another powerful tool for writing Alex rules are `Start Codes`. They
allow you to add state to your lexer. I am going to hold off discussing
them until a later post but they are really useful for things like
string templating.

# The Lexer

For a minimal Untyped Lambda Calculus using the traditional syntax we
will need the following lexemes:

``` haskell
data Token
  = Identifier String
  | Lambda
  | Dot
  | OpenParen
  | CloseParen
```

Our intial lexer is very straight forward:

``` haskell
{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z09]

tokens :-

-- Whitespace insensitive
$white+                       ;

-- Comments
"#".*                         ;

-- Syntax
\\                            { \_ -> Lambda }
\.                            { \_ -> Dot }
\(                            { \_ -> OpenParen }
\)                            { \_ -> CloseParen }
$alpha [$alpha $digit \_ \-]* { \s -> Identifier s }


{
data Token
  = Identifier String
  | Lambda
  | Dot
  | OpenParen
  | CloseParen
  deriving Show

lexer :: String -> [Token]
lexer = alexScanTokens
}
```

`alexScanTokens` is supplied by the `basic` wrapper and we bind it to
`lexer` for convinence.

There isn\'t a whole lot to say about this lexer. It does pretty much
what you expect:

``` haskell
> lexer "\\x. y x"
[Lambda,Identifier "x",Dot,Identifier "y",Identifier "x"]
> lexer "\\x. y x # this is a comment"
[Lambda,Identifier "x",Dot,Identifier "y",Identifier "x"]
```

Note: we had to escape the `\` because we are in a Haskell repl.

The main issues are that it provides no safe error handling and provides
no spans:

``` haskell
> lexer "[\\x.x]"
*** Exception: lexical error
CallStack (from HasCallStack):
  error, called at templates/wrappers.hs:336:32 in main:Lexer
```

If you look at the source for
[alexScanTokens](https://github.com/simonmar/alex/blob/master/data/AlexWrappers.hs#L336)
you can see that it simply calls `error` for any lex errors.

We will fix these shortcomings soon, but lets move on to the parser for
now.

# The Parser

Now that we have a `String -> [Token]` lexer function, we can implement
our Happy parser. In my [last
post](https://blog.cofree.coffee/2021-10-29-dont-worry-be-happy/) I
described the basic syntax of a Happy file. Here I will include a
complete Happy file and go over some of the specific syntax not
mentioned in the last post.

``` haskell
{
module Parser where

import qualified Lexer as L
}

%name parser expr
%tokentype { L.Token }
%error { parseError }

%token

ident  { L.Identifier $$ }
lambda { L.Lambda }
'.'    { L.Dot }
'('    { L.OpenParen }
')'    { L.CloseParen }

%%

expr
  : lambda ident '.' ap { Abs $2 $4 }
  | ap                  { $1 }

ap
  : ap atom { Ap $1 $2 }
  | atom { $1 }

atom
  : '(' expr  ')' { $2 }
  | ident { Var $1 }

{
parseError :: [L.Token] -> a
parseError [] = error "ParseError: Empty token stream."
parseError (tok:_) = error $ "ParseError: Unexpected token '" <> show tok <> "'."

data Term =
    Var String
  | Abs String Term
  | Ap Term Term
  deriving Show
}
```

Lines beginning with `%` are known as Directives. These are input to the
Happy parser generator.

In this file we use:

-   `%name`: Declares the name of the final parser function and the
    initial production rule to kick off the parsing process. You can
    actually have multiple name directives yielding several different
    parsers from a single grammar.
-   `%tokenType`: Declares the type of the lexeme token used as input to
    the parser.
-   `%error`: The error handling function. This function is applied to
    the remaining token stream when a parse error is encountered.
-   `%token`: This kicks off the terminal symbols section of our
    grammar. Lexemes matched in curly brackets are replaced by the
    identifers on their left in the following production rules. You can
    think of this section as a case statement on the lexeme token type.
-   `%%`: This kicks off the production rules section of the grammar.
    Production rules are described in my previous post, so I will omit
    them here.

In addition to our directives, the happy file can be bookended with
Haskell code like the Alex file.

We can now execute our parser in the repl and build an AST:

``` haskell
> parser $ lexer "\\x. y x"
Abs "x" (Ap (Var "y") (Var "x"))
> parser $ lexer "\\x. y x # this is a comment"
Abs "x" (Ap (Var "y") (Var "x"))
```

# Final Thoughts

Here we have built a minimally complete lexer and parser for Untyped
Lambda Calculus. The implementation is available
[here](https://github.com/solomon-b/Dont-Worry-Be-Happy/tree/main/app/basic)
along with all other lexers and parsers for this series.

In my next post we will rewrite this parser without the `basic` wrapper
while adding spans and error handling. Then in a subsequent post I will
move on to a more complex language requiring monadic state.

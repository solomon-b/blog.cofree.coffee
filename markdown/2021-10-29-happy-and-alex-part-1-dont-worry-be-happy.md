---
author: Solomon Bothwell
title: "Happy and Alex Part 1: Don\\'t Worry Be Happy"
---

When I was first learning Haskell it was emphasized to me via blog
posts, books, and conversations how wonderful Haskell is for language
design. One of the major points in favor of this was parser combinators.

parser combinators are a beautiful example of the powers of abstraction
and compositionality inherent in Haskell\'s design.

``` haskell
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }
```

This simple type signature gives rise to a hierarchy of typeclass
instances and combinators that grant us an eDSL for compositionally
building up parsing subroutines.

But despite their elegant design, parser combinators have a dark secret.
It is really hard to scale them up to a feature rich language.

They do nothing to help with left recursion, they do not easily
correlate to a BNF grammar, you get no warnings about ambiguity, and it
is difficult to control operator precedence. At the end of the day, they
are simply too powerful.

> Liberties constrain, constraints liberate.

But this blog post isn\'t really about parser combinators. It is about
another powerful tool the in Haskell arsenal. A tool that does not get
much attention, but which is used in GHC itself in place of parser
combinators.

# The challenges of left recursion and ambiguity

Imagine we wanted to parse a basic arithmetic language with addition,
negation, and multiplication of integers. We want infix operators and
parenthesis to set operator precedence.

Using a Megaparsec, we might construct the following parser:

``` haskell
data Expr =
    Num Int
  | Add Expr Expr
  | Negate Expr
  | Multiply Expr Expr

parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') p <* space

parseInt :: Parser Expr
parseInt = (Num <$> some digitChar) <* space

parseNegate :: Parser Expr
parseNegate = do
  char '-'
  space
  Negate <$> parseExpr

parseAdd :: Parser Expr
parseAdd = do
  t1 <- parseExpr
  space
  char '+'
  space
  t2 <- parseExpr
  space
  pure (Add t1 t2)

parseMult :: Parser Expr
parseMult = do
  t1 <- parseExpr
  space
  char '*'
  space
  t2 <- parseExpr
  space
  pure (Multiply t1 t2)

parseExpr :: Parser Expr
parseExpr =
  parseInt <|> parseNegate <|> parseAdd <|> parseMult <|> parens parseExpr
```

This looks really nice and uses all our familiar monadic and applicative
combinators. Other then having to pepper the parsers with `space`, it is
fairly clean and we can reasonably infer the intended grammer from the
functions. However, it has one big problem.

It doesn\'t actually work as is.

The precedence of subparsers in `parseExpr` is wrong so it will exit
early on an Int, and the parens parsing fails due to left recursion.

We can try to fix the parser precedence by changing the order of the
alternatives in `parseExpr`:

``` haskell
parseExpr :: Parser Expr
parseExpr =
  parseNegate <|> parseAdd <|> parseMult <|> parseInt <|> parens parseExpr
```

But now all input fails on left recursion. Oops!

We can fix the left recursion using a special [Operator
type](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Monad-Combinators-Expr.html#t:Operator),
or by using [clever tricks](https://github.com/glebec/left-recursion).
But there is no way to perform static analysis on this parser and
identify these sorts of issues. We have to run the code and see the
error. This means creating an elaborate test suite to try to ensure your
parser doesn\'t crash.

But in order to ensure test coverage, you need to define the formal
grammar for your language. So now we have to write a parser, a grammar,
and a test suite all independently.

# Simplifying the problem

Lexical analysis is the process of converting a sequence of characters
into a sequence of tokens called lexemes. Lexemes typically will form a
[regular language](https://en.wikipedia.org/wiki/Regular_language) which
means that their grammar rules can be defined by regular expressions
(regex). As such, it is trivial to recognize a regular language in one
sequential scan of a string.

A parser on the other hand recognizes a [context-free
language](https://en.wikipedia.org/wiki/Context-free_language).
Context-Free Languages are a superset of Regular Languages and so we can
use a parser to perform lexical analysis, however it is overpowered for
the job.

Typically when using parser combinators we perform lexical analysis
while parsing the input stream. There are arguments in favor of this
approach but in my experience the parsing experience is greatly
simplified by factoring out the more trivial lexical analysis.

Libraries such as MegaParsec offer
[tools](https://hackage.haskell.org/package/megaparsec-9.2.0/docs/Text-Megaparsec-Stream.html)
to integrate your own lexer.

# Introducing Happy

Happy is a [LALR](https://en.wikipedia.org/wiki/LALR_parser) Parser
Generator library, which we can think of as a compiler from some
high-level description of a grammar into an efficient parser. As it is a
compiler, it can do a lot of static analysis on the grammar we have
provided it, which solves a lot of the aforementioned issues.

To build a parser, we must provide Happy with a [formal
grammar](https://en.wikipedia.org/wiki/Formal_grammar) consisting of a
set of `Production Rules` mapping `Terminal` and `Non-Terminal` symbols
to terms in our AST.

The `Production Rules` are essentially a set of recursive substition
rules which are applied to the lexeme stream input to the parser.
`Terminal` symbols are the base cases and `Non-Terminal` symbols are the
recursive cases.

When a lexeme, or a series of lexemes, match a production rule a series
of recursive substitutions are applied until we land on a `Terminal`
symbol.

If the lexeme stream to the parser matches the `Production Rules` then
the stream is considered valid in the language and you output a parse
tree. If not, then you output a parse error.

A picture paints a thousand words. Lets construct a parser for the same
arithmetic language:

``` haskell
data Expr =
    Num Int
  | Add Expr Expr
  | Negate Expr
  | Multiply Expr Expr
```

First we declare our parser and specify our initial symbol:

``` haskell
%name parser expr
```

The initial symbol is the first production rule to be applied to the
token stream and is essentially the entry point into the parsing
automaton.

Next we declare our terminal symbols and their corresponding lexemes:

``` haskell
%token

'+' { L.Plus }
'-' { L.Negate }
'*' { L.Multiply }
'(' { L.OpenParen }
')' { L.CloseParen }
int { L.Num $$ }

%left '+' '*' '-'
```

On the left are the `terminal symbols` and on the right within curly
brackets are the corresponding lexemes that map to them. The `$$` is a
placeholder that allows us to pick out a specific part of the lexeme to
correlate with the `terminal symbol`.

The `%left` declaration informs Happy that those symbols are correlated
with left recursion. `LALR` parsers completely eliminate the need for
special handling of left recursion.

For reference, here is the lexeme datatype that corresponds to this
token declaration:

``` haskell
data Token
  = Num Int
  | Plus
  | Negate
  | Multiply
  | Divide
  | OpenParen
  | CloseParen
```

Note: This type would be defined in the lexer which generates the token
stream we pass into our Parser. I am omitting the lexer in this example,
but in later posts I will be introducing `Alex` and using.

Next we must define the `Production Rules` for `Non-Terminal
  Symbols`. These take the form:

``` haskell
n   : t_1 ... t_n   { E1 }
    | s_1 ... s_n   { E2 }
```

Where `n` is a `non-terminal symbol` and to the right of the colon is a
set of one or more possible expansions of `n` seperated by `∣` symbols.
`E1` and `E2` are bits of Haskell code.

When the symbols `t_1 ... t_n` are found in the token stream we
construct the symbol `N` and give it the value `E1`.

The parser recursively applies production rules to the input until the
input stream has been consumed and a parse tree has been constructed.

For our arithmetic language the production rules will be:

``` haskell
expr
  : int           { Num $1 }
  | expr '+' expr { Add $1 $3 }
  | expr '*' expr { Multiply $1 $3 }
  | '-' expr      { Negate $2 }
  | '(' expr ')'  { $2 }
```

We then use Happy to generate a haskell module containing our parser.
Assuming we have a `lexer :: String -> [Token]` we can run our parser in
the repl and try it out:

``` haskell
> parser $ lexer "1"
Num 1
> parser $ lexer "1 + 2"
Add (Num 1) (Num 2)
> parser $ lexer "1 + 2 * -3"
Multiply (Add (Num 1) (Num 2)) (Negate (Num 3))
```

Oops we forgot to handle operator precedence! If we break up our
production rules a bit then we can establish precedence in the grammar:

``` haskell
expr
  : expr '+' expr1 { Add $1 $3 }
  | expr1 { $1 }

expr1
  : expr1 '*' expr2 { Multiply $1  $3 }
  | expr2 { $1 }

expr2
  : int { Num $1 }
  | '-' expr2 { Negate $2 }
  | '(' expr ')' { $2 }
```

``` haskell
> parser $ lexer "1 + 2 * -3"
Add (Num 1) (Multiply (Num 2) (Negate (Num 3)))
> parser $ lexer "(1 + 2) * -3"
Multiply (Add (Num 1) (Num 2)) (Negate (Num 3))
```

A complete working example of this parser with an associated lexer is
available [here](https://github.com/solomon-b/Dont-Worry-Be-Happy).
I\'ll be adding more parsers to this repo as I write out the rest of
this series. My intention is to incrementally build up to a parser for a
lambda calulus which includes nice error handling span generation all
using Happy and Alex.

I started off this blog post being somewhat critical of parser
combinators, but I would like to clarify now that I am not strictly
opposed to them, I think they are one of many tools available to us.
Parser generators offer another powerful tool which often doesn\'t get
the attention they deserve in the Haskell community.

PS: In my last blog post I promised a brief introduction to `Freer`, how
it works, and how it further expands our ability to perform algebraic
effects. I haven\'t forgotten and I intend to write that up soon.

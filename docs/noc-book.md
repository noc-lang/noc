# Noc book

*In this tutorial, we are going to use the REPL Noc but the Noc interpreter use will explained.*

## Table of contents
  - [Introduction to concatenative programming](#introduction-to-concatenative-programming)
  - [Basics](#basics)
    - [Expressions](#expressions)
    - [Datatypes](#datatypes)
    - [REPL](#repl)
      - [Commands](#commands)
      - [Declare functions](#functions)
    - [CLI](#cli)
  - [Advanced Topic](#advanced-topic)
    - [Quotes](#quotes)
      - [List](#list)
      - [Dict](#)
    - [Macros](#)
    - [Control Flow](#)
      - [Case statement](#)
      - [If statement](#)
    - [Modules](#)
      - [Load Noc files](#)
      - [Standard Library](#)

---

<div id="introduction-to-concatenative-programming">

## Introduction to concatenative programming

Noc language is based on the concatenative paradigm, this paradigm is based on the [function composition](https://en.wikipedia.org/wiki/Function_composition_(computer_science)) in which we compose several functions to construct a more complex function, for instance:

```
[Applicative language]
y = foo(x)
z = bar(y)
w = baz(z)

[Concatenative language]
foo bar baz
```

Noc is also [stack-based](https://en.wikipedia.org/wiki/Stack-oriented_programming) so the expressions can be easily evaluated thanks to the stack machine, when we write an expression it's pushed in an stack instead of processus registers. Consequently, Noc use the [Reverse polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation) for evaluate expressions, it's the most used notation when we compose functions.

```
Infix        Postfix (reverse polish notation)
5 + 6   ->   5 6 +
```

</div>

---

<div id="basics">

## Basics

Now, we are going to learn the fundamentals of Noc language.

<div id="expressions">

### Expressions
As said before, the Noc expressions uses the reverse polish notation so the syntax used is like this: 

```
Syntax          Noc language 
f(x)      ->    x f
f(x,y)    ->    y x f
g(f(x))   ->    f g
g(f(x,y)) ->    y x f g 
```

Example:
```scala
noc> 5 6 +
=> [11]
```
To understand this, here is the steps:
```scala
noc> 5
=> [5]
noc> 6
=> [5 6]
noc> + (# here '+' operation pop the 2 top-stack elements and operate them)
=> [11]
```

With function composition, we can combine several operators, like:
```scala
noc> 5 6 + 2 * 10 /
=> [2.2]
```

Explanations:
```scala
noc> 5
=> [5]
noc> 6
=> [5 6]
noc> +
=> [11]
noc> 2
=> [11 2]
noc> *
=> [22]
noc> 10
=> [22 10]
noc> /
=> [2.2]
```
The equivalent in infix notation is: ``(5+6) * 2 / 10``, the reverse polish notation implements operator precedence.

We saw some "primitive operators" (the native operators of the interpreter) but it's obviously possible to combine with the native functions, called "primitive functions". These operators,functions are grouped in one module name Prelude.

We can access to the documentation of the all primitive functions,operators [here](#).

Example: 
```scala
noc> 1 2 3 dup + * /
=> [8.333333333333333e-2] # e is the scientific notation => 8.3*10^(-2)
```

Explanations:
```scala
noc> 1 2 3
=> [1 2 3]
noc> dup
=> [1 2 3 3]
noc> +
=> [1 2 6]
noc> *
=> [1 12]
noc> 
=> [8.333333333333333e-2]
```

</div>

---

<div id="datatypes">

## Datatypes 

Noc is dynamically typed language like the most concatenative languages (except some languages like Cat), it means that the type errors are reported at the runtime, also it's strongly typed (when types does not match it report at the runtime).

Example:
```scala
noc> 1 "1" +
TypeError "cannot operate with different types."
```

### Primitive types
| Name | Description | Example
| ----------- | ----------- | ----------- |
| Integer | An arbitrary precision integers, relative number | 15 or -86
| Float | Double-precision floating point numbers (real numbers) | 1.5 or -3.9
| String | UTF8 string | "Hello,World!"
| Bool | Boolean type | True or False
| Word | Word type | dup
| Quote | Anonymous stack containing instuctions | [5 5 +] [1 2 3] [["a" 1] ["b" 2] ["c" 3]]
</div>

---

<div id="repl">

## REPL

We are going to more experiment with the REPL of Noc. In this REPL, we can go back old expressions, commands or function declaration, thanks to the '.noc_history' file that's generated (at the first time) in the system and it stores all commands.

#### Keys
``arrow_up => go back to old commands``

``arrow_down => go back to new commands``

```scala
noc> 5 5 +
=> [10]
noc> zap
=> []
noc> 5 dup *
=> [25]
noc> zap # go back to get zap function
noc> 5 dup * # go back to new command
```

We can also put the cursor pointer at the beggining or at the end.

#### Keys
``ctrl + A => cursor pointer at the beggining``

`` ctrl + E => cursor pointer at the end``

*The parenthesis are here to indicate cursor pointer*

```scala
noc> 5 5 +() # current cursor pointer
noc> (5) 5 + # cursor pointer with ctrl + A
noc> 5 5 +() # cursor pointer with ctrl + E
```

---

<div id="commands">

### Commands

There are some commands in the REPL: 
```
noc> :help
Commands available from the prompt:

:quit | Exit REPL.
:load [filepath] | Load Noc file.
:reset | Resetting global stack.
:env | Show environment.
```

We will show the 'load' command in [Modules](#modules) topic.

| Name | Description 
| ----------- | ----------- |
:quit | Quit REPL and clear the .noc_history file
:reset | Reset the stack and the env (declared functions)

</div>

---

<div id="functions">

### Declare functions

We can declare function like this:
```
def function = { word1 word2 ... wordn }
```

Example:
```scala
noc> def square = {dup *}
noc> 5 square
=> [25]
noc> def number = {6} # We define a constant
noc> number square
=> [36]
```

It's like the substitution, when we called the declared function, in fact it give this:
```scala
noc> 5 dup *
```

When we declare a function, the function is pushed in the env. And we can access to this env with the command 'env':
```scala
noc> :env
square: [dup *], 
```

</div>
</div>

---

<div id="interpreter">

## CLI

Previously, we used the REPL but now we are going to see the CLI Noc. With this CLI we can create Noc files and evaluate these files.

#### Syntax
```
noc [file.noc]
```

**IMPORTANT: we have to declare the 'main' function, it's the function that the CLI evaluate.**

Example:
```scala
def main = {
  "Hello,World!" print
}
```
Output:
```scala
"Hello,World!"
```

Nothing (except comments but it's ignored in parsing) is in the top-level, we write programs in functions. (see [function-level](https://en.wikipedia.org/wiki/Function-level_programming))

Example:
```scala
def number = {5} # constant

def square = { dup * }

def main = {
  number square
  print
}
```
Output:
```scala
25
```

---

## Comments
We can create comments like this:

#### Simple comments
```
def square = {dup *}

def main = {
  # Output: [25]
  5 square
  print
}
```

or multiline comments:
```scala
/*
Hello World in Noc !
*/

def main = {
  "Hello,World!" print
}
```
</div>
</div>

<div id="advanced-topic">

## Advanced topics
Tha advanced concepts of Noc language.

<div id="quotes">

### Quotes

A quote is a kind of anonymous stack containing instructions, it's pretty useful because it's lazy. The quote allows to delay execution in order to evaluate later. The quotes are based on the [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity), it means that it can represent a program (the set of instructions in the quote) like a data (the quote).

To evaluate the quote, we use the ``unquote`` function available in the Prelude.

Example:
```scala
noc> 2
noc> def square = {dup *}
noc> [square]
=> [2 [square]]
noc> unquote
=> [4]
```

</div>

---

To explore all functions, we can check the [STD documentation](#).

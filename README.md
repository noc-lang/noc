# The Noc Programming Language

This repository contains the Noc interpreter and the Noc Standard Library.

## Noc versions

To get Noc, check the [Installation guide](https://noc-lang.github.io/manual/installation.html).

#### The Noc latest version

> This version was rewritten partially (check this [section](https://noc-lang.github.io/manual/optimizations.html)) in C to embark a VM and it's not longer maintained, there are still some bugs or lack of features:
> - callstack not works with recursive functions calls
> - cannot format quotes
> - cannot operate big ints (Arbitrary precision integers)

#### The Noc v0.1.0.0

> This version is more operational but slower than the latest version because it's fully interpreted.

In this version, theses functions are defined in these modules:

|         Module          |                                 Functions                                         |
| ----------------------- | --------------------------------------------------------------------------------- |
| [prelude](https://noc-lang.github.io/manual/library/prelude.html) | [putchar](https://noc-lang.github.io/manual/library/prelude.html#[putchar]) | 
| [seq](https://noc-lang.github.io/manual/library/seq.html)         | [pushr](https://noc-lang.github.io/manual/library/seq.html#[pushr]), [popr](https://noc-lang.github.io/manual/library/seq.html#[popr]), [unquote](https://noc-lang.github.io/manual/library/seq.html#[unquote]) | 
| [char](https://noc-lang.github.io/manual/library/char.html)       | [chr](https://noc-lang.github.io/manual/library/char.html#[chr]), [ord](https://noc-lang.github.io/manual/library/char.html#[ord]) | 
| [sys](https://noc-lang.github.io/manual/library/sys.html)         | [args](https://noc-lang.github.io/manual/library/sys.html#[args]), [catch](https://noc-lang.github.io/manual/library/sys.html#[catch]) |

### Some examples

fact.noc
```scala
def fact = {
  dup
   [
     [[0] [pop 1]]
     [[_] [dup 1 - fact *]]
   ] case
}

def main = {
  6 fact print
}
```

greater-or-less.noc
```scala
load std:stack
load std:bool

def readInput = {"Choose a number: " ask}

def mysteryNumber = { 45 }

def greaterOrLess = {
    dup
    [
        [[mysteryNumber] ["You won !" print]]
        [[_] [
            ["Less!" print readInput int greaterOrLess] ["Greater!" print readInput int greaterOrLess] 3 -1 rotNM mysteryNumber < if 
        ]]
    ] case
}

def main = {
    readInput int greaterOrLess
}
```

caesar.noc
```scala
load seq
load std:string
/* in the latest version the functions of the 'char' module are integrated in the Noc Prelude */
load char
load std:stack
load std:io
load sys

def caesar = {
    [
        [[[cipher]] [
                [ord swap dup 3 -1 rotNM + chr swap] step
            ]]
        [[[decipher]] [
                [ord swap dup 3 -1 rotNM swap - chr swap] step
            ]]
        [[_] ["error." putstrln 1 exit]]
    ] case

    popr pop swap quote swap <> tostr
}

def encrypt = {
    [cipher] caesar
}

def decrypt = {
    [decipher] caesar
}

def main = {
    /* [offset] [msg in array of chars] [encrypt/decrypt] */
    3 "Hello"$ encrypt print
    3 "Khoor"$ decrypt print
}
```

## Features
- Stack-based language
- An embbeded REPL **(only v0.1.0.0)**
- Running in VM **(only the latest version)**
- A list of combinators for the stack manipulation
- Typed dynamically
- Homoiconic (quotes)
- Provides a module system
- Includes a STD (Standard Library)
- Pattern matching

## Resources
- [vscode-noc](https://github.com/noc-lang/vscode-noc)
- [nocdoc](https://github.com/noc-lang/nocdoc)

## Contributing
To contribute to The Noc Programming Language, you can add an [issue](https://github.com/noc-lang/noc/issues/) to report some bugs, ask for a new feature, questions, etc... Or you can fork this repository to evolve her with the [pull requests](https://github.com/noc-lang/noc/pulls).

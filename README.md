# noc-lang

<img src="/assets/images/icon.png" alt="Noc icon" align=right width="128" />

User-friendly stack-based concatenative programming language.

### Installing noc
This command builds project and copies binary in **~/.local/bin** for GNU/Linux system or in **%APPDATA%\local\bin** for Windows system.
```
stack install
```

### Usage
You can check all commands here.
```
$ noc --help
```

You can also run Noc REPL, like this:
```
$ noc
```

### Noc grammar
That's Noc grammar with BNF notation.
```bnf
-- <reserved> ::= "def"
-- <alpha> ::= [A-Za-z-_]
-- <num> ::= [0-9]

-- <word> ::= (<alpha>)+
-- <numbers> ::= (<num>)+ | (<num>)+.(<num>)+
-- <quote> ::= "[" (<numbers> | <word> | <quote>)+ "]"
-- <stack> ::= (<numbers> | <word> | <quote>)*

-- <declaration> ::= <reserved> "=" "{" (stack) "}"
-- <program> ::= (<declaration>)*
```

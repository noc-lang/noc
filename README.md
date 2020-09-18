# noc

<img src="/assets/images/icon.png" alt="Noc icon" align=right width="128" />

User-friendly stack-based concatenative programming language.

### Installing noc
This command builds project and copies binary in **~/.local/bin** in GNU/Linux.
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

---

### Noc grammar
That's Noc grammar with BNF notation.
```bnf
-- <word> ::= [A-Za-z-_]+
-- <numbers> ::= [0-9]+ | [0-9]+.[0-9]+
-- <quote> ::= "[" (<numbers> | <word> | <quote>)+ "]"
-- <stack> ::= (<numbers> | <word> | <quote>)*
```


# RPN Compiler and Interpreter
A simple RPN compiler and interpreter for instructional purposes.

## Overview
This project was developed for use in a Cerner DevCon talk. The essential goal of this project is to
demonstrate construction of a simple compiler using a purely functional style. With the exception of
I/O side effects to accept expressions as input and various steps that produce output, the compiler
is purely functional.

## Grammar
The language recognized by the compiler is specified by the following BNF-style grammar. The input
is essentially simple mathematical expressions using infix notation. It is worth noting that the
grammar is defined in such a way that a recursive-descent parser can be easily constructed, which is
the method used in this project. The grammar also reflects the natural precedence of operators. As a
simple rule of thumb, the farther down in the grammar, the higher the precedence.

`e` stands for epsilon, which means the nonterminal on the left hand side can also be an empty
production. See [this article](https://www.cs.rochester.edu/~nelson/courses/csc_173/grammars/cfg.html)
for an introduction to context-free grammars.

```
p0 ::= <p2> <p1>
p1 ::= '+' <p2> <p1>
   ::= '-' <p2> <p1>
   ::= e
p2 ::= <p4> <p3>
p3 ::= '*' <p4> <p3>
   ::= '/' <p4> <p3>
   ::= '%' <p4> <p3>
   ::= '^' <p4> <p3>
   ::= e
p4 ::= <p6> <p5>
p5 ::= 'min' <p6> <p5>
   ::= 'max' <p6> <p5>
   ::= e
p6 ::= '(' <p0> ')'
   ::= <symbol>
   ::= <number>
```

### Tokens
The following nontrivial tokens are specified as regular expressions.
```
<symbol> = [a-zA-Z]+
<number> = \d+|\d+\.\d+
```

### Examples
```
x + 1
```

```
(a ^ 2 + b ^ 2) ^ (1 / 2)
```

## Usage
### Compiler `rpnc`
The compiler reads an expression on `stdin` conforming to the aforementioned grammar and emits a
sequence of instructions that can be executed by the interpreter.

Example:
```
$ echo "x + 1" | rpnc
sym x
pushsym x
push 1.0
add 2
```

To see other options:
```
$ rpnc -?
```

The compiler can also be instructed to only tokenize the input or only produce a syntax tree, which
means the output cannot be executed by the interpreter. These features are provided to examine the
output from various phases of the compilation pipeline.

To tokenize only, which emits a list of tokens, use `-t` option:
```
$ echo "x + 1" | rpnc -t
SymbolToken(x)
PlusToken
NumberToken(1)
```

To parse only, which emits a syntax tree, use `-p` option:
```
$ echo "x + 1" | rpnc -p
Add
  Symbol(x)
  Number(1.0)
```

Finally, the compiler can be instructed to perform an optimization over the sequence of instructions,
which is disabled by default. The methods of optimization are documented in the source code (see
`Optimizer.scala`).

An unoptimized compilation:
```
$ echo "x + 1 + y + 2" | rpnc
sym x
sym y
pushsym x
push 1.0
add 2
pushsym y
add 2
push 2.0
add 2
```

An optimized compilation using the `-o` option:
```
$ echo "x + 1 + y + 2" | rpnc
sym x
sym y
pushsym x
pushsym y
push 3.0
add 3
```

### Interpreter `rpn`

## License
Copyright 2015 David Edwards

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

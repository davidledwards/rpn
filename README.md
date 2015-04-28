# RPN Compiler and Interpreter
A simple RPN compiler and interpreter for instructional purposes.

## Overview
This project was developed for use in a Cerner DevCon talk. The essential goal of this project is to
demonstrate construction of a simple compiler using a purely functional style. With the exception of
I/O side effects to accept expressions as input and various steps that produce output, the compiler
is purely functional.

The compiler is designed to accept very simple expressions in standard infix notation and generate
instructions that an interpreter can evaluate in postfix form using a stack, hence the reference to
RPN (reverse polish notation).

## Building
This project is built using [sbt](http://www.scala-sbt.org/0.13/tutorial/Setup.html). After installing
`sbt`, clone the repository and issue the command `sbt packArchive` in the root directory of the project.
This produces a redistributable assembly in the form of a single file, either
`target/rpn-<version>.tar.gz` or `target/rpn-<version>.zip`.

Unpacking the assembly produces a directory structure with the following format:
```
rpn-<version>/
+ bin/
  + rpnc
  + rpnc.bat
  + rpn
  + rpn.bat
+ lib/
  ...
```

For convenience, you might place `rpn-<version>/bin/rpnc` and `rpn-<version>/bin/rpn` in your `PATH` or
create an alias.

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

## Instructions
The interpreter understands the following instruction set, a sequence of which is produced by the
compiler. Each instruction must be separated by a newline and there can be no blank lines. The
instruction loader is tolerant to extraneous whitespace on a per-instruction basis, such as leading
or trailing spaces.
```
sym <symbol>
pushsym <symbol>
push <number>
add <args>
sub <args>
mul <args>
div <args>
min <args>
max <args>
mod
pow
nop
```

### `sym <symbol>`
Declares `<symbol>`, which allows the interpreter to bind `<symbol>` to a value in its execution
environment.

### `pushsym <symbol>`
Pushes the value of `<symbol>` onto the evaluation stack, which implies that it must have been
previously bound with the `sym` instruction.

### `push <number>`
Pushes `<number>` onto the evaluation stack.

### `add <args>`
Pops `<args>` operands from the evaluation stack, computes the sum, and pushes the result
onto the evaluation stack.

### `sub <args>`
Pops `<args>` operands from the evaluation stack, computes the difference, and pushes the result
onto the evaluation stack.

### `mul <args>`
Pops `<args>` operands from the evaluation stack, computes the product, and pushes the result
onto the evaluation stack.

### `div <args>`
Pops `<args>` operands from the evaluation stack, computes the quotient, and pushes the result
onto the evaluation stack.

### `min <args>`
Pops `<args>` operands from the evaluation stack, computes the minimum, and pushes the result
onto the evaluation stack.

### `max <args>`
Pops `<args>` operands from the evaluation stack, computes the maximum, and pushes the result
onto the evaluation stack.

### `mod`
Pops two (2) operands from the evaluation stack, computes the remainder, and pushes the result
onto the evaluation stack.

### `pow`
Pops two (2) operands from the evaluation stack, computes the exponentiation, and pushes the result
onto the evaluation stack.

### `nop`
An instruction that has no effect. This is used by the compiler during the optimization phase,
but typically never appears in the final output. Nonetheless, the interpreter will still
recognize this instruction.

## Usage
### Compiler `rpnc`
The compiler reads an expression on `stdin` conforming to the aforementioned grammar and emits a
sequence of instructions to `stdout` that can be executed by the interpreter.

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
$ echo "x + 1 + y + 2" | rpnc -o
sym x
sym y
pushsym x
pushsym y
push 3.0
add 3
```

### Interpreter `rpn`
The interpreter reads and evaluates a sequence of instructions from `stdin` and writes the result
to `stdout`.

Given the input file `foo.i`:
```
push 1
push 2
add 2
```

The interpreter evaluates instructions in `foo.i`:
```
$ cat foo.i | rpn
3.0
```

Another method is to pipe the output of the compiler as input to the interpreter:
```
$ echo "1 + 2" | rpnc | rpn
3.0
```

Typically, an instruction sequence will have symbol declarations that must be bound to values
before the interpreter can evaluate. This is achieved by passing pairs of symbol/value bindings as
arguments to the interpreter. Consider the expression `(a ^ 2 + b ^ 2) ^ (1 / 2)`, which computes
the hypotenuse of a right triangle. The lengths of the other sides, `a` and `b`, must be provided
during evaluation.

Symbols are bound to values by passing as arguments:
```
$ echo "(a ^ 2 + b ^ 2) ^ (1 / 2)" | rpnc | rpn a 4 b 3
5.0
```

To see other options:
```
$ rpn -?
```

The interpreter can be asked to produce a list of symbols occurring in the instruction sequence
provided as input. If so, it does not attempt evaluation. This option is useful to discover
symbols that must be bound prior to evaluation.

To print the list of symbols, use the `-s` option:
```
$ echo "(a ^ 2 + b ^ 2) ^ (1 / 2)" | rpnc | rpn -s
a
b
```

## Internals
In order to demonstrate the use of functional style, the compiler is essentially a composition of
functions that represent the various stages one might see in a typical design.

The stages of compilation and their order of execution follows:
* Tokenize
* Parse
* Generate
* Optimize

The output of a prior stage is input to the next, as can be seen by the type signatures of each
function.

### Tokenize: `Stream[Char]` => `Stream[Token]`
Tokenization, or lexical analysis, transforms a stream of characters into a stream of tokens.

### Parse: `Stream[Token]` => `AST`
Parsing transforms a stream of tokens into an abstract syntax tree.

### Generate: `AST` => `Seq[Code]`
Generation transforms an abstract syntax tree into a sequence of unoptimized instructions.

### Optimize: `Seq[Code]` => `Seq[Code]`
Optimization transforms a sequence of instructions into a sequence of optimized instructions.

These functions are composed to form a pipeline that conforms to the following type signature:
`Stream[Char]` => `Seq[Code]`. In essence, an expression in the input stream can be fed into the
pipeline, producing a sequence of instructions that can be written to the output stream.

The only necessary side effects that occur in this program are those that exist in
`Compiler.scala` and `Interpreter.scala`, in which `stdin` and `stdout` are wired into their
respective pipelines.

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

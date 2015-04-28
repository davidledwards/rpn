# RPN Compiler and Interpreter
A simple RPN compiler and interpreter for instructional purposes.

## Overview
This project was developed for use in a Cerner DevCon talk. The essential goal of this project is to
demonstrate construction of a simple compiler using a purely functional style. With the exception of
I/O side effects to accept expressions as input and various steps that produce output, the compiler
is purely functional.

## Grammar
The language recognized by the compiler is specified by the following BNF-style grammar. The input
is essentially simple mathematical expressions using infix notation.
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

### Examples
```
x + 1
```

```
(a ^ 2 + b ^ 2) ^ (1 / 2)
```

## Usage
### Compiler
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

### Interpreter
```
```

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

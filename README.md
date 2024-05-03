# Build Instructions

Run `cabal build Crisp` to build the project. The program allows for two modes of execution, either a script or a REPL. A script can be executed by using 

```
./run-script.sh <script>
```

or 

```
cabal exec Crisp -- -s <script>
```

While the REPL can be launched by

```
./run-repl.sh
```

or 

```
cabal exec Crisp -- -r
```

# Documentation

The language uses [S-expressions](https://en.wikipedia.org/wiki/S-expression) to represent to represent the code. A program is a series of valid S-expressions and its result is the result of the last S-expression. The features we've implemented include

## In-built functions

- Numeric operators: `+`, `*`, `-`, `/`
- String concatenation: `++`
- Comparison operators: `<`, `>`, `<=`, `>=`, `==`, `!=`
- Logical operators: `&&`, `||`, `!`

## Define statement

Syntax: `(define var <expr>)`

This just binds the variable `var` to the expression `expr` which can then be used in the following statements. For example

```
(define y 1)
(define y (+ 2 2))
```

## Let expression

Syntax: `(let (<space_separated_key_value_pairs>) <expr>)`

This binds the keys to the given values and then evaluates `expr` with these bindings in mind. Note the language has lexical scoping so all previous bindings are shadowed. For example

```
(let (y 42 x 2) (* y x))
```

## Begin statement

Syntax: `(begin <statements>)`

This statement can be used to use a new fresh environment for execution. For example

```
(begin
    (define y 1)
    (define add (lambda (x) (+ x y)))
    (let (y 10) (add y))
)
// begin separates env
(begin
    (define foo (lambda (x) 2))
    (let (y 42) (foo 0))
)
```

## Lambda function

Syntax: `(lambda <parameter_list> <expr>)`

This makes an anonymous lambda function with parameters as in `parameter_list` and function body as `expr`. For example

```
(define append (lambda (x) (++ x "world")))
(append "hello ")
```

## If statement

Syntax: `(if <cond> <true_expr> <false_expr>)`

Just your regular if statement which evaluates to `true_expr` if `cond` evaluates to `true` and `false_expr` otherwise. For example

```
// functions are first-class objects so you can pass them as arguments
// and even pass a function to itself
(define fact (lambda (x f) (if (== x 0) 1 (* x (f (- x 1) f)))))
(fact 5 fact)
```

## Comments

We've used C-style comments i.e., single line comments start with `//` and multi-line comments are enclosed in `/* ... */`

# Examples

```
(define append (lambda (x) (++ x "world")))
(append "hello ")

// output: "hello world"
```

```
(&& (< 1 2) (|| (>= 3 4) (== 4 4) (!= 2 3)) true)

// output: True
```

```
(define x 2)
(define y (* x 3))
(+ x y (/ 5 -3))

// output: 6
```

```
// recursive function
(define fact (lambda (x f) (if (== x 0) 1 (* x (f (- x 1) f)))))
(fact 5 fact)

// output: 120
```

```
(begin
    (define y 1)
    (define add (lambda (x) (+ x y)))
    (let (y 10) (add y))
)

// output: 11
```

```
(begin
    (define y 1)
    (define add (lambda (x) (+ x y)))
    (let (y 10) (add y))
)
// begin separates env
(begin
    (define foo (lambda (x) y))
    (let (y 42) (foo 0))
)

// output: Crisp: Unbound variable: y
```





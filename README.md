# Mini FORTH Interpreter

## Overview
This project implements a minimal FORTH interpreter in Haskell. The interpreter evaluates stack-based FORTH programs using Reverse Polish Notation (RPN). The interpreter processes .4TH files and produces output according to the defined built-in operations.

## How To Run 
I have created a Makefile so we can use that to do all the things.
```
clean
make clean

build
make build

run unit tests
make unit

run single test cases in test folder
make run FILE={fileName without .4TH} (make run FILE=t1)

run all test cases
make run_all
```

## Project Structure
Main.hs          -> Program entry point
Interpret.hs     -> Interpreter logic (tokenization + evaluation)
Eval.hs          -> Built-in operators and functions
Val.hs           -> Value definitions and helpers
ValSpec.hs       -> Unit tests for Val module
EvalSpec.hs      -> Unit tests for Eval module
InterpretSpec.hs -> Unit tests for Interpret module
tests/           -> Functional test files (.4TH and .out)
Makefile         -> Build and test automation
FORTH.cabal      -> Cabal configuration

## Implemented Built-in Operations
- Arithmetic (+,-,*,/,^)
- Stack Operations (DUP)
- Output Operations
  - . (print top of stack)
  - EMIT (print ASCII character)
  - CR (newline)
- String Operations
  - STR (convert value to string)
  - CONCAT2
  - CONCAT3

- Bonus: User-Defined Words
  - The interpreter supports defining new FORTH words using standard FORTH syntax:
    ```
    : NAME body ;
    Example
    : SQUARE DUP * ;
    5 SQUARE .
    
    Output:
    25
    ```
  - I also did nested Definations.
    ```
    : SQUARE DUP * ;
    : FOURTH SQUARE SQUARE ;
    2 FOURTH .

    Output:
    16
    ```

## Design Notes
- The interpreter is purely functional.
- No global mutable state is used.
- Interpreter state includes:
  - Stack
  - Output string
  - A Map storing user-defined word definitions
  - When a user-defined word is encountered, it is expanded into its stored body and executed.
  - Definitions are stored using Data.Map from the containers library.

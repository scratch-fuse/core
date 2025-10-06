# FUSE Language Specification

This document describes the syntax and semantics of the FUSE programming language using Extended Backus-Naur Form (EBNF).

## Table of Contents

- [Lexical Elements](#lexical-elements)
- [Syntax](#syntax)
- [Semantics](#semantics)
- [Built-in Namespaces](#built-in-namespaces)

## Lexical Elements

### Tokens

```ebnf
Token = Number | String | Identifier | Keyword | Operator | Punctuation | Comment | Eol ;
```

### Numbers

```ebnf
Number = DecimalNumber | HexNumber ;

DecimalNumber = Digit+ [ "." Digit+ ] [ Exponent ] ;

HexNumber = "0" ("x" | "X") HexDigit+ ;

Exponent = ("e" | "E") [ "+" | "-" ] Digit+ ;

Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;

HexDigit = Digit | "a" | "b" | "c" | "d" | "e" | "f" | "A" | "B" | "C" | "D" | "E" | "F" ;
```

### Strings

```ebnf
String = '"' StringChar* '"' | "'" StringChar* "'" ;

StringChar = EscapeSequence | (AnyChar - ('"' | "'" | "\\")) ;

EscapeSequence = "\\" ("n" | "t" | "r" | '"' | "'" | "\\" | UnicodeEscape) ;

UnicodeEscape = "u" HexDigit HexDigit HexDigit HexDigit ;
```

### Identifiers

```ebnf
Identifier = Letter (Letter | Digit | "_")* ;

Letter = UnicodeLetterCategory ;
```

### Keywords

```ebnf
Keyword = "fn" | "let" | "global" | "if" | "else" | "while" | "for" | "loop"
        | "return" | "true" | "false" | "once" | "namespace" ;
```

### Operators

```ebnf
Operator = ".." | "+" | "-" | "*" | "/" | "%"
         | "=" | "==" | "!=" | "<" | ">" | "<=" | ">="
         | "&&" | "||" | "!" | "."
         | "->" | "+=" | "-=" | "*=" | "/=" | "%=" | "..="
         | "++" | "--" ;
```

### Punctuation

```ebnf
Punctuation = "(" | ")" | "{" | "}" | "[" | "]" | "," | ":" | "@" ;
```

### Comments

```ebnf
Comment = SingleLineComment | MultiLineComment ;

SingleLineComment = "//" (AnyChar - "\n")* ;

MultiLineComment = "/*" (AnyChar | MultiLineComment)* "*/" ;
```

### End of Line

```ebnf
Eol = "\n" | ";" ;
```

## Syntax

### Program Structure

```ebnf
Program = TopLevelStatement* ;

TopLevelStatement = VariableDeclaration
                  | FunctionDeclaration
                  | HatBlock
                  | NamespaceDeclaration ;
```

### Declarations

#### Variable Declaration

```ebnf
VariableDeclaration = ("let" | "global") Identifier "=" Expression Eol ;
```

#### Function Declaration

```ebnf
FunctionDeclaration = [ DecoratorStatement ] "fn" Identifier "(" ParameterList? ")" [ "once" ] "->" Type BlockStatement ;

ParameterList = Parameter ("," Parameter)* ;

Parameter = Identifier ":" Type ;

Type = Identifier ;
```

#### Namespace Declaration

```ebnf
NamespaceDeclaration = "namespace" Identifier "=" "{" NamespaceBody "}" ;

NamespaceBody = NamespaceProperty* ;

NamespaceProperty = Identifier "=" NamespaceValue Eol ;

NamespaceValue = Object | Array | String | Number | Boolean ;

Object = "{" [ ObjectProperty ("," ObjectProperty)* ] "}" ;

ObjectProperty = String ":" NamespaceValue ;

Array = "[" [ NamespaceValue ("," NamespaceValue)* ] "]" ;

Boolean = "true" | "false" ;
```

### Decorators

```ebnf
DecoratorStatement = "@" Identifier "(" ArgumentList? ")" Target ;

Target = FunctionDeclaration | VariableDeclaration ;

ArgumentList = Literal ("," Literal)* ;
```

**Note**: Currently, only the `@export` decorator is supported.

### Hat Blocks (Event Handlers)

```ebnf
HatBlock = Expression BlockStatement ;
```

Examples:

```fuse
event.start { /* statements */ }
event.keyPressed("space") { /* statements */ }
```

### Statements

```ebnf
Statement = VariableDeclaration
          | AssignmentStatement
          | IncrementStatement
          | IfStatement
          | WhileStatement
          | ForStatement
          | LoopStatement
          | ReturnStatement
          | BlockStatement
          | ExpressionStatement
          | NoopStatement ;
```

#### Assignment Statement

```ebnf
AssignmentStatement = Expression AssignmentOperator Expression Eol ;

AssignmentOperator = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "..=" ;
```

#### Increment/Decrement Statement

```ebnf
IncrementStatement = IncrementOperator Expression Eol
                   | Expression IncrementOperator Eol ;

IncrementOperator = "++" | "--" ;
```

#### If Statement

```ebnf
IfStatement = "if" "(" Expression ")" Statement [ "else" Statement ] ;
```

#### While Statement

```ebnf
WhileStatement = "while" "(" Expression ")" Statement ;
```

#### For Statement

```ebnf
ForStatement = "for" "(" [ ForInit ] ";" [ Expression ] ";" [ ForUpdate ] ")" Statement ;

ForInit = AssignmentStatement | IncrementStatement ;

ForUpdate = AssignmentStatement | IncrementStatement ;
```

#### Loop Statement

```ebnf
LoopStatement = "loop" Statement ;
```

#### Return Statement

```ebnf
ReturnStatement = "return" [ Expression ] Eol ;
```

#### Block Statement

```ebnf
BlockStatement = "{" Statement* "}" ;
```

#### Expression Statement

```ebnf
ExpressionStatement = Expression Eol ;
```

#### Noop Statement

```ebnf
NoopStatement = Eol ;
```

### Expressions

```ebnf
Expression = LogicalOrExpression ;

LogicalOrExpression = LogicalAndExpression ( "||" LogicalAndExpression )* ;

LogicalAndExpression = EqualityExpression ( "&&" EqualityExpression )* ;

EqualityExpression = ComparisonExpression ( ("==" | "!=") ComparisonExpression )* ;

ComparisonExpression = TermExpression ( (">" | ">=" | "<" | "<=") TermExpression )* ;

TermExpression = FactorExpression ( ("+" | "-" | "..") FactorExpression )* ;

FactorExpression = UnaryExpression ( ("*" | "/" | "%") UnaryExpression )* ;

UnaryExpression = ( ("+" | "-" | "!") UnaryExpression ) | CallExpression ;

CallExpression = PrimaryExpression ( "(" ArgumentList? ")" [ BlockStatement ]
                                   | "[" Expression "]"
                                   | "." Identifier )* ;

PrimaryExpression = Literal
                  | Identifier
                  | ArrayLiteral
                  | "(" Expression ")" ;

ArgumentList = Expression ("," Expression)* ;

ArrayLiteral = "[" [ Expression ("," Expression)* ] "]" ;

Literal = Number | String | Boolean ;
```

## Semantics

### Top-Level Statements

The FUSE program structure allows only the following at the top level:

1. **Variable Declarations** (`let` or `global`)
   - `let` declares a local variable (sprite/stage specific)
   - `global` declares a global variable (accessible across all sprites)

2. **Function Declarations** (`fn`)
   - Define custom blocks/procedures
   - Can have `once` modifier to indicate the function runs without screen refresh

3. **Hat Blocks** (Event Handlers)
   - Event-driven code execution
   - Syntax: `expression { statements }`
   - Examples: `event.start { ... }`, `event.keyPressed("space") { ... }`

4. **Namespace Declarations**
   - Define configuration or mapping data
   - Used for metadata or built-in block definitions

### Variable Scoping

- **`global` variables**: Shared across all sprites and the stage
- **`let` variables**: Local to the current sprite or stage
- **Function parameters**: Local to the function scope

### Type System

FUSE uses a simple type system:

- `void`: No return value
- `any`: Any Scratch value (string, number, boolean)
- `bool`: Boolean values (`true` or `false`)

### Decorators

#### `@export` Decorator

The `@export` decorator is used to export Scratch blocks.

```fuse
@export("test [params]") fn functionName(params: any) once -> returnType {
  // The function will appear as a Scratch block with the given name and parameters
}

@export("variable description") let variableName = initialValue ;
```

### Operators

#### Arithmetic Operators

- `+`: Addition
- `-`: Subtraction
- `*`: Multiplication
- `/`: Division
- `%`: Modulo

#### String Operators

- `..`: String concatenation

#### Comparison Operators

- `==`: Equal
- `!=`: Not equal
- `<`: Less than
- `>`: Greater than
- `<=`: Less than or equal
- `>=`: Greater than or equal

#### Logical Operators

- `&&`: Logical AND
- `||`: Logical OR
- `!`: Logical NOT

#### Assignment Operators

- `=`: Assignment
- `+=`, `-=`, `*=`, `/=`, `%=`, `..=`: Compound assignment

#### Increment/Decrement

- `++`: Increment by 1
- `--`: Decrement by 1

### Control Flow

#### If-Else

```fuse
if (condition) {
  // statements
} else {
  // statements
}
```

#### While Loop

```fuse
while (condition) {
  // statements
}
```

#### For Loop

```fuse
for (init; condition; update) {
  // statements
}
```

#### Infinite Loop

```fuse
loop {
  // statements
}
```

### Function Calls

Functions can be called with arguments:

```fuse
functionName(arg1, arg2, arg3)
```

Some Scratch blocks can have a "then" block:

```fuse
someBlock(args) {
  // then statements
}
```

### Member Access

Access namespace members or object properties:

```fuse
namespace.property
object.method(args)
array[index]
```

## Built-in Namespaces

FUSE provides several built-in namespaces that map to Scratch blocks:

- `motion`: Motion blocks
- `looks`: Looks blocks
- `sound`: Sound blocks
- `event`: Event blocks (hat blocks)
- `control`: Control blocks
- `sensing`: Sensing blocks
- `math`: Math operations
- `pen`: Pen extension blocks
- `translate`: Translation blocks

For detailed API documentation, see the `builtin.fuse` file.

## Examples

### Basic Function

```fuse
fn greet(name: any) once -> void {
  looks.say("Hello, " .. name .. "!")
}

event.start {
  greet("World")
}
```

### Variable Declaration

```fuse
global score = 0
let lives = 3

fn updateScore(points: any) once -> void {
  score += points
}
```

### Control Flow

```fuse
fn checkWinCondition() once -> void {
  if (score >= 100) {
    looks.say("You win!")
  } else if (score >= 50) {
    looks.say("Almost there!")
  } else {
    looks.say("Keep going!")
  }
}
```

### Loops

```fuse
fn countdown() once -> void {
  let i = 10
  while (i > 0) {
    looks.say(i)
    i--
  }
  looks.say("Blast off!")
}

fn processArray() once -> void {
  global items = [1, 2, 3, 4, 5]
  for (let i = 0; i < items.length; i++) {
    looks.say(items[i])
  }
}
```

### exportal Blocks

```fuse
@export("custom block [arg1] and [arg2]")
fn customBlock(arg1: any, arg2: any) once -> any {
  return arg1 + arg2
}
```

## Grammar Summary (Complete EBNF)

```ebnf
(* Program Structure *)
Program = TopLevelStatement* ;
TopLevelStatement = VariableDeclaration | FunctionDeclaration | HatBlock | NamespaceDeclaration ;

(* Declarations *)
VariableDeclaration = ("let" | "global") Identifier "=" Expression Eol ;
FunctionDeclaration = [ DecoratorStatement ] "fn" Identifier "(" ParameterList? ")" [ "once" ] "->" Type BlockStatement ;
ParameterList = Parameter ("," Parameter)* ;
Parameter = Identifier ":" Type ;
Type = Identifier ;

(* Decorators *)
DecoratorStatement = "@" Identifier "(" ArgumentList? ")" Target ;
Target = FunctionDeclaration | VariableDeclaration ;

(* Hat Blocks *)
HatBlock = Expression BlockStatement ;

(* Statements *)
Statement = VariableDeclaration | AssignmentStatement | IncrementStatement
          | IfStatement | WhileStatement | ForStatement | LoopStatement
          | ReturnStatement | BlockStatement | ExpressionStatement | NoopStatement ;

AssignmentStatement = Expression AssignmentOperator Expression Eol ;
AssignmentOperator = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "..=" ;

IncrementStatement = IncrementOperator Expression Eol | Expression IncrementOperator Eol ;
IncrementOperator = "++" | "--" ;

IfStatement = "if" "(" Expression ")" Statement [ "else" Statement ] ;
WhileStatement = "while" "(" Expression ")" Statement ;
ForStatement = "for" "(" [ ForInit ] ";" [ Expression ] ";" [ ForUpdate ] ")" Statement ;
ForInit = AssignmentStatement | IncrementStatement ;
ForUpdate = AssignmentStatement | IncrementStatement ;
LoopStatement = "loop" Statement ;
ReturnStatement = "return" [ Expression ] Eol ;
BlockStatement = "{" Statement* "}" ;
ExpressionStatement = Expression Eol ;
NoopStatement = Eol ;

(* Expressions *)
Expression = LogicalOrExpression ;
LogicalOrExpression = LogicalAndExpression ( "||" LogicalAndExpression )* ;
LogicalAndExpression = EqualityExpression ( "&&" EqualityExpression )* ;
EqualityExpression = ComparisonExpression ( ("==" | "!=") ComparisonExpression )* ;
ComparisonExpression = TermExpression ( (">" | ">=" | "<" | "<=") TermExpression )* ;
TermExpression = FactorExpression ( ("+" | "-" | "..") FactorExpression )* ;
FactorExpression = UnaryExpression ( ("*" | "/" | "%") UnaryExpression )* ;
UnaryExpression = ( ("+" | "-" | "!") UnaryExpression ) | CallExpression ;
CallExpression = PrimaryExpression ( "(" ArgumentList? ")" [ BlockStatement ] | "[" Expression "]" | "." Identifier )* ;
PrimaryExpression = Literal | Identifier | ArrayLiteral | "(" Expression ")" ;
ArgumentList = Expression ("," Expression)* ;
ArrayLiteral = "[" [ Expression ("," Expression)* ] "]" ;
Literal = Number | String | Boolean ;

(* Lexical Elements *)
Number = DecimalNumber | HexNumber ;
DecimalNumber = Digit+ [ "." Digit+ ] [ Exponent ] ;
HexNumber = "0" ("x" | "X") HexDigit+ ;
Exponent = ("e" | "E") [ "+" | "-" ] Digit+ ;
String = '"' StringChar* '"' | "'" StringChar* "'" ;
StringChar = EscapeSequence | (AnyChar - ('"' | "'" | "\\")) ;
EscapeSequence = "\\" ("n" | "t" | "r" | '"' | "'" | "\\" | UnicodeEscape) ;
UnicodeEscape = "u" HexDigit HexDigit HexDigit HexDigit ;
Identifier = Letter (Letter | Digit | "_")* ;
Boolean = "true" | "false" ;

(* Namespace Declaration *)
NamespaceDeclaration = "namespace" Identifier "=" "{" NamespaceBody "}" ;
NamespaceBody = NamespaceProperty* ;
NamespaceProperty = Identifier "=" NamespaceValue Eol ;
NamespaceValue = Object | Array | String | Number | Boolean ;
Object = "{" [ ObjectProperty ("," ObjectProperty)* ] "}" ;
ObjectProperty = String ":" NamespaceValue ;
Array = "[" [ NamespaceValue ("," NamespaceValue)* ] "]" ;
```

# Assignemnt - 2

To run type:

```bash
make run
```

This will create executable by name a2

Run the executable by

```bash
./a2 "filename"
```

this will print 2 lines.

First will be tokens identified

Second will be postorder traversal of tree

In case of errors extra lines with type of error and location of error is printed

Terminals : without quotes or angular brackerts

Non-terminals : with angular brackets

Actual value of token : with double quotes

### Test Cases:

#### Parser Error:

```bash
IF x EQUALS z THEN TRUE ELSE a a XOR b;
```

syntax error prints following things:

````bash
Syntax Error:1:34:found at ID
unhandled exception: ParseError```
````

first line is list of tokens

second line is syntax error location and token which led to the error

##### Lexer Error:

```bash
IF x EQUALS z1 THEN TRUE ELSE a a XOR b;
```

lexer error produces location of error message:

```bash
[ IF "IF", ID "x", EQUALS "EQUALS", ID "z", Unknown token:1:15:1, unhandled exception: LexError
```

#### Large input:

#### Manual Parse tree checking:

![](!./image/README/index.jpeg)

```bash
A AND B;
```

```bash
[ ID "A", AND "AND", ID "B", TERM ";", ]
"A", <ID>, <formula>, "AND", AND, "B", <ID>, <formula>, <formula>, ";", <TERM>, <statement>, <program>
```

Which matches with the tree generated

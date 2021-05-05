# What's this?
This is a Git repository for *Topos*, which is a programming language designed to be able to treat set and topology (mathematical features are under development).

# How to install
To install Topos, you need [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

1. Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
2. Pull this Git repository.
3. ```cd Topos``` and run ```stack build``` on terminal.
4. Run ```stack install``` to install Topos.

# How to use
## Interactive mode
You can simply type ```topos``` on terminal to start the interactive shell of Topos.
In the interactive mode, you may directly input expressions like ```(3 + 5) / 2``` and ```3 * succ 4```, then Topos evaluate them and display results.

To quit the interactive mode, press ```Ctrl-D```.

## Script mode
If you specify path of your script file after ```topos``` command, you can run Topos with the script mode.

Let us look at simple "Hello, world!" example.
First, craete ```hello.top``` file and write following:

```hello.top
print "Hello, world!"
```

Then run ```topos hello.top``` in the same directry to see the expected output.
You may also check ```test.top``` file to see some examples of Topos code.

In script mode, error occurence stops the execution of script.

# Basic concepts
## Types
Topos has following basic types.

|Type Name|Formal Expression|    Example    |
|---------|-----------------|---------------|
|   Int   |       Int       |       5       |
| Double  |      Double     |     3.14      |
| String  |      String     |"Hello, world!"|
| Bool    |       Bool      |     True      |
|Function |Function <t1,..., tn -> t>|Function <Int -> Int>: x -> x * x|
|  List   |      List t     |    [2,3,5]    |
|  Tuple  |   (t1,..., tn)  |("Takaoka", 31)|

Number without decimal point will be automatically interpreted as integer type.

## Operators
Here's a list of basic topos operators.

|Operator|  Description  | Class |Priority|Example Usage|
|--------|---------------|-------|--------|-----------|
|```\|\|```|Logical "or"|BinaryOp|1|```4 < 2 \|\| 4 > 5``` => ```False```|
|```&&```|Logical "and"|BinaryOp|2|```4 >= 2 && 4 <= 5``` => ```True```|
|```!```|Logical "not"|UnaryOp|3|```!(3 > 0)``` => ```False```|
|```==```|Return ```True``` if both sides are equal, otherwise ```False```|BinaryOp|4|```2 + 3 == 5``` => ```True```|
|```!=```|Return ```True``` if both sides are not equal, otherwise ```False```|BinaryOp|4|```2 + 3 != 5``` => ```False```|
|```>```|Return ```True``` if LHS is greater than RHS, otherwise ```False```|BinaryOp|4|```4 > 5``` => ```False```|
|```>=```|Return ```True``` if LHS is greater than or equal to RHS, otherwise ```False```|BinaryOp|4|```2 + 3 >= 5``` => ```True```|
|```<```|Return ```True``` if LHS is less than RHS, otherwise ```False```|BinaryOp|4|```4 < 5``` => ```True```|
|```<=```|Return ```True``` if LHS is less than or equal to RHS, otherwise ```False```|BinaryOp|4|```2 + 3 <= 5``` => ```True```|
|```+```|Add two numbers / Concatenate two lists or strings|BinaryOp|5|```2 + 3``` => ```5``` / ```"foo" + "bar"``` => ```foobar```|
|```-```|Subtract the right number from the left number / Change sign of number|BinaryOp/UnaryOp|5|```5 - 2``` => ```3``` / ```4 * (-2)``` => ```-8```|
|```*```|Multiply two numbers|BinaryOp|6|```3 * 5``` => ```15```|
|```/```|Divide the left number by the right number (the result is a floating point number)|BinaryOp|6|```5 / 2``` => ```2.5```|
|```//```|Divide the left number by the right number and omit the part after decimal point|BinaryOp|6|```5 // 2``` => ```2```|
|```%```|Yields the remainder from the division of the left number by the right number|BinaryOp|6|```8 % 3``` => ```2```|
|```^```|Returns ```x``` to the power of ```y```, where ```x``` and ```y``` are the first and second argument, respectively|BinaryOp|7|```4 ^ 0.5``` => ```2.0```|
|```$```|Converts the input to string|UnaryOp|8|```"I'm " + $(25 + 6) + " years old."``` => ```"I'm 31 years old."```|

All operators in Topos are left associative.
Priority 8 is the highest priority.

## Functions
In Topos, functions have the same priority, which is higher than that of operators (except for ```print``` function).
That is, ```print``` function has priority 0 (lowest), and other functions (predefined and user defined) have priority 9 (highest).

Basic predefined functions are follows:

| Function name | Signature |Description | Example Usage |
|---------------|-----------|------------|---------------|
|```print```|```Int/Double/Bool/String -> Print```|Evaluates given expression and prints result to the display|```print (3 + 5)``` => ```8```|
|```succ```|```Int -> Int``` |Adds one to the given integer and returns it|```succ 5``` => ```6```|
|```floor```|```Double -> Int```|Retuens the largest integer which doesn't exceed the given number|```floor 5.3``` => ```Int 5```|
|```toDouble```|```Int -> Double```|Converts the given integer to Double type|```toDouble 5``` => ```Double 5.0```|
|```head```|```List a -> a```|Returns the first element of given list|```head [1,2,3]``` => ```1```|
|```tail```|```List a -> List a```|Omits the first element of given list and returns it|```tail [1,2,3]``` => ```[2,3]```|
|```pop```|```List a -> (a, List a)```|Returns the tuple compsists of first element of the given list and the rest list|```pop [1,2,3]``` => ```(1,[2,3])```|
|```length```|```List a -> Int```|Returns the number of elements of given list|```length [1,2,3]``` => ```3```|
|```isEmpty```|```List a -> Bool```|Returns ```True``` if the given list is empty and ```False``` otherwise|```isEmpty []``` => ```True```|
|```take```|```Int, List a -> List a```|Returns list consists of initial ```n``` elements of the list given in the second argument, where ```n``` is integer specified in the first argument|```take 2 ["foo", "bar", "baz"]``` => ```["foo", "bar"]```|
|```drop```|```Int, List a -> List a```|Removes initial ```n``` elements from the list given in the second argument, where ```n``` is integer specified in the first argument|```drop 2 ["foo", "bar", "baz"]``` => ```["baz"]```|
|```seq```|```Int, Int -> List a```|Returns integer list starts with number specified in the first argument, ends with the second argument|```seq 1 3``` => ```[1,2,3]```|
|```map```|```Function <a -> b>, List a -> List b```|Applies function given in the first argument to each element of list given in the second argument and returns it|```map succ [1,2,3]``` => ```[2,3,4]```|
|```fst```|```fst (a, b) -> a```|Returns the first element of given 2-tuple|```fst ("Takaoka", 31)``` => ```"Takaoka"```|
|```snd```|```snd (a, b) -> b```|Returns the second element of given 2-tuple|```snd ("Takaoka", 31)``` => ```31```|

## Comments
In a Topos program, characters placed after '```#```' will be ignored by the interpreter and have no meaning.

```
# This line will be ignored
print "Hello!"  # This part will be ignored
```

## Line transition
In a statement in a Topos program, you can change line by using '```\```' synbol.
For example,

```
define fact as Function <Int -> Int>:\
    x -> if x > 0 then x * (fact (x - 1)) else 1
```

is equivalent to the following code:

```
define fact as Function <Int -> Int>: x -> if x > 0 then x * (fact (x - 1)) else 1
```

## Variables
Any value can be bound to a variable by using ```let``` keyword.
That is, you can set the value ```2.5``` to the variable ```a``` by

```
let a = 2.5
```

Variable identifiers can contain underscores, letters and digits, and must start with a letter.

A ```let``` statement returns a value. For example, the expression

```
if (let a = 4) > 3 then 2 * a else a - 2
```

is evaludated to be ```8```.
If you don't want it to return value, use ```letn```.
Like, 

```
define localVar as Function <Double -> Double>: x ->\
    (letn a = 0.5)\
    (letn b = 2.5)\
    a * x + b
print (localVar 6.0)  # => 5.5
```

## Function definition
There are two ways to define user functions.

The first one is the way using the *function literal*.
The syntax is as following:

```
Function <t_1, ..., t_n -> t_y>: x_1 ... x_n -> y
```

Where, ```x_1, ..., x_n``` are the parameters, and ```y``` is the expression of the return value, ```t_1, ..., t_n``` are the types of ```x_1, ..., x_n```, and ```t_y``` is the type of the return value.

For instance,

```
let sqr = Function <Int -> Int>: x -> x * x
print (sqr 4)  # => 16
```

Function literals can be used alone:

```
(Function <Int -> Int>: x -> x * x) 4  # => 16
```

The second is the way using ```define``` statement.
The syntax is as following:

```
define [functionName] as Function <t_1, ..., t_n -> t_y>: x_1 ... x_n -> y
```

For instance,

```
define fact as Function <Int -> Int>:\
    x -> if x > 0 then x * (fact (x - 1)) else 1
```

Note that the ```define``` statement is not simply a syntax sugar of the first way.
It is the special syntax that the function name can be used in its definition.
Therefore, the following doesn't work.

```
# *** DO NOT DO THIS! ***
let fact = Function <Int -> Int>:\
    x -> if x > 0 then x * (fact (x - 1)) else 1
```
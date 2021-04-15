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

## Operators
Here's a list of basic topos operators.

|Operator|  Description  | Class |Priority|Example of Use|
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
|```$```|Converts input to string|UnaryOp|7|```"I'm " + $(25 + 6) + " years old."``` => ```"I'm 31 years old."```|

All operators in Topos are left associative.
Priority 9 is the most high priority.

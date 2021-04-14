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
In the interactive mode, you may directly input expressions like ```(3 + 5) / 2``` and ```3 * succ 4```, then Topos evaluate and display them.

To quit the interactive mode, press ```Ctrl-D```.

## Script mode
If you specify path of your script file after ```topos``` command, you can run Topos with the script mode.

Let us look at simple "Hello, world!" example.
First, craete ```hello.top``` file and write following:

```hello.top
print "Hello, world!"
```

Then run ```topos hello.top``` in the same directry to see the expected output.

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

You may also check ```test.top``` file to see some examples of Topos code.

# Assignment 8, CS3250

## How to interact with a CatML program

First build CatML with "dune build", then use "dune exec CatML [OPTION]" to use the program.

For a list of options, use "dune exec CatML help".
To run the test expression, use "dune exec CatML test".
To run a .catml file, use "dune exec CatML run [filename]".

#### Modified syntax:

- For a Let statement, the syntax is: "Let [VARIABLE NAME]:[TYPE] = [VALUE]".

- For a Fun statement, the syntax is: "Fun [RETURN TYPE] [VARIABLE NAME]:[VARIABLE TYPE] -> [EXPRESSION]"

- For a Fix statement, the syntax is: "Fix [RETURN TYPE] [FUNCTION NAME].[VARIABLE NAME]:[VARIABLE TYPE] -> [EXPRESSION]"

#### New syntax:

- Strings have been added as a third arith_type, any quote-enclosed text will be read as a string expression by the lexer.

- Integers, Booleans, and Strings can be cast into each-other using the syntax "(type)expression".

- Strings can be concatenated using the "^" symbol.

- Print(string) : Prints a string to the terminal.

- Input() : Prompts the user to enter some text in the terminal, reduces to the inputted string.

- Rand(low, high: Int, Int) : Reduces to a random integer in the range [low, high)

- Expressions can be broken with the ";" symbol. A broken expression will reduce the left-hand side until it is no longer reducable, then jump over to the right-hand side. Only the value of the right-hand side is returned when the break expression is reduced.

- Comments can be written with the "#" symbol. Everything past the first instance of a "#" symbol that isn't inside of a string until the end of the line will be ignored by the parser.

# Reflection Questions

1. If you worked as a group, list all the group members.

N/A

2. Did you use generative AI to assist you in implementing this project? If so, briefly explain how you used it and for which components.

N/A

3. Did you make any design decisions that we should know about?

Let, Fun, and Fix all use static typing. I also implemented a feature to break expressions using a semicolon.

4. Did you implement any of the extra credit features? Briefly summarize your work.

At first I just wanted to implement user input and printing, but in this process I ended up implementing strings and breaks and a bunch of other adjacent things like concatenation.

A full list of the things I added:
- String expressions.
- Concat expressions, concatenates two String expressions into one.
- Print expressions, prints a String expression to the terminal.
- Input expressions, pauses execution to read a line from the terminal and returns a String expression.
- Break expresssions, reduces the left-hand side until it cannot be reduced further, then reduces into the right-hand side expression.
- Rand expressions, takes two Int expressions and reduces to a random number in that range.

I also implemented a rudimentary error handler that catches type, reduction, and parsing errors. For parsing errors, it reports what line the parsing error occurred on (depending on the expression, it might say that the error occurred on the next line, however, because "1\n*2" is a valid expression but "1\n2" is not). For reduction errors, it says what expression it got stuck on and prints out the trace. And for type errors, it says what expression the type could not be resolved for and tries to give a hint as to what the issue was but I didn't write hints for every single possible error, just the ones I figured would be the most common.

5. Any feedback on the structure of this assignment?

I thought this assignment was really neat, but now I kinda wish it was instead a semester-long project that we had built upon through the entire semester. And it would've also been awesome if we had the freedom between building the project from the examples in class and homeworks or doing our own thing entirely (so long as it still met the requirements).
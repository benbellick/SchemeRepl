# SchemeCompiler

This is a basic Scheme REPL which follows [this tutorial](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

To begin the interactive REPL, run with no commands the following:
> stack run

To execute only a single expression, run the following:
> stack run "{expr}"

Note that the proper way to pass the expression in is environment dependent. 
It is up to the user to properly escape necessary characters (e.g. quotes).

Isaac Peters, 	ONID:petersis
Claire Cahill,	ONID:cahillc
Justin Parks,	ONID:parksju


Our language is a statically typed imperative programming language. We chose an imperative language since we wanted the execution order and structure of our program to be clear to users; things happen from top to bottom in the order that the user entered the expressions (with the exception of functions and while loops that can repeat a set of expressions in any location). We chose a statically typed language, which checks over the entered program to type check before any execution, so that we’d have the advantage of only having to evaluate loops/functions once even if they are executed multiple times, theoretically making our language faster to run. 

Running instructions: 
1. open ghci
2. load lang.hs (:l lang.hs)
3. run our test programs, using the following commands

Good Examples: 

    ex1, an example good program to sum all numbers 1 to 100:
     runProg ex1

     expected output: Just (fromList [("n",Left 101),("sum",Left 5050)])

    ex2, an example good program to demonstrate function calling. defines a "square" function and uses it to square 3 and 4
     runProg ex2

     expected output: Just (fromList [("n",Left 16),("x",Left 9)])

     ex4, an example of a good program to demonstrate functionality on custom string functions. It uses the Upper constructor to convert the word "uppercase" to all uppercase letters
        runProg ex4

        expected output: Just (fromList [("upper",Center "UPPERCASE")])

    ex7, an example of a good program that concatenates 2 strings
        runProg ex7

        expected output: Just (fromList [("longString",Center "functional programming is awesome")])

Bad Examples
* all expected outputs: Nothing

    ex3, a type error example
     runProg ex3
    
    ex5, an example of trying to run a recursive function (doesn't work)
     runProg ex5

    ex6, a type error example on string functionality
     runProg ex6
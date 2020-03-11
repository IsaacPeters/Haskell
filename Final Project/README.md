Isaac Peters, 	ONID:petersis
Dakota Cleeves,	ONID:cleevesd
Claire Cahill,	ONID:cahillc
Justin Parks,	ONID:parksju


Our lanuage is called NSTM which is short for Names Suck To Make. The language paradigm we choose is imperative. NSTM is meant to be run on GHCi and you will want to load the Lang module. 

This language includes Expressions, Statements, Lists, and a custom Str data type to represent strings. However, the basic data types in our language are Int and Bool. Expressions include addition, less than or equal to conditionals, negation, and variable reference. There is a helper function for expressions that looks at the arguments and varifies whether it is supposed to be an integer or boolean value, and if the type fails it returns an error. There is a similar type checking function for statements. Statements include binding a variable to an expression, if statements, while loops, and executing a block of statements. The Prog data type is supposed to resemble a program, since it takes a group of declarations and executes a statement on them. Lists have associated functions listCount and listFind, which return integers that show the number of given elements in the list and the first position of the given element, respectively. Errors for these functions are both -1, for simplicity's sake. 

// All token codes are small integers with #defines that begin with "TK_"
%token_prefix TK_

// The type of the data attached to each token is Token.  This is also the
// default type for non-terminals.
//
%token_type {Token}
%default_type {Token}

// The generated parser function takes a 4th argument as follows:
%extra_argument {Parse *pParse}




// The name of a column or table can be any of the following:
//
%type nm {Token}
nm(A) ::= id(A).
nm(A) ::= STRING(A).
nm(A) ::= JOIN_KW(A).



///////////////////// The CREATE TABLE statement ////////////////////////////
//

cmd ::= CreateTableStmt .
CreateTableStmt ::= CREATE OptTemp(T) TABLE OptExists(E) nm(X) dbnm(Y) . {
	transformCreateTableStmt();
}


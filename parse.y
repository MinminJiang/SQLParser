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

%type dbnm {Token}
dbnm(A) ::= .          {A.z=0; A.n=0;}
dbnm(A) ::= DOT nm(X). {A = X;}




///////////////////// The CREATE TABLE statement ////////////////////////////
//

cmd ::= CreateTableStmt .
CreateTableStmt(A) ::= CREATE opttemp(T) TABLE optexists(E) qualified_name(Q) . {
	transformCreateTableStmt();
}
opttemp(A) ::= .   { A = 0; }
opttemp(A) :: = TEMP {A = 1; }

optexists(A) ::=  . { A = 0; } 
optexists(A)




/*****************************************************************************
 *
 *		QUERY:
 *				INSERT STATEMENTS
 *
 *****************************************************************************/

cmd ::= InsertStmt .
InsertStmt(A) ::= opt_with_clause(C) INSERT INTO insert_target(T) insert_rest(R) SelectStmt(S)
opt_on_conflict(N) returning_clause(M) . {
	transfromInsertStmt();
}

opt_with_clause(A) ::= . { A = 0; }

insert_target(A) ::= qualified_name(X) . {;}
insert_target(A) ::= qualified_name(X) AS ColId(Y) . {;}

qualified_name(A) ::= nm(X) dbnm(Y) . {;}

insert_rest(A) ::= . { A = 0; }
insert_rest(A) ::= LP idlist(X) RP . ( A = X; )

idlist(A) ::= idlist(A) COMMA nm(X) . {;}
idlist(A) ::= nm(Y) . {;}


opt_on_conflict(A) ::=  . { A = 0; }

returning_clause(A) ::= . { A = 0; } 







/*****************************************************************************
 *
 *		QUERY:
 *				UPDATE STATEMENTS
 *
 *****************************************************************************/

cmd ::= UpdateStmt .
UpdateStmt(A) ::= opt_with_clause(C) UPDATE relation_expr_opt_alias(R) SET set_clause_list(S)
from_clause(F) where_or_current_clause(W) returning_clause(N) . {
	transfromUpdateStmt();
}

/*****************************************************************************
 *
 *		QUERY:
 *				DELETE STATEMENTS
 *
 *****************************************************************************/

cmd ::= DeleteStmt .
DeleteStmt(A) ::= opt_with_clause(C) DELETE FROM relation_expr_opt_alias(R)
			using_clause(U) where_or_current_clause(W) returning_clause(N) . {
	transformSelectStmt();
}


/*****************************************************************************
 *
 *		QUERY:
 *				SELECT STATEMENTS
 *
 *****************************************************************************/

cmd ::=SelectStmt .
SelectStmt(A) ::= select_no_parens(X) . { A = X; }
SelectStmt(A) ::= select_with_parens(X) . { A = X; } 
select_no_parens(A) ::= simple_select(X) . { A = X; }

simple_select(A) ::= SELECT opt_all_clause(C) opt_target_list(L) into_clause(I) from_clause(F) 
	where_clause(W) group_clause(G) having_clause(H) window_clause(N) . {
	A = tranformSimpleSelect();
}






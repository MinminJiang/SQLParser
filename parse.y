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

//insert opt_with_clause
opt_with_clause(A) ::= . { A = 0; }
opt_with_clause(A) ::= with_clause(X) . { A = X; }


with_clause(A) ::= WITH cte_list(X) . {}
with_clause(A) ::= WITH_LA cte_list(X) . {}
with_clause(A) ::= WITH RECURSIVE cte_list(X) . {}

cte_list(A) ::= common_table_expr(X) . {}
cte_list(A) ::= cte_list(X) COMMA common_table_expr(Y) . {}

common_table_expr(A) ::= name() opt_name_list() AS LP PreparableStmt() RP . {}

name(A) ::= ColId . {}

ColId(A) ::= IDENT. {}
ColId(A) ::= unreserved_keyword() . {}
ColId(A) ::= col_name_keyword() . {}

unreserved_keyword(A) ::= . {}

col_name_keyword(A) ::= . {}

opt_name_list(A) ::= . {}
opt_name_list(A) ::= LP name_list(X) RP . {}

name_list(A) ::=  name(X) . {}
name_list(A) ::= name_list(X) COMMA name(Y) . {}

PreparableStmt(A) ::=  SelectStmt(X) . {}
PreparableStmt(A) ::=  InsertStmt(X) . {}
PreparableStmt(A) ::=  UpdateStmt(X) . {}
PreparableStmt(A) ::=  DeleteStmt(X) . {}

//insert insert_target
insert_target(A) ::= qualified_name(X) . {}
insert_target(A) ::= qualified_name(X) AS ColId(Y) . {}

qualified_name(A) ::= ColId(X) . {;}
qualified_name(A) ::= ColId(X) indirection(Y) . {;}

//insert insert_rest
insert_rest(A) ::= SelectStmt(X) . {}
insert_rest(A) ::= LP insert_column_list(X) RP SelectStmt(Y) . {}
insert_rest(A) ::= DEFAULT VALUES . {}

insert_column_list(A) ::= insert_column_item(X) . {}
insert_column_list(A) ::= insert_column_list(X) COMMA insert_column_item(Y) . {}

insert_column_item(A) ::= ColId(X) opt_indirection(Y) . {}

opt_indirection(A) ::= . {}
opt_indirection(A) ::= opt_indirection(X) indirection_el(Y) . {}

indirection_el(A) ::= DOT attr_name(X) . {}
indirection_el(A) ::= DOT STAR . {}
indirection_el(A) ::= '[' a_expr(X) ']' . {}
indirection_el(A) ::= '[' a_expr ':' a_expr ']' . {}

attr_name(A) ::= ColLabel(X) . {}

ColLabel(A) ::= IDENT . {}
ColLabel(A) ::= unreserved_keyword(X) . {}
ColLabel(A) ::= col_name_keyword(X) . {}
ColLabel(A) ::= type_func_name_keyword(X) . {}
ColLabel(A) ::= reserved_keyword(X) . {}

type_func_name_keyword(A) ::= . {}

reserved_keyword(A) ::= . {}

//insert opt_on_conflict
opt_on_conflict(A) ::= . {}
opt_on_conflict(A) ::= ON CONFLICT opt_conf_expr(X) DO UPDATE SET set_clause_list(Y)	where_clause(Z) . {}
opt_on_conflict(A) ::= ON CONFLICT opt_conf_expr(X) DO NOTHING . {}

opt_conf_expr(A) ::= . {}
opt_conf_expr(A) ::= ON CONSTRAINT name(X) . {}
opt_conf_expr(A) ::= LP index_params RP where_clause . {}

index_params(A) ::= index_elem(X) . {}
index_params(A) ::= index_params(X) COMMA index_elem(Y) . {}

index_elem(A) ::= ColId(X) opt_collate(Y) opt_class(Z) opt_asc_desc(M) opt_nulls_order(N) . {}
index_elem(A) ::= func_expr_windowless(X) opt_collate(Y) opt_class(Z) opt_asc_desc(M) opt_nulls_order(N) . {}
index_elem(A) ::= LP a_expr(X) RP opt_collate(Y) opt_class(Z) opt_asc_desc(M) opt_nulls_order(N) . {}

opt_collate(A) ::= . {}
opt_collate(A) ::= COLLATE any_name(X) . {}

any_name(A) ::= ColId(X) . {}
any_name(A) ::= ColId(X) attrs(Y) . {}

attrs(A) ::=  DOT attr_name(X) . {}
attrs(A) ::=  attrs(X) DOT attr_name(Y) . {}

opt_class(A) ::= . {}
opt_class(A) ::= any_name(X) . {}
opt_class(A) ::= USING any_name() . {}

opt_asc_desc(A) ::= . {}
opt_asc_desc(A) ::= ASC . {}
opt_asc_desc(A) ::= DESC . {}

opt_nulls_order(A) ::=  . {}
opt_nulls_order(A) ::= NULLS_LA FIRST_P . {}
opt_nulls_order(A) ::= NULLS_LA LAST_P . {}

func_expr_windowless(A) ::= func_application(X) . {}
func_expr_windowless(A) ::= func_expr_common_subexpr(X) . {}

func_application(A) ::= func_name(X) LP RP . {}
func_application(A) ::= func_name(X) LP func_arg_list(Y) opt_sort_clause(Z) RP . {}

func_expr_common_subexpr(A) ::= COLLATION FOR LP a_expr RP . {} //TODO FUNCTION PROCESS

func_name(A) ::= type_function_name(X) . {}
func_name(A) ::= ColId(X) indirection(Y) . {}

type_function_name(A) ::= IDENT . {}
type_function_name(A) ::= unreserved_keyword(X) . {}
type_function_name(A) ::= type_func_name_keyword(X) . {}

indirection(A) ::= indirection_el(X) . {}
indirection(A) ::= indirection(X) indirection_el(Y) . {}

func_arg_list(A) ::= func_arg_expr(X) . {}
func_arg_list(A) ::= func_arg_list(X) COMMA func_arg_expr(Y) . {}

func_arg_expr(A) ::= a_expr(X) . {}
func_arg_expr(A) ::= param_name(X) COLON_EQUALS a_expr(Y) . {}
func_arg_expr(A) ::= param_name(X) EQUALS_GREATER a_expr(Y) . {}

a_expr(A) ::= . {} //TODO

param_name(A) ::= type_function_name(X) . {}

opt_sort_clause(A) ::= . {}
opt_sort_clause(A) ::= sort_clause(X) . {}

sort_clause(A) ::= ORDER BY sortby_list(X) . {}

sortby_list(A) ::= sortby(X) . {}
sortby_list(A) ::= sortby_list(X) COMMA sortby(Y) . {}

sortby(A) ::= a_expr(X) USING qual_all_Op(Y) opt_nulls_order(Z) . {}
sortby(A) ::= a_expr(X) opt_asc_desc(Y) opt_nulls_order(Z) . {}

qual_all_Op(A) ::= all_Op(X) . {}
qual_all_Op(A) ::= OPERATOR LP any_operator(X) RP . {}

all_Op(A) ::= Op . {}
all_Op(A) ::= MathOp(X) . {} 

MathOp(X) ::= . {} //TODO

any_operator(A) ::= all_Op(X) . {}
any_operator(A) ::= ColId(X) DOT any_operator(Y) . {}

where_clause(A) ::= . {}
where_clause(A) ::= WHERE a_expr(X) . {}

set_clause_list(A) ::= set_clause(X) . {}
set_clause_list(A) ::= set_clause_list(X) COMMA set_clause(Y) . {}

set_clause(A) ::= single_set_clause(X) . {}
set_clause(A) ::= multiple_set_clause(X) . {}

single_set_clause(A) ::= set_target(X) EQ ctext_expr(Y) . {}

set_target(A) ::= ColId(X) opt_indirection(Y) . {}

ctext_expr(A) ::= a_expr(X) . {}  
ctext_expr(A) ::= DEFAULT . {}  

multiple_set_clause(A) ::= LP set_target_list(X) RP EQ ctext_row(Y) . {}
multiple_set_clause(A) ::= LP set_target_list(X) RP EQ select_with_parens(Y) . {}

set_target_list(A) ::= set_target(X) . {}
set_target_list(A) ::= set_target_list(X) COMMA set_target(Y) . {}

ctext_row(A) ::= LP ctext_expr_list(X) RP . {}

ctext_expr_list(A) ::= ctext_expr(X) . {}
ctext_expr_list(A) ::= ctext_expr_list(X) COMMA ctext_expr(Y) . {}

select_with_parens(A) ::= LP select_no_parens(X) RP . {}
select_with_parens(A) ::= LP select_with_parens(x) RP . {}

//returning_clause
returning_clause(A) ::= . { A = 0; } 
returning_clause(A) ::= RETURNING target_list(X) . {} 

target_list(A) ::= target_el(X) . {}
target_list(A) ::= target_list(X) COMMA target_el	(Y) . {}

target_el(A) ::= a_expr(X) AS ColLabel(Y) . {}
target_el(A) ::= a_expr(X) IDENT . {}
target_el(A) ::= a_expr(X) . {}
target_el(A) ::= STAR . {}


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

//relation_expr_opt_alias
relation_expr_opt_alias(A) ::= relation_expr(X) . {}
relation_expr_opt_alias(A) ::= relation_expr(X) ColId(Y) . {}
relation_expr_opt_alias(A) ::= relation_expr(X) AS ColId(Y) . {}

relation_expr(A) ::= qualified_name(X) . {}
relation_expr(A) ::= qualified_name(X) STAR . {}
relation_expr(A) ::= ONLY qualified_name(X) . {}
relation_expr(A) ::= ONLY LP qualified_name(X) RP . {}

//from_clause
from_clause(A) ::= . {}
from_clause(A) ::= FROM from_list() . {}

from_list(A) ::= table_ref(X) . {}
from_list(A) ::= from_list COMMA table_ref(X) . {}

table_ref(A) ::= relation_expr(X) opt_alias_clause(Y) . {}
table_ref(A) ::= relation_expr(X) opt_alias_clause(Y) tablesample_clause(Z) . {}
table_ref(A) ::= func_table(X) func_alias_clause(Y) . {}
table_ref(A) ::= LATERAL_P func_table(X) func_alias_clause(Y) . {}
table_ref(A) ::= select_with_parens(X) opt_alias_clause(Y) . {}
table_ref(A) ::= LATERAL_P select_with_parens(X) opt_alias_clause(Y) . {}
table_ref(A) ::= joined_table(X) . {}
table_ref(A) ::= LP joined_table(X) RP alias_clause(Y) . {}

opt_alias_clause(A) ::= . {}
opt_alias_clause(A) ::= alias_clause(X) . {}

alias_clause(A) ::= AS ColId(X) LP name_list(Y) RP . {}
alias_clause(A) ::= AS ColId(X) . {}
alias_clause(A) ::= ColId(X) LP name_list() RP . {}
alias_clause(A) ::= ColId(X) . {}

tablesample_clause(A) ::= TABLESAMPLE func_name(X) LP expr_list(Y) RP opt_repeatable_clause(Z) . {}

expr_list(A) ::= a_expr(X) . {}
expr_list(A) ::= expr_list(X) COMMA a_expr(Y) . {}

opt_repeatable_clause(A) ::= REPEATABLE LP a_expr(X) RP . {}








func_table(A) ::= func_expr_windowless(X) opt_ordinality(Y) . {}
func_table(A) ::= ROWS FROM LP rowsfrom_list(X) RP opt_ordinality(Y) . {}

opt_ordinality(A) ::= . {}
opt_ordinality(A) ::= WITH_LA ORDINALITY . {}

rowsfrom_list(A) ::= rowsfrom_item(X) . {}
rowsfrom_list(A) ::= rowsfrom_list(X) COMMA rowsfrom_item(Y) . {}

rowsfrom_item(A) ::= func_expr_windowless opt_col_def_list . {}

opt_col_def_list(A) ::= . {}
opt_col_def_list(A) ::= AS LP TableFuncElementList() RP . {}

TableFuncElementList(A) ::= TableFuncElement(X) . {}
TableFuncElementList(A) ::= TableFuncElementList(X) COMMA TableFuncElement(Y) . {}

TableFuncElement(A) ::= ColId(X) Typename(Y) opt_collate_claus(Z) . {}

Typename(A) ::= SimpleTypename(X) opt_array_bounds(Y) . {}

SimpleTypename(A) ::=  GenericType(X) . {}
SimpleTypename(A) ::=  Numeric(X) . {}
SimpleTypename(A) ::=  Bit(X) . {}
SimpleTypename(A) ::=  Character(X) . {}
SimpleTypename(A) ::=  ConstDatetime(X) . {}
SimpleTypename(A) ::=  ConstInterval(X) opt_interval(Y) . {}
SimpleTypename(A) ::=  ConstInterval(X) LP Iconst(Y) RP . {}

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







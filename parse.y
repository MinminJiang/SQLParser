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
CreateStmt(A)	::= CREATE OptTemp(X) TABLE qualified_name(Y) LP OptTableElementList(Z) RP OptInherit(M) OptWith(N) OnCommitOption(O) OptTableSpace(P) .  {
	transformCreateTableStmt();
}
CreateStmt(A)	::= CCREATE OptTemp(X) TABLE IF_P NOT EXISTS qualified_name(Y) LP OptTableElementList(Z) RP OptInherit(M) OptWith(N) OnCommitOption(O) OptTableSpace(P) .  {
	transformCreateTableStmt();
}
CreateStmt(A)	::=CREATE OptTemp(X) TABLE qualified_name(Y) OF any_name(Z) OptTypedTableElementList(M) OptWith(N) OnCommitOption(O) OptTableSpace(P) .  {
	transformCreateTableStmt();
}
CreateStmt(A)	::= CREATE OptTemp(X) TABLE IF_P NOT EXISTS qualified_name(Y) OF any_name(Z) OptTypedTableElementList(M) OptWith(N) OnCommitOption(O) OptTableSpace(P) .  {
	transformCreateTableStmt();
}
OptTemp(A) ::= .   {  }
OptTemp(A) ::= TEMPORARY { }
OptTemp(A) ::= TEMP .   {  }
OptTemp(A) ::= LOCAL TEMPORARY { }
OptTemp(A) ::= LOCAL TEMP .   {  }
OptTemp(A) ::= GLOBAL TEMPORARY { }
OptTemp(A) ::= GLOBAL TEMP .   {  }
OptTemp(A) ::= UNLOGGED { }

OptTableElementList(A) ::= . {}
OptTableElementList(A) ::= TableElementList(X) . {};

TableElementList(A) ::= TableElement(X) . {}
TableElementList(A) ::= TableElementList(X) COMMA TableElement(Y) . {}

TableElement(A) ::= columnDef(X) . {}
TableElement(A) ::= TableLikeClause(X) . {}
TableElement(A) ::= TableConstraint(X) . {}

columnDef(A) ::= ColId(X) Typename(Y) create_generic_options(Z) ColQualList(M) . {}

create_generic_options(A) ::= . {}
create_generic_options(A) ::= OPTIONS LP generic_option_list(X) RP . {}

generic_option_list(A) ::= generic_option_elem(X) . {}
generic_option_list(A) ::= generic_option_list(X) COMMA generic_option_elem(Y) . {}

generic_option_elem(A) ::= generic_option_name(X) generic_option_arg(Y) . {}

generic_option_name(A) ::= ColLabel(X) . {}

generic_option_arg(A) ::= Sconst(X) . {}

ColQualList(A) ::= . {}
ColQualList(A) ::= ColQualList(X) ColConstraint(Y) . {}

ColConstraint(A) ::= CONSTRAINT name(X) ColConstraintElem(Y) . {}
ColConstraint(A) ::= ColConstraintElem(X) . {}
ColConstraint(A) ::= ConstraintAttr(X) . {}
ColConstraint(A) ::= COLLATE any_name(X) . {}

ColConstraintElem(A) ::= NOT NULL_P . {}
ColConstraintElem(A) ::= NULL_P . {}
ColConstraintElem(A) ::= UNIQUE opt_definition(X) OptConsTableSpace(Y) . {}
ColConstraintElem(A) ::= PRIMARY KEY opt_definition(X) OptConsTableSpace(Y) . {}
ColConstraintElem(A) ::= CHECK LP a_expr(X) RP opt_no_inherit(Y) . {}
ColConstraintElem(A) ::= DEFAULT b_expr(X) . {}
ColConstraintElem(A) ::= REFERENCES qualified_name(X) opt_column_list(Y) key_match(Z) key_actions(M) . {}

opt_definition(A) ::= . {}
opt_definition(A) ::= WITH definition(X) . {}

definition(A) ::= LP def_list(X) RP . {}

def_list(A) ::= def_elem(X) . {}
def_list(A) ::= def_list(X) COMMA def_elem(Y) . {}

def_elem(A) ::= ColLabel(X) EQ def_arg(Y) . {}
def_elem(A) ::= ColLabel(X) . {}

def_arg(A) ::= func_type(X) . {}
def_arg(A) ::= reserved_keyword(X) . {}
def_arg(A) ::= qual_all_Op(X) . {}
def_arg(A) ::= NumericOnly(X) . {}
def_arg(A) ::= Sconst(X) . {}

func_type(A) ::= Typename(X) . {}
func_type(A) ::= type_function_name attrs(X) '%' TYPE_P . {}
func_type(A) ::= SETOF type_function_name attrs(X) '%' TYPE_P . {}

NumericOnly(A) ::= FCONST . {}
NumericOnly(A) ::= '-' FCONST . {}
NumericOnly(A) ::= SignedIconst(X) . {}

SignedIconst(A) ::= Iconst(X) . {}
SignedIconst(A) ::= '+' Iconst(X) . {}
SignedIconst(A) ::= '-' Iconst(X) . {}

OptConsTableSpace(A) ::= . {}
OptConsTableSpace(A) ::= USING INDEX TABLESPACE name(X) . {}

opt_no_inherit(A) ::= . {}
opt_no_inherit(A) ::= NO INHERIT . {}

b_expr(A) ::= . {} //TODO

opt_column_list(A) ::= . {}
opt_column_list(A) ::= LP columnList(X) RP . {}

columnList(A) ::= columnElem(X) . {}
columnList(A) ::= columnList COMMA columnElem(X)	 . {}

columnElem(A) ::= ColId(X) . {}

key_match(A) ::= . {}
key_match(A) ::= MATCH FULL . {}
key_match(A) ::= MATCH PARTIAL . {}
key_match(A) ::= MATCH SIMPLE . {}

key_actions(A) ::= . {}
key_actions(A) ::= key_update(X) . {}
key_actions(A) ::= key_delete(X) . {}
key_actions(A) ::= key_update(X) key_delete(Y) . {}
key_actions(A) ::= key_delete(X) key_update(Y) . {}

key_update(A) ::= ON UPDATE key_action(X) . {}

key_delete(A) ::= ON DELETE_P key_action(X) . {}

key_action(A) ::= NO ACTION . {}
key_action(A) ::= RESTRICT . {}
key_action(A) ::= CASCADE . {}
key_action(A) ::= SET NULL_P . {}
key_action(A) ::= SET DEFAULT . {}

ConstraintAttr(A) ::= DEFERRABLE . {} 
ConstraintAttr(A) ::= NOT DEFERRABLE . {} 
ConstraintAttr(A) ::= INITIALLY DEFERRED . {} 
ConstraintAttr(A) ::= INITIALLY IMMEDIATE . {} 

TableLikeClause(A) ::= LIKE qualified_name(X) TableLikeOptionList(Y) . {}

TableLikeOptionList(A) ::= . {}
TableLikeOptionList(A) ::= TableLikeOptionList(X) INCLUDING TableLikeOption(Y) . {}
TableLikeOptionList(A) ::= TableLikeOptionList(X) EXCLUDING TableLikeOption(Y) . {}

TableLikeOption(A) ::= DEFAULTS . {}
TableLikeOption(A) ::= CONSTRAINTS . {}
TableLikeOption(A) ::= INDEXES . {}
TableLikeOption(A) ::= STORAGE . {}
TableLikeOption(A) ::= COMMENTS . {}
TableLikeOption(A) ::= ALL . {}

TableConstraint(A) ::= CONSTRAINT name(X) ConstraintElem(Y) . {}
TableConstraint(A) ::= ConstraintElem(X) . {}

ConstraintElem(A) ::= CHECK LP a_expr(X) RP ConstraintAttributeSpec(Y) . {} 
ConstraintElem(A) ::= UNIQUE LP columnList(X) RP opt_definition(Y) OptConsTableSpace(Z) ConstraintAttributeSpec(M) . {} 
ConstraintElem(A) ::= UNIQUE ExistingIndex(X) ConstraintAttributeSpec(Y) . {} 
ConstraintElem(A) ::= PRIMARY KEY LP columnList(X) RP opt_definition(Y) OptConsTableSpace(Z) ConstraintAttributeSpec(M) . {} 
ConstraintElem(A) ::= PRIMARY KEY ExistingIndex(X) ConstraintAttributeSpec(Y) . {} 
ConstraintElem(A) ::= EXCLUDE access_method_clause(X) LP ExclusionConstraintList(Y) RP opt_definition(Z) OptConsTableSpace(M) ExclusionWhereClause(N) ConstraintAttributeSpec(O). {} 
ConstraintElem(A) ::= FOREIGN KEY  LP columnList(X) RP REFERENCES qualified_name(Y) opt_column_list(Z) key_match key_actions(M) ConstraintAttributeSpec(N) . {} 

ConstraintAttributeSpec(A) ::= . {}
ConstraintAttributeSpec(A) ::= ConstraintAttributeSpec(X) ConstraintAttributeElem(Y) . {}

ConstraintAttributeElem(A) ::= NOT DEFERRABLE . {}
ConstraintAttributeElem(A) ::= DEFERRABLE . {}
ConstraintAttributeElem(A) ::= INITIALLY IMMEDIATE . {}
ConstraintAttributeElem(A) ::= INITIALLY DEFERRED . {}
ConstraintAttributeElem(A) ::= NOT VALID . {}
ConstraintAttributeElem(A) ::= NO INHERIT . {}

ExistingIndex(A) ::= USING INDEX index_name(X) . {}

index_name(A) ::= ColId(X) . {}

access_method_clause(A) ::= . {}
access_method_clause(A) ::= USING access_method(X) . {}

access_method(A) ::= ColId(X) . {}

ExclusionConstraintList(A) ::= ExclusionConstraintElem(X) . {}
ExclusionConstraintList(A) ::= ExclusionConstraintList(X) COMMA ExclusionConstraintElem(Y) . {}

ExclusionConstraintElem(A) ::= index_elem(X) WITH any_operator(Y) . {}
ExclusionConstraintElem(A) ::=index_elem(X) WITH OPERATOR LP any_operator(X) RP . {}

ExclusionWhereClause(A) ::= . {}
ExclusionWhereClause(A) ::= WHERE LP a_expr(X) RP . {}

//OptInherit
OptInherit(A) ::= . {}
OptInherit(A) ::= INHERITS LP qualified_name_list(X) RP . {}

qualified_name_list(A) ::= qualified_name(X) . {}
qualified_name_list(A) ::= qualified_name_list(X) COMMA qualified_name(Y) . {}

//OptWith
OptWith(A) ::= . {}
OptWith(A) ::= WITH reloptions(X) . {}
OptWith(A) ::= WITH OIDS . {}
OptWith(A) ::= WITHOUT OIDS . {}

reloptions(A) ::= LP reloption_list(A) RP . {}

reloption_list(A) ::= reloption_elem(X) . {}
reloption_list(A) ::= reloption_list(X) COMMA reloption_elem(Y) . {}

reloption_elem(A) ::= ColLabel(X) EQ def_arg(Y) . {}
reloption_elem(A) ::= ColLabel(X) . {}
reloption_elem(A) ::= ColLabel(X) DOT ColLabel(Y) EQ def_arg(Z) . {}
reloption_elem(A) ::= ColLabel DOT ColLabel . {}

//OnCommitOption
OnCommitOption(A) ::= . {}
OnCommitOption(A) ::= ON COMMIT DROP . {}
OnCommitOption(A) ::= ON COMMIT DELETE_P ROWS . {}
OnCommitOption(A) ::= ON COMMIT PRESERVE ROWS . {}

//OptTableSpace
OptTableSpace(A) ::= . {} 
OptTableSpace(A) ::= TABLESPACE name(X) . {} 

//OptTypedTableElementList
OptTypedTableElementList(A) ::= . {}
OptTypedTableElementList(A) ::= LP TypedTableElementList(X) RP . {}

TypedTableElementList(A) ::= TypedTableElement(X) . {}
TypedTableElementList(A) ::= TypedTableElementList(X) COMMA TypedTableElement(Y) . {}

TypedTableElement(A) ::= columnOptions(X) . {}
TypedTableElement(A) ::= TableConstraint(X) . {}

columnOptions(A) ::= ColId(X) WITH OPTIONS ColQualList(Y) . {}

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

TableFuncElement(A) ::= ColId(X) Typename(Y) opt_collate_clause(Z) . {}

Typename(A) ::= SimpleTypename(X) opt_array_bounds(Y) . {}

opt_array_bounds(A) ::= . {}
opt_array_bounds(A) ::= opt_array_bounds(X) '[' ']' . {}
opt_array_bounds(A) ::= opt_array_bounds(X) '[' Iconst(Y) ']' . {}

opt_collate_clause(A) ::= . {}
opt_collate_clause(A) ::= COLLATE any_name(X) . {}

SimpleTypename(A) ::=  GenericType(X) . {}
SimpleTypename(A) ::=  Numeric(X) . {}
SimpleTypename(A) ::=  Bit(X) . {}
SimpleTypename(A) ::=  Character(X) . {}
SimpleTypename(A) ::=  ConstDatetime(X) . {}
SimpleTypename(A) ::=  ConstInterval(X) opt_interval(Y) . {}
SimpleTypename(A) ::=  ConstInterval(X) LP Iconst(Y) RP . {}

GenericType(A) ::= type_function_name(X) opt_type_modifiers(Y) . {}
GenericType(A) ::= type_function_name(X) attrs(X) opt_type_modifiers(Y) . {}

opt_type_modifiers(A) ::= LP expr_list(X) RP . {}

Numeric(A) ::= INT_P . {}
Numeric(A) ::= INTEGER . {}
Numeric(A) ::= SMALLINT . {}
Numeric(A) ::= BIGINT . {}
Numeric(A) ::= REAL . {}
Numeric(A) ::= FLOAT_P opt_float(X) . {}
Numeric(A) ::= DOUBLE_P PRECISION . {}
Numeric(A) ::= DECIMAL_P opt_type_modifiers(X) . {}
Numeric(A) ::= DEC opt_type_modifiers(X) . {}
Numeric(A) ::= NUMERIC opt_type_modifiers(X) . {}
Numeric(A) ::= BOOLEAN_P . {}

opt_float(A) ::= . {}
opt_float(A) ::= LP Iconst(X) RP . {}

Iconst(A) ::= ICONST . {}
Sconst(A) ::= SCONST . {}

Bit(A) ::= BitWithLength(X) . {}
Bit(A) ::= BitWithoutLength(X) . {}

BitWithLength(A) ::= BIT opt_varying(X) LP expr_list(Y) RP

opt_varying(A) ::= . {}
opt_varying(A) ::= VARYING . {}

BitWithoutLength(A) ::= BIT opt_varying(X) . {}

Character(A) ::= CharacterWithLength(X) . {}
Character(A) ::= CharacterWithoutLength(X) . {}

CharacterWithLength(A) ::= character(X) LP Iconst(Y) RP opt_charset(Z) . {}

character(A) ::= CHARACTER opt_varying(X) . {}
character(A) ::= CHAR_P opt_varying(X) . {}
character(A) ::= VARCHAR . {}
character(A) ::= NATIONAL CHARACTER opt_varying(X) . {}
character(A) ::= NATIONAL CHAR_P opt_varying(X) . {}
character(A) ::= NCHAR opt_varying(X) . {}

opt_charset(A) ::= . {}
opt_charset(A) ::= CHARACTER SET ColId(X) . {}

CharacterWithoutLength(A) ::= character(X) opt_charset(Y) . {}

ConstDatetime(A) ::= TIMESTAMP LP Iconst(X) RP opt_timezone(Y) . {}
ConstDatetime(A) ::= TIMESTAMP opt_timezone(X) . {}
ConstDatetime(A) ::= TIME LP Iconst(X) RP opt_timezone(Y) . {}
ConstDatetime(A) ::= TIME opt_timezone(X) . {}

opt_timezone(A) ::= . {}
opt_timezone(A) ::= WITH_LA TIME ZONE . {}
opt_timezone(A) ::= WITHOUT TIME ZONE . {}

ConstInterval(A) ::= INTERVAL . {}

opt_interval(A) ::= . {}
opt_interval(A) ::= YEAR_P . {}
opt_interval(A) ::= MONTH_P . {}
opt_interval(A) ::= DAY_P . {}
opt_interval(A) ::= HOUR_P . {}
opt_interval(A) ::= MINUTE_P . {}
opt_interval(A) ::= interval_second(X) . {}
opt_interval(A) ::= YEAR_P TO MONTH_P . {}
opt_interval(A) ::= DAY_P TO HOUR_P . {}
opt_interval(A) ::= DAY_P TO MINUTE_P . {}
opt_interval(A) ::= DAY_P TO interval_second(X) . {}
opt_interval(A) ::= HOUR_P TO MINUTE_P . {}
opt_interval(A) ::= HOUR_P TO interval_second . {}
opt_interval(A) ::= MINUTE_P TO interval_second . {}

interval_second(A) ::= SECOND_P . {}
interval_second(A) ::= SECOND_P LP Iconst(X) RP . {}

func_alias_clause(A) ::= . {}
func_alias_clause(A) ::= alias_clause(X) . {}
func_alias_clause(A) ::= AS LP TableFuncElementList(X) RP . {}
func_alias_clause(A) ::= AS ColId(X) LP TableFuncElementList(Y) RP . {}
func_alias_clause(A) ::= ColId(X)  TableFuncElementList(Y) . {}

joined_table(A) ::= LP joined_table(X) RP . {}
joined_table(A) ::= table_ref(X) CROSS JOIN table_ref(Y) . {}
joined_table(A) ::= table_ref(X) join_type(Y) JOIN table_ref(Z) join_qual(M) . {}
joined_table(A) ::= table_ref(X) JOIN table_ref(Y) join_qual(Z) . {}
joined_table(A) ::= table_ref(X) NATURAL join_type(Y) JOIN table_ref(Z) . {}
joined_table(A) ::= table_ref(X) NATURAL JOIN table_ref(Y) . {}

join_qual(A) ::= USING LP name_list(X) RP . {}
join_qual(A) ::= ON a_expr(X) . {}

//where_or_current_clause
where_or_current_clause(A) ::= . {}
where_or_current_clause(A) ::= WHERE a_expr(X) . {}
where_or_current_clause(A) ::= WHERE CURRENT_P OF cursor_name(X) . {}

cursor_name(A) ::= name(x). {}

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

using_clause(A) ::= . {}
using_clause(A) ::= USING from_list(X) . {}


/*****************************************************************************
 *
 *		QUERY:
 *				SELECT STATEMENTS
 *
 *****************************************************************************/

cmd ::=SelectStmt .
SelectStmt(A) ::= select_no_parens(X) . {}
SelectStmt(A) ::= select_with_parens(X) . {} 

select_with_parens(A) ::= LP select_no_parens(X) RP. {}
select_with_parens(A) ::= LP select_with_parens(X) RP. {}

select_no_parens(A) ::= simple_select(X) . {}
select_no_parens(A) ::= select_clause(X) sort_clause(Y) . {}
select_no_parens(A) ::= select_clause(X) opt_sort_clause(Y) for_locking_clause(Z) opt_select_limit(M) . {}
select_no_parens(A) ::= select_clause(X) opt_sort_clause(Y) select_limit(Z) opt_for_locking_clause(M) . {}
select_no_parens(A) ::= with_clause(X) select_clause(Y) . {}
select_no_parens(A) ::= with_clause(X) select_clause(Y) sort_clause(Z) . {}
select_no_parens(A) ::= with_clause(X) select_clause(Y) opt_sort_clause(Z) for_locking_clause(M) opt_select_limit(N) . {}
select_no_parens(A) ::= with_clause(X) select_clause(Y) opt_sort_clause(Z) select_limit(M) opt_for_locking_clause(M) . {}

select_clause(A) ::= simple_select(X) . {}
select_clause(A) ::= select_with_parens(X) . {}

//simple_select
simple_select(A) ::= SELECT opt_all_clause(X) opt_target_list(Y) into_clause(Z) from_clause(M) where_clause(N) group_clause(O) having_clause(P) window_clause(Q) . {}
simple_select(A) ::= SELECT distinct_clause(X) target_list(Y) into_clause(Z) from_clause(M) where_clause(N) group_clause(O) having_clause(P) window_clause(Q) . {}
simple_select(A) ::= values_clause(X) . {}
simple_select(A) ::= TABLE relation_expr(X) . {}
simple_select(A) ::= select_clause(X) UNION all_or_distinct(Y) select_clause(Z) . {}
simple_select(A) ::= select_clause(X) INTERSECT all_or_distinct(Y) select_clause(Z) . {}
simple_select(A) ::= select_clause(X) EXCEPT all_or_distinct(Y) select_clause(Z) . {}

opt_all_clause(A) ::= . {}
opt_all_clause(A) ::= ALL . {}

opt_target_list(A) ::= . {}
opt_target_list(A) ::= target_list(X) . {}

into_clause(A) ::= . {}
into_clause(A) ::= INTO OptTempTableName(X) . {}

OptTempTableName(A) ::= TEMPORARY opt_table(X) qualified_name(Y) . {}
OptTempTableName(A) ::= TEMP opt_table(X) qualified_name(Y) . {}
OptTempTableName(A) ::= LOCAL TEMPORARY opt_table(X) qualified_name(Y) . {}
OptTempTableName(A) ::= LOCAL TEMP opt_table(X) qualified_name(Y) . {}
OptTempTableName(A) ::= GLOBAL TEMPORARY opt_table(X) qualified_name(Y) . {}
OptTempTableName(A) ::= GLOBAL TEMP opt_table(X) qualified_name(Y) . {}
OptTempTableName(A) ::= UNLOGGED opt_table(X) qualified_name(Y) . {}
OptTempTableName(A) ::= TABLE qualified_name(X) . {}
OptTempTableName(A) ::= qualified_name(X) . {}

opt_table(A) ::= . {}
opt_table(A) ::= TABLE . {}

group_clause(A) ::= . {}
group_clause(A) ::= GROUP_P BY group_by_list() . {}

group_by_list(A) ::= group_by_item(X) . {}
group_by_list(A) ::= group_by_list(X) COMMA group_by_item(Y) . {}

group_by_item(A) ::= a_expr(X) . {}
group_by_item(A) ::= empty_grouping_set(X) . {}
group_by_item(A) ::= cube_clause(X) . {}
group_by_item(A) ::= rollup_clause(X) . {}
group_by_item(A) ::= grouping_sets_clause(X) . {}

empty_grouping_set(A) ::= LP RP . {}

cube_clause(A) ::= CUBE LP expr_list(X) RP . {};

rollup_clause(A) ::= ROLLUP LP expr_list(X) RP . {};

grouping_sets_clause(A) ::= GROUPING SETS LP group_by_list(X) RP . {}

having_clause(A) ::= . {}
having_clause(A) ::= HAVING a_expr(X) . {}

window_clause(A) ::= WINDOW window_definition_list(X) . {}

window_definition_list(A) ::= window_definition(X) . {}
window_definition_list(A) ::= window_definition_list(X) COMMA window_definition(Y) . {}

window_definition(A) ::= ColId(X) AS window_specification(Y) . {}

window_specification(A) ::= LP opt_existing_window_name(X) opt_partition_clause(Y) opt_sort_clause(Z) opt_frame_clause(M) RP . {}

opt_existing_window_name(A) ::= . {}
opt_existing_window_name(A) ::= ColId(X) . {}

opt_partition_clause(A) ::= . {}
opt_partition_clause(A) ::= PARTITION BY expr_list(X) . {}

opt_frame_clause(A) ::= . {}
opt_frame_clause(A) ::= RANGE frame_extent(X) . {}
opt_frame_clause(A) ::= ROWS frame_extent(X) . {}

frame_extent(A) ::= frame_bound(X) . {}
frame_extent(A) ::= BETWEEN frame_bound(X) AND frame_bound(Y) . {}

frame_bound(A) ::= UNBOUNDED PRECEDING . {}
frame_bound(A) ::= UNBOUNDED FOLLOWING . {}
frame_bound(A) ::= CURRENT_P ROW . {}
frame_bound(A) ::= a_expr(X) PRECEDING . {}
frame_bound(A) ::= a_expr(X) FOLLOWING . {}

distinct_clause(A) ::= DISTINCT . {}
distinct_clause(A) ::= DISTINCT ON LP expr_list(X) RP . {}

values_clause(A) ::= VALUES ctext_row(X) . {}
values_clause(A) ::= values_clause(X) COMMA ctext_row(Y) . {}

all_or_distinct(A) ::= . {}
all_or_distinct(A) ::= ALL . {}
all_or_distinct(A) ::= DISTINCT . {}

//for_locking_clause
for_locking_clause(A) ::= for_locking_items(X) . {}
for_locking_clause(A) ::= FOR READ ONLY . {}

for_locking_items(A) ::= for_locking_item(X) . {}
for_locking_items(A) ::= for_locking_items(X) for_locking_item(Y) . {}

for_locking_item(A) ::= for_locking_strength(X) locked_rels_list(Y) opt_nowait_or_skip(Z) . {}

for_locking_strength(A) ::= FOR UPDATE  . {}
for_locking_strength(A) ::= FOR NO KEY UPDATE . {}
for_locking_strength(A) ::= FOR SHARE  . {}
for_locking_strength(A) ::= FOR KEY SHARE  . {}

locked_rels_list(A) ::= . {}
locked_rels_list(A) ::= OF qualified_name_list(X) . {}

opt_nowait_or_skip(A) ::= . {}
opt_nowait_or_skip(A) ::= NOWAIT . {}
opt_nowait_or_skip(A) ::= SKIP LOCKED . {}

//opt_select_limit
opt_select_limit(A) ::= . {}
opt_select_limit(A) ::= select_limit(X) . {}

//select_limit
select_limit(A) ::= limit_clause(X) offset_clause(Y) . {}
select_limit(A) ::= offset_clause(X) limit_clause(Y) . {}
select_limit(A) ::= limit_clause(X) . {}
select_limit(A) ::= offset_clause(Y) . {}

limit_clause(A) ::= LIMIT select_limit_value(X) . {}
limit_clause(A) ::= LIMIT select_limit_value(X) COMMA select_offset_value(Y) . {}
limit_clause(A) ::= FETCH first_or_next(X) opt_select_fetch_first_value(Y) row_or_rows(Z) ONLY . {}

select_limit_value(A) ::= a_expr(X) . {}
select_limit_value(A) ::= . {}

offset_clause(A) ::= OFFSET select_offset_value(X) . {} 
offset_clause(A) ::= OFFSET select_offset_value2(X) row_or_rows(Y) . {} 

select_offset_value(A) ::= a_expr(X) . {}

select_offset_value2(A) ::= c_expr(X) . {}

c_expr(A) ::= columnref(X) . {}
c_expr(A) ::= AexprConst(X) . {}
c_expr(A) ::= PARAM opt_indirection(X) . {}
c_expr(A) ::= LP a_expr(X) RP opt_indirection(Y) . {}
c_expr(A) ::= case_expr(X) . {}
c_expr(A) ::= func_expr(X) . {}
c_expr(A) ::= select_with_parens(X) . {}
c_expr(A) ::= select_with_parens(X) indirection(Y) . {}
c_expr(A) ::= EXISTS select_with_parens(X) . {}
c_expr(A) ::= ARRAY select_with_parens(X) . {}
c_expr(A) ::= ARRAY array_expr(X) . {}
c_expr(A) ::= explicit_row(X) . {}
c_expr(A) ::= implicit_row(X) . {}
c_expr(A) ::= GROUPING LP expr_list(X) RP . {}

columnref(A) ::= ColId(X) . {}
columnref(A) ::= ColId(X) indirection(Y) . {}

AexprConst(A) ::= Iconst(X) . {}
AexprConst(A) ::= FCONST . {}
AexprConst(A) ::= Sconst(X) . {}
AexprConst(A) ::= BCONST . {}
AexprConst(A) ::= XCONST . {}
AexprConst(A) ::= func_name(X) Sconst(Y) . {}
AexprConst(A) ::= func_name(X) LP func_arg_list(Y) opt_sort_clause(Z) RP Sconst(M) . {}
AexprConst(A) ::= ConstTypename(X) Sconst(Y) . {}
AexprConst(A) ::= ConstInterval(X) Sconst(Y) opt_interval(Z) . {}
AexprConst(A) ::= ConstInterval(X) LP Iconst(Y) RP  Sconst(Z) . {}
AexprConst(A) ::= TRUE_P . {}
AexprConst(A) ::= FALSE_P . {}
AexprConst(A) ::= NULL_P . {}

ConstTypename(A) ::= Numeric(X) . {}
ConstTypename(A) ::= ConstBit(X) . {}
ConstTypename(A) ::= ConstCharacter(X) . {}
ConstTypename(A) ::= ConstDatetime(X) . {}

ConstBit(A) ::= BitWithLength(X) . {}
ConstBit(A) ::= BitWithoutLength(X) . {}

ConstCharacter(A) ::= CharacterWithLength(X) . {}
ConstCharacter(A) ::= CharacterWithoutLength(X) . {}

case_expr(A) ::= CASE case_arg(X) when_clause_list(Y) case_default(Z) END_P . {}

case_arg(A) ::= . {}
case_arg(A) ::= a_expr(X) . {}

when_clause_list(A) ::= when_clause(X) . {}
when_clause_list(A) ::= when_clause_list(X) when_clause(Y) . {}

when_clause(A) ::= WHEN a_expr(X) THEN a_expr(Y) . {}

case_default(A) ::= . {}
case_default(A) ::= ELSE a_expr(X) . {}

func_expr(A) ::= func_application(X) within_group_clause(Y) filter_clause(Z) over_clause(M) . {}
func_expr(A) ::= func_expr_common_subexpr(X) . {}

within_group_clause(A) ::= . {}
within_group_clause(A) ::= WITHIN GROUP_P LP sort_clause(X) RP . {}

filter_clause(A) ::= . {}
filter_clause(A) ::= FILTER LP WHERE a_expr(X) RP	 . {}

over_clause(A) ::= . {}
over_clause(A) ::= OVER window_specification(X) . {}
over_clause(A) ::= OVER ColId(X) . {}

array_expr(A) ::= '[' expr_list(X) ']' . {}
array_expr(A) ::= '[' array_expr_list(X) ']' . {}
array_expr(A) ::= '[' ']' . {}

array_expr_list(A) ::= array_expr(X) . {}
array_expr_list(A) ::= array_expr_list(X) COMMA array_expr(Y) . {}

explicit_row(A) ::= ROW LP expr_list(X) RP . {}
explicit_row(A) ::= ROW LP RP . {}

implicit_row(A) ::= LP expr_list(X) COMMA a_expr(Y) RP . {}

first_or_next(A) ::= FIRST_P . {}
first_or_next(A) ::= NEXT . {}

opt_select_fetch_first_value(A) ::= . {}
opt_select_fetch_first_value(A) ::= SignedIconst(X) . {}
opt_select_fetch_first_value(A) ::= LP a_expr(X) RP. {}

row_or_rows(A) ::= ROW . {}
row_or_rows(A) ::= ROWS . {}

//opt_for_locking_clause
opt_for_locking_clause(A) ::= . {}
opt_for_locking_clause(A) ::= for_locking_clause(X) . {}
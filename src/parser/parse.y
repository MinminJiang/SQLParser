// All token codes are small integers with #defines that begin with "TK_"
%token_prefix TK_

// The type of the data attached to each token is Token.  This is also the
// default type for non-terminals.
//
%token_type {Token}
%default_type {Token}

// The generated parser function takes a 4th argument as follows:
%extra_argument {Parse *pParse}

// The following text is included near the beginning of the C source
// code file that implements the parser.
//
%include {
#include "parsenodes.h"
}

// The name of the generated procedure that implements the parser
// is as follows:
%name ACEProxy3Parser


// The name of a column or table can be any of the following:
//
%type IDENT {Token}
IDENT(A) ::= id(A).
IDENT(A) ::= STRING(A).
IDENT(A) ::= JOIN_KW(A).

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
func_type(A) ::= type_function_name attrs(X) REM TYPE_P . {}
func_type(A) ::= SETOF type_function_name attrs(X) REM TYPE_P . {}

NumericOnly(A) ::= FCONST . {}
NumericOnly(A) ::= MINUS FCONST . {}
NumericOnly(A) ::= SignedIconst(X) . {}

SignedIconst(A) ::= Iconst(X) . {}
SignedIconst(A) ::= PLUS Iconst(X) . {}
SignedIconst(A) ::= MINUS Iconst(X) . {}

OptConsTableSpace(A) ::= . {}
OptConsTableSpace(A) ::= USING INDEX TABLESPACE name(X) . {}

opt_no_inherit(A) ::= . {}
opt_no_inherit(A) ::= NO INHERIT . {}

//"+" PLUS
//"-" MINUS
//"*" STAR
//"/" SLASH
//"%" REM
//"^"
//"<" LT
//">" GT
//"=" EQ
//">=" GE
//"<=" LE
//"<>" NE
b_expr(A) ::= c_expr(X) . {}
b_expr(A) ::= b_expr(X) TYPECAST Typename(Y) . {}
b_expr(A) ::= PLUS b_expr(X). {}
b_expr(A) ::= MINUS b_expr(X) . {}
b_expr(A) ::= b_expr(X) PLUS b_expr(Y) . {}           
b_expr(A) ::= b_expr(X) MINUS b_expr(Y) . {}       
b_expr(A) ::= b_expr(X) STAR b_expr(Y) . {}
b_expr(A) ::= b_expr(X) SLASH b_expr(Y) . {}
b_expr(A) ::= b_expr(X) LT b_expr(Y) . {}
b_expr(A) ::= b_expr(X) GT b_expr(Y) . {}
b_expr(A) ::= b_expr(X) EQ b_expr(Y) . {}
b_expr(A) ::= b_expr(X) GE b_expr(Y) . {}
b_expr(A) ::= b_expr(X) LE b_expr(Y) . {}
b_expr(A) ::= b_expr(X) NE b_expr(Y) . {}
b_expr(A) ::= b_expr(X) REM b_expr(Y) . {}
b_expr(A) ::= b_expr(X) CONCAT b_expr(Y) . {}
b_expr(A) ::= qual_Op(X) b_expr(Y) . {}
b_expr(A) ::= b_expr(X) qual_Op(Y) . {}
b_expr(A) ::= b_expr(X) IS DISTINCT FROM b_expr(Y) . {}
b_expr(A) ::= b_expr(X) IS NOT DISTINCT FROM b_expr(Y) . {}
b_expr(A) ::= b_expr(X) IS OF LP type_list(Y) RP . {}
b_expr(A) ::= b_expr(X) IS NOT OF LP type_list(Y) . {}
b_expr(A) ::= b_expr(X) IS DOCUMENT_P . {}
b_expr(A) ::= b_expr(X) IS NOT DOCUMENT_P . {}

qual_Op(A) ::= Op . {}
qual_Op(A) ::= OPERATOR LP any_operator(X) RP . {};

type_list(A) ::= Typename(X) . {}
type_list(A) ::= type_list(X) COMMA Typename(Y) . {};

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
%type opt_with_clause {WithClause*}
%destructor opt_with_clause {ACEProxyWithClaseDelete($$);}
opt_with_clause(A) ::= . { A = 0; }
opt_with_clause(A) ::= with_clause(X) . { A = X; }

%type with_clause {WithClause*}
%destructor with_clause {ACEProxyWithClaseDelete($$);}
with_clause(A) ::= WITH cte_list(X) . { A = X; }
with_clause(A) ::= WITH_LA cte_list(X) . { A = X; }
with_clause(A) ::= WITH RECURSIVE cte_list(X) . { A = X; }

cte_list(A) ::= common_table_expr(X) . {}
cte_list(A) ::= cte_list(X) COMMA common_table_expr(Y) . {}

%type common_table_expr {CommonTableExpr*}
%destructor common_table_expr {ACEProxyCommonTableExprDelete($$);}
common_table_expr(A) ::= name(X) opt_name_list(Y) AS LP PreparableStmt(Z) RP . {
		CommonTableExpr *n = makeNode(CommonTableExpr);
		n->ctname = X;
		n->aliascolnames = Y;
		n->ctequery = Z;
		n->location = T_CommonTableExpr;
		A = n;
	}

%type name {Token*}
name(A) ::= ColId(X) . { A = X; }

%type ColId {Token*}
ColId(A) ::= IDENT(X) . { A = X; }
ColId(A) ::= unreserved_keyword(X) . { A.z = 0; A.n = 0; A.nKey = X}
ColId(A) ::= col_name_keyword(X) . { A.z = 0; A.n = 0; A.nKey = X }

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
indirection_el(A) ::= '[' a_expr(X) ':' a_expr(Y) ']' . {}

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

a_expr(A) ::= c_expr(X) . {} 
a_expr(A) ::= a_expr(X) TYPECAST Typename(Y) . {}
a_expr(A) ::= a_expr(X) COLLATE any_name(Y) . {}
a_expr(A) ::= a_expr(X) AT TIME ZONE a_expr(Y) . {}
a_expr(A) ::= PLUS a_expr(X). {}
a_expr(A) ::= MINUS a_expr(X) . {}
a_expr(A) ::= a_expr(X) PLUS a_expr(Y) . {}           
a_expr(A) ::= a_expr(X) MINUS a_expr(Y) . {}       
a_expr(A) ::= a_expr(X) STAR a_expr(Y) . {}
a_expr(A) ::= a_expr(X) SLASH a_expr(Y) . {}
a_expr(A) ::= a_expr(X) LT a_expr(Y) . {}
a_expr(A) ::= a_expr(X) GT a_expr(Y) . {}
a_expr(A) ::= a_expr(X) EQ a_expr(Y) . {}
a_expr(A) ::= a_expr(X) GE a_expr(Y) . {}
a_expr(A) ::= a_expr(X) LE a_expr(Y) . {}
a_expr(A) ::= a_expr(X) NE a_expr(Y) . {}
a_expr(A) ::= a_expr(X) REM a_expr(Y) . {}
a_expr(A) ::= a_expr(X) CONCAT a_expr(Y) . {}
a_expr(A) ::= a_expr(X) qual_Op(Y) a_expr(Z) . {} 
a_expr(A) ::= qual_Op(X) a_expr(Y). {} 
a_expr(A) ::= a_expr(X) qual_Op(Y) . {} 
a_expr(A) ::= a_expr(X) AND a_expr(Y) . {} 
a_expr(A) ::= a_expr(X) OR a_expr(Y) . {} 
a_expr(A) ::= NOT a_expr(X) . {} 
a_expr(A) ::= NOT_LA a_expr(X)	. {} 
a_expr(A) ::= a_expr(X) LIKE a_expr(Y) . {} 
a_expr(A) ::= a_expr(X) LIKE a_expr(Y) ESCAPE a_expr(Z)	 . {} 
a_expr(A) ::= a_expr(X) NOT_LA LIKE a_expr(Y) . {} 
a_expr(A) ::= a_expr(X) NOT_LA LIKE a_expr(Y) ESCAPE a_expr(Z)	 . {} 
a_expr(A) ::= a_expr(X) ILIKE a_expr(Y) . {} 
a_expr(A) ::= a_expr(X) ILIKE a_expr(Y) ESCAPE a_expr(Z)	 . {} 
a_expr(A) ::= a_expr(X) NOT_LA ILIKE a_expr(Y) . {} 
a_expr(A) ::= a_expr(X) NOT_LA ILIKE a_expr(Y) ESCAPE a_expr(Z)	 . {} 
a_expr(A) ::= a_expr(X) SIMILAR TO a_expr(Y) . {} 
a_expr(A) ::= a_expr(X) SIMILAR TO a_expr(Y) ESCAPE a_expr(Z)	 . {} 
a_expr(A) ::= a_expr(X) NOT_LA SIMILAR TO a_expr(Y) . {} 
a_expr(A) ::= a_expr(X) NOT_LA SIMILAR TO a_expr(Y) ESCAPE a_expr(Z) . {} 
a_expr(A) ::= a_expr(X) IS NULL_P . {} 
a_expr(A) ::= a_expr(X) ISNULL . {} 
a_expr(A) ::= a_expr(X) IS NOT NULL_P. {} 
a_expr(A) ::= a_expr(X) NOTNULL . {} 
a_expr(A) ::= row OVERLAPS row . {} 
a_expr(A) ::= a_expr(X) IS TRUE_P . {} 
a_expr(A) ::= a_expr(X) IS NOT TRUE_P . {} 
a_expr(A) ::= a_expr(X) IS FALSE_P . {} 
a_expr(A) ::= a_expr(X) IS NOT FALSE_P . {} 
a_expr(A) ::= a_expr(X) IS UNKNOWN . {} 
a_expr(A) ::= a_expr(X) IS NOT UNKNOWN . {} 
a_expr(A) ::= a_expr(X) IS DISTINCT FROM a_expr(Y) . {} 
a_expr(A) ::= a_expr(X) IS NOT DISTINCT FROM a_expr(Y) . {} 
a_expr(A) ::= a_expr(X) IS OF LP type_list(Y) RP . {} 
a_expr(A) ::= a_expr(X) IS NOT OF LP type_list(Y) RP . {} 
a_expr(A) ::= a_expr(X) BETWEEN opt_asymmetric(Y) b_expr(Z) AND a_expr(M) . {} 
a_expr(A) ::= a_expr(X) NOT_LA BETWEEN opt_asymmetric(Y) b_expr(Z)	 AND a_expr(M) . {} 
a_expr(A) ::= a_expr(X) BETWEEN SYMMETRIC b_expr(Y) AND a_expr(Z)	 . {} 
a_expr(A) ::= a_expr(X) NOT_LA BETWEEN SYMMETRIC b_expr(Y) AND a_expr(Z) . {} 
a_expr(A) ::= a_expr(X) IN_P in_expr(Y) . {} 
a_expr(A) ::= a_expr(X) NOT_LA IN_P in_expr(Y) . {} 
a_expr(A) ::= a_expr(X) subquery_Op(Y) sub_type(Z) select_with_parens(M) . {} 
a_expr(A) ::= a_expr(X) subquery_Op(Y) sub_type(Z) LP a_expr(M) RP . {} 
a_expr(A) ::= UNIQUE select_with_parens(X) . {} 
a_expr(A) ::= a_expr(X) IS DOCUMENT_P . {} 
a_expr(A) ::= a_expr(X) IS NOT DOCUMENT_P . {} 

opt_asymmetric(A) ::= . {}
opt_asymmetric(A) ::= ASYMMETRIC . {}

in_expr(A) ::= select_with_parens(X) . {}
in_expr(A) ::= LP expr_list(X) RP . {}

subquery_Op(A) ::= all_Op(X) . {}
subquery_Op(A) ::= OPERATOR LP any_operator(X) RP . {}
subquery_Op(A) ::= LIKE . {}
subquery_Op(A) ::= NOT_LA LIKE . {}
subquery_Op(A) ::= ILIKE . {}
subquery_Op(A) ::= NOT_LA ILIKE . {}

sub_type(A) ::= ANY . {}
sub_type(A) ::= SOME . {}
sub_type(A) ::= ALL . {}

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

//"+" PLUS
//"-" MINUS
//"*" STAR
//"/" SLASH
//"%" REM
//"^" CONCAT
//"<" LT
//">" GT
//"=" EQ
//">=" GE
//"<=" LE
//"<>" NE
MathOp(X) ::= PLUS . {} 
MathOp(X) ::= MINUS . {} 
MathOp(X) ::= STAR . {} 
MathOp(X) ::= SLASH . {} 
MathOp(X) ::= REM . {} 
MathOp(X) ::= CONCAT . {} 
MathOp(X) ::= LT . {} 
MathOp(X) ::= GT . {} 
MathOp(X) ::= EQ . {} 
MathOp(X) ::= LE . {} 
MathOp(X) ::= GE . {} 
MathOp(X) ::= NE . {} 

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
DeleteStmt(A) ::= opt_with_clause(C) DELETE FROM relation_expr_opt_alias(R) using_clause(U) where_or_current_clause(W) returning_clause(N) . {}

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

unreserved_keyword(A) ::= ABORT_P . {}
unreserved_keyword(A) ::= ABSOLUTE_P . {}
unreserved_keyword(A) ::= ACCESS . {}
unreserved_keyword(A) ::= ACTION . {}
unreserved_keyword(A) ::= ADD_P . {}
unreserved_keyword(A) ::= ADMIN . {}
unreserved_keyword(A) ::= AFTER . {}
unreserved_keyword(A) ::= AGGREGATE . {}
unreserved_keyword(A) ::= ALSO . {}
unreserved_keyword(A) ::= ALTER . {}
unreserved_keyword(A) ::= ALWAYS . {}
unreserved_keyword(A) ::= ASSERTION . {}
unreserved_keyword(A) ::= ASSIGNMENT . {}
unreserved_keyword(A) ::= AT . {}
unreserved_keyword(A) ::= ATTRIBUTE . {}
unreserved_keyword(A) ::= BACKWARD . {}
unreserved_keyword(A) ::= BEFORE . {}
unreserved_keyword(A) ::= BEGIN_P . {}
unreserved_keyword(A) ::= BY . {}
unreserved_keyword(A) ::= CACHE . {}
unreserved_keyword(A) ::= CALLED . {}
unreserved_keyword(A) ::= CASCADE . {}
unreserved_keyword(A) ::= CASCADED . {}
unreserved_keyword(A) ::= CATALOG_P . {}
unreserved_keyword(A) ::= CHAIN . {}
unreserved_keyword(A) ::= CHARACTERISTICS . {}
unreserved_keyword(A) ::= CHECKPOINT . {}
unreserved_keyword(A) ::= CLASS . {}
unreserved_keyword(A) ::= CLOSE . {}
unreserved_keyword(A) ::= CLUSTER . {}
unreserved_keyword(A) ::= COMMENT . {}
unreserved_keyword(A) ::= COMMENTS . {}
unreserved_keyword(A) ::= COMMIT . {}
unreserved_keyword(A) ::= COMMITTED . {}
unreserved_keyword(A) ::= CONFIGURATION . {}
unreserved_keyword(A) ::= CONFLICT . {}
unreserved_keyword(A) ::= CONNECTION . {}
unreserved_keyword(A) ::= CONSTRAINTS . {}
unreserved_keyword(A) ::= CONTENT_P . {}
unreserved_keyword(A) ::= CONTINUE_P . {}
unreserved_keyword(A) ::= CONVERSION_P . {}
unreserved_keyword(A) ::= COPY . {}
unreserved_keyword(A) ::= COST . {}
unreserved_keyword(A) ::= CSV . {}
unreserved_keyword(A) ::= CUBE . {}
unreserved_keyword(A) ::= CURRENT_P . {}
unreserved_keyword(A) ::= CURSOR . {}
unreserved_keyword(A) ::= CYCLE . {}
unreserved_keyword(A) ::= DATA_P . {}
unreserved_keyword(A) ::= DATABASE . {}
unreserved_keyword(A) ::= DAY_P . {}
unreserved_keyword(A) ::= DEALLOCATE . {}
unreserved_keyword(A) ::= DECLARE . {}
unreserved_keyword(A) ::= DEFAULTS . {}
unreserved_keyword(A) ::= DEFERRED . {}
unreserved_keyword(A) ::= DEFINER . {}
unreserved_keyword(A) ::= DELETE_P . {}
unreserved_keyword(A) ::= DELIMITER . {}
unreserved_keyword(A) ::= DELIMITERS . {}
unreserved_keyword(A) ::= DICTIONARY . {}
unreserved_keyword(A) ::= DISABLE_P . {}
unreserved_keyword(A) ::= DISCARD . {}
unreserved_keyword(A) ::= DOCUMENT_P . {}
unreserved_keyword(A) ::= DOMAIN_P . {}
unreserved_keyword(A) ::= DOUBLE_P . {}
unreserved_keyword(A) ::= DROP . {}
unreserved_keyword(A) ::= EACH . {}
unreserved_keyword(A) ::= ENABLE_P . {}
unreserved_keyword(A) ::= ENCODING . {}
unreserved_keyword(A) ::= ENCRYPTED . {}
unreserved_keyword(A) ::= ENUM_P . {}
unreserved_keyword(A) ::= ESCAPE . {}
unreserved_keyword(A) ::= EVENT . {}
unreserved_keyword(A) ::= EXCLUDE . {}
unreserved_keyword(A) ::= EXCLUDING . {}
unreserved_keyword(A) ::= EXCLUSIVE . {}
unreserved_keyword(A) ::= EXECUTE . {}
unreserved_keyword(A) ::= EXPLAIN . {}
unreserved_keyword(A) ::= EXTENSION . {}
unreserved_keyword(A) ::= EXTERNAL . {}
unreserved_keyword(A) ::= FAMILY . {}
unreserved_keyword(A) ::= FILTER . {}
unreserved_keyword(A) ::= FIRST_P . {}
unreserved_keyword(A) ::= FOLLOWING . {}
unreserved_keyword(A) ::= FORCE . {}
unreserved_keyword(A) ::= FORWARD . {}
unreserved_keyword(A) ::= FUNCTION . {}
unreserved_keyword(A) ::= FUNCTIONS . {}
unreserved_keyword(A) ::= GLOBAL . {}
unreserved_keyword(A) ::= GRANTED . {}
unreserved_keyword(A) ::= HANDLER . {}
unreserved_keyword(A) ::= HEADER_P . {}
unreserved_keyword(A) ::= HOLD . {}
unreserved_keyword(A) ::= HOUR_P . {}
unreserved_keyword(A) ::= IDENTITY_P . {}
unreserved_keyword(A) ::= IF_P . {}
unreserved_keyword(A) ::= IMMEDIATE . {}
unreserved_keyword(A) ::= IMMUTABLE . {}
unreserved_keyword(A) ::= IMPLICIT_P . {}
unreserved_keyword(A) ::= IMPORT_P . {}
unreserved_keyword(A) ::= INCLUDING . {}
unreserved_keyword(A) ::= INCREMENT . {}
unreserved_keyword(A) ::= INDEX . {}
unreserved_keyword(A) ::= INDEXES . {}
unreserved_keyword(A) ::= INHERIT . {}
unreserved_keyword(A) ::= INHERITS . {}
unreserved_keyword(A) ::= INLINE_P . {}
unreserved_keyword(A) ::= INPUT_P . {}
unreserved_keyword(A) ::= INSENSITIVE . {}
unreserved_keyword(A) ::= INSERT . {}
unreserved_keyword(A) ::= INSTEAD . {}
unreserved_keyword(A) ::= INVOKER . {}
unreserved_keyword(A) ::= ISOLATION . {}
unreserved_keyword(A) ::= KEY . {}
unreserved_keyword(A) ::= LABEL . {}
unreserved_keyword(A) ::= LANGUAGE . {}
unreserved_keyword(A) ::= LARGE_P . {}
unreserved_keyword(A) ::= LAST_P . {}
unreserved_keyword(A) ::= LEAKPROOF . {}
unreserved_keyword(A) ::= LEVEL . {}
unreserved_keyword(A) ::= LISTEN . {}
unreserved_keyword(A) ::= LOAD . {}
unreserved_keyword(A) ::= LOCAL . {}
unreserved_keyword(A) ::= LOCATION . {}
unreserved_keyword(A) ::= LOCK_P . {}
unreserved_keyword(A) ::= LOCKED . {}
unreserved_keyword(A) ::= LOGGED . {}
unreserved_keyword(A) ::= MAPPING . {}
unreserved_keyword(A) ::= MATCH . {}
unreserved_keyword(A) ::= MATERIALIZED . {}
unreserved_keyword(A) ::= MAXVALUE . {}
unreserved_keyword(A) ::= MINUTE_P . {}
unreserved_keyword(A) ::= MINVALUE . {}
unreserved_keyword(A) ::= MODE . {}
unreserved_keyword(A) ::= MONTH_P . {}
unreserved_keyword(A) ::= MOVE . {}
unreserved_keyword(A) ::= NAME_P . {}
unreserved_keyword(A) ::= NAMES . {}
unreserved_keyword(A) ::= NEXT . {}
unreserved_keyword(A) ::= NO . {}
unreserved_keyword(A) ::= NOTHING . {}
unreserved_keyword(A) ::= NOTIFY . {}
unreserved_keyword(A) ::= NOWAIT . {}
unreserved_keyword(A) ::= NULLS_P . {}
unreserved_keyword(A) ::= OBJECT_P . {}
unreserved_keyword(A) ::= OF . {}
unreserved_keyword(A) ::= OFF . {}
unreserved_keyword(A) ::= OIDS . {}
unreserved_keyword(A) ::= OPERATOR . {}
unreserved_keyword(A) ::= OPTION . {}
unreserved_keyword(A) ::= OPTIONS . {}
unreserved_keyword(A) ::= ORDINALITY . {}
unreserved_keyword(A) ::= OVER . {}
unreserved_keyword(A) ::= OWNED . {}
unreserved_keyword(A) ::= OWNER . {}
unreserved_keyword(A) ::= PARSER . {}
unreserved_keyword(A) ::= PARTIAL . {}
unreserved_keyword(A) ::= PARTITION . {}
unreserved_keyword(A) ::= PASSING . {}
unreserved_keyword(A) ::= PASSWORD . {}
unreserved_keyword(A) ::= PLANS . {}
unreserved_keyword(A) ::= POLICY . {}
unreserved_keyword(A) ::= PRECEDING . {}
unreserved_keyword(A) ::= PREPARE . {}
unreserved_keyword(A) ::= PREPARED . {}
unreserved_keyword(A) ::= PRESERVE . {}
unreserved_keyword(A) ::= PRIOR . {}
unreserved_keyword(A) ::= PRIVILEGES . {}
unreserved_keyword(A) ::= PROCEDURAL . {}
unreserved_keyword(A) ::= PROCEDURE . {}
unreserved_keyword(A) ::= PROGRAM . {}
unreserved_keyword(A) ::= QUOTE . {}
unreserved_keyword(A) ::= RANGE . {}
unreserved_keyword(A) ::= READ . {}
unreserved_keyword(A) ::= REASSIGN . {}
unreserved_keyword(A) ::= RECHECK . {}
unreserved_keyword(A) ::= RECURSIVE . {}
unreserved_keyword(A) ::= REF . {}
unreserved_keyword(A) ::= REFRESH . {}
unreserved_keyword(A) ::= REINDEX . {}
unreserved_keyword(A) ::= RELATIVE_P . {}
unreserved_keyword(A) ::= RELEASE . {}
unreserved_keyword(A) ::= RENAME . {}
unreserved_keyword(A) ::= REPEATABLE . {}
unreserved_keyword(A) ::= REPLACE . {}
unreserved_keyword(A) ::= REPLICA . {}
unreserved_keyword(A) ::= RESET . {}
unreserved_keyword(A) ::= RESTART . {}
unreserved_keyword(A) ::= RESTRICT . {}
unreserved_keyword(A) ::= RETURNS . {}
unreserved_keyword(A) ::= REVOKE . {}
unreserved_keyword(A) ::= ROLE . {}
unreserved_keyword(A) ::= ROLLBACK . {}
unreserved_keyword(A) ::= ROLLUP . {}
unreserved_keyword(A) ::= ROWS . {}
unreserved_keyword(A) ::= RULE . {}
unreserved_keyword(A) ::= SAVEPOINT . {}
unreserved_keyword(A) ::= SCHEMA . {}
unreserved_keyword(A) ::= SCROLL . {}
unreserved_keyword(A) ::= SEARCH . {}
unreserved_keyword(A) ::= SECOND_P . {}
unreserved_keyword(A) ::= SECURITY . {}
unreserved_keyword(A) ::= SEQUENCE . {}
unreserved_keyword(A) ::= SEQUENCES . {}
unreserved_keyword(A) ::= SERIALIZABLE . {}
unreserved_keyword(A) ::= SERVER . {}
unreserved_keyword(A) ::= SESSION . {}
unreserved_keyword(A) ::= SET . {}
unreserved_keyword(A) ::= SETS . {}
unreserved_keyword(A) ::= SHARE . {}
unreserved_keyword(A) ::= SHOW . {}
unreserved_keyword(A) ::= SIMPLE . {}
unreserved_keyword(A) ::= SKIP . {}
unreserved_keyword(A) ::= SNAPSHOT . {}
unreserved_keyword(A) ::= SQL_P . {}
unreserved_keyword(A) ::= STABLE . {}
unreserved_keyword(A) ::= STANDALONE_P . {}
unreserved_keyword(A) ::= START . {}
unreserved_keyword(A) ::= STATEMENT . {}
unreserved_keyword(A) ::= STATISTICS . {}
unreserved_keyword(A) ::= STDIN . {}
unreserved_keyword(A) ::= STDOUT . {}
unreserved_keyword(A) ::= STORAGE . {}
unreserved_keyword(A) ::= STRICT_P . {}
unreserved_keyword(A) ::= STRIP_P . {}
unreserved_keyword(A) ::= SYSID . {}
unreserved_keyword(A) ::= SYSTEM_P . {}
unreserved_keyword(A) ::= TABLES . {}
unreserved_keyword(A) ::= TABLESPACE . {}
unreserved_keyword(A) ::= TEMP . {}
unreserved_keyword(A) ::= TEMPLATE . {}
unreserved_keyword(A) ::= TEMPORARY . {}
unreserved_keyword(A) ::= TEXT_P . {}
unreserved_keyword(A) ::= TRANSACTION . {}
unreserved_keyword(A) ::= TRANSFORM . {}
unreserved_keyword(A) ::= TRIGGER . {}
unreserved_keyword(A) ::= TRUNCATE . {}
unreserved_keyword(A) ::= TRUSTED . {}
unreserved_keyword(A) ::= TYPE_P . {}
unreserved_keyword(A) ::= TYPES_P . {}
unreserved_keyword(A) ::= UNBOUNDED . {}
unreserved_keyword(A) ::= UNCOMMITTED . {}
unreserved_keyword(A) ::= UNENCRYPTED . {}
unreserved_keyword(A) ::= UNKNOWN . {}
unreserved_keyword(A) ::= UNLISTEN . {}
unreserved_keyword(A) ::= UNLOGGED . {}
unreserved_keyword(A) ::= UNTIL . {}
unreserved_keyword(A) ::= UPDATE . {}
unreserved_keyword(A) ::= VACUUM . {}
unreserved_keyword(A) ::= VALID . {}
unreserved_keyword(A) ::= VALIDATE . {}
unreserved_keyword(A) ::= VALIDATOR . {}
unreserved_keyword(A) ::= VALUE_P . {}
unreserved_keyword(A) ::= VARYING . {}
unreserved_keyword(A) ::= VERSION_P . {}
unreserved_keyword(A) ::= VIEW . {}
unreserved_keyword(A) ::= VIEWS . {}
unreserved_keyword(A) ::= VOLATILE . {}
unreserved_keyword(A) ::= WHITESPACE_P . {}
unreserved_keyword(A) ::= WITHIN . {}
unreserved_keyword(A) ::= WITHOUT . {}
unreserved_keyword(A) ::= WORK . {}
unreserved_keyword(A) ::= WRAPPER . {}
unreserved_keyword(A) ::= WRITE . {}
unreserved_keyword(A) ::= XML_P . {}
unreserved_keyword(A) ::= YEAR_P . {}
unreserved_keyword(A) ::= YES_P . {}
unreserved_keyword(A) ::= ZONE . {}

col_name_keyword(A) ::= BETWEEN . {}
col_name_keyword(A) ::= BIGINT . {}
col_name_keyword(A) ::= BIT . {}
col_name_keyword(A) ::= BOOLEAN_P . {}
col_name_keyword(A) ::= CHAR_P . {}
col_name_keyword(A) ::= CHARACTER . {}
col_name_keyword(A) ::= COALESCE . {}
col_name_keyword(A) ::= DEC . {}
col_name_keyword(A) ::= DECIMAL_P . {}
col_name_keyword(A) ::= EXISTS . {}
col_name_keyword(A) ::= EXTRACT . {}
col_name_keyword(A) ::= FLOAT_P . {}
col_name_keyword(A) ::= GREATEST . {}
col_name_keyword(A) ::= GROUPING . {}
col_name_keyword(A) ::= INOUT . {}
col_name_keyword(A) ::= INT_P . {}
col_name_keyword(A) ::= INTEGER . {}
col_name_keyword(A) ::= INTERVAL . {}
col_name_keyword(A) ::= LEAST . {}
col_name_keyword(A) ::= NATIONAL . {}
col_name_keyword(A) ::= NCHAR . {}
col_name_keyword(A) ::= NONE . {}
col_name_keyword(A) ::= NULLIF . {}
col_name_keyword(A) ::= NUMERIC . {}
col_name_keyword(A) ::= OUT_P . {}
col_name_keyword(A) ::= OVERLAY . {}
col_name_keyword(A) ::= POSITION . {}
col_name_keyword(A) ::= PRECISION . {}
col_name_keyword(A) ::= REAL . {}
col_name_keyword(A) ::= ROW . {}
col_name_keyword(A) ::= SETOF . {}
col_name_keyword(A) ::= SMALLINT . {}
col_name_keyword(A) ::= SUBSTRING . {}
col_name_keyword(A) ::= TIME . {}
col_name_keyword(A) ::= TIMESTAMP . {}
col_name_keyword(A) ::= TREAT . {}
col_name_keyword(A) ::= TRIM . {}
col_name_keyword(A) ::= VALUES . {}
col_name_keyword(A) ::= VARCHAR . {}
col_name_keyword(A) ::= XMLATTRIBUTES . {}
col_name_keyword(A) ::= XMLCONCAT . {}
col_name_keyword(A) ::= XMLELEMENT . {}
col_name_keyword(A) ::= XMLEXISTS . {}
col_name_keyword(A) ::= XMLFOREST . {}
col_name_keyword(A) ::= XMLPARSE . {}
col_name_keyword(A) ::= XMLPI . {}
col_name_keyword(A) ::= XMLROOT . {}
col_name_keyword(A) ::= XMLSERIALIZE . {}
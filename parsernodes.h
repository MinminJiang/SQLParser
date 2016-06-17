/*-------------------------------------------------------------------------
 *
 * parsenodes.h
 *	  definitions for parse tree nodes
 *
 * Many of the node types used in parsetrees include a "location" field.
 * This is a byte (not character) offset in the original source text, to be
 * used for positioning an error cursor when there is an error related to
 * the node.  Access to the original source text is needed to make use of
 * the location.
 *
 *-------------------------------------------------------------------------
 */

#ifndef PARSENODES_H
#define PARSENODES_H


#include "nodes.h"


typedef unsigned char uint8;	/* == 8 bits */
typedef unsigned short uint16;	/* == 16 bits */
typedef unsigned int uint32;	/* == 32 bits */

typedef unsigned int Oid;



/* Possible sources of a Query */
typedef enum QuerySource
{
	QSRC_ORIGINAL,				/* original parsetree (explicit query) */
	QSRC_PARSER,				/* added by parse analysis (now unused) */
	QSRC_INSTEAD_RULE,			/* added by unconditional INSTEAD rule */
	QSRC_QUAL_INSTEAD_RULE,		/* added by conditional INSTEAD rule */
	QSRC_NON_INSTEAD_RULE		/* added by non-INSTEAD rule */
} QuerySource;

/*
 * CommonTableExpr -
 *	   representation of WITH list element
 *
 * We don't currently support the SEARCH or CYCLE clause.
 */
typedef struct CommonTableExpr
{
	NodeTag		type;
	char	   *ctename;		/* query name (never qualified) */
	List	   *aliascolnames;	/* optional list of column names */
	/* SelectStmt/InsertStmt/etc before parse analysis, Query afterwards: */
	Node	   *ctequery;		/* the CTE's subquery */
	int			location;		/* token location, or -1 if unknown */
	/* These fields are set during parse analysis: */
	bool		cterecursive;	/* is this CTE actually recursive? */
	int			cterefcount;	/* number of RTEs referencing this CTE
								 * (excluding internal self-references) */
	List	   *ctecolnames;	/* list of output column names */
	List	   *ctecoltypes;	/* OID list of output column type OIDs */
	List	   *ctecoltypmods;	/* integer list of output column typmods */
	List	   *ctecolcollations;		/* OID list of column collation OIDs */
} CommonTableExpr;

/*
 * Expression kinds distinguished by transformExpr().  Many of these are not
 * semantically distinct so far as expression transformation goes; rather,
 * we distinguish them so that context-specific error messages can be printed.
 *
 * Note: EXPR_KIND_OTHER is not used in the core code, but is left for use
 * by extension code that might need to call transformExpr().  The core code
 * will not enforce any context-driven restrictions on EXPR_KIND_OTHER
 * expressions, so the caller would have to check for sub-selects, aggregates,
 * and window functions if those need to be disallowed.
 */
typedef enum ParseExprKind
{
	EXPR_KIND_NONE = 0,			/* "not in an expression" */
	EXPR_KIND_OTHER,			/* reserved for extensions */
	EXPR_KIND_JOIN_ON,			/* JOIN ON */
	EXPR_KIND_JOIN_USING,		/* JOIN USING */
	EXPR_KIND_FROM_SUBSELECT,	/* sub-SELECT in FROM clause */
	EXPR_KIND_FROM_FUNCTION,	/* function in FROM clause */
	EXPR_KIND_WHERE,			/* WHERE */
	EXPR_KIND_HAVING,			/* HAVING */
	EXPR_KIND_FILTER,			/* FILTER */
	EXPR_KIND_WINDOW_PARTITION, /* window definition PARTITION BY */
	EXPR_KIND_WINDOW_ORDER,		/* window definition ORDER BY */
	EXPR_KIND_WINDOW_FRAME_RANGE,		/* window frame clause with RANGE */
	EXPR_KIND_WINDOW_FRAME_ROWS,	/* window frame clause with ROWS */
	EXPR_KIND_SELECT_TARGET,	/* SELECT target list item */
	EXPR_KIND_INSERT_TARGET,	/* INSERT target list item */
	EXPR_KIND_UPDATE_SOURCE,	/* UPDATE assignment source item */
	EXPR_KIND_UPDATE_TARGET,	/* UPDATE assignment target item */
	EXPR_KIND_GROUP_BY,			/* GROUP BY */
	EXPR_KIND_ORDER_BY,			/* ORDER BY */
	EXPR_KIND_DISTINCT_ON,		/* DISTINCT ON */
	EXPR_KIND_LIMIT,			/* LIMIT */
	EXPR_KIND_OFFSET,			/* OFFSET */
	EXPR_KIND_RETURNING,		/* RETURNING */
	EXPR_KIND_VALUES,			/* VALUES */
	EXPR_KIND_CHECK_CONSTRAINT, /* CHECK constraint for a table */
	EXPR_KIND_DOMAIN_CHECK,		/* CHECK constraint for a domain */
	EXPR_KIND_COLUMN_DEFAULT,	/* default value for a table column */
	EXPR_KIND_FUNCTION_DEFAULT, /* default parameter value for function */
	EXPR_KIND_INDEX_EXPRESSION, /* index expression */
	EXPR_KIND_INDEX_PREDICATE,	/* index predicate */
	EXPR_KIND_ALTER_COL_TRANSFORM,		/* transform expr in ALTER COLUMN TYPE */
	EXPR_KIND_EXECUTE_PARAMETER,	/* parameter value in EXECUTE */
	EXPR_KIND_TRIGGER_WHEN,		/* WHEN condition in CREATE TRIGGER */
	EXPR_KIND_POLICY			/* USING or WITH CHECK expr in policy */
} ParseExprKind;

/****************************************************************************
 *	Nodes for a Query tree
 ****************************************************************************/

/*--------------------
 * RangeTblEntry -
 *	  A range table is a List of RangeTblEntry nodes.
 *
 *	  A range table entry may represent a plain relation, a sub-select in
 *	  FROM, or the result of a JOIN clause.  (Only explicit JOIN syntax
 *	  produces an RTE, not the implicit join resulting from multiple FROM
 *	  items.  This is because we only need the RTE to deal with SQL features
 *	  like outer joins and join-output-column aliasing.)  Other special
 *	  RTE types also exist, as indicated by RTEKind.
 *
 *	  Note that we consider RTE_RELATION to cover anything that has a pg_class
 *	  entry.  relkind distinguishes the sub-cases.
 *
 *	  alias is an Alias node representing the AS alias-clause attached to the
 *	  FROM expression, or NULL if no clause.
 *
 *	  eref is the table reference name and column reference names (either
 *	  real or aliases).  Note that system columns (OID etc) are not included
 *	  in the column list.
 *	  eref->aliasname is required to be present, and should generally be used
 *	  to identify the RTE for error messages etc.
 *
 *	  In RELATION RTEs, the colnames in both alias and eref are indexed by
 *	  physical attribute number; this means there must be colname entries for
 *	  dropped columns.  When building an RTE we insert empty strings ("") for
 *	  dropped columns.  Note however that a stored rule may have nonempty
 *	  colnames for columns dropped since the rule was created (and for that
 *	  matter the colnames might be out of date due to column renamings).
 *	  The same comments apply to FUNCTION RTEs when a function's return type
 *	  is a named composite type.
 *
 *	  In JOIN RTEs, the colnames in both alias and eref are one-to-one with
 *	  joinaliasvars entries.  A JOIN RTE will omit columns of its inputs when
 *	  those columns are known to be dropped at parse time.  Again, however,
 *	  a stored rule might contain entries for columns dropped since the rule
 *	  was created.  (This is only possible for columns not actually referenced
 *	  in the rule.)  When loading a stored rule, we replace the joinaliasvars
 *	  items for any such columns with null pointers.  (We can't simply delete
 *	  them from the joinaliasvars list, because that would affect the attnums
 *	  of Vars referencing the rest of the list.)
 *
 *	  inh is TRUE for relation references that should be expanded to include
 *	  inheritance children, if the rel has any.  This *must* be FALSE for
 *	  RTEs other than RTE_RELATION entries.
 *
 *	  inFromCl marks those range variables that are listed in the FROM clause.
 *	  It's false for RTEs that are added to a query behind the scenes, such
 *	  as the NEW and OLD variables for a rule, or the subqueries of a UNION.
 *	  This flag is not used anymore during parsing, since the parser now uses
 *	  a separate "namespace" data structure to control visibility, but it is
 *	  needed by ruleutils.c to determine whether RTEs should be shown in
 *	  decompiled queries.
 *
 *	  requiredPerms and checkAsUser specify run-time access permissions
 *	  checks to be performed at query startup.  The user must have *all*
 *	  of the permissions that are OR'd together in requiredPerms (zero
 *	  indicates no permissions checking).  If checkAsUser is not zero,
 *	  then do the permissions checks using the access rights of that user,
 *	  not the current effective user ID.  (This allows rules to act as
 *	  setuid gateways.)  Permissions checks only apply to RELATION RTEs.
 *
 *	  For SELECT/INSERT/UPDATE permissions, if the user doesn't have
 *	  table-wide permissions then it is sufficient to have the permissions
 *	  on all columns identified in selectedCols (for SELECT) and/or
 *	  insertedCols and/or updatedCols (INSERT with ON CONFLICT DO UPDATE may
 *	  have all 3).  selectedCols, insertedCols and updatedCols are bitmapsets,
 *	  which cannot have negative integer members, so we subtract
 *	  FirstLowInvalidHeapAttributeNumber from column numbers before storing
 *	  them in these fields.  A whole-row Var reference is represented by
 *	  setting the bit for InvalidAttrNumber.
 *--------------------
 */
typedef enum RTEKind
{
	RTE_RELATION,				/* ordinary relation reference */
	RTE_SUBQUERY,				/* subquery in FROM */
	RTE_JOIN,					/* join */
	RTE_FUNCTION,				/* function in FROM */
	RTE_VALUES,					/* VALUES (<exprlist>), (<exprlist>), ... */
	RTE_CTE						/* common table expr (WITH list element) */
} RTEKind;

typedef struct RangeTblEntry
{
	NodeTag		type;

	RTEKind		rtekind;		/* see above */

	/*
	 * XXX the fields applicable to only some rte kinds should be merged into
	 * a union.  I didn't do this yet because the diffs would impact a lot of
	 * code that is being actively worked on.  FIXME someday.
	 */

	/*
	 * Fields valid for a plain relation RTE (else zero):
	 */
	Oid			relid;			/* OID of the relation */
	char		relkind;		/* relation kind (see pg_class.relkind) */
	struct TableSampleClause *tablesample;		/* sampling info, or NULL */

	/*
	 * Fields valid for a subquery RTE (else NULL):
	 */
	Query	   *subquery;		/* the sub-query */
	bool		security_barrier;		/* is from security_barrier view? */

	/*
	 * Fields valid for a join RTE (else NULL/zero):
	 *
	 * joinaliasvars is a list of (usually) Vars corresponding to the columns
	 * of the join result.  An alias Var referencing column K of the join
	 * result can be replaced by the K'th element of joinaliasvars --- but to
	 * simplify the task of reverse-listing aliases correctly, we do not do
	 * that until planning time.  In detail: an element of joinaliasvars can
	 * be a Var of one of the join's input relations, or such a Var with an
	 * implicit coercion to the join's output column type, or a COALESCE
	 * expression containing the two input column Vars (possibly coerced).
	 * Within a Query loaded from a stored rule, it is also possible for
	 * joinaliasvars items to be null pointers, which are placeholders for
	 * (necessarily unreferenced) columns dropped since the rule was made.
	 * Also, once planning begins, joinaliasvars items can be almost anything,
	 * as a result of subquery-flattening substitutions.
	 */
	JoinType	jointype;		/* type of join */
	List	   *joinaliasvars;	/* list of alias-var expansions */

	/*
	 * Fields valid for a function RTE (else NIL/zero):
	 *
	 * When funcordinality is true, the eref->colnames list includes an alias
	 * for the ordinality column.  The ordinality column is otherwise
	 * implicit, and must be accounted for "by hand" in places such as
	 * expandRTE().
	 */
	List	   *functions;		/* list of RangeTblFunction nodes */
	bool		funcordinality; /* is this called WITH ORDINALITY? */

	/*
	 * Fields valid for a values RTE (else NIL):
	 */
	List	   *values_lists;	/* list of expression lists */
	List	   *values_collations;		/* OID list of column collation OIDs */

	/*
	 * Fields valid for a CTE RTE (else NULL/zero):
	 */
	char	   *ctename;		/* name of the WITH list item */
	//Index		ctelevelsup;	/* number of query levels up */
	bool		self_reference; /* is this a recursive self-reference? */
	List	   *ctecoltypes;	/* OID list of column type OIDs */
	List	   *ctecoltypmods;	/* integer list of column typmods */
	List	   *ctecolcollations;		/* OID list of column collation OIDs */

	/*
	 * Fields valid in all RTEs:
	 */
	Alias	   *alias;			/* user-written alias clause, if any */
	Alias	   *eref;			/* expanded reference names */
	bool		lateral;		/* subquery, function, or values is LATERAL? */
	bool		inh;			/* inheritance requested? */
	bool		inFromCl;		/* present in FROM clause? */
	//AclMode		requiredPerms;	/* bitmask of required access permissions */
	Oid			checkAsUser;	/* if valid, check access as this role */
	//Bitmapset  *selectedCols;	/* columns needing SELECT permission */
	//Bitmapset  *insertedCols;	/* columns needing INSERT permission */
	//Bitmapset  *updatedCols;	/* columns needing UPDATE permission */
	List	   *securityQuals;	/* any security barrier quals to apply */
} RangeTblEntry;


/*
 * State information used during parse analysis
 *
 * parentParseState: NULL in a top-level ParseState.  When parsing a subquery,
 * links to current parse state of outer query.
 *
 * p_sourcetext: source string that generated the raw parsetree being
 * analyzed, or NULL if not available.  (The string is used only to
 * generate cursor positions in error messages: we need it to convert
 * byte-wise locations in parse structures to character-wise cursor
 * positions.)
 *
 * p_rtable: list of RTEs that will become the rangetable of the query.
 * Note that neither relname nor refname of these entries are necessarily
 * unique; searching the rtable by name is a bad idea.
 *
 * p_joinexprs: list of JoinExpr nodes associated with p_rtable entries.
 * This is one-for-one with p_rtable, but contains NULLs for non-join
 * RTEs, and may be shorter than p_rtable if the last RTE(s) aren't joins.
 *
 * p_joinlist: list of join items (RangeTblRef and JoinExpr nodes) that
 * will become the fromlist of the query's top-level FromExpr node.
 *
 * p_namespace: list of ParseNamespaceItems that represents the current
 * namespace for table and column lookup.  (The RTEs listed here may be just
 * a subset of the whole rtable.  See ParseNamespaceItem comments below.)
 *
 * p_lateral_active: TRUE if we are currently parsing a LATERAL subexpression
 * of this parse level.  This makes p_lateral_only namespace items visible,
 * whereas they are not visible when p_lateral_active is FALSE.
 *
 * p_ctenamespace: list of CommonTableExprs (WITH items) that are visible
 * at the moment.  This is entirely different from p_namespace because a CTE
 * is not an RTE, rather "visibility" means you could make an RTE from it.
 *
 * p_future_ctes: list of CommonTableExprs (WITH items) that are not yet
 * visible due to scope rules.  This is used to help improve error messages.
 *
 * p_parent_cte: CommonTableExpr that immediately contains the current query,
 * if any.
 *
 * p_windowdefs: list of WindowDefs representing WINDOW and OVER clauses.
 * We collect these while transforming expressions and then transform them
 * afterwards (so that any resjunk tlist items needed for the sort/group
 * clauses end up at the end of the query tlist).  A WindowDef's location in
 * this list, counting from 1, is the winref number to use to reference it.
 */
struct ParseState
{
	struct ParseState *parentParseState;		/* stack link */
	const char *p_sourcetext;	/* source text, or NULL if not available */
	List	   *p_rtable;		/* range table so far */
	List	   *p_joinexprs;	/* JoinExprs for RTE_JOIN p_rtable entries */
	List	   *p_joinlist;		/* join items so far (will become FromExpr
								 * node's fromlist) */
	List	   *p_namespace;	/* currently-referenceable RTEs (List of
								 * ParseNamespaceItem) */
	bool		p_lateral_active;		/* p_lateral_only items visible? */
	List	   *p_ctenamespace; /* current namespace for common table exprs */
	List	   *p_future_ctes;	/* common table exprs not yet in namespace */
	CommonTableExpr *p_parent_cte;		/* this query's containing CTE */
	List	   *p_windowdefs;	/* raw representations of window clauses */
	ParseExprKind p_expr_kind;	/* what kind of expression we're parsing */
	int			p_next_resno;	/* next targetlist resno to assign */
	List	   *p_multiassign_exprs;	/* junk tlist entries for multiassign */
	List	   *p_locking_clause;		/* raw FOR UPDATE/FOR SHARE info */
	Node	   *p_value_substitute;		/* what to replace VALUE with, if any */
	bool		p_hasAggs;
	bool		p_hasWindowFuncs;
	bool		p_hasSubLinks;
	bool		p_hasModifyingCTE;
	bool		p_is_insert;
	bool		p_locked_from_parent;
	//Relation	p_target_relation;
	RangeTblEntry *p_target_rangetblentry;

	/*
	 * Optional hook functions for parser callbacks.  These are null unless
	 * set up by the caller of make_parsestate.
	 */
	//PreParseColumnRefHook p_pre_columnref_hook;
	//PostParseColumnRefHook p_post_columnref_hook;
	//ParseParamRefHook p_paramref_hook;
	//CoerceParamHook p_coerce_param_hook;
	void	   *p_ref_hook_state;		/* common passthrough link for above */
};




/*----------
 * FromExpr - represents a FROM ... WHERE ... construct
 *
 * This is both more flexible than a JoinExpr (it can have any number of
 * children, including zero) and less so --- we don't need to deal with
 * aliases and so on.  The output column set is implicitly just the union
 * of the outputs of the children.
 *----------
 */
typedef struct FromExpr
{
	NodeTag		type;
	List	   *fromlist;		/* List of join subtrees */
	Node	   *quals;			/* qualifiers on join, if any */
} FromExpr;


/*----------
 * OnConflictExpr - represents an ON CONFLICT DO ... expression
 *
 * The optimizer requires a list of inference elements, and optionally a WHERE
 * clause to infer a unique index.  The unique index (or, occasionally,
 * indexes) inferred are used to arbitrate whether or not the alternative ON
 * CONFLICT path is taken.
 *----------
 */
typedef struct OnConflictExpr
{
	NodeTag		type;
	OnConflictAction action;	/* DO NOTHING or UPDATE? */

	/* Arbiter */
	List	   *arbiterElems;	/* unique index arbiter list (of
								 * InferenceElem's) */
	Node	   *arbiterWhere;	/* unique index arbiter WHERE clause */
	Oid			constraint;		/* pg_constraint OID for arbiter */

	/* ON CONFLICT UPDATE */
	List	   *onConflictSet;	/* List of ON CONFLICT SET TargetEntrys */
	Node	   *onConflictWhere;	/* qualifiers to restrict UPDATE to */
	int			exclRelIndex;	/* RT index of 'excluded' relation */
	List	   *exclRelTlist;	/* tlist of the EXCLUDED pseudo relation */
} OnConflictExpr;



/*****************************************************************************
 *	Query Tree
 *****************************************************************************/

/*
 * Query -
 *	  Parse analysis turns all statements into a Query tree
 *	  for further processing by the rewriter and planner.
 *
 *	  Utility statements (i.e. non-optimizable statements) have the
 *	  utilityStmt field set, and the Query itself is mostly dummy.
 *	  DECLARE CURSOR is a special case: it is represented like a SELECT,
 *	  but the original DeclareCursorStmt is stored in utilityStmt.
 *
 *	  Planning converts a Query tree into a Plan tree headed by a PlannedStmt
 *	  node --- the Query structure is not used by the executor.
 */
typedef struct Query
{
	NodeTag		type;

	CmdType		commandType;	/* select|insert|update|delete|utility */

	QuerySource querySource;	/* where did I come from? */

	uint32		queryId;		/* query identifier (can be set by plugins) */

	bool		canSetTag;		/* do I set the command result tag? */

	Node	   *utilityStmt;	/* non-null if this is DECLARE CURSOR or a
								 * non-optimizable statement */

	int			resultRelation; /* rtable index of target relation for
								 * INSERT/UPDATE/DELETE; 0 for SELECT */

	bool		hasAggs;		/* has aggregates in tlist or havingQual */
	bool		hasWindowFuncs; /* has window functions in tlist */
	bool		hasSubLinks;	/* has subquery SubLink */
	bool		hasDistinctOn;	/* distinctClause is from DISTINCT ON */
	bool		hasRecursive;	/* WITH RECURSIVE was specified */
	bool		hasModifyingCTE;	/* has INSERT/UPDATE/DELETE in WITH */
	bool		hasForUpdate;	/* FOR [KEY] UPDATE/SHARE was specified */
	bool		hasRowSecurity; /* row security applied? */

	List	   *cteList;		/* WITH list (of CommonTableExpr's) */

	List	   *rtable;			/* list of range table entries */
	FromExpr   *jointree;		/* table join tree (FROM and WHERE clauses) */

	List	   *targetList;		/* target list (of TargetEntry) */

	OnConflictExpr *onConflict; /* ON CONFLICT DO [NOTHING | UPDATE] */

	List	   *returningList;	/* return-values list (of TargetEntry) */

	List	   *groupClause;	/* a list of SortGroupClause's */

	List	   *groupingSets;	/* a list of GroupingSet's if present */

	Node	   *havingQual;		/* qualifications applied to groups */

	List	   *windowClause;	/* a list of WindowClause's */

	List	   *distinctClause; /* a list of SortGroupClause's */

	List	   *sortClause;		/* a list of SortGroupClause's */

	Node	   *limitOffset;	/* # of result tuples to skip (int8 expr) */
	Node	   *limitCount;		/* # of result tuples to return (int8 expr) */

	List	   *rowMarks;		/* a list of RowMarkClause's */

	Node	   *setOperations;	/* set-operation tree if this is top level of
								 * a UNION/INTERSECT/EXCEPT query */

	List	   *constraintDeps; /* a list of pg_constraint OIDs that the query
								 * depends on to be semantically valid */

	List	   *withCheckOptions;	/* a list of WithCheckOption's, which are
									 * only added during rewrite and therefore
									 * are not written out as part of Query. */
} Query;


/*
 * WithClause -
 *	   representation of WITH clause
 *
 * Note: WithClause does not propagate into the Query representation;
 * but CommonTableExpr does.
 */
typedef struct WithClause
{
	NodeTag		type;
	List	   *ctes;			/* list of CommonTableExprs */
	bool		recursive;		/* true = WITH RECURSIVE */
	int			location;		/* token location, or -1 if unknown */
} WithClause;


/*
 * InferClause -
 *		ON CONFLICT unique index inference clause
 *
 * Note: InferClause does not propagate into the Query representation.
 */
typedef struct InferClause
{
	NodeTag		type;
	List	   *indexElems;		/* IndexElems to infer unique index */
	Node	   *whereClause;	/* qualification (partial-index predicate) */
	char	   *conname;		/* Constraint name, or NULL if unnamed */
	int			location;		/* token location, or -1 if unknown */
} InferClause;


/*
 * OnConflictClause -
 *		representation of ON CONFLICT clause
 *
 * Note: OnConflictClause does not propagate into the Query representation.
 */
typedef struct OnConflictClause
{
	NodeTag		type;
	OnConflictAction action;	/* DO NOTHING or UPDATE? */
	InferClause *infer;			/* Optional index inference clause */
	List	   *targetList;		/* the target list (of ResTarget) */
	Node	   *whereClause;	/* qualifications */
	int			location;		/* token location, or -1 if unknown */
} OnConflictClause;



/*****************************************************************************
 *		Optimizable Statements
 *****************************************************************************/

/* ----------------------
 *		Insert Statement
 *
 * The source expression is represented by SelectStmt for both the
 * SELECT and VALUES cases.  If selectStmt is NULL, then the query
 * is INSERT ... DEFAULT VALUES.
 * ----------------------
 */

typedef struct InsertStmt
{
	NodeTag		type;
	RangeVar   *relation;		/* relation to insert into */
	List	   *cols;			/* optional: names of the target columns */
	Node	   *selectStmt;		/* the source SELECT/VALUES, or NULL */
	OnConflictClause *onConflictClause; /* ON CONFLICT clause */
	List	   *returningList;	/* list of expressions to return */
	WithClause *withClause;		/* WITH clause */
} InsertStmt;

/* ----------------------
 *		Delete Statement
 * ----------------------
 */
typedef struct DeleteStmt
{
	NodeTag		type;
	RangeVar   *relation;		/* relation to delete from */
	List	   *usingClause;	/* optional using clause for more tables */
	Node	   *whereClause;	/* qualifications */
	List	   *returningList;	/* list of expressions to return */
	WithClause *withClause;		/* WITH clause */
} DeleteStmt;

/* ----------------------
 *		Update Statement
 * ----------------------
 */
typedef struct UpdateStmt
{
	NodeTag		type;
	RangeVar   *relation;		/* relation to update */
	List	   *targetList;		/* the target list (of ResTarget) */
	Node	   *whereClause;	/* qualifications */
	List	   *fromClause;		/* optional from clause for more tables */
	List	   *returningList;	/* list of expressions to return */
	WithClause *withClause;		/* WITH clause */
} UpdateStmt;

typedef struct SelectStmt
{
	NodeTag		type;

	/*
	 * These fields are used only in "leaf" SelectStmts.
	 */
	List	   *distinctClause; /* NULL, list of DISTINCT ON exprs, or
								 * lcons(NIL,NIL) for all (SELECT DISTINCT) */
	IntoClause *intoClause;		/* target for SELECT INTO */
	List	   *targetList;		/* the target list (of ResTarget) */
	List	   *fromClause;		/* the FROM clause */
	Node	   *whereClause;	/* WHERE qualification */
	List	   *groupClause;	/* GROUP BY clauses */
	Node	   *havingClause;	/* HAVING conditional-expression */
	List	   *windowClause;	/* WINDOW window_name AS (...), ... */

	/*
	 * In a "leaf" node representing a VALUES list, the above fields are all
	 * null, and instead this field is set.  Note that the elements of the
	 * sublists are just expressions, without ResTarget decoration. Also note
	 * that a list element can be DEFAULT (represented as a SetToDefault
	 * node), regardless of the context of the VALUES list. It's up to parse
	 * analysis to reject that where not valid.
	 */
	List	   *valuesLists;	/* untransformed list of expression lists */

	/*
	 * These fields are used in both "leaf" SelectStmts and upper-level
	 * SelectStmts.
	 */
	List	   *sortClause;		/* sort clause (a list of SortBy's) */
	Node	   *limitOffset;	/* # of result tuples to skip */
	Node	   *limitCount;		/* # of result tuples to return */
	List	   *lockingClause;	/* FOR UPDATE (list of LockingClause's) */
	WithClause *withClause;		/* WITH clause */

	/*
	 * These fields are used only in upper-level SelectStmts.
	 */
	SetOperation op;			/* type of set op */
	bool		all;			/* ALL specified? */
	struct SelectStmt *larg;	/* left child */
	struct SelectStmt *rarg;	/* right child */
	/* Eventually add fields for CORRESPONDING spec here */
} SelectStmt;




#endif





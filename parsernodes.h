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







#endif





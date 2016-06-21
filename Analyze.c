#include "Parsenodes.h"
#include "Nodes.h"

static Query *transformDeleteStmt(ParseState *pstate, DeleteStmt *stmt);
static Query *transformInsertStmt(ParseState *pstate, InsertStmt *stmt);
static Query *transformSelectStmt(ParseState *pstate, SelectStmt *stmt);
static Query *transformUpdateStmt(ParseState *pstate, UpdateStmt *stmt);
//static Query *transformExplainStmt(ParseState *pstate,ExplainStmt *stmt);


static Query *
transformDeleteStmt(ParseState *pstate, DeleteStmt *stmt)
{
	
}


static Query *
transformInsertStmt(ParseState *pstate, InsertStmt *stmt)
{
	
}

static Query *
transformSelectStmt(ParseState *pstate, SelectStmt *stmt)
{
	
}


static Query *
transformUpdateStmt(ParseState *pstate, UpdateStmt *stmt)
{
	
}


/*
 * transformStmt -
 *	  recursively transform a Parse tree into a Query tree.
 */
Query *
transformStmt(ParseState *pstate, Node *parseTree)
{
	Query	   *result;

	switch (nodeTag(parseTree))
	{
			/*
			 * Optimizable statements
			 */
		case T_InsertStmt:
			result = transformInsertStmt(pstate, (InsertStmt *) parseTree);
			break;

		case T_DeleteStmt:
			result = transformDeleteStmt(pstate, (DeleteStmt *) parseTree);
			break;

		case T_UpdateStmt:
			result = transformUpdateStmt(pstate, (UpdateStmt *) parseTree);
			break;

		case T_SelectStmt:
			{
				SelectStmt *n = (SelectStmt *) parseTree;

				if (n->valuesLists)
					result = transformValuesClause(pstate, n);
				else if (n->op == SETOP_NONE)
					result = transformSelectStmt(pstate, n);
				else
					result = transformSetOperationStmt(pstate, n);
			}
			break;

		default:

			/*
			 * other statements don't require any transformation; just return
			 * the original parsetree with a Query node plastered on top.
			 */
			result = makeNode(Query);
			result->commandType = CMD_UTILITY;
			result->utilityStmt = (Node *) parseTree;
			break;
	}

	/* Mark as original query until we learn differently */
	result->querySource = QSRC_ORIGINAL;
	result->canSetTag = true;

	return result;
}





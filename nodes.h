/*-------------------------------------------------------------------------
 *
 * nodes.h
 *	  Definitions for tagged nodes.
 *
 *-------------------------------------------------------------------------
 */
#ifndef NODES_H
#define NODES_H


typedef struct ListCell ListCell;

typedef struct List
{
	NodeTag		type;			/* T_List, T_IntList, or T_OidList */
	int			length;
	ListCell   *head;
	ListCell   *tail;
} List;

/*
 * CmdType -
 *	  enums for type of operation represented by a Query or PlannedStmt
 *
 * This is needed in both parsenodes.h and plannodes.h, so put it here...
 */
typedef enum CmdType
{
	CMD_UNKNOWN,
	CMD_SELECT,					/* select stmt */
	CMD_UPDATE,					/* update stmt */
	CMD_INSERT,					/* insert stmt */
	CMD_DELETE,
	CMD_UTILITY,				/* cmds like create, destroy, copy, vacuum,
								 * etc. */
	CMD_NOTHING					/* dummy command for instead nothing rules
								 * with qual */
} CmdType;


typedef struct Alias
{
	NodeTag		type;
	char	   *aliasname;		/* aliased rel name (never qualified) */
	List	   *colnames;		/* optional list of column aliases */
} Alias;

typedef enum InhOption
{
	INH_NO,						/* Do NOT scan child tables */
	INH_YES,					/* DO scan child tables */
	INH_DEFAULT					/* Use current SQL_inheritance option */
} InhOption;

typedef struct RangeVar
{
	NodeTag		type;
	char	   *catalogname;	/* the catalog (database) name, or NULL */
	char	   *schemaname;		/* the schema name, or NULL */
	char	   *relname;		/* the relation/sequence name */
	InhOption	inhOpt;			/* expand rel by inheritance? recursively act
								 * on children? */
	char		relpersistence; /* see RELPERSISTENCE_* in pg_class.h */
	Alias	   *alias;			/* table alias & optional column aliases */
	int			location;		/* token location, or -1 if unknown */
} RangeVar;


/*
 * OnConflictAction -
 *	  "ON CONFLICT" clause type of query
 *
 * This is needed in both parsenodes.h and plannodes.h, so put it here...
 */
typedef enum OnConflictAction
{
	ONCONFLICT_NONE,			/* No "ON CONFLICT" clause */
	ONCONFLICT_NOTHING,			/* ON CONFLICT ... DO NOTHING */
	ONCONFLICT_UPDATE			/* ON CONFLICT ... DO UPDATE */
} OnConflictAction;


/*
 * The first field of a node of any type is guaranteed to be the NodeTag.
 * Hence the type of any node can be gotten by casting it to Node. Declaring
 * a variable to be of Node * (instead of void *) can also facilitate
 * debugging.
 */
typedef struct Node
{
	NodeTag		type;
} Node;

#define nodeTag(nodeptr)		(((const Node*)(nodeptr))->type)

/*
** Each token coming out of the lexer is an instance of
** this structure.  Tokens are also used as part of an expression.
**
** Note if Token.z==0 then Token.dyn and Token.n are undefined and
** may contain random values.  Do not make any assumptions about Token.dyn
** and Token.n when Token.z==0.
*/
struct Token {
  const char *z;     /* Text of the token.  Not NULL-terminated! */
  unsigned int n;    /* Number of characters in this token */
};


typedef enum NodeTag
{
	T_Invalid = 0,

	T_InsertStmt,
	T_DeleteStmt,
	T_UpdateStmt,
	T_SelectStmt

}NodeTag;





#endif

#ifndef COMMON_H
#define COMMON_H


#define AceProxy_OK  0
#define AceProxy_ERR 1

typedef struct Value
{
	NodeTag		type;			/* tag appropriately (eg. T_String) */
	union ValUnion
	{
		long		ival;		/* machine integer */
		char	   *str;		/* string */
	}			val;
} Value;

#define intVal(v)		(((Value *)(v))->val.ival)
#define floatVal(v)		atof(((Value *)(v))->val.str)
#define strVal(v)		(((Value *)(v))->val.str)

extern Value *makeInteger(long i);
extern Value *makeFloat(char *numericStr);
extern Value *makeString(char *str);
extern Value *makeBitString(char *str);


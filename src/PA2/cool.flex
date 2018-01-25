/*
 *  The scanner definition for COOL.
 */

%option stack

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>
#include <ctype.h>
#include <stdio.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

#define APPEND_CHR(chr) do { \
	*string_buf_ptr++ = (chr); \
	if (string_buf_ptr == string_buf + MAX_STR_CONST) { \
	    cool_yylval.error_msg = "String constant too long"; \
		BEGIN(endstr); \
	} \
} while (0)

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
%}

%x str
%x endstr
%x comment
%x linecomment

/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN			<-
LE				<=
DIGIT			[0-9]
LINE_COMMENT    "--"

%%

 /*
  *  Nested comments
  */

"(*"         { yy_push_state(comment); }

<comment>{
    "(*"            { yy_push_state(comment); }
    [^*\n(]*         { ; /* eat anything that's not a '*' */ }
    \n              { ++curr_lineno; }
    "*"+")"         { yy_pop_state(); }
    <<EOF>>		    { cool_yylval.error_msg = "EOF in comment"; yy_pop_state(); return ERROR; }
    . { ; }
}

--[^\n]* { BEGIN(linecomment); }
<linecomment>\n { ++curr_lineno; BEGIN(INITIAL); }

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }
{ASSIGN}		{ return ASSIGN; }
{LE}			{ return LE; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:class)      { return CLASS; }
(?i:if)         { return IF; }
(?i:else)       { return ELSE; }
(?i:fi)         { return FI; }
(?i:in)         { return IN; }
(?i:inherits)   { return INHERITS; }
(?i:let)        { return LET; }
(?i:loop)       { return LOOP; }
(?i:pool)       { return POOL; }
(?i:then)       { return THEN; }
(?i:while)      { return WHILE; }
(?i:case)       { return CASE; }
(?i:esac)       { return ESAC; }
(?i:of)         { return OF; }
(?i:new)        { return NEW; }
(?i:isvoid)     { return ISVOID; }
(?i:not)	    { return NOT; }
t(?i:rue)       { cool_yylval.boolean = 1; return BOOL_CONST; }
f(?i:alse)      { cool_yylval.boolean = 0; return BOOL_CONST; }
\n				{ curr_lineno++; }


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\"              { string_buf_ptr = string_buf; BEGIN(str); }
<str>{
    \"          { /* saw closing quote - all done */
        *string_buf_ptr = '\0';
        cool_yylval.symbol = stringtable.add_string(string_buf);
        BEGIN(INITIAL);
        return STR_CONST;
    }

    \\n         { APPEND_CHR('\n'); }
    \\t         { APPEND_CHR('\t'); }
    \\b         { APPEND_CHR('\b'); }
    \\f         { APPEND_CHR('\f'); }
    \\\0        {
        cool_yylval.error_msg = "String contains escaped null character.";
        BEGIN(endstr);
    }
    \\[^ntbf\0] { APPEND_CHR(yytext[1]); }
    \0          {
        cool_yylval.error_msg = "String contains null character";
        BEGIN(endstr);
    }
    [^\\\n\"\0]+  {
        char *yptr = yytext;

    	while ( *yptr )
        	APPEND_CHR(*yptr++);
	}
    <str>\n		{
	    curr_lineno++;
	    cool_yylval.error_msg = "Unterminated string constant";
	    BEGIN(INITIAL);
	    return ERROR;
    }
    \\\n { ; }

    <<EOF>>		    {
        cool_yylval.error_msg = "EOF in string constant";
        BEGIN(INITIAL);
        return ERROR;
    }
}

<endstr>[^\n\"]*\"?    {
    BEGIN(INITIAL);
    return ERROR;
}

{DIGIT}+          { cool_yylval.symbol = inttable.add_string(yytext); return INT_CONST; }
[[:alpha:]][[:alnum:]_]* {
	cool_yylval.symbol = idtable.add_string(yytext);
	return isupper(yytext[0]) ? TYPEID : OBJECTID;
}

[+/~*<=.@;{}:(),-] { return yytext[0]; }

[^[:space:]] { cool_yylval.error_msg = yytext; return ERROR; }

"*)"         { cool_yylval.error_msg = "Unmatched *)"; return ERROR; }

. { ; }

%%

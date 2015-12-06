%{
#include <stdio.h>
#include "y.tab.h"
%}

DIGIT		[0-9]
NUMBER		{DIGIT}+
HEX_DIGIT	[0-9a-fA-F]
LETTER		[a-zA-Z]
SPECIAL_IN  [!\$%\^&*/:<=>?_~]
SPECIAL_SUB [+-\.@]
TRUE		(TRUE|True|true|#t)
FALSE		(FALSE|False|false|#f|NIL|nil)
BOOLEAN		({TRUE}|{FALSE})
WHITESPACE	[ \t\n]
STRELEM		([^\^"[:print:]]|\"|\\)
UNICODE		u{HEX_DIGIT}{4}
HEX			x{HEX_DIGIT}{2}
CHARPREFIX	#\\
CHAR		([:print:]|{UNICODE}|{HEX})
CHAR_NAME	(space|newline)
CHARACTER	{CHARPREFIX}({CHAR}|{CHAR_NAME})
VECTORSTART #\(
DELIMITER	({WHITESPACE}|(|)|"|;)
COMMENT		;[:print:]$
ATMOSPHERE	({WHITESPACE}|{COMMENT})

INITIAL		({LETTER}|{SPECIAL_IN})
SUBSEQUENT	({INITIAL}|{DIGIT}|{SPECIAL_SUB})
IDENTIFIER	{INITIAL}{SUBSEQUENT}*
STRING		"{STRELEM}*"
TOKEN		({IDENTIFIER}|{BOOLEAN}|{NUMBER}|{CHARACTER}|{STRING}|[()'\.]|{VECTORSTART})
IT_SPACE	{ATMOSPHERE}*

QUOTE		quote
LAMBDA		lambda
IF			if
SETBANG		set!
BEGIN_		begin
COND		cond
AND			and
OR			or
CASE		case
LET			let
LETSTAR		let*
LETREC		letrec
DO			do
DELAY		delay
QUASIQUOTE	quasiquote
EXP_KEYS	({QUOTE}|{LAMBDA}|{IF}|{SETBANG}|{BEGIN_}|{COND}|{AND}|{OR}|{CASE}|{LET}|{LETSTART}|{LETREC}|{DO}|{DELAY}|{QUASIQUOTE})

ELSE		else
EQUALRIGHT	=>
DEFINE		define
UNQUOTE		unquote
COMMAT		,@
UNQUOTE_SPLICING	unquote-splicing
SYN_KEYS	({EXP_KEYS}|{ELSE}|{EQUALRIGHT}|{DEFINE}|{UNQUOTE}|{UNQUOTE_SPLICING})

VARIABLE	[^({SYN_KEYS})({IDENTIFIER})]

%%
{VECTORSTART}	return VECTORSTART;
\(				printf("lex OPEN\n"); return OPEN;
\)				printf("lex CLOSE\n"); return CLOSE;
\.				printf("lex DOT\n"); return DOT;
{TRUE}			return TRUE;
{FALSE}			return FALSE;
{STRING}		return STRING;
{CHARACTER}		return CHARACTER;

{IT_SPACE}		/* empty */;
{NUMBER}		yylval=atoi(yytext); return NUMBER;
'				return SINGLEQUOTE;
{COMMAT}		return COMMAT;

{QUOTE}			return QUOTE;
{LAMBDA}		return LAMBDA;
{IF}			return IF;
{SETBANG}		return SETBANG;
{BEGIN_}		return BEGIN_;
{COND}			return COND;
{AND}			return AND;
{OR}			return OR;
{CASE}			return CASE;
{LET}			return LET;
{LETSTAR}		return LETSTAR;
{LETREC}		return LETREC;
{DO}			return DO;
{DELAY}			return DELAY;
{QUASIQUOTE}	return QUASIQUOTE;
{ELSE}			return ELSE;
{EQUALRIGHT}	return EQUALRIGHT;
{DEFINE}		printf("lex DEFINE\n"); return DEFINE;
{UNQUOTE}		return UNQUOTE;
{UNQUOTE_SPLICING}	return UNQUOTE_SPLICING;

let-syntax		return LET_SYNTAX;
letrec-syntax	return LETREC_SYNTAX;
syntax-rules	return SYNTAX_RULES;
define-syntax	return DEFINE_SYNTAX;

\.\.\.				return ELLIPSIS;

{VARIABLE}		return VARIABLE;
{IDENTIFIER}	return IDENTIFIER;

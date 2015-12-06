%{
#include <stdio.h>
#include "y.tab.h"
%}

TRUE		(TRUE|True|true|#t)
FALSE		(FALSE|False|false|#f)
DIGIT		[0-9]
HEX_DIGIT	[0-9a-fA-F]
LETTER		[a-zA-Z]
SPECIAL_IN  [!\$%\^&*/:<=>?_~]
SPECIAL_SUB [+-\.@]
WHITESPACE	[ \t\n]
STRELEM		[^\^"[:print:]]
UNICODE		u{HEX_DIGIT}{4}
HEX			x{HEX_DIGIT}{2}
CHAR		([:print:]|{UNICODE}|{HEX})


%%
{DIGIT}			return DIGIT;
{LETTER}		return LETTER;
'				return SINGLEQUOTE;
#\\				return CHARPREFIX;
#\(				return VECTORSTART;
{SPECIAL_IN}	return SPECIAL_IN;
{SPECIAL_SUB}	return SPECIAL_SUB;
{TRUE}			return TRUE;
{FALSE}			return FALSE;
\"				return ESCDBLQUOTE;
\\				return ESCESC;
{STRELEM}		return STRELEM;
{CHAR}			return CHAR;
quote			return QUOTE;
lambda			return LAMBDA;
if				return IF;
set!			return SETBANG;
begin			return BEGIN;
cond			return COND;
and				return AND;
or				return OR;
case			return CASE;
let				return LET;
let*			return LETSTAR;
letrec			return LETREC;
do				return DO;
delay			return DELAY;
quasiquote		return QUASIQUOTE;
else			return ELSE;
=>				return EQUALRIGHT;
define			return DEFINE;
unquote			return UNQUOTE;
unquote-splicing	return UNQUOTE_SPLICING;

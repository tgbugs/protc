%{
#include <stdio.h>
#include "y.tab.h"
%}

DIGIT		[0-9]
HEX_DIGIT	[0-9a-fA-F]
LETTER		[a-zA-Z]
SPECIAL_IN  [!\$%\^&*/:<=>?_~]
SPECIAL_SUB [+-\.@]
TRUE		(TRUE|True|true|#t)
FALSE		(FALSE|False|false|#f)
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
TOKEN		({IDENTIFIER}|{BOOLEAN}|{NUMBER}|{CHARACTER}|{STRING}|[()'\.]|{VECTORSTART})
IT_SPACE	{ATMOSPHERE}*

QUOTE		quote
LAMBDA		lambda
IF			if
SETBANG		set!
BEGIN		begin
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
EXP_KEYS	({QUOTE}|{LAMBDA}|{IF}|{SETBANG}|{BEGIN}|{COND}|{AND}|{OR}|{CASE}|{LET}|{LETSTART}|{LETREC}|{DO}|{DELAY}|{QUASIQUOTE})

ELSE		else
EQUALRIGHT	=>
DEFINE		define
UNQUOTE		unquote
UNQUOTE_SPLICING	unquote-splicing
SYN_KEYS	({EXP_KEYS}|{ELSE}|{EQUALRIGHT}|{DEFINE}|{UNQUOTE}|{UNQUOTE_SPLICING})

VARIABLE	[^({SYN_KEYS})({IDENTIFIER})]

%%
{IT_SPACE}		return /* empty */;
{DIGIT}			return DIGIT;
{LETTER}		return LETTER;
'				return SINGLEQUOTE;
{CHARPREFIX}	return CHARPREFIX;
{VECTORSTART}	return VECTORSTART;
{SPECIAL_IN}	return SPECIAL_IN;
{SPECIAL_SUB}	return SPECIAL_SUB;
{TRUE}			return TRUE;
{FALSE}			return FALSE;
{STRELEM}		return STRELEM;
{CHAR}			return CHAR;

{QUOTE}			return QUOTE;
{LAMBDA}		return LAMBDA;
{IF}			return IF;
{SETBANG}		return SETBANG;
{BEGIN}			return BEGIN;
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
{DEFINE}		return DEFINE;
{UNQUOTE}		return UNQUOTE;
{UNQUOTE_SPLICING}	return UNQUOTE_SPLICING;

let-syntax		return LET_SYNTAX;
letrec-syntax	return LETREC_SYNTAX;
syntax-rules	return SYNTAX_RULES;
define-syntax	return DEFINE_SYNTAX;

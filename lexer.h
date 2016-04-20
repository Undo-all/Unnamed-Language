#pragma once

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEX_SUCCESSFUL(l) ((l).type==0)

struct lexerr {
    int type;
    int loc;
};

#define TOKEN_HACK \
    t(NEWLINE), \
    t(INDENT), \
    t(DEDENT), \
    t(LPAREN), \
    t(RPAREN), \
    t(IDENT), \
    t(INT), \
    t(REAL), \
    t(END)

#define t(x) x##_TOK
enum token_type {
    TOKEN_HACK
};

struct token {
    enum token_type type;
    union {
        char* ident;
        int intv;
        float real;
    };
};

struct lexerr lex(char*, struct token**);
void display_token(struct token token);
void display_lexerr(char*, struct lexerr);


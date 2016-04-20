#include "lang.h"
#include "lexer.h"
#include "parse.h"

int main(void) {
    struct token* toks;
    struct lexerr lexerr;
    char* str = "f x y\n    g x y";
    lexerr = lex(str, &toks);

    if (!LEX_SUCCESSFUL(lexerr)) {
        display_lexerr(str, lexerr);
        exit(1);
    }

    struct expr* exprs;
    int len;
    parse(toks, &exprs, &len);

    return 0;
}


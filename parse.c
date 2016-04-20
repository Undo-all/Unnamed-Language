#include "parse.h"

enum {
    PARSE_SUCCESS,
    PARSE_FINISHED,
    PARSE_EXPECTED,
    PARSE_MISMATCHED_PARENS,
};

#define SUCCESS ((struct parse_err) { .type = PARSE_SUCCESS })
#define FINISHED ((struct parse_err) { .type = PARSE_FINISHED })
#define EXPECTED(t) ((struct parse_err) { .type = PARSE_EXPECTED, .expected = t })
#define MISMATCHED_PARENS \
    ((struct parse_err) { .type = PARSE_MISMATCHED_PARENS })

struct state {
    int loc;
    int indent;
    int parens;
};

int accept(struct token tok, struct token* toks, struct state* state) {
    if (toks[state->loc].type == tok.type) {
        ++state->loc;
        return 1;
    }

    return 0;
}

struct parse_err expect(struct token tok, struct token* toks, struct state* state) {
    if (accept(tok, toks, state))
        return SUCCESS;
    return EXPECTED(tok); 
}

struct parse_err parse_arg(struct token* toks, struct state* state, struct expr* ret);

struct parse_err parse_expr(struct token* toks, struct state* state, struct expr* ret) {
    if (toks[state->loc].type == DEDENT_TOK) {
        --state->indent;
        ++state->loc;
        return SUCCESS;
    }

    if (toks[state->loc].type == INDENT_TOK) {
        printf("unexpected indent\n");
        printf("tokens and location:\n");
        for (int i = 0; toks[i].type != END_TOK; ++i) {
            if (i == state->loc)
                printf(">");
            display_token(toks[i]);
            printf(" ");
        }

        printf("\n");
        exit(1);
        return SUCCESS;
    }

    if (toks[state->loc].type == NEWLINE_TOK) {
        ++state->loc;
        return SUCCESS;
    }
    
    struct parse_err err;
    ret->apply.list = malloc(sizeof(struct expr));
    ret->apply.len = 0;
    while ( toks[state->loc].type != NEWLINE_TOK && 
            toks[state->loc].type != END_TOK     && 
            toks[state->loc].type != INDENT_TOK  && 
            toks[state->loc].type != RPAREN_TOK  ){
        ret->apply.list = realloc(ret->apply.list, sizeof(struct expr) * ++ret->apply.len);
        if (!PARSE_SUCCESSFUL(err = parse_arg(toks, state, &ret->apply.list[ret->apply.len-1])))
            return err;
        //++state->loc;
    }

    if (toks[state->loc].type == INDENT_TOK) {
        // It's a definition now.
        ret->type = DEFINE;
        if (toks[0].type != IDENT_TOK)
            printf("SHIT");
        ret->define.name = ret->apply.list[0].ident;
        ret->define.num_args = ret->apply.len;
        ret->define.args = malloc(sizeof(char*) * ret->apply.len);

        for (int i = 1; i < ret->apply.len; ++i) {
            if (ret->apply.list[i].type != IDENT)
                printf("SHIT");

            strcpy(ret->define.args[i-1], ret->apply.list[i].ident);
        }

        free(ret->apply.list);

        struct parse_err err;
        ret->define.body = malloc(sizeof(struct expr));
        ret->define.body_len = 0;
        int indent = state->indent;
        ++state->indent;
        ++state->loc;

        while (state->indent != indent && toks[state->loc].type != END_TOK) {
            ret->define.body = realloc(ret->define.body, sizeof(struct expr) * ++ret->define.body_len);
            err = parse_expr(toks, state, &ret->define.body[ret->define.body_len-1]);
            if (!PARSE_SUCCESSFUL(err))
                return err;
        }

        return SUCCESS;
    } else {
        ret->type = APPLY;
        return SUCCESS;    
    }
}

struct parse_err parse_arg(struct token* toks, struct state* state, struct expr* ret) {
    if (toks[state->loc].type == END_TOK     ||
        toks[state->loc].type == NEWLINE_TOK ||
        toks[state->loc].type == DEDENT_TOK) {
        return SUCCESS;
    }

    if (toks[state->loc].type == LPAREN_TOK) {
        ++state->parens;
        ++state->loc;
        struct parse_err err;
        printf("Parsing subexpression!\n");
        if (!PARSE_SUCCESSFUL(err = parse_expr(toks, state, ret)))
            return err;
        printf("Exiting parse of subexpression...\n");

        if (toks[state->loc].type != RPAREN_TOK) {
            return MISMATCHED_PARENS;
        } else {
            --state->parens;
            ++state->loc;
            return SUCCESS;
        }
    } else if (toks[state->loc].type == RPAREN_TOK) {
        if (state->parens == 0) {
            printf("unexpected )");
            exit(1);
        } else {
            return SUCCESS;
        }
    } else if (toks[state->loc].type == INT_TOK) {
        ret->type = LIT_INT;
        ret->lint = toks[state->loc].intv;
        ++state->loc;
        return SUCCESS;
    } else if (toks[state->loc].type == REAL_TOK) {
        ret->type = LIT_REAL;
        ret->lreal = toks[state->loc].real;
        ++state->loc;
        return SUCCESS;
    } else if (toks[state->loc].type == IDENT_TOK) {
        ret->type = IDENT;
        ret->ident = toks[state->loc].ident;
        ++state->loc;
        return SUCCESS;
    } 

    return SUCCESS;
}

struct parse_err parse(struct token* toks, struct expr** exprs, int* len) {
    struct state state = { .loc = 0, .indent = 0, .parens = 0 };
    struct parse_err err;
    *exprs = malloc(sizeof(struct expr));
    *len = 0;

    while (toks[state.loc].type != END_TOK) {
        *exprs = realloc(*exprs, sizeof(struct expr) * ++*len);
        if (!PARSE_SUCCESSFUL(err = parse_expr(toks, &state, &(*exprs)[*len-1])))
            return err;
    }

    return SUCCESS;
}


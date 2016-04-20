#include "lexer.h"

enum lexerr_type {
    LEX_SUCCEEDED,
    UNEXPECTED_CHAR,
    INCORRECT_INDENT,
};

#define SUCCESS ((struct lexerr) { .type = LEX_SUCCEEDED })
#define MK_TOK(t) ((struct token) { .type = t })
#define MK_LEXERR(e,s) ((struct lexerr) { .type = e, .loc = (s).loc })

struct state {
    int loc;
    struct indent_list* indents;
};

struct indent_list {
    int indent;
    struct indent_list* next;
};

void indent(struct state* state, int amount) {
    struct indent_list* new = malloc(sizeof(struct indent_list));
    new->indent = amount;
    new->next = state->indents;
    state->indents = new;
}

void dedent(struct state* state) {
    struct indent_list* new = state->indents->next;
    free(state->indents);
    state->indents = new;
}

int get_indent(struct state* state) {
    if (state->indents == NULL)
        return 0;
    else
        return state->indents->indent;
}

int get_last_indent(struct state* state) {
    if (state->indents == NULL)
        return -1;
    else if (state->indents->next == NULL)
        return 0;
    else
        return state->indents->next->indent;
}

struct lexerr lex_ident(char* str, struct state* state, struct token* ret) {
    int i = state->loc;
    for (; isalpha(str[i]) || isdigit(str[i]) || str[i] == '_'; ++i);

    int len = i - state->loc;
    char* ident = malloc(sizeof(char) * (len+1));
    memcpy(ident, str+state->loc, len);
    ident[len] = '\0';

    state->loc = i;
    ret->type = IDENT_TOK;
    ret->ident = ident;
    return SUCCESS;
}

struct lexerr lex_number(char* str, struct state* state, struct token* ret) {
    int i;
    int iacc = 0;

    for (i = state->loc; isdigit(str[i]); ++i) {
        iacc = iacc * 10 + (str[i] - '0');
    }

    if (str[i] == '.') {
        double racc = 0;
        double exp = 0.1;

        while (isdigit(str[i])) {
            racc += exp * (str[i]);
            exp /= 10;
        }
        
        state->loc = i;
        ret->type = REAL_TOK;
        ret->real = racc;
        return SUCCESS;
    } else {
        state->loc = i;
        ret->type = INT_TOK;
        ret->intv = iacc;
        return SUCCESS;
    }
}

struct lexerr lex_token(char* str, struct state* state, struct token* ret) {
    if (str[state->loc] == '\0') {
        ret->type = END_TOK;
        return SUCCESS;
    } else if (str[state->loc] == '(') {
        ++state->loc;
        ret->type = LPAREN_TOK;
        return SUCCESS;
    } else if (str[state->loc] == ')') {
        ++state->loc;
        ret->type = RPAREN_TOK;
        return SUCCESS;
    } else if (isalpha(str[state->loc]) || str[state->loc] == '_') {
        return lex_ident(str, state, ret);
    } else if (isdigit(str[state->loc])) {
        return lex_number(str, state, ret);
    } else {
        return MK_LEXERR(UNEXPECTED_CHAR, *state);
    }
}

struct lexerr lex(char* str, struct token** ret) {
    struct state state = { .loc = 0, .indents = NULL };
    struct token tok;
    struct token* toks = malloc(sizeof(struct token));
    struct lexerr err;
    int num_toks = 0;

    for (;;) {
        if (str[state.loc] == '\n') {
            int new_indent = 0;
            int i;
            for (i = state.loc + 1; str[i] == ' '; ++i, ++new_indent);
            state.loc = i;
            
            int last_indent = get_last_indent(&state);
            if (last_indent != -1 && new_indent <= last_indent) {
                int num_dedents = 0;
                struct indent_list* iter = state.indents;
                for (; iter != NULL && iter->indent != new_indent; iter = iter->next, ++num_dedents);

                if (iter != NULL || (iter == NULL && new_indent == 0)) {
                    for (int i = 0; i < num_dedents; ++i) {
                        dedent(&state);
                        toks = realloc(toks, sizeof(struct token) * ++num_toks);
                        toks[num_toks-1].type = DEDENT_TOK;
                    } 
                } else {
                    return MK_LEXERR(INCORRECT_INDENT, state);
                }
            } else if (new_indent > get_indent(&state)) {
                indent(&state, new_indent);
                toks = realloc(toks, sizeof(struct token) * ++num_toks);
                toks[num_toks-1].type = INDENT_TOK; 
            } else if (new_indent == get_indent(&state)) {
                toks = realloc(toks, sizeof(struct token) * ++num_toks);
                toks[num_toks-1].type = NEWLINE_TOK;
            } else {
                printf("This shouldn't happen...\n");
            }
        }

        while (isspace(str[state.loc])) ++state.loc;

        err = lex_token(str, &state, &tok);
        if (!LEX_SUCCESSFUL(err)) {
            return err;
        } else {
            toks = realloc(toks, sizeof(struct token) * ++num_toks);
            toks[num_toks-1] = tok;

            if (tok.type == END_TOK) {
                *ret = toks;
                return SUCCESS;
            }
        }
    }
}

#undef t
#define t(x) #x
char* token2str_arr[] = {
    TOKEN_HACK
};

void display_token(struct token token) {
    printf("%s", token2str_arr[token.type]);
    
    switch (token.type) {
        case IDENT_TOK:
            printf(" %s", token.ident);
            break;
        case INT_TOK: 
            printf(" %d", token.intv);
            break;
        case REAL_TOK:
            printf(" %f", token.real);
            break;
        default: break;
    }
}

void display_lexerr(char* str, struct lexerr err) {
    switch (err.type) {
        case LEX_SUCCEEDED:
            break;
        case UNEXPECTED_CHAR:
            fprintf(stderr, "unexpected char %c at %d\n", str[err.loc], err.loc);
            break;
        case INCORRECT_INDENT:
            fprintf(stderr, "incorrect indent at %d\n", err.loc);
            break;
    }
}


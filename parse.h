#pragma once

#include "lang.h"
#include "lexer.h"

#include <string.h>

// 0 is always success in this world
#define PARSE_SUCCESSFUL(e) ((e).type==0)

struct parse_err {
    int type;
    struct token expected;
};

struct parse_err parse(struct token*, struct expr**, int*);


#pragma once

#include <stdio.h>
#include <stdint.h>

/* Dynamically-typed values */

enum {
    INT,
    REAL,
    FUNCTION,
};

#define TYPE_WIDTH 1
#define TYPE_MASK ((1<<TYPE_WIDTH)-1)

typedef uint64_t value;

static inline uint64_t f2ui(float f) {
    return *(uint64_t*) &f;
}

static inline float ui2f(uint64_t ui) {
    return *(float*) &ui;
}

#define VALUE(t,n) (((n)<<TYPE_WIDTH)|(t))
#define INTVAL(n) VALUE((uint64_t)n,INT)
#define REALVAL(x) VALUE(f2ui(x), REAL)

#define GETTYPE(v) ((v)&TYPE_MASK)

#define GETRAW(v) ((v)>>TYPE_WIDTH)
#define GETINT(v) ((uint32_t)GETRAW(v))
#define GETREAL(v) (ui2f(GETRAW(v)))

/* Expressions */

enum {
    LIT_INT,
    LIT_REAL,
    IDENT,
    APPLY,
    DEFINE,
};

struct expr {
    uint8_t type;
    union {
        int lint;
        float lreal;
        char* ident;

        struct {
            // First element is function, rest are arguments.
            struct expr* list;
            int len;
        } apply;

        struct {
            char* name;
            char** args;
            int num_args;
            struct expr* body;
            int body_len;
        } define;
    };
};


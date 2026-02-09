module mini_caml_fsharp.GenC

module GenC =
    let private std_prelude =
        @"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef enum min_caml_unit { UNIT } u_t;
typedef struct min_caml_value {
    union {
        u_t u;
        int64_t i;
        double f;
        struct min_caml_value * v;
    };
    int64_t extra;
} v_t;
#define min_caml_make_unit() { .extra = 0, .u = UNIT }
#define min_caml_make_int(__value) { .extra = 0, .i = (__value) }
#define min_caml_make_float(__value) { .extra = 0, .f = (__value) }
v_t min_caml_alloc_vector(int64_t count) {
    if (count <= 0) {
        printf(""can't allocate vector with size <= 0"");
        exit(-1);
    }

    v_t* res_v = (v_t*) malloc(sizeof(v_t) * (size_t) count);
    if (!res_v) {
        printf(""failed to allocate vector, die in panic, see ya!\n"");
        exit(-1);
    }

    v_t v = min_caml_make_unit();
    v_t res = { .extra = count, .v = res_v };
    for(int64_t i = 0; i < count; i++) {
        res.v[i] = v;
    }
    return res;
}
v_t min_caml_deep_copy(v_t v) {
    if (!v.extra)
        return v;

    v_t res = min_caml_alloc_vector(v.extra);
    for(int64_t i = 0; i < v.extra; i++) {
        res.v[i] = min_caml_deep_copy(v.v[i]);
    }
    return res;
}
void min_caml_free(v_t v) {
    if (!v.extra)
        return;

    for(int64_t i = 0; i < v.extra; i++) {
        min_caml_free(v.v[i]);
    }

    free(v.v);
}"

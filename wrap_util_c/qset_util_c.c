
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "set.h"

#ifdef __cplusplus
extern "C" {
#endif

SetIterator* qu_set_new_iterator_c() {
    SetIterator* iter = malloc(sizeof(SetIterator));
    return iter;
}

int qu_set_size_c(Set *set) {
    return (int) set_num_entries(set);
}

void qu_set_toarray_c(Set *set, void* val_array, size_t size_data) {
	unsigned char *dp = val_array;
    SetValue* arr_p = set_to_array(set);
    unsigned int ssize = set_num_entries(set);
	unsigned int i;

    for (i=0; i<ssize; ++i) {
        memcpy(dp, arr_p[i], size_data);
        dp += size_data;
	}
	free(arr_p);
}

void qu_set_toarray_str_c(Set *set, void* str_array, size_t size_data) {
    unsigned char *dp = str_array;
    SetValue* arr_p = set_to_array(set);
    unsigned int ssize = set_num_entries(set);
	unsigned int i;
	size_t slen;

    for (i=0; i<ssize; ++i) {
        slen = strlen((char*) arr_p[i]);
        memcpy(dp, arr_p[i], slen);
        dp += size_data;
	}
	free(arr_p);
}

void qu_set_copy_c(Set *to, Set *from, size_t val_size, bool is_str) {
    if (from == NULL) {
        return;
    }

    SetIterator iterator;
    if (is_str) {
        char *str_p;
        char *str_copy;
        set_iterate(from, &iterator);
        while (set_iter_has_more(&iterator)) {
            str_p = (char *) set_iter_next(&iterator);
            str_copy = (char *) malloc((strlen(str_p)+1)*sizeof(char));
            strcpy(str_copy, str_p);

            set_insert(to, str_copy);
        }
    }
    else {
        void *val_p;
        void *val_copy;
        set_iterate(from, &iterator);
        while (set_iter_has_more(&iterator)) {
            val_p = set_iter_next(&iterator);
            val_copy = malloc(val_size);
            memcpy(val_copy, val_p, val_size);

            set_insert(to, val_copy);
        }
    }
}

#ifdef __cplusplus
}
#endif

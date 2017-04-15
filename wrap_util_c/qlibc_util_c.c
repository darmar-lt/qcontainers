

#ifndef QLIBC_UTIL_H
#define QLIBC_UTIL_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

void qlibc_copy_data_c(void *val_data_from, void *val_data_to, size_t size_data, bool freemem) {
    memcpy(val_data_to, val_data_from, size_data);
    if (freemem) {
        free(val_data_from);
    }
}

void qlibc_copy_data_new_c(void* val_from, void** val_to_p, size_t val_size) {
    *val_to_p = malloc(val_size);
    memcpy(*val_to_p, val_from, val_size);
}

void qlibc_copy_string_new_c(char str_from[], char* str_to_p[], size_t str_f_len) {
    *str_to_p = malloc(sizeof(char)*(str_f_len+1));
    memcpy(*str_to_p, str_from, sizeof(char)*str_f_len);
    (*str_to_p)[str_f_len] = '\0';
}

size_t qlibc_cstr_len_c(char str[]) {
     return strlen(str);
}

void qlibc_free_c(void *obj) {
    free(obj);
}


#ifdef __cplusplus
}
#endif

#endif /* QLIBC_UTIL_H */

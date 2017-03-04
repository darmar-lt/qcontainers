

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

void qlibc_free_c(void *obj) {
    free(obj);
}


#ifdef __cplusplus
}
#endif

#endif /* QLIBC_UTIL_H */

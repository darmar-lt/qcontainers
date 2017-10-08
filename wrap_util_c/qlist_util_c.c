
#ifndef QLIST_UTIL_H
#define QLIST_UTIL_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "qlist.h"

#ifdef __cplusplus
extern "C" {
#endif

void qlist_copy_c(qlist_t **to, qlist_t *from) {
    if ((*to) != NULL) {
        qlist_free(*to);
    }
    if (from == NULL) {
        *to = NULL;
        return;
    }

    *to = qlist(0);
    qlist_setsize(*to, from->max);
    qlist_obj_t obj;
    memset((void*)&obj, 0, sizeof(obj));
    while(qlist_getnext(from, &obj, false) == true) {
        qlist_addat(*to, -1, obj.data, obj.size);
    }
}

bool qlist_debug_c(qlist_t *list) {
    if (list->qmutex == NULL) {
        fprintf(stdout, "Qlist is NOT Threadsafe");
    }
    else
        fprintf(stdout, "Qlist is Threadsafe");

    return qlist_debug(list, stdout);
}

qlist_obj_t* qlist_getobj_c() {
    qlist_obj_t *obj = (qlist_obj_t *) calloc(1, sizeof(qlist_obj_t));
    if (obj == NULL) {
        return NULL;
    }
    memset((void *) obj, 0, sizeof(qlist_obj_t));
    return obj;
}

void qlist_objinit_c(qlist_obj_t *obj) {
    memset((void *) obj, 0, sizeof(qlist_obj_t));;
}

void qlist_objfree_c(qlist_obj_t *obj) {
    free(obj);
}


void qlist_getdata_c(qlist_obj_t *obj, void *data) {
    if (obj == NULL) {
        return;
    }
    memcpy(data, obj->data, obj->size);
}

void qlist_getat_c(qlist_t *list, int index, void *val_data, bool *suc) {
    size_t size;
    void *data = qlist_getat(list, index, &size, false);
    if (data == NULL) {
        suc = false;
    }
    else {
        *suc = true;
        memcpy(val_data, data, size);
    }
}

void qlist_popat_c(qlist_t *list, int index, void *val_data, bool *suc) {
    size_t size;
    void *data = qlist_popat(list, index, &size);
    if (data != NULL) {
        memcpy(val_data, data, size);
        free(data);
        *suc = true;
    }
    else {
        *suc = false;
    }
}

void qlist_toarray_c(qlist_t *list, void *val_array) {
    unsigned char *dp = val_array;

    qlist_obj_t *obj;
    for (obj = list->first; obj; obj = obj->next) {
        memcpy(dp, obj->data, obj->size);
        dp += obj->size;
    }
}


#ifdef __cplusplus
}
#endif

#endif /* QLIST_UTIL_H */

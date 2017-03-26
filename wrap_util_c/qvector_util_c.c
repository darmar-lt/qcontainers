

#ifndef QVECTOR_UTIL_H
#define QVECTOR_UTIL_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include "containers/qvector.h"

#ifdef __cplusplus
extern "C" {
#endif

void qvector_copy_c(qvector_t **to, qvector_t *from) {
    if ((*to) != NULL) {
        qvector_free(*to);
    }
    if (from == NULL) {
        *to = NULL;
        return;
    }

    *to = qvector(from->max, from->objsize, from->options);
    if ((*to)->options & QVECTOR_RESIZE_LINEAR) {
        (*to)->initnum = from->initnum;
    }
    memcpy((*to)->data, from->data, from->num * from->objsize);
    (*to)->num = from->num;
}

void qvector_toarray_c(qvector_t *vector, void *array) {
    if (vector->num <= 0) {
        return;
    }

    /* vector->lock(vector); */

    memcpy(array, vector->data, vector->num * vector->objsize);

    /* vector->unlock(vector); */
    return;
}

qvector_obj_t* qvector_getobj_c() {
    qvector_obj_t *obj = (qvector_obj_t *) calloc(1, sizeof(qvector_obj_t));
    if (obj == NULL) {
        return NULL;
    }
    /* memset((void *) obj, 0, sizeof(obj)); */
    return obj;
}

void qvector_objinit_c(qvector_obj_t *obj) {
    memset((void *) obj, 0, sizeof(obj));;
}

void qvector_getdata_c(qvector_obj_t *obj, void *data, size_t objsize) {
    memcpy(data, obj->data, objsize);
}



#ifdef __cplusplus
}
#endif

#endif /* QVECTOR_UTIL_H */

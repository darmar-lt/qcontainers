
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "containers/qhashtbl.h"


#ifdef __cplusplus
extern "C" {
#endif

void qhashtbl_copy_c(qhashtbl_t **to, qhashtbl_t *from) {
    if ((*to) != NULL) {
        qhashtbl_free(*to);
    }
    if (from == NULL) {
        *to = NULL;
        return;
    }

    *to = qhashtbl(from->range, 0);
    qhashtbl_obj_t obj;
    memset((void*)&obj, 0, sizeof(obj));
    while(qhashtbl_getnext(from, &obj, false) == true) {
        qhashtbl_put(*to, obj.name, obj.data, obj.size);
    }
}

qhashtbl_obj_t* qhashtbl_getobj_c() {
    qhashtbl_obj_t *obj = (qhashtbl_obj_t *) calloc(1, sizeof(qhashtbl_obj_t));
    if (obj == NULL) {
        return NULL;
    }
    return obj;
}

void qhashtbl_objinit_c(qhashtbl_obj_t *obj) {
    memset((void *) obj, 0, sizeof(obj));;
}

void qhashtbl_getname_c(qhashtbl_obj_t *obj, void *name) {
    memcpy(name, obj->name, strlen(obj->name));
}

size_t qhashtbl_getnamesize_c(qhashtbl_obj_t *obj) {
    return strlen(obj->name);
}

void qhashtbl_getdata_c(qhashtbl_obj_t *obj, void *data) {
    memcpy(data, obj->data, obj->size);
}

#ifdef __cplusplus
}
#endif


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "containers/qtreetbl.h"


#ifdef __cplusplus
extern "C" {
#endif

void qtreetbl_copy_c(qtreetbl_t **to, qtreetbl_t *from) {
    if ((*to) != NULL) {
        qtreetbl_free(*to);
    }
    if (from == NULL) {
        *to = NULL;
        return;
    }

    *to = qtreetbl(0);
    qtreetbl_obj_t obj;
    memset((void*)&obj, 0, sizeof(obj));
    while(qtreetbl_getnext(from, &obj, false) == true) {
        qtreetbl_put_by_obj(*to, obj.name, obj.namesize, obj.data, obj.datasize);
    }
}

qtreetbl_obj_t* qtreetbl_getobj_c() {
    qtreetbl_obj_t *obj = (qtreetbl_obj_t *) calloc(1, sizeof(qtreetbl_obj_t));
    if (obj == NULL) {
        return NULL;
    }
    /* memset((void *) obj, 0, sizeof(obj)); */
    return obj;
}

void qtreetbl_objinit_c(qtreetbl_obj_t *obj) {
    memset((void *) obj, 0, sizeof(qtreetbl_obj_t));;
}

void qtreetbl_getname_c(qtreetbl_obj_t *obj, void *name) {
    memcpy(name, obj->name, obj->namesize);
}

size_t qtreetbl_getnamesize_c(qtreetbl_obj_t *obj) {
    return obj->namesize;
}

void qtreetbl_getdata_c(qtreetbl_obj_t *obj, void *data) {
    memcpy(data, obj->data, obj->datasize);
}

void qtreetbl_find_nearest_c(qtreetbl_t *tbl, const void *name, size_t namesize, bool newmem, qtreetbl_obj_t *obj, bool *found) {
    *obj = qtreetbl_find_nearest(tbl, name, namesize, newmem);
    if (obj->name == NULL) {
        *found = false;
    }
    else {
        *found = true;
    }
}





#ifdef __cplusplus
}
#endif

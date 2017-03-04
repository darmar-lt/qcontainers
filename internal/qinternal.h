/******************************************************************************
 * qLibc
 *
 * Copyright (c) 2010-2015 Seungyoung Kim.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *****************************************************************************/

#ifndef QINTERNAL_H
#define QINTERNAL_H

/*
 * Macro Functions
 */
#define ASSERT(c)           assert(c)
#define CONST_STRLEN(s)     (sizeof(s) - 1)
#define IS_EMPTY_STR(s)     ((*s == '\0') ? true : false)
#define ENDING_CHAR(s)      (*(s + strlen(s) - 1))

#define DYNAMIC_VSPRINTF(s, f) do {                                     \
        size_t _strsize;                                                \
        for(_strsize = 1024; ; _strsize *= 2) {                         \
            s = (char*)malloc(_strsize);                                \
            if(s == NULL) {                                             \
                DEBUG("DYNAMIC_VSPRINTF(): can't allocate memory.");    \
                break;                                                  \
            }                                                           \
            va_list _arglist;                                           \
            va_start(_arglist, f);                                      \
            int _n = vsnprintf(s, _strsize, f, _arglist);               \
            va_end(_arglist);                                           \
            if(_n >= 0 && _n < _strsize) break;                         \
            free(s);                                                    \
        }                                                               \
    } while(0)

/*
 * Q_MUTEX Macros
 */
#ifndef _MULTI_THREADED
#define _MULTI_THREADED
#endif

#if defined(_MSC_VER)
#if defined(_WIN32) || defined(_WIN64)
#  if defined(_WIN64)
typedef __int64 LONG_PTR;
#  else
typedef long LONG_PTR;
#  endif
typedef LONG_PTR SSIZE_T;
typedef SSIZE_T ssize_t;
#endif

#else
#include <unistd.h>
#endif

#define Q_MUTEX_NEW(m,r) do {                                          \
		m = NULL;                                                      \
	} while(0)

#define Q_MUTEX_LEAVE(m) do {                                          \
    } while(0)

#define MAX_MUTEX_LOCK_WAIT (5000)

#define Q_MUTEX_ENTER(m) do {                                           \
    } while(0)

#define Q_MUTEX_DESTROY(m) do {                                         \
    } while(0)


#include <errno.h>
#ifndef ENOBUFS
#define ENOBUFS 119
#endif


/*
 * Debug Macros
 */
#ifdef BUILD_DEBUG
#define DEBUG(fmt, args...) fprintf(stderr, "[DEBUG] " fmt " (%s:%d)\n", \
                                    ##args, __FILE__, __LINE__);
#else
#ifdef __cplusplus
#define DEBUG(fmt, args...) static_cast<void>(0)
#else
/* #define DEBUG(fmt, args...) (void)(0) */
#define DEBUG(fmt) (void)(0)
#endif
#endif  /* BUILD_DEBUG */

/*
 * Other internal use
 */
#define MAX_HUMANOUT        (60)

/*
 * qInternal.c
 */

extern char _q_x2c(char hex_up, char hex_low);
extern char *_q_makeword(char *str, char stop);
extern void _q_textout(FILE *fp, void *data, size_t size, size_t max);

extern void *qmemdup(const void *data, size_t size);

#endif /* QINTERNAL_H */

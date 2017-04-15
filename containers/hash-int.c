/*

Copyright (c) 2005-2008, Simon Howard

Permission to use, copy, modify, and/or distribute this software
for any purpose with or without fee is hereby granted, provided
that the above copyright notice and this permission notice appear
in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

 */

#include "hash-int.h"
#include <stdint.h>

/* Hash function for a pointer to an integer */

unsigned int int8_hash(void *vlocation)
{
	int8_t *location;

	location = (int8_t *) vlocation;

	return (unsigned int) *location;
}

unsigned int int16_hash(void *vlocation)
{
	int16_t *location;

	location = (int16_t *) vlocation;

	return (unsigned int) *location;
}

unsigned int int32_hash(void *vlocation)
{
	int32_t *location;

	location = (int32_t *) vlocation;

	return (unsigned int) *location;
}

unsigned int int64_hash(void *vlocation)
{
	int64_t *location;

	location = (int64_t *) vlocation;

	return (unsigned int) *location;
}


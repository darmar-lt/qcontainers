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

#include "compare-int.h"
#include <stdint.h>

/* Comparison functions for a pointer to an integer */

int int8_equal(void *vlocation1, void *vlocation2)
{
	int8_t *location1;
	int8_t *location2;

	location1 = (int8_t *) vlocation1;
	location2 = (int8_t *) vlocation2;

	return *location1 == *location2;
}

int int16_equal(void *vlocation1, void *vlocation2)
{
	int16_t *location1;
	int16_t *location2;

	location1 = (int16_t *) vlocation1;
	location2 = (int16_t *) vlocation2;

	return *location1 == *location2;
}

int int32_equal(void *vlocation1, void *vlocation2)
{
	int32_t *location1;
	int32_t *location2;

	location1 = (int32_t *) vlocation1;
	location2 = (int32_t *) vlocation2;

	return *location1 == *location2;
}

int int64_equal(void *vlocation1, void *vlocation2)
{
	int64_t *location1;
	int64_t *location2;

	location1 = (int64_t *) vlocation1;
	location2 = (int64_t *) vlocation2;

	return *location1 == *location2;
}

int int_compare(void *vlocation1, void *vlocation2)
{
	int *location1;
	int *location2;

	location1 = (int *) vlocation1;
	location2 = (int *) vlocation2;

	if (*location1 < *location2) {
		return -1;
	} else if (*location1 > *location2) {
		return 1;
	} else {
		return 0;
	}
}



#include <stdint.h>
#include "langc.h"

#define MAX_UTF8_BYTES 6

static const uint8_t shift_trailing = 6;
static const uint8_t shifts[MAX_UTF8_BYTES] = {
	7,    5,    4,    3,    2,    1
};
static const uint8_t header_trailing = 0x02;
static const uint8_t headers[MAX_UTF8_BYTES] = {
	0x00, 0x06, 0x0E, 0x1E, 0x3E, 0x7E
};
static const uint8_t mask_trailing = 0x3F;
static const uint8_t masks[MAX_UTF8_BYTES] = {
	0x7F, 0x1F, 0x0F, 0x07, 0x03, 0x01
};

int str_to_code_point(uint32_t *code_point, const char *src)
{
	int nbytes, i;
	const uint8_t *s = (uint8_t *) src;

	for (nbytes = 1; nbytes <= MAX_UTF8_BYTES; nbytes++) {
		if (s[0] >> shifts[nbytes - 1] == headers[nbytes - 1]) {
			break;
		}
	}
	if (nbytes > MAX_UTF8_BYTES) {
		goto invalid;
	}
	*code_point = s[0] & masks[0];
	for (i = 1; i < nbytes; i++) {
		if (s[i] >> shift_trailing != header_trailing) {
			goto invalid;
		}
		*code_point <<= shift_trailing;
		*code_point |= s[i] & mask_trailing;
	}
	return nbytes;
invalid:
	return 0;
}

bool is_valid_utf8(const char *s)
{
	uint8_t c;
	int nbytes;

	do {
		nbytes = str_to_code_point(&c, s);
		if (nbytes == 0) {
			return false;
		}
		s += nbytes;
	} while (c != '\0');
	return true;
}

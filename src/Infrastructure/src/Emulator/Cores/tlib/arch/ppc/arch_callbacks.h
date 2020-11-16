#ifndef ARCH_CALLBACKS_H_
#define ARCH_CALLBACKS_H_

#include <stdint.h>

uint32_t tlib_read_tbl(void);
uint32_t tlib_read_tbu(void);
uint64_t tlib_read_decrementer(void);
uint32_t tlib_is_vle_enabled(void);
void tlib_write_decrementer(uint64_t value);

#endif

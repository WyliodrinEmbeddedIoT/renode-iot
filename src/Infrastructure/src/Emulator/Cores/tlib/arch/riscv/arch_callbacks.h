#ifndef ARCH_CALLBACKS_H_
#define ARCH_CALLBACKS_H_

#include <stdint.h>

uint64_t tlib_get_cpu_time();
uint32_t tlib_is_in_debug_mode();

int32_t tlib_has_nonstandard_csr(uint64_t csr);
uint64_t tlib_read_csr(uint64_t csr);
void tlib_write_csr(uint64_t csr, uint64_t value);
void tlib_mip_changed(uint64_t value);

int32_t tlib_handle_custom_instruction(uint64_t id, uint64_t opcode);

#endif

/*
 *  RISC-V interface functions
 *
 *  Copyright (c) Antmicro
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */
#include <stdint.h>
#include "cpu.h"

void tlib_set_hart_id(uint32_t id)
{
    cpu->mhartid = id;
}

uint32_t tlib_get_hart_id()
{
    return cpu->mhartid;
}

void tlib_set_mip_bit(uint32_t position, uint32_t value)
{
    pthread_mutex_lock(&cpu->mip_lock);
    // here we might have a race
    if (value) {
        cpu->mip |= ((target_ulong)1 << position);
    } else {
        cpu->mip &= ~((target_ulong)1 << position);
    }
    pthread_mutex_unlock(&cpu->mip_lock);
}

void tlib_allow_feature(uint32_t feature_bit)
{
    cpu->misa_mask |= (1L << feature_bit);
    cpu->misa |= (1L << feature_bit);

    // availability of F/D extensions
    // is indicated by a bit in MSTATUS
    if (feature_bit == 'F' - 'A' || feature_bit == 'D' - 'A') {
        set_default_mstatus();
    }
}

void tlib_mark_feature_silent(uint32_t feature_bit, uint32_t value)
{
    if (value) {
        cpu->silenced_extensions |= (1L << feature_bit);
    } else {
        cpu->silenced_extensions &= ~(1L << feature_bit);
    }
}

uint32_t tlib_is_feature_enabled(uint32_t feature_bit)
{
    return (cpu->misa & (1L << feature_bit)) != 0;
}

uint32_t tlib_is_feature_allowed(uint32_t feature_bit)
{
    return (cpu->misa_mask & (1L << feature_bit)) != 0;
}

void tlib_set_privilege_architecture(int32_t privilege_architecture)
{
    if (privilege_architecture > RISCV_PRIV1_11) {
        tlib_abort("Invalid privilege architecture set. Highest suppported version is 1.11");
    }
    cpu->privilege_architecture = privilege_architecture;
}

uint64_t tlib_install_custom_instruction(uint64_t mask, uint64_t pattern, uint64_t length)
{
    if (cpu->custom_instructions_count == CPU_CUSTOM_INSTRUCTIONS_LIMIT) {
        // no more empty slots
        return 0;
    }

    custom_instruction_descriptor_t *ci = &cpu->custom_instructions[cpu->custom_instructions_count];

    ci->id = ++cpu->custom_instructions_count;
    ci->mask = mask;
    ci->pattern = pattern;
    ci->length = length;

    return ci->id;
}

void helper_wfi(CPUState *env);
void tlib_enter_wfi()
{
    helper_wfi(cpu);
}

void tlib_set_csr_validation_level(uint32_t value)
{
    switch (value) {
        case CSR_VALIDATION_FULL:
        case CSR_VALIDATION_PRIV:
        case CSR_VALIDATION_NONE:
            cpu->csr_validation_level = value;
            break;

        default:
            tlib_abortf("Unexpected CSR validation level: %d", value);
    }
}

uint32_t tlib_get_csr_validation_level()
{
    return cpu->csr_validation_level;
}

void tlib_set_nmi_vector(uint64_t nmi_adress, uint32_t nmi_length)
{
    if (nmi_adress > (TARGET_ULONG_MAX - nmi_length)) {
        cpu_abort(cpu, "NMIVectorAddress or NMIVectorLength value invalid. "
                  "Vector defined with these parameters will not fit in memory address space.");
    } else {
        cpu->nmi_address = (target_ulong)nmi_adress;
    }
    if (nmi_length > 32) {
        cpu_abort(cpu, "NMIVectorLength %d too big, maximum length supported is 32", nmi_length);
    } else {
        cpu->nmi_length = nmi_length;
    }
}

void tlib_set_nmi(int32_t nmi, int32_t state)
{
    if (state) {
        cpu_set_nmi(cpu, nmi);
    } else {
        cpu_reset_nmi(cpu, nmi);
    }
}

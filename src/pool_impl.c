/* SPDX-FileCopyrightText: Copyright (C) 2025 Stefan Muenzel
 * SPDX-License-Identifier: MPL-2.0
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>

CAMLprim value caml_pool_alloc(value v_block_wosize, value v_block_count) {
  size_t block_wosize = Long_val(v_block_wosize);
  size_t block_count = Long_val(v_block_count);
  size_t block_byte_size = (1+block_wosize)*sizeof(value);
  void* pool = calloc(block_count, block_byte_size);

  value* vpool = (value*)pool;
  for (size_t i = 0; i < block_count; i++) {
    vpool[i * block_wosize] = Caml_out_of_heap_header(block_wosize, 0);
    for(size_t j = 0; j < block_wosize; j++) {
      vpool[i * block_wosize + j + 1] = Val_unit;
    }
  }

  ptrdiff_t pool_ptr = (ptrdiff_t)pool;
  return Val_long(pool_ptr - 1);
}

CAMLprim value caml_pool_free(value v_pool_ptr) {
  ptrdiff_t pool_ptr = Long_val(v_pool_ptr);
  void* pool = (void*)(pool_ptr+1);
  free(pool);
  return Val_unit;
}

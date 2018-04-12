// gcc test-mark-buffers.c -lcyclone -pthread -lcyclone -lck -lm -ltommath -ldl  -L. -Iinclude -g
#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include "cyclone/runtime-main.h"

void main()
{
  int a, b, c, d;
  mark_buffer *mb = mark_buffer_init(2);
  printf("%p\n", mb);

  mark_buffer_set(mb, 0, &a);
  mark_buffer_set(mb, 1, &b);
  mark_buffer_set(mb, 2, &c);
  mark_buffer_set(mb, 3, &d);

  printf("%p - %p\n", &a, mark_buffer_get(mb, 0));
  printf("%p - %p\n", &b, mark_buffer_get(mb, 1));
  printf("%p - %p\n", &c, mark_buffer_get(mb, 2));
  printf("%p - %p\n", &d, mark_buffer_get(mb, 3));

  mark_buffer_free(mb);

  mb = mark_buffer_init(2);
  for(a = 0; a < 32; a++) {
    mark_buffer_set(mb, a, &b);
  }
  for(a = 0; a < 32; a++) {
    printf("%p\n", mark_buffer_get(mb, a));
  }
  mark_buffer_free(mb);
  return;
}

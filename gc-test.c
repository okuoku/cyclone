#include "include/cyclone/types.h"
#define GC_BLOCK_BITS 5
#define GC_BLOCK_SIZE (1 << GC_BLOCK_BITS)

/* HEAP definitions, based off heap from Chibi scheme */
#define gc_heap_first_block(h) ((object)(h->data + gc_heap_align(gc_free_chunk_size)))
#define gc_heap_last_block(h) ((object)((char*)h->data + h->size - gc_heap_align(gc_free_chunk_size)))
#define gc_heap_end(h) ((object)((char*)h->data + h->size))
#define gc_heap_pad_size(s) (sizeof(struct gc_heap_t) + (s) + gc_heap_align(1))
#define gc_free_chunk_size (sizeof(gc_free_list))

#define gc_align(n, bits) (((n)+(1<<(bits))-1)&(((uintptr_t)-1)-((1<<(bits))-1)))

//#define gc_word_align(n) gc_align((n), 2)
#define gc_heap_align(n) gc_align(n, GC_BLOCK_BITS)

// TODO: eventually would integrate this into gc_heap_create
void init_heap_bump_n_pop(gc_
/*
gc_heap *gc_heap_create(int heap_type, size_t size, size_t max_size,
                        size_t chunk_size, gc_thread_data *thd)
{
  gc_free_list *free, *next;
  gc_heap *h;
  size_t padded_size = gc_heap_pad_size(size);
  h = malloc(padded_size);      // TODO: mmap?
  if (!h)
    return NULL;
  h->type = heap_type;
  h->size = size;
  h->ttl = 10;
  if (heap_type < 3) { // Fixed size
    // TODO: I think this is overkill with the padding??
    h->remaining = size - (size % ((heap_type + 1) * 32));
  } else {
    h->remaining = 0;
  }
  h->next_free = h;
  h->next_frees = NULL;
  h->last_alloc_size = 0;
  //h->free_size = size;
  ck_pr_add_ptr(&(thd->cached_heap_total_sizes[heap_type]), size);
  ck_pr_add_ptr(&(thd->cached_heap_free_sizes[heap_type]), size);
  h->chunk_size = chunk_size;
  h->max_size = max_size;
  h->data = (char *)gc_heap_align(sizeof(h->data) + (uintptr_t) & (h->data));
  h->next = NULL;
  free = h->free_list = (gc_free_list *) h->data;
  next = (gc_free_list *) (((char *)free) + gc_heap_align(gc_free_chunk_size));
  free->size = 0;               // First one is just a dummy record
  free->next = next;
  next->size = size - gc_heap_align(gc_free_chunk_size);
  next->next = NULL;
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG h->data addr: %p\n", &(h->data));
  fprintf(stderr, "DEBUG h->data addr: %p\n", h->data);
  fprintf(stderr, ("heap: %p-%p data: %p-%p size: %zu\n"),
          h, ((char *)h) + gc_heap_pad_size(size), h->data, h->data + size,
          size);
  fprintf(stderr, ("first: %p end: %p\n"), (object) gc_heap_first_block(h),
          (object) gc_heap_end(h));
  fprintf(stderr, ("free1: %p-%p free2: %p-%p\n"), free,
          ((char *)free) + free->size, next, ((char *)next) + next->size);
#endif
  return h;
}
*/

// TODO: take a fixed-size heap page, possibly using bump&pop and convert it to use a free list
// one complication here is we probably don't want to always have the 0-size "next" at the 
/*init_free_list() {

} 
*/

// TODO: for these next 2 to be any good at all, the free list impl needs to be faster than what cyclone
//       already does, since bump&pop will be used rarely, and may not even make sense for us
// TODO: try_alloc using bump&pop or new free list
// TODO: sweep using bump&pop or new free list
void main(){
  printf("32 heap aligned - %lu\n", gc_heap_align(32));
  printf("64 heap aligned - %lu\n", gc_heap_align(64));
  printf("96 heap aligned - %lu\n", gc_heap_align(96));
}

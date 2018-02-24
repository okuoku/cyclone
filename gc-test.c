#include "include/cyclone/types.h"
#include <time.h>
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
gc_heap *init_heap_bump_n_pop(int heap_type, size_t size)
/*
gc_heap *gc_heap_create(int heap_type, size_t size, size_t max_size,
                        size_t chunk_size, gc_thread_data *thd) */
{
  gc_free_list *free, *next;
  gc_heap *h;
  size_t padded_size = gc_heap_pad_size(size);
  h = malloc(padded_size);
  if (!h)
    return NULL;
  h->type = heap_type;
  h->size = size;
  h->ttl = 10;
//  h->next_free = h;
//  h->next_frees = NULL;
//  h->last_alloc_size = 0;
//
  h->data = (char *)gc_heap_align(sizeof(h->data) + (uintptr_t) & (h->data));
//  h->next = NULL;
//  free = h->free_list = (gc_free_list *) h->data;
//  next = (gc_free_list *) (((char *)free) + gc_heap_align(gc_free_chunk_size));
//  free->size = 0;               // First one is just a dummy record
//  free->next = next;
//  next->size = size - gc_heap_align(gc_free_chunk_size);
//  next->next = NULL;

  if (heap_type < 3) { // Fixed size
    h->block_size = (heap_type + 1) * 32;
    h->remaining = size - (size % h->block_size);
    h->data_end = h->data + h->remaining;
  } else {
    h->block_size = 0;
    h->remaining = 0;
    h->data_end = NULL;
  }
  h->free_list = NULL; // No free lists with bump+pop
  return h;
}

// TODO: want 2 flavors of this, one with unitialized memory (for testing) and another that
//       maps across a region of memory with allocated objects in it
//       the second flavor needs to take into account h->remaining
//
// TODO: take a fixed-size heap page, possibly using bump&pop and convert it to use a free list
// one complication here is we probably don't want to always have the 0-size "next" at the 
void init_free_list(gc_heap *h) {
  // for this flavor, just layer a free list on top of unitialized memory
  gc_free_list *next;
  int remaining = h->size - (h->size % h->block_size) - h->block_size; // Remove first one
  next = h->free_list = (gc_free_list *)h->data;
  while (remaining) {
    printf("init remaining=%d next = %p\n", remaining, next);
    next->next = (gc_free_list *)(((char *) next) + h->block_size);
    next = next->next;
    remaining -= h->block_size;
  }
  next->next = NULL;
  h->data_end = NULL; // Indicate we are using free lists
} 

#define TEST_COLOR_MARK 0
#define TEST_COLOR_CLEAR 1
#define RANDOM_COLOR (rand() % 2)

void convert_to_free_list(gc_heap *h) {
  gc_free_list *next;
  int remaining = h->size - (h->size % h->block_size) - h->block_size; // Remove first one
  if (h->data_end == NULL) return; // Already converted

  next = h->free_list = NULL;
  while (remaining >= h->remaining) {
    object obj = h->data_end - remaining - h->block_size;
    int tag = type_of(obj);
    int color = mark(obj);
    printf("found object %d color %d at %p with remaining=%lu\n", tag, color, obj, remaining);
    // free space, add it to the free list
    if (color == TEST_COLOR_CLEAR) {
      if (next == NULL) {
        next = h->free_list = obj;
      }
      else {
        next->next = obj;
        next = next->next;
      }
    }
    remaining -= h->block_size;
  }

  // Convert any empty space at the end
  while (remaining) {
    object obj = h->data_end - remaining - h->block_size;
    printf("no object at %p fill with free list\n", obj);
    if (next == NULL) {
      next = h->free_list = obj;
    }
    else {
      next->next = (gc_free_list *)(((char *) next) + h->block_size);
      next = next->next;
    }
    remaining -= h->block_size;
  }

  if (next) {
    next->next = NULL;
  }
  // Let GC know this heap is not bump&pop
  h->remaining = NULL;
  h->data_end = NULL;
}

// TODO: for these next 2 to be any good at all, the free list impl needs to be faster than what cyclone
//       already does, since bump&pop will be used rarely, and may not even make sense for us
// Done: try_alloc using bump&pop or new free list
// TODO: sweep using bump&pop or new free list

void *alloc(gc_heap *h, int heap_type)
{
  // TODO: real version will loop over all heap pages

  //TODO: if h->free_list, take from there (favor free lists for us)
  // else check remaining (bump+pop)
  // else heap is full

  if (h->free_list) {
    void *result = h->free_list;
    h->free_list = h->free_list->next;
    return result;
  } else if (h->remaining) {
    h->remaining -= h->block_size;
    return h->data_end - h->remaining - h->block_size;
  }

  return NULL; // Unable to allocate
}

void test_allocate_objects_on_bump_n_pop(gc_heap *h){
  int i, end = rand() % 10 + 20;
  for (i = 0; i < end; i++) {
  // TODO: for each next (randomly end, do not necessarily allocate everything
    // allocate
    pair_type *p = alloc(h, 0);
    // assign an object to it
    p->tag = pair_tag;
    car(p) = NULL;
    cdr(p) = NULL;
    // randomly assign a color (either white or black)
    mark(p) = RANDOM_COLOR;
    grayed(p) = 0;
    printf("test allocated list at %p\n", p);
  }
}

void main(){
  int i;
  gc_heap *h = init_heap_bump_n_pop(0, 1000);
  srand(time(NULL));
  printf("data start = %p\n", h->data);
  printf("data end = %p\n", h->data_end);
  printf("remaining = %d\n", h->remaining);
  printf(" 1 heap aligned - %lu\n", gc_heap_align(1));
  printf("32 heap aligned - %lu\n", gc_heap_align(32));
  printf("64 heap aligned - %lu\n", gc_heap_align(64));
  printf("96 heap aligned - %lu\n", gc_heap_align(96));

  printf("data start: %p\n", h->data);
  for (i = 0; i < 34; i++) {
    printf("alloc %d: %p remaining: %lu\n", i, alloc(h, 0), h->remaining);
  }

  h = init_heap_bump_n_pop(0, 1000);
  test_allocate_objects_on_bump_n_pop(h);
  convert_to_free_list(h);
  // TODO: repeat above allocation with convertd free list
  printf("data start: %p\n", h->data);
  for (i = 0; i < 34; i++) {
    printf("alloc %d: %p remaining: %lu\n", i, alloc(h, 0), h->remaining);
  }

  // repeat above allocation with free list
  init_free_list(h);
  printf("free list data start: %p\n", h->data);
  for (i = 0; i < 34; i++) {
    printf("free list alloc %d: %p remaining: %lu\n", i, alloc(h, 0), h->remaining);
  }

  // TODO: sweeping (both of bump and of free list)
}

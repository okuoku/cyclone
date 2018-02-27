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
  size_t padded_size;
  size = gc_heap_align(size);
  padded_size = gc_heap_pad_size(size);
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
  h->next = NULL;
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
  int i = 0, remaining = h->size - (h->size % h->block_size) - h->block_size; // Starting at first one so skip it
  next = h->free_list = (gc_free_list *)h->data;
  printf("data start = %p\n", h->data);
  printf("data end = %p\n", h->data + h->size);
  while (remaining >= h->block_size) {
    printf("%d init remaining=%d next = %p\n", i++, remaining, next);
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

// Essentially this is half of the sweep code, for sweeping bump&pop
void convert_to_free_list(gc_heap *h) {
  gc_free_list *next;
  int remaining = h->size - (h->size % h->block_size);
  if (h->data_end == NULL) return; // Already converted

  next = h->free_list = NULL;
  while (remaining > h->remaining) {
    object obj = h->data_end - remaining;
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
    object obj = h->data_end - remaining;
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
  h->remaining = 0;
  h->data_end = NULL;
}

// TODO: for these next 2 to be any good at all, the free list impl needs to be faster than what cyclone
//       already does, since bump&pop will be used rarely, and may not even make sense for us
// Done: try_alloc using bump&pop or new free list
// TODO: sweep using bump&pop or new free list

//TODO: to sweep free list, let's take the code from gc.c (below) and simplify as needed
/**
 * @brief Sweep portion of the GC algorithm
 * @param h           Heap to sweep
 * @param heap_type   Type of heap, based on object sizes allocated on it
 * @param sum_freed_ptr Out parameter tracking the sum of freed data, in bytes.
 *                      This parameter is ignored if NULL is passed.
 * @param thd           Thread data object for the mutator using this heap
 * @return Return the size of the largest object freed, in bytes
 *
 * This portion of the major GC algorithm is responsible for returning unused
 * memory slots to the heap. It is only called by the collector thread after
 * the heap has been traced to identify live objects.
 */
void my_gc_sweep(gc_heap * h, int heap_type, size_t * sum_freed_ptr, gc_thread_data *thd)
{
  size_t freed, heap_freed = 0, sum_freed = 0, size;
  object p, end;
  gc_free_list *q, *r, *s;
#if GC_DEBUG_SHOW_SWEEP_DIAG
  gc_heap *orig_heap_ptr = h;
#endif
  gc_heap *prev_h = NULL;

  //
  // Lock the heap to prevent issues with allocations during sweep
  // This coarse-grained lock actually performed better than a fine-grained one.
  //
//  pthread_mutex_lock(&(thd->heap_lock));
  h->next_free = h;

#if GC_DEBUG_SHOW_SWEEP_DIAG
  fprintf(stderr, "\nBefore sweep -------------------------\n");
  fprintf(stderr, "Heap %d diagnostics:\n", heap_type);
  gc_print_stats(orig_heap_ptr);
#endif

  for (; h; prev_h = h, h = h->next) {      // All heaps

    //gc_free_list *next;
    int remaining = h->size - (h->size % h->block_size); // - h->block_size; // Remove first one??
    char *data_end = h->data + remaining;
    q = h->free_list;
    while (remaining) {
      p = data_end - remaining;
      if (mark(p) == TEST_COLOR_CLEAR) {
        if (h->free_list == NULL) {
          q = h->free_list = p;
          h->free_list->next = NULL;
          printf("sweep remaining=%d, %p, assign h->free_list\n", remaining, p);
        } else if ((char *)p <= (char *)h->free_list) {
          s = (gc_free_list *)p;
          s->next = h->free_list->next;
          q = h->free_list = p;
          printf("sweep remaining=%d, %p, assign h->free_list and next\n", remaining, p);
        } else {
          // find preceding/succeeding free list pointers for p
          for (r = q->next; r && ((char *)r < (char *)p); q = r, r = r->next) ;
          if ((char *)r == (char *)p) {     // this is a free block, skip it
            continue;
          }
          s = (gc_free_list *)p;
          s->next = r;
          q->next = s;
          printf("sweep remaining=%d, %p, q=%p, r=%p\n", remaining, p, q, r);
        }

      } else {
        printf("sweep block is still used remaining=%d p = %p\n", remaining, p);
      }
      //next->next = (gc_free_list *)(((char *) next) + h->block_size);
      //next = next->next;
      remaining -= h->block_size;
    }


//#if GC_DEBUG_TRACE
//    fprintf(stderr, "sweep heap %p, size = %zu\n", h, (size_t) h->size);
//#endif
//    p = gc_heap_first_block(h);
//    q = h->free_list;
//TODO: need to debug all of this. for example, q could be null if all of the free slots have been exhausted
//also, h->free_list keeps moving down, closer to the end. we may need to have a special case for the first
//instance of q... h->free_list will most likely be reassigned to the first garbage we find
//
//    end = gc_heap_end(h);
//    while (p < end) {
//      // find preceding/succeeding free list pointers for p
//      for (r = q->next; r && ((char *)r < (char *)p); q = r, r = r->next) ;
//
//      if ((char *)r == (char *)p) {     // this is a free block, skip it
//        p = (object) (((char *)p) + h->block_size);
//#if GC_DEBUG_VERBOSE
//        fprintf(stderr, "skip free block %p size = %zu\n", p, h->block_size);
//#endif
//        continue;
//      }
//      size = h->block_size;
//
//#if GC_SAFETY_CHECKS
//      if (!is_object_type(p)) {
//        fprintf(stderr, "sweep: invalid object at %p", p);
//        exit(1);
//      }
//      if ((char *)q + h->block_size > (char *)p) {
//        fprintf(stderr, "bad size at %p < %p + %u", p, q, h->block_size);
//        exit(1);
//      }
//      if (r && ((char *)p) + size > (char *)r) {
//        fprintf(stderr, "sweep: bad size at %p + %zu > %p", p, size, r);
//        exit(1);
//      }
//#endif
//
//printf("sweep p=%p q=%p r=%p\n", p, q, r);
//if (0) {
//  // TODO: different assumptions so need to sweep differently. in our new approach each free chunk is guaranteed to be h->block_size large (is last one a possible exception though?)
//
//      if (mark(p) == /*gc_color_clear*/ TEST_COLOR_CLEAR) {
//#if GC_DEBUG_VERBOSE
//        fprintf(stderr, "sweep is freeing unmarked obj: %p with tag %d\n", p,
//                type_of(p));
//#endif
//        mark(p) = gc_color_blue;        // Needed?
///* TODO:
//        if (type_of(p) == mutex_tag) {
//#if GC_DEBUG_VERBOSE
//          fprintf(stderr, "pthread_mutex_destroy from sweep\n");
//#endif
//          if (pthread_mutex_destroy(&(((mutex) p)->lock)) != 0) {
//            fprintf(stderr, "Error destroying mutex\n");
//            exit(1);
//          }
//        } else if (type_of(p) == cond_var_tag) {
//#if GC_DEBUG_VERBOSE
//          fprintf(stderr, "pthread_cond_destroy from sweep\n");
//#endif
//          if (pthread_cond_destroy(&(((cond_var) p)->cond)) != 0) {
//            fprintf(stderr, "Error destroying condition variable\n");
//            exit(1);
//          }
//        } else if (type_of(p) == bignum_tag) {
//          // TODO: this is no good if we abandon bignum's on the stack
//          // in that case the finalizer is never called
//#if GC_DEBUG_VERBOSE
//          fprintf(stderr, "mp_clear from sweep\n");
//#endif
//          mp_clear(&(((bignum_type *)p)->bn));
//        }
//*/
//        // free p
//        heap_freed += size;
//        if (((((char *)q) + h->block_size) == (char *)p) && (q != h->free_list)) {
//          /* merge q with p */
//          if (r && h->block_size && ((((char *)p) + size) == (char *)r)) {
//            // ... and with r
//            q->next = r->next;
//            freed = q->size + size + r->size;
//            p = (object) (((char *)p) + size + r->size);
//          } else {
//            freed = q->size + size;
//            p = (object) (((char *)p) + size);
//          }
//          q->size = freed;
//        } else {
//          s = (gc_free_list *) p;
//          if (r && r->size && ((((char *)p) + size) == (char *)r)) {
//            // merge p with r
//            s->size = size + r->size;
//            s->next = r->next;
//            q->next = s;
//            freed = size + r->size;
//          } else {
//            s->size = size;
//            s->next = r;
//            q->next = s;
//            freed = size;
//          }
//          p = (object) (((char *)p) + freed);
//        }
//      } else {
////#if GC_DEBUG_VERBOSE
////        fprintf(stderr, "sweep: object is marked %p\n", p);
////#endif
//        p = (object) (((char *)p) + size);
//      }
//    }


//    ck_pr_add_ptr(&(thd->cached_heap_free_sizes[heap_type]), heap_freed);
    // Free the heap page if possible.
    //
    // With huge heaps, this becomes more important. one of the huge
    // pages only has one object, so it is likely that the page
    // will become free at some point and could be reclaimed.
    //
    // The newly created flag is used to attempt to avoid situtaions
    // where a page is allocated because there is not enough free space,
    // but then we do a sweep and see it is empty so we free it, and
    // so forth. A better solution might be to keep empty heap pages
    // off to the side and only free them if there is enough free space
    // remaining without them.
    //
    // Experimenting with only freeing huge heaps
//    if (gc_is_heap_empty(h) && 
//          (h->type == HEAP_HUGE || !(h->ttl--))) {
//        unsigned int h_size = h->size;
//        gc_heap *new_h = gc_heap_free(h, prev_h);
//        if (new_h) { // Ensure free succeeded
//          h = new_h;
////          ck_pr_sub_ptr(&(thd->cached_heap_free_sizes[heap_type] ), h_size);
////          ck_pr_sub_ptr(&(thd->cached_heap_total_sizes[heap_type]), h_size);
//        }
//    }
    sum_freed += heap_freed;
    heap_freed = 0;
  }

#if GC_DEBUG_SHOW_SWEEP_DIAG
  fprintf(stderr, "\nAfter sweep -------------------------\n");
  fprintf(stderr, "Heap %d diagnostics:\n", heap_type);
  gc_print_stats(orig_heap_ptr);
#endif

//  pthread_mutex_unlock(&(thd->heap_lock));
  if (sum_freed_ptr)
    *sum_freed_ptr = sum_freed;
}

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

void print_free_list(gc_heap *h) {
  gc_free_list *f = h->free_list;
  printf("printing free list:\n");
  while(f) {
    printf("%p\n", f);
    f = f->next;
  }
  printf("done\n");
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
    printf("test allocated list %d at %p\n", mark(p), p);
  }
}

void main(){
  int i;
  void *tmp;
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
    tmp = alloc(h, 0);
    printf("alloc %d: %p remaining: %lu\n", i, tmp, h->remaining);
  }

  h = init_heap_bump_n_pop(0, 1000);
  test_allocate_objects_on_bump_n_pop(h);
  convert_to_free_list(h);
  // TODO: repeat above allocation with convertd free list
  printf("data start: %p\n", h->data);
  for (i = 0; i < 34; i++) {
    tmp = alloc(h, 0);
    printf("alloc %d: %p remaining: %lu\n", i, tmp, h->remaining);
  }

  // repeat above allocation with free list
  init_free_list(h);
  printf("free list data start: %p\n", h->data);
  for (i = 0; i < 34; i++) {
    pair_type *p = alloc(h, 0);
    int color = -1;
    if (p) {
      p->tag = pair_tag;
      car(p) = cdr(p) = NULL;
      color = mark(p) = RANDOM_COLOR;
      grayed(p) = 0;
    }
    printf("free list alloc %d: %p color: %d remaining: %lu\n", i, p, color, h->remaining);
  }

  // TODO: sweeping (both of bump and of free list)
  size_t tmpsz;
  my_gc_sweep(h, 0, &tmpsz, NULL);
  print_free_list(h);

  // do more allocs
  for (i = 0; i < 34; i++) {
    pair_type *p = alloc(h, 0);
    int color = -1;
    if (p) {
      p->tag = pair_tag;
      car(p) = cdr(p) = NULL;
      color = mark(p) = RANDOM_COLOR;
      grayed(p) = 0;
    }
    printf("free list alloc %d: %p color: %d remaining: %lu\n", i, p, color, h->remaining);
  }
  my_gc_sweep(h, 0, &tmpsz, NULL);
  print_free_list(h);
}

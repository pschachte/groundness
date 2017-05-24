/*****************************************************************
  File     : bryant.c
  RCS      : $Id: bryant.c,v 1.28 2004/04/02 14:04:36 schachte Exp $
  Author   : Peter Schachte, based on code by Tania Armstrong
  Purpose  : Manipulation of boolean functions
  Copyright: © 1995 Peter Schachte.  All rights reserved.

*****************************************************************/

/*****************************************************************

		     Controlling #defined Symbols


  This code is conditionalized on a number of #defined symbols.  They are:

    STATISTICS		Controls collecting of computed and unique
			table hash performance.  If defined,
			concludeRep() prints out a bunch of
			statistics.

    CLEAR_CACHES	If defined, all computed and unique tables are
			cleared on a call to initRep().  This makes
			running a number of tests in sequence more
			fair.

    USE_ITE_CONSTANT	Include the ite_constant function.

    SHARING		Include algorithms useful in sharing analysis
			performed using Boolean functions.

    ELIM_DUPS		If defined, iff_conj_array will not assume
			that the input array has no duplicates.  It
			will still assume that it's sorted.

    EQUAL_TEST		If defined, lub(), glb(), and projected_glb
			compare their arguments for equality, and if
			equal return it as value.  ite and ite_var
			always do this with their last 2 args.  The
			equality test is pretty cheap, and the savings
			will sometimes be huge, so this should
			probably always be defined.

    NO_CHEAP_SHIFT	If defined, shifting an integer variable number
			of places is relatively expensive on this platform,
			and so should be avoided.  In this case we use a
			table where possible to avoid shifting.


*****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <limits.h>
#include "bryant.h"

/* #define VERBOSE_DEBUG */


#define UNUSED_MAPPING -1	/* this MUST BE -1 */

#if !defined(max)
#define max(a,b) ((a)<(b) ? (b) : (a))
#endif
#if !defined(min)
#define min(a,b) ((a)<(b) ? (a) : (b))
#endif

#define PERCENTAGE(x,y) ((100.0 * (float)(x)) / (float)(y))

typedef struct pool {
    node data[POOL_SIZE];
    struct pool *prev;
} pool;


#if defined(NO_CHEAP_SHIFT) && BITS_PER_WORD == 32
bitmask following_bits[BITS_PER_WORD] =
    {  0xffffffff, 0xfffffffe, 0xfffffffc, 0xfffffff8,
       0xfffffff0, 0xffffffe0, 0xffffffc0, 0xffffff80,
       0xffffff00, 0xfffffe00, 0xfffffc00, 0xfffff800,
       0xfffff000, 0xffffe000, 0xffffc000, 0xffff8000,
       0xffff0000, 0xfffe0000, 0xfffc0000, 0xfff80000,
       0xfff00000, 0xffe00000, 0xffc00000, 0xff800000,
       0xff000000, 0xfe000000, 0xfc000000, 0xf8000000,
       0xf0000000, 0xe0000000, 0xc0000000, 0x80000000
    };

bitmask preceding_bits[BITS_PER_WORD] =
    {  0x00000001, 0x00000003, 0x00000007, 0x0000000f,
       0x0000001f, 0x0000003f, 0x0000007f, 0x000000ff,
       0x000001ff, 0x000003ff, 0x000007ff, 0x00000fff,
       0x00001fff, 0x00003fff, 0x00007fff, 0x0000ffff,
       0x0001ffff, 0x0003ffff, 0x0007ffff, 0x000fffff,
       0x001fffff, 0x003fffff, 0x007fffff, 0x00ffffff,
       0x01ffffff, 0x03ffffff, 0x07ffffff, 0x0fffffff,
       0x1fffffff, 0x3fffffff, 0x7fffffff, 0xffffffff
    };
#endif


unsigned char first_one_bit[256] =
    {255, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  16 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  32 */
       5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  48 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  64 */
       6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  80 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  96 */
       5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 112 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 128 */
       7, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 144 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 160 */
       5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 176 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 192 */
       6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 208 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 224 */
       5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 240 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 256 */
    };

unsigned char last_one_bit[256] =
    {255, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, /*  16 */
       4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, /*  32 */
       5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, /*  48 */
       5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, /*  64 */
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, /*  80 */
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, /*  96 */
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, /* 112 */
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, /* 128 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 144 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 160 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 176 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 192 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 208 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 224 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 240 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 256 */
    };


/* automatically initialized to all 0 bits: */
bitset emptyset;

/* To give as a renaming array when we don't want any renaming */
int identity_renaming[MAXVAR];

/****************************************************************

			   Macros to avoid #ifdefs

 ****************************************************************/


#if defined(CLEAR_CACHES)
#define CLEAR_CACHE(op)	\
memset(op##_computed_cache, 0, sizeof(op##_computed_cache))
#else /* !CLEAR_CACHES */
#define CLEAR_CACHE(op)
#endif


#if defined(STATISTICS)

/* The largest bucket size to be separately counted.  Larger buckets
 * will be listed as "> MAX_COUNT." 
 */
#define MAX_COUNT 1000

int unique_table_hits, unique_table_misses;

#define DECLARE_FN_COUNT(op) int op##_count;

#define COUNT_FN(fn) (++fn##_count)

#define INIT_FN_COUNT(fn) fn##_count = 0
#define PRINT_FN_COUNT(fn) \
  if (fn##_count!=0) printf("%6d calls to " #fn "\n", fn##_count);

#define COUNT_UNIQUE_HIT (++unique_table_hits)
#define COUNT_UNIQUE_MISS (++unique_table_misses)

#define COUNT_HIT(op) (++op##_computed_hits)
#define COUNT_MISS(op) (++op##_computed_misses, ++cache->count)
#define INIT_CACHE(op)			\
    do {				\
	op##_computed_misses = 0;	\
	op##_computed_hits = 0;		\
	CLEAR_CACHE(op);		\
    } while (0)

#define CACHE_COUNT_MEMBER int count;
#define PRINT_CACHE_PERFORMANCE(op)					\
do {									\
    if (op##_computed_misses > 0 ) {					\
	int i, size_count[MAX_COUNT+2];					\
	printf(#op " computed table:  %d hits, %d misses, %.2f%% hit rate\n",\
	       op##_computed_hits, op##_computed_misses,		\
		PERCENTAGE(op##_computed_hits,				\
			   op##_computed_hits + op##_computed_misses));	\
	memset(size_count, 0, sizeof(size_count));			\
	for (i=0; i<COMPUTED_TABLE_SIZE; ++i) {				\
	    int count = op##_computed_cache[i].count;			\
	    ++size_count[(count<=MAX_COUNT ? count : MAX_COUNT+1)];	\
	}								\
	print_distribution(size_count, MAX_COUNT);			\
    }									\
} while (0)
#else /* ! STATISTICS */
#define DECLARE_FN_COUNT(op)
#ifdef VERBOSE_DEBUG
#define COUNT_FN(fn) (QP_printf("Calling " #fn "()\n"), QP_flush(QP_curout))
#else
#define COUNT_FN(fn)
#endif
#define INIT_FN_COUNT(fn)
#define PRINT_FN_COUNT(fn)
#define COUNT_UNIQUE_HIT
#define COUNT_UNIQUE_MISS
#define INIT_CACHE(op) CLEAR_CACHE(op)
#define PRINT_CACHE_PERFORMANCE(op)
#define COUNT_HIT(fn)
#define COUNT_MISS(fn)
#define CACHE_COUNT_MEMBER
#endif /* STATISTICS */


#if defined(EQUAL_TEST)
#define DIRECT_EQUAL_TEST(f,g,result) if ((f) == (g)) return (result)
#define ELSE_TRY_EQUAL_TEST(f,g,result) \
  else DIRECT_EQUAL_TEST(f,g,result);
#else /* ! EQUAL_TEST */
#define DIRECT_EQUAL_TEST(f,g,result)
#define ELSE_TRY_EQUAL_TEST(f,g,result)
#endif

#if defined(CLEAR_CACHES)
#define INIT_UNIQUE_TABLE				\
    do {						\
	memset(unique_table, 0, sizeof(unique_table));	\
	while (curr_pool!=NULL) {			\
	    pool *p = curr_pool->prev;			\
	    free(curr_pool);				\
	    curr_pool = p;				\
	}						\
	curr_pool_ptr = curr_pool_end_ptr = NULL;	\
	pool_count = 0;					\
    } while (0)
#else /* !CLEAR_CACHES */
#define INIT_UNIQUE_TABLE
#endif /* CLEAR_CACHES */



/********************************************************************

		      Caching of computed values

  For improved efficiency, we maintain a
  cache of previously computed values to certain functions, and use
  this to avoid costly computations when possible.  This is
  particularly important for ROBDDs, because the high degree of fan-in
  causes frequent repeated computations.

  Because caching is in many ways extraneous to the functions being
  cached, we put the caching code here as macros, so that the
  presentation of the algorithms is minimally impacted by caching.

 ********************************************************************/

#if defined(STATISTICS)
#define DECLARE_CACHE(op,type)			\
static int op##_computed_hits;			\
static int op##_computed_misses;		\
static type op##_computed_cache[COMPUTED_TABLE_SIZE]
#else /* !STATISTICS */
#define DECLARE_CACHE(op,type)			\
static type op##_computed_cache[COMPUTED_TABLE_SIZE]
#endif /* STATISTICS */

/**************************** the cache for ite **************************/

#define TERNARY_NODE_HASH(f,g,h) \
  (((INTCAST(f)>>4)+INTCAST(g)+(INTCAST(h)<<1)) % COMPUTED_TABLE_SIZE)

typedef struct {
    node *f;
    node *g;
    node *h;
    node *result;
    CACHE_COUNT_MEMBER
} ite_cache_entry;

DECLARE_CACHE(ite, ite_cache_entry);
#if defined(USE_ITE_CONSTANT)
DECLARE_CACHE(ite_constant, ite_cache_entry);
#endif /* USE_ITE_CONSTANT */

#define DECLARE_ITE_CACHE_ENTRY ite_cache_entry *cache;

#define TRY_ITE_CACHE(n1,n2,n3,op)				\
do {								\
	cache = &op##_computed_cache[TERNARY_NODE_HASH(n1,n2,n3)]; \
	if (cache->f==n1 && cache->g==n2 && cache->h==n3) {	\
	    COUNT_HIT(op);					\
	    return cache->result;				\
	}							\
} while (0)

#define UPDATE_ITE_CACHE(n1,n2,n3,node,op)	\
do {					\
	cache->f = n1;			\
	cache->g = n2;			\
	cache->h = n3;			\
	cache->result = node;		\
	COUNT_MISS(op);			\
} while (0)


/******************** the cache for unary operations ************/

#define UNARY_NODE_HASH(a) \
      (INTCAST(a) % COMPUTED_TABLE_SIZE)

typedef struct {
    node *f;
    node *result;
    CACHE_COUNT_MEMBER
} unary_cache_entry;

#if defined(SHARING)
DECLARE_CACHE(upclose, unary_cache_entry);
DECLARE_CACHE(complete_one, unary_cache_entry);
#endif /* SHARING */

#define DECLARE_UNARY_CACHE_ENTRY unary_cache_entry *cache;

#define UPDATE_UNARY_CACHE(n,node,op)	\
do {					\
	cache->f = n;			\
	cache->result = node;		\
	COUNT_MISS(op);		\
} while (0)

#define TRY_UNARY_CACHE(n,op)						\
do {									\
	cache = &((op##_computed_cache)[UNARY_NODE_HASH(n)]);		\
	if (cache->f==(n)) {						\
	    COUNT_HIT(op);						\
	    return cache->result;					\
	}								\
} while (0)


/******************** the cache for var_entailed ************/

#define VAR_ENTAILED_HASH(a,n) \
      ((INTCAST(a)+n) % COMPUTED_TABLE_SIZE)

typedef struct {
    node *f;
    int  n;
    int  result;
    CACHE_COUNT_MEMBER
} var_entailed_cache_entry;

DECLARE_CACHE(var_entailed, var_entailed_cache_entry);

#define DECLARE_VAR_ENTAILED_CACHE_ENTRY var_entailed_cache_entry *cache;

#define UPDATE_VAR_ENTAILED_CACHE(node,var,val)	\
do {						\
	cache->f = node;			\
	cache->n = var;				\
	cache->result = val;			\
	COUNT_MISS(var_entailed);		\
} while (0)

#define TRY_VAR_ENTAILED_CACHE(node,var)		\
do {							\
	cache = &((var_entailed_computed_cache)		\
		  [VAR_ENTAILED_HASH(node,var)]);	\
	if (cache->f==(node) && cache->n==(var)) {	\
	    COUNT_HIT(op);				\
	    return cache->result;			\
	}						\
} while (0)


/**************** the cache for unary set-valued operations ********/

typedef struct {
    node *f;
    bitset result;
    CACHE_COUNT_MEMBER
} unary_bitset_cache_entry;

DECLARE_CACHE(vars_entailed, unary_bitset_cache_entry);

#define DECLARE_UNARY_BITSET_CACHE_ENTRY \
	unary_bitset_cache_entry *cache;


#define UPDATE_UNARY_BITSET_CACHE(n,set,op)		\
do {							\
	cache->f = n;					\
	cache->result = set;				\
	COUNT_MISS(op);					\
} while (0)

#define TRY_UNARY_BITSET_CACHE(n,op)			\
do {								\
	cache = &((op##_computed_cache)[UNARY_NODE_HASH(n)]);	\
	if (cache->f==(n)) {					\
	    COUNT_HIT(op);					\
	    return &cache->result;				\
	}							\
} while (0)


/******************** the cache for symmetric binary operations ************/

/* NB:  Since glb and lub are commutative, cache entries will work both
 * ways round, so we want a symmetrical cache, ie, (a,b) should hash
 * to the same value as (b,a).  We achieve this by first exchanging a and b
 * if a > b (comparing their addresses) in TRY_BIN_CACHE.  We assume that
 * they won't be changed (or exchanged) before UPDATE_BIN_CACHE is called.
 */

#define BINARY_NODE_HASH(a,b) \
      ((INTCAST(a)+(INTCAST(b)<<1)) % COMPUTED_TABLE_SIZE)

typedef struct {
    node *f;
    node *g;
    node *result;
    CACHE_COUNT_MEMBER
} bin_cache_entry;

DECLARE_CACHE(glb, bin_cache_entry);
DECLARE_CACHE(lub, bin_cache_entry);
#if defined(SHARING)
DECLARE_CACHE(bin, bin_cache_entry);
DECLARE_CACHE(complete, bin_cache_entry);
#endif

#define DECLARE_BIN_CACHE_ENTRY bin_cache_entry *cache;

#define UPDATE_BIN_CACHE(n1,n2,node,op)	\
do {					\
	cache->f = n1;			\
	cache->g = n2;			\
	cache->result = node;		\
	COUNT_MISS(op);		\
} while (0)

#define TRY_BIN_CACHE(n1,n2,op)						\
do {									\
	if (n2 < n1) { node *temp = (n2); (n2) = (n1); (n1) = temp; }	\
	cache = &((op##_computed_cache)[BINARY_NODE_HASH(n1,n2)]);	\
	if (cache->f==(n1) && cache->g==(n2)) {				\
	    COUNT_HIT(op);						\
	    return cache->result;					\
	}								\
} while (0)

/******************** the cache for asymmetric binary operations ************/

#if defined(SHARING)
DECLARE_CACHE(complete_one_or, bin_cache_entry);

#define DECLARE_ASYM_BIN_CACHE_ENTRY bin_cache_entry *cache;

#define UPDATE_ASYM_BIN_CACHE(n1,n2,node,op) \
	UPDATE_BIN_CACHE(n1,n2,node,op)

#define TRY_ASYM_BIN_CACHE(n1,n2,op)					\
do {									\
	cache = &((op##_computed_cache)[BINARY_NODE_HASH(n1,n2)]);	\
	if (cache->f==(n1) && cache->g==(n2)) {				\
	    COUNT_HIT(op);						\
	    return cache->result;					\
	}								\
} while (0)
#endif /* SHARING */

/************************ the cache for rename_pglb ********************/

typedef struct {
    node *f;
    node *g;
    node *result;
    unsigned int unique;
    CACHE_COUNT_MEMBER
} rename_pglb_cache_entry;

static unsigned int oae_accumulator = 0;

DECLARE_CACHE(rename_pglb, rename_pglb_cache_entry);

#define DECLARE_RENAME_PGLB_CACHE_ENTRY rename_pglb_cache_entry *cache;

#define UPDATE_RENAME_PGLB_CACHE(n1,n2,node,acc) \
do {					\
	cache->f = n1;			\
	cache->g = n2;			\
	cache->result = node;		\
	cache->unique = acc;		\
	COUNT_MISS(rename_pglb);			\
} while (0)

#define TRY_RENAME_PGLB_CACHE(n1,n2,acc)				\
do {									\
	cache = &((rename_pglb_computed_cache)[BINARY_NODE_HASH(n1,n2)]);\
	if (cache -> unique == acc && cache->f==(n1) && cache->g==(n2)) {	\
	    COUNT_HIT(op);						\
	    return cache->result;					\
	}								\
} while (0)

/*
#define CLEAR_RENAME_PGLB_CACHE \
do { \
	memset(rename_pglb_computed_cache, 0x0, sizeof(bin_cache_entry) * COMPUTED_TABLE_SIZE) \
} while(0)

#define CLEAR_RENAME_PGLB_CACHE \
do { \
	int i; \
	for (i = 0; i < COMPUTED_TABLE_SIZE; i++) { \
		rename_pglb_computed_cache[i].f = (node *) 2; \
		rename_pglb_computed_cache[i].g = (node *) 2; \
		rename_pglb_computed_cache[i].result = (node *) 2; \
	} \
} while(0)
*/

/**************************** the cache for rglb ***********************/

typedef struct {
    node *f;
    node *g;
    node *result;
    int  thresh;
    CACHE_COUNT_MEMBER
} rglb_cache_entry;

DECLARE_CACHE(rglb, rglb_cache_entry);

#define DECLARE_RGLB_CACHE_ENTRY rglb_cache_entry *cache;

#define TRY_RGLB_CACHE(n1,n2,th)				\
do {								\
	if (n2 < n1) { node *temp = (n2); (n2) = (n1); (n1) = temp; }	\
	cache = &rglb_computed_cache[BINARY_NODE_HASH(n1,n2)];	\
	if (cache->f==(n1) && cache->g==(n2) &&			\
	    cache->thresh >= th) {				\
	    COUNT_HIT(rglb);					\
	    if (cache->thresh == th) return cache->result;	\
	    return projectThresh(th, cache->result);		\
	}							\
} while (0)

#define UPDATE_RGLB_CACHE(n1,n2,th,node) \
do {					\
	cache->f = n1;			\
	cache->g = n2;			\
	cache->thresh = th;		\
	cache->result = node;		\
	COUNT_MISS(rglb);		\
} while (0)

/************************ the cache for complete_or *******************/

#if defined(SHARING)
typedef struct {
    node *f;
    node *g;
    node *prev;
    node *result;
    CACHE_COUNT_MEMBER
} complete_or_cache_entry;

DECLARE_CACHE(complete_or, complete_or_cache_entry);

#define DECLARE_COMPLETE_OR_CACHE_ENTRY complete_or_cache_entry *cache;

#define TRY_COMPLETE_OR_CACHE(n1,n2,pr)				\
do {								\
	if (n2 < n1) { node *temp = (n2); (n2) = (n1); (n1) = temp; }	\
	cache = &complete_or_computed_cache[TERNARY_NODE_HASH(n1,n2,pr)]; \
	if ((cache->f==n1 && cache->g==n2 && cache->prev==pr)) { \
	    COUNT_HIT(complete_or);				\
	    return cache->result;				\
	}							\
} while (0)

#define UPDATE_COMPLETE_OR_CACHE(n1,n2,pr,node) \
do {					\
	cache->f = n1;			\
	cache->g = n2;			\
	cache->prev = pr;		\
	cache->result = node;		\
	COUNT_MISS(complete_or);	\
} while (0)

#endif /* SHARING */

/**************************** the cache for ite_var ***********************/

#define ITE_VAR_COMPUTED_HASH(f,g,h) \
  ((f+INTCAST(g)+(INTCAST(h)<<1)) % COMPUTED_TABLE_SIZE)

typedef struct {
    int f;
    node *g;
    node *h;
    node *result;
    CACHE_COUNT_MEMBER
} ite_var_cache_entry;

DECLARE_CACHE(ite_var, ite_var_cache_entry);

#define DECLARE_ITE_VAR_CACHE_ENTRY ite_var_cache_entry *cache;

#define TRY_ITE_VAR_CACHE(n1,n2,h)				\
do {								\
	cache =							\
	  &ite_var_computed_cache[ITE_VAR_COMPUTED_HASH(n1,n2,h)];\
	if (cache->f==n1 && cache->g==n2 && cache->h==h) {	\
	    COUNT_HIT(ite_var);				\
	    return cache->result;				\
	  }							\
} while (0)

#define UPDATE_ITE_VAR_CACHE(n1,n2,h,node)\
do {					\
	cache->f = n1;			\
	cache->g = n2;			\
	cache->h = h;			\
	cache->result = node;		\
	COUNT_MISS(ite_var);		\
} while (0)



/****************************************************************

			   The Unique Table

 ****************************************************************/


static node *unique_table[UNIQUE_TABLE_SIZE];

#define UNIQUE_HASH(var,tr,fa) \
  (((var)+INTCAST(tr)+(INTCAST(fa)<<1)) % UNIQUE_TABLE_SIZE)



/****************************************************************

				 Prototypes

 ****************************************************************/

node *complete(node *f, node *g);
node *bin_univ(node *f);

static int intcompare(const void *i, const void *j);

/****************************************************************

			 Inline Bit Set Stuff

 ****************************************************************/

/*  next_element()	finds the next element in set beginning with var,
 *			and updates var, word, and mask to refer to it.
 *  prev_element()	finds the next earlier element in set.
 *  next_nonelement()	finds the next potential element of set that is in
 *			fact not an element of set.
 *  prev_nonelement()	finds the next earlier non-element of set.
 *
 * NB:  if the initally supplied element is a member of the set, next_element
 *	and prev_element will happily return that one.  Similarly,
 *	next_nonelement and prev_nonelement will happily return the initial
 *	input if it fits the bill.  That is, these find the next or next
 *	earlier element INLCUDING THE INITIAL ONE that meets the criterion.
 */

#if defined(NO_CHEAP_SHIFT)
__inline int next_element(bitset *set, int *var, int *word, bitmask *mask)
    {
	int vr = *var;
	int wd = *word;
	bitmask f = FOLLOWING_BITS(vr&(BITS_PER_WORD-1));
	bitmask *ptr = &(set->bits[wd]);
	bitmask bits = *ptr;
	bitmask msk = *mask;

	assert(vr >= 0 && vr < MAXVAR);

	if ((bits&f) == 0) {
	    do {
		if (++wd > ((MAXVAR-1)/BITS_PER_WORD)) return FALSE;
	    } while ((bits=*++ptr) == 0);
	    vr = wd<<LOG_BITS_PER_WORD;
	    msk = 1;
	}
	/* I know there's a later bit set in bits, so this is safe */
	while ((bits&msk) == 0) {
	    ++vr;
	    msk <<= 1;
	    assert(vr < (wd+1)<<LOG_BITS_PER_WORD);
	}
	*var = vr;
	*word = wd;
	*mask = msk;
	return TRUE;
    }
#else /* !NO_CHEAP_SHIFT */
__inline int next_element(bitset *set, int *var, int *word, bitmask *mask)
    {
	int vr = *var;
	int wd = *word;
	bitmask *ptr = &(set->bits[wd]);
	bitmask bits = *ptr&FOLLOWING_BITS(vr&(BITS_PER_WORD-1));

	assert(vr >= 0 && vr < MAXVAR);

	while (bits == 0) {
	    if (++wd > (MAXVAR-1)/BITS_PER_WORD) return FALSE;
	    bits = *++ptr;
	}
	vr = wd<<LOG_BITS_PER_WORD;
	/* I know there's a later bit set in bits, so this is safe */
	while ((bits & CHAR_MASK) == 0) {
	    bits >>= CHAR_BIT;
	    vr += CHAR_BIT;
	    assert(vr < (wd+1)<<LOG_BITS_PER_WORD);
	}
	vr += first_one_bit[bits & CHAR_MASK];

	*var = vr;
	*word = wd;
	*mask = 1<<(vr&(BITS_PER_WORD-1));
	return TRUE;
    }
#endif /* NO_CHEAP_SHIFT */


#if defined(NO_CHEAP_SHIFT)
__inline int prev_element(bitset *set, int *var, int *word, bitmask *mask)
    {
	int vr = *var;
	int wd = *word;

	assert(vr >= 0 && vr < MAXVAR);

	bitmask f = PRECEDING_BITS(vr&(BITS_PER_WORD-1));
	bitmask *ptr = &(set->bits[wd]);
	bitmask bits = *ptr;
	bitmask msk = *mask;

	if ((bits&f) == 0) {
	    do {
		if (--wd < 0) return FALSE;
	    } while ((bits=*--ptr) == 0);
	    vr = (wd<<LOG_BITS_PER_WORD) + BITS_PER_WORD-1;
	    msk = 1<<(BITS_PER_WORD-1);
	}
	/* I know there's an earlier bit set in bits, so this is safe */
	while ((bits&msk) == 0) {
	    --vr;
	    msk >>= 1;
	    assert(vr >= 0);
	}
	*var = vr;
	*word = wd;
	*mask = msk;
	return TRUE;
    }
#else /* !NO_CHEAP_SHIFT */
__inline int prev_element(bitset *set, int *var, int *word, bitmask *mask)
    {
	int vr = *var;
	int wd = *word;
	bitmask *ptr = &(set->bits[wd]);
	bitmask bits = *ptr&PRECEDING_BITS(vr&(BITS_PER_WORD-1));
	bitmask temp;

	assert(vr >= 0 && vr < MAXVAR);

	while (bits == 0) {
	    if (--wd < 0) return FALSE;
	    bits = *--ptr;
	}

	vr = BITS_PER_WORD - CHAR_BIT;
	/* I know there's an earlier bit set in bits, so this is safe */
	while ((temp=((bits>>vr)&CHAR_MASK)) == 0) {
	    vr -= CHAR_BIT;
	    assert(vr >= 0);
	}
	vr += (int)last_one_bit[(int)temp];
	vr += (int)wd<<LOG_BITS_PER_WORD;

	*var = vr;
	*word = wd;
	*mask = 1<<(vr&(BITS_PER_WORD-1));
	return TRUE;
    }
#endif /* NO_CHEAP_SHIFT */


__inline int next_nonelement(bitset *set, int *var, int *word, bitmask *mask)
    {
	int vr = *var;
	int wd = *word;
	bitmask f = FOLLOWING_BITS(vr&(BITS_PER_WORD-1));
	bitmask *ptr = &(set->bits[wd]);
	bitmask bits = *ptr;
	bitmask msk = *mask;

	assert(vr >= 0 && vr < MAXVAR);

	if ((bits&f) == f) {
	    do {
		if (++wd >= MAXVAR/BITS_PER_WORD) return FALSE;
	    } while ((bits=*++ptr) == ~0);
	    vr = wd<<LOG_BITS_PER_WORD;
	    msk = 1;
	}
	/* I know there's a later bit clear in bits, so this is safe */
	while ((bits&msk) != 0) {
	    ++vr;
	    msk <<= 1;
	}
	*var = vr;
	*word = wd;
	*mask = msk;
	return TRUE;
    }


__inline int prev_nonelement(bitset *set, int *var, int *word, bitmask *mask)
    {
	int vr = *var;
	    int wd = *word;
	    bitmask f = PRECEDING_BITS(vr&(BITS_PER_WORD-1));
	    bitmask *ptr = &(set->bits[wd]);
	    bitmask bits = *ptr;
	    bitmask msk = *mask;

	assert(vr >= 0 && vr < MAXVAR);

	if ((bits&f) == f) {
	    do {
		if (--wd < 0) return FALSE;
	    } while ((bits=*--ptr) == ~0);
	    vr = (wd<<LOG_BITS_PER_WORD) + BITS_PER_WORD-1;
	    msk = 1<<(BITS_PER_WORD-1);
	}
	/* I know there's an earlier bit clear in bits, so this is safe */
	while ((bits&msk) != 0) {
	    --vr;
	    msk >>= 1;
	}
	*var = vr;
	*word = wd;
	*mask = msk;
	return TRUE;
    }



/* returns 1 if set1 is identical to set2 */
__inline int bitset_equal(bitset *set1, bitset *set2)
    {
	bitmask *ptr1 = &set1->bits[0];
	bitmask *ptr2 = &set2->bits[0];
	bitmask *ptr1end = &set1->bits[((MAXVAR-1)/BITS_PER_WORD)+1];

	for (;;) {
	    if (*ptr1 != *ptr2) return 0;
	    if (++ptr1 >= ptr1end) return 1;
	    ++ptr2;
	}
	
    }

/* returns 1 if 2 sets are disjoint, else 0 */
__inline int bitset_disjoint(bitset *set1, bitset *set2)
    {
	bitmask *ptr1 = &set1->bits[0];
	bitmask *ptr2 = &set2->bits[0];
	bitmask *ptr1end = &set1->bits[((MAXVAR-1)/BITS_PER_WORD)+1];

	for (;;) {
	    if ((*ptr1 & *ptr2) != 0) return 0;
	    if (++ptr1 >= ptr1end) return 1;
	    ++ptr2;
	}
    }


/* returns 1 if set1 is a subset of set2 */
__inline int bitset_subset(bitset *set1, bitset *set2)
    {
	bitmask *ptr1 = &set1->bits[0];
	bitmask *ptr2 = &set2->bits[0];
	bitmask *ptr1end = &set1->bits[((MAXVAR-1)/BITS_PER_WORD)+1];

	for (;;) {
	    if ((*ptr1 | *ptr2) != *ptr2) return 0;
	    if (++ptr1 >= ptr1end) return 1;
	    ++ptr2;
	}
	
    }


/* returns 1 if set1 is a subset of set2 */
__inline int bitset_empty(bitset *set)
    {
	bitmask *ptr = &set->bits[0];
	bitmask *ptrend = &set->bits[((MAXVAR-1)/BITS_PER_WORD)+1];

	for (;;) {
	    if ((*ptr) != 0) return 0;
	    if (++ptr >= ptrend) return 1;
	}
	
    }


/****************************************************************

			     Making Nodes

 ****************************************************************/

static pool *curr_pool = NULL;
static node *curr_pool_ptr = NULL;
static node *curr_pool_end_ptr = NULL;
static int pool_count = 0;

static node *alloc_node(int value, node* tr, node* fa)
    {
	pool *newpool;
	node *n;

	if (curr_pool_ptr >= curr_pool_end_ptr) {
	    /* allocate a new pool */
            newpool = malloc(sizeof(pool));
            newpool->prev = curr_pool;
            curr_pool = newpool;
            curr_pool_ptr = &(newpool->data[0]);
            curr_pool_end_ptr = &(newpool->data[POOL_SIZE]);
            ++pool_count;
        }
	n = curr_pool_ptr++;
        n->value = value;
        n->tr = tr;
        n->fa = fa;
        return n;
    }

/* return the number of graph nodes that have been created. */
int nodes_in_use(void)
    {
        return pool_count*POOL_SIZE - (curr_pool_end_ptr - curr_pool_ptr);
    }


DECLARE_FN_COUNT(make_node)

node *make_node(int var, node *tr, node *fa)
    {
	node **bucket;
	node *ptr;
#ifdef VERBOSE_DEBUG
    QP_printf("make_node(%d, ", var);
    printOut(tr);
    QP_printf(", ");
    printOut(fa);
    QP_printf(")\n");
    QP_flush(QP_curout);
#endif

	assert(var>=0);
	assert(var<MAXVAR);
	assert(IS_TERMINAL(tr) || tr->value > var);
	assert(IS_TERMINAL(fa) || fa->value > var);

	COUNT_FN(make_node);

	if (tr == fa) return tr;

	bucket = &unique_table[UNIQUE_HASH(var,tr,fa)];
	ptr = *bucket;
	while (ptr!=NULL && (var!=ptr->value || tr!=ptr->tr || fa!=ptr->fa))
	    ptr = ptr->unique;

	if (ptr!=NULL) {
	    COUNT_UNIQUE_HIT;
	    return ptr;
	}

	/* node doesn't exist so create it and put in bucket */
	COUNT_UNIQUE_MISS;
	ptr = alloc_node(var, tr, fa);
	ptr->unique = *bucket;
	*bucket = ptr;
	return ptr;
    }


void free_rep(node *n)
    {
	/* never free ROBDD nodes */
    }



/****************************************************************

			    The Basic Algorithms

 ****************************************************************/

int max_variable(void)
    {
	return MAXVAR;
    }


node *trueVar(void)
    {
	return one;
    }


node *falseVar(void)
    {
	return zero;
    }
    

DECLARE_FN_COUNT(variableRep)

node *variableRep(int var)
    {
	COUNT_FN(variableRep);
	assert(var >= 0);
	return make_node(var,one,zero);
    }


DECLARE_FN_COUNT(ite)

/* Compute (f \wedge g) \vee (\neg f \wedge h) */
node *ite(node *f,node *g,node *h)
    {
	node *f_tr, *f_fa;
	node *g_tr, *g_fa;
	node *h_tr, *h_fa;
	node *newnode;
	int top;
	DECLARE_ITE_CACHE_ENTRY

	COUNT_FN(ite);
	/* terminal cases */
	if (f == one)
	  return g;
	if (f == zero)
	  return h;
	if ((g == one) && (h == zero))
	  return f;
	if (g == h)
	  return g;

	/* look it up in computed table; finished if found */
	TRY_ITE_CACHE(f, g, h, ite);

	/* find top variable */
	top = f->value;		/* we know f is not terminal */
	if (!IS_TERMINAL(g) && (g->value < top)) top = g->value;
	if (!IS_TERMINAL(h) && (h->value < top)) top = h->value;
	
	/* find then and else branches for recursive calls */
	if (f->value==top) {
	    f_tr = f->tr; f_fa = f->fa;
	} else {
	    f_tr = f; f_fa = f;
	}
	if (!IS_TERMINAL(g) && g->value==top) {
	    g_tr = g->tr; g_fa = g->fa;
	} else {
	    g_tr = g; g_fa = g;
	}
	if (!IS_TERMINAL(h) && h->value==top) {
	    h_tr = h->tr; h_fa = h->fa;
	} else {
	    h_tr = h; h_fa = h;
	}
	
	/* create new node and add to table */
	newnode = make_node(top,
			    ite(f_tr, g_tr, h_tr),
			    ite(f_fa, g_fa, h_fa));
	UPDATE_ITE_CACHE(f,g,h,newnode,ite);
	return newnode;
    }


#if defined(USE_ITE_CONSTANT)

/* This is sort of an "approximate ite()."  It returns zero or one if
 * that's what ite() would do.  Otherwise it just returns the
 * pseudo-node `nonterminal' or some real node.  In any case, it does
 * not create any new nodes.
 */
node *ite_constant(node *f,node *g,node *h)
    {
	node *f_tr, *f_fa;
	node *g_tr, *g_fa;
	node *h_tr, *h_fa;
	node *tr_part, *fa_part;
	node *result;
	int top;
	DECLARE_ITE_CACHE_ENTRY

	COUNT_FN(ite);
	/* terminal cases */
	if (f == one)
	    return g;
	if (f == zero)
	    return h;
	if (g == h)
	    return g;
	if (IS_TERMINAL(g) && IS_TERMINAL(h))
	    /* either f or ~f, which is nonterminal since f is */
	    return nonterminal;

	/* look it up in computed table; finished if found */
	TRY_ITE_CACHE(f, g, h, ite_constant);

	/* find top variable */
	top = f->value;		/* we know f is not terminal */
	if (!IS_TERMINAL(g) && (g->value < top)) top = g->value;
	if (!IS_TERMINAL(h) && (h->value < top)) top = h->value;
	
	/* find then and else branches for recursive calls */
	if (f->value==top) {
	    f_tr = f->tr; f_fa = f->fa;
	} else {
	    f_tr = f; f_fa = f;
	}
	if (!IS_TERMINAL(g) && g->value==top) {
	    g_tr = g->tr; g_fa = g->fa;
	} else {
	    g_tr = g; g_fa = g;
	}
	if (!IS_TERMINAL(h) && h->value==top) {
	    h_tr = h->tr; h_fa = h->fa;
	} else {
	    h_tr = h; h_fa = h;
	}
	
	tr_part = ite_constant(f_tr, g_tr, h_tr);
	fa_part = ite_constant(f_fa, g_fa, h_fa);
	if (tr_part == fa_part) {
	    result = tr_part;
	} else {
	    result = nonterminal;
	}

	UPDATE_ITE_CACHE(f,g,h,result,ite_constant);
	return result;
    }
#endif /* USE_ITE_CONSTANT */


DECLARE_FN_COUNT(implies)

node *implies(node *a, node *b)
    {
	COUNT_FN(implies);
	return ite(a,b,one);
    }


node *copy(node *a)        /* returns a copy of graph a */
    {
        return a;
    }


int equiv(node *a, node *b)	/* returns true if graph a and b are equiv */
    {
	return (a == b);
    }


DECLARE_FN_COUNT(lub)
DECLARE_FN_COUNT(glb)

node *lub(node *f, node *g)
    {
	COUNT_FN(lub);
	if (IS_TERMINAL(f)) {
	    return f == one ? one : g;
	} else if (IS_TERMINAL(g)) {
	    return g == one ? one : f;
	} ELSE_TRY_EQUAL_TEST(f,g,f)
	else {
	    node *result;
	    DECLARE_BIN_CACHE_ENTRY

	    TRY_BIN_CACHE(f, g, lub);

	    if (f->value < g->value) {
		result = make_node(f->value, lub(f->tr, g), lub(f->fa, g));
	    } else if (f->value > g->value) {
		result = make_node(g->value, lub(f, g->tr), lub(f, g->fa));
	    } else /* f->value == g->value */{
		result = make_node(f->value,
				   lub(f->tr, g->tr),
				   lub(f->fa, g->fa));
	    }
	    UPDATE_BIN_CACHE(f, g, result, lub);
	    return result;
	}
    }


node *glb(node *f, node *g)
    {
	COUNT_FN(glb);
	if (IS_TERMINAL(f)) {
	    return f == one ? g : zero;
	} else if (IS_TERMINAL(g)) {
	    return g == one ? f : zero;
	} ELSE_TRY_EQUAL_TEST(f,g,f)
	else {
	    node *result;
	    DECLARE_BIN_CACHE_ENTRY

	    TRY_BIN_CACHE(f, g, glb);

	    if (f->value < g->value) {
		result = make_node(f->value, glb(f->tr, g), glb(f->fa, g));
	    } else if (f->value > g->value) {
		result = make_node(g->value, glb(f, g->tr), glb(f, g->fa));
	    } else /* f->value == g->value */{
		result = make_node(f->value,
				   glb(f->tr, g->tr),
				   glb(f->fa, g->fa));
	    }
	    UPDATE_BIN_CACHE(f, g, result, glb);
	    return result;
	}
    }


node *glb_array(int n, int arr[])
    {
	int i;
	node *result = one;

	qsort((char *)arr, n, sizeof(int), intcompare);
	for (i=n-1; i>=0; --i) {
	    assert(arr[i] >= 0);
	    result = make_node(arr[i], result, zero);
	}
	return result;
    }

/****************************************************************

	 Projection (Projection, Existential Quantification)

 ****************************************************************/

DECLARE_FN_COUNT(project)

/* projects c in f. */
node *project(int c, node *f)
    {
	COUNT_FN(project);

	assert(c >= 0);

        if (IS_TERMINAL(f) || (f->value > c)) {
	    return f;
        } else if (f->value < c) {
	    return make_node(f->value, project(c,f->tr), project(c,f->fa));
        } else {
	    return lub(f->tr, f->fa);
	}
    }


DECLARE_FN_COUNT(projectThresh)

node *projectThresh(int thresh, node *f)
    {
	/* projects all variables greater than thresh. */

	COUNT_FN(projectThresh);
	if (IS_TERMINAL(f)) {
	    return f;
	} else if (f->value <= thresh) {
	    return make_node(f->value,
			     projectThresh(thresh, f->tr),
			     projectThresh(thresh, f->fa));
	} else {
	    return one;
	}
    }

static int exists_glb(node *f, node *g)
    {
	if (f == zero) {
	    return FALSE;
	} else if (g == zero) {
	    return FALSE;
	} else if (f == one) {
	    /* since we know that g != zero... */
	    return TRUE;
	} else if (g == one) {
	    /* likewise f... */
	    return TRUE;
	} else if (f->value < g->value) {
	    return exists_glb(f->tr, g) || exists_glb(f->fa, g);
	} else if (f->value > g->value) {
	    return exists_glb(f, g->tr) || exists_glb(f, g->fa);
	} else {
	    return exists_glb(f->tr, g->tr) || exists_glb(f->fa, g->fa);
	}
    }


node *projected_glb(int c, node *f, node *g)
    {
	if (IS_TERMINAL(f)) {
	    return (f == one) ? projectThresh(c, g) : zero;
	} else if (IS_TERMINAL(g)) {
	    return (g == one) ? projectThresh(c, f) : zero;
	} ELSE_TRY_EQUAL_TEST(f,g,projectThresh(c, f))
	else {
	    int v;
	    node *tr1, *tr2, *fa1, *fa2;
	    node *result;

	    DECLARE_RGLB_CACHE_ENTRY

	    TRY_RGLB_CACHE(f, g, c);

	    if (f->value < g->value) {
		v = f->value;
		tr1 = f->tr;
		tr2 = g;
		fa1 = f->fa;
		fa2 = g;
	    } else if (f->value > g->value) {
		v = g->value;
		tr1 = f;
		tr2 = g->tr;
		fa1 = f;
		fa2 = g->fa;
	    } else /* f->value == g->value */{
		v = f->value;
		tr1 = f->tr;
		tr2 = g->tr;
		fa1 = f->fa;
		fa2 = g->fa;
	    }
	    if (v > c) {
		result =
		  (exists_glb(tr1,tr2)||exists_glb(fa1,fa2)) ? one : zero;
	    } else {
		result = make_node(v,
				   projected_glb(c, tr1, tr2),
				   projected_glb(c, fa1, fa2));
	    }
	    UPDATE_RGLB_CACHE(f,g,c,result);
	    return result;
	}
    }
    

node *rename_projected_glb(int c, node *f, node *g, int rename_result[])
    {
	if (IS_TERMINAL(f)) {
          return (f == one) ? rename_project(g, rename_result, c) : zero;
	} else if (IS_TERMINAL(g)) {
	    return (g == one) ? rename_project(f, rename_result, c) : zero;
	} ELSE_TRY_EQUAL_TEST(f,g,projectThresh(c, f))
	else {
	    int v;
	    node *tr1, *tr2, *fa1, *fa2;
	    node *result;

	    DECLARE_RENAME_PGLB_CACHE_ENTRY

	    TRY_RENAME_PGLB_CACHE(f,g, oae_accumulator);

	    if (f->value < g->value) {
		v = f->value;
		tr1 = f->tr;
		tr2 = g;
		fa1 = f->fa;
		fa2 = g;
	    } else if (f->value > g->value) {
		v = g->value;
		tr1 = f;
		tr2 = g->tr;
		fa1 = f;
		fa2 = g->fa;
	    } else /* f->value == g->value */{
		v = f->value;
		tr1 = f->tr;
		tr2 = g->tr;
		fa1 = f->fa;
		fa2 = g->fa;
	    }
	    if (v > c) {
		result =
		  (exists_glb(tr1,tr2)||exists_glb(fa1,fa2)) ? one : zero;
	    } else {
	    	node * tr = rename_projected_glb(c, tr1, tr2, rename_result);
		node * fa = rename_projected_glb(c, fa1, fa2, rename_result);

	    	if (rename_result[v] < 0) {
		    result = lub(tr, fa);
		} else {
		    assert(rename_result[v] >= 0);
		    result = make_node(rename_result[v], tr, fa);
		}
	    }

	    UPDATE_RENAME_PGLB_CACHE(f, g, result, oae_accumulator);

	    return result;
	}
    }


/* 
 * node * project(node * f, int thresh, int * rename_result)
 * {
 * 	if (IS_TERMINAL(f)) {
 * 		return f;
 * 	} else if (f -> value > thresh) {
 * 		return one;
 * 	} else {
 * 		node * tr = project(f -> tr, thresh, rename_result);
 * 		node * fa = project(f -> fa, thresh, rename_result);
 * 
 * 		if (rename_result[f -> value] < 0)
 * 			return lub(tr, fa);
 * 		else
 * 			return make_node(f -> value, tr, fa);
 * 	}
 * }
 */

node * rename_project(node * f, int * rename_result, int thresh)
{
	if (IS_TERMINAL(f)) {
		return f;
	} else if (f -> value > thresh) {
		return one;
	} else {
		node * tr = rename_project(f -> tr, rename_result, thresh);
		node * fa = rename_project(f -> fa, rename_result, thresh);

		if (rename_result[f -> value] < 0) {
		    return lub(tr, fa);
		} else {
		    assert(rename_result[f->value] >= 0);
		    return make_node(rename_result[f -> value], tr, fa);
		}
	}

	/*
		else if (f -> value > higher) {
		return f;
	}
	*/
}

/* Should do caching here... */

node * project_rename(node * g, int * rename_g, int * rename_result, int thresh)
{
	if (IS_TERMINAL(g)) {
		return g;
	} else if (rename_g[g -> value] > thresh) {
		return one;
	} else {
		int g_val = rename_g[g -> value];
		node * tr = project_rename(g -> tr, rename_g, rename_result,
			thresh);
		node * fa = project_rename(g -> fa, rename_g, rename_result,
			thresh);

		if (rename_result[g_val] < 0) {
		    return lub(tr, fa);
		} else {
		    assert(g_val >= 0);
		    return make_node(g_val, tr, fa);
		}
	}
}

/* Should do caching here... */

node * rename_project_rename(node * g, int * rename_g, int * rename_result,
	int thresh)
{
	if (IS_TERMINAL(g)) {
		return g;
	} else if (rename_g[g -> value] > thresh) {
		return one;
	} else {
		int g_val = rename_g[g -> value];
		node * tr = rename_project_rename(g -> tr, rename_g,
			rename_result, thresh);
		node * fa = rename_project_rename(g -> fa, rename_g,
			rename_result, thresh);

		if (rename_result[g_val] < 0) {
		    return lub(tr, fa);
		} else {
		    assert(rename_result[g_val] >= 0);
		    return make_node(rename_result[g_val], tr, fa);
		}
	}
}

node * project_glb_rename(int thresh, node * f, node * g, int * rename_g,
	int * rename_result)
{
#ifdef VERBOSE_DEBUG
    static int depth = 0;
    int i;
    char c;
#endif

	if (IS_TERMINAL(f)) {
		return (f == one) ? project_rename(g, rename_g, rename_result,
			thresh) : zero;
	}
	if (IS_TERMINAL(g)) {
		return (g == one) ?
                  rename_project(f, rename_result, thresh) : zero;
	} /* else f and g cannot be equal!!! */
	else {
		int v;
		node *tr1, *tr2, *fa1, *fa2;
		int g_val = rename_g[g -> value];
		node * result;

		DECLARE_RENAME_PGLB_CACHE_ENTRY

		TRY_RENAME_PGLB_CACHE(f, g, oae_accumulator);

#ifdef VERBOSE_DEBUG
    ++depth;
    for (i=0; i<depth; ++i) QP_printf("  ");
    QP_printf("pgr(%d, ", thresh);
    printOut(f);
    QP_printf(", ");
    printOut(g);
    QP_printf(", ");
    for(i=1, c='['; i<=thresh && rename_g[i]>=0; ++i) {
	QP_printf("%c%d", c, rename_g[i]);
	c=',';
    }
    QP_printf("], ");
    for(i=1, c='{'; i<=thresh && rename_result[i]<=thresh; ++i) {
	if (rename_result[i]>=0) {
	    QP_printf("%c%d", c, rename_result[i]);
	    c=',';
	}
    }
    if (c=='{') QP_printf("{");
    QP_printf("} )\n");
    QP_flush(QP_curout);
#endif

		/* do caching */
		if (f -> value < g_val) {
			v = f -> value;
			tr1 = f -> tr;
			tr2 = g;
			fa1 = f -> fa;
			fa2 = g;
		}
		else if (f -> value > g_val) {
			v = g_val;
			tr1 = f;
			tr2 = g -> tr;
			fa1 = f;
			fa2 = g -> fa;
		}
		else {
			v = g_val;
			tr1 = f -> tr;
			tr2 = g -> tr;
			fa1 = f -> fa;
			fa2 = g -> fa;
 		}

		if (v > thresh) {
			result = (exists_glb_rename(tr1, tr2, rename_g) ||
				  exists_glb_rename(fa1, fa2, rename_g)) ?
			    one : zero ;
		} else {
			node * tr = project_glb_rename(thresh, tr1, tr2,
				rename_g, rename_result);
			node * fa = project_glb_rename(thresh, fa1, fa2,
				rename_g, rename_result);

			if (rename_result[v] < 0)
				result = lub(tr, fa);
			else
#ifdef VERBOSE_DEBUG
    for (i=0; i<depth; ++i) QP_printf("  ");
    QP_printf("make_node(%d, ", v);
    printOut(tr);
    QP_printf(", ");
    printOut(fa);
    QP_printf(")\n");
    QP_flush(QP_curout);
#endif
				result = make_node(v, tr, fa);

			UPDATE_RENAME_PGLB_CACHE(f, g, result, oae_accumulator);
		}
#ifdef VERBOSE_DEBUG
    for (i=0; i<depth; ++i) QP_printf("  ");
    QP_printf("= ");
    printOut(result);
    QP_printf("\n");
    QP_flush(QP_curout);
    depth--;
#endif
		return result;

	}
}

node * rename_project_glb_rename(int thresh, node * f, node * g, int * rename_g,
	int * rename_result)
{
#ifdef VERBOSE_DEBUG
    static int depth = 0;
    int i;
    char c;
#endif
	if (IS_TERMINAL(f)) {
		return (f == one) ? rename_project_rename(g, rename_g,
			rename_result, thresh) : zero;
	}
	if (IS_TERMINAL(g)) {
		return (g == one) ? rename_project(f, rename_result, thresh) :
			zero;
	} /* else f and g cannot be equal!!! */
	else {
		int v;
		node *tr1, *tr2, *fa1, *fa2;
		int g_val = rename_g[g -> value];
		node * result;

		DECLARE_RENAME_PGLB_CACHE_ENTRY

		TRY_RENAME_PGLB_CACHE(f, g, oae_accumulator);

#ifdef VERBOSE_DEBUG
    ++depth;
    for (i=0; i<depth; ++i) QP_printf("  ");
    QP_printf("rpgr(%d, ", thresh);
    printOut(f);
    QP_printf(", ");
    printOut(g);
    QP_printf(", ");
    for(i=1, c='['; i<=thresh; ++i) {
	QP_printf("%c%d", c, rename_g[i]);
	c=',';
    }
    QP_printf("], ");
    for(i=1, c='['; i<=thresh; ++i) {
	QP_printf("%c%d", c, rename_result[i]);
	c=',';
    }
    if (c=='[') QP_printf("[");
    QP_printf("] )\n");
    QP_flush(QP_curout);
#endif

		/* do caching */
		if (f -> value < g_val) {
			v = f -> value;
			tr1 = f -> tr;
			tr2 = g;
			fa1 = f -> fa;
			fa2 = g;
		}
		else if (f -> value > g_val) {
			v = g_val;
			tr1 = f;
			tr2 = g -> tr;
			fa1 = f;
			fa2 = g -> fa;
		}
		else {
			v = g_val;
			tr1 = f -> tr;
			tr2 = g -> tr;
			fa1 = f -> fa;
			fa2 = g -> fa;
		}

		if (v > thresh) {
		    result = (exists_glb_rename(tr1, tr2, rename_g) ||
			      exists_glb_rename(fa1, fa2, rename_g)) ?
			     one : zero;
		} else {
			node * tr = rename_project_glb_rename(thresh, tr1, tr2,
				rename_g, rename_result);
			node * fa = rename_project_glb_rename(thresh, fa1, fa2,
				  rename_g, rename_result);

			if (rename_result[v] < 0) {
			    result = lub(tr, fa);
			} else {
#ifdef VERBOSE_DEBUG
    for (i=0; i<depth; ++i) QP_printf("  ");
    QP_printf("make_node(%d, ", v);
    printOut(tr);
    QP_printf(", ");
    printOut(fa);
    QP_printf(")\n");
    QP_flush(QP_curout);
#endif
			    result = make_node(rename_result[v], tr, fa);
			}

			UPDATE_RENAME_PGLB_CACHE(f, g, result, oae_accumulator);
		}

#ifdef VERBOSE_DEBUG
    for (i=0; i<depth; ++i) QP_printf("  ");
    QP_printf("= ");
    printOut(result);
    QP_printf("\n");
    QP_flush(QP_curout);
    depth--;
#endif
		return result;

	}

}


/****************************************************************

			       Renaming

 ****************************************************************/

DECLARE_FN_COUNT(renameArray)
DECLARE_FN_COUNT(reverseRenameArray)

DECLARE_FN_COUNT(ite_var)

/* A special case version of ite_var, where we know that f < g->value */
static node *ite_var_g(int f, node *g, node *h)
    {
	COUNT_FN(ite_var);

        assert(IS_TERMINAL(g) || f < g->value);

	if (IS_TERMINAL(h) || f < h->value) {
	    /* f < g && f < h */
	    return make_node(f, g, h);
	} else if (f == h->value) {
	    return make_node(f, g, h->fa);
	} else {
	    /* h < f < g */
	    node *result;
	    DECLARE_ITE_VAR_CACHE_ENTRY

	    TRY_ITE_VAR_CACHE(f, g, h);

	    result = make_node(h->value,
			       ite_var_g(f, g, h->tr),
			       ite_var_g(f, g, h->fa));
	    UPDATE_ITE_VAR_CACHE(f, g, h, result);
	    return result;
	}
    }


/* A special case version of ite_var, where we know that f < h->value */
static node *ite_var_h(int f, node *g, node *h)
    {
	COUNT_FN(ite_var);

        assert(IS_TERMINAL(h) || f < h->value);

	if (IS_TERMINAL(g) || f < g->value) {
	    /* f < g && f < h */
	    return make_node(f, g, h);
	} else if (f == g->value) {
	    return make_node(f, g->tr, h);
	} else {
	    /* h < f < g */
	    node *result;
	    DECLARE_ITE_VAR_CACHE_ENTRY

	    TRY_ITE_VAR_CACHE(f, g, h);
	    result = make_node(g->value,
			       ite_var_h(f, g->tr, h),
			       ite_var_h(f, g->fa, h));
	    UPDATE_ITE_VAR_CACHE(f, g, h, result);
	    return result;
	}
    }


/* A special case version of ite, where we know that !IS_TERMINAL(f) &&
 * f->tr == one && f->fa == zero.  In fact, we refine this further and
 * make f be just (what would have been) f->value.
 *
 * Recall the code for ite:  it finds the minimum value of f, g, and h (call
 * it top), and sets each of ft, gt and ht to the tr branch of the
 * corresponding arg if its value == top, else sets it to the arg itself, and
 * correspondingly for ff, gf, and hf.  Then the value of ite is:
 *
 *    mn(top, i(ft, gt, ht), i(ff, gf, hf))
 *
 * (abbreviating make_node as mn and ite as i).  Given this, we can simplify
 * things in several cases.  Here are all the cases, and the simplified value.
 * (We introduce ig for ite_var_g and ih for ite_var_h as special cases where
 * we know f < g or f < h, respectively.)
 *
 * a)	f = g = h    (1)   mn(f, g->tr, h->fa) *** Impossible
 *
 * b)	f < g < h    (2)   mn(f, g, h)
 * c)	f < g = h    (2)   mn(f, g, h)
 * d)	f < h < g    (2)   mn(f, g, h)
 * e)	f = g < h    (3)   mn(f, g->tr, h) *** Impossible
 * f)	f = h < g    (4)   mn(f, g, h->fa) *** Impossible
 *
 * g)	g < f < h    (5)   mn(gv, ih(f, g->tr, h), ih(f, g->fa, h))
 * h)	g < f = h    (6)   mn(gv, i(f, g->tr, h), i(f, g->fa, h)) *** Impossible
 * i)	g < h < f    (6)   mn(gv, i(f, g->tr, h), i(f, g->fa, h))
 * j)	g = h < f    (7)   mn(gv, i(f, g->tr, h->tr), i(f, g->fa, h->fa))
 *
 * k)	h < f < g    (8)   mn(hv, ig(f, g, h->tr), ig(f, g, h->fa))
 * l)	h < f = g    (9)   mn(hv, i(f, g, h->tr), i(f, g, h->fa)) *** Impossible
 * m)	h < g < f    (9)   mn(hv, i(f, g, h->tr), i(f, g, h->fa))
 */	    

node *ite_var(int f,node *g,node *h)
    {
	int g_val = MAXVAR;
	int h_val = MAXVAR;
	node *result;
	DECLARE_ITE_VAR_CACHE_ENTRY

	COUNT_FN(ite_var);
	DIRECT_EQUAL_TEST(g,h,g);
	TRY_ITE_VAR_CACHE(f, g, h);

	if (!IS_TERMINAL(g)) g_val = g->value;
	if (!IS_TERMINAL(h)) h_val = h->value;

	if (f < g_val) {
	    if (f < h_val) /* case 2 (b,c,d):  f < g && f < h */ {
		result = make_node(f, g, h);
	    } else /* case 8 (k):  h < f < g */ {
		result = make_node(h_val,
				   ite_var_g(f, g, h->tr),
				   ite_var_g(f, g, h->fa));
	    }
	/* g < f */
	} else if (f < h_val) /* g < f < h */ {
	    result = make_node(g_val,
			       ite_var_h(f, g->tr, h),
			       ite_var_h(f, g->fa, h));
	/* g < f && h < f */
	} else if (h_val < g_val) /* case 9 (l,m): h < g < f */ {
	    result = make_node(h_val,
			       ite_var(f, g, h->tr),
			       ite_var(f, g, h->fa));
	/* g < f && g <= h */
	} else if (g_val == h_val) /* g == h < f */ {
	    result = make_node(g_val,
			       ite_var(f, g->tr, h->tr),
			       ite_var(f, g->fa, h->fa));
	} else /* case 6 (h,i):  g < h < f */ {
	    result = make_node(g_val,
			       ite_var(f, g->tr, h),
			       ite_var(f, g->fa, h));
	}

	UPDATE_ITE_VAR_CACHE(f, g, h, result);
	return result;
    }

node *renameArray(node *f, int count, int mapping[])
    {
	int newval;

	COUNT_FN(renameArray);
	if (IS_TERMINAL(f)) {
	    return f;
	} else if (f->value > count) {
	    return one;
	} else if (UNUSED_MAPPING==(newval=mapping[f->value])) {
	    return lub(renameArray(f->tr, count, mapping),
		       renameArray(f->fa, count, mapping));
	} else {
	    return ite_var(newval,
			   renameArray(f->tr, count, mapping),
			   renameArray(f->fa, count, mapping));
	}
    }

node *reverseRenameArray(node *f, int count, int mapping[])
    {
	int i, val, max;
	int rev_map[MAXVAR];

	COUNT_FN(reverseRenameArray);
	/* NB:  four -1 bytes is the same as a -1 word */
	memset(rev_map, -1, sizeof(rev_map));
	for (i=1,max=-1; i<=count; ++i) {
	    rev_map[(val=mapping[i])] = i;
	    if (max < val) max = val;
	}

	return renameArray(f, max, rev_map);
    }


node * renaming(node * f, int * rename_f)
{
	if (IS_TERMINAL(f)) {
		return f;
	} else {
		return make_node(rename_f[f -> value],
			renaming(f -> tr, rename_f),
			renaming(f -> fa, rename_f));
	}
}

node * glb_rename(node * f, node * g, int * rename_g)
{

	if (IS_TERMINAL(f)) {
		return f == one ? renaming(g, rename_g) : zero;
	} else if (IS_TERMINAL(g)) {
		return g == one ? f : zero;
	} else {
		int g_val = rename_g[g -> value];

		if (f -> value < g_val) {
			return make_node(f -> value,
				glb_rename(f -> tr, g, rename_g),
				glb_rename(f -> fa, g, rename_g));
		} else if (f -> value > g_val) {
			return make_node(g_val,
				glb_rename(f, g -> tr, rename_g),
				glb_rename(f, g -> fa, rename_g));
		} else {
			return make_node(f -> value,
				glb_rename(f -> tr, g -> tr, rename_g),
				glb_rename(f -> fa, g -> fa, rename_g));
		}
	}
}

int exists_glb_rename(node * f, node * g, int * rename_g)
{
	if (f == zero) {
		return FALSE;
	} else if (g == zero) {
		return FALSE;
	} else if (f == one || g == one) {
		return TRUE;
	} else if (f -> value < rename_g[g -> value]) {
		return (exists_glb_rename(f -> tr, g, rename_g) ||
			exists_glb_rename(f -> fa, g, rename_g));
	} else if (f -> value > rename_g[g -> value]) {
		return (exists_glb_rename(f, g -> tr, rename_g) ||
			exists_glb_rename(f, g -> fa, rename_g));
	} else {
		return (exists_glb_rename(f -> tr, g -> tr, rename_g) ||
			exists_glb_rename(f -> fa, g -> fa, rename_g));
	}
}


/****************************************************************

	 Abstract Exit (renaming + conjunction + projection)

 ****************************************************************/

static int array_is_ordered(int array[], int size)
    {
	int i;
	int prev = array[1];

	for (i=2; i<=size; ++i) {
	    if (array[i] >= 0) { /* ignore negative elements */
		if (array[i] <= prev) return 0;
		prev = array[i];
	    }
	}
	return 1;
    }
    


node *abstract_exit(node *context, node *f, int count, int mapping[],
		    int thresh, int rename_result[])
    {
    	oae_accumulator++;
	if (array_is_ordered(mapping, count)) {
	    return project_glb_rename(thresh, context, f, mapping,
				      rename_result);
	} else {
	    return project_glb_rename(thresh, context,
				      renameArray(f, count, mapping),
				      identity_renaming, rename_result);
	}
    }

node *final_abstract_exit(node *context, node *f, int count, int mapping[],
	int thresh, int rename_result[])
    {
	int unrenamed_thresh = rename_result[thresh];
	int rev_map[MAXVAR];
	int i;

	/* NB:  four -1 bytes is the same as a -1 word */
	memset(rev_map, -1, sizeof(int)*unrenamed_thresh);
	for (i=1; i<=count; ++i) { rev_map[rename_result[i]] = i; }

    	oae_accumulator++;
	if (array_is_ordered(mapping, count)) {
	    if (array_is_ordered(rename_result, thresh)) {
		return rename_project_glb_rename(unrenamed_thresh, context, f, mapping,
						 rev_map);
	    } else {
	    return renameArray(project_glb_rename(unrenamed_thresh, context, f, mapping,
						  rev_map),
			       unrenamed_thresh, rev_map);
	    }
	} else {
	    if (array_is_ordered(rename_result, thresh)) {
		return rename_project_glb_rename(unrenamed_thresh, context,
						 renameArray(f, count,
							     mapping),
						 identity_renaming,
						 rev_map);
	    } else {
		return renameArray(project_glb_rename(unrenamed_thresh, context,
						      renameArray(f, count,
								  mapping),
						      identity_renaming,
						      rev_map),
				   unrenamed_thresh, rev_map);
	    }
	}
    }


/* =================================================== */

/*
	ordered_abstract_exit()

	-----------------------

	thresh --> threshold projection
	f --> the ROBDD of the current iteration, but not including the
		Abstract Exit of the previous iteration
	g --> the ROBDD which is the abstract exit of the previous iteration.
	rename_g[] --> ARRAY, index is argument position, output is the value
	rename_result[] --> ARRAY, index is value, output is argument
		position. < 0 infer project away

	hence, ordered_abstract_exit() will do a conjunction of:
		f AND g:
			check if g is in order, if it is, we cheat
			if it is not, we can't cheat

*/

node * naive_ordered_abstract_exit(int thresh, node * f, node * g, int * rename_g,
	int * rename_result)
{
	return naive_project_glb_rename(thresh, f, g, rename_g, rename_result);
}

node * naive_project_glb_rename(int thresh, node * f, node * g,
	int * rename_g, int * rename_result)
{
	return rename_project(glb(f, renaming(g, rename_g)),
                              rename_result, thresh);

}

node * naive_rename_project_glb_rename(int thresh, node * f, node * g,
	int * rename_g, int * rename_result)
{
	return renaming(rename_project(glb(f, renaming(g, rename_g)),
		rename_result, thresh), rename_result);
}


node * ordered_abstract_exit(int thresh, node * f, node * g, int * rename_g,
	int * rename_result)
{
	oae_accumulator++;
	return project_glb_rename(thresh, f, g, rename_g, rename_result);
}

node * final_ordered_abstract_exit(int thresh, node * f, node * g,
	int * rename_g, int * rename_result)
{
	oae_accumulator++;
	return rename_project_glb_rename(thresh, f, g, rename_g, rename_result);
}



/****************************************************************

			 Abstract Unification

 ****************************************************************/

/* NB:  iff_conj_array, and so all the functions that call it, now
 * allow v0 to apear in the array of variables arr[].  This makes the
 * analyzer robust for handling goals like X = f(X,Y).  Since it's
 * cheaper and easier to check this in C, I do it here.
 */

#if defined(ELIM_DUPS)
#define DECLARE_PREV(init) int prev = (init);
#define REDECLARE_PREV(init) prev = (init);
#define HANDLE_DUP(this,rel)	\
	if ((this) == prev) continue;	\
	assert((this) rel prev);	\
	prev = (this);
#elif !defined(NDEBUG)
#define DECLARE_PREV(init) int prev = (init);
#define REDECLARE_PREV(init) prev = (init);
#define HANDLE_DUP(this,rel)	\
	assert((this) != prev);	\
	assert((this) rel prev);	\
	prev = (this);
#else
#define DECLARE_PREV(init)
#define REDECLARE_PREV(init)
#define HANDLE_DUP(this,rel)
#endif


DECLARE_FN_COUNT(iff_conj)


node *iff_conj_array(int v0, int n, int arr[])
    {
	node *thens = one, *elses = zero;
	int *ptr;
	int vi = 0;	/* this value doesn't matter */
	DECLARE_PREV(MAXVAR)

	COUNT_FN(iff_conj);
	/* first build part of graph below v0.  For this, we build two
	 * subgraphs:  one for when v0 is true, and one for false.
	 * These are called thens and elses.
	 */

	for (ptr=&arr[n-1]; ptr>=arr && v0<(vi=*ptr); --ptr) {
	    HANDLE_DUP(vi, <)
	    thens = make_node(vi, thens, zero);
	    elses = make_node(vi, elses, one);
	}

	if (v0 == vi) --ptr;

	/* make v0 node */
	thens = make_node(v0,thens,elses);

	/* Finally build part of graph above v0.  For this, we build
	 * only one graph, whose then branch is the graph we've built
	 * so far and whose else branch is ~v0.
	 */

	if (ptr >= arr) {
	    REDECLARE_PREV(MAXVAR)
	    /* make ~v0 */
	    elses = make_node(v0,zero,one);

	    do {
		vi = *ptr;
		HANDLE_DUP(vi, <)
		thens = make_node(vi, thens, elses);
	    } while (--ptr >= arr);
	}

	return thens;
    }


#if 0
node *projected_iff_conj_array(int v0, int n, int arr[], int thresh)
    {
	if (v0 > thresh) return one;
	while (--n>=0 && arr[n]>thresh);
	return iff_conj_array(v0, n+1, arr);
    }
#endif


#if 1
node *abstract_unify(node *f, int v0, int n, int arr[], int thresh)
    {
	return projected_glb(thresh, f, iff_conj_array(v0, n, arr));
    }
#else
node *abstract_unify(node *f, int v0, int n, int arr[],
		     int thresh, int proj[])
    {
	return project_glb_rename(thresh, f, iff_conj_array(v0, n, arr),
				  identity_renaming, proj);
    }
#endif

/****************************************************************

		      Finding Entailed Variables

 ****************************************************************/

/* returns TRUE iff var is implied by the boolean function specified by f */
int var_entailed(node *f, int var)
    {
	if (IS_TERMINAL(f)) {
	    return f==zero;
	} else {
	    DECLARE_VAR_ENTAILED_CACHE_ENTRY
	    int result;

	    TRY_VAR_ENTAILED_CACHE(f, var);
	    if (f->value < var) {
		result = var_entailed(f->tr, var)
		      && var_entailed(f->fa, var);
	    } else if (f->value == var) {
		result = (f->fa==zero);
	    } else /* f->value > var */ {
		result = FALSE;
	    }
	    UPDATE_VAR_ENTAILED_CACHE(f, var, result);
	    return result;
	}
    }


/* returns a bitset of all the variables implied by f.  A variable i
 * is implied by f iff
 *
 *	BITSET_IS_MEMBER(*result, i)
 */

bitset *vars_entailed(node *f)
    {
	static bitset tmp_bitset;
	DECLARE_UNARY_BITSET_CACHE_ENTRY

	if (f == zero) {
	    BITSET_UNIVERSE(tmp_bitset);
	} else if (f == one) {
	    BITSET_CLEAR(tmp_bitset);
	} else {
	    bitset bs;

	    TRY_UNARY_BITSET_CACHE(f, vars_entailed);
	    tmp_bitset = *vars_entailed(f->tr);
	    bs = *vars_entailed(f->fa);
	    BITSET_INTERSECTION(tmp_bitset, tmp_bitset, bs);
	    if (f->fa == zero) BITSET_ADD_ELEMENT(tmp_bitset, f->value);
	    UPDATE_UNARY_BITSET_CACHE(f, tmp_bitset, vars_entailed);
	}
	return &tmp_bitset;
    }



/****************************************************************

			Set Sharing Operations

 ****************************************************************/
#if defined(SHARING)

/* returns the Boolean function
 *	~1&~2&...~n | 1&~2&...~n | ~1&2&...~n | ~1&~2&...n
 */
node *init_set_sharing(int n)
    {
	node *result = one;
	node *other = one;

	while (n>1) {
	    other = make_node(n, zero, other);
	    result = make_node(--n, other, result);
	}

	return result;
    }


DECLARE_FN_COUNT(complete_one_or)

/* the same as lub(complete_one(f), prev) */
node *complete_one_or(node *f, node *prev)
    {
	node *result;
	node *complete_one(node *f);
	DECLARE_ASYM_BIN_CACHE_ENTRY

	COUNT_FN(complete_one_or);

	if (IS_TERMINAL(prev)) return (prev==one) ? one : complete_one(f);
	if (IS_TERMINAL(f)) return (f==one) ? one : prev;

	TRY_ASYM_BIN_CACHE(f, prev, complete_one_or);

	/* we want to return:
	 *     lub(make_node(f->value, lub(complete_one(f->tr),
	 *				   complete_one(f->fa)),
	 *		     complete_one(f->fa)),
	 *	   prev);
	 * but we want to unfold the outer lub call, so we compare
	 * prev->value and f->value.  Actually, this isn't right when
	 * f->value <= prev->value, because the make_node call may return
	 * a node whose label is > prev->value, but in that case,
	 *    lub(that, prev) = make_node(prev->value, lub(that, prev->tr),
	 *				  lub(that, prev->fa))
	 */

	if (f->value < prev->value) {
	    node *cfa = complete_one_or(f->fa, prev);
	    result = make_node(f->value, complete_one_or(f->tr, cfa), cfa);
	} else if (f->value == prev->value) {
	    result = make_node(f->value,
			       complete_one_or(f->tr,
					       complete_one_or(f->fa,
							       prev->tr)),
			       complete_one_or(f->fa, prev->fa));
	} else {
	    result = make_node(prev->value,
			       complete_one_or(f, prev->tr),
			       complete_one_or(f, prev->fa));
	}
	UPDATE_ASYM_BIN_CACHE(f, prev, result, complete_one_or);
	return result;
    }


DECLARE_FN_COUNT(complete_one)

node *complete_one(node *f)
    {
	node *result, *cfa;
	DECLARE_UNARY_CACHE_ENTRY

	COUNT_FN(complete_one);

	if (IS_TERMINAL(f)) return f;
	TRY_UNARY_CACHE(f, complete_one);
	cfa = complete_one(f->fa);
	result = make_node(f->value, complete_one_or(f->tr, cfa), cfa);
	UPDATE_UNARY_CACHE(f, result, complete_one);
	return result;
    }


DECLARE_FN_COUNT(complete_or)

/* the same as lub(complete(f, g), prev) */
node *complete_or(node *f, node *g, node *prev)
    {
	node *result;
	node *lo, *hi;
	DECLARE_COMPLETE_OR_CACHE_ENTRY

	COUNT_FN(complete_or);

	if (prev == one) return one;
	if (f == g) return lub(f, prev);
	if (f == zero || g == zero) return prev;

	if (f == one) return complete_one_or(g, prev);
	if (g == one) return complete_one_or(f, prev);
	if (prev == zero) return complete(f, g);

	TRY_COMPLETE_OR_CACHE(f, g, prev);

	if (f->value > g->value) lo=g, hi=f; else lo=f, hi=g;

	if (prev->value < lo->value) {
	    result = make_node(prev->value,
			       complete_or(lo, hi, prev->tr),
			       complete_or(lo, hi, prev->fa));
	} else if (prev->value == lo->value) {
	    if (lo->value == hi->value) {
		node *n = complete_or(lo->tr, hi->tr, prev->tr);
		n = complete_or(lo->tr, hi->fa, n);
		n = complete_or(lo->fa, hi->tr, n);
		result = make_node(lo->value, n,
				   complete_or(lo->fa, hi->fa, prev->fa));
	    } else {
		node *n = complete_or(lo->fa, hi, prev->tr);
		n = complete_or(lo->tr, hi, n);
		result = make_node(lo->value, n,
				   complete_or(lo->fa, hi, prev->fa));
	    }
	} else if (lo->value == hi->value) {
	    node *n = complete_or(lo->tr, hi->tr, prev);
	    n = complete_or(lo->tr, hi->fa, n);
	    n = complete_or(lo->fa, hi->tr, n);
	    result = make_node(lo->value, n,
			       complete_or(lo->fa, hi->fa, prev));
	} else {
	    node *n = complete_or(lo->fa, hi, prev);
	    result = make_node(lo->value, complete_or(lo->tr, hi, n), n);
	}
	UPDATE_COMPLETE_OR_CACHE(f, g, prev, result);

	return result;
    }


DECLARE_FN_COUNT(complete)

node *complete(node *f, node *g)
    {
	node *result;
	int fvar, gvar;
	DECLARE_BIN_CACHE_ENTRY

	COUNT_FN(complete);

	if (f == g) return f;
	if (f == zero || g == zero) return zero;

	TRY_BIN_CACHE(f, g, complete);

	fvar = (f == one) ? MAXVAR : f->value;
	gvar = (g == one) ? MAXVAR : g->value;
	if (fvar == gvar) {
	    result = make_node(fvar,
			 complete_or(f->fa, g->tr,
			     complete_or(f->tr, g->tr,
				 complete(f->tr,g->fa))),
			 complete(f->fa,g->fa));
	} else if (fvar < gvar) {
	    node *cfa = complete(f->fa, g);

	    result = make_node(fvar, complete_or(f->tr, g, cfa), cfa);
	} else /* fvar > gvar */ {
	    node *cfa = complete(f, g->fa);

	    result = make_node(gvar, complete_or(f, g->tr, cfa), cfa);
	}
	UPDATE_BIN_CACHE(f, g, result, complete);
	return result;
    }

DECLARE_FN_COUNT(upclose)

node *upclose(node *f)
    {
	node *utr, *ufa, *result;
	DECLARE_UNARY_CACHE_ENTRY

	COUNT_FN(upclose);

	if (IS_TERMINAL(f)) return f;
	TRY_UNARY_CACHE(f, upclose);
	utr = upclose(f->tr);
	ufa = upclose(f->fa);
	result = make_node(f->value, complete_or(utr, ufa, utr), ufa);
	UPDATE_UNARY_CACHE(f, result, upclose);
	return result;
    }

/*
 * bin(f, g)
 *
 * We note that the set of Boolean functions of n variables is
 * isomorphic to the powerset of the set of n variables.  Thus we can
 * use a Boolean function to represent a subset of the powerset.  We
 * choose to let the *absence* of a variable from the set be indicated
 * by that variable being true in the boolean function.
 *
 * This function computes the set of all possible unions of sets from
 * f and g.
 */

node *bin(node *f, node *g)
    {
	if (f == zero || g == zero) {
	    return zero;
	} else if (f == one) {
	    return bin_univ(g);
	} else if (g == one) {
	    return bin_univ(f);
	} else {
	    node *result;
	    DECLARE_BIN_CACHE_ENTRY

	    TRY_BIN_CACHE(f, g, bin);

	    if (f->value == g->value) {
		node *th = bin(f->tr, g->tr);
		node *el = bin(f->tr, g->fa);

		if (el != one) {
		    el = lub(el, bin(f->fa, g->tr));
		    if (el != one) {
			el = lub(el, bin(f->fa, g->fa));
		    }
		}
		result = make_node(f->value, th, el);
	    } else {
		node *tmp;

		if (f->value > g->value) { tmp = f; f = g; g = tmp; }
		/* now f->value < g->value */
		tmp = bin(f->tr, g);
		if (tmp == one) return one;
		result = make_node(f->value, tmp, lub(tmp, bin(f->fa, g)));
	    }
	    UPDATE_BIN_CACHE(f, g, result, bin);
	    return result;
	}
    }


/* Auxilliary function for bin:  special case code for bin(f, one). */
node *bin_univ(node *f)
    {
	node *g;

	if (IS_TERMINAL(f)) return f;
	g = bin_univ(f->tr);
	if (g == one) return one;
	return make_node(f->value, g, lub(g, bin_univ(f->fa)));
    }

#endif /* SHARING */

/****************************************************************

		      Initialization and Cleanup

 ****************************************************************/


#if defined(STATISTICS)
void print_distribution(int array[], int max)
    {
	int count;
	int sum;
	int total, zero_count;

	for (count=0, total=0; count<=max; ++count) { total += array[count]; }
	zero_count = array[0];
	for (count=0, sum=0; count<=max; ++count) {
	    if (array[count] > 0) {
		sum += array[count];
		printf("%5d nodes:%6d %5.2f%% (cum = %6d %5.1f%%, >0 = %6d %5.1f%%)\n",
		       count, array[count], PERCENTAGE(array[count],total),
		       sum, PERCENTAGE(sum,total), sum-zero_count,
		       total==zero_count ? 999.999
		       : PERCENTAGE(sum-zero_count, total-zero_count));
	    }
	}
	if (array[max+1] > 0) {
	    printf(">%4d nodes:%6d %2.2f%%\n",
		   max, array[max], PERCENTAGE(array[max],total));
	}
	printf("\n");
    }
#endif /* STATISTICS */

void initRep(void)
    {
	int i;

	for (i=0; i<MAXVAR; ++i) identity_renaming[i] = i;

	INIT_FN_COUNT(make_node);
	INIT_FN_COUNT(variableRep);
	INIT_FN_COUNT(implies);
	INIT_FN_COUNT(lub);
	INIT_FN_COUNT(glb);
	INIT_FN_COUNT(project);
	INIT_FN_COUNT(projectThresh);
	INIT_FN_COUNT(renameArray);
	INIT_FN_COUNT(reverseRenameArray);
	INIT_FN_COUNT(ite);
#if defined(USE_ITE_CONSTANT)
	INIT_FN_COUNT(ite_constant);
#endif /* USE_ITE_CONSTANT */
	INIT_FN_COUNT(iff_conj);
	INIT_FN_COUNT(ite_var);
	INIT_FN_COUNT(vars_entailed);
#if defined(SHARING)
        INIT_FN_COUNT(complete_one_or);
        INIT_FN_COUNT(complete_one);
        INIT_FN_COUNT(complete_or);
        INIT_FN_COUNT(complete);
        INIT_FN_COUNT(upclose);
#endif /* SHARING */
	/* This code clears the unique table and all the computed
	 * caches.  This should make it possible to time repeated
	 * execution of computations without the effect that after the
	 * first time the appropriate cache will probably hold the
	 * right result, so after the first time through the timing is
	 * useless.  Even if this isn't the case, after the first time
	 * performing a computation no new nodes will ever be created
	 * because they will already exist in the unique table.
	 *
	 * After this, all old pointers to ROBDDs *must* be forgotten,
	 * because we free all allocated nodes, thus giving us a fresh
	 * start.
	 */

	INIT_UNIQUE_TABLE;

	INIT_CACHE(ite);
#if defined(USE_ITE_CONSTANT)
	INIT_CACHE(ite_constant);
#endif /* USE_ITE_CONSTANT */
#if defined(SHARING)
	INIT_CACHE(upclose);
	INIT_CACHE(complete);
	INIT_CACHE(complete_one);
	INIT_CACHE(complete_or);
	INIT_CACHE(complete_one_or);
	INIT_CACHE(bin);
#endif /* SHARING */
	INIT_CACHE(glb);
	INIT_CACHE(lub);
	INIT_CACHE(rglb);
	INIT_CACHE(ite_var);
	INIT_CACHE(vars_entailed);
    }


void concludeRep(void)
    {
#if defined(STATISTICS)
	node *ptr;
	int size_count[MAX_COUNT+2];
	int i, count;

	printf("\n\n\n================ Operation Counts ================\n\n");
	PRINT_FN_COUNT(make_node);
	PRINT_FN_COUNT(variableRep);
	PRINT_FN_COUNT(implies);
	PRINT_FN_COUNT(lub);
	PRINT_FN_COUNT(glb);
	PRINT_FN_COUNT(project);
	PRINT_FN_COUNT(restrictThresh);
	PRINT_FN_COUNT(renameArray);
	PRINT_FN_COUNT(reverseRenameArray);
	PRINT_FN_COUNT(ite);
#if defined(USE_ITE_CONSTANT)
	PRINT_FN_COUNT(ite_constant);
#endif /* USE_ITE_CONSTANT */
	PRINT_FN_COUNT(iff_conj);
	PRINT_FN_COUNT(ite_var);
	PRINT_FN_COUNT(vars_entailed);
#if defined(SHARING)
        PRINT_FN_COUNT(complete_one_or);
        PRINT_FN_COUNT(complete_one);
        PRINT_FN_COUNT(complete_or);
        PRINT_FN_COUNT(complete);
        PRINT_FN_COUNT(upclose);
#endif /* SHARING */
	printf("\n================ Cache Performance ================\n\n");
	printf("%d nodes extant\n\n", nodes_in_use());
	printf("Unique table:  %d hits, %d misses, %f%% hits\n",
	       unique_table_hits, unique_table_misses,
	       PERCENTAGE(unique_table_hits,
			  unique_table_hits+unique_table_misses));
	memset(size_count, 0, sizeof(size_count));
	for (i=0; i<UNIQUE_TABLE_SIZE; ++i) {
	    for (ptr=unique_table[i],count=0;
		 ptr!=NULL;
		 ptr=ptr->unique,++count);
	    ++size_count[(count<=MAX_COUNT ? count : MAX_COUNT+1)];
	}
	print_distribution(size_count, MAX_COUNT);

	PRINT_CACHE_PERFORMANCE(ite);
#if defined(USE_ITE_CONSTANT)
	PRINT_CACHE_PERFORMANCE(ite_constant);
#endif /* USE_ITE_CONSTANT */
	PRINT_CACHE_PERFORMANCE(lub);
	PRINT_CACHE_PERFORMANCE(glb);
#if defined(SHARING)
	PRINT_CACHE_PERFORMANCE(upclose);
	PRINT_CACHE_PERFORMANCE(complete);
	PRINT_CACHE_PERFORMANCE(complete_or);
	PRINT_CACHE_PERFORMANCE(complete_one_or);
	PRINT_CACHE_PERFORMANCE(complete_one);
#endif /* SHARING */

	PRINT_CACHE_PERFORMANCE(rglb);

	PRINT_CACHE_PERFORMANCE(ite_var);
	PRINT_CACHE_PERFORMANCE(vars_entailed);
#endif /* STATISTICS */
    }



/****************************************************************

			   Testing Support

 ****************************************************************/

/* special version of iff_conj_array() that is the fast version no matter
 * which options are selected.  I hope this gives more reliable test times.
 */

node *testing_iff_conj_array(int v0, int n, int arr[])
    {
	node *thens = one, *elses = zero;
	int *ptr;
	int vi;

	/* first build part of graph below v0.  For this, we build two
	 * subgraphs:  one for when v0 is true, and one for false.
	 * These are called thens and elses.
	 */

	for (ptr=&arr[n-1]; ptr>=arr && v0<(vi=*ptr); --ptr) {
	    thens = make_node(vi, thens, zero);
	    elses = make_node(vi, elses, one);
	}
	
	/* make v0 node */
	thens = make_node(v0,thens,elses);

	/* Finally build part of graph above v0.  For this, we build
	 * only one graph, whose then branch is the graph we've built
	 * so far and whose else branch is ~v0.
	 */

	if (ptr >= arr) {
	    /* make ~v0 */
	    elses = make_node(v0,zero,one);

	    do {
		vi = *ptr;
		thens = make_node(vi, thens, elses);
	    } while (--ptr >= arr);
	}

	return thens;
    }

static int intcompare(const void *a, const void *b)
    {
	return (*((const int *)a) - *((const int *)b));
    }

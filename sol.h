#ifndef SOL_H_INCLUDE_GUARD_
#define SOL_H_INCLUDE_GUARD_

#define _GNU_SOURCE

// Defining here enables the LSP to check the source file.
// #define SOL_DEF

#include <stddef.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <math.h>
#include <float.h>
#include <x86gprintrin.h>
#include <x86intrin.h>
#include <immintrin.h>
#include <time.h>
#include <errno.h>
#include <dirent.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#define SOL_THREAD

#ifdef SOL_THREAD
#include <pthread.h>
#endif

/* @Todo

    - copy/set routines, e.g.

inline static void copy_small(void *to, void *from, size_t size)
{
    uchar *t = to;
    uchar *f = from;
    for(size_t i=0; i < size; ++i)
        t[i] = from[i];
}

inline static void copy_small_16x(void *to, void *from, size_t size)
{
    size >> 4;
    for(size_t i=0; i < size; ++i) {
        __m128i a = _mm_load_si128((__m128i*)((uchar*)from + (i<<4)));
        _mm_store_si128((__m128i*)((uchar*)to + (i<<4)), a);
    }
}
*/

//                                  ## BEGIN FILE ##

////////////////////////////////////////////////////////////////////////////////
// defs.h

#define THREAD_COUNT 4
#define DEBUG 1

typedef unsigned int uint;

typedef uint32_t uint32;
typedef int32_t  int32;
typedef uint64_t uint64;
typedef int64_t  int64;
typedef uint16_t uint16;
typedef int16_t  int16;
typedef uint8_t  uint8;
typedef int8_t   int8;
typedef unsigned char uchar;

#define asm __asm__

#define Max_f32    FLT_MAX
#define Max_uint64 UINT64_MAX
#define Max_uint32 UINT32_MAX
#define Max_uint16 UINT16_MAX
#define Max_uint8  UINT8_MAX
#define Max_u64 UINT64_MAX
#define Max_u32 UINT32_MAX
#define Max_u16 UINT16_MAX
#define Max_u8  UINT8_MAX
#define Max_s64  INT64_MAX
#define Max_s32  INT32_MAX
#define Max_s16  INT16_MAX
#define Max_s8   INT8_MAX

#ifndef _WIN32
    #define FN_ADDR __builtin_frame_address(0)
#else
    #error Todo
#endif

#define float_or_max(f) ((float)((uint64)(f) | Max_u64))

#ifndef _WIN32
    #define cl_align(s) __attribute__((aligned(s))) // compiler align
#else
    #error Todo
#endif

#define max64_if_true(eval)  (Max_uint64 + (uint64)((eval) == 0))
#define max32_if_true(eval)  (Max_uint32 + (uint32)((eval) == 0))
#define max8_if_true(eval)   (Max_uint8  +  (uint8)((eval) == 0))
#define max64_if_false(eval) (Max_uint64 + (uint64)((eval) != 0))
#define max32_if_false(eval) (Max_uint32 + (uint32)((eval) != 0))
#define max8_if_false(eval)  (Max_uint8  +  (uint8)((eval) != 0))

#define max_if(eval) (!(eval) - 1)
#define zero_if(eval) ((eval) - 1)
#define maxif(eval)  (!(eval) - 1)
#define zeroif(eval)  ((eval) - 1)

static inline bool is_in_srange_inc(int64 i, int64 min, int64 max)
{
    return i >= min && i <= max;
}

static inline bool is_in_srange_exc(int64 i, int64 min, int64 max)
{
    return i > min && i < max;
}

static inline bool is_in_urange_inc(uint64 i, uint64 min, uint64 max)
{
    return i >= min && i <= max;
}

static inline bool is_in_urange_exc(uint64 i, uint64 min, uint64 max)
{
    return i > min && i < max;
}

struct mem_req {
    size_t size;
    size_t alignment;
};

struct range {
    size_t offset;
    size_t size;
};

struct pair_uint {
    uint a,b;
};

struct pair_float {
    float a,b;
};

static inline void update_range(struct range *range, size_t offset, size_t size) {
    range->offset -= range->offset & max64_if_true(offset < range->offset);
    range->offset += offset & max64_if_true(offset < range->offset);
    range->size -= range->size & max64_if_true(offset + size > range->size);
    range->size += offset + size & max64_if_true(offset + size > range->size);
}

static inline uint64 align(uint64 size, uint64 alignment) {
  const uint64 alignment_mask = alignment - 1;
  return (size + alignment_mask) & ~alignment_mask;
}

static inline int max(int x, int y) {
    return x > y ? x : y;
}

static inline uint inc_and_wrap_no_mod(uint num, uint inc, uint max) {
    return (num+inc) & (max-1);
}

static inline uint dec_and_wrap_no_mod(uint num, uint dec, uint max) {
    return (num-dec) & (max-1);
}

static inline uint inc_and_wrap(uint num, uint inc, uint max) {
    return (num+inc) % max;
}

static inline bool before(uint64_t a, uint64_t b) {
    return ((int64)b - (int64)a) > 0;
}

static inline uint32 set_bit_idx(uint32 mask, uint i) {
    return mask | (1 << i);
}

static inline uint32 clear_bit_idx(uint32 mask, uint i) {
    return mask & ~(1 << i);
}

// This function is useful sometimes, such as for low level string building,
// but I cannot think of a decent name. I use copied as in "copied <size> bytes",
// but that is weak...
// Alternatives: copy_size, copy_bytes, memcpyd (same as current, but clearer that it inherits from memcpy).
static inline size_t copied(void *to, const void *from, size_t size) {
    memcpy(to, from, size);
    return size;
}

// This might be sort of relying on UB but whatever.
static inline void memcpy_if(void *to, const void *from, size_t size, bool b) {
    memcpy(to, from, size & max_if(b));
}

#define zero(t) memset(&t,0,sizeof(t))
#define smemset(to, num, type, count) memset(to, num, sizeof(type) * count)
#define smemcpy(to, from, type, count) memcpy(to, from, sizeof(type) * count)
#define smemmove(to, from, type, count) memmove(to, from, sizeof(type) * count)
#define carrlen(a) (sizeof(a)/sizeof(a[0]))
#define cstrarrlen(a) ((sizeof(a)/sizeof(a[0])) - 1)
#define cstr_as_array_len(a) ((sizeof(a)/sizeof(a[0])) - 1)

#define smalloc(cnt, type) malloc(cnt * sizeof(type))
#define srealloc(p, cnt, type) realloc(p, cnt * sizeof(type))

#define smemcpy_if(to, from, type, count, b) \
    memcpy(to, from, sizeof(type) * (count & max_if(b)))

#ifndef _WIN32

    /* bit manipulation */
static inline int ctz16(unsigned short a) {
    // @Note This has to be copied between compiler directives because gcc will not compile
    // tzcnt16 with only one leading undescore. I assume this is a compiler bug, because tzcnt32
    // and 64 both want one leading underscore...
    return __tzcnt_u16(a);
}
static inline int ctz32(unsigned int a) {
    return _tzcnt_u32(a);
}
static inline int ctz64(uint64 a) {
    return _tzcnt_u64(a);
}
static inline int clz16(uint16 mask) {
    return __builtin_clzs(mask);
}
static inline int clz32(uint32 mask) {
    return __builtin_clz(mask);
}
static inline int clz64(uint64 mask) {
    return __builtin_clzl(mask);
}

//
// I am not sure about the best way to do pop counts. Writing explicitly the type in the function call
// seems useful, but having the compiler just use the type is also nice. Furthermore, it seems like these
// are super safe using shorter widths than expected by the function, such as:
//        uint64 x = Max; uint16 *y = &x; popcnt(*y);
// But using the bigger widths than expected does not work. Idk if there is any overhead to using the bigger types
// for smaller widths (then I never have to think about the type in the function name). I would expect that they
// are implemented identically.
//
// I am going to try always using the 64 bit version, and seeing if I get any bugs. I feel like if I get somewhat used
// to not specifying the type, but having to only in the case of 64 bit, I will start making mistakes. So I will use
// the option where I never have to consider it.
//
// #define popcnt(x) __builtin_popcount(x)
//
// Extending this point, gcc documents type generic versions __builtin_ctzg and __builtin_ctzg. But when using them
// they are undefined. These would also be very nice, but as I say, undefined. WTF?
// I am going to have a go using macro versions of these using subtractions. I want to see if run into stuff. From
// preliminary tests, its seems like they work fine with optimizations on, and I would have thought that the
// behviour would always be the same since they are instrinsics (as in their behaviour would not change regardless
// of the context they are in), but we will see I guess.
//

#define popcnt(x) __builtin_popcountl(x)

#define ctzc(x) (__builtin_ctz(x)-24)
#define ctzs(x) (__builtin_ctz(x)-16)
#define ctzi(x) __builtin_ctz(x)
#define ctzl(x) __builtin_ctzl(x)

#define ctz(x) __builtin_ctzl(x)

#define clzc(x) (__builtin_clz(x)-24)
#define clzs(x) (__builtin_clz(x)-16)
#define clz(x) __builtin_clz(x)
#define clzl(x) __builtin_clzl(x)

static inline int popcnt8(uint8 num) {
    uint32 tmp = num;
    tmp &= 0x000000ff;
    return (int)__builtin_popcount(tmp);
}
static inline int popcnt16(uint16 num) {
    uint32 tmp = num;
    tmp &= 0x0000ffff; // just to be sure;
    return (int)__builtin_popcount(tmp);
}
static inline int popcnt32(uint32 num) {
    return (int)__builtin_popcount(num);
}
static inline int popcnt64(uint64 num) {
    return (int)__builtin_popcountl(num);
}
static inline int pop_count16(uint16 num) {
    uint32 tmp = num;
    tmp &= 0x0000ffff; // just to be sure;
    return (int)__builtin_popcount(tmp);
}
static inline int pop_count32(uint32 num) {
    return (int)__builtin_popcount(num);
}
static inline int pop_count64(uint64 num) {
    return (int)__builtin_popcountl(num);
}

#else

static inline int ctz16(unsigned short a) {
    return (int)_tzcnt_u16(a);
}
static inline int ctz32(unsigned int a) {
    return (int)_tzcnt_u32(a);
}
static inline int ctz64(uint64 a) {
    return (int)_tzcnt_u64(a);
}
static inline int clz16(uint16 mask) {
    return __lzcnt16(mask);
}
static inline int clz32(uint32 mask) {
    return __lzcnt(mask);
}
static inline int clz64(uint64 mask) {
    return __lzcnt64(mask);
}
static inline int pop_count16(uint16 num) {
    return (int)__popcnt16(num);
}
static inline int pop_count32(uint32 num) {
    return (int)__popcnt(num);
}
static inline int pop_count64(uint64 num) {
    return (int)__popcnt64(num);
}
#endif // WIN32 or not

static inline int packed_sparse_array_index_to_bit(uint i, uint32 bits) {
    bits &= ~(0xffffffff << i);
    return pop_count32(bits);
}

static inline uint tzclr(uint64 *mask)
{
    uint tz = ctz(*mask);
    *mask &= ~(1<<tz);
    return tz;
}

// ma == mask array, ip == in place
static inline uint64 ma_and(uint64 *ma, uint64 i)
{
    uint64 one = 1;
    return ma[i>>6] & (one<<(i & 63));
}

static inline uint64 ma_or(uint64 *ma, uint64 i)
{
    uint64 one = 1;
    return ma[i>>6] | (one<<(i & 63));
}

static inline uint64 ma_and_not(uint64 *ma, uint64 i)
{
    uint64 one = 1;
    return ma[i>>6] & ~(one<<(i & 63));
}

static inline uint64 ma_or_if(uint64 *ma, uint64 i, bool b)
{
    uint64 one = 1 & b;
    return ma[i>>6] | (one<<(i & 63));
}

// @TODO Check that everywhere that uses the ma_ functions understands that they do not
// do the operation in place by default, but instead return a copy of the new mask.
static inline void ma_or_if_ip(uint64 *ma, uint64 i, bool b)
{
    uint64 one = 1 & b;
    ma[i>>6] |= (one<<(i & 63));
}

static inline bool flag_check(uint64 flags, uint64 bit) {
    return (flags & bit) != 0;
}

static inline bool mask_array_check_no_mod(uint64 *masks, uint index) {
    uint64 one = 1;
    return masks[index >> 6] & (one << (index & 63));
}

static inline void mask_array_set_no_mod(uint64 *masks, uint index) {
    uint64 one = 1;
    masks[index >> 6] |= (one << (index & 63));
}

////////////////////////////////////////////////////////////////////////////////
// assert.h

#if DEBUG
#if _WIN32

#define assert(x) \
    if (!(x)) {printf("\n    [file: %s, line: %i, fn %s]\n        ** ASSERTION FAILED **: %s\n\n", __FILE__, __LINE__, __FUNCTION__, #x); __debugbreak;}

#else

#define assert(x) \
    if (!(x)) {printf("\n    [file: %s, line: %i, fn %s]\n        ** ASSERTION FAILED **: %s\n\n", __FILE__, __LINE__, __FUNCTION__, #x); asm("int $3");}

#endif // _WIN32 or not

#else
#define assert(x)
#endif // if DEBUG

////////////////////////////////////////////////////////////////////////////////
// print.h

// 'args' must have been started, ends 'args' itself
void string_format_backend(char *format_buffer, const char *fmt, va_list args);

static inline void string_format(char *format_buffer, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    string_format_backend(format_buffer, fmt, args);
}

static inline void print(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    char format_buffer[1024];
    string_format_backend(format_buffer, fmt, args);

    fwrite(format_buffer, 1, strlen(format_buffer), stdout);
}

static inline void println(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    char format_buffer[1024];
    string_format_backend(format_buffer, fmt, args);

    int tmp = strlen(format_buffer);
    format_buffer[tmp] = '\n';

    fwrite(format_buffer, 1, tmp + 1, stdout);
}

static inline void print_count_chars(const char *data, int count) {
    fwrite(data, 1, count, stdout);
}

static inline void print_count_chars_ln(const char *data, int count) {
    fwrite(data, 1, count, stdout);
    println("");
}

////////////////////////////////////////////////////////////////////////////////
// log.h

typedef enum {
    LOG_LEVEL_NONE = 0,
    LOG_LEVEL_ERROR = 1,
} log_level_severity;

static const char *LOG_LEVEL_MSG_TABLE[] = {
    "NONE",
    "ERROR",
};

#define LOG_LEVEL 1
#define LOG_BREAK 1

static inline void fn_log_print(
    log_level_severity  severity,
    const char         *file,
    int                 line,
    const char         *function,
    const char         *msg, ...)
{
    char buf[1024];
    va_list args;
    va_start(args, msg);
    string_format_backend(buf, msg, args);

    if (severity >= LOG_LEVEL) {
        print("LOG %s (%s, line %i, fn %s): ", LOG_LEVEL_MSG_TABLE[severity], file, line, function);
        buf[strlen(buf) + 1] = '\0';
        buf[strlen(buf) + 0] = '\n';
        print_count_chars(buf, strlen(buf));
    }
}

#ifndef _WIN32
    #define asm __asm__
#endif

#if LOG_LEVEL

#if LOG_BREAK
    #define LOG_BREAKPOINT println("LOG BREAK"); asm("int $3")
#else
    #define LOG_BREAKPOINT
#endif


#define log_print(severity, msg...)                                                \
    do {                                                                           \
        fn_log_print(severity, __FILE__, __LINE__, __FUNCTION__, msg);             \
        LOG_BREAKPOINT;                                                            \
    } while(0)

#define log_print_if(predicate, severity, msg...)                          \
    do {                                                                   \
        if ((predicate)) {                                                 \
            fn_log_print(severity, __FILE__, __LINE__, __FUNCTION__, msg); \
            LOG_BREAKPOINT;                                                \
        }                                                                  \
    } while(0)

#else

#define log_print(severity, msg...)
#define log_print_if(predicate, severity, msg...)

#endif

#define log_print_error(msg...) \
    log_print(LOG_LEVEL_ERROR, msg)
#define log_print_error_if(predicate, msg...) \
    log_print_if(predicate, LOG_LEVEL_ERROR, msg)

////////////////////////////////////////////////////////////////////////////////
// math.h

// @Todo Idk if this is too big/too small. Same magnitude as used in test.c.
#define FLOAT_ERROR 0.000001

static inline bool feq(float a, float b)
{
    return fabsf(a - b) < FLOAT_ERROR;
}

static inline float lerp(float a, float b, float c)
{
    return a + c * (b - a);
}

static inline float clamp(float num, float min, float max)
{
    if (num > max)
        num = max;
    else if (num < min)
        num = min;
    return num;
}

#define PI 3.1415926
#define PI_OVER_180 0.01745329

static inline float radf(float x) {
    return x * PI_OVER_180;
}

typedef struct {
    float x;
    float y;
    float z;
    float w;
} vector cl_align(16);

struct trs {
    vector t;
    vector r;
    vector s;
};

typedef struct matrix {
    float m[16];
} matrix cl_align(16);

static inline vector get_vector(float x, float y, float z, float w)
{
    return (vector) {.x = x, .y = y, .z = z, .w = w};
}
#define vector4(x, y, z, w) get_vector(x, y, z, w)
#define vector3(x, y, z)    get_vector(x, y, z, 0)
#define vector2(x, y)       get_vector(x, y, 0, 0)

static inline void get_matrix(vector colx, vector coly, vector colz, vector colw, matrix *m)
{
    __m128 a = _mm_load_ps(&colx.x);
    __m128 b = _mm_load_ps(&coly.x);
    __m128 c = _mm_load_ps(&colz.x);
    __m128 d = _mm_load_ps(&colw.x);
    _mm_store_ps(&m->m[ 0], a);
    _mm_store_ps(&m->m[ 4], b);
    _mm_store_ps(&m->m[ 8], c);
    _mm_store_ps(&m->m[12], d);
}
#define matrix3(x, y, z, m) get_matrix(x, y, z, (vector){0}, m)

static inline float vector_i(vector v, uint i)
{
    assert(i < 4);
    return ((float*)&v)[i];
}

static inline void get_trs(vector t, vector r, vector s, struct trs *trs)
{
    *trs = (struct trs) {
        .t = t,
        .r = r,
        .s = s,
    };
}

static inline void print_matrix(matrix *m)
{
    print("[\n");
    const uint cols[] = {0,4,8,12};
    uint i,j;
    for(i=0;i<4;++i) {
        print("    ");
        for(j=0;j<4;++j)
            print("%f, ", m->m[cols[j]+i]);
        print("\n");
    }
    print("]\n");
}

static inline void println_vector(vector v)
{
    print("[%f, %f, %f, %f]\n", v.x, v.y, v.z, v.w);
}

static inline void set_vector_if(vector *x, vector *y, bool z)
{
    uint32 p = zeroif(z);
    uint32 q =  maxif(z);
    __m128i a = _mm_load_si128((__m128i*)x);
    __m128i b = _mm_load_si128((__m128i*)y);
    __m128i c = _mm_set1_epi32(p);
    __m128i d = _mm_set1_epi32(q);
    a = _mm_and_si128(a, c);
    b = _mm_and_si128(b, d);
    a = _mm_add_epi32(a, b);
    _mm_store_si128((__m128i*)x, a);
}

static inline void array_to_vector(float *arr, vector v)
{
    smemcpy(&v, arr, *arr, 4);
}

static inline vector scalar_mul_vector(vector v, float s)
{
    __m128 a = _mm_load_ps(&v.x);
    __m128 b = _mm_set1_ps(s);
    a = _mm_mul_ps(a,b);
    vector r;
    _mm_store_ps(&r.x, a);
    return r;
}
#define scale_vector(v, s) scalar_mul_vector(v, s)

static inline vector scalar_div_vector(vector v, float s)
{
    __m128 a = _mm_load_ps(&v.x);
    __m128 b = _mm_set1_ps(s);
    a = _mm_div_ps(a,b);
    vector r;
    _mm_store_ps(&r.x, a);
    return r;
}

static inline vector mul_vector(vector v1, vector v2)
{
    __m128 a = _mm_load_ps(&v1.x);
    __m128 b = _mm_load_ps(&v2.x);
    a = _mm_mul_ps(a,b);
    vector r;
    _mm_store_ps(&r.x, a);
    return r;
}

static inline float dot(vector v1, vector v2)
{
    vector v3 = mul_vector(v1, v2);
    return v3.x + v3.y + v3.z + v3.w;
}

static inline vector cross(vector p, vector q)
{
    vector ret;
    ret.x = p.y * q.z - p.z * q.y;
    ret.y = p.z * q.x - p.x * q.z;
    ret.z = p.x * q.y - p.y * q.x;
    return ret;
}

static inline vector add_vector(vector v1, vector v2)
{
    __m128 a = _mm_load_ps(&v1.x);
    __m128 b = _mm_load_ps(&v2.x);
    a = _mm_add_ps(a,b);
    vector r;
    _mm_store_ps(&r.x, a);
    return r;
}

static inline vector sub_vector(vector v1, vector v2)
{
    __m128 a = _mm_load_ps(&v1.x);
    __m128 b = _mm_load_ps(&v2.x);
    a = _mm_sub_ps(a,b);
    vector r;
    _mm_store_ps(&r.x, a);
    return r;
}

static inline float magnitude_vector(vector v) {
    __m128 a = _mm_load_ps(&v.x);
    __m128 b = a;
    a = _mm_mul_ps(a,b);
    float *f = (float*)&a;
    return sqrtf(f[0] + f[1] + f[2]);
}

static inline vector normalize(vector v) {
    float f = magnitude_vector(v);
    vector r = scalar_div_vector(v, f);
    r.w = 0;
    return r;
}

static inline vector lerp_vector(vector a, vector b, float c) {
    vector ret;
    ret = sub_vector(b, a);
    ret = scalar_mul_vector(ret, c);
    return add_vector(a, ret);
}

// angle in radians
static inline vector quaternion(float angle, vector v)
{
    float f = angle/2;
    float sf = sinf(f);
    vector r;
    __m128 a;
    __m128 b;
    a = _mm_load_ps(&v.x);
    b = _mm_set1_ps(sf);
    a = _mm_mul_ps(a, b);
    _mm_store_ps(&r.x, a);
    r.w = cosf(f);
    return r;
}

// equivalent to applying rotation q2, followed by rotation q1
static inline vector hamilton_product(vector *q2, vector *q1)
{
    return (vector) {
        .w = q1->w * q2->w - q1->x * q2->x - q1->y * q2->y - q1->z * q2->z,
        .x = q1->w * q2->x + q1->x * q2->w + q1->y * q2->z - q1->z * q2->y,
        .y = q1->w * q2->y - q1->x * q2->z + q1->y * q2->w + q1->z * q2->x,
        .z = q1->w * q2->z + q1->x * q2->y - q1->y * q2->x + q1->z * q2->w,
    };
}

static inline void copy_matrix(matrix *to, matrix *from)
{
    __m128i a = _mm_load_si128((__m128i*)(from->m+0));
    __m128i b = _mm_load_si128((__m128i*)(from->m+4));
    __m128i c = _mm_load_si128((__m128i*)(from->m+8));
    __m128i d = _mm_load_si128((__m128i*)(from->m+12));
    _mm_store_si128((__m128i*)(to->m+0),a);
    _mm_store_si128((__m128i*)(to->m+4),b);
    _mm_store_si128((__m128i*)(to->m+8),c);
    _mm_store_si128((__m128i*)(to->m+12),d);
}

static inline void identity_matrix(matrix *m)
{
    __m128 a = _mm_set_ps(0,0,0,1);
    __m128 b = _mm_set_ps(0,0,1,0);
    __m128 c = _mm_set_ps(0,1,0,0);
    __m128 d = _mm_set_ps(1,0,0,0);
    _mm_store_ps(m->m+ 0, a);
    _mm_store_ps(m->m+ 4, b);
    _mm_store_ps(m->m+ 8, c);
    _mm_store_ps(m->m+12, d);
}

// @Optimise Idk if using a global here is faster than initializing the matrix.
// When I was testing this, my testing was majorly flawed.
matrix IDENTITY_MATRIX = {
    .m = {
        1,0,0,0,
        0,1,0,0,
        0,0,1,0,
        0,0,0,1,
    },
};

inline static bool is_ident(matrix *m)
{
    return memcmp(m, &IDENTITY_MATRIX, sizeof(*m)) == 0;
}

// @Optimise It looks like letting the compiler decide how to init stuff is
// better. See above implementation of identity_matrix
static inline void count_identity_matrix(uint count, matrix *m)
{
    __m128i a = (__m128i)_mm_set_ps(0,0,0,1);
    __m128i b = (__m128i)_mm_set_ps(0,0,1,0);
    __m128i c = (__m128i)_mm_set_ps(0,1,0,0);
    __m128i d = (__m128i)_mm_set_ps(1,0,0,0);
    for(uint i = 0; i < count; ++i) {
        _mm_store_si128((__m128i*)(m[i].m+0), a);
        _mm_store_si128((__m128i*)(m[i].m+4), b);
        _mm_store_si128((__m128i*)(m[i].m+8), c);
        _mm_store_si128((__m128i*)(m[i].m+12), d);
    }
}

static inline void scale_matrix(vector v, matrix *m)
{
    memset(m, 0, sizeof(*m));
    m->m[0] = v.x;
    m->m[5] = v.y;
    m->m[10] = v.z;
    m->m[15] = 1;
}

static inline void translation_matrix(vector v, matrix *m)
{
    identity_matrix(m);
    m->m[12] = v.x;
    m->m[13] = v.y;
    m->m[14] = v.z;
}

static inline void rotation_matrix(vector r, matrix *m)
{
    __m128 a = _mm_load_ps(&r.x);
    __m128 b = a;
    a = _mm_mul_ps(a,b);
    float *f = (float*)&a;

    float xy = 2 * r.x * r.y;
    float xz = 2 * r.x * r.z;
    float yz = 2 * r.y * r.z;
    float wx = 2 * r.w * r.x;
    float wy = 2 * r.w * r.y;
    float wz = 2 * r.w * r.z;

    identity_matrix(m);

    m->m[0] = f[3] + f[0] - f[1] - f[2];
    m->m[4] = xy - wz;
    m->m[8] = xz + wy;

    m->m[1] = xy + wz;
    m->m[5] = f[3] - f[0] + f[1] - f[2];
    m->m[9] = yz - wx;

    m->m[2] = xz - wy;
    m->m[6] = yz + wx;
    m->m[10] = f[3] - f[0] - f[1] + f[2];
}

static inline void mul_matrix(matrix *x, matrix *y, matrix *z)
{
    const uint cols[] = {0,4,8,12};
    __m128 a;
    __m128 b;
    float *f = (float*)&b;
    matrix m;
    uint i,j;
    for(i=0;i<4;++i) {
        a = _mm_set_ps(x->m[12+i],x->m[8+i],x->m[4+i],x->m[0+i]);
        for(j=0;j<4;++j) {
            b = _mm_load_ps(y->m + 4*j);
            b = _mm_mul_ps(a,b);
            m.m[cols[j]+i] = f[0]+f[1]+f[2]+f[3];
        }
    }
    copy_matrix(z,&m);
}

static inline void convert_trs(struct trs *trs, matrix *ret)
{
    matrix t,r,s;
    translation_matrix(trs->t, &t);
    rotation_matrix(trs->r, &r);
    scale_matrix(trs->s, &s);
    mul_matrix(&t,&r,&r);
    mul_matrix(&r,&s,ret);
}

static inline void scalar_mul_matrix(matrix *m, float f)
{
    __m128 a = (__m128)_mm_load_si128((__m128i*)(m->m+0));
    __m128 b = (__m128)_mm_load_si128((__m128i*)(m->m+4));
    __m128 c = (__m128)_mm_load_si128((__m128i*)(m->m+8));
    __m128 d = (__m128)_mm_load_si128((__m128i*)(m->m+12));
    __m128 e = _mm_set1_ps(f);
    a = _mm_mul_ps(a,e);
    b = _mm_mul_ps(b,e);
    c = _mm_mul_ps(c,e);
    d = _mm_mul_ps(d,e);
    _mm_store_si128((__m128i*)(m->m+0),(__m128i)a);
    _mm_store_si128((__m128i*)(m->m+4),(__m128i)b);
    _mm_store_si128((__m128i*)(m->m+8),(__m128i)c);
    _mm_store_si128((__m128i*)(m->m+12),(__m128i)d);
    m->m[15] = 1;
}

static inline vector mul_matrix_vector(matrix *m, vector p)
{
    __m128 a;
    __m128 b;
    __m128 c;

    a = _mm_load_ps(m->m + 0);
    b = _mm_set1_ps(p.x);
    c = _mm_mul_ps(a, b);

    a = _mm_load_ps(m->m + 4);
    b = _mm_set1_ps(p.y);
    a = _mm_mul_ps(a, b);
    c = _mm_add_ps(a, c);

    a = _mm_load_ps(m->m + 8);
    b = _mm_set1_ps(p.z);
    a = _mm_mul_ps(a, b);
    c = _mm_add_ps(a, c);

    a = _mm_load_ps(m->m + 12);
    b = _mm_set1_ps(p.w);
    a = _mm_mul_ps(a, b);
    c = _mm_add_ps(a, c);

    float *f = (float*)&c;
    return get_vector(f[0], f[1], f[2], f[3]);
}

static inline void transpose(matrix *m)
{
    uint cols[] = {0, 4, 8, 12};
    int cnt = -1;
    matrix t;
    for(uint i=0; i < 16; ++i) {
        cnt += (i & 3) == 0;
        t.m[cols[i & 3] + cnt] = m->m[i];
    }
    copy_matrix(m, &t);
}

// Inverts a 3x3
static inline bool invert(matrix *x, matrix *y)
{
    float m[3][8] cl_align(16);
    memset(m, 0, sizeof(m));

    m[0][0] = x->m[0]; m[1][0] = x->m[4]; m[2][0] = x->m[8];
    m[0][1] = x->m[1]; m[1][1] = x->m[5]; m[2][1] = x->m[9];
    m[0][2] = x->m[ 2]; m[1][2] = x->m[ 6]; m[2][2] = x->m[10];

    m[0][3] = 1; m[0][4] = 0; m[0][5] = 0;
    m[1][3] = 0; m[1][4] = 1; m[1][5] = 0;
    m[2][3] = 0; m[2][4] = 0; m[2][5] = 1;

    __m128 a,b,c,d,e,f,g;

    for(uint j=0; j < 3; ++j) {
        float max = 0;
        uint r;
        for(uint row=j; row < 3; ++row)
            if (fabs(m[row][j]) > max) {
                max = fabs(m[row][j]);
                r = row;
            }

        if (feq(max, 0))
            return false;

        a = _mm_load_ps(m[j] + 0);
        b = _mm_load_ps(m[j] + 4);

        if (r != j) {
            c = _mm_load_ps(m[r] + 0);
            d = _mm_load_ps(m[r] + 4);
            // @Optimise I feel that I can remove half these stores by avoiding
            // loading the same data later...
            _mm_store_ps(m[r] + 0, a);
            _mm_store_ps(m[r] + 4, b);
            _mm_store_ps(m[j] + 0, c);
            _mm_store_ps(m[j] + 4, d);
            a = c;
            b = d;
        }

        e = _mm_set1_ps(1 / m[j][j]);
        a = _mm_mul_ps(a, e);
        b = _mm_mul_ps(b, e);
        _mm_store_ps(m[j] + 0, a);
        _mm_store_ps(m[j] + 4, b);

        for(r=0; r < 3; ++r) {
            if (r == j)
                continue;

            e = _mm_set1_ps(-m[r][j]);
            f = _mm_mul_ps(e, a);
            g = _mm_mul_ps(e, b);
            c = _mm_load_ps(m[r] + 0);
            d = _mm_load_ps(m[r] + 4);
            c = _mm_add_ps(c, f);
            d = _mm_add_ps(d, g);
            _mm_store_ps(m[r] + 0, c);
            _mm_store_ps(m[r] + 4, d);
        }
    }

    matrix3(vector3(m[0][3], m[0][4], m[0][5]),
            vector3(m[1][3], m[1][4], m[1][5]),
            vector3(m[2][3], m[2][4], m[2][5]), y);

    return true;
}

static inline void view_matrix(vector pos, vector fwd, matrix *m)
{
    vector fo = get_vector(0, 0, 1, 0);
    vector fn = normalize(fwd);
    vector ax = cross(fo, fn);
    normalize(ax);

    matrix mr;
    float angle = acosf(dot(fo, fn));
    vector rot = quaternion(angle, ax);
    rotation_matrix(rot, &mr);

    matrix mt;
    vector vt = get_vector(-pos.x, -pos.y, -pos.z, 0);
    translation_matrix(vt, &mt);

    mul_matrix(&mr, &mt, m);
}

// args: horizontal fov, 1 / aspect ratio, near plane, far plane
static inline void proj_matrix(float fov, float a, float n, float f, matrix *m)
{
    float e = 1 / tanf(fov / 2);
    float l = -n / e;
    float r = n / e;
    float t = (a * n) / e;
    float b = -(a * n) / e;

    memset(m, 0, sizeof(*m));
    m->m[0] = (2 * n) / (r - l);
    m->m[5] = -(2 * n) / (t - b); // negate because Vulkan - or not?
    m->m[8] = (r + l) / (r - l);
    m->m[9] = (t + b) / (t - b);
    m->m[10] = -(f + n) / (f - n);
    m->m[11] = -1;
    m->m[14] = -(2 * n * f) / (f - n);
}

// point of intersection of three planes, does not check det == 0
static inline vector intersect_three_planes(vector l1, vector l2, vector l3)
{
    matrix m;
    vector d = vector3(-l1.w, -l2.w, -l3.w);
    matrix3(vector3(l1.x, l2.x, l3.x), vector3(l1.y, l2.y, l3.y), vector3(l1.z, l2.z, l3.z), &m);

    invert(&m, &m);
    return mul_matrix_vector(&m, d);
}

// find the point of intersection of two planes l1 and l2, q1 and q2 are points on the respective planes
static inline vector intersect_two_planes_point(vector l1, vector l2, vector q1, vector q2)
{
    matrix m;
    vector v,q,d;
    float d1,d2;

    d1 = dot(vector3(-l1.x, -l1.y, -l1.z), vector3(q1.x, q1.y, q1.z));
    d2 = dot(vector3(-l2.x, -l2.y, -l2.z), vector3(q2.x, q2.y, q2.z));

    v = cross(l1, l2);
    d = vector3(-d1, -d2, 0);
    matrix3(vector3(l1.x, l2.x, v.x), vector3(l1.y, l2.y, v.y), vector3(l1.z, l2.z, v.z), &m);

    invert(&m, &m);
    q = mul_matrix_vector(&m, d);

    return add_vector(q, scale_vector(v, -dot(v, q) / dot(v, v)));
}

////////////////////////////////////////////////////////////////////////////////
// ascii.h

// This file uses a lot of unaligned loads as lots of these functions are used for parsing
// text, where a char could be at any place in the file. It is simple to make aligned
// versions (see Appendix at EOF for examples), but reading the intel intrinsics docs, there
// seems to be no penalty to unaligned loads vs aligned:
//
//     movdqu xmm, m128
//         Latency and Throughput
//         Architecture	Latency	Throughput (CPI)
//         Alderlake	    6	0.333333333
//         Icelake Xeon	    6	0.55
//         Sapphire Rapids	6	0.333333333
//         Skylake	        6	0.5
//
//     movdqa xmm, m128
//         Latency and Throughput
//         Architecture	Latency	Throughput (CPI)
//         Alderlake	    6	0.333333333
//         Icelake Xeon	    6	0.55
//         Sapphire Rapids	6	0.333333333
//         Skylake	        6	0.5
//
// This seems pretty bizarre, but I cannot imagine that the tables are wrong. My guess is
// I am misunderstanding the implications of the table, as maybe the load itself is longer,
// but the instructions themselves take the same time, but that seems a silly notion, as
// the instruction would be considered complete once the data is there. Idk... but I am
// not going to add cycles aligning the addresses if I cannot see a proper benefit.

static inline bool is_num_char(char c)
{
    return c >= '0' && c <= '9';
}

static inline bool is_hex_char(char c)
{
    return is_num_char(c) || is_in_srange_inc(c, 'A', 'F') || is_in_srange_inc(c, 'a', 'f');
}

static inline uint uint_to_ascii(uint32 num, char *buf) {
    uint cap = num < 10 ? 1 : log10(num) + 1;
    for(uint i = 0; i < cap; ++i) {
        buf[cap-i-1] = (num % 10) + '0';
        num /= 10;
    }
    buf[cap] = 0;
    return cap;
}

static inline uint32 simd_find_char(const char *data, char c) {
    __m128i a;
    __m128i b = _mm_set1_epi8(c);
    uint16 m0 = 0;
    uint32 i;
    for(i = 0; !m0; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data + i));
        a = _mm_cmpeq_epi8(a, b);
        m0 = _mm_movemask_epi8(a);
    }
    return i + ctz16(m0) - 16;
}

static inline uint32 simd_find_char_with_len(uint len, const char *data, char c) {
    __m128i a;
    __m128i b = _mm_set1_epi8(c);
    uint16 m0 = 0;
    uint32 i;
    for(i = 0; !m0; i += 16) {
        if (i >= len)
            return Max_u32;
        a = _mm_loadu_si128((__m128i*)(data + i));
        a = _mm_cmpeq_epi8(a, b);
        m0 = _mm_movemask_epi8(a);
    }
    return i + ctz16(m0) - 16;
}

static inline uint32 simd_find_number_char(const char *data) {
    __m128i a;
    __m128i b = _mm_set1_epi8('0' - 1);
    __m128i c = _mm_set1_epi8('9' + 1);
    __m128i d, e;
    uint16 m0 = 0;
    uint32 i;
    for(i = 0; !m0; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data + i));
        d = _mm_cmpgt_epi8(a, b);
        e = _mm_cmplt_epi8(a, c);
        d = _mm_and_si128(d, e);
        m0 = _mm_movemask_epi8(d);
    }
    return i + ctz16(m0) - 16;
}

static inline bool simd_find_char_interrupted(const char *data, char c0, char c1, uint32 *ret) {
    __m128i a;
    __m128i b = _mm_set1_epi8(c0);
    __m128i c = _mm_set1_epi8(c1);
    __m128i d;
    uint16 m0 = 0;
    uint16 m1 = 0;
    uint32 i;
    for(i = 0; !m0 && !m1; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data + i));
        d = _mm_cmpeq_epi8(a, b);
        m0 = _mm_movemask_epi8(d);
        d = _mm_cmpeq_epi8(a, c);
        m1 = _mm_movemask_epi8(d);
    }
    uint32 tz0 = ctz16(m0) | max32_if_false(m0);
    uint32 tz1 = ctz16(m1) | max32_if_false(m1);
    *ret = 0;
    *ret += (tz0 + i - 16) & max32_if_true(tz0 < tz1);
    *ret += (tz1 + i - 16) & max32_if_true(tz1 < tz0);
    return tz0 < tz1;
}

static inline uint16 simd_match_char(const char *data, char c) {
    __m128i a = _mm_loadu_si128((__m128i*)(data));
    __m128i b = _mm_set1_epi8(c);
    a = _mm_cmpeq_epi8(a, b);
    return _mm_movemask_epi8(a);
}

// only for contiguous number chars, does not count '.' or 'e'
static inline uint32 simd_ascii_integer_len(const char *data) {
    __m128i a;
    __m128i b = _mm_set1_epi8('0' - 1);
    __m128i c = _mm_set1_epi8('9' + 1);
    __m128i d, e;
    uint16 m0 = 0xffff;
    uint32 i;
    for(i = 0; m0 == 0xffff; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data + i));
        d = _mm_cmpgt_epi8(a, b);
        e = _mm_cmplt_epi8(a, c);
        d = _mm_and_si128(d, e);
        m0 = _mm_movemask_epi8(d);
    }
    m0 &= ~(0xffff << ctz16(~m0));
    return i - 16 + pop_count16(m0);
}

// Will count a trailing 'e', e.g. 123.456e, validate floats elsewhere.
static inline uint32 simd_ascii_double_len(const char *data) {
    __m128i a;
    __m128i b = _mm_set1_epi8('0' - 1);
    __m128i c = _mm_set1_epi8('9' + 1);
    __m128i d = _mm_set1_epi8('-');
    __m128i e = _mm_set1_epi8('e');
    __m128i f = _mm_set1_epi8('.');
    __m128i g,h;
    uint16 m0 = 0xffff;
    uint32 i;
    for(i = 0; m0 == 0xffff; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data + i));
        g = _mm_cmpgt_epi8(a, b);
        h = g;
        g = _mm_cmplt_epi8(a, c);
        h = _mm_and_si128(h, g);
        g = _mm_cmpeq_epi8(a, d);
        h = _mm_or_si128(h, g);
        g = _mm_cmpeq_epi8(a, e);
        h = _mm_or_si128(h, g);
        g = _mm_cmpeq_epi8(a, f);
        h = _mm_or_si128(h, g);
        m0 = _mm_movemask_epi8(h);
    }
    return i - 16 + ctz16(~m0);
}

static inline uint32 simd_ascii_double_e(const char *data) {
    __m128i a;
    __m128i b = _mm_set1_epi8('0' - 1);
    __m128i c = _mm_set1_epi8('9' + 1);
    __m128i d = _mm_set1_epi8('.');
    __m128i e, f;
    uint16 m0 = 0xffff;
    uint32 i;
    for(i = 0; m0 == 0xffff; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data + i));
        e = _mm_cmpgt_epi8(a, b);
        f = _mm_cmplt_epi8(a, c);
        e = _mm_and_si128(e, f);
        f = _mm_cmpeq_epi8(a, d);
        e = _mm_or_si128(e, f);
        m0 = _mm_movemask_epi8(e);
    }
    int tz0 = ctz16(~m0);
    return (i + tz0 - 16) | max32_if_false(data[i + tz0 - 16] == 'e');
}

static inline uint32 simd_skip_to_whitespace(const char *data) {
    __m128i a;
    __m128i b = _mm_set1_epi8(' ');
    __m128i c = _mm_set1_epi8('\n');
    __m128i d, e;
    uint16 m0 = 0;
    uint32 i;
    for(i = 0; !m0; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data + i));
        d = _mm_cmpeq_epi8(a, b);
        e = _mm_cmpeq_epi8(a, c);
        d = _mm_or_si128(d, e);
        m0 = _mm_movemask_epi8(d);
    }
    return i + ctz16(m0) - 16;
}

static inline uint32 simd_skip_over_whitespace(const char *data) {
    __m128i a;
    __m128i b = _mm_set1_epi8(' ');
    __m128i c = _mm_set1_epi8('\n');
    __m128i d, e;
    uint16 m0 = 0xffff;
    uint32 i;
    for(i = 0; m0 == 0xffff; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data + i));
        d = _mm_cmpeq_epi8(a, b);
        e = _mm_cmpeq_epi8(a, c);
        d = _mm_or_si128(d, e);
        m0 = _mm_movemask_epi8(d);
    }
    return i + ctz16(~m0) - 16;
}

static inline bool simd_find_char_interrupted_by_not_whitespace(const char *data, char x) {
    __m128i a;
    __m128i b = _mm_set1_epi8(x);
    __m128i c = _mm_set1_epi8(' ');
    __m128i d = _mm_set1_epi8('\n');
    __m128i e;
    uint16 m0;
    uint16 m1 = 0xffff;
    for(int i = 0; m1 == 0xffff; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data + i));
        e = _mm_cmpeq_epi8(a, b);
        m0 = _mm_movemask_epi8(e);
        e = _mm_cmpeq_epi8(a, c);
        m1 = _mm_movemask_epi8(e);
        e = _mm_cmpeq_epi8(a, d);
        m1 |= _mm_movemask_epi8(e);
    }
    int lz0 = ctz16(m0);
    int lz1 = ctz16(~m1);
    return lz0 == lz1 && pop_count16(m0);
}

static inline bool simd_find_char_interrupted_by_not_whitespace_reverse(const char *data, char x) {
    __m128i a;
    __m128i b = _mm_set1_epi8(x);
    __m128i c = _mm_set1_epi8(' ');
    __m128i d = _mm_set1_epi8('\n');
    __m128i e;
    uint16 m0;
    uint16 m1 = 0xffff;
    for(int i = 0; m1 == 0xffff; i += 16) {
        a = _mm_loadu_si128((__m128i*)(data - i));
        e = _mm_cmpeq_epi8(a, b);
        m0 = _mm_movemask_epi8(e);
            e = _mm_cmpeq_epi8(a, c);
        m1 = _mm_movemask_epi8(e);
        e = _mm_cmpeq_epi8(a, d);
        m1 |= _mm_movemask_epi8(e);
    }
    int lz0 = clz16(m0);
    int lz1 = clz16(~m1);
    return lz0 == lz1 && pop_count16(m0);
}

static inline int64 ascii_to_integer(const char *data) {
    data += simd_find_number_char(data);
    int64 ret = 0;
    for(uint32 i = 0; data[i] >= '0' && data[i] <= '9'; ++i) {
        ret *= 10;
        ret += data[i] - '0';
    }
    uint64 tmp = ret;
    ret -= tmp & max64_if_true(data[-1] == '-');
    ret -= tmp & max64_if_true(data[-1] == '-');
    return ret;
}

// This does not search forward so is not as friendly which is bad design.
static inline int64 ascii_hex_to_integer(const char *data) {
    if (data[0] == 0 && data[1] == 'x')
        data += 2;
    else if (data[0] == '#')
        data++;

    int64 ret = 0;
    for(uint32 i = 0; is_hex_char(data[i]); ++i) {
        ret *= 16;
        if (is_num_char(data[i])) {
            ret += data[i] - '0';
        } else if (is_in_srange_inc(data[i], 'a', 'f')) {
            ret += 10 + (data[i] - 'a');
        } else if (is_in_srange_inc(data[i], 'A', 'F')) {
            ret += 10 + (data[i] - 'A');
        }
    }
    uint64 tmp = ret;
    ret -= tmp & max64_if_true(data[-1] == '-');
    ret -= tmp & max64_if_true(data[-1] == '-');
    return ret;
}

static inline double ascii_to_double(const char *data) {
    data += simd_find_number_char(data);
    int len_bd = simd_ascii_integer_len(data);
    int len_ad = simd_ascii_integer_len(data + len_bd + 1) & max32_if_true(data[len_bd] == '.');
    int64 bd = ascii_to_integer(data);
    int64 ad = ascii_to_integer(data + ((len_bd + 1) & max32_if_true(len_ad)));

    double ret = ad & max64_if_true(len_ad);
    ret /= pow(10, len_ad);
    ret += bd < 0 ? -bd : bd;

    uint32 e_idx = simd_ascii_double_e(data);
    e_idx &= max32_if_true(e_idx != Max_u32);
    int64 exp = ascii_to_integer(data + e_idx);
    exp &= max64_if_true(e_idx);

    ret *= pow(10, exp);
    ret = data[-1] == '-' && ret > 0 ? -ret : ret;

    return ret;
}

////////////////////////////////////////////////////////////////////////////////
// allocator.h

/*
    Code that would normally use an allocator uses the standard allocator
    #ifndef SOL_ALLOC. In this case, any 'allocator*' arg can be passed as NULL.
*/

#define ALLOCATOR_ALIGNMENT 16

#define allocalign(sz) align(sz, ALLOCATOR_ALIGNMENT)
#define allocalign_type(t) align(sizeof(t), ALLOCATOR_ALIGNMENT)

struct allocation {
    void *data;
    size_t size;
};

typedef struct heap_allocator {
    uint64 cap;
    uint64 used;
    uint8 *mem;
    void *tlsf_handle;
} heap_allocator;

typedef struct linear_allocator {
    uint64 cap;
    uint64 used;
    uint8 *mem;
} linear_allocator;

#define ARENA_FOOTER_CHECK_BYTES 0xfeedbeef

struct arena_footer {
    uint check_bytes;
    uint allocation_count;
    size_t used;
    size_t cap;
    void *base;
    struct arena_footer *next;
};

typedef struct arena_allocator {
    struct arena_footer *tail;
    struct arena_footer *head;
    size_t max_total_size;
    size_t min_block_size;
} arena_allocator;

typedef enum {
    ALLOCATOR_ARENA_BIT       = 0x001,
    ALLOCATOR_LINEAR_BIT      = 0x002,
    ALLOCATOR_DO_NOT_FREE_BIT = 0x100,

    ALLOCATOR_TYPE_BITS = ALLOCATOR_LINEAR_BIT | ALLOCATOR_ARENA_BIT,
} allocator_flag_bits;

// @Todo new_allocator should not return by value, I just realised how big
// allocator is with all the function pointers.

typedef struct allocator allocator;
typedef void* (*allocator_allocate)  (allocator*, size_t);
typedef void* (*allocator_reallocate)(allocator*, void *, size_t, size_t);
typedef void  (*allocator_deallocate)(allocator*, void*);

struct allocator {
    uint32 flags;
    allocator_allocate allocate;
    allocator_reallocate reallocate;
    allocator_deallocate deallocate;
    union {
        linear_allocator linear;
        arena_allocator arena;
    };
};

#ifdef SOL_ALLOC
void init_allocator(allocator *alloc, size_t cap, void *buffer, allocator_flag_bits type);
void free_allocator(allocator *alloc);
#else
static inline void init_allocator(allocator *alloc, size_t cap, void *buffer, allocator_flag_bits type) {}
static inline void free_allocator(allocator *alloc) {}
#endif

#define init_heap_allocator(alloc, cap, buffer)   init_allocator(alloc, cap, buffer, ALLOCATOR_HEAP_BIT)
#define init_linear_allocator(alloc, cap, buffer) init_allocator(alloc, cap, buffer, ALLOCATOR_LINEAR_BIT)
#define init_arena_allocator(alloc, cap)          init_allocator(alloc, cap, NULL, ALLOCATOR_ARENA_BIT)

static inline void *allocate(allocator *alloc, size_t size) {
#ifdef SOL_ALLOC
    return alloc->allocate(alloc, size);
#else
    return malloc(size);
#endif
}

static inline void *reallocate(allocator *alloc, void *ptr, size_t old_size, size_t new_size) {
#ifdef SOL_ALLOC
    return alloc->reallocate(alloc, ptr, old_size, new_size);
#else
    return realloc(ptr, new_size);
#endif
}

static inline void deallocate(allocator *alloc, void *ptr) {
#ifdef SOL_ALLOC
    return alloc->deallocate(alloc, ptr);
#else
    return free(ptr);
#endif
}

#define sallocate(alloc, type, count) allocate(alloc, sizeof(type) * count)
#define sreallocate(alloc, ptr, type, count) reallocate(alloc, ptr, sizeof(type) * count)
#define sallocate_unaligned(alloc, type, count) allocate_unaligned(alloc, sizeof(type), sizeof(type) * count)

static inline size_t allocator_used(allocator *alloc) {
    switch(alloc->flags & ALLOCATOR_TYPE_BITS) {
    case ALLOCATOR_ARENA_BIT:
        return Max_u64; // arena_used(); @Todo
    case ALLOCATOR_LINEAR_BIT:
        return alloc->linear.used;
    default:
        assert(false && "Invalid allocator flags");
        return Max_u64;
    }
}

static inline void allocator_reset_linear(allocator *alloc) {
    if (alloc->flags & ALLOCATOR_LINEAR_BIT)
        alloc->linear.used = 0;
}

static inline void allocator_reset_linear_to(allocator *alloc, uint64 to) {
    assert(to <= alloc->linear.used);
    if (alloc->flags & ALLOCATOR_LINEAR_BIT)
        alloc->linear.used = to;
}

////////////////////////////////////////////////////////////////////////////////
// ringbuffer.h

enum {
    RB_NOTAIL = 0x01,
};

typedef struct ringbuffer {
    uchar *data;
    uint flags;
    int size;
    int head;
    int tail;
} ringbuffer;

bool init_ringbuffer(ringbuffer *buf, uint size, uint flags);
void* rballoc(ringbuffer *rb, uint size);
bool rbfree(ringbuffer *rb, uint size);

////////////////////////////////////////////////////////////////////////////////
// array.h

#define ARRAY_METADATA_WIDTH 4
#define ARRAY_METADATA_SIZE (sizeof(uint64) * ARRAY_METADATA_WIDTH)

// Backend
static inline void* fn_new_array(uint64 cap, uint64 width, allocator *alloc) {
    uint64 *ret = allocate(alloc, cap * width + ARRAY_METADATA_SIZE);
    ret[0] = width * cap;
    ret[1] = 0;
    ret[2] = width;
    ret[3] = (uint64)alloc;
    ret += ARRAY_METADATA_WIDTH;
    assert(((uint64)ret % 16) == 0); // align for SIMD
    return ret;
}

static inline void* fn_realloc_array(void *a) {
    uint64 *array = (uint64*)a - ARRAY_METADATA_WIDTH;
    if (array[0] <= array[1] * array[2]) {
        array = reallocate((allocator*)(array[3]),
                array, array[0] + ARRAY_METADATA_SIZE, array[0] * 2 + ARRAY_METADATA_SIZE);
        assert(array);
        array[0] *= 2;
    }
    assert(((uint64)(array + ARRAY_METADATA_WIDTH) % 16) == 0); // align for SIMD
    return array + ARRAY_METADATA_WIDTH;
}

static inline size_t array_cap(void *array) {
    return ((uint64*)array - 4)[0] / ((uint64*)array - 4)[2];
}

static inline size_t array_len(void *array) {
    return ((uint64*)array - 4)[1];
}

static inline void array_set_len(void *array, size_t len) {
    ((uint64*)array - 4)[1] = len;
}

static inline void reset_array(void *array)
{
    array_set_len(array, 0);
}

static inline size_t array_elem_width(void *array)
{
    return ((uint64*)array-4)[2];
}

static inline size_t array_byte_len(void *a)
{
    return array_elem_width(a)*array_len(a);
}

static inline struct allocation array_allocation(void *a)
{
    struct allocation ret;
    ret.size = array_byte_len(a)+ARRAY_METADATA_SIZE;
    ret.data = (uint64*)a-4;
    return ret;
}

static inline void array_inc(void *array) {
   ((uint64*)array - 4)[1] += 1;
}

static inline void array_dec(void *array) {
   ((uint64*)array - 4)[1] -= 1;
}

static inline void free_array(void *a) {
    uint64 *array = (uint64*)a - 4;
    allocator *alloc = (allocator*)(array[3]);
    deallocate(alloc, array);
}

void* load_array(const char *fname, allocator *alloc);
void store_array(const char *fname, void *a);

// Frontend
#define new_array(cap, type, alloc) fn_new_array(cap, sizeof(type), alloc)
#define array_last(array) (array[array_len(array)-1])
#define array_add(array, elem) \
    do { \
        array = fn_realloc_array(array); \
        array[array_len(array)] = elem; \
        array_inc(array); \
    } while (0)

////////////////////////////////////////////////////////////////////////////////
// string.h

// @Note Below is the justification for my new type and function naming
// conventions:
//
// Short function names; Descriptive type names.
//
// I want to have a proper little think about the sorts of interface naming
// conventions I want to have. I personally really like the very terse language
// of the C standard functions, linux functions, etc., but I prefer more
// descriptive type names. Certain functions, like string_buffer_get_string()
// are not annoying to type once perhaps, but if you are iterating designs
// and deleting and retyping, or just trying to shit smtg out because you know
// exactly what you need, I certainly notice that I am typing long variable
// or function names. Type names not so much, as variable declarations are
// relatively rare.
//
// Typing strbuf_get() I think is better than string_buffer_get_string().
// By seeing the descriptive type name, it is clear what 'strbuf' is an
// abbreviation of, so there is little confusion in the function usage, such
// as whether the caller is using something which acts on a string_builder
// or string_buffer. After thinking it through, I will at least stick to this
// philosophy in future, but I doubt I will convert many interfaces to it, that
// would be a waste.

typedef struct {
    const char *cstr;
    uint64 len;
} string;

enum {
    STRING_BUFFER_REALLOCATE_BIT = 0x01,
    // if a string buffer reallocates, strings using it will be invalidated;
    // sometimes this is fine, as perhaps the buffer is being grown before
    // strings are directed to it, but being able to control this behaviour
    // is useful.
};

typedef struct {
    uint flags;
    char *data;
    uint used;
    uint cap;
    allocator *alloc;
} string_buffer;

typedef struct string_array {
    struct pair_uint *r; // dynamic array of string offsets and lengths
    string_buffer b;
} string_array;

// **THIS SAME COMMENT APPLIES TO THE ABOVE 'string'**
// Debating removing typedef. I used to more easily justify typedefs
// as being structs which are used in a sort of dynamic manner (a good
// symbol begin a verb in the name i.e. '_builder') but the more I use
// C, the I like the struct keyword to differentiate them from basic types.
typedef struct {
    uint32 used;
    uint32 cap;
    char *data;
} string_builder;

static inline string cstr_to_str(const char *cstr)
{
    return (string){.cstr = cstr, .len = strlen(cstr)};
}

string_buffer new_strbuf(uint64 cap, uint flags, allocator *alloc);
void free_strbuf(string_buffer *buf);

static inline string_buffer new_dyn_strbuf(uint64 cap, allocator *alloc)
{
    return new_strbuf(cap, STRING_BUFFER_REALLOCATE_BIT, alloc);
}
static inline string_buffer new_static_strbuf(uint64 cap, allocator *alloc)
{
    return new_strbuf(cap, 0x0, alloc);
}

string strbuf_add(string_buffer *buf, string *str);
string strbuf_add_cstr(string_buffer *buf, const char *cstr);

static inline void reset_strbuf(string_buffer *b)
{
    b->used = 0;
}

static inline string_array new_strarr(uint initial_buffer_size, uint initial_array_len,
                                      allocator *alloc)
{
    string_array ret;
    ret.r = new_array(initial_array_len, *ret.r, alloc);
    ret.b = new_dyn_strbuf(initial_buffer_size, alloc);
    return ret;
}

static inline void free_strarr(string_array *sa)
{
    free_strbuf(&sa->b);
    free_array(sa->r);
}

static inline uint strarr_len(string_array *arr)
{
    return array_len(arr->r);
}

static inline void strarr_add(string_array *arr, string *str)
{
    strbuf_add(&arr->b, str);
    struct pair_uint r = { // +-1 for null byte
        .a = arr->b.used - (str->len+1),
        .b = str->len,
    };
    array_add(arr->r, r);
}

static inline void strarr_add_cstr(string_array *arr, const char *cstr)
{
    strbuf_add_cstr(&arr->b, cstr);
    uint len = strlen(cstr);
    struct pair_uint r = { // +-1 for null byte
        .a = arr->b.used - (len+1),
        .b = len,
    };
    array_add(arr->r, r);
}

static inline string strarr_get(string_array *arr, uint i)
{
    return (string){
        .cstr = arr->b.data + arr->r[i].a,
        .len = arr->r[i].b,
    };
}

static inline void reset_strarr(string_array *arr)
{
    reset_array(arr->r);
    reset_strbuf(&arr->b);
}

string_array str_split(string *str, char split_char, allocator *alloc);

// @Note Although one can argue that 'sb_' prefix clashes with string_buffer,
// the descriptive type names and the compiler assurance of type safety to me
// nullifies the complaint. string_builder functions are often called frequently
// and repeatedly, making a longer prefix more frustrating to use than one which
// is more distinguishing.
static inline string_builder sb_new(uint32 size, char *data)
{
    return (string_builder){0, size, data};
}

static inline void sb_inc(string_builder *sb, uint sz)
{
    assert(sb->used + sz <= sb->cap);
    sb->used += sz;
}

static inline void sb_null_term(string_builder *sb)
{
    assert(sb->used+1 <= sb->cap);
    sb->data[sb->used] = 0;
    sb->used++;
}

static inline void sb_add(string_builder *sb, uint32 size, const char *data)
{
    assert(sb->used + size <= sb->cap);
    memcpy(sb->data + sb->used, data, size);
    sb->used += size;
}

static inline void sb_addc(string_builder *sb, const char c)
{
    assert(sb->used + 1 <= sb->cap);
    sb->data[sb->used] = c;
    sb->used++;
}

static inline void sb_adduint(string_builder *sb, uint i)
{
    assert(i < 1000000);
    char nb[8];
    uint nl = uint_to_ascii(i,nb);
    assert(sb->used + nl <= sb->cap);
    memcpy(sb->data+sb->used,nb,nl);
    sb->used += nl;
}

static inline void sb_addnl(string_builder *sb)
{
    assert(sb->used + 1 <= sb->cap);
    sb->data[sb->used] = '\n';
    sb->used++;
}

static inline void sb_add_if(string_builder *sb, uint32 size, const char *data, bool b)
{
    assert(sb->used + size <= sb->cap);
    size &= max_if(b);
    memcpy(sb->data + sb->used, data, size);
    sb->used += size;
}

static inline void sb_addc_if(string_builder *sb, const char c, bool b)
{
    assert(sb->used + 1 <= sb->cap);
    sb->data[sb->used] = c;
    sb->used += b;
}

static inline void sb_addnl_if(string_builder *sb, bool b)
{
    assert(sb->used + 1 <= sb->cap);
    sb->data[sb->used] = '\n';
    sb->used += b;
}

static inline void sb_endl_if(string_builder *sb, bool b) {
    assert(sb->used + 2 <= sb->cap);
    sb->data[sb->used] = ';';
    sb->data[sb->used+1] = '\n';
    sb->used += b + b;
}

static inline void sb_addarr(string_builder *sb, uint32 before_size, const char *before,
        uint32 after_size, const char *after, uint32 n)
{
    sb_add(sb, before_size, before);
    sb_addc(sb, '[');
    char nb[8];
    uint nl = uint_to_ascii(n, nb);
    sb_add(sb, nl, nb);
    sb_addc(sb, ']');
    sb_add(sb, after_size, after);
}

static inline void sb_insertnum(string_builder *sb, uint32 before_size, const char *before,
        uint32 after_size, const char *after, uint32 n)
{
    sb_add(sb, before_size, before);
    char nb[8];
    uint nl = uint_to_ascii(n, nb);
    sb_add(sb, nl, nb);
    sb_add(sb, after_size, after);
}

static inline void sb_addarr_if(string_builder *sb, uint32 before_size, const char *before,
        uint32 after_size, const char *after, uint32 n, bool b)
{
    sb_add_if(sb, before_size, before, b);
    sb_addc_if(sb, '[', b);
    char nb[8];
    uint nl = uint_to_ascii(n, nb);
    sb_add_if(sb, nl, nb, b);
    sb_addc_if(sb, ']', b);
    sb_add_if(sb, after_size, after, b);
}

static inline void sb_close_arr_and_endl_if(string_builder *sb, bool b) {
    assert(sb->used + 3 <= sb->cap);
    sb->data[sb->used+0] = ']';
    sb->data[sb->used+1] = ';';
    sb->data[sb->used+2] = '\n';
    sb->used += b*3;
}

static inline void sb_insertnum_if(string_builder *sb, uint32 before_size, const char *before,
        uint32 after_size, const char *after, uint32 n, bool b)
{
    sb_add_if(sb, before_size, before, b);
    char nb[8];
    uint nl = uint_to_ascii(n, nb);
    sb_add_if(sb, nl, nb, b);
    sb_add_if(sb, after_size, after, b);
}

static inline void sb_add_digit_if(string_builder *sb, uint n, bool b)
{
    assert(sb->used+1 <= sb->cap);
    sb->data[sb->used] &= zero_if(b);
    sb->data[sb->used] += (n + '0') & max_if(b);
    sb->used += b;
}

static inline void sb_replace(string_builder *sb, uint i, uint len, const char *str)
{
    assert(i+len < sb->used);
    memcpy(sb->data+i,str,len);
}

static inline void sb_replace_c(string_builder *sb, uint i, char c)
{
    assert(i < sb->used);
    sb->data[i] = c;
}

static inline void sb_replace_digit(string_builder *sb, uint i, int n)
{
    assert(i < sb->used);
    sb->data[i] = n + '0';
}

static inline void sb_replace_digit_if(string_builder *sb, uint i, int n, bool b)
{
    assert(i < sb->used || !b);
    sb->data[i] &= zero_if(b);
    sb->data[i] += (n + '0') & max_if(b);
}

static inline void sb_replace_uint(string_builder *sb, uint i, int u)
{
    assert(i < sb->used);
    assert(u < 1000000);
    char nb[8];
    uint nl = uint_to_ascii(u,nb);
    assert(sb->used + nl <= sb->cap);
    memcpy(sb->data+sb->used,nb,nl);
}

////////////////////////////////////////////////////////////////////////////////
// file.h

struct file {
    char *data;
    size_t size;
};

struct dir {
    string_array f; // files
    string_array d; // directories
};

// @Note breaks on windows?
static inline uint get_file_parent_dir(const char *file_name, char *buf) {
    int p = 0;
    int len = strlen(file_name);
    for(uint i = 0; i < len; ++i) {
        if (file_name[i] == '/')
            p = i;
        buf[i] = file_name[i];
    }
    buf[p+1] = 0;
    return (uint)p+1;
}

static inline void file_last_modified(const char *path, struct timespec *ts) {
    struct stat s;
    int err = stat(path, &s);
    log_print_error_if(err, "failed to stat file %s", path);
    smemcpy(ts, &s.st_mtim, s.st_mtim, 1);
}

static inline bool has_file_changed(const char *path, struct timespec *then) {
    struct timespec now;
    file_last_modified(path, &now);
    return now.tv_sec != then->tv_sec || now.tv_nsec != then->tv_nsec;
}

struct file file_read_bin_all(const char *file_name, allocator *alloc);
struct file file_read_char_all(const char *file_name, allocator *alloc);
void file_read_bin_size(const char *file_name, size_t size, void *buffer);
void file_read_char_count(const char *file_name, size_t count, char *buffer);

void file_write_bin(const char *file_name, size_t size, const void *data);
void file_write_char(const char *file_name, size_t count, const char *data);
void file_append_bin(const char *file_name, size_t size, const void *data);
void file_append_char(const char *file_name, size_t count, const char *data);

struct dir getdir(const char *name, allocator *alloc);

////////////////////////////////////////////////////////////////////////////////
// hashmap.h

/*
                                               ** BEGIN WYHASH **

    -- This is not my (Solomon Brown's) code:
           see wyhash github link at file start and below, wyhash full license before dict implementation

*/

// This is free and unencumbered software released into the public domain under The Unlicense (http://unlicense.org/)
// main repo: https://github.com/wangyi-fudan/wyhash
// author: 王一 Wang Yi <godspeed_china@yeah.net>
// contributors: Reini Urban, Dietrich Epp, Joshua Haberman, Tommy Ettinger, Daniel Lemire, Otmar Ertl, cocowalla, leo-yuriev, Diego Barrios Romero, paulie-g, dumblob, Yann Collet, ivte-ms, hyb, James Z.M. Gao, easyaspi314 (Devin), TheOneric


#ifndef wyhash_final_version_4
#define wyhash_final_version_4

#ifndef WYHASH_CONDOM
//protections that produce different results:
//1: normal valid behavior
//2: extra protection against entropy loss (probability=2^-63), aka. "blind multiplication"
#define WYHASH_CONDOM 1
#endif

#ifndef WYHASH_32BIT_MUM
//0: normal version, slow on 32 bit systems
//1: faster on 32 bit systems but produces different results, incompatible with wy2u0k function
#define WYHASH_32BIT_MUM 0
#endif

//includes
#include <stdint.h>
#include <string.h>
#if defined(_MSC_VER) && defined(_M_X64)
  #include <intrin.h>
  #pragma intrinsic(_umul128)
#endif

//likely and unlikely macros
#if defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)
  #define _likely_(x)  __builtin_expect(x,1)
  #define _unlikely_(x)  __builtin_expect(x,0)
#else
  #define _likely_(x) (x)
  #define _unlikely_(x) (x)
#endif

//128bit multiply function
static inline uint64_t _wyrot(uint64_t x) { return (x>>32)|(x<<32); }
static inline void _wymum(uint64_t *A, uint64_t *B){
#if(WYHASH_32BIT_MUM)
  uint64_t hh=(*A>>32)*(*B>>32), hl=(*A>>32)*(uint32_t)*B, lh=(uint32_t)*A*(*B>>32), ll=(uint64_t)(uint32_t)*A*(uint32_t)*B;
  #if(WYHASH_CONDOM>1)
  *A^=_wyrot(hl)^hh; *B^=_wyrot(lh)^ll;
  #else
  *A=_wyrot(hl)^hh; *B=_wyrot(lh)^ll;
  #endif
#elif defined(__SIZEOF_INT128__)
  __uint128_t r=*A; r*=*B;
  #if(WYHASH_CONDOM>1)
  *A^=(uint64_t)r; *B^=(uint64_t)(r>>64);
  #else
  *A=(uint64_t)r; *B=(uint64_t)(r>>64);
  #endif
#elif defined(_MSC_VER) && defined(_M_X64)
  #if(WYHASH_CONDOM>1)
  uint64_t  a,  b;
  a=_umul128(*A,*B,&b);
  *A^=a;  *B^=b;
  #else
  *A=_umul128(*A,*B,B);
  #endif
#else
  uint64_t ha=*A>>32, hb=*B>>32, la=(uint32_t)*A, lb=(uint32_t)*B, hi, lo;
  uint64_t rh=ha*hb, rm0=ha*lb, rm1=hb*la, rl=la*lb, t=rl+(rm0<<32), c=t<rl;
  lo=t+(rm1<<32); c+=lo<t; hi=rh+(rm0>>32)+(rm1>>32)+c;
  #if(WYHASH_CONDOM>1)
  *A^=lo;  *B^=hi;
  #else
  *A=lo;  *B=hi;
  #endif
#endif
}

//multiply and xor mix function, aka MUM
static inline uint64_t _wymix(uint64_t A, uint64_t B){ _wymum(&A,&B); return A^B; }

//endian macros
#ifndef WYHASH_LITTLE_ENDIAN
  #if defined(_WIN32) || defined(__LITTLE_ENDIAN__) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
    #define WYHASH_LITTLE_ENDIAN 1
  #elif defined(__BIG_ENDIAN__) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    #define WYHASH_LITTLE_ENDIAN 0
  #else
    #warning could not determine endianness! Falling back to little endian.
    #define WYHASH_LITTLE_ENDIAN 1
  #endif
#endif

//read functions
#if (WYHASH_LITTLE_ENDIAN)
static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return v;}
static inline uint64_t _wyr4(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return v;}
#elif defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)
static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return __builtin_bswap64(v);}
static inline uint64_t _wyr4(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return __builtin_bswap32(v);}
#elif defined(_MSC_VER)
static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return _byteswap_uint64(v);}
static inline uint64_t _wyr4(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return _byteswap_ulong(v);}
#else
static inline uint64_t _wyr8(const uint8_t *p) {
  uint64_t v; memcpy(&v, p, 8);
  return (((v >> 56) & 0xff)| ((v >> 40) & 0xff00)| ((v >> 24) & 0xff0000)| ((v >>  8) & 0xff000000)| ((v <<  8) & 0xff00000000)| ((v << 24) & 0xff0000000000)| ((v << 40) & 0xff000000000000)| ((v << 56) & 0xff00000000000000));
}
static inline uint64_t _wyr4(const uint8_t *p) {
  uint32_t v; memcpy(&v, p, 4);
  return (((v >> 24) & 0xff)| ((v >>  8) & 0xff00)| ((v <<  8) & 0xff0000)| ((v << 24) & 0xff000000));
}
#endif
static inline uint64_t _wyr3(const uint8_t *p, size_t k) { return (((uint64_t)p[0])<<16)|(((uint64_t)p[k>>1])<<8)|p[k-1];}
//wyhash main function
static inline uint64_t wyhash(const void *key, size_t len, uint64_t seed, const uint64_t *secret){
  const uint8_t *p=(const uint8_t *)key; seed^=_wymix(seed^secret[0],secret[1]);	uint64_t	a,	b;
  if(_likely_(len<=16)){
    if(_likely_(len>=4)){ a=(_wyr4(p)<<32)|_wyr4(p+((len>>3)<<2)); b=(_wyr4(p+len-4)<<32)|_wyr4(p+len-4-((len>>3)<<2)); }
    else if(_likely_(len>0)){ a=_wyr3(p,len); b=0;}
    else a=b=0;
  }
  else{
    size_t i=len;
    if(_unlikely_(i>48)){
      uint64_t see1=seed, see2=seed;
      do{
        seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed);
        see1=_wymix(_wyr8(p+16)^secret[2],_wyr8(p+24)^see1);
        see2=_wymix(_wyr8(p+32)^secret[3],_wyr8(p+40)^see2);
        p+=48; i-=48;
      }while(_likely_(i>48));
      seed^=see1^see2;
    }
    while(_unlikely_(i>16)){  seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed);  i-=16; p+=16;  }
    a=_wyr8(p+i-16);  b=_wyr8(p+i-8);
  }
  a^=secret[1]; b^=seed;  _wymum(&a,&b);
  return  _wymix(a^secret[0]^len,b^secret[1]);
}

//the default secret parameters
static const uint64_t _wyp[4] = {0xa0761d6478bd642full, 0xe7037ed1a0b428dbull, 0x8ebc6af09c88c6e3ull, 0x589965cc75374cc3ull};

//a useful 64bit-64bit mix function to produce deterministic pseudo random numbers that can pass BigCrush and PractRand
static inline uint64_t wyhash64(uint64_t A, uint64_t B){ A^=0xa0761d6478bd642full; B^=0xe7037ed1a0b428dbull; _wymum(&A,&B); return _wymix(A^0xa0761d6478bd642full,B^0xe7037ed1a0b428dbull);}

//The wyrand PRNG that pass BigCrush and PractRand
static inline uint64_t wyrand(uint64_t *seed){ *seed+=0xa0761d6478bd642full; return _wymix(*seed,*seed^0xe7037ed1a0b428dbull);}

//convert any 64 bit pseudo random numbers to uniform distribution [0,1). It can be combined with wyrand, wyhash64 or wyhash.
static inline double wy2u01(uint64_t r){ const double _wynorm=1.0/(1ull<<52); return (r>>12)*_wynorm;}

//convert any 64 bit pseudo random numbers to APPROXIMATE Gaussian distribution. It can be combined with wyrand, wyhash64 or wyhash.
static inline double wy2gau(uint64_t r){ const double _wynorm=1.0/(1ull<<20); return ((r&0x1fffff)+((r>>21)&0x1fffff)+((r>>42)&0x1fffff))*_wynorm-3.0;}

#ifdef	WYTRNG
#include <sys/time.h>
//The wytrand true random number generator, passed BigCrush.
static inline uint64_t wytrand(uint64_t *seed){
	struct	timeval	t;	gettimeofday(&t,0);
	uint64_t	teed=(((uint64_t)t.tv_sec)<<32)|t.tv_usec;
	teed=_wymix(teed^_wyp[0],*seed^_wyp[1]);
	*seed=_wymix(teed^_wyp[0],_wyp[2]);
	return _wymix(*seed,*seed^_wyp[3]);
}
#endif

#if(!WYHASH_32BIT_MUM)
//fast range integer random number generation on [0,k) credit to Daniel Lemire. May not work when WYHASH_32BIT_MUM=1. It can be combined with wyrand, wyhash64 or wyhash.
static inline uint64_t wy2u0k(uint64_t r, uint64_t k){ _wymum(&r,&k); return k; }
#endif

//make your own secret
static inline void make_secret(uint64_t seed, uint64_t *secret){
  uint8_t c[] = {15, 23, 27, 29, 30, 39, 43, 45, 46, 51, 53, 54, 57, 58, 60, 71, 75, 77, 78, 83, 85, 86, 89, 90, 92, 99, 101, 102, 105, 106, 108, 113, 114, 116, 120, 135, 139, 141, 142, 147, 149, 150, 153, 154, 156, 163, 165, 166, 169, 170, 172, 177, 178, 180, 184, 195, 197, 198, 201, 202, 204, 209, 210, 212, 216, 225, 226, 228, 232, 240 };
  for(size_t i=0;i<4;i++){
    uint8_t ok;
    do{
      ok=1; secret[i]=0;
      for(size_t j=0;j<64;j+=8) secret[i]|=((uint64_t)c[wyrand(&seed)%sizeof(c)])<<j;
      if(secret[i]%2==0){ ok=0; continue; }
      for(size_t j=0;j<i;j++) {
#if defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)
        if(__builtin_popcountll(secret[j]^secret[i])!=32){ ok=0; break; }
#elif defined(_MSC_VER) && defined(_M_X64)
        if(_mm_popcnt_uint64(secret[j]^secret[i])!=32){ ok=0; break; }
#else
        //manual popcount
        uint64_t x = secret[j]^secret[i];
        x -= (x >> 1) & 0x5555555555555555;
        x = (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333);
        x = (x + (x >> 4)) & 0x0f0f0f0f0f0f0f0f;
        x = (x * 0x0101010101010101) >> 56;
        if(x!=32){ ok=0; break; }
#endif
      }
    }while(!ok);
  }
}

#endif
                             /* WYHASH LICENCE */
/* The Unlicense
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>
*/

#define HASH_MAP_FULL        0b01111111
#define HASH_MAP_EMPTY       0b11111111
#define HASH_MAP_DELETED     0b10000000
#define HASH_MAP_GROUP_WIDTH 16

typedef struct {
    int cap;
    int remaining;
    int kv_stride;
    uint8 *data;
    bool resize;
    allocator *alloc;
} hash_map;

bool  fn_hash_map_insert_hash(hash_map *map, uint64 hash, void *elem, int elem_width);
void  fn_hash_map_if_full(hash_map *map, int elem_width);
void* fn_hash_map_find_hash(hash_map *map, uint64 hash);
void* fn_hash_map_delete_hash(hash_map *map, uint64 hash);

                        /* Frontend Functions */
static inline uint64 get_hash(int byte_len, void *bytes) {
    return wyhash(bytes, byte_len, 0, _wyp);
}

static inline hash_map fn_new_hash_map(int cap, int elem_width, bool resize, allocator *alloc) {
    hash_map ret  = {};
    ret.cap       = align(cap, 16);
    ret.remaining = ((cap + 1) / 8) * 7;
    ret.kv_stride = align(8 + elem_width, 16);
    ret.data      = allocate(alloc, ret.cap + ret.cap * ret.kv_stride);
    ret.alloc = alloc;
    ret.resize = resize;
    memset(ret.data, HASH_MAP_EMPTY, ret.cap);
    return ret;
}

static inline void fn_free_hash_map(hash_map *map) {
    deallocate(map->alloc, map->data);
}

static inline bool fn_hash_map_insert(hash_map *map, int byte_len, void *key, void *elem, int elem_width) {
    if (map->remaining == 0)
        fn_hash_map_if_full(map, elem_width);

    uint64 hash = wyhash(key, byte_len, 0, _wyp);
    return fn_hash_map_insert_hash(map, hash, elem, elem_width);
}
static inline bool fn_hash_map_insert_str(hash_map *map, const char *key, void *elem, int elem_width) {
    if (map->remaining == 0)
        fn_hash_map_if_full(map, elem_width);

    uint64 hash = wyhash(key, strlen(key), 0, _wyp);
    return fn_hash_map_insert_hash(map, hash, elem, elem_width);
}

static inline void* fn_hash_map_find(hash_map *map, int byte_len, void *bytes) {
    uint64 hash = wyhash(bytes, byte_len, 0, _wyp);
    return fn_hash_map_find_hash(map, hash);
}
static inline void* fn_hash_map_find_str(hash_map *map, const char* key) {
    uint64 hash = wyhash(key, strlen(key), 0, _wyp);
    return fn_hash_map_find_hash(map, hash);
}

static inline void* fn_hash_map_delete(hash_map *map, int byte_len, void *bytes) {
    uint64 hash = wyhash(bytes, byte_len, 0, _wyp);
    return fn_hash_map_delete_hash(map, hash);
}
static inline void* fn_hash_map_delete_str(hash_map *map, const char* key) {
    uint64 hash = wyhash(key, strlen(key), 0, _wyp);
    return fn_hash_map_delete_hash(map, hash);
}

                        /* Frontend Macros */

#define new_hashmap(cap, type, alloc) fn_new_hash_map(cap, sizeof(type), true, alloc)

#define new_dynamic_hashmap(cap, type, alloc) fn_new_hash_map(cap, sizeof(type), true, alloc)
#define new_static_hashmap(cap, type, alloc) fn_new_hash_map(cap, sizeof(type), false, alloc)
#define free_hashmap(p_map) fn_free_hash_map(p_map)

#define hashmap_insert(p_map, p_key, p_value) fn_hash_map_insert(p_map, sizeof(*(p_key)), p_key, p_value, sizeof(*p_value))
#define hashmap_insert_str(p_map, str_key, p_value) fn_hash_map_insert_str(p_map, str_key, p_value, sizeof(*p_value))

#define hashmap_find(p_map, p_key) fn_hash_map_find(p_map, sizeof(*(p_key)), p_key)
#define hashmap_find_str(p_map, str_key) fn_hash_map_find_str(p_map, str_key)

#define find_hash(map, hash) fn_hash_map_find_hash(map, hash)

#define hashmap_delete(p_map, p_key) fn_hash_map_delete(p_map, sizeof(*(key)), p_key)
#define hashmap_delete_str(p_map, str_key) fn_hash_map_delete_str(p_map, str_key)

////////////////////////////////////////////////////////////////////////////////
// thread.h

#ifdef SOL_THREAD

#define THREAD_PRINT_STATUS 0

#if THREAD_PRINT_STATUS
    #define thread_status(m...) println(m)
#else
    #define thread_status(m...)
#endif

#ifndef _WIN32
    #define mutex pthread_mutex_t
    #define thread_handle pthread_t
#else
    // idk if this is right yet, really just a placeholder for now.
    #define mutex HANDLE
    #define thread_handle HANDLE
#endif

#define THREAD_WORK_QUEUE_COUNT 3
#define THREAD_WORK_QUEUE_SIZE 1024

#if THREAD_WORK_QUEUE_SIZE & (THREAD_WORK_QUEUE_SIZE - 1)
    #error "power of 2 please"
#endif

// @Todo windows equivalents
#ifndef _WIN32
    #define atomic_add(p, val) __sync_fetch_and_add   (p, val)
    #define atomic_sub(p, val) __sync_fetch_and_sub   (p, val)
    #define atomic_or(p, val) __sync_fetch_and_or     (p, val)
    #define atomic_and(p, val) __sync_fetch_and_and   (p, val)
    #define atomic_xor(p, val) __sync_fetch_and_xor   (p, val)
    #define atomic_nand(p, val) __sync_fetch_and_nand (p, val)
    #define atomic_cmpxchg_type(p, old_val, new_val) __sync_type_compare_and_swap(p, old_val, new_val)
    #define atomic_cmpxchg_bool(p, old_val, new_val) __sync_bool_compare_and_swap(p, old_val, new_val)

    #define atomic_load(from, to) __atomic_load(from, to, __ATOMIC_ACQUIRE)
    #define atomic_store(to, from) __atomic_store(to, from, __ATOMIC_RELEASE)

    #define init_mutex(m) pthread_mutex_init(m, NULL)
    #define shutdown_mutex(m) pthread_mutex_destroy(m)
    #define acquire_mutex(m) pthread_mutex_lock(m)
    #define try_to_acquire_mutex(m) pthread_mutex_trylock(m)
    #define release_mutex(m) pthread_mutex_unlock(m)

    #define create_thread(handle, info, fn, arg) pthread_create(handle, info, (void*(*)(void*))fn, (void*)arg)
    #define join_thread(handle, ret) pthread_join(handle, ret)
#endif


typedef struct thread_work {
    uint id;
    allocator *alloc_heap;
    allocator *alloc_temp;
    // Only fill in these slots to call add work
    void *(*fn)(void *);
    void *arg;
} thread_work;

typedef enum {
    THREAD_WORK_QUEUE_PRIORITY_HIGH   = 0,
    THREAD_WORK_QUEUE_PRIORITY_MEDIUM = 1,
    THREAD_WORK_QUEUE_PRIORITY_LOW    = 2,
} thread_work_queue_priority;

#define vol volatile // @Test without this.

typedef struct thread_work_queue {
    uint tail;
    uint head;
    mutex *lock;
    thread_work *work;
    allocator *alloc;
} thread_work_queue;

typedef struct thread_shutdown_info {
    uint64 work_items_completed;
    uint64 time_working; // milliseconds
    uint64 time_paused; // milliseconds
} thread_shutdown_info;

typedef struct thread {
    uint thread_write_flags;
    uint id;
    thread_handle handle;
    thread_work_queue *work_queues;
    allocator alloc_heap;
    allocator alloc_temp;
    uint pool_write_flags;
    thread_shutdown_info prog_info;
} thread;

typedef struct thread_pool {
    thread threads[THREAD_COUNT];
    thread_work_queue work_queues[THREAD_WORK_QUEUE_COUNT];
} thread_pool;

int new_thread_pool(struct allocation buffers_heap[THREAD_COUNT], struct allocation buffers_temp[THREAD_COUNT], allocator *alloc, thread_pool *pool);
void free_thread_pool(thread_pool *pool, bool graceful);

uint thread_add_work(thread_pool *pool, uint count, thread_work *work, thread_work_queue_priority priority);

#define thread_cast_work_fn(fn) ((void* (*)(void*))fn)
#define thread_cast_work(w)     ((void*)(w))
#define thread_add_work_high(pool, cnt, work)   thread_add_work(pool, cnt, work, THREAD_WORK_QUEUE_PRIORITY_HIGH)
#define thread_add_work_medium(pool, cnt, work) thread_add_work(pool, cnt, work, THREAD_WORK_QUEUE_PRIORITY_MEDIUM)

#endif // #ifdef SOL_THREAD

////////////////////////////////////////////////////////////////////////////////
// time.h

static inline struct timespec get_time_cpu_proc() {
    struct timespec ts;
    int ok = clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
    log_print_error_if(ok == (clock_t)-1, "failed to get time");
    return ts;
}

static inline struct timespec get_time_cpu_thread() {
    struct timespec ts;
    int ok = clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
    log_print_error_if(ok == (clock_t)-1, "failed to get time");
    return ts;
}

static inline struct timespec diffts(struct timespec *x, struct timespec *y)
{
    return (struct timespec){
        .tv_sec = labs(x->tv_sec - y->tv_sec),
        .tv_nsec = labs(x->tv_nsec - y->tv_nsec),
    };
}

static inline float floatts(struct timespec *ts)
{
    float f = ts->tv_sec;
    return f + (ts->tv_nsec * 10e-10);
}

static inline void print_ts_float(struct timespec *ts)
{
    println("%fs", floatts(ts));
}

static inline void print_ts(struct timespec *ts)
{
    println("%us and %uns", ts->tv_sec, ts->tv_nsec);
}

// @Test @Optimise Might be worth making const char* not string
// here to save a couple bytes just to make the timer call sites
// faster. I would not be suprised if this does nothing, since
// 256 bit mov would comfortably do this size. But still, might
// be nice to check it...
struct block_time {
    string fn;
    float t;
};

typedef struct block_timer {
    struct block_time *t;
} block_timer;

typedef struct program_timer {
    pthread_t h[THREAD_COUNT + 1]; // thread handles (+1 main thread)
    block_timer t[THREAD_COUNT + 1];
} program_timer;

static inline block_timer new_block_timer(uint fn_count, allocator *alloc)
{
    block_timer ret;
    ret.t = new_array(fn_count, *ret.t, alloc);
    return ret;
}

static inline void free_block_timer(block_timer *bt)
{
    free_array(bt->t);
}

static inline void block_timer_add(block_timer *bt, float t, string *fn)
{
    struct block_time r = {.fn = *fn, .t = t};
    array_add(bt->t, r);
}

static inline struct block_time block_timer_get(block_timer *bt, uint i)
{
    return bt->t[i];
}

static inline void reset_block_timer(block_timer *bt)
{
    reset_array(&bt->t);
}

static inline void print_block_times(block_timer *bt)
{
    for(uint i = 0; i < array_len(bt->t); ++i)
        println("%s: %f", bt->t[i].fn, i, bt->t[i].t);
}

static inline void new_prog_timer(thread_pool *tp, uint fn_count, allocator *alloc, program_timer *ret)
{
    ret->t[0] = new_block_timer(fn_count, alloc);
    ret->h[0] = pthread_self();
    for(uint i = 1; i < THREAD_COUNT + 1; ++i) {
        ret->t[i] = new_block_timer(fn_count, alloc);
        ret->h[i] = tp->threads[i-1].handle;
    }
}

static inline void free_prog_timer(program_timer *pt)
{
    free_block_timer(&pt->t[0]);
    for(uint i = 1; i < THREAD_COUNT + 1; ++i)
        free_block_timer(&pt->t[i]);
}

static inline void prog_timer_add(program_timer *pt, float t, string *fn)
{
    pthread_t id = pthread_self();
    uint bt = 0;
    for(uint i = 0; i < THREAD_COUNT + 1; ++i)
        bt += i & max_if(pthread_equal(id, pt->h[i]));
    block_timer_add(&pt->t[bt], t, fn);
}

void dump_prog_timer(program_timer *pt, const char *file, allocator *alloc);

// @NOTE: I have a feeling that these timers are rubbish but not sure...
#define NS_TIMER_START(timer) struct timespec timer = get_time_cpu_proc();
#define NS_TIMER_STOP(timer) size_t ns = get_time_cpu_proc().tv_nsec - timer.tv_nsec;

#define SIMPLE_TIMER_START(timer) \
    struct timespec timer = get_time_cpu_proc()
#define SIMPLE_TIMER_STOP(timer) \
    do { \
        struct timespec m__t_stop = get_time_cpu_proc(); \
        m__t_stop = diffts(&m__t_stop, &timer);          \
        print_ts_float(&m__t_stop);                      \
    } while(0)

#define BT_START() \
    struct timespec m__t_##__COUNTER__ = get_time_cpu_thread()
#define BT_STOP(bt)                                                    \
    do {                                                               \
        struct timespec m__t = get_time_cpu_thread();                  \
        m__t = diffts(&m__t, &m__t_##__COUNTER__);                     \
        float m__tf = floatts(&m__t);                                  \
        string m__s = {cstr_as_array_len(__FUNCTION__), __FUNCTION__}; \
        block_timer_add(bt, m__tf, &m__s);                             \
    } while(0)

#define PT_START() \
    struct timespec m__t_##__COUNTER__ = get_time_cpu_thread()
#define PT_STOP(pt)                                                                   \
    do {                                                                              \
        struct timespec m__t = get_time_cpu_thread();                                 \
        m__t = diffts(&m__t, &m__t_##__COUNTER__);                                    \
        float m__tf = floatts(&m__t);                                                 \
        string m__s = {.len = cstr_as_array_len(__FUNCTION__), .cstr = __FUNCTION__}; \
        prog_timer_add(pt, m__tf, &m__s);                                             \
    } while(0)

////////////////////////////////////////////////////////////////////////////////
// test.h

#define TEST_LIST_BROKEN true
#define TEST_LIST_SKIPPED true

// Define 'TEST_LIST_BROKEN' to list broken tests, and 'TEST_LIST_SKIPPED' to list skipped tests

typedef enum {
    TEST_RESULT_SUCCESS = 0,
    TEST_RESULT_FAIL = 1,
    TEST_RESULT_SKIPPED = 2,
    TEST_RESULT_BROKEN = 3,
} test_result;

typedef struct {
    string info;
    test_result result;
    uint32 start;
    uint32 end;
} test_module;

typedef struct {
    string info;
    test_result result;
    uint32 index;
} test;

typedef struct {
    uint32 fail;
    uint32 skipped;
    uint32 broken;
    string_buffer str_buf;
    test_module *modules;
    test *tests;
    allocator *alloc;
} test_suite;

test_suite load_tests(allocator *alloc);
void end_tests(test_suite *suite);

void begin_test_module(
    test_suite *suite,
    const char *name,
    const char *function_name,
    const char *file_name,
    bool broken,
    bool skipped);

void test_eq(
    test_suite *suite,
    const char *test_name,
    const char *function_name,
    const char *arg1_name,
    const char *arg2_name,
    int64       arg1,
    int64       arg2,
    bool        broken,
    int         line_number,
    const char *file_name);

void test_lt(
 test_suite *suite,
 const char *test_name,
 const char *function_name,
 const char *arg1_name,
 const char *arg2_name,
 int64       arg1,
 int64       arg2,
 bool        broken,
 int         line_number,
 const char *file_name);

void test_floateq(
 test_suite *suite,
 const char *test_name,
 const char *function_name,
 const char *arg1_name,
 const char *arg2_name,
 float       arg1,
 float       arg2,
 bool        broken,
 int         line_number,
 const char *file_name);

void test_streq(
 test_suite *suite,
 const char *test_name,
 const char *function_name,
 const char *arg1_name,
 const char *arg2_name,
 const       char* arg1,
 const       char* arg2,
 bool        broken,
 int         line_number,
 const char *file_name);

void test_ptreq(
 test_suite *suite,
 const char *test_name,
 const char *function_name,
 const char *arg1_name,
 const char *arg2_name,
 const       void *arg1,
 const       void* arg2,
 bool        broken,
 int         line_number,
 const char *file_name);

#define BEGIN_TEST_MODULE(name, broken, skipped) \
    begin_test_module(suite, name, __FUNCTION__, __FILE__, broken, skipped)

#define END_TEST_MODULE() array_last(suite->modules).end = (array_len(suite->tests) - 1)

#define TEST_EQ(test_name, arg1, arg2, broken) \
    test_eq(suite, test_name, __FUNCTION__, #arg1, #arg2, arg1, arg2, broken, __LINE__, __FILE__)
#define TEST_LT(test_name, arg1, arg2, broken) \
    test_lt(suite, test_name, __FUNCTION__, #arg1, #arg2, arg1, arg2, broken, __LINE__, __FILE__)
#define TEST_STREQ(test_name, arg1, arg2, broken) \
    test_streq(suite, test_name, __FUNCTION__, #arg1, #arg2, arg1, arg2, broken, __LINE__, __FILE__)
#define TEST_FEQ(test_name, arg1, arg2, broken) \
    test_floateq(suite, test_name, __FUNCTION__, #arg1, #arg2, arg1, arg2, broken, __LINE__, __FILE__)
#define TEST_PTREQ(test_name, arg1, arg2, broken) \
    test_ptreq(suite, test_name, __FUNCTION__, #arg1, #arg2, arg1, arg2, broken, __LINE__, __FILE__)

#ifdef SOL_DEF

////////////////////////////////////////////////////////////////////////////////
// print.c

typedef enum {
    PRINT_HEX_BIT    = 0x00000001,
    PRINT_BIN_BIT    = 0x00000002,
    PRINT_LZ_BIT     = 0x00000004,
    PRINT_UINT_BIT   = 0x00000008,
    PRINT_SINT_BIT   = 0x00000010,
    PRINT_FLOAT_BIT  = 0x00000020,
    PRINT_STRING_BIT = 0x00000040,
    PRINT_CHAR_BIT   = 0x00000080,
} Print_Flag_Bits;

typedef uint32 Print_Flags;

typedef enum {
    PRINT_INT_SIZE_NIL = 0,
    PRINT_INT_SIZE_8   = 8,
    PRINT_INT_SIZE_16  = 16,
    PRINT_INT_SIZE_32  = 32,
    PRINT_INT_SIZE_64  = 64,
} Print_Int_Size;

typedef struct {
    Print_Flags flags;
    Print_Int_Size int_size;
} Print_Config;

inline static void print_parse_int_dec(uint64 i, int *len, char *buf) {
    if (i == 0) {
        buf[*len] = '0';
        *len += 1;
        return ;
    }

    while(i > 0) {
        buf[*len] = (i % 10) + '0';
        *len += 1;
        i /= 10;
    }
}

inline static void print_parse_int_hex(uint64 i, int *len, char *buf) {
    if (i == 0) {
        buf[*len] = '0';
        *len += 1;
        return ;
    }

    char hex[] = {'a','b','c','d','e','f'};
    uint64 mask = 0x000000000000000f;
    int tmp;
    while(i > 0) {
        tmp = i & mask;

        if (tmp < 10)
            buf[*len] = tmp + '0';
        else
            buf[*len] = hex[tmp % 10];

        *len += 1;
        i >>= 4;
    }
}

inline static void print_parse_int_bin(uint64 i, int *len, char *buf) {
    if (i == 0) {
        buf[*len] = '0';
        *len += 1;
        return ;
    }

    uint64 mask = 0x0000000000000001;
    while(i > 0) {
        buf[*len] = (i & mask) + '0';
        *len += 1;
        i >>= 1;
    }
}

static void print_parse_int(Print_Config *config, uint64 i, int *buf_pos, char *print_buffer) {
    int max_zeros;

    switch(config->int_size) { // hex zeros
    case PRINT_INT_SIZE_8:
        max_zeros = 2;
        break;
    case PRINT_INT_SIZE_16:
        max_zeros = 4;
        break;
    case PRINT_INT_SIZE_32:
        max_zeros = 8;
        break;
    case PRINT_INT_SIZE_64:
        max_zeros = 16;
        break;
    default:
        max_zeros = 16;
        break;
    }

    char int_buf[96];
    int int_pos = 0;
    if (config->flags & PRINT_HEX_BIT) {
        print_parse_int_hex(i, &int_pos, int_buf);

        print_buffer[*buf_pos + 0] = '0';
        print_buffer[*buf_pos + 1] = 'x';

        *buf_pos += 2;
    } else if (config->flags & PRINT_BIN_BIT) {
        print_parse_int_bin(i, &int_pos, int_buf);

        print_buffer[*buf_pos + 0] = '0';
        print_buffer[*buf_pos + 1] = 'b';

        max_zeros *= 4;
        *buf_pos += 2;
    } else {
        print_parse_int_dec(i, &int_pos, int_buf);
        max_zeros = 0;
    }

    if (config->flags & PRINT_LZ_BIT) {
        int zeros;
        if (i > 0) {
            zeros = clz64(i);
            zeros &= max_zeros - 1;
        } else {
            zeros = max_zeros;
        }
        for(int j = 0; j < zeros; ++j) {
            int_buf[int_pos] = '0';
            int_pos++;
        }
    }

    // reverse int
    for(int j = 0; j < int_pos; ++j) {
        print_buffer[*buf_pos] = int_buf[(int_pos - 1) - j];
        *buf_pos += 1;
    }
}
inline static void print_parse_signed_int(Print_Config *config, int64 i, int *buf_pos, char *print_buffer) {
    if (i < 0) {
        print_buffer[*buf_pos] = '-';
        *buf_pos += 1;
        i = -i;
    }
    print_parse_int(config, (uint64)i, buf_pos, print_buffer);
}
inline static void print_parse_unsigned_int(Print_Config *config, uint64 i, int *buf_pos, char *print_buffer) {
    print_parse_int(config, i, buf_pos, print_buffer);
}

typedef enum {
    PRINT_VALUE_STRING,
    PRINT_VALUE_CHAR,
    PRINT_VALUE_UINT,
    PRINT_VALUE_SINT,
    PRINT_VALUE_FLOAT,
    PRINT_VALUE_HEX,
    PRINT_VALUE_BIN,
    PRINT_VALUE_LZ,
} Print_Value;

inline static bool print_check_config_flags(Print_Flags flags, Print_Value value) {
    switch(value) {
    case PRINT_VALUE_STRING:
    case PRINT_VALUE_CHAR:
    case PRINT_VALUE_FLOAT:
        return flags;
    case PRINT_VALUE_SINT:
        return flags & ~(PRINT_HEX_BIT | PRINT_BIN_BIT | PRINT_LZ_BIT | PRINT_UINT_BIT);
    case PRINT_VALUE_UINT:
        return flags & ~(PRINT_HEX_BIT | PRINT_BIN_BIT | PRINT_LZ_BIT | PRINT_SINT_BIT);
    case PRINT_VALUE_HEX:
        return flags & (PRINT_STRING_BIT | PRINT_FLOAT_BIT | PRINT_CHAR_BIT | PRINT_HEX_BIT);
    case PRINT_VALUE_BIN:
        return flags & (PRINT_STRING_BIT | PRINT_FLOAT_BIT | PRINT_CHAR_BIT | PRINT_BIN_BIT);
    case PRINT_VALUE_LZ:
        return flags & (PRINT_STRING_BIT | PRINT_FLOAT_BIT | PRINT_CHAR_BIT | PRINT_LZ_BIT);
    default:
        assert(false && "Invalid Flag Check");
        return false;
    }
}

void string_format_backend(char *format_buffer, const char *fmt, va_list args) { // args must have been started
    int buf_pos = 0;
    char c;
    char *s;
    uint64 u;
    int64 i;
    double f;

    Print_Config config = {};
    bool is_ident    = false;

    int tmp;
    // char last_char = 0; -Wunused
    for(int j = 0; fmt[j] != 0; ++j) {

        if (fmt[j] != '%' && fmt[j] != '-') {
            format_buffer[buf_pos] = fmt[j];
            buf_pos++;
            continue;
        } else if (fmt[j] == '-' || fmt[j+1] == 0) {
            if (fmt[j+1] == '%') {
                format_buffer[buf_pos + 0] = fmt[j + 0];
                format_buffer[buf_pos + 1] = fmt[j + 1];
                buf_pos += 2;
                j++;
            } else {
                format_buffer[buf_pos] = fmt[j];
                buf_pos++;
            }
            continue;
        } else {
            is_ident = true;
            config   = (Print_Config){};
            j++;
            while(is_ident && fmt[j] != 0) {
                switch(fmt[j]) {
                case 0:
                    goto not_ident;
                case '\\':
                    goto not_ident;
                case 'f':
                    if (print_check_config_flags(config.flags, PRINT_VALUE_FLOAT)) {
                        goto not_ident;
                    }
                    f = va_arg(args, double);
                    // normally floats are handled by the stb implementation, but that is not really
                    // necessary for this header. Plus the code bloat would be too annoying. I could
                    // just use full default and add a println, but I like be able to add and play
                    // with stuff, such as the different print formats, etc.
                    buf_pos += sprintf(format_buffer + buf_pos, "%f", f);
                    j++;
                    goto not_ident;
                case 'c':
                    if (print_check_config_flags(config.flags, PRINT_VALUE_CHAR)) {
                        goto not_ident;
                    }
                    c = (char)va_arg(args, int);
                    format_buffer[buf_pos] = c;
                    buf_pos++;

                    j++;
                    goto not_ident;
                case 's':
                    if (print_check_config_flags(config.flags, PRINT_VALUE_STRING)) {
                        goto not_ident;
                    }
                    s = va_arg(args, char*);
                    tmp = strlen(s);
                    memcpy(format_buffer + buf_pos, s, tmp);
                    buf_pos += tmp;

                    j++;
                    goto not_ident;
                case 'h':
                    if (print_check_config_flags(config.flags, PRINT_VALUE_HEX)) {
                        j++;
                        goto not_ident;
                    } else {
                        config.flags |= PRINT_HEX_BIT;
                    }
                    break;
                case 'b':
                    if (print_check_config_flags(config.flags, PRINT_VALUE_BIN)) {
                        j++;
                        goto not_ident;
                    } else {
                        config.flags |= PRINT_BIN_BIT;
                    }
                    break;
                case 'z':
                    if (print_check_config_flags(config.flags, PRINT_VALUE_LZ)) {
                        j++;
                        goto not_ident;
                    } else {
                        config.flags |= PRINT_LZ_BIT;
                    }
                    break;
                case 'i':
                    if (print_check_config_flags(config.flags, PRINT_VALUE_SINT)) {
                        j++;
                        goto not_ident;
                    } else {
                        i = va_arg(args, int64);
                        config.flags |= PRINT_SINT_BIT;
                    }
                    break;
                case 'u':
                    if (print_check_config_flags(config.flags, PRINT_VALUE_UINT)) {
                        j++;
                        goto not_ident;
                    } else {
                        u = va_arg(args, uint64);
                        config.flags |= PRINT_UINT_BIT;
                    }
                    break;
                default:
                    goto not_ident;
                }
                j++;
            }

            not_ident:
            j--; // ensure next loop iteration sees non ident value

            if (config.flags & PRINT_UINT_BIT) {
                print_parse_unsigned_int(&config, u, &buf_pos, format_buffer);
            } else if (config.flags & PRINT_SINT_BIT) {
                print_parse_signed_int(&config, i, &buf_pos, format_buffer);
            }
        }
    }

    va_end(args);
    format_buffer[buf_pos] = 0;
}

////////////////////////////////////////////////////////////////////////////////
// file.c

struct file file_read_bin_all(const char *file_name, allocator *alloc)
{
    FILE *f = fopen(file_name, "rb");
    if (!f) {
        println("FILE: Failed to open file '%s'", file_name);
        return (struct file){};
    }
    fseek(f, 0, SEEK_END);
    struct file ret;
    ret.size = ftell(f);
    ret.data = allocate(alloc, ret.size);
    fseek(f, 0, SEEK_SET);
    size_t s = fread(ret.data, 1, ret.size, f);
    log_print_error_if(s != ret.size, "failed to read entire file %s: file size %u, read %u", file_name, ret.size, s);
    fclose(f);
    return ret;
}

struct file file_read_char_all(const char *file_name, allocator *alloc)
{
    FILE *f = fopen(file_name, "r");
    if (!f) {
        println("FILE: Failed to open file '%s'", file_name);
        return (struct file){};
    }
    fseek(f, 0, SEEK_END);
    struct file ret;
    ret.size = ftell(f);
    ret.data = allocate(alloc, ret.size);
    fseek(f, 0, SEEK_SET);
    size_t s = fread(ret.data, 1, ret.size, f);
    log_print_error_if(s != ret.size, "failed to read entire file %s: file size %u, read %u", file_name, ret.size, s);
    fclose(f);
    return ret;
}

void file_read_bin_size(const char *file_name, size_t size, void *buffer)
{
    FILE *f = fopen(file_name, "rb");
    if (!f) {
        println("FILE: Failed to open file '%s'", file_name);
        return;
    }
    size_t s = fread(buffer, 1, size, f);
    log_print_error_if(s != size, "failed to read %u bytes from file %s, read %u", size, file_name, s);
    fclose(f);
}

void file_read_char_count(const char *file_name, size_t count, char *buffer)
{
    FILE *f = fopen(file_name, "r");
    if (!f) {
        println("FILE: Failed to open file '%s'", file_name);
        return;
    }
    size_t s = fread(buffer, 1, count, f);
    log_print_error_if(s != count, "failed to read size from file %s", file_name);
    fclose(f);
}

void file_write_bin(const char *file_name, size_t size, const void *data)
{
    FILE *f = fopen(file_name, "wb");
    if (!f) {
        println("FILE: Failed to open file '%s'", file_name);
        return;
    }
    fwrite(data, 1, size, f);
    fclose(f);
}

void file_write_char(const char *file_name, size_t count, const char *data)
{
    FILE *f = fopen(file_name, "w");
    if (!f) {
        println("FILE: Failed to open file '%s'", file_name);
        return;
    }
    fwrite(data, 1, count, f);
    fclose(f);
}

void file_append_bin(const char *file_name, size_t size, const void *data)
{
    FILE *f = fopen(file_name, "ab");
    if (!f) {
        println("FILE: Failed to open file '%s'", file_name);
        return;
    }
    fwrite(data, 1, size, f);
    fclose(f);
}

void file_append_char(const char *file_name, size_t count, const char *data)
{
    FILE *f = fopen(file_name, "a");
    if (!f) {
        println("FILE: Failed to open file '%s'", file_name);
        return;
    }
    fwrite(data, 1, count, f);
    fclose(f);
}

struct dir getdir(const char *name, allocator *alloc)
{
    if (!name) {
        log_print_error("passing null argument 'name' to getdir()");
        return (struct dir){};
    }

    struct dir ret = {
        .f = new_strarr(128, 16, alloc),
        .d = new_strarr(64, 8, alloc),
    };

    DIR *d = opendir(name);

    struct dirent *de;
    while((de = readdir(d)))
        if (memcmp(de->d_name, ".", 1) && memcmp(de->d_name, "..", 2)) {
            if (de->d_type == DT_REG)
                strarr_add_cstr(&ret.f, de->d_name);
            else if (de->d_type == DT_DIR)
                strarr_add_cstr(&ret.d, de->d_name);
        }
    return ret;
}

////////////////////////////////////////////////////////////////////////////////
// allocator.c

// @Todo This is one of my oldest files and so has numerous inconsistencies and
// is rife with ugliness.

#ifdef SOL_ALLOC

static linear_allocator fn_new_linear_allocator(uint64 cap, void *buffer);
static void free_linear_allocator(linear_allocator *alloc);
static void* malloc_linear(allocator *alloc, uint64 size);
static void* realloc_linear(allocator *alloc, void *ptr, uint64 old_size, uint64 new_size);

// Currently does nothing, but now I am thinking that a flag to activate a
// reference count might sometimes be useful, and then the allocator is reset
// on reference count (allocation count zero.
static void free_allocation_linear(allocator *alloc, void *ptr);

static void init_arena(allocator *alloc, size_t min_block_size);
static void* arena_alloc(allocator *alloc, size_t size);

/* return values for unmapping functions:
     - NULL on success
     - the footer identifying the block that was being freed to allow for recovery from
       an error unmapping said block.
     - 0xcafebabecafebabe if deallocate is passed an address which is not in the address
       space of the allocator. */
static struct arena_footer* free_arena(arena_allocator *alloc);
static struct arena_footer* arena_dealloc(allocator *alloc, void* ptr);

/* @Todo Interface for realloc is not great. I wanted to maintain the typical
   realloc interface where it returns a pointer, so if the allocation fails,
   then it returns NULL, but if the free fails, then it returns the footer.
   Annoying because neither '== 0' nor '!= 0' inidicates error. I could return
   a (void*)-1 one on allocation fail maybe. Idk. I rarely use realloc directly
   and when I do I never check the return codes, just let it crash. */
static void* arena_realloc(allocator *alloc, void *ptr, uint old_size, uint new_size);

void init_allocator(allocator *alloc, size_t cap, void *buffer, allocator_flag_bits type)
{
    alloc->flags = type;

    cap = align(cap, ALLOCATOR_ALIGNMENT);

    if (!buffer)
        buffer = malloc(cap);
    else
        alloc->flags |= ALLOCATOR_DO_NOT_FREE_BIT;

    switch(type) {
        case ALLOCATOR_LINEAR_BIT:
            alloc->linear = fn_new_linear_allocator(cap, buffer);
            alloc->allocate = malloc_linear;
            alloc->reallocate = realloc_linear;
            alloc->deallocate = free_allocation_linear;
            break;
        case ALLOCATOR_ARENA_BIT:
            init_arena(alloc, cap);
            alloc->allocate = arena_alloc;
            alloc->deallocate = (void*)arena_dealloc;
            alloc->reallocate = (void*)arena_realloc;
            break;
        default:
            assert(false && "Invalid allocator type");
            break;
    }
}

void free_allocator(allocator *alloc)
{
    if (alloc->flags & ALLOCATOR_DO_NOT_FREE_BIT)
        return;
    switch(alloc->flags & ALLOCATOR_TYPE_BITS) {
    case ALLOCATOR_LINEAR_BIT:
        free_linear_allocator(&alloc->linear);
        break;
    case ALLOCATOR_ARENA_BIT:
        free_arena(&alloc->arena);
        break;
    default:
        break;
    }
}
static linear_allocator fn_new_linear_allocator(uint64 cap, void *buffer)
{
    linear_allocator ret;
    ret.cap = align(cap, ALLOCATOR_ALIGNMENT);
    ret.mem = buffer;
    ret.used = 0;
    return ret;
}

static void free_linear_allocator(linear_allocator *alloc)
{
    free(alloc->mem);
    alloc->cap = 0;
}

static void* malloc_linear(allocator *alloc, uint64 size)
{
    assert(alloc->flags & ALLOCATOR_LINEAR_BIT);
    size = align(size, ALLOCATOR_ALIGNMENT);
    alloc->linear.used = align(alloc->linear.used, ALLOCATOR_ALIGNMENT); // really this is unnecessary, as only aligned sizes are ever allocated.
    void *ret = (void*)(alloc->linear.mem + alloc->linear.used);
    alloc->linear.used += size;
    assert(alloc->linear.used <= alloc->linear.cap && "Linear Allocator Overflow");
    return ret;
}

static void* realloc_linear(allocator *alloc, void *ptr, uint64 old_size, uint64 new_size)
{
    void *p_old = ptr;
    ptr = malloc_linear(alloc, new_size);
    memcpy(ptr, p_old, old_size);
    return ptr;
}

static void free_allocation_linear(allocator *alloc, void *ptr) {}

// Arena
static struct arena_footer* arena_add_block(uint size, uint min_size)
{
    size += sizeof(struct arena_footer);
    size = size < min_size ? min_size : align(size, getpagesize());
    void *data = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
    if (!data) {
        log_print_error("Failed to allocate arena block - %s", strerror(errno));
        return NULL;
    }
    size -= sizeof(struct arena_footer);
    struct arena_footer *ret = (struct arena_footer*)((uchar*)data + size);
    ret->check_bytes = ARENA_FOOTER_CHECK_BYTES;
    ret->allocation_count = 0;
    ret->used = 0;
    ret->base = data;
    ret->next = NULL;
    return ret;
}

static void init_arena(allocator *alloc, size_t min_block_size)
{
    arena_allocator *arena = &alloc->arena;
    arena->min_block_size = align(min_block_size + sizeof(struct arena_footer), getpagesize());
    arena->head = NULL;
    arena->tail = NULL;
}

static struct arena_footer* free_arena(arena_allocator *arena)
{
    uint block_i = 0;
    do {
        log_print_error_if(arena->tail->check_bytes != ARENA_FOOTER_CHECK_BYTES,
                "block %u has invalid header, check bytes == %uh", block_i, arena->tail->check_bytes);
        struct arena_footer *tmp = arena->tail;
        arena->tail = arena->tail->next;
        if (munmap(tmp->base, align(tmp->cap + sizeof(struct arena_footer), getpagesize()))) {
            log_print_error("failed to unmap block %u - arena address %uh, %s",
                    block_i, arena, strerror(errno));
            return tmp;
        }
        block_i++;
    } while(arena->tail);
    *arena = (arena_allocator){};
    return NULL;
}

static void* arena_alloc(allocator *alloc, size_t size)
{
    arena_allocator *arena = &alloc->arena;
    log_print_error_if(arena->head && arena->head->check_bytes != ARENA_FOOTER_CHECK_BYTES,
                "head has invalid header, check bytes == %uh", arena->head->check_bytes);
    if (size == 0)
        return NULL;
    size = allocalign(size);
    if (!arena->head) {
        arena->head = arena_add_block(size, arena->min_block_size);
        arena->tail = arena->head;
    } else if(arena->head->used + size > arena->head->cap) {
        struct arena_footer *new_block = arena_add_block(size, arena->min_block_size);
        arena->head->next = new_block;
        arena->head = new_block;
    }
    void *ret = (uchar*)arena->head->base + arena->head->used;
    arena->head->used += size;
    arena->head->allocation_count++;
    return ret;
}

static struct arena_footer* arena_dealloc(allocator *alloc, void* ptr)
{
    arena_allocator *arena = &alloc->arena;
    // Makes the allocator very fast if only using one block. Basically just
    // a reference counted linear allocator at that point.
    if (arena->tail == arena->head) {
        log_print_error_if(!(arena->head->base <= ptr &&
                            (uchar*)arena->head->base + arena->head->used > (uchar*)ptr),
                            "arena_deallocate was passed an invalid pointer - arena address %uh, pointer %uh", arena, ptr);
        arena->head->allocation_count -= 1;
        if (!arena->head->allocation_count)
            arena->head->used = 0;
        return NULL;
    }
    struct arena_footer *pos = arena->tail;
    struct arena_footer *prev = arena->tail;
    uint block_i = 0;
    while(1) {
        log_print_error_if(pos->check_bytes != ARENA_FOOTER_CHECK_BYTES,
                "block %u has invalid header, check bytes == %uh", block_i, pos->check_bytes);
        if (pos->base <= ptr && (uchar*)pos->base + pos->used > (uchar*)ptr) {
            break;
        } else if (!pos->next) {
            log_print_error("arena_deallocate was passed an invalid pointer - arena address %uh, pointer %uh", arena, ptr);
            return (struct arena_footer*)0xcafebabecafebabe;
        }
        prev = pos;
        pos = pos->next;
        block_i++;
    }
    pos->allocation_count -= 1;
    if (pos->allocation_count)
        return NULL;

    if (pos == arena->head) {
        arena->head = prev;
        arena->head->next = NULL;
    } else if (pos == arena->tail) {
        arena->tail = pos->next;
    } else {
        prev->next = pos->next;
    }
    size_t size = pos->cap;
    if (munmap(pos->base, align(pos->cap + sizeof(struct arena_footer), getpagesize()))) {
        log_print_error("failed to unmap block %u - arena address %uh, %s",
                block_i, arena, strerror(errno));
        return pos;
    }
    return NULL;
}

static void* arena_realloc(allocator *alloc, void *ptr, uint old_size, uint new_size)
{
    arena_allocator *arena = &alloc->arena;
    void *ret = arena_alloc(alloc, new_size);
    if (!ret) return NULL;
    memcpy(ret, ptr, old_size);
    struct arena_footer *freed = arena_dealloc(alloc, ptr);
    log_print_error_if(freed, "failed to free pointer %uh - arena address %uh", ptr, alloc);
    return freed ? freed : ret;
}

#endif

//////////////////////////////////////////////////////////////
// array.c

void* load_array(const char *fname, allocator *alloc)
{
    struct file f = file_read_bin_all(fname,alloc);
    uint64 *ret = (uint64*)f.data;
    ret[3] = (uint64)alloc;
    ret += ARRAY_METADATA_WIDTH;
    assert(((uint64)ret % 16) == 0); // align for SIMD
    return ret;
}

void store_array(const char *fname, void *a)
{
    FILE *f = fopen(fname,"wb");
    struct allocation alloc = array_allocation(a);
    fwrite(alloc.data,1,alloc.size,f);
    fclose(f);
}

//////////////////////////////////////////////////////////////
// ringbuffer.c

bool init_ringbuffer(ringbuffer *buf, uint size, uint flags)
{
    size = align(size, getpagesize());
    assert(size < Max_s32);

    void *m = mmap(NULL, size * 2, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
    if (m == MAP_FAILED) {
        log_print_error("failed to map data, size %u", size * 2);
        return false;
    }

    int fd = memfd_create("ringbuffer", 0x0);
    if(fd == -1) {
        log_print_error("failed to memfd_create");
        goto fail_fd_create;
    }

    if (ftruncate(fd, size) == -1) {
        log_print_error("failed to truncate file - size %u", size);
        goto fail_truncate;
    }

    void *p1 = mmap((uchar*)m + 0,    size, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, fd, 0);
    void *p2 = mmap((uchar*)m + size, size, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, fd, 0);

    if (p1 == MAP_FAILED || p2 == MAP_FAILED) {
        log_print_error("failed to map pages to file");
        goto fail_truncate;
    }

    buf->size = size;
    buf->data = m;
    buf->flags = flags;
    buf->head = 0;
    buf->tail = 0;

    return true;

fail_truncate:
    close(fd);
fail_fd_create:
    munmap(m, size*2);
    return false;
}

static inline bool rb_head_crosses_tail(ringbuffer *rb, uint size)
{
    assert(rb->head < Max_s32 - size);
    // @Todo There *must* be a nice algorithm to find this without branches.
    int h = rb->head;
    int t = rb->tail;
    if (h > t && h + size - rb->size > t)
        return true;
    if (h < t && h + size > t)
        return true;
    return false;
}

static inline bool rb_tail_crosses_head(ringbuffer *rb, uint size)
{
    assert(rb->tail < Max_s32 - size);
    // @Todo There *must* be a nice algorithm to find this without branches.
    int t = rb->tail;
    int h = rb->head;
    if (t > h && t + size - rb->size > h)
        return true;
    if (t < h && t + size > h)
        return true;
    return false;
}

void* rballoc(ringbuffer *rb, uint size)
{
    size = allocalign(size);
    assert(size <= rb->size);

    if (!(rb->flags & RB_NOTAIL) && rb_head_crosses_tail(rb, size)) {
        log_print_error("ringbuffer overflow, head would cross tail: allocation size = %u, rb = {.size = %u, .head = %u, .tail = %u}",
                        size, rb->size, rb->head, rb->tail);
        return NULL;
    }

    void *ret = rb->data + rb->head;
    rb->head += size;
    rb->head -= rb->size & maxif(rb->head >= rb->size);

    return ret;
}

bool rbfree(ringbuffer *rb, uint size)
{
    if (!(rb->flags & RB_NOTAIL) && rb_tail_crosses_head(rb, size)) {
        log_print_error("ringbuffer underflow, tail would cross head: allocation size = %u, rb = {.size = %u, .head = %u, .tail = %u}",
                        size, rb->size, rb->head, rb->tail);
        return 0;
    }

    rb->tail += size;
    rb->tail -= rb->size & maxif(rb->tail >= rb->size);

    return true;
}

//////////////////////////////////////////////////////////////
// string.c

static void realloc_strbuf(string_buffer *buf, uint64 newsz);

string_buffer new_strbuf(uint64 cap, uint flags, allocator *alloc)
{
    string_buffer ret;
    ret.flags = flags;
    ret.used = 0;
    ret.cap = align(cap, 16);
    ret.data = allocate(alloc, ret.cap);
    ret.alloc = alloc;
    return ret;
}

void free_strbuf(string_buffer *buf)
{
    deallocate(buf->alloc, buf->data);
    buf->cap = 0;
    buf->used = 0;
}

// @Note The flags field and reallocation is different to my string_buffer
// implementation elsewhere. It is useful for a thing I am currently working
// on, and if it seems like something I would really use in more substantial
// projects, I will clone it.
string strbuf_add(string_buffer *buf, string *str)
{
    if (buf->used + str->len + 1 > buf->cap) {
        if(buf->flags & STRING_BUFFER_REALLOCATE_BIT) {
            realloc_strbuf(buf, (buf->cap + str->len + 1) * 2);
        } else {
            log_print_error("string_buffer overflow");
            return (string){};
        }
    }

    memcpy(buf->data + buf->used, str->cstr, str->len);
    string ret;
    ret.len = str->len;
    ret.cstr = buf->data + buf->used;

    buf->used += str->len + 1;
    buf->data[buf->used-1] = '\0';

    return ret;
}

string strbuf_add_cstr(string_buffer *buf, const char *cstr)
{
    uint64 len = strlen(cstr);
    if (buf->used + len + 1 > buf->cap) {
        if(buf->flags & STRING_BUFFER_REALLOCATE_BIT) {
            realloc_strbuf(buf, (buf->cap + len + 1) * 2);
        } else {
            log_print_error("string_buffer overflow");
            return (string){};
        }
    }

    memcpy(buf->data + buf->used, cstr, len);
    string ret;
    ret.len = len;
    ret.cstr = buf->data + buf->used;

    buf->used += len + 1;
    buf->data[buf->used-1] = '\0';

    return ret;
}

static void realloc_strbuf(string_buffer *buf, uint64 newsz)
{
    newsz = align(newsz, 16);
    buf->cap  = newsz;
    buf->data = reallocate(buf->alloc, buf->data, buf->used, newsz);
}

string_array str_split(string *str, char split_char, allocator *alloc)
{
    string_array ret = new_strarr(str->len + (str->len>>3), str->len>>3, alloc);
    string tmp;
    uint pos = 0;
    uint inc;
    while(1) {
        tmp.cstr = str->cstr + pos;
        tmp.len = pos;

        inc = simd_find_char_with_len(str->len - pos, str->cstr + pos, split_char);

        if (inc == Max_u32)
            break;

        pos += inc;
        tmp.len = pos - tmp.len;
        strarr_add(&ret, &tmp);

        pos++;
    }
    return ret;
}

////////////////////////////////////////////////////////////////////////////////
// hashmap.c

bool fn_hash_map_insert_hash(hash_map *map, uint64 hash, void *elem, int elem_width)
{
    int g_idx = (hash & (map->cap - 1));
    g_idx    -= g_idx & 15;

    uint8  *data   = map->data;
    int  cap    = map->cap;
    int  stride = map->kv_stride;

    int  tz;
    uint16  mask;
    uint64 *phash;

    __m128i a;
    int inc = 0;
    while(inc < cap) {
        a    = _mm_load_si128((__m128i*)(data + g_idx));
        mask = _mm_movemask_epi8(a);

        if (!mask) {
            inc   += 16;
            g_idx += inc;
            g_idx &= cap - 1;
            continue;
        } else {
            tz = ctz16(mask);

            uint8 top7 = (hash >> 57) & HASH_MAP_FULL;
            data[g_idx + tz] = 0x0 | top7;

            phash  = (uint64*)(data + cap + (stride * (tz + g_idx)));
           *phash  =  hash;
            memcpy(data + cap + (stride * (tz + g_idx)) + 8, elem, elem_width);

            map->remaining -= 1;
            return true;
        }
    }
    return false;
}

void fn_hash_map_if_full(hash_map *map, int elem_width)
{
    assert(map->cap * 2 > map->cap && "mul overflow");
    log_print_error_if(!map->resize, "hash map overflow but resize is false");

    uint8 *old_data = map->data;
    int old_cap  = map->cap;

    map->cap      *= 2;
    map->data      = allocate(map->alloc, map->cap + map->cap * map->kv_stride);
    map->remaining = ((map->cap + 1) / 8) * 7;

    memset(map->data, HASH_MAP_EMPTY, map->cap);

    int stride = map->kv_stride;

    int  pc;
    int  tz;
    uint16  mask;
    uint64 *phash;

    __m128i a;
    for(int i = 0; i < old_cap; i += 16) {
        a    = _mm_load_si128((__m128i*)(old_data + i));
        mask = ~(_mm_movemask_epi8(a));

        pc = pop_count16(mask);
        for(int j = 0; j < pc; ++j) {
            tz    = ctz16(mask);
            mask ^= 1 << tz;

            phash = (uint64*)(old_data + old_cap + (stride * (tz + i)));
            assert(fn_hash_map_insert_hash(map, *phash, (uint8*)phash + 8, elem_width) && "hash map grow failure");
        }
    }
    free(old_data);
}
void* fn_hash_map_find_hash(hash_map *map, uint64 hash)
{
    uint8 top7   = (hash >> 57) & HASH_MAP_FULL;
    int g_idx = hash & (map->cap - 1);
    g_idx -= g_idx & 15;

    uint8 *data   = map->data;
    int stride = map->kv_stride;
    int cap    = map->cap;

    int  pc;
    int  tz;
    uint16  mask;
    uint64 *phash;

    __m128i a;
    __m128i b = _mm_set1_epi8(top7);

    int inc = 0;
    while(inc < cap) {
        a    = _mm_load_si128((__m128i*)(data + g_idx));
        a    = _mm_cmpeq_epi8(a, b);

        mask = _mm_movemask_epi8(a);
        pc   = pop_count16(mask);

        for(int i = 0; i < pc; ++i) {
            tz    = ctz16(mask);
            mask ^= 1 << tz;
            phash = (uint64*)(data + cap + (stride * (tz + g_idx)));
            if (*phash == hash)
                return (uint8*)phash + 8;
        }
        g_idx += 16;
        g_idx &= cap - 1;
        inc   += 16;
    }
    return NULL;
}

void* fn_hash_map_delete_hash(hash_map *map, uint64 hash)
{
    uint8 top7   = (hash >> 57) & HASH_MAP_FULL;
    int g_idx = hash & (map->cap - 1);
    g_idx    -= g_idx & 15;

    __m128i a;
    __m128i b = _mm_set1_epi8(top7);

    uint8 *data   = map->data;
    int stride = map->kv_stride;
    int cap    = map->cap;

    int  pc;
    int  tz;
    uint16  mask;
    uint64 *phash;

    int inc = 0;
    while(inc < cap) {
        a    = _mm_load_si128((__m128i*)(data + g_idx));
        a    = _mm_cmpeq_epi8(a, b);

        mask = _mm_movemask_epi8(a);
        pc   = pop_count16(mask);

        for(int i = 0; i < pc; ++i) {
            tz    = ctz16(mask);
            mask ^= 1 << tz;
            phash = (uint64*)(data + cap + (stride * (tz + g_idx)));
            if (*phash == hash) {
                data[g_idx + tz] = HASH_MAP_DELETED;
                return (uint8*)phash + 8;
            }
        }
        g_idx += 16;
        g_idx &= cap - 1;
        inc   += 16;
    }
    return NULL;
}

////////////////////////////////////////////////////////////////////////////////
// thread.c

#ifdef SOL_THREAD

/*
    Note that the interface for this file is going under some maintenance. The
    actual thread work implementation stuff I am pretty confident in, but there
    is some poor stuff with return values when freeing pools, for instance.
*/

static mutex thread_work_queue_locks[THREAD_WORK_QUEUE_COUNT];

static thread_work_queue new_thread_work_queue(uint i, allocator *alloc);
static uint thread_work_queue_add(thread_work_queue *queue, uint work_count, thread_work *work);
static void free_thread_work_queue(thread_work_queue *work_queue);

static void* thread_start(thread *self);
static uint thread_try_to_acquire_work(thread_work_queue *queue, thread_work *work);
static void* thread_shutdown(thread *self);

enum {
    THREAD_RESULT_SUCCESS      = 0,
    THREAD_RESULT_QUEUE_FULL   = 1,
    THREAD_RESULT_QUEUE_EMPTY  = 2,
    THREAD_RESULT_QUEUE_IN_USE = 3,
    THREAD_RESULT_POOL_BUSY    = 4,
    THREAD_RESULT_MUTEX_ERROR  = 5,
};

enum {
    THREAD_BUSY_BIT                     = 0x01,
    THREAD_SLEEP_BIT                    = 0x02,
    THREAD_SHUTDOWN_BIT                 = 0x04,
    THREAD_GRACEFUL_BIT                 = 0x08,
    THREAD_QUEUE_EMPTY_BIT              = 0x10,
    THREAD_QUEUE_TRYING_TO_ADD_WORK_BIT = 0x20,
};

int new_thread_pool(struct allocation buffers_heap[THREAD_COUNT], struct allocation buffers_temp[THREAD_COUNT], allocator *alloc, thread_pool *pool)
{
    uint i;
    for(i=0;i<THREAD_WORK_QUEUE_COUNT;++i)
        pool->work_queues[i] = new_thread_work_queue(i, alloc);

    int res;
    for(i=0;i<THREAD_COUNT;++i) {
        pool->threads[i].id = i;
        pool->threads[i].pool_write_flags = 0x0;
        pool->threads[i].thread_write_flags = 0x0;
#ifdef SOL_ALLOC
        init_arena_allocator(&pool->threads[i].alloc_heap, buffers_heap[i].size);
        init_linear_allocator(&pool->threads[i].alloc_temp, buffers_temp[i].size, buffers_temp[i].data);
#endif
        pool->threads[i].work_queues = pool->work_queues;
        res = create_thread(&pool->threads[i].handle, NULL, thread_start, &pool->threads[i]);
        log_print_error_if(res, "failed to create thread %u", i);
    }
    return res;
}

#define THREAD_WAIT_POOL_IDLE_MAX_LOOP_COUNT 0xffff

static inline bool is_thread_idle(thread *t) {
    return t->thread_write_flags & THREAD_BUSY_BIT;
}

static inline bool thread_signal_shutdown(thread *t, uint graceful_bit) {
    atomic_or(&t->pool_write_flags, THREAD_SHUTDOWN_BIT | graceful_bit);
    return is_thread_idle(t);
}

// @Todo Although I am pretty sure that the threading implementation itself is pretty solid
// (from memory) this interface is pretty weak. A second ago this function was literally just
// blocking forever or always returning true, but the return value was thread_result.
// I have seen similar interface issues elsewhere. Although it is not a desperate thing to
// work on, as I know what everything is doing when I see it break, but this is definitely
// a file that needs some makeup.
void free_thread_pool(thread_pool *pool, bool graceful)
{
    thread_status("Freeing threadpool");
    uint i;
    for(i=0;i<THREAD_COUNT;++i)
        thread_signal_shutdown(&pool->threads[i], THREAD_GRACEFUL_BIT & max32_if_true(graceful));

    void *rets[THREAD_COUNT];
    for(i=0;i<THREAD_COUNT;++i)
        join_thread(pool->threads[i].handle, &rets[i]);

    for(i=0;i<THREAD_WORK_QUEUE_COUNT;++i)
        free_thread_work_queue(&pool->work_queues[i]);

    thread_status("Thread pool free");
}

uint thread_add_work(thread_pool *pool, uint count, thread_work *work, thread_work_queue_priority priority)
{
    return thread_work_queue_add(&pool->work_queues[priority], count, work);
}

static thread_work_queue new_thread_work_queue(uint i, allocator *alloc)
{
    thread_work_queue ret;
    int err = init_mutex(&thread_work_queue_locks[i]);
    log_print_error_if(err, "failed to init mutex, err code %s", strerror(err));
    ret.lock = &thread_work_queue_locks[i];
    ret.work = sallocate(alloc, *ret.work, THREAD_WORK_QUEUE_SIZE);
    ret.tail = 0;
    ret.head = 0;
    ret.alloc = alloc;
    return ret;
}

static void free_thread_work_queue(thread_work_queue *work_queue)
{
    deallocate(work_queue->alloc, work_queue->work);
    work_queue->work = NULL;
    shutdown_mutex(work_queue->lock);
}

static uint thread_work_queue_add(thread_work_queue *queue, uint work_count, thread_work *work)
{
    assert(work_count);
    uint hi = inc_and_wrap(queue->head, 1, THREAD_WORK_QUEUE_SIZE);
    uint tail;
    atomic_load(&queue->tail, &tail);
    uint cnt = 0;
    while(hi != tail && cnt < work_count) {
        for(;hi!=tail && cnt < work_count;hi = inc_and_wrap_no_mod(hi, 1, THREAD_WORK_QUEUE_SIZE)) {
            queue->work[hi] = work[cnt];
            cnt++;
            atomic_store(&queue->head, &hi);
        }
        atomic_load(&queue->tail, &tail);
    }
    return cnt;
}

static inline uint thread_pause(uint pause_mask) {
    int const max = 64; // MAX_BACKOFF @Test Find a good value.
    for (uint i=pause_mask; i; --i)
        _mm_pause();

    pause_mask = pause_mask < max ? pause_mask << 1 : max;
    return pause_mask;
}

static inline void thread_begin_work(thread *self, thread_work *w) {
    atomic_or(&self->thread_write_flags, THREAD_BUSY_BIT);
    w->id = self->id;
    w->alloc_heap = &self->alloc_heap;
    w->alloc_temp = &self->alloc_temp;
    w->fn(w);
    atomic_and(&self->thread_write_flags, ~THREAD_BUSY_BIT);
}

static void* thread_start(thread *self)
{
    thread_status("Begin thread %u", self->id);
    thread_work w;
    w.id = self->id;
    assert(THREAD_WORK_QUEUE_COUNT < 32 && "need bigger masks");
    uint32 in_use_mask, empty_mask;
    uint pause_mask = 1;
    uint i;
    uint head, tail;
    while(!(self->pool_write_flags & THREAD_SHUTDOWN_BIT)) {
        in_use_mask = 0x0;
        empty_mask = 0x0;
        for(i=0;i<THREAD_WORK_QUEUE_COUNT;++i) {

            // @Test This could be done with one load using flags,
            // but that would also generate a write. I assume two
            // loads is faster.
            atomic_load(&self->work_queues[i].head, &head);
            atomic_load(&self->work_queues[i].tail, &tail);
            if (tail == head) {
                empty_mask |= 1 << i;
                continue;
            }

            switch(thread_try_to_acquire_work(&self->work_queues[i], &w)) {
            case THREAD_RESULT_SUCCESS:
                thread_begin_work(self, &w);
                i = 0; // try to acquire highest priority work
                in_use_mask &= ~(1 << i);
                empty_mask &= ~(1 << i);
                pause_mask = 1;
                break;
            case THREAD_RESULT_QUEUE_IN_USE:
                in_use_mask |= 1 << i;
                break;
            case THREAD_RESULT_QUEUE_EMPTY:
                empty_mask |= 1 << i; // may have become empty between cmpxchg and thread_begin_work()
                break;
            case THREAD_RESULT_MUTEX_ERROR:
                log_print_error("mutex error acquiring work from queue %u - thread id %u", i, self->id);
                break;
            default:
                break;
            }
        }
        // @Todo Probably want to react differently depending on empty vs full, or maybe do not want to control that
        // here, and only want to react the main thread setting flags, as it will understand the workload.
        if ((in_use_mask | empty_mask) == ~(Max_u32 << THREAD_WORK_QUEUE_COUNT)) {
            thread_status("thread %u pausing for %u cycles", self->id, pause_mask);
            pause_mask = thread_pause(pause_mask);
        }
    }

    // @Todo Return work stats instead of null to judge core utilisation.
    return thread_shutdown(self);
}

static uint thread_try_to_acquire_work(thread_work_queue *queue, thread_work *work)
{
    switch(try_to_acquire_mutex(queue->lock)) {
    case 0:
        break;
    case EBUSY:
        thread_status("thread %u found in use queue", work->id);
        return THREAD_RESULT_QUEUE_IN_USE;
    default:
        log_print_error("mutex attempted acquisition returned abnormal error code %s, thread id %u", strerror(errno), work->id);
        return THREAD_RESULT_MUTEX_ERROR;
    }

    uint result = THREAD_RESULT_SUCCESS;
    if (atomic_cmpxchg_bool(&queue->head, queue->tail, queue->tail)) {
        thread_status("thread %u found empty queue", work->id);
        result = THREAD_RESULT_QUEUE_EMPTY;
        goto release_lock;
    }

    uint ti = inc_and_wrap_no_mod(queue->tail, 1, THREAD_WORK_QUEUE_SIZE);
    assert(queue->work[ti].fn);

    thread_status("thread %u got work item %u", work->id, ti);

    *work = queue->work[ti];
    queue->work[ti].arg = NULL; // Crash on invalid access.
    queue->work[ti].fn = NULL;
    queue->tail = ti;

release_lock: // @Todo This release can happen earlier
    release_mutex(queue->lock);
    return result;
}

static void* thread_shutdown(thread *self)
{
    if (!(self->pool_write_flags & THREAD_GRACEFUL_BIT)) {
        thread_status("thread %u shutting down ungracefully", self->id);
        return NULL;
    }
    thread_status("thread %u shutting down with grace", self->id);

    thread_work w;
    w.id = self->id;
    uint empty_mask = 0x0;
    uint in_use_mask = 0x0;
    uint pause_mask = 1;
    uint i;
    while(empty_mask != ~(Max_u32 << THREAD_WORK_QUEUE_COUNT)) {
        in_use_mask = 0x0;
        for(i=0;i<THREAD_WORK_QUEUE_COUNT;++i) {
            if (empty_mask & (1<<i))
                continue;

            switch(thread_try_to_acquire_work(&self->work_queues[i], &w)) {
            case THREAD_RESULT_SUCCESS:
                thread_begin_work(self, &w);
                in_use_mask &= ~(1 << i);
                pause_mask = 1;
                break;
            case THREAD_RESULT_QUEUE_IN_USE:
                in_use_mask |= 1 << i;
                break;
            case THREAD_RESULT_QUEUE_EMPTY:
                empty_mask |= 1 << i;
                break;
            case THREAD_RESULT_MUTEX_ERROR:
                log_print_error("mutex error acquiring work from queue %u - thread id %u", i, self->id);
                break;
            default:
                break;
            }
        }
        // @Todo Probably want to react differently depending on empty vs full, or maybe do not want to control that
        // here, and only want to react the main thread setting flags, as it will understand the workload.
        if (in_use_mask == ~(Max_u32 << THREAD_WORK_QUEUE_COUNT)) {
            thread_status("thread %u pausing for %u cycles", self->id, pause_mask);
            pause_mask = thread_pause(pause_mask);
        }
    }
    return NULL;
}

#endif // #ifdef SOL_THREAD

////////////////////////////////////////////////////////////////////////////////
// time.c

static void dump_prog_timer_thread(program_timer *pt, string_builder *sb, uint idx);

void dump_prog_timer(program_timer *pt, const char *file, allocator *alloc)
{
    uint sz = 0;
    for(uint i = 0; i < THREAD_COUNT+1; ++i)
        sz += array_len(pt->t[i].t);
    sz <<= 6;

    char *buf = allocate(alloc, sz);

    // @Todo Make string_builder able to be dynamic...
    string_builder sb = sb_new(sz, buf);

    dump_prog_timer_thread(pt, &sb, 0);
    for(uint i = 0; i < THREAD_COUNT; ++i)
        dump_prog_timer_thread(pt, &sb, i+1);

    if (file)
        file_write_char(file, sb.used, sb.data);
    else
        print_count_chars(sb.data, sb.used);
}

static void dump_prog_timer_thread(program_timer *pt, string_builder *sb, uint idx)
{
    const char main_msg[] = ", (main thread)";
    const char thread_msg[] = "Thread ";
    char nb[8];
    uint nl = uint_to_ascii(idx, nb);

    sb_add(sb, cstr_as_array_len(thread_msg), thread_msg);
    sb_add(sb, nl, nb);
    sb_add_if(sb, cstr_as_array_len(main_msg), main_msg, idx == 0);
    sb_add(sb, 2, ":\n");

    for(uint i = 0; i < array_len(pt->t[idx].t); ++i) {
        sb_add(sb, 2, "  ");
        sb_add(sb, pt->t[idx].t[i].fn.len, pt->t[idx].t[i].fn.cstr);
        sb_add(sb, 2, ", ");

        // @Optimise Should use a specialised function here, not just general string_format()
        nl = sb->used;
        assert(sb->used - sb->cap > 32 && "Inserting float may overflow");
        string_format(sb->data + sb->used, "%f", pt->t[idx].t[i].t);
        sb_inc(sb, strlen(sb->data + nl));

        sb_addnl(sb);
    }

    reset_block_timer(&pt->t[idx]);
}

////////////////////////////////////////////////////////////////////////////////
// test.c

// Format colors
#define RED    "\e[1;31m"
#define GREEN  "\e[1;32m"
#define YELLOW "\e[1;33m"
#define BLUE   "\e[1;34m"
#define NC     "\e[0m"

#define TEST_PRINT_TESTS 1

#if TEST_PRINT_TESTS
    #define print_test_status(...) println(__VA_ARGS__)
#else
    #define print_test_status(...)
#endif

test_suite load_tests(allocator *alloc)
{
    print_test_status("\nTest Config:");
    if (TEST_LIST_BROKEN) {
        print_test_status("TEST_LIST_BROKEN  == true, printing broken tests...");
    } else {
        print_test_status("TEST_LIST_BROKEN  == false, broken tests silent...");
    }
    if (TEST_LIST_SKIPPED) {
        print_test_status("TEST_LIST_SKIPPED == true, printing skipped tests...");
    } else {
        print_test_status("TEST_LIST_SKIPPED == false, skipped tests silent...");
    }
    print_test_status("\nBeginning Tests...");

    test_suite ret = (test_suite){};
    ret.tests   = new_array(128, *ret.tests, alloc);
    ret.modules = new_array(128, *ret.modules, alloc);
    ret.str_buf = new_static_strbuf(1024, alloc);
    ret.alloc = alloc;
    return ret;
}

void end_tests(test_suite *suite)
{
    if (suite->fail)
        print_test_status("");

    // -Wunused
    // uint32 skipped_count = 0;
    // uint32 broken_count = 0;
    // uint32 fail_count = 0;

    // Ofc these should just be separate arrays on the suite. But it does not matter for now. Bigger fish.
    for(uint32 i = 0; i < array_len(suite->modules); ++i) {
        switch(suite->modules[i].result) {
        case TEST_RESULT_BROKEN:
            print_test_status("%sMODULE BROKEN:%s", YELLOW, NC);
            print_test_status("    %s", suite->modules[i].info.cstr);
            // broken_count++; -Wunused
            break;
        case TEST_RESULT_SKIPPED:
            print_test_status("%sMODULE SKIPPED:%s", YELLOW, NC);
            print_test_status("    %s", suite->modules[i].info.cstr);
            // skipped_count++; -Wunused
            break;
        case TEST_RESULT_FAIL:
            print_test_status("%sMODULE FAILED:%s", RED, NC);
            print_test_status("    %s", suite->modules[i].info.cstr);
            // fail_count++; -Wunused
            break;
        case TEST_RESULT_SUCCESS:
            break;
        }
    }

    if (suite->fail || (suite->broken && TEST_LIST_BROKEN) || (suite->skipped && TEST_LIST_SKIPPED))
        print_test_status("");

    if (suite->skipped)
        print_test_status("%sTESTS WERE SKIPPED%s", YELLOW, NC);
    else
        print_test_status("%sNO TESTS SKIPPED%s", GREEN, NC);

    if (suite->broken)
        print_test_status("%sTESTS ARE BROKEN%s", YELLOW, NC);
    else
        print_test_status("%sNO TESTS BROKEN%s", GREEN, NC);

    if (suite->fail)
        print_test_status("%sTESTS WERE FAILED%s", RED, NC);
    else
        print_test_status("%sNO TESTS FAILED%s", GREEN, NC);

    print_test_status("");

    assert(!suite->skipped && !suite->broken && !suite->fail);

    free_strbuf(&suite->str_buf);
    free_array(suite->tests);
    free_array(suite->modules);

    allocator_reset_linear(suite->alloc);
}

void begin_test_module(
    test_suite *suite,
    const char *name,
    const char *function_name,
    const char *file_name,
    bool        broken,
    bool        skipped)
{
    test_module mod = (test_module){};
    mod.start = array_len(suite->tests);

    char info_buffer[256];
    string_format(info_buffer, "[%s, fn %s] %s", file_name, function_name, name);
    assert(strlen(info_buffer) < 255);

    mod.info = strbuf_add_cstr(&suite->str_buf, info_buffer);
    if (broken) {
        mod.result = TEST_RESULT_BROKEN;
    } else if (skipped) {
        mod.result = TEST_RESULT_SKIPPED;
    } else {
        mod.result = TEST_RESULT_SUCCESS;
    }
    array_add(suite->modules, mod);
}

// Message macros
#if TEST_LIST_BROKEN
#define TEST_MSG_BROKEN(info) \
    print_test_status("%sBroken Test: %s%s", YELLOW, NC, info)
#else
#define TEST_MSG_BROKEN(name)
#endif

#if TEST_LIST_SKIPPED
#define TEST_MSG_SKIPPED(name) \
    print_test_status("%sSkipped Test%s '%s'", YELLOW, NC, name)
#else
#define TEST_MSG_SKIPPED(name)
#endif

#define SKIP_BROKEN_TEST_MSG(mod) \
    print_test_status("%sWarning: Module Skips %u Broken Tests...%s", YELLOW, mod.skipped_broken_test_names.len, NC)

#define TEST_MSG_FAIL(name, info, msg) print_test_status("%sFAILED TEST %s%s:\n%s\n    %s", RED, name, NC, info, msg)

#define TEST_MSG_PASS print_test_status("%sOK%s", GREEN, NC)

// Tests
void test_backend(
    test_suite *suite,
    const char *function_name,
    const char *file_name,
    int         line_number,
    const char *test_name,
    const       char *msg,
    bool        test_passed,
    bool        broken)
{
    test_result result;
    test_module *module = &array_last(suite->modules);

    char info[127];
    string_format(info, "%s, line %i, fn %s", file_name,  line_number, function_name);
    string str = strbuf_add_cstr(&suite->str_buf, info);

    if (broken || module->result == TEST_RESULT_BROKEN || module->result == TEST_RESULT_SKIPPED) {
        if (broken || module->result == broken) {
            TEST_MSG_BROKEN(info);
            result = TEST_RESULT_BROKEN;
            suite->broken++;
        } else {
            print_test_status("%s", str.cstr);
            TEST_MSG_SKIPPED(test_name);
            result = TEST_RESULT_SKIPPED;
            suite->skipped++;
        }
    } else {
        if (test_passed) {
            result = TEST_RESULT_SUCCESS;
        } else {
            TEST_MSG_FAIL(test_name, info, msg);
            result = TEST_RESULT_FAIL;
            module->result = TEST_RESULT_FAIL;
            suite->fail++;
        }
    }

    if (result != TEST_RESULT_SUCCESS) {
        test t = (test){.info = str, .result = result, .index = (uint)array_len(suite->tests)};
        array_add(suite->tests, t);
    }

    return;
}

void test_eq(
    test_suite *suite,
    const char *test_name,
    const char *function_name,
    const char *arg1_name,
    const char *arg2_name,
    int64       arg1,
    int64       arg2,
    bool        broken,
    int         line_number,
    const char *file_name)
{
    bool test_passed = arg1 == arg2;

    char msg[256];
    string_format(msg, "%s = %i, %s = %i", arg1_name, arg1, arg2_name, arg2);
    test_backend(suite, function_name, file_name, line_number, test_name, msg, test_passed, broken);

    return;
}

void test_lt(
    test_suite *suite,
    const char *test_name,
    const char *function_name,
    const char *arg1_name,
    const char *arg2_name,
    int64       arg1,
    int64       arg2,
    bool        broken,
    int         line_number,
    const char *file_name)
{
    bool test_passed = arg1 < arg2;

    char msg[256];
    string_format(msg, "%s = %i, %s = %i", arg1_name, arg1, arg2_name, arg2);
    test_backend(suite, function_name, file_name, line_number, test_name, msg, test_passed, broken);

    return;
}

void test_floateq(
    test_suite *suite,
    const char *test_name,
    const char *function_name,
    const char *arg1_name,
    const char *arg2_name,
    float       arg1,
    float       arg2,
    bool        broken,
    int         line_number,
    const char *file_name)
{
    // float f1 = arg1 - arg2;
    // float f2 = arg2 - arg1;
    // bool test_passed = (f1 < inaccuracy) && (f2 < inaccuracy) ? true : false;

    float inaccuracy = 0.000001;
    bool test_passed = fabsf(arg1 - arg2) < inaccuracy;

    char msg[256];
    string_format(msg, "%s = %f, %s = %f", arg1_name, arg1, arg2_name, arg2);
    test_backend(suite, function_name, file_name, line_number, test_name, msg, test_passed, broken);

    return;
}

void test_streq(
    test_suite *suite,
    const char *test_name,
    const char *function_name,
    const char *arg1_name,
    const char *arg2_name,
    const char* arg1,
    const char* arg2,
    bool        broken,
    int         line_number,
    const char *file_name)
{
    bool test_passed = strcmp(arg1, arg2) == 0;

    char msg[256];
    string_format(msg, "%s = %s, %s = %s", arg1_name, arg1, arg2_name, arg2);
    test_backend(suite, function_name, file_name, line_number, test_name, msg, test_passed, broken);

    return;
}

void test_ptreq(
    test_suite *suite,
    const char *test_name,
    const char *function_name,
    const char *arg1_name,
    const char *arg2_name,
    const       void *arg1,
    const       void* arg2,
    bool        broken,
    int         line_number,
    const char *file_name)
{
    bool test_passed = arg1 == arg2;

    char msg[256];
    string_format(msg, "%s = %uh, %s = %uh", arg1_name, arg1, arg2_name, arg2);
    test_backend(suite, function_name, file_name, line_number, test_name, msg, test_passed, broken);

    return;
}

#endif // source guard
#endif // include guard

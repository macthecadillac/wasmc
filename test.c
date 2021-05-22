/* Automically generated by wasm2c */
#include <math.h>
#include <string.h>

#include "../test.h"
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#define LIKELY(x) __builtin_expect(!!(x), 1)

#define TRAP(x) (wasm_rt_trap(WASM_RT_TRAP_##x), 0)

#define FUNC_PROLOGUE                                            \
  if (++wasm_rt_call_stack_depth > WASM_RT_MAX_CALL_STACK_DEPTH) \
    TRAP(EXHAUSTION)

#define FUNC_EPILOGUE --wasm_rt_call_stack_depth

#define UNREACHABLE TRAP(UNREACHABLE)

#define CALL_INDIRECT(table, t, ft, x, ...)          \
  (LIKELY((x) < table.size && table.data[x].func &&  \
          table.data[x].func_type == func_types[ft]) \
       ? ((t)table.data[x].func)(__VA_ARGS__)        \
       : TRAP(CALL_INDIRECT))

#if WASM_RT_MEMCHECK_SIGNAL_HANDLER
#define MEMCHECK(mem, a, t)
#else
#define MEMCHECK(mem, a, t)  \
  if (UNLIKELY((a) + sizeof(t) > mem->size)) TRAP(OOB)
#endif

#if WABT_BIG_ENDIAN
static inline void load_data(void *dest, const void *src, size_t n) {
  size_t i = 0;
  u8 *dest_chars = dest;
  memcpy(dest, src, n);
  for (i = 0; i < (n>>1); i++) {
    u8 cursor = dest_chars[i];
    dest_chars[i] = dest_chars[n - i - 1];
    dest_chars[n - i - 1] = cursor;
  }
}
#define LOAD_DATA(m, o, i, s) load_data(&(m.data[m.size - o - s]), i, s)
#define DEFINE_LOAD(name, t1, t2, t3)                                                 \
  static inline t3 name(wasm_rt_memory_t* mem, u64 addr) {                            \
    MEMCHECK(mem, addr, t1);                                                          \
    t1 result;                                                                        \
    __builtin_memcpy(&result, &mem->data[mem->size - addr - sizeof(t1)], sizeof(t1)); \
    return (t3)(t2)result;                                                            \
  }

#define DEFINE_STORE(name, t1, t2)                                                     \
  static inline void name(wasm_rt_memory_t* mem, u64 addr, t2 value) {                 \
    MEMCHECK(mem, addr, t1);                                                           \
    t1 wrapped = (t1)value;                                                            \
    __builtin_memcpy(&mem->data[mem->size - addr - sizeof(t1)], &wrapped, sizeof(t1)); \
  }
#else
static inline void load_data(void *dest, const void *src, size_t n) {
  memcpy(dest, src, n);
}
#define LOAD_DATA(m, o, i, s) load_data(&(m.data[o]), i, s)
#define DEFINE_LOAD(name, t1, t2, t3)                        \
  static inline t3 name(wasm_rt_memory_t* mem, u64 addr) {   \
    MEMCHECK(mem, addr, t1);                                 \
    t1 result;                                               \
    __builtin_memcpy(&result, &mem->data[addr], sizeof(t1)); \
    return (t3)(t2)result;                                   \
  }

#define DEFINE_STORE(name, t1, t2)                                     \
  static inline void name(wasm_rt_memory_t* mem, u64 addr, t2 value) { \
    MEMCHECK(mem, addr, t1);                                           \
    t1 wrapped = (t1)value;                                            \
    __builtin_memcpy(&mem->data[addr], &wrapped, sizeof(t1));          \
  }
#endif

DEFINE_LOAD(i32_load, u32, u32, u32);
DEFINE_LOAD(i64_load, u64, u64, u64);
DEFINE_LOAD(f32_load, f32, f32, f32);
DEFINE_LOAD(f64_load, f64, f64, f64);
DEFINE_LOAD(i32_load8_s, s8, s32, u32);
DEFINE_LOAD(i64_load8_s, s8, s64, u64);
DEFINE_LOAD(i32_load8_u, u8, u32, u32);
DEFINE_LOAD(i64_load8_u, u8, u64, u64);
DEFINE_LOAD(i32_load16_s, s16, s32, u32);
DEFINE_LOAD(i64_load16_s, s16, s64, u64);
DEFINE_LOAD(i32_load16_u, u16, u32, u32);
DEFINE_LOAD(i64_load16_u, u16, u64, u64);
DEFINE_LOAD(i64_load32_s, s32, s64, u64);
DEFINE_LOAD(i64_load32_u, u32, u64, u64);
DEFINE_STORE(i32_store, u32, u32);
DEFINE_STORE(i64_store, u64, u64);
DEFINE_STORE(f32_store, f32, f32);
DEFINE_STORE(f64_store, f64, f64);
DEFINE_STORE(i32_store8, u8, u32);
DEFINE_STORE(i32_store16, u16, u32);
DEFINE_STORE(i64_store8, u8, u64);
DEFINE_STORE(i64_store16, u16, u64);
DEFINE_STORE(i64_store32, u32, u64);

#define I32_CLZ(x) ((x) ? __builtin_clz(x) : 32)
#define I64_CLZ(x) ((x) ? __builtin_clzll(x) : 64)
#define I32_CTZ(x) ((x) ? __builtin_ctz(x) : 32)
#define I64_CTZ(x) ((x) ? __builtin_ctzll(x) : 64)
#define I32_POPCNT(x) (__builtin_popcount(x))
#define I64_POPCNT(x) (__builtin_popcountll(x))

#define DIV_S(ut, min, x, y)                                 \
   ((UNLIKELY((y) == 0)) ?                TRAP(DIV_BY_ZERO)  \
  : (UNLIKELY((x) == min && (y) == -1)) ? TRAP(INT_OVERFLOW) \
  : (ut)((x) / (y)))

#define REM_S(ut, min, x, y)                                \
   ((UNLIKELY((y) == 0)) ?                TRAP(DIV_BY_ZERO) \
  : (UNLIKELY((x) == min && (y) == -1)) ? 0                 \
  : (ut)((x) % (y)))

#define I32_DIV_S(x, y) DIV_S(u32, INT32_MIN, (s32)x, (s32)y)
#define I64_DIV_S(x, y) DIV_S(u64, INT64_MIN, (s64)x, (s64)y)
#define I32_REM_S(x, y) REM_S(u32, INT32_MIN, (s32)x, (s32)y)
#define I64_REM_S(x, y) REM_S(u64, INT64_MIN, (s64)x, (s64)y)

#define DIVREM_U(op, x, y) \
  ((UNLIKELY((y) == 0)) ? TRAP(DIV_BY_ZERO) : ((x) op (y)))

#define DIV_U(x, y) DIVREM_U(/, x, y)
#define REM_U(x, y) DIVREM_U(%, x, y)

#define ROTL(x, y, mask) \
  (((x) << ((y) & (mask))) | ((x) >> (((mask) - (y) + 1) & (mask))))
#define ROTR(x, y, mask) \
  (((x) >> ((y) & (mask))) | ((x) << (((mask) - (y) + 1) & (mask))))

#define I32_ROTL(x, y) ROTL(x, y, 31)
#define I64_ROTL(x, y) ROTL(x, y, 63)
#define I32_ROTR(x, y) ROTR(x, y, 31)
#define I64_ROTR(x, y) ROTR(x, y, 63)

#define FMIN(x, y)                                          \
   ((UNLIKELY((x) != (x))) ? NAN                            \
  : (UNLIKELY((y) != (y))) ? NAN                            \
  : (UNLIKELY((x) == 0 && (y) == 0)) ? (signbit(x) ? x : y) \
  : (x < y) ? x : y)

#define FMAX(x, y)                                          \
   ((UNLIKELY((x) != (x))) ? NAN                            \
  : (UNLIKELY((y) != (y))) ? NAN                            \
  : (UNLIKELY((x) == 0 && (y) == 0)) ? (signbit(x) ? y : x) \
  : (x > y) ? x : y)

#define TRUNC_S(ut, st, ft, min, minop, max, x)                             \
  ((UNLIKELY((x) != (x)))                        ? TRAP(INVALID_CONVERSION) \
   : (UNLIKELY(!((x)minop(min) && (x) < (max)))) ? TRAP(INT_OVERFLOW)       \
                                                 : (ut)(st)(x))

#define I32_TRUNC_S_F32(x) TRUNC_S(u32, s32, f32, (f32)INT32_MIN, >=, 2147483648.f, x)
#define I64_TRUNC_S_F32(x) TRUNC_S(u64, s64, f32, (f32)INT64_MIN, >=, (f32)INT64_MAX, x)
#define I32_TRUNC_S_F64(x) TRUNC_S(u32, s32, f64, -2147483649., >, 2147483648., x)
#define I64_TRUNC_S_F64(x) TRUNC_S(u64, s64, f64, (f64)INT64_MIN, >=, (f64)INT64_MAX, x)

#define TRUNC_U(ut, ft, max, x)                                            \
  ((UNLIKELY((x) != (x)))                       ? TRAP(INVALID_CONVERSION) \
   : (UNLIKELY(!((x) > (ft)-1 && (x) < (max)))) ? TRAP(INT_OVERFLOW)       \
                                                : (ut)(x))

#define I32_TRUNC_U_F32(x) TRUNC_U(u32, f32, 4294967296.f, x)
#define I64_TRUNC_U_F32(x) TRUNC_U(u64, f32, (f32)UINT64_MAX, x)
#define I32_TRUNC_U_F64(x) TRUNC_U(u32, f64, 4294967296.,  x)
#define I64_TRUNC_U_F64(x) TRUNC_U(u64, f64, (f64)UINT64_MAX, x)

#define DEFINE_REINTERPRET(name, t1, t2)  \
  static inline t2 name(t1 x) {           \
    t2 result;                            \
    memcpy(&result, &x, sizeof(result));  \
    return result;                        \
  }

DEFINE_REINTERPRET(f32_reinterpret_i32, u32, f32)
DEFINE_REINTERPRET(i32_reinterpret_f32, f32, u32)
DEFINE_REINTERPRET(f64_reinterpret_i64, u64, f64)
DEFINE_REINTERPRET(i64_reinterpret_f64, f64, u64)


static u32 func_types[7];

static void init_func_types(void) {
  func_types[0] = wasm_rt_register_func_type(0, 1, WASM_RT_I32);
  func_types[1] = wasm_rt_register_func_type(2, 1, WASM_RT_I32, WASM_RT_I32, WASM_RT_I32);
  func_types[2] = wasm_rt_register_func_type(3, 0, WASM_RT_I32, WASM_RT_I32, WASM_RT_I32);
  func_types[3] = wasm_rt_register_func_type(3, 1, WASM_RT_I32, WASM_RT_I32, WASM_RT_I32, WASM_RT_I32);
  func_types[4] = wasm_rt_register_func_type(2, 0, WASM_RT_I32, WASM_RT_I32);
  func_types[5] = wasm_rt_register_func_type(1, 1, WASM_RT_I32, WASM_RT_I32);
  func_types[6] = wasm_rt_register_func_type(0, 0);
}

static u32 w2c_f0(void);
static u32 w2c_f1(void);
static u32 w2c_offsetFromCoordinate(u32, u32);
static void w2c_setCell(u32, u32, u32);
static u32 w2c_getCell(u32, u32);
static u32 w2c_liveNeighbourCount(u32, u32);
static u32 w2c_inRange(u32, u32, u32);
static u32 w2c_isCellAlive(u32, u32);
static void w2c_setCellStateForNextGeneration(u32, u32, u32);
static void w2c_evolveCellToNextGeneration(u32, u32);
static u32 w2c_f10(u32);
static void w2c_evolveAllCells(void);
static void w2c_promoteNextGeneration(void);
static void w2c_tick(void);

static void init_globals(void) {
}

static wasm_rt_memory_t w2c_memory;

static wasm_rt_table_t w2c_T0;

static u32 w2c_f0(void) {
  FUNC_PROLOGUE;
  u32 w2c_i0;
  w2c_i0 = 1u;
  FUNC_EPILOGUE;
  return w2c_i0;
}

static u32 w2c_f1(void) {
  FUNC_PROLOGUE;
  u32 w2c_i0;
  w2c_i0 = 0u;
  FUNC_EPILOGUE;
  return w2c_i0;
}

static u32 w2c_offsetFromCoordinate(u32 w2c_p0, u32 w2c_p1) {
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1, w2c_i2;
  w2c_i0 = 200u;
  w2c_i1 = w2c_p1;
  w2c_i0 *= w2c_i1;
  w2c_i1 = 4u;
  w2c_i2 = w2c_p0;
  w2c_i1 *= w2c_i2;
  w2c_i0 += w2c_i1;
  FUNC_EPILOGUE;
  return w2c_i0;
}

static void w2c_setCell(u32 w2c_p0, u32 w2c_p1, u32 w2c_p2) {
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1;
  w2c_i0 = w2c_p0;
  w2c_i1 = w2c_p1;
  w2c_i0 = w2c_offsetFromCoordinate(w2c_i0, w2c_i1);
  w2c_i1 = w2c_p2;
  i32_store((&w2c_memory), (u64)(w2c_i0), w2c_i1);
  FUNC_EPILOGUE;
}

static u32 w2c_getCell(u32 w2c_p0, u32 w2c_p1) {
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1, w2c_i2, w2c_i3;
  w2c_i0 = 0u;
  w2c_i1 = 50u;
  w2c_i2 = w2c_p0;
  w2c_i0 = w2c_inRange(w2c_i0, w2c_i1, w2c_i2);
  w2c_i1 = 0u;
  w2c_i2 = 50u;
  w2c_i3 = w2c_p1;
  w2c_i1 = w2c_inRange(w2c_i1, w2c_i2, w2c_i3);
  w2c_i0 &= w2c_i1;
  if (w2c_i0) {
    w2c_i0 = w2c_p0;
    w2c_i1 = w2c_p1;
    w2c_i0 = w2c_offsetFromCoordinate(w2c_i0, w2c_i1);
    w2c_i0 = i32_load8_u((&w2c_memory), (u64)(w2c_i0));
  } else {
    w2c_i0 = 0u;
  }
  FUNC_EPILOGUE;
  return w2c_i0;
}

static u32 w2c_liveNeighbourCount(u32 w2c_p0, u32 w2c_p1) {
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1, w2c_i2, w2c_i3;
  w2c_i0 = 0u;
  w2c_i1 = w2c_p0;
  w2c_i2 = 1u;
  w2c_i1 += w2c_i2;
  w2c_i2 = w2c_p1;
  w2c_i1 = w2c_isCellAlive(w2c_i1, w2c_i2);
  w2c_i0 += w2c_i1;
  w2c_i1 = w2c_p0;
  w2c_i2 = 4294967295u;
  w2c_i1 += w2c_i2;
  w2c_i2 = w2c_p1;
  w2c_i1 = w2c_isCellAlive(w2c_i1, w2c_i2);
  w2c_i0 += w2c_i1;
  w2c_i1 = w2c_p0;
  w2c_i2 = w2c_p1;
  w2c_i3 = 4294967295u;
  w2c_i2 += w2c_i3;
  w2c_i1 = w2c_isCellAlive(w2c_i1, w2c_i2);
  w2c_i0 += w2c_i1;
  w2c_i1 = w2c_p0;
  w2c_i2 = 4294967295u;
  w2c_i1 += w2c_i2;
  w2c_i2 = w2c_p1;
  w2c_i3 = 4294967295u;
  w2c_i2 += w2c_i3;
  w2c_i1 = w2c_isCellAlive(w2c_i1, w2c_i2);
  w2c_i0 += w2c_i1;
  w2c_i1 = w2c_p0;
  w2c_i2 = 1u;
  w2c_i1 += w2c_i2;
  w2c_i2 = w2c_p1;
  w2c_i3 = 4294967295u;
  w2c_i2 += w2c_i3;
  w2c_i1 = w2c_isCellAlive(w2c_i1, w2c_i2);
  w2c_i0 += w2c_i1;
  w2c_i1 = w2c_p0;
  w2c_i2 = w2c_p1;
  w2c_i3 = 1u;
  w2c_i2 += w2c_i3;
  w2c_i1 = w2c_isCellAlive(w2c_i1, w2c_i2);
  w2c_i0 += w2c_i1;
  w2c_i1 = w2c_p0;
  w2c_i2 = 4294967295u;
  w2c_i1 += w2c_i2;
  w2c_i2 = w2c_p1;
  w2c_i3 = 1u;
  w2c_i2 += w2c_i3;
  w2c_i1 = w2c_isCellAlive(w2c_i1, w2c_i2);
  w2c_i0 += w2c_i1;
  w2c_i1 = w2c_p0;
  w2c_i2 = 1u;
  w2c_i1 += w2c_i2;
  w2c_i2 = w2c_p1;
  w2c_i3 = 1u;
  w2c_i2 += w2c_i3;
  w2c_i1 = w2c_isCellAlive(w2c_i1, w2c_i2);
  w2c_i0 += w2c_i1;
  FUNC_EPILOGUE;
  return w2c_i0;
}

static u32 w2c_inRange(u32 w2c_p0, u32 w2c_p1, u32 w2c_p2) {
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1, w2c_i2;
  w2c_i0 = w2c_p2;
  w2c_i1 = w2c_p0;
  w2c_i0 = (u32)((s32)w2c_i0 >= (s32)w2c_i1);
  w2c_i1 = w2c_p2;
  w2c_i2 = w2c_p1;
  w2c_i1 = (u32)((s32)w2c_i1 < (s32)w2c_i2);
  w2c_i0 &= w2c_i1;
  FUNC_EPILOGUE;
  return w2c_i0;
}

static u32 w2c_isCellAlive(u32 w2c_p0, u32 w2c_p1) {
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1;
  w2c_i0 = w2c_p0;
  w2c_i1 = w2c_p1;
  w2c_i0 = w2c_getCell(w2c_i0, w2c_i1);
  w2c_i1 = 1u;
  w2c_i0 &= w2c_i1;
  FUNC_EPILOGUE;
  return w2c_i0;
}

static void w2c_setCellStateForNextGeneration(u32 w2c_p0, u32 w2c_p1, u32 w2c_p2) {
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1, w2c_i2, w2c_i3, w2c_i4;
  w2c_i0 = w2c_p0;
  w2c_i1 = w2c_p1;
  w2c_i2 = w2c_p0;
  w2c_i3 = w2c_p1;
  w2c_i2 = w2c_isCellAlive(w2c_i2, w2c_i3);
  w2c_i3 = w2c_p2;
  w2c_i4 = 1u;
  w2c_i3 <<= (w2c_i4 & 31);
  w2c_i2 |= w2c_i3;
  w2c_setCell(w2c_i0, w2c_i1, w2c_i2);
  FUNC_EPILOGUE;
}

static void w2c_evolveCellToNextGeneration(u32 w2c_p0, u32 w2c_p1) {
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1, w2c_i2, w2c_i3, w2c_i4;
  w2c_i0 = w2c_p0;
  w2c_i1 = w2c_p1;
  w2c_i2 = 9u;
  w2c_i3 = w2c_p0;
  w2c_i4 = w2c_p1;
  w2c_i3 = w2c_isCellAlive(w2c_i3, w2c_i4);
  w2c_i2 *= w2c_i3;
  w2c_i3 = w2c_p0;
  w2c_i4 = w2c_p1;
  w2c_i3 = w2c_liveNeighbourCount(w2c_i3, w2c_i4);
  w2c_i2 |= w2c_i3;
  w2c_i2 = CALL_INDIRECT(w2c_T0, u32 (*)(void), 0, w2c_i2);
  w2c_setCellStateForNextGeneration(w2c_i0, w2c_i1, w2c_i2);
  FUNC_EPILOGUE;
}

static u32 w2c_f10(u32 w2c_p0) {
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1;
  w2c_i0 = w2c_p0;
  w2c_i1 = 1u;
  w2c_i0 += w2c_i1;
  FUNC_EPILOGUE;
  return w2c_i0;
}

static void w2c_evolveAllCells(void) {
  u32 w2c_l0 = 0, w2c_l1 = 0;
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1;
  w2c_i0 = 0u;
  w2c_l1 = w2c_i0;
  w2c_L1: 
    w2c_i0 = 0u;
    w2c_l0 = w2c_i0;
    w2c_L3: 
      w2c_i0 = w2c_l0;
      w2c_i1 = w2c_l1;
      w2c_evolveCellToNextGeneration(w2c_i0, w2c_i1);
      w2c_i0 = w2c_l0;
      w2c_i0 = w2c_f10(w2c_i0);
      w2c_l0 = w2c_i0;
      w2c_i0 = w2c_l0;
      w2c_i1 = 50u;
      w2c_i0 = w2c_i0 == w2c_i1;
      if (w2c_i0) {goto w2c_B2;}
      goto w2c_L3;
    w2c_B2:;
    w2c_i0 = w2c_l1;
    w2c_i0 = w2c_f10(w2c_i0);
    w2c_l1 = w2c_i0;
    w2c_i0 = w2c_l1;
    w2c_i1 = 50u;
    w2c_i0 = w2c_i0 == w2c_i1;
    if (w2c_i0) {goto w2c_B0;}
    goto w2c_L1;
  w2c_B0:;
  FUNC_EPILOGUE;
}

static void w2c_promoteNextGeneration(void) {
  u32 w2c_l0 = 0, w2c_l1 = 0;
  FUNC_PROLOGUE;
  u32 w2c_i0, w2c_i1, w2c_i2, w2c_i3;
  w2c_i0 = 0u;
  w2c_l1 = w2c_i0;
  w2c_L1: 
    w2c_i0 = 0u;
    w2c_l0 = w2c_i0;
    w2c_L3: 
      w2c_i0 = w2c_l0;
      w2c_i1 = w2c_l1;
      w2c_i2 = w2c_l0;
      w2c_i3 = w2c_l1;
      w2c_i2 = w2c_getCell(w2c_i2, w2c_i3);
      w2c_i3 = 1u;
      w2c_i2 >>= (w2c_i3 & 31);
      w2c_setCell(w2c_i0, w2c_i1, w2c_i2);
      w2c_i0 = w2c_l0;
      w2c_i0 = w2c_f10(w2c_i0);
      w2c_l0 = w2c_i0;
      w2c_i0 = w2c_l0;
      w2c_i1 = 50u;
      w2c_i0 = w2c_i0 == w2c_i1;
      if (w2c_i0) {goto w2c_B2;}
      goto w2c_L3;
    w2c_B2:;
    w2c_i0 = w2c_l1;
    w2c_i0 = w2c_f10(w2c_i0);
    w2c_l1 = w2c_i0;
    w2c_i0 = w2c_l1;
    w2c_i1 = 50u;
    w2c_i0 = w2c_i0 == w2c_i1;
    if (w2c_i0) {goto w2c_B0;}
    goto w2c_L1;
  w2c_B0:;
  FUNC_EPILOGUE;
}

static void w2c_tick(void) {
  FUNC_PROLOGUE;
  w2c_evolveAllCells();
  w2c_promoteNextGeneration();
  FUNC_EPILOGUE;
}


static void init_memory(void) {
  wasm_rt_allocate_memory((&w2c_memory), 1, 65536);
}

static void init_table(void) {
  uint32_t offset;
  wasm_rt_allocate_table((&w2c_T0), 18, 4294967295);
  offset = 0u;
  w2c_T0.data[offset + 0] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 1] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 2] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 3] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f0)};
  w2c_T0.data[offset + 4] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 5] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 6] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 7] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 8] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 9] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 10] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 11] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f0)};
  w2c_T0.data[offset + 12] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f0)};
  w2c_T0.data[offset + 13] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 14] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 15] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 16] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
  w2c_T0.data[offset + 17] = (wasm_rt_elem_t){func_types[0], (wasm_rt_anyfunc_t)(&w2c_f1)};
}

/* export: 'tick' */
void (*WASM_RT_ADD_PREFIX(Z_tickZ_vv))(void);
/* export: 'promoteNextGeneration' */
void (*WASM_RT_ADD_PREFIX(Z_promoteNextGenerationZ_vv))(void);
/* export: 'evolveAllCells' */
void (*WASM_RT_ADD_PREFIX(Z_evolveAllCellsZ_vv))(void);
/* export: 'evolveCellToNextGeneration' */
void (*WASM_RT_ADD_PREFIX(Z_evolveCellToNextGenerationZ_vii))(u32, u32);
/* export: 'setCellStateForNextGeneration' */
void (*WASM_RT_ADD_PREFIX(Z_setCellStateForNextGenerationZ_viii))(u32, u32, u32);
/* export: 'isCellAlive' */
u32 (*WASM_RT_ADD_PREFIX(Z_isCellAliveZ_iii))(u32, u32);
/* export: 'inRange' */
u32 (*WASM_RT_ADD_PREFIX(Z_inRangeZ_iiii))(u32, u32, u32);
/* export: 'offsetFromCoordinate' */
u32 (*WASM_RT_ADD_PREFIX(Z_offsetFromCoordinateZ_iii))(u32, u32);
/* export: 'liveNeighbourCount' */
u32 (*WASM_RT_ADD_PREFIX(Z_liveNeighbourCountZ_iii))(u32, u32);
/* export: 'getCell' */
u32 (*WASM_RT_ADD_PREFIX(Z_getCellZ_iii))(u32, u32);
/* export: 'setCell' */
void (*WASM_RT_ADD_PREFIX(Z_setCellZ_viii))(u32, u32, u32);
/* export: 'memory' */
wasm_rt_memory_t (*WASM_RT_ADD_PREFIX(Z_memory));

static void init_exports(void) {
  /* export: 'tick' */
  WASM_RT_ADD_PREFIX(Z_tickZ_vv) = (&w2c_tick);
  /* export: 'promoteNextGeneration' */
  WASM_RT_ADD_PREFIX(Z_promoteNextGenerationZ_vv) = (&w2c_promoteNextGeneration);
  /* export: 'evolveAllCells' */
  WASM_RT_ADD_PREFIX(Z_evolveAllCellsZ_vv) = (&w2c_evolveAllCells);
  /* export: 'evolveCellToNextGeneration' */
  WASM_RT_ADD_PREFIX(Z_evolveCellToNextGenerationZ_vii) = (&w2c_evolveCellToNextGeneration);
  /* export: 'setCellStateForNextGeneration' */
  WASM_RT_ADD_PREFIX(Z_setCellStateForNextGenerationZ_viii) = (&w2c_setCellStateForNextGeneration);
  /* export: 'isCellAlive' */
  WASM_RT_ADD_PREFIX(Z_isCellAliveZ_iii) = (&w2c_isCellAlive);
  /* export: 'inRange' */
  WASM_RT_ADD_PREFIX(Z_inRangeZ_iiii) = (&w2c_inRange);
  /* export: 'offsetFromCoordinate' */
  WASM_RT_ADD_PREFIX(Z_offsetFromCoordinateZ_iii) = (&w2c_offsetFromCoordinate);
  /* export: 'liveNeighbourCount' */
  WASM_RT_ADD_PREFIX(Z_liveNeighbourCountZ_iii) = (&w2c_liveNeighbourCount);
  /* export: 'getCell' */
  WASM_RT_ADD_PREFIX(Z_getCellZ_iii) = (&w2c_getCell);
  /* export: 'setCell' */
  WASM_RT_ADD_PREFIX(Z_setCellZ_viii) = (&w2c_setCell);
  /* export: 'memory' */
  WASM_RT_ADD_PREFIX(Z_memory) = (&w2c_memory);
}

void WASM_RT_ADD_PREFIX(init)(void) {
  init_func_types();
  init_globals();
  init_memory();
  init_table();
  init_exports();
}

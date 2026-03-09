#include <R.h>
#include <Rinternals.h>

#define XXH_STATIC_LINKING_ONLY   /* access advanced declarations */
#define XXH_IMPLEMENTATION        /* access definitions */
#define XXH_NO_STREAM

#include "xxhash.h"
#include "R-xxhash-utils.h"

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Serialize an R object
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP xxhash_raw_(SEXP robj_, SEXP algo_, SEXP as_raw_) {
  
  const char *algo = CHAR(asChar((algo_)));
  
  void *src;
  size_t len;
  char *tmp;
  
  if (TYPEOF(robj_) == RAWSXP) {
    src = (void *)RAW(robj_);
    len = (size_t)length(robj_);
  } else if (TYPEOF(robj_) == STRSXP) {
    if (length(robj_) != 1) {
      error("xxhash_raw_(): Only single string expected");
    }
    tmp = (char *)CHAR(STRING_ELT(robj_, 0));
    src = (void *)tmp;
    len = strlen(tmp);
  } else {
    error("xxhash_raw_(): Only raw vectors and strings are supported");
  }
  
  SEXP res_ = R_NilValue;
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Set up the state
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (strcmp(algo, "xxh128") == 0) {
    XXH128_hash_t const hash = XXH3_128bits(src, len);
    res_ = PROTECT(xxh128_hash_to_robj(hash, as_raw_));
  } else if (strcmp(algo, "xxh3") == 0){
    XXH64_hash_t const hash = XXH3_64bits(src, len);
    res_ = PROTECT(xxh64_hash_to_robj(hash, as_raw_));
  } else if (strcmp(algo, "xxh32") == 0) {
    XXH32_hash_t const hash = XXH32(src, len, 0);
    res_ = PROTECT(xxh32_hash_to_robj(hash, as_raw_));
  } else if (strcmp(algo, "xxh64") == 0) {
    XXH64_hash_t const hash = XXH64(src, len, 0);
    res_ = PROTECT(xxh64_hash_to_robj(hash, as_raw_));
  } else {
    error("xxhash_raw_(): Unknown algo '%s'\n", algo);
  }
  
  UNPROTECT(1);
  return res_;
}

// TODO: Put in a separate file to make it easier to tell apart which code falls under which license
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Hash individual elements of a character vector and return raw result // NOTE: dv-specific
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#include <Rversion.h>
#if R_VERSION >= R_Version(4, 4, 2)
#define GET_STRING_PTR STRING_PTR_RO // assumes no ALTREP
#else
#define GET_STRING_PTR STRING_PTR
#endif

SEXP dv_xxh128_char_input_(SEXP robj_){
  SEXP res_ = R_NilValue;

  if (TYPEOF(robj_) != STRSXP) error("dv_xxh128_char_input_(): Only supported arguments are character vectors");
  if(!XXH_CPU_LITTLE_ENDIAN) error("dv_xxh128_char_input_(): Big endian architectures are not supported"); // FIXME? Move this check to DLL attaching time

  int n_row = 16;
  int n_col = length(robj_);

  res_ = PROTECT(Rf_allocMatrix(RAWSXP, n_row, n_col)); // NOTE: uninitialized memory

  Rbyte *res_raw = RAW(res_);
  xxh_u64 *res_u64 = (xxh_u64 *) res_raw;

  const SEXP* strings = GET_STRING_PTR(robj_);

  for(int i = 0; i < n_col; i += 1){
    const char *s = CHAR(strings[i]);
    int len = Rf_length(strings[i]); // Rf_length returns the byte count of CHARSXP objects. Saves us from `strlen`ing.

    // little endian; we swap unconditionally
    XXH128_hash_t const hash = XXH3_128bits(s, len);
    
    *res_u64 = XXH_swap64(hash.high64);
    res_u64 += 1;
    *res_u64 = XXH_swap64(hash.low64);
    res_u64 += 1;
  }

  UNPROTECT(1);
  return res_;
}

#include <stdint.h>

#if defined(_MSC_VER)     /* Visual Studio */
#  define dv_swap16 _byteswap_ushort
#elif (defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8))) || defined(__clang__)
#  define dv_swap16 __builtin_bswap16
#else
static uint16_t dv_swap16(uint16_t x){
    return (uint16_t)((x << 8) | (x >> 8));
}
#endif

SEXP dv_xxh16_char_input_(SEXP robj_){
  SEXP res_ = R_NilValue;

  if (TYPEOF(robj_) != STRSXP) error("dv_xxh128_char_input_(): Only supported arguments are character vectors");
  if(!XXH_CPU_LITTLE_ENDIAN) error("dv_xxh128_char_input_(): Big endian architectures are not supported"); // FIXME? Move this check to DLL attaching time

  int n_row = 2;
  int n_col = length(robj_);

  res_ = PROTECT(Rf_allocMatrix(RAWSXP, n_row, n_col)); // NOTE: uninitialized memory

  Rbyte *res_raw = RAW(res_);
  uint16_t *res_u16 = (uint16_t *) res_raw;

  const SEXP* strings = GET_STRING_PTR(robj_);

  for(int i = 0; i < n_col; i += 1){
    const char *s = CHAR(strings[i]);
    int len = Rf_length(strings[i]); // Rf_length returns the byte count of CHARSXP objects. Saves us from `strlen`ing.

    // little endian; we swap unconditionally
    XXH32_hash_t const hash = XXH32(s, len, 0);
    *res_u16 = dv_swap16(hash>>16);
    res_u16 += 1;
  }

  UNPROTECT(1);
  return res_;
}

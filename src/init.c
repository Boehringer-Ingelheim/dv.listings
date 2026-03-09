
#include <R.h>
#include <Rinternals.h>

SEXP xxhash_raw_ (SEXP robj_, SEXP algo_, SEXP as_raw_); // Kept for reference
SEXP dv_xxh128_char_input_(SEXP robj_);
SEXP dv_xxh16_char_input_(SEXP robj_);

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// .C      R_CMethodDef
// .Call   R_CallMethodDef
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static const R_CallMethodDef CEntries[] = {
  {"xxhash_raw_" , (DL_FUNC) &xxhash_raw_ , 3}, // Kept for reference
  {"dv_xxh128_char_input_" , (DL_FUNC) &dv_xxh128_char_input_, 1},
  {"dv_xxh16_char_input_" , (DL_FUNC) &dv_xxh16_char_input_, 1},
  {NULL, NULL, 0}
};


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Register the methods
//
// Change the '_simplecall' suffix to match your package name
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void R_init_dv_listings(DllInfo *info) {
  R_registerRoutines(
    info,      // DllInfo
    NULL,      // .C
    CEntries,  // .Call
    NULL,      // Fortran
    NULL       // External
  );
  R_useDynamicSymbols(info, FALSE);
}

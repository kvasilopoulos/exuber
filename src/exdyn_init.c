#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _exdyn_srls_gsadf_cpp(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_exdyn_srls_gsadf_cpp", (DL_FUNC) &_exdyn_srls_gsadf_cpp, 3},
  {NULL, NULL, 0}
};

void R_init_exdyn(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

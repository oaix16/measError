#include <Rcpp.h>
#include <Rcpp/attributes.h>

Rcpp::NumericVector centeredSums(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector sigmaU);

// [[Rcpp::export]]
Rcpp::NumericVector _measError_centeredSums(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector sigmaU) {
    return centeredSums(x, y, sigmaU);
}

static const R_CallMethodDef CallEntries[] = {
    {"_measError_centeredSums", (DL_FUNC) &_measError_centeredSums, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_measError(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

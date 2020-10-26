// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// groupEdges
List groupEdges(const IntegerVector from, const IntegerVector to);
RcppExport SEXP _lshr_groupEdges(SEXP fromSEXP, SEXP toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type from(fromSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type to(toSEXP);
    rcpp_result_gen = Rcpp::wrap(groupEdges(from, to));
    return rcpp_result_gen;
END_RCPP
}
// startProfiler
SEXP startProfiler(SEXP str);
RcppExport SEXP _lshr_startProfiler(SEXP strSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type str(strSEXP);
    rcpp_result_gen = Rcpp::wrap(startProfiler(str));
    return rcpp_result_gen;
END_RCPP
}
// stopProfiler
SEXP stopProfiler();
RcppExport SEXP _lshr_stopProfiler() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(stopProfiler());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_lshr_groupEdges", (DL_FUNC) &_lshr_groupEdges, 2},
    {"_lshr_startProfiler", (DL_FUNC) &_lshr_startProfiler, 1},
    {"_lshr_stopProfiler", (DL_FUNC) &_lshr_stopProfiler, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_lshr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

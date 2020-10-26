#include <Rcpp.h>
#include <gperftools/profiler.h>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP startProfiler(SEXP str) {
  ProfilerStart(as<const char*>(str));
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP stopProfiler() {
  ProfilerStop();
  return R_NilValue;
}


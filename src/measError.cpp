#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::NumericVector centeredSums(Rcpp::NumericVector x, Rcpp::NumericVector y,
                                 Rcpp::NumericVector sigmaU) {
    int n=x.size();
    if(y.size()!=n || sigmaU.size()!=n) Rcpp::stop("length mismatch");
    double mx=Rcpp::mean(x), my=Rcpp::mean(y), sxy=0, sxx=0;
    for(int i=0;i<n;++i){ double dx=x[i]-mx, dy=y[i]-my; sxy += dx*dy; sxx += dx*dx-sigmaU[i]*sigmaU[i]; }
    return Rcpp::NumericVector::create(Rcpp::Named("sxy")=sxy,Rcpp::Named("corrected_sxx")=sxx);
}

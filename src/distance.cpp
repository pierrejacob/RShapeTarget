
#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
double dist_point_to_poly_C_(NumericVector point, NumericMatrix polygon, List ABC) {
  double x = point(0);
  double y = point(1);
  int npoints = polygon.nrow();
  NumericVector xv = polygon(_, 0);
  NumericVector yv = polygon(_, 1);
  NumericVector vv = as<NumericVector>(ABC["A"])*x + as<NumericVector>(ABC["B"])*y+ as<NumericVector>(ABC["C"]);
  NumericVector xp = x - as<NumericVector>(ABC["AAB"])*vv;
  NumericVector yp = y - as<NumericVector>(ABC["BAB"])*vv;
//  # % Test for the case where a polygon rib is 
//  # % either horizontal or vertical. From Eric Schmitz
  LogicalVector id = (as<NumericVector>(ABC["B"]) == 0);
  xp = Rcpp::wrap(ifelse(id == 0, xp, xv));
  id = (as<NumericVector>(ABC["A"]) == 0);
  yp = Rcpp::wrap(ifelse(id == 0, yp, yv));
//  # % find all cases where projected point is inside the segment
  LogicalVector idx_x = ((xp >= head(xv, npoints-1)) & (xp <= tail(xv, npoints - 1))) | ((xp >= tail(xv, npoints - 1)) & (xp <= head(xv, npoints - 1)));
  LogicalVector idx_y = ((yp >= head(yv, npoints-1)) & (yp <= tail(yv, npoints - 1))) | ((yp >= tail(yv, npoints - 1)) & (yp <= head(yv, npoints - 1)));
  LogicalVector idx = idx_x & idx_y;
  NumericVector xvhead = head(xv, npoints-1);
  NumericVector yvhead = head(yv, npoints-1);
  NumericVector dv = sqrt((xvhead - x)*(xvhead - x) + (yvhead-y)*(yvhead-y));
  double d = 0.0;
  if (!(any(idx).is_true())) {
//    #   all projections are outside of polygon ribs
    int ind_min = which_min(dv);
    return dv(ind_min);
  } else {
//    #     % distance from point (x,y) to the projection on ribs
    NumericVector dp_ = sqrt((xp-x)*(xp-x) + (yp-y)*(yp-y));
    NumericVector dp = Rcpp::wrap(ifelse(idx, dp_, 100000000.0));
    int ind_min1 = which_min(dv);
    int ind_min2 = which_min(dp);
    if (dv(ind_min1) < dp(ind_min2)){
      return dv(ind_min1);
    } else {
      return dp(ind_min2);
    }
  }
  return d ;
}

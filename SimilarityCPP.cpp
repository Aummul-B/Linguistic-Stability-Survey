#include <Rcpp.h>
using namespace Rcpp;

//Calculating the Similarity coefficient using C++ 
//without using the matrix storage

// [[Rcpp::export]]
double SimilarityCoeffCPP(Rcpp::NumericVector l1, Rcpp::NumericVector l2){
  
  if(l1.size() != l2.size()){exit(1);}  //making sure both vectors are equal
  
  double correlation = 0; //final result
  
  int mark1, mark2; //variables to check wheather the ith and the jth element 
  //of vector 1 and vector 2 are in the same cluster or not
  
  //variables to keep track of all the  dot products
  double sum1 = 0, sum2 = 0, sum = 0;
  
  //comparing each cluster lable of both vectors with all other vector labels
  for(int x = 0; x<l1.size(); x++){ 
    for(int y = 0; y<l1.size(); y++){
      mark1 = 0;
      mark2 = 0;
      if(x != y  && l1[x] == l1[y]){mark1 = 1;}
      if(x != y  && l2[x] == l2[y]){mark2 = 1;}
      
      //updating the dot products
      sum1 += (mark1*mark1);
//      Rcout << sum1 << " is the sum1. \n";
      sum2 += (mark2*mark2);
//      Rcout << sum2 << " is the sum2. \n";
      sum += (mark1*mark2); 
//      Rcout << sum << " is the sum. \n";
    }
  }
  correlation = sum/sqrt(sum1 * sum2);
  return(correlation);  
}
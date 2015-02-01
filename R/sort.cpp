#include <Rcpp.h>
#include <iostream>     // std::cout
#include <algorithm>    // std::swap
#include <vector>       // std::vector
#include <string>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
Rcpp::DataFrame sortApellidos(Rcpp::DataFrame DF) {
  int len;
  std::string tmp = "";
  
  Rcpp::CharacterVector paterno = DF["paterno"];
  Rcpp::CharacterVector materno = DF["materno"];
  
  len = DF.nrows();
  for(int i=0; i < len; i++) {
    //if(i % 1000 == 0)
      //Rcpp::Rcout << Rcpp::as<std::string>(paterno[i]).compare(Rcpp::as<std::string>(materno[i]));
      //Rcpp::Rcout << tmp << '\n';
    if(Rcpp::as<std::string>(paterno[i]).compare(Rcpp::as<std::string>(materno[i])) > 0) {
       tmp = paterno[i];
       paterno[i] = materno[i];
       materno[i] = tmp;
    }
  }
  // create a new data frame
  Rcpp::DataFrame NDF =
    Rcpp::DataFrame::create(Rcpp::Named("paterno")=paterno,
                            Rcpp::Named("materno")=materno);   
  return NDF;
}

#include<RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;
using namespace arma;
using namespace std;
// [[Rcpp::depends("RcppArmadillo")]]

typedef arma::mat (*funcPtr)(arma::vec MKV);

// [[Rcpp::export]]
arma::mat Tukey1 (arma::vec MKV) {
  arma::vec Weights;
  arma::vec NewWeights;
  Weights = MKV / sum(MKV);
  NewWeights = pow(Weights, -2);
  return conv_to<vec>::from(NewWeights / sum(NewWeights));
}

// [[Rcpp::export]]
arma::mat Tukey2 (arma::vec MKV) {
  arma::vec Weights;
  arma::vec NewWeights;
  Weights = MKV / sum(MKV);
  NewWeights = pow(Weights, -1);
  return conv_to<vec>::from(NewWeights / sum(NewWeights));
}

// [[Rcpp::export]]
arma::mat Tukey3 (arma::vec MKV) {
  arma::vec Weights;
  arma::vec NewWeights;
  Weights = MKV / sum(MKV);
  NewWeights = pow(Weights, -0.5);
  return conv_to<vec>::from(NewWeights / sum(NewWeights));
}

// [[Rcpp::export]]
arma::mat Tukey4 (arma::vec MKV) {
  arma::vec Weights;
  arma::vec NewWeights;
  Weights = MKV / sum(MKV);
  NewWeights = log(Weights);
  return conv_to<vec>::from(NewWeights / sum(NewWeights));
}


// [[Rcpp::export]]
arma::mat Tukey5 (arma::vec MKV) {
  arma::vec Weights;
  arma::vec NewWeights;
  Weights = MKV / sum(MKV);
  NewWeights = pow(Weights, 0);
  return conv_to<vec>::from(NewWeights / sum(NewWeights));
}


// [[Rcpp::export]]
arma::mat Tukey6 (arma::vec MKV) {
  arma::vec Weights;
  arma::vec NewWeights;
  Weights = MKV / sum(MKV);
  NewWeights = pow(Weights, 0.5);
  return conv_to<vec>::from(NewWeights / sum(NewWeights));
}

// [[Rcpp::export]]
arma::mat Tukey7 (arma::vec MKV) {
  arma::vec Weights;
  arma::vec NewWeights;
  Weights = MKV / sum(MKV);
  NewWeights = pow(Weights, 1);
  return conv_to<vec>::from(NewWeights / sum(NewWeights));
}


// [[Rcpp::export]]
arma::mat Tukey8 (arma::vec MKV) {
  arma::vec Weights;
  arma::vec NewWeights;
  Weights = MKV / sum(MKV);
  NewWeights = pow(Weights, 2);
  return conv_to<vec>::from(NewWeights / sum(NewWeights));
}

// [[Rcpp::export]]
double getDollars(int newTime, int oldTime, double newValue, arma::vec CPI) {
  int n = CPI.size();
  if(newTime > n || newTime < -1 || oldTime > n || oldTime < -1) {
    return -1;
  } 
  return newValue * (CPI(newTime) / CPI(oldTime));
}


// [[Rcpp::export]]
XPtr<funcPtr> putFunPtrInXPtr(String fstr) {
  if (fstr == "Tukey1")
    return(XPtr<funcPtr>(new funcPtr(&Tukey1)));
  else if (fstr == "Tukey2")
    return(XPtr<funcPtr>(new funcPtr(&Tukey2)));
  else if (fstr == "Tukey3")
    return(XPtr<funcPtr>(new funcPtr(&Tukey3)));
  else if (fstr == "Tukey4")
    return(XPtr<funcPtr>(new funcPtr(&Tukey4)));
  else if (fstr == "Tukey5")
    return(XPtr<funcPtr>(new funcPtr(&Tukey5)));
  else if (fstr == "Tukey6")
    return(XPtr<funcPtr>(new funcPtr(&Tukey6)));
  else if (fstr == "Tukey7")
    return(XPtr<funcPtr>(new funcPtr(&Tukey7)));
  else if (fstr == "Tukey8")
    return(XPtr<funcPtr>(new funcPtr(&Tukey8)));
  else
    return XPtr<funcPtr>(R_NilValue); // runtime error as NULL no XPtr
}

// [[Rcpp::export]]
arma::mat TukeyViaStr(arma::vec MKV, String funname) {
  XPtr<funcPtr> xpfun = putFunPtrInXPtr(funname);
  funcPtr fun = *xpfun;
  arma::mat NewWeights = fun(MKV);
  return NewWeights;
}

// [[Rcpp::export]]
double sumR(mat vec) {
  double result = 0;
  for (unsigned int i = 0; i < vec.n_elem; i ++) {
    if(is_finite(vec[i])) {
      result = result + vec[i];
    }
  }
  return result;
}

// [[Rcpp::export]]
double countNA(vec vec) {
  double result = 0;
  for (unsigned int i = 0; i < vec.n_elem; i ++) {
    if(!is_finite(vec[i])) {
      result = result + 1;
    }
  }
  return result;
}

// [[Rcpp::export]]
arma::mat covWeights(arma::mat weights, arma::uvec ID, unsigned int N) {
  arma::mat newWeights = zeros<mat>(1,N);
  if(weights.n_elem != ID.n_elem) {
    cout << "dim not equal" <<endl;
    return NULL;
  }
  unsigned int idx;
  for(unsigned int i = 0; i < ID.n_elem; i++) {
    idx = ID(i);
    newWeights(idx) = weights(i);
  }
  return newWeights;
}


// [[Rcpp::export]]
List getReturn(String funname, arma::mat PRCS, arma::mat SHRS, 
                     arma::mat RETS,arma::vec MONTH, arma::vec CPI, int N) {
  arma::uvec nonNA = find_finite(PRCS.row(0));
  arma::uvec nSelect = Rcpp::RcppArmadillo::sample(nonNA, N, TRUE);
  arma::uvec oldID;
  arma::uvec replaceID;
  arma::mat mkv;
  arma::mat prc;
  arma::mat shrs;
  arma::mat rets;
  
  arma::mat prcSS;
  arma::mat shrsSS;
  arma::mat retsSS;
  
  arma::mat bidaskspd;
  arma::mat Weights;
  
  arma::mat newWeights;
  arma::mat oldWeights;
  
  int perADJ = MONTH(0) + 1;
  double dayReturn = 0;
  double bidaskFee = 0;
  double adminFee = 0;
  double bidaskCumu = 0;
  double adminCumu = 0;

  // initialized
  arma::vec CumuReturn = arma::zeros(PRCS.n_rows, 1);
  CumuReturn(0) = pow(10,5);
  prc = PRCS.row(0);
  shrs = SHRS.row(0);
  rets = RETS.row(0);
  prcSS = prc.elem(nSelect);
  shrsSS = shrs.elem(nSelect);
  retsSS = rets.elem(nSelect);
  mkv = prcSS % prcSS;
  Weights = TukeyViaStr(mkv, funname);
  oldID = nSelect;
  oldWeights = covWeights(Weights, oldID, 1701);
  
  for(unsigned int i = 1; i < PRCS.n_rows; i++) {
    rets = RETS.row(i);
    prc = PRCS.row(i);
    shrs = SHRS.row(i);
    prcSS = prc.elem(nSelect);
    shrsSS = shrs.elem(nSelect);
    retsSS = rets.elem(nSelect);
    dayReturn = sumR(retsSS % Weights);

    if(countNA(prcSS) > 0) {
      nonNA = find_finite(prc);
      replaceID = find_nonfinite(prcSS);
      nSelect.elem(replaceID) = Rcpp::RcppArmadillo::sample(nonNA, replaceID.n_elem, TRUE);
      prcSS = prc.elem(nSelect);
      shrsSS = shrs.elem(nSelect);
      mkv = prcSS % shrsSS;
      Weights = TukeyViaStr(mkv, funname);
    }

    if(MONTH(i) == perADJ) {
      perADJ = perADJ + 1;
      if(perADJ == 13) {
        perADJ = 1;
      }
      mkv = prcSS % shrsSS;
      Weights = TukeyViaStr(mkv, funname);
      newWeights = covWeights(Weights, nSelect, 1701);
      bidaskspd = PRCS.row(i) / 2000;
      
      adminFee = - N * getDollars(i, (PRCS.n_rows - 1), 1, CPI);
      bidaskFee = - sumR((abs(CumuReturn(i-1) * (oldWeights - newWeights)) / prc) % (bidaskspd));
      adminCumu = adminCumu + adminFee;
      bidaskCumu = bidaskCumu + bidaskFee;
      CumuReturn(i) = (CumuReturn(i-1) + adminFee + bidaskFee) * (1 + dayReturn);
      oldWeights = newWeights;
    } else {
      CumuReturn(i) = (CumuReturn(i-1)) * (1 + dayReturn);
    }

  }
  return List::create(Named("CumuReturn") = CumuReturn,
                      Named("bidaskCumu") = bidaskCumu,
                      Named("adminCumu") = adminCumu);
}

/*** R

*/





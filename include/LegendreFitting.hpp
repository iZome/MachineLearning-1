#ifndef LEGENDREFITTING
#define LEGENDREFITTING
#include<armadillo>
#include <gsl/gsl_rng.h>

class LegendreFitting{
public:
  LegendreFitting();
  ~LegendreFitting();
  void run();



private:
  gsl_rng * r = gsl_rng_alloc (gsl_rng_taus);
  void setupRandomGenerator();
  void fitHypothesis(int N, double sigma, int order);
  void generatePowerMatrix(arma::vec& x);
  double evaluateBias();
  int rand_identifier;

  arma::mat& generateModelMatrix(double value, int i, int j);
  arma::vec& generateBetas(int size);
  arma::vec& generateX(int N);
  arma::vec& generateY(arma::vec& x, double sigma, int order);
  std::vector<int> v;
  arma::vec sig;
  arma::vec x;
  arma::vec x_non_unif;
  arma::vec y;
  arma::vec betas;
  arma::vec est;

  arma::mat modelMatrix;
  arma::mat result;

};

#endif

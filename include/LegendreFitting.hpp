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
  double fitHypothesis(int N, double sigma, int order);
  int rand_identifier;

  arma::vec& generateBetas(int size);
  void generateX(int N);
  void generateY(double sigma);

  std::vector<int> v;
  arma::vec sig;
  arma::vec x;
  arma::vec x_non_unif;
  arma::vec y;
  arma::vec betas;
  arma::vec est;
  arma::vec newdata;
  arma::vec target;

  arma::mat modelMatrix;
  arma::mat result;

};

#endif

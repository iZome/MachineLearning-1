#ifndef LEGENDREFITTING
#define LEGENDREFITTING
#include<armadillo>
#include <gsl/gsl_vector.h>

#include <gsl/gsl_rng.h>

class LegendreFitting{
public:
  gsl_rng * r;  /* global generator */
  LegendreFitting(int N);
  ~LegendreFitting();



private:
  arma::vec x;
  gsl_vector* generateObservation(int Qf, int N, double sigma);
  arma::vec& generateBetas(int size);
  arma::vec betas;
  arma::vec& generateX(int N);
  arma::vec est;
  //arma::vec y;

  void setupRandomGenerator();
  void fitHypothesis(int N, double sigma, int order);
  double evaluateBias();
  arma::mat result;

  int Qf{10};
  int N{20};
};

#endif

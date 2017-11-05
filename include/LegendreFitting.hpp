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
  arma::vec sig;
  std::vector<int> v;
  void run();



private:
  arma::vec x;
  gsl_vector* generateObservation(int Qf, int N, double sigma);
  arma::vec& generateBetas(int size);
  arma::vec betas;
  arma::vec& generateX(int N);
  arma::vec est;
  int rand_identifier;
  arma::vec l0;
  arma::vec l1;
  arma::vec l2;
  arma::vec l3;
  arma::vec l4;
  arma::vec l5;
  arma::vec l6;
  arma::vec l7;
  arma::vec l8;
  arma::vec l9;
  arma::vec l10;

  arma::vec& p0(arma::vec& x);
  arma::vec& p1(arma::vec& x);
  arma::vec& p2(arma::vec& x);
  arma::vec& p3(arma::vec& x);
  arma::vec& p4(arma::vec& x);
  arma::vec& p5(arma::vec& x);
  arma::vec& p6(arma::vec& x);
  arma::vec& p7(arma::vec& x);
  arma::vec& p8(arma::vec& x);
  arma::vec& p9(arma::vec& x);
  arma::vec& p10(arma::vec& x);

  //arma::vec y;

  void setupRandomGenerator();
  void fitHypothesis(int N, double sigma, int order);
  double evaluateBias();
  arma::mat result;

  int Qf{10};
  int N{20};
};

#endif

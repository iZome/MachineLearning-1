#include "LegendreFitting.hpp"
#include "legendre.h"
#include <armadillo>
#include <stdlib.h>
#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_sf_legendre.h>
#include <gsl/gsl_integration.h>
#include <iomanip>
#include <iostream>
#include <boost/math/special_functions/binomial.hpp>

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

extern "C" double gsl_sf_fact(unsigned int n);

#include <time.h>

using namespace std;

LegendreFitting::LegendreFitting(int N):N(N){
  setupRandomGenerator();

  sig = arma::linspace(0.1, 1.7, 10);
  rand_identifier = rand()%10000000;
  for(int i = 20; i<=120; i++){
    v.push_back(i);
  }

  result.set_size(v.size(), sig.n_elem);
}

void LegendreFitting::run(){
  int a = 50;

  //#pragma omp parallel for
  for( int k = 0; k < a; k++){
    LegendreFitting *lq = new LegendreFitting(2);
    unsigned int id = omp_get_thread_num();

    if(id == 0){
      cout << (4 * k) / ((double) a) << '\r';
      cout.flush();
    }

    for( int i = 0; i < lq->v.size(); i++){
      for( int j = 0; j<lq->sig.n_elem; j++){
        lq->fitHypothesis(lq->v[i], lq->sig(j), 11);
        float err10 = lq->evaluateBias();

        lq->fitHypothesis(lq->v[i], lq->sig(j), 3);
        float err2 = lq->evaluateBias();
        lq->result(i,j) +=  err10 - err2;
      }
    }

    stringstream filename;
    filename << "results/res" << "_" << lq->rand_identifier << ".csv";
    lq->result.save(filename.str().c_str(), arma::csv_ascii);
    filename.str("");

    if(id != 0){delete lq; lq = NULL;}
  }
}


LegendreFitting::~LegendreFitting(){
  gsl_rng_free (r);
}


arma::vec& LegendreFitting::generateX(int N){
  x.set_size(N);
  for( int i = 0; i < N; i++ ){
    x(i) =  gsl_ran_flat(r, -1, 1 );
  }
  return x;
}

gsl_vector* LegendreFitting::generateObservation(int Qf, int N, double sigma){
  betas = generateBetas(Qf + 1);
  x = generateX(N);
  gsl_vector *y = gsl_vector_alloc(x.n_elem);
  gsl_vector_set_zero(y);

  double noise = 0;


  for( int i = 0; i <= Qf; i++){
    for( int j = 0; j < x.n_elem; j++){
      if(i == Qf){noise = gsl_ran_gaussian(r, pow(sigma,2)); }
      gsl_vector_set(y, j, gsl_vector_get(y,j) + betas(i)*Legendre::Pn(i, x(j)) + noise);

    }
  }
  return y;
}


void LegendreFitting::fitHypothesis(int N, double sigma, int order){
  const size_t n = N;
  const size_t p = order;

  gsl_multifit_linear_workspace * w = gsl_multifit_linear_alloc (N, p);
  gsl_matrix *X = gsl_matrix_alloc(n, p);

  gsl_vector *c = gsl_vector_alloc(p);
  gsl_matrix *cov = gsl_matrix_alloc(p,p);
  //gsl_vector *y = generateObservation(10, N, sigma);

  x = generateX(N);
  arma::vec y1 = p0(x) + p1(x) + p2(x);// + p3(x) + p4(x) + p5(x) + p6(x) + p7(x) + p8(x) + p9(x) + p10(x)

  //gsl_vector *y = y1.memptr();
  gsl_vector *y = reinterpret_cast<gsl_vector*>(y1.memptr());


  x = arma::linspace(-1,1, N);
  double chisq;

  for( int i = 0; i < n; i++ ){
    for( int j = 0; j < p; j++){
      double element = Legendre::Pn(i, x(j));
      gsl_matrix_set(X, i, j, element);
    }
  }
  int lin = gsl_multifit_linear(X, y, c, cov, &chisq, w);

  #define C(i) (gsl_vector_get(c,(i)))

  est.set_size(order);
  for(int i = 0; i < order; i++){
    est(i) = C(i);
  }
  gsl_multifit_linear_free(w);
  gsl_matrix_free (X);
  gsl_vector_free (y);
  gsl_vector_free (c);
  gsl_matrix_free (cov);
}

arma::vec& LegendreFitting::generateBetas(int size){
  betas.set_size(size);
  for( int i = 0; i < size; i++ ){
    betas(i) = gsl_ran_flat(r, -1, 1);
  }
  return betas;
}


void LegendreFitting::setupRandomGenerator(){
    const gsl_rng_type * T;

    gsl_rng_env_setup();

    T = gsl_rng_default;
    r = gsl_rng_alloc (T);
}


double f (double x, void * params) {
  arma::vec difference = *(arma::vec *)params;
  double result = difference(0);
  for( int i = 1; i < difference.n_elem; i++ ){
    result += difference(i)*pow(x,i);
  }

  return pow(result,2);
}

double LegendreFitting::evaluateBias(){

  gsl_integration_workspace * w
    = gsl_integration_workspace_alloc (1000);

  double result, error;

  if( est.n_elem != betas.n_elem ) { est.reshape(betas.n_elem, 1); }
  arma::vec difference = est - betas;

  gsl_function F;
  F.function = &f;
  F.params = &difference;

  gsl_integration_qags (&F, -1, 1, 0, 1e-2, 1000,
                        w, &result, &error);

  gsl_integration_workspace_free (w);
  return result;
}

arma::vec& LegendreFitting::p0(arma::vec& x){
  l0 = arma::ones(x.n_elem);
  return l0;
}

arma::vec& LegendreFitting::p1(arma::vec& x){
  l1 = x;
  return l1;
}

arma::vec& LegendreFitting::p2(arma::vec& x){
  l2 = 0.5*(3 * arma::pow(x,2) - 1);
  return l2;
}

arma::vec& LegendreFitting::p3(arma::vec& x){
  l3 = 0.5*(5 * arma::pow(x, 3) - 3 * x);
  return l3;
}

arma::vec& LegendreFitting::p4(arma::vec& x){
  l4 = (1/8.0) * (35*arma::pow(x,4) - 30*arma::pow(x,2) + 3);
  return l4;
}

arma::vec& LegendreFitting::p5(arma::vec& x){
  l5 = (1/8.0) * (63*arma::pow(x,5) - 70*arma::pow(x,3) + 15 * x);
  return l5;
}

arma::vec& LegendreFitting::p6(arma::vec& x){
  l6 = (1/16.0) * (231 * arma::pow(x,6) - 315 * arma::pow(x,4) + 105*arma::pow(x,2) - 5);
  return l6;
}

arma::vec& LegendreFitting::p7(arma::vec& x){
  l7 = (1/16.0) * (429 * arma::pow(x,7) - 693 * arma::pow(x,5) + 315*arma::pow(x,3) - 35*x);
  return l7;
}

arma::vec& LegendreFitting::p8(arma::vec& x){
  l8 = (1/128.0) * (6435 * arma::pow(x,8) - 12012*arma::pow(x,6) + 6930*arma::pow(x,4) - 1260 * arma::pow(x,2) + 35);
  return l8;
}

arma::vec& LegendreFitting::p9(arma::vec& x){
  l9 = (1/128.0) * (12155 * arma::pow(x,9) - 25740*arma::pow(x,7) + 18018*arma::pow(x,5) - 4620 * arma::pow(x,3) + 315 * x);
  return l9;
}

arma::vec& LegendreFitting::p10(arma::vec& x){
  l10 = (1/256.0) * (46189 * arma::pow(x,10) - 109395*arma::pow(x,8) + 90090*arma::pow(x,6) - 30030 * arma::pow(x,4) + 3465 * arma::pow(x,2) - 63);
  return l10;
}

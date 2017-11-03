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

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

extern "C" double gsl_sf_fact(unsigned int n);

#include <time.h>

using namespace std;

LegendreFitting::LegendreFitting(int N):N(N){
  setupRandomGenerator();

  std::vector<int> v(120);
  std::iota(v.begin(), v.end(), 20);
  arma::vec sig = arma::linspace(0.1, 2.5, 1000);
  int a = 10;

  result.set_size(v.size(), sig.n_elem);

  for( int k = 0; k < a; k++){
    cout << k << endl;
    for( int i = 0; i < v.size(); i++){
      for( int j = 0; j<sig.n_elem; j++){
        fitHypothesis(v[i], sig(j), 11);
        double err10 = evaluateBias();

        fitHypothesis(v[i], sig(j), 3);
        double err2 = evaluateBias();

        result(i,j) += err10 - err2;
      }
    }
  }

  result /= a;
  result.save("res.csv", arma::csv_ascii);

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
  gsl_vector *y = generateObservation(10, N, sigma);

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

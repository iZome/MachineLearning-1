#include "LegendreFitting.hpp"
#include <armadillo>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

using namespace std;

LegendreFitting::LegendreFitting(){
  setupRandomGenerator();
  sig = arma::linspace(0.1, 1.7, 10000);
  rand_identifier = rand()%10000000;
  for(int i = 20; i<=120; i++){
    v.push_back(i);
  }
  result.set_size(v.size(), sig.n_elem);
}

void LegendreFitting::run(){
  int a = 1000;

  #pragma omp parallel for
  for( int k = 0; k < a; k++){
    LegendreFitting *lq = new LegendreFitting();
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

arma::mat& LegendreFitting::generateModelMatrix(arma::vec& x, int Qf){
  X.set_size(x.n_elem, Qf);
  X.col(0) = l0; X.col(1) = l1; X.col(2) = l2;
  if( Qf > 5 ){
    X.col(3) = l3; X.col(4) = l4; X.col(5) = l5; X.col(6) = l6;
    X.col(7) = l7; X.col(8) = l8; X.col(9) = l9; X.col(10) = l10;
  }
  return X;
}

arma::vec& LegendreFitting::generateY(arma::vec& x, double sigma){
  betas = generateBetas(10 + 1);
  y.set_size(x.n_elem);
  generatePowerMatrix(x);

  y = betas(0) * p0(x) + betas(1) * p1(x) + betas(2) * p2(x) + betas(3) * p3(x) + betas(4) * p4(x) +
      betas(5) * p5(x) + betas(6) * p6(x) + betas(7) * p7(x) + betas(8) * p8(x) + betas(9) * p9(x) + betas(10) * p10(x);
  for( int i = 0; i < x.n_elem; i++ ){
    y(i) += gsl_ran_gaussian (r, pow(sigma,2));
  }
  return y;
}

void LegendreFitting::generatePowerMatrix(arma::vec& x){
  powerMatrix.set_size(x.n_elem, 11);
  for(int i = 0; i < 11; i++){
    powerMatrix.col(i) = arma::pow(x, i);
  }
}


void LegendreFitting::fitHypothesis(int N, double sigma, int order){
  const size_t n = N;
  const size_t p = order;

  gsl_multifit_linear_workspace * w = gsl_multifit_linear_alloc (n, p);

  gsl_vector *c = gsl_vector_alloc(p);
  gsl_matrix *cov = gsl_matrix_alloc(p,p);

  x = sort(generateX(n));
  gsl_vector *y = gsl_vector_alloc(x.n_elem);
  y->data = generateY(x, sigma).memptr();

  x = arma::linspace(-1, 1, n);
  gsl_matrix *X = gsl_matrix_alloc(n,p);
  X->data = generateModelMatrix(x, p).memptr();

  double chisq;

  int lin = gsl_multifit_linear(X, y, c, cov, &chisq, w);

  #define C(i) (gsl_vector_get(c,(i)))

  est.set_size(order);
  for(int i = 0; i < order; i++){
    est(i) = C(i);
  }
  gsl_multifit_linear_free(w); gsl_matrix_free (X);
  gsl_vector_free (y); gsl_vector_free (c);
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

  gsl_integration_qags (&F, -1, 1, 0, 1e-4, 1000,
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
  l2 = 0.5*(3 * powerMatrix.col(2) - 1);
  return l2;
}

arma::vec& LegendreFitting::p3(arma::vec& x){
  l3 = 0.5*(5 * powerMatrix.col(3) - 3 * x);
  return l3;
}

arma::vec& LegendreFitting::p4(arma::vec& x){
  l4 = (1/8.0) * (35*powerMatrix.col(4) - 30*powerMatrix.col(2) + 3);
  return l4;
}

arma::vec& LegendreFitting::p5(arma::vec& x){
  l5 = (1/8.0) * (63*powerMatrix.col(5) - 70*arma::pow(x,3) + 15 * x);
  return l5;
}

arma::vec& LegendreFitting::p6(arma::vec& x){
  l6 = (1/16.0) * (231 * powerMatrix.col(6) - 315 * powerMatrix.col(4) + 105*powerMatrix.col(2) - 5);
  return l6;
}

arma::vec& LegendreFitting::p7(arma::vec& x){
  l7 = (1/16.0) * (429 * powerMatrix.col(7) - 693 * powerMatrix.col(5) + 315*arma::pow(x,3) - 35*x);
  return l7;
}

arma::vec& LegendreFitting::p8(arma::vec& x){
  l8 = (1/128.0) * (6435 * powerMatrix.col(8) - 12012*powerMatrix.col(6) + 6930*powerMatrix.col(4) - 1260 * powerMatrix.col(2) + 35);
  return l8;
}

arma::vec& LegendreFitting::p9(arma::vec& x){
  l9 = (1/128.0) * (12155 * powerMatrix.col(9) - 25740*powerMatrix.col(7) + 18018*powerMatrix.col(5) - 4620 * arma::pow(x,3) + 315 * x);
  return l9;
}

arma::vec& LegendreFitting::p10(arma::vec& x){
  l10 = (1/256.0) * (46189 * powerMatrix.col(10) - 109395*powerMatrix.col(8) + 90090*powerMatrix.col(6) - 30030 * powerMatrix.col(4) + 3465 * powerMatrix.col(2) - 63);
  return l10;
}

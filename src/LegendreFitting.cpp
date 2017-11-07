#include "LegendreFitting.hpp"
#include <Legendre.h>
#include <armadillo>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_sf_legendre.h>

using namespace std;

LegendreFitting::LegendreFitting(){
  sig = arma::linspace(0.1, 1.4, 1000);
  rand_identifier = rand()%10000000;
  for(int i = 20; i<=120; i++){
    v.push_back(i);
  }

  result.set_size(v.size(), sig.n_elem);
}

void LegendreFitting::run(){
  int a = 10;

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
  //gsl_rng_free (r);
}


arma::vec& LegendreFitting::generateX(int N){
  gsl_rng * r = gsl_rng_alloc (gsl_rng_taus);
  x.set_size(N);
  for( int i = 0; i < N; i++ ){
    gsl_rng_set (r, rand()%100000000);
    x(i) =  gsl_ran_flat(r, -1, 1 );
  }
  gsl_rng_free (r);
  return x;
}


arma::vec& LegendreFitting::generateY(arma::vec& x, double sigma, int order){
  int Qf = 11;
  betas = generateBetas(Qf);

  y.set_size(x.n_elem);
  y.fill(0);
  modelMatrix.set_size(x.n_elem, order);

  gsl_rng * r = gsl_rng_alloc (gsl_rng_taus);

  double noise = 0;
  for(int q = 0; q < Qf; q++ ){
    for( int i = 0; i < x.n_elem; i++ ){
      gsl_rng_set (r, rand()%100000000);
      if(q == (Qf - 1)){ noise = gsl_ran_gaussian(r, pow(sigma, 2)); }
      y(i) += betas(q) * Legendre::Pn (q, x(i)) + noise;
      if( q < (order) ){ modelMatrix(i, q) = Legendre::Pn(q, x_non_unif(i)); }
    }
  }
  gsl_rng_free (r);
  return y;
}




void LegendreFitting::fitHypothesis(int N, double sigma, int order){
  const size_t n = N;
  const size_t p = order;

  gsl_multifit_linear_workspace * w = gsl_multifit_linear_alloc (n, p);

  gsl_vector *c = gsl_vector_alloc(p);
  gsl_matrix *cov = gsl_matrix_alloc(p,p);

  x = generateX(n);
  x_non_unif = arma::linspace(-0.9999, 0.9999, n);
  gsl_vector *y = gsl_vector_alloc(x.n_elem);
  y->data = generateY(x, sigma, p).memptr();

  gsl_matrix *X = gsl_matrix_alloc(n,p);
  X->data = modelMatrix.memptr();

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
  gsl_rng * r = gsl_rng_alloc (gsl_rng_taus);
  betas.set_size(size);
  for( int i = 0; i < size; i++ ){
    gsl_rng_set (r, rand()%100000000);
    betas(i) = gsl_ran_flat(r, -1, 1);
  }
  gsl_rng_free (r);
  return betas;
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

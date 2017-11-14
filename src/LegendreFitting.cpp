#include "LegendreFitting.hpp"
#include <Legendre.h>
#include <armadillo>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_poly.h>

using namespace std;

LegendreFitting::LegendreFitting(){
  sig = arma::linspace(0, 1.5, 500);
  rand_identifier = rand()%10000000;
  for(int i = 20; i<120; i++){
    v.push_back(i);
  }

  //fitHypothesis(20, 0.5, 11);
  result.set_size(v.size(), sig.n_elem);
}

void LegendreFitting::run(){
  int a = 3000;
  double q = 0;

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
        double err10 = lq->fitHypothesis(lq->v[i], lq->sig(j), 11);
        double err2 = lq->fitHypothesis(lq->v[i], lq->sig(j), 3);
        lq->result(i,j) = err10 - err2;
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
  gsl_rng_set(r, rand());
  for( int i = 0; i < N; i++ ){
    x(i) =  gsl_ran_flat(r, -1, 1 );
  }
  return x;
}


arma::vec& LegendreFitting::generateY(arma::vec& x, double sigma, int order){
  int Qf = 11;
  betas = generateBetas(Qf);

  y.set_size(x.n_elem);
  y.fill(0);
  modelMatrix.set_size(x.n_elem, order);
  gsl_rng_set(r, rand());

  target.set_size(x.n_elem);
  target.fill(0);

  double noise = 0;
  for(int q = 0; q < Qf; q++ ){
    for( int i = 0; i < x.n_elem; i++ ){
      if(q == (Qf - 1)){ noise = gsl_ran_gaussian(r, pow(sigma, 2)); }
      double legendre = Legendre::Pn (q, x(i));
      y(i) += betas(q) * legendre + noise;
      target(i) += betas(q) * legendre;
      if( q < (order) ){ modelMatrix(i, q) = Legendre::Pn(q, x_non_unif(i)); }
    }
  }
  /*
  target.save("target.csv", arma::csv_ascii);
  x.save("x.csv", arma::csv_ascii);
  y.save("y.csv", arma::csv_ascii);*/
  return y;
}




double LegendreFitting::fitHypothesis(int N, double sigma, int order){
  const size_t n = N;
  const size_t p = order;

  gsl_multifit_linear_workspace * w = gsl_multifit_linear_alloc (n, p);

  gsl_vector *c = gsl_vector_alloc(p);
  gsl_matrix *cov = gsl_matrix_alloc(p,p);

  x = sort(generateX(n));
  x_non_unif = arma::linspace(-1, 1, n);
  gsl_vector *y = gsl_vector_alloc(x.n_elem);
  y->data = generateY(x, sigma, p).memptr();

  gsl_matrix *X = gsl_matrix_alloc(n,p);
  modelMatrix = modelMatrix.t();
  X->data = modelMatrix.memptr();

  double chisq;
  int lin = gsl_multifit_linear(X, y, c, cov, &chisq, w);
  #define C(i) (gsl_vector_get(c,(i)))

  est.set_size(p);
  for(int i = 0; i < order; i++){
    est(i) = C(i);
  }

  arma::vec pred(x.n_elem);
  pred.fill(0);

  for( int i = 0; i < est.n_elem; i++ ){
    for( int v = 0; v < x.n_elem; v++ ){
      pred(v) += est(i) * Legendre::Pn(i, x(v));
    }
  }

  //pred.save("pred.csv", arma::csv_ascii);

  gsl_multifit_linear_free(w); gsl_matrix_free (X);
  gsl_vector_free (y); gsl_vector_free (c);
  gsl_matrix_free (cov);

  return arma::mean(arma::pow(pred - target, 2));
}

arma::vec& LegendreFitting::generateBetas(int size){
  betas.set_size(size);
  gsl_rng_set(r, rand());
  for( int i = 0; i < size; i++ ){
    betas(i) = gsl_ran_flat(r, -1, 1);
  }
  return betas;
}

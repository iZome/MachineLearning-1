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
  sig = arma::linspace(0.2, 1.5, 1000);
  rand_identifier = rand()%10000000;
  for(int i = 20; i<120; i++){
    v.push_back(i);
  }
  result.set_size(v.size(), sig.n_elem);

  double err10 = 0;
  double err2 = 0;
  double t = 1;

  //err10 += fitHypothesis(40, 0.2, 2);
  //err2  += fitHypothesis(40, 0.2, 2);

  //cout << "Err 10: " << err10/t << endl;
  //cout << "Err 2:  " << err2/t << endl;
}


void LegendreFitting::run(){
  int a = 10000;
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
        double err10 = lq->fitHypothesis(lq->v[i], lq->sig(j), 10);
        double err2 = lq->fitHypothesis(lq->v[i], lq->sig(j), 2);

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


void LegendreFitting::generateX(int N){
  x.fill(0);
  x.set_size(N);
  gsl_rng_set(r, rand());
  for( int i = 0; i < N; i++ ){
    x(i) =  gsl_ran_flat(r, -1, 1 );
  }
}


void LegendreFitting::generateY(double sigma){
  int Qf = 11;
  betas = generateBetas(Qf);

  y.set_size(x.n_elem);
  y.fill(0);
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
    }
  }

  //target.save("target.csv", arma::csv_ascii);
  //x.save("x.csv", arma::csv_ascii);
  //y.save("y.csv", arma::csv_ascii);
}


double LegendreFitting::fitHypothesis(int N, double sigma, int order){
  generateX(N);
  x = sort(x);
  generateY(sigma);

  arma::vec coefficients = arma::polyfit(x,y,order);

  generateX(80);
  x = sort(x);
  generateY(sigma);

  arma::vec predict = arma::polyval(coefficients, x);
  return arma::mean(arma::pow(predict - target, 2));
}

arma::vec& LegendreFitting::generateBetas(int size){
  betas.set_size(size);
  gsl_rng_set(r, 10);
  for( int i = 0; i < size; i++ ){
    betas(i) = gsl_ran_flat(r, -1, 1);
  }
  return betas;
}

//
// This Stan program defines a bayesian kernel machine regression, with a
// vector of values 'y' and 'n' modeled as binomially distributed
// with  'mu'=inv-logit(eta = X*beta + h ) and h ~ N(0,Sigma).
//

data {
	int<lower=0> N;
	int<lower=1> p;
	int<lower=1> m;
	int<lower=0,upper=1> est_phi;
	real<lower = 0> phi_a;
	real<lower = 0> phi_b;
	real<lower = 0> sigma_a;
	real<lower = 0> sigma_b;
	int<lower=0> y[N];
	int<lower=1> n[N];
	matrix[N,p] Q;
	matrix[p,p] R_inv;
	matrix[N,m] M;
	matrix[m,m] P;
}
parameters {
	vector[p] beta_;
	vector[m] h;
	real<lower=0> phi[est_phi];
	real<lower=0> sigma;
}

transformed parameters{
	vector[p] beta = R_inv * beta_;
}
model {

  if(est_phi==1){
    h ~ multi_normal(rep_vector(0,m), pow(sigma,2) * exp( - P / pow(phi[est_phi],2) ) );
    phi ~ normal(phi_a,phi_b);
  }
	 else
	   h ~ multi_normal(rep_vector(0,m),pow(sigma,2) * exp( - P ) );


	y ~ binomial_logit(n,Q*beta_ + M*h);

  sigma ~ normal(sigma_a,sigma_b);
}
generated quantities{
	vector[N] eta_hat = inv_logit(Q*beta_ + M*h);
}


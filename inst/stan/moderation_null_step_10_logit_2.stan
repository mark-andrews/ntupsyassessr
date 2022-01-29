// this is the one

data {
  int N;
  int K;
  int J;
  int<lower=1, upper=K> y[N];
  int<lower=1, upper=J> x[N]; // identity 
}

parameters {  
  real<lower=0> scale[J];
  real threshold;
  vector<lower=0>[K-2] offset;
  real eta[J];
  real offset_mu;
  real<lower=0> offset_sigma;
  real<lower=0> eta_sigma;  
  real<lower=0> scale_sigma;  
}

transformed parameters {
  
  ordered[K-1] cuts;
  vector[K] theta[J];

  cuts[1] = threshold;
  for (k in 2:(K-1)) {
    cuts[k] = threshold + sum(offset[1:(k-1)]);
  }
  
  for (j in 1:J){
    theta[j, 1] = 1 - inv_logit((eta[j] - cuts[1])/scale[j]);
    for (k in 2:(K-1)){
      theta[j, k] = inv_logit((eta[j] - cuts[k-1])/scale[j]) - inv_logit((eta[j] - cuts[k])/scale[j]);
    }
    theta[j,K] = inv_logit((eta[j] - cuts[K-1])/scale[j]) - 0;
  }
  
}

model {
  threshold ~ normal(-4, 0.5);
  offset ~ normal(offset_mu, offset_sigma);
  offset_mu ~ std_normal();
  offset_sigma ~ lognormal(0, 0.1);

  eta ~ normal(0, eta_sigma);
  eta_sigma ~ student_t(10, 0, 1.0);
  
  scale ~ normal(1, scale_sigma);
  scale_sigma ~ student_t(10, 0, 1.0);

  for (i in 1:N){
    y[i] ~ categorical(theta[x[i],1:K]);
  }
  
}

generated quantities {
  
  int<lower=1, upper=K> y_ppc[J];
  for (j in 1:J){
    y_ppc[j] = categorical_rng(theta[j, 1:K]);
  }
  
}

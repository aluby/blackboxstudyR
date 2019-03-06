data {
  int<lower=1> I;               // # items
  int<lower=1> J;               // # persons
  int<lower=1> N;               // # responses
  int<lower=1, upper=I> ii[N];  // item index
  int<lower=1, upper=J> jj[N];  // person index
  int<lower=0, upper=1> y[N];   // response for n; y = 0 (incorrect), 1 (correct)
}
parameters {
  vector[I] b;              // difficulty for item i
  vector[J] theta;          // proficiency for person j
  real mu_b;                 // mean of difficulties
  real<lower=0> sigmaB;     // variance of difficulties
  real<lower=0> sigmaT;     // variance of proficiencies
}
model {
  vector[N] eta;
  b ~ normal(mu_b, sigmaB);
  theta ~ normal(0, sigmaT);
  y ~ bernoulli_logit(theta[jj] - b[ii]);
  mu_b ~ normal(0,10);
  sigmaT ~ cauchy(0, 2.5);
  sigmaB ~ cauchy(0, 2.5);
}

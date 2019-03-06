functions {
  real pcm(int y, real theta, vector beta) {
    vector[rows(beta) + 1] unsummed;
    vector[rows(beta) + 1] probs;
    unsummed = append_row(rep_vector(0.0, 1), theta - beta);
    probs = softmax(cumulative_sum(unsummed));
    return categorical_lpmf(y + 1 | probs);
  }
}
data {
  int<lower=1> I;               // # items
  int<lower=1> J;               // # persons
  int<lower=1> N;               // # responses
  int<lower=1,upper=I> ii[N];   // item index
  int<lower=1,upper=J> jj[N];   // person index
  int<lower=0> y[N];            // response for n; y = 0 (incorrect), 1 (partial), 2 (correct)
}
parameters {
  vector[2] beta[I];
  vector[J] theta;
  real<lower=0> sigmaB;
  real<lower=0> sigmaT;
  real muB;
}
model {
  for(i in 1:I)
    beta[i] ~ normal(muB, sigmaB);
  theta ~ normal(0, sigmaT);
  muB ~ normal(0,10);
  sigmaT ~ cauchy(0, 2.5);
  sigmaB ~ cauchy(0, 2.5);
  for (n in 1:N)
    target += pcm(y[n], theta[jj[n]], beta[ii[n]]);
} 

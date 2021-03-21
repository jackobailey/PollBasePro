// PollBasePro Stan Model
data {
  // Data counts
  int N; // Number of polls
  int T; // Number of days
  int P; // Number of pollsters

  // Data
  vector<lower = 0, upper = 1>[N] y; // Vector of vote intention polls
  vector<lower = 0>[N] s; // Vector of polling errors
  int<lower = 1> index[N]; // Index of days since election
  int<lower = 1> pollster[N]; // Vector of pollster indices
  real<lower = 0, upper = 1> alpha_init; // Vote share at initial election
}
parameters {
  vector[T - 1] omega; // Initalise random shock parameters
  real<lower = 0> tau; // Initialise scale of innovation parameter
  vector[P] delta; // Initialise house effect parameters
  real<lower = 0> sigma; // Initialise residual error parameter
}
transformed parameters {
  vector[T] alpha; // Initalise latent voting intention parameters
  vector[N] mu; // Initalise expectated outcome parameter
  alpha[1] = alpha_init; // Constrain initial alpha to equal election result

  // Specify dynamic model
  for (i in 2:T) {
    alpha[i] = alpha[i - 1] + tau * omega[i - 1];
  }

  // Specify measurement model
  for (i in 1:N) {
    mu[i] = alpha[index[i]] + delta[pollster[i]];
  }
}
model {
  // Priors
  target += normal_lpdf(delta | 0, 0.05); // Prior on pollster effects, delta
  target += normal_lpdf(omega | 0, 0.025); // Prior on random shocks, omega
  target += normal_lpdf(tau | 0, 0.05); // Prior on scale of innovations, tau
  target += normal_lpdf(alpha[T] | alpha[T - 1], tau); // Prior on final alpha
  target += exponential_lpdf(sigma | 20); // Prior on residual error

  // Likelihood
  target += normal_lpdf(y | mu, sqrt(square(sigma) + square(s))); // Normal likelihood function
}

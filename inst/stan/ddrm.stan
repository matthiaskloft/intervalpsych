////////////////////////////////////////////////////////////////////////////////
// Dirichlet Dual Response Model (DDRM)
////////////////////////////////////////////////////////////////////////////////
data{
  // RS2
  int<lower=1> I; // number of persons
  int<lower=1> J; // number of items
  int<lower=1> K; // number of nodes
  int<lower=1> N; // number of observed responses
  array[N] int<lower=1> ii; // person indices
  array[N] int<lower=1> jj; // item indices
  array[N] int<lower=1> nn; // response indices
  array[N] simplex[3] Y; // RS2 responses
}
////////////////////////////////////////////////////////////////////////////////
parameters{
  // person parameters
  vector[I] theta;
  vector[I] eta;
  // scaling parameter
  vector<lower=0>[2] alpha;

  // item parameters
  // raws for non-centered parameterization
  matrix[J,2] J_raw;
  vector<lower=0>[J] tau_raw;
  // hyperpriors means
  vector[2] mu_J;
  real<lower=0> mu_tau;
  // hyperpriors SDs
  vector<lower=0>[3] sigma_J;
}
////////////////////////////////////////////////////////////////////////////////
transformed parameters{
  // Reparameterization of item parameters
  vector[J] delta        = J_raw[,1] * sigma_J[1] + mu_J[1];
  vector[J] gamma        = J_raw[,2] * sigma_J[2] + mu_J[2];
  vector<lower=0>[J] tau = tau_raw   * sigma_J[3] + mu_tau;
}
////////////////////////////////////////////////////////////////////////////////
model{
// person patameters
  theta  ~ std_normal();
  eta    ~ std_normal();
// scaling parameters
  alpha  ~ student_t(3,0,1);
// item parameter priors
  // raws for non-centered parameterization
  to_vector(J_raw) ~ std_normal();
  tau_raw          ~ std_normal();
  // RS2
  mu_J      ~ student_t(3,0,2);
  mu_tau    ~ student_t(3,0,1);
  sigma_J   ~ student_t(3,0,1);

  // Model
  {
  array[N] vector[3] pars_dir; // parameters for Dirichlet
    for(n in 1:N){
    pars_dir[n,1] = exp( alpha[1] * (theta[ii[n]] - delta[jj[n]])   + tau[jj[n]]);
    pars_dir[n,2] = exp( alpha[2] * (eta[ii[n]] + gamma[jj[n]]) + tau[jj[n]]);
    pars_dir[n,3] = exp(-alpha[1] * (theta[ii[n]] - delta[jj[n]])   + tau[jj[n]]);
  }
    // Likelihood
    Y[nn] ~ dirichlet(pars_dir[nn]);
  }
}
////////////////////////////////////////////////////////////////////////////////
generated quantities{
  // person parameter correlations
  // log-likelihoods
  vector[N] log_lik;
  // replicated responses
  array[N] vector[3] Y_rep;   // posterior predicted responses
  array[N] vector[3] Y_e_rep; // posterior predicted expected responses

// DDRM
 {
  for(n in 1:N){
    vector[3] pars_dir; // parameters for Dirichlet
    pars_dir[1] = exp( alpha[1] * (theta[ii[n]] - delta[jj[n]])   + tau[jj[n]]);
    pars_dir[2] = exp( alpha[2] * (eta[ii[n]] + gamma[jj[n]]) + tau[jj[n]]);
    pars_dir[3] = exp(-alpha[1] * (theta[ii[n]] - delta[jj[n]])   + tau[jj[n]]);
    // loglik
    log_lik[n] = dirichlet_lpdf(Y[n] | pars_dir);

    // replicated responses for PPC
    Y_rep[n] = dirichlet_rng(pars_dir);
    Y_e_rep[n,1] = pars_dir[1] / sum(pars_dir);
    Y_e_rep[n,2] = pars_dir[2] / sum(pars_dir);
    Y_e_rep[n,3] = pars_dir[3] / sum(pars_dir);
  }
 }
}


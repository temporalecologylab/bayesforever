functions {
  vector gamma_tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {
    vector[2] deltas;

    if (!is_nan(y[1]) && !is_nan(y[2])) {
      deltas[1] = gamma_cdf(theta[1], exp(y[1]), exp(y[2])) - 0.01;
      deltas[2] = gamma_cdf(theta[2], exp(y[1]), exp(y[2])) - 0.99;
    } else {
      deltas[1] = 0;
      deltas[2] = 0;
    }
    return deltas;
  }

  vector inv_gamma_tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {
    vector[2] deltas;

    if (!is_nan(y[1]) && !is_nan(y[2])) {
      deltas[1] = inv_gamma_cdf(theta[1], exp(y[1]), exp(y[2])) - 0.01;
      deltas[2] = inv_gamma_cdf(theta[2], exp(y[1]), exp(y[2])) - 0.99;
    } else {
      deltas[1] = 0;
      deltas[2] = 0;
    }
    return deltas;
  }
}

transformed data {
  real l = 0.05;
  real u = 0.5;
  real delta = 3;

  real alpha_inv_gamma = square(delta * (u + l) / (u - l)) + 2;
  real beta_inv_gamma =  ((u + l) / 2) * ( square(delta * (u + l) / (u - l)) + 1);
  vector[2] y_inv_gamma_guess = [log(alpha_inv_gamma), log(beta_inv_gamma)]';

  real alpha_gamma = square(delta * (u + l) / (u - l));
  real beta_gamma =  2 * square(delta) * (u + l) / square(u - l);
  vector[2] y_gamma_guess = [log(alpha_gamma), log(beta_gamma)]';

  vector[2] theta = [l, u]';
  vector[2] y;
  real x_r[0];
  int x_i[0];

  y = algebra_solver(gamma_tail_delta, y_gamma_guess, theta, x_r, x_i);
  print("a_gamma = ", exp(y[1]));
  print("b_gamma = ", exp(y[2]));
  print("");

  y = algebra_solver(inv_gamma_tail_delta, y_inv_gamma_guess, theta, x_r, x_i);
  print("a_inv_gamma = ", exp(y[1]));
  print("b_inv_gamma = ", exp(y[2]));
}

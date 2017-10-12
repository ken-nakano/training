data {
  int N;                        // サンプル数
  int N_pot;                    // 植木鉢の数
  int PID[N];                   // 各サンプルの植木鉢
  int f[N];                     // 施肥処理
  int y[N];                     // 種子数
}

parameters {
  real b1;
  real b2;
  real<lower=0> s_p;
  real<lower=0> s_r;
  real r[N];
  real pot[N_pot];
}

transformed parameters {
  real<lower=0> lambda[N];
  for (n in 1:N) {
    lambda[n] = exp(b1 + b2*f[n] + pot[PID[n]] + r[n]); // 種子数の平均lambdaを表現
  }
}

model {
  for (n_pot in 1:N_pot) {
    pot[n_pot] ~ normal(0, s_p); // 階層事前分布
  }
  for (n in 1:N) {
    r[n] ~ normal(0, s_r);      // 階層事前分布
    y[n] ~ poisson(lambda[n]);  // 種子数が平均lambdaのポアソン分布に従って生成されるとする
  }
  
  b1 ~ normal(0, 1000);         // 無情報事前分布
  b2 ~ normal(0, 1000);         // 無情報事前分布
  s_p ~ uniform(0, 1000);       // 無情報事前分布
  s_r ~ uniform(0, 1000);       // 無情報事前分布
}

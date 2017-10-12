data {
  int N;                        //サンプルサイズ
  int y[N];                     //生存種子数
}

parameters {
  real b;                       //切片
  real r[N];                    //ランダム効果(個人差)
  real<lower=0> s;              //個人差の標準偏差
}

transformed parameters {
  real<lower=0,upper=1> q[N];
  for (n in 1:N) {
    q[n] = inv_logit(b + r[n]); //切片とランダム効果で生存確率を表現
  }
}

model {
  for (n in 1:N) {
    r[n] ~ normal(0, s);        //階層事前分布
    y[n] ~ binomial(8, q[n]);   //生存種子数を二項分布でモデル化
  }

  b ~ normal(0, 1000);          //無情報事前分布
  s ~ uniform(0, 1000);         //無情報事前分布
}

generated quantities {
  real y_pred[N];
  for (n in 1:N){
    y_pred[n] = binomial_rng(8, q[n]); //おまけ（事後分布からサンプリング）
  }
}

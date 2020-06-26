data {
  int <lower=1> M; // number of countries
  int <lower=1> N0; // number of days for which to impute infections
  int<lower=1> N; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
  int cases[N2,M]; // reported cases
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  int deathsh[N2, M];
  int deathsc[N2, M];
  int hosp[N2, M]; // reported hospitalizations -- the rows with i > N contain -1 and should be ignored
  int hospin[N2, M];
  int hospout[N2, M];
  int icu[N2, M]; // reported icus -- the rows with i > N contain -1 and should be ignored
  int icuin[N2, M];
  int icuout[N2, M];
  matrix[N2, M] f1; // h * s
  matrix[N2, M] f2; // h * s
  matrix[N2, M] f3; // h * s
  matrix[N2, M] f4;
  matrix[N2, M] f5;
  matrix[N2, M] f6;
  matrix[N2, M] f7;
  matrix[N2, 3] covariate;
              int EpidemicStart;
  real pop;
  real SI[N2]; // fixed pre-calculated SI using emprical data from Neil
}

parameters {
real<lower=0> mu[M]; // intercept for Rt

matrix<lower=0>[3, M] alpha_hier; // sudo parameter for the hier term for alpha

real<lower=0> y[M];
real<lower=0> phi_dc;
real<lower=0> phi_dh;
real<lower=0> phi_c;
real<lower=0> phi_h;
real<lower=0> phi_hi;
real<lower=0> phi_ho;
real<lower=0> phi_i;
real<lower=0> phi_ii;
real<lower=0> phi_io;
real<lower=0> tau;
//real<lower=0> alpha_mean[3//];
real <lower=0> ifr_noise_1[M];
real <lower=0> ifr_noise_2[M];
real <lower=0> ifr_noise_3[M];
real <lower=0> ifr_noise_5[M];
real <lower=0> ifr_noise_7[M];
//real<lower=0> alpha_var;
real<lower=0> var_phi_dc;
real<lower=0> var_phi_dh;
real<lower=0> var_phi_c;
real<lower=0> var_phi_h;
real<lower=0> var_phi_hi;
real<lower=0> var_phi_ho;
real<lower=0> var_phi_i;
real<lower=0> var_phi_ii;
real<lower=0> var_phi_io;
}

transformed parameters {
matrix[3, M] alpha;
matrix[N2, M] prediction = rep_matrix(0,N2,M);
matrix[N2, M] E_deaths  = rep_matrix(0,N2,M);
matrix[N2, M] E_deathsh  = rep_matrix(0,N2,M);
matrix[N2, M] E_deathsc  = rep_matrix(0,N2,M);
matrix[N2, M] E_cases  = rep_matrix(0,N2,M);
matrix[N2, M] E_hospitals  = rep_matrix(0,N2,M);
matrix[N2, M] E_hospitalsi  = rep_matrix(0,N2,M);
matrix[N2, M] E_hospitalso  = rep_matrix(0,N2,M);
matrix[N2, M] E_icus  = rep_matrix(0,N2,M);
matrix[N2, M] E_icusi  = rep_matrix(0,N2,M);
matrix[N2, M] E_icuso  = rep_matrix(0,N2,M);
matrix[N2, M] Rt = rep_matrix(0,N2,M);
matrix[N2, M] Rt_adj = Rt;

{
  matrix[N2,M] cumm_sum = rep_matrix(0,N2,M);
  for(m in 1:M){
  for(i in 1:3){
  alpha[i, m] = alpha_hier[i, m] - log(2);
  }
  }
  for (m in 1:M){
  for (i in 2:N0){
  cumm_sum[i,m] = cumm_sum[i-1,m] + y[m];
  }
  prediction[1:N0,m] = rep_vector(y[m],N0); // learn the number of cases in the first N0 days
  Rt[,m] = mu[m]*exp(covariate[,1]*(-alpha[1,m])+covariate[,2]*(-alpha[2,m])+covariate[,3]*(-alpha[3,m]));
  Rt_adj[1:N0,m] = Rt[1:N0,m];
  for (i in (N0+1):N2) {
  real convolution=0;
  for(j in 1:(i-1)) {
  convolution += prediction[j, m] * SI[i-j];
  }
  cumm_sum[i,m] = cumm_sum[i-1,m] + prediction[i-1,m];
  Rt_adj[i,m] = ((pop-cumm_sum[i,m]) / pop) * Rt[i,m];
  prediction[i, m] = Rt_adj[i,m] * convolution;
  }
  
  
  
  E_cases[1, m]= 1e-15 * prediction[1,m];
  for (i in 2:N2){
  for(j in 1:(i-1)){
  E_cases[i,m] += prediction[j,m] * f1[i-j,m] * ifr_noise_1[m];
  }
  }
  
  
  E_hospitalsi[1, m]= 1e-15 * prediction[1,m];
  for (i in 2:N2){
  for(j in 1:(i-1)){
  E_hospitalsi[i,m] += prediction[j,m] * f3[i-j,m] * ifr_noise_3[m];
  }
  }
  
  E_hospitalso[1, m]= 1e-15 * E_hospitalsi[1,m];
  for (i in 2:N2){
  for(j in 1:(i-1)){
  E_hospitalso[i,m] += E_hospitalsi[j,m] * f4[i-j,m] ;
  }
  }
  
  
  E_hospitals[1, m] =  E_hospitalsi[1,m]-E_hospitalso[1,m];
  for (i in 2:N2){
  E_hospitals[i,m]+=  E_hospitals[i-1,m]+  E_hospitalsi[i,m]-E_hospitalso[i,m];
  }
  
  E_icusi[1, m]= 1e-15 * E_hospitalsi[1,m];
  for (i in 2:N2){
  for(j in 1:(i-1)){
  E_icusi[i,m] += E_hospitalsi[j,m] * f5[i-j,m] * ifr_noise_5[m];
  }
  }
  
  E_icuso[1, m]= 1e-15 * E_icusi[1,m];
  for (i in 2:N2){
  for(j in 1:(i-1)){
  E_icuso[i,m] += E_icusi[j,m] * f6[i-j,m] ;
  }
  }
  
  
  E_icus[1, m] = E_icusi[1,m]-E_icuso[1,m];
  for (i in 2:N2){
  E_icus[i,m]+=  E_icus[i-1,m]+  E_icusi[i,m]-E_icuso[i,m];
  }
  
  E_deathsc[1, m]=  1e-15 * prediction[1,m];
  for (i in 2:N2){
  for(j in 1:(i-1)){
  E_deathsc[i,m] +=  prediction[j,m] * f2[i-j,m] * ifr_noise_2[m];
  }
  }
  
  E_deathsh[1, m]=  1e-15*E_hospitalsi[1,m];
  for (i in 2:N2){
  for(j in 1:(i-1)){
  E_deathsh[i,m] +=  E_hospitalsi[j,m] * f7[i-j,m] * ifr_noise_7[m];
  }
  }
  
  
  for (i in 1:N2){
  E_deaths[i,m] += E_deathsh[i,m]+E_deathsc[i,m];
  }
  
  }
}
}
model {
tau ~ exponential(0.03);
for (m in 1:M){
y[m] ~ exponential(1/tau);
}

var_phi_dc ~ normal(8,2);
var_phi_dh ~ normal(8,2);
var_phi_c ~ normal(8,2);//normal(8,2)
var_phi_h ~ normal(8,2);
var_phi_hi ~ normal(8,2);
var_phi_ho ~ normal(8,2);
var_phi_i ~ normal(8,2);
var_phi_ii ~ normal(8,2);
var_phi_io ~ normal(8,2);

phi_dc ~ normal(0,var_phi_dc); // normal(0,5)
phi_dh ~ normal(0,var_phi_dh); // normal(0,5)
phi_c ~ normal(0,var_phi_c);// normal(0,8.5)
phi_h ~ normal(0,var_phi_h);// normal(0,6)
phi_hi ~ normal(0,var_phi_hi);// normal(0,6)
phi_ho ~ normal(0,var_phi_ho);// normal(0,6)
phi_i ~ normal(0,var_phi_i);// normal(0,6)
phi_ii ~ normal(0,var_phi_ii);// normal(0,6)
phi_io ~ normal(0,var_phi_io);// normal(0,6)
mu ~ normal(3.28, 0.25); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319

//for (i in 1:3){
//alpha_mean[i] ~ gamma(mualpha, 2); //.1667,1
//}
//alpha_var~normal(0,2);
//
for(i in 1:3){

for(m in 1:M){
alpha_hier[i, m] ~ gamma(1,1); //  alpha_mean[i],alpha_var    .1667, alpha_var
}
}
ifr_noise_1 ~ normal(1,.5);//.1
ifr_noise_2 ~ normal(1,.5);
ifr_noise_3 ~ normal(1,.5);
ifr_noise_5 ~ normal(1,.5);
ifr_noise_7 ~ normal(1,.5);

for(m in 1:M){
deathsh[EpidemicStart:N, m] ~ neg_binomial_2(E_deathsh[EpidemicStart:N, m], phi_dh);
}
for(m in 1:M){
deathsc[EpidemicStart:N, m] ~ neg_binomial_2(E_deathsc[EpidemicStart:N, m], phi_dc);
}
for(m in 1:M){
cases[EpidemicStart:N, m] ~ neg_binomial_2(E_cases[EpidemicStart:N, m], phi_c);
}

for(m in 1:M){
hosp[EpidemicStart:N, m] ~ neg_binomial_2(E_hospitals[EpidemicStart:N, m], phi_h);
}

for(m in 1:M){
hospin[EpidemicStart:N, m] ~ neg_binomial_2(E_hospitalsi[EpidemicStart:N, m], phi_hi);
}

for(m in 1:M){
hospout[EpidemicStart:N, m] ~ neg_binomial_2(E_hospitalso[EpidemicStart:N, m], phi_ho);
}
for(m in 1:M){
icu[EpidemicStart:N, m] ~ neg_binomial_2(E_icus[EpidemicStart:N, m], phi_i);
}

for(m in 1:M){
icuin[EpidemicStart:N, m] ~ neg_binomial_2(E_icusi[EpidemicStart:N, m], phi_ii);
}

for(m in 1:M){
icuout[EpidemicStart:N, m] ~ neg_binomial_2(E_icuso[EpidemicStart:N, m], phi_io);
}
}

generated quantities {
matrix[N2, M] prediction0 = rep_matrix(0,N2,M);

{
  matrix[N2,M] cumm_sum0 = rep_matrix(0,N2,M);
  for (m in 1:M){
  for (i in 2:N0){
  cumm_sum0[i,m] = cumm_sum0[i-1,m] + y[m];
  }
  prediction0[1:N0,m] = rep_vector(y[m],N0);
  for (i in (N0+1):N2) {
  real convolution0 = 0;
  for(j in 1:(i-1)) {
  convolution0 += prediction0[j, m] * SI[i-j];
  }
  cumm_sum0[i,m] = cumm_sum0[i-1,m] + prediction0[i-1,m];
  prediction0[i, m] =  ((pop-cumm_sum0[i,m]) / pop) * mu[m] * convolution0;
  }
  
  
  }
}
}

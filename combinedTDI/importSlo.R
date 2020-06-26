#####packages needed
Sys.setenv(MAKEFLAGS = "-j8")

# source("readData.R") #comment out if data.Rdata exists and you don't want to update the data

load("data.Rdata")



# library(devtools)
library(splines)
 

# install_github("maxconway/gsheet")
library(gsheet)

library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(optparse)


######parameters

if(!exists('interventions')) interventions<-as.Date(c("2020-03-10","2020-03-20","2020-03-30","2020-04-30")) #need to be arranged by order, update when neccessary



if(!exists('fit.spline')) fit.spline=TRUE #if FALSE fit step function for Rt, otherwise spline. TRUE gives nonsense results thus far, don't use!
if(!exists('natural.spline')) natural.spline=TRUE #if true natural spline is fitted. else cubic spline in which case you can also define the degree. boundary points are c(1,N) in both cases.
if(!exists('spline.degree')) spline.degree=1 #1 is for piecewise linear, 2 for quadratic, 3 for cubic,...



if(!exists('out_of_time_fixed')) out_of_time_fixed <- FALSE
if(out_of_time_fixed) no_of_days <- max(out_times)
if(!exists('out_of_time_var')) out_of_time_var <- FALSE


#### OPTIMAL PARAMETERS:
 
mu.serial_opt <- 6.5
forecast_opt <- 10


tau1<-0.1
tau2<-0.02*0.3
tau3<-0.07
tau5<-0.2
tau7<-0.2
tau6<-1
tau4<-1

mu1<-4
mu2<-18
mu3<-2
mu4<-10
mu5<-2
mu6<-11
mu7<-8


nms<-paste("t1",tau1,"t2",tau2,"t3",tau3,"t4",tau4,"t5",tau5,"t6",tau6,"t7",tau7,"m1",mu1,"m2",mu2,"m3",mu3,"m4",mu4,"m5",mu5,"m6",mu6,"m7",mu7,sep="")

#save(ifr_deaths_opt, ifr_cases_opt, ifr_hosp_opt, ifr_icu_opt,
 #    mu.serial_opt, forecast_opt, mean1_opt, mean2_opt,
  #   mean3_opt, mean4_opt, mean5_opt, 
   #  file=paste0('combined/optimal_pars.RData'))
###


# Define whether model should be built using data on deaths, cases, icu, hosp (these two still not implemented)
model_input <- "combinedTDI" #ortion of subjects which move from truly infected to icu


# Serial interval. updated by rok
# serial.interval = read.csv("data/serial_interval.csv") #need to change for sensitivity analysis, Nina?, change the path!
if(!exists('mu.serial')) mu.serial <- mu.serial_opt
cv.serial <- 0.62



##code is later


####use either forecast, or N2, set the other to NULL!
if(!exists('forecast')) forecast = forecast_opt #it is overriden later by forecats=N-N2, where N is the number of data for Slo
N2 =NULL #90 # increase if you need more forecast equal to number of observed data + num forcast data

d1_pop = 2078890 #population size of Slo, change if JiJs forcasts will someday come true.


# various distributions required for modeling
mean0 = 5.1
cv0 = 0.86; # infection to onset


x1 = rgammaAlt(1e7,mean0,cv0) # infection-to-onset distribution
cv1 =0.25
cv2 = 0.45  

cv3 = 0.45  

cv4 = 0.45  

cv5 = 0.45 

cv6=0.45
cv7=0.45

cvs<-paste("m0",mean0,"cv1",cv1,"cv2",cv2,"cv3",cv3,"cv4",cv4,"cv5",cv5,"cv6",cv6,"cv7",cv7,sep="")

x2 = rgammaAlt(1e7,mu1,cv1) 

pi1<-x1+x2

 
xd = rgammaAlt(1e7,mu2,cv2)

pi2=x1+xd

xh=rgammaAlt(1e7,mu3,cv3)

pi3 = x1+xh  
 pi4 = rgammaAlt(1e7,mu4,cv4)  
 pi5 = rgammaAlt(1e7,mu5,cv5)  
pi6 = rgammaAlt(1e7,mu6,cv6)  
pi7 = rgammaAlt(1e7,mu7,cv7)  

 
ecdf.saved.1 = ecdf(pi1)   
ecdf.saved.2 = ecdf(pi2) 
ecdf.saved.3 = ecdf(pi3) 
ecdf.saved.4 = ecdf(pi4) 
ecdf.saved.5 = ecdf(pi5) 
ecdf.saved.6 = ecdf(pi6) 
ecdf.saved.7 = ecdf(pi7)  



####data
if(exists('limit_date')) dd <- dd[as.Date(substr(dd$datum, 1,7), format = "%d. %m.")<=limit_date,] # limit data to a given date
idrow<-1:(nrow(dd)) #-which.max((diff(cumsum(is.na(rev(dd$datum)))))==0)
# dd <- dd[as.Date(substr(dd$datum, 1,7), format = "%d. %m.")<=as.Date("2020-04-13"),] # limit data to a given date
dd <- dd[idrow,]

#load("data.Rdata")

if(out_of_time_fixed) dd <- dd[1:(nrow(dd)-no_of_days),]
if(out_of_time_var) dd<-dd[1:(nrow(dd)-forecast),]

hospitalizirani<-as.numeric(dd$hospitalizirani..trenutno.[-1]) #cumulative! prispevek je lahko negativen!
hospitalizirani[is.na(hospitalizirani)]<-0

hospitalizirani<-c(rep(0,50),hospitalizirani)



hospitaliziraniin<-dd$novi[-1]

 hospitaliziraniin[is.na(hospitaliziraniin)]<-0

hospitaliziraniin<-c(rep(0,50),hospitaliziraniin)


hospitaliziraniout<-as.numeric(dd$iz.bol..oskrbe..vsi.[-1])
hospitaliziraniout[is.na(hospitaliziraniout)]<-0

hospitaliziraniout<-c(rep(0,50),hospitaliziraniout)
hospitaliziraniouti<-hospitaliziraniout
for (i in 2:length(hospitaliziraniout)){
  hospitaliziraniouti[i]<-hospitaliziraniout[i]-hospitaliziraniout[i-1]
}
hospitaliziraniout<-hospitaliziraniouti


cases<- c(rep(0,50),as.numeric(dd$potrjeni.danes[-1]))
cases[is.na(cases)]<-0


deaths<- as.numeric(dd$umrli..vsi.[-1]) #cumulative!
deaths[is.na(deaths)]<-0

deaths<-c(rep(0,50),deaths)
deathsi<-deaths
for (i in 2:length(deaths)){
  deathsi[i]<-deaths[i]-deaths[i-1]
}
deaths<-deathsi


icu<-c(rep(0,50), as.numeric(dd$intenzivna.enota..trenutno.[-1])) #cumulative! prispevek je lahko negativen!
icu[is.na(icu)] <- 0




icuin<-c(rep(0,50), as.numeric(dd$I.i[-1])) #cumulative! prispevek je lahko negativen!
icuin[is.na(icuin)] <- 0

icuout<-c(rep(0,50), as.numeric(dd$I.o[-1])) #cumulative! prispevek je lahko negativen!
icuout[is.na(icuout)] <- 0



deathsc<-c(rep(0,50), as.numeric(dd$D.u[-1])) #cumulative! prispevek je lahko negativen!
deathsc[is.na(deathsc)] <- 0

deathsh<-c(rep(0,50), as.numeric(dd$H.D[-1])) #cumulative! prispevek je lahko negativen!
deathsh[is.na(deathsh)] <- 0

 
day<-strsplit(dd[-1,2],split="\\.")

ff<-function(i,x){
  decembr <- which(unlist(lapply(x, function(iter) iter[2]=='12')))
  if(identical(integer(0), decembr)){
    leto <- '.2020'
  } else{
    if(max(december)<i){
      leto <- '.2021'
    } else{
      leto <- '.2020'
    }
  }
  paste(x[[i]][1],".",strsplit(day[[i]][2]," ")[[1]][2],leto,sep="")
  
}

day<-unlist(lapply(1:length(day),ff,day))
day<-as.Date(day,format="%d.%m.%Y")

day<-c((day[1]-1:50)[order(day[1]-1:50)],day)


#####

###added due to ofoftime predictions



cov.true<-list()
for (i in 1:length(interventions)){
  if (sum(day==interventions[i])==0) cov.true[[i]]<-FALSE else cov.true[[i]]<-TRUE

}

interventions<-interventions[which(unlist(cov.true)==TRUE)]

####updated by rok: now the interpretation of alpha is different!

covariates.step<-matrix(0,ncol=length(interventions),nrow=length(day))

for (i in 1:ncol(covariates.step)){

  if (i<ncol(covariates.step))  covariates.step[which(day==interventions[i]):(which(day==interventions[i+1])-1),i]<-1   else covariates.step[which(day==interventions[i]):nrow(covariates.step),i]<-1

}


###############Bayes part

Country="Slovenia"
countries <- Country




dates = list()
reported_cases = list()

deathsc_by_country =deathsh_by_country =deaths_by_country =hosps_by_country=icus_by_country=hospsin_by_country=hospsout_by_country=icusin_by_country=icusout_by_country= list()



d1=data.frame(DateRep=day,Cases=cases,Deaths=deaths,Deathsh=deathsh,Deathsc=deathsc,Hosp=hospitalizirani,Icu=icu,Hospin=hospitaliziraniin,Hospout=hospitaliziraniout,Icuin=icuin,Icuout=icuout)
d1$date<-d1$DateRep
d1$t = decimal_date(d1$DateRep)
d1=d1[order(d1$t),]

date_min <- dmy('31/12/2019')
if ( d1$DateRep[1]  > as.Date(date_min, format='%d/%m/%Y')){
  print(paste(Country,'In padding'))
  pad_days <- d1$DateRep[1]  - date_min
  pad_dates <- date_min + days(1:pad_days[[1]]-1)
  padded_data <- data.frame("DateRep" =  pad_dates ,
                            "t" = decimal_date(as.Date(pad_dates,format='%d/%m/%Y')),
                            "date" = as.Date(pad_dates,format='%d/%m/%Y'),
                            "Cases" = as.integer(rep(0, pad_days)),
                            "Deaths" = as.integer(rep(0, pad_days)),
                            "Deathsh" = as.integer(rep(0, pad_days)),
                            "Deathsc" = as.integer(rep(0, pad_days)),
                            "Hosp" = as.integer(rep(0, pad_days)),
                            "Icu" = as.integer(rep(0, pad_days)),
				"Hospin" = as.integer(rep(0, pad_days)),
				"Hospout" = as.integer(rep(0, pad_days)),
				"Icuin" = as.integer(rep(0, pad_days)),
				"Icuout" = as.integer(rep(0, pad_days)),
                            stringsAsFactors=F)

  d1 <- bind_rows(padded_data, d1)
  covariates.step<-rbind(matrix(0,nrow=nrow(padded_data),ncol=ncol(covariates.step)),covariates.step)
}
index = which(d1$Cases>0)[1]
index1 = which(cumsum(d1$Deaths)>=10)[1] # also 5
index2 = index1-30

print(sprintf("First non-zero cases is on day %d, and 30 days before 10 deaths is day %d",index,index2))

covariates.step<-covariates.step[index2:nrow(d1),]
d1=d1[index2:nrow(d1),]






dates[[Country]] = d1$date
# hazard estimation
N = length(d1$Cases)
print(sprintf("%s has %d days of data",Country,N))
if (is.null(forecast)) forecast = N2 - N
if (is.null(N2)) N2<-N+forecast

###Roks serial interval code

x.si<-rgammaAlt(1e7,mu.serial,cv.serial)
ecdf.saved.si = ecdf(x.si)

convolution.si = function(u) ( ecdf.saved.si(u))

f.si = rep(0,N2) # f is the probability of dying on day i given infection
f.si[1] = (convolution.si(1.5) - convolution.si(0))
for(i in 2:N2) {
  f.si[i] = (convolution.si(i+.5) - convolution.si(i-.5))
}

serial.interval.r <- data.frame(X=1:N2, fit=rep(NA,N2))
serial.interval.r[,"fit"]<-f.si
serial.interval<-serial.interval.r



stan_data = list(M=1,N=NULL,deaths=NULL,deathsh=NULL,deathsc=NULL,hosp=NULL,icu=NULL,hospin=NULL,hospout=NULL,icuin=NULL,icuout=NULL,f1=NULL,f2=NULL,f3=NULL,f4=NULL,f5=NULL,f6=NULL,f7=NULL,
                 N0=6,cases=NULL,SI=serial.interval$fit[1:N2],
                 EpidemicStart = NULL, pop = NULL) # N0 = 6 to make it consistent with Rayleigh

stan_data$EpidemicStart = c(stan_data$EpidemicStart,index1+1-index2)
stan_data$pop = c(stan_data$pop, d1_pop)


# IFR is the overall probability of dying given infection
convolution.1 = function(u) (tau1 * ecdf.saved.1(u))

f1 = rep(0,N2) # f is the probability of dying on day i given infection
f1[1] = (convolution.1(1.5) - convolution.1(0))
for(i in 2:N2) {
  f1[i] = (convolution.1(i+.5) - convolution.1(i-.5))
}

convolution.2 = function(u) (tau2 * ecdf.saved.2(u))

f2 = rep(0,N2) # f is the probability of dying on day i given infection
f2[1] = (convolution.2(1.5) - convolution.2(0))
for(i in 2:N2) {
  f2[i] = (convolution.2(i+.5) - convolution.2(i-.5))
}


convolution.3 = function(u) (tau3 * ecdf.saved.3(u))


f3 = rep(0,N2) # f is the probability of dying on day i given infection
f3[1] = (convolution.3(1.5) - convolution.3(0))
for(i in 2:N2) {
  f3[i] = (convolution.3(i+.5) - convolution.3(i-.5))
}

convolution.4 = function(u) (tau4 * ecdf.saved.4(u))


f4 = rep(0,N2) # f is the probability of dying on day i given infection
f4[1] = (convolution.4(1.5) - convolution.4(0))
for(i in 2:N2) {
  f4[i] = (convolution.4(i+.5) - convolution.4(i-.5))
}

convolution.5 = function(u) (tau5 * ecdf.saved.5(u))


f5 = rep(0,N2) # f is the probability of dying on day i given infection
f5[1] = (convolution.5(1.5) - convolution.5(0))
for(i in 2:N2) {
  f5[i] = (convolution.5(i+.5) - convolution.5(i-.5))
}

convolution.6 = function(u) (tau6 * ecdf.saved.6(u))


f6 = rep(0,N2) # f is the probability of dying on day i given infection
f6[1] = (convolution.6(1.5) - convolution.6(0))
for(i in 2:N2) {
  f6[i] = (convolution.6(i+.5) - convolution.6(i-.5))
}

convolution.7 = function(u) (tau7 * ecdf.saved.7(u))


f7 = rep(0,N2) # f is the probability of dying on day i given infection
f7[1] = (convolution.7(1.5) - convolution.7(0))
for(i in 2:N2) {
  f7[i] = (convolution.7(i+.5) - convolution.7(i-.5))
}


reported_cases[[Country]] = as.vector(as.numeric(d1$Cases))
cases=c(as.vector(as.numeric(d1$Cases)),rep(-1,forecast))


  deaths=c(as.vector(as.numeric(d1$Deaths)),rep(-1,forecast))
  deaths_by_country[[Country]] = as.vector(as.numeric(d1$Deaths))

  
  deathsh=c(as.vector(as.numeric(d1$Deathsh)),rep(-1,forecast))
  deathsh_by_country[[Country]] = as.vector(as.numeric(d1$Deathsh))
  
  deathsc=c(as.vector(as.numeric(d1$Deathsc)),rep(-1,forecast))
  deathsc_by_country[[Country]] = as.vector(as.numeric(d1$Deathsc))
  
  
  hosps=c(as.vector(as.numeric(d1$Hosp)),rep(-1,forecast))
  hosps_by_country[[Country]] = as.vector(as.numeric(d1$Hosp))

 hospsin=c(as.vector(as.numeric(d1$Hospin)),rep(-1,forecast))
  hospsin_by_country[[Country]] = as.vector(as.numeric(d1$Hospin))

 hospsout=c(as.vector(as.numeric(d1$Hospout)),rep(-1,forecast))
  hospsout_by_country[[Country]] = as.vector(as.numeric(d1$Hospout))


  icus=c(as.vector(as.numeric(d1$Icu)),rep(-1,forecast))
  icus_by_country[[Country]] = as.vector(as.numeric(d1$Icu))

  icusin=c(as.vector(as.numeric(d1$Icuin)),rep(-1,forecast))
  icusin_by_country[[Country]] = as.vector(as.numeric(d1$Icuin))

  icusout=c(as.vector(as.numeric(d1$Icuout)),rep(-1,forecast))
  icusout_by_country[[Country]] = as.vector(as.numeric(d1$Icuout))

stan_data$f1 = cbind(stan_data$f1,f1)
 stan_data$f2 = cbind(stan_data$f2,f2)
stan_data$f3 = cbind(stan_data$f3,f3)
stan_data$f4 = cbind(stan_data$f4,f4)
stan_data$f5 = cbind(stan_data$f5,f5)
stan_data$f6 = cbind(stan_data$f6,f6)
stan_data$f7 = cbind(stan_data$f7,f7)

stan_data$deaths = cbind(stan_data$deaths,deaths)

stan_data$deathsh = cbind(stan_data$deathsh,deathsh)
stan_data$deathsc = cbind(stan_data$deathsc,deathsc)


stan_data$cases = cbind(stan_data$cases,cases)

stan_data$hosp = cbind(stan_data$hosp,hosps)
stan_data$hospin = cbind(stan_data$hospin,hospsin)
stan_data$hospout = cbind(stan_data$hospout,hospsout)

stan_data$icu = cbind(stan_data$icu,icus)
stan_data$icuin = cbind(stan_data$icuin,icusin)
stan_data$icuout = cbind(stan_data$icuout,icusout)


stan_data$N2=N2
stan_data$x=1:N2

stan_data$N = c(stan_data$N,N)


covariates.step<-rbind(covariates.step,matrix(0,ncol=ncol(covariates.step),nrow=N2-nrow(covariates.step)))
covariates.step[(N+1):N2,ncol(covariates.step)]<-1

#careful with spline! 10 days extra are for prediction, do we take these days in construction? probably yes
knot.pos<- which(d1$date%in%interventions==TRUE)


if (natural.spline==FALSE){
  covariates.spline<-bs(1:nrow(covariates.step),knots=knot.pos,degree=spline.degree,Boundary.knots =c(1,N))
} else {
  covariates.spline<-ns(1:nrow(covariates.step),knots=knot.pos,Boundary.knots =c(1,N))
}

if (fit.spline==FALSE) stan_data$covariate<-covariates.step else stan_data$covariate<-covariates.spline



# create the `any intervention` covariate
# stan_data$covariate <- cbind(stan_data$covariate,
#                              1*(rowSums(stan_data$covariate) >= 1))

####Bayes model, adjust hiperparameters in the function if needed. could make a function where the hyperparameters are arguments, but its not difficult to change them by hand

stan_filename <- paste0('natural_spline',natural.spline,
                        '_sens_',mu.serial,
                        '_last_date_',
                        day(tail(d1, 1)$DateRep),"-",month(tail(d1, 1)$DateRep)
)

gen.stan<-function(nAlpha, stan_filename){
  txt1<-"data {
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
  "
  #txt2<-paste("matrix[N2, M] covariate",1:nAlpha,";\n",sep="")
  txt2<-paste("matrix[N2, ",nAlpha,"] covariate;
              ",sep="")
  
  
  txt3<-"int EpidemicStart;
  real pop;
  real SI[N2]; // fixed pre-calculated SI using emprical data from Neil
}

parameters {
real<lower=0> mu[M]; // intercept for Rt
"
txt4<-"
matrix<lower=0>["
txt41<-", M] alpha_hier; // sudo parameter for the hier term for alpha
"
txt5<-"
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
"
txt6<-"//real<lower=0> alpha_mean["
txt61<-"//];"
txt7<-"
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
"
txt8<-"matrix["
txt81<-", M] alpha;"
txt9<-"
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
  for(m in 1:M){"
  
  txt10<-"
  for(i in 1:"
  txt10.1<-"){"
  txt11<-"
  alpha[i, m] = alpha_hier[i, m] - log(2);
  }
  }
  for (m in 1:M){
  for (i in 2:N0){
  cumm_sum[i,m] = cumm_sum[i-1,m] + y[m];
  }
  prediction[1:N0,m] = rep_vector(y[m],N0); // learn the number of cases in the first N0 days
  "
  #txt12<-paste("Rt[,m] = mu[m]*exp(",paste(paste("covariate",1:nAlpha,"[,m]*(-alpha[",1:nAlpha,",m])",sep=""),collapse="+"),");",sep="")
  txt12<-paste("Rt[,m] = mu[m]*exp(",paste(paste("covariate[,",1:nAlpha,"]*(-alpha[",1:nAlpha,",m])",sep=""),collapse="+"),");",sep="")
  
  txt13<-"
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
"
txt14<-"
//for (i in 1:"
txt141<-"){"
txt15<-"
//alpha_mean[i] ~ gamma(mualpha, 2); //.1667,1
//}
//alpha_var~normal(0,2);
//"
txt16<-"
for(i in 1:"
txt161<-"){
"
txt17<-"
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
"

cat(txt1,txt2,txt3,txt4,nAlpha,txt41,txt5,txt6,nAlpha,txt61,txt7,txt8,nAlpha,txt81,txt9,txt10,nAlpha,txt10.1,
    txt11,txt12,txt13,txt14,nAlpha,txt141,txt15,txt16,nAlpha,txt161,txt17,file=paste0("combinedTDI/stan-models/", stan_filename, ".stan"),sep="")



}

#######end function


#######start analysis


gen.stan(ncol(stan_data$covariate), stan_filename)

options(mc.cores = 4)


m = stan_model(paste0('combinedTDI/stan-models/',stan_filename,'.stan'))

# fit = sampling(m,data=stan_data,iter=50,warmup=20,chains=2,control = list(adapt_delta = 0.98, max_treedepth = 15)) #for debug
fit = sampling(m,data=stan_data,iter=1600,warmup=800,chains=4,thin=4,control = list(adapt_delta = 0.98, max_treedepth = 15)) #used for testing

out = rstan::extract(fit)

#par(mfrow=c(4,3))
#plot(apply(out$prediction[,,1],2,median)[1:N],type="l")
#plot(apply(out$E_deaths[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$deaths[1:N],type="p")

#plot(apply(out$E_deathsc[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$deathsc[1:N],type="p")

#plot(apply(out$E_deathsh[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$deathsh[1:N],type="p")


#plot(apply(out$E_cases[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$cases[1:N],type="p")

#plot(apply(out$E_hospitals[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$hosp[1:N],type="p")

#plot(apply(out$E_hospitalsi[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$hospin[1:N],type="p")

#plot(apply(out$E_hospitalso[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$hospout[1:N],type="p")

#plot(apply(out$E_icus[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$icu[1:N],type="p")

#plot(apply(out$E_icusi[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$icuin[1:N],type="p")

#plot(apply(out$E_icuso[,,1],2,median)[1:N],type="l")
#lines(1:N,stan_data$icuout[1:N],type="p")


#plot(apply(out$Rt[,,1],2,median)[1:N],type="l")

###########################
# Plot functions
###########################

prediction = out$prediction
estimated.deaths = out$E_deaths
estimated.deathsh = out$E_deathsh
estimated.deathsc = out$E_deathsc
estimated.cases = out$E_cases
estimated.hospitals = out$E_hospitals
estimated.hospitalsi = out$E_hospitalsi
estimated.hospitalso = out$E_hospitalso
estimated.icus = out$E_icus
estimated.icusi = out$E_icusi
estimated.icuso = out$E_icuso
estimated.deaths.cf = out$E_deaths0

covariates <- stan_data$covariate

JOBID <- paste0('natural_spline',natural.spline,'-',"-ifr-",34,"-SImean-",mu.serial,"-input","-",model_input,"-","last_date_", day(tail(d1, 1)$DateRep),"-",month(tail(d1, 1)$DateRep),'-out_of_time_fixed-',out_of_time_fixed,'-out_of_time_var-',out_of_time_var, '-forecast-', forecast)
print(sprintf("Jobid = %s",JOBID))

# save.image(paste0(model_input,'/results/',JOBID,'.Rdata'))
save(fit,prediction,dates,reported_cases,deaths_by_country,deathsh_by_country,deathsc_by_country,hosps_by_country,hospsin_by_country,hospsout_by_country,icus_by_country,icusin_by_country,icusout_by_country,countries,estimated.deaths,estimated.deathsc,estimated.deathsh,estimated.deaths.cf,estimated.cases,estimated.hospitals,estimated.hospitalsi,estimated.hospitalso,estimated.icus,estimated.icusi,estimated.icuso,out,covariates,interventions,fit.spline,forecast,natural.spline,fit.spline,file=paste0(model_input,'/results/',JOBID,'-stanfit.Rdata'))
library(bayesplot)
filename <- JOBID
system(paste0("Rscript covariate-size-effects.r ", filename,'-stanfit.Rdata ', model_input))
mu = (as.matrix(out$mu))
colnames(mu) = countries
g = (mcmc_intervals(mu,prob = .9))
system(paste0("Rscript ", model_input, "/plot-3-panel.r ", filename,'-stanfit.Rdata'))
system(paste0("Rscript ", model_input, "/plot-forecast.r ",filename,'-stanfit.Rdata'))

# setwd("combinedTDI/figures")
# file.rename(paste0("Slovenia_three_pannel_",filename,".pdf"),paste0(nms,cvs,".pdf"))
# setwd("..")
# setwd("..")


tmp = lapply(1:length(countries), function(i) (out$Rt_adj[,stan_data$N[i],i]))
Rt_adj = do.call(cbind,tmp)
colnames(Rt_adj) = countries
g = (mcmc_intervals(Rt_adj,prob = .9))
ggsave(paste0(model_input, "/results/",filename,"-final-rt.pdf"),g,width=4,height=6)
# system(paste0("Rscript ", model_input, "/plot-3-panel.r ", filename,'-stanfit.Rdata'))
# system(paste0("Rscript ", model_input, "/plot-forecast.r ",filename,'-stanfit.Rdata'))
system(paste0("Rscript plot-sens.r ",filename,'-stanfit.Rdata ', model_input))
# if(optimal_pars) 
system(paste0("Rscript plot-convergence.R ",filename,'-stanfit.Rdata ', model_input))

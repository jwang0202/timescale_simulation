rm(list=ls())
library(dplyr)
library(plyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(MASS)
library(lme4)
library(afex)
library(table1)
library(readxl)
mutate <- dplyr::mutate
select <- dplyr::select
rename <- dplyr::rename
summarise <- dplyr::summarise

sampling_alpha0 <- function(C,alpha1,mean_prob=0.6){
  # Wide range of alpha0
  alpha0_list = seq(-5,5,by=0.5)
  # Get mean prob of education according to each alpha0
  mean_prob_list = apply(as.matrix(alpha0_list),1,function(x) mean(exp(x + C*alpha1)/(1+exp(x + C*alpha1))))
  # Find alpha0 such that mean prob is closest to mean_prob
  alpha0 = alpha0_list[which.min(abs(mean_prob - mean_prob_list))]
  # Narrow down range of alpha0
  alpha0_list = seq(alpha0-.5,alpha0+.5,by=0.002)
  # Get mean prob of education according to each alpha0
  mean_prob_list = apply(as.matrix(alpha0_list),1,function(x) mean(exp(x + C*alpha1)/(1+exp(x + C*alpha1))))
  # Return the one giving closest mean prob to mean_prob
  return(alpha0_list[which.min(abs(mean_prob - mean_prob_list))])
}

sampling_gamma0t <- function(educ,U,gamma1,gamma2,mean_prob_t){
  gamma0t = c()
  # for each gamma0
  for (mean_prob in mean_prob_t) {
    # Wide range of gamma0
    gamma0_list = seq(-10,10,by=0.5)
    # Get mean prob of education according to each gamma0
    mean_prob_list = apply(as.matrix(gamma0_list),1,function(x) mean(exp(x + educ*gamma1 + U*gamma2)/(1+exp(x + educ*gamma1 + U*gamma2))))
    # Find gamma0 such that mean prob is closest to mean_prob
    gamma0 = gamma0_list[which.min(abs(mean_prob - mean_prob_list))]
    # Narrow down range of alpha0
    gamma0_list = seq(gamma0-.25,gamma0+.25,by=0.03)
    # Get mean prob of education according to each alpha0
    mean_prob_list = apply(as.matrix(gamma0_list),1,function(x) mean(exp(x + educ*gamma1 + U*gamma2)/(1+exp(x + educ*gamma1 + U*gamma2))))
    gamma0t = c(gamma0t,gamma0_list[which.min(abs(mean_prob - mean_prob_list))])
    # print(gamma0_list[which.min(abs(mean_prob - mean_prob_list))])
  }
  return(gamma0t)
}

# Download 2010 census stat
temp = tempfile(fileext = ".xlsx")
download.file('https://www2.census.gov/programs-surveys/popest/tables/2010-2019/national/asrh/nc-est2019-syasexn.xlsx', destfile=temp, mode='wb')
census = readxl::read_excel(temp, sheet =1)[5:105,1:2]
names(census) = c("Age","Total.Population")
# Proportion of ppl older than age 55, 56, ..., 90 among population of 50+
census %<>% mutate(Age = 0:100, Total.Population = as.numeric(Total.Population)) %<>%
  filter(Age >= 50) %<>%
  mutate(percentage = Total.Population/sum(Total.Population)) %<>%
  mutate(cum_percentage = rev(cumsum(rev(percentage)))) %<>%
  filter(Age >= 55 & Age <=90)


data_gen <- function(N,n,seed,alpha1,gamma1,gamma2,beta00,beta10,beta01,beta11,beta02,beta12,
                     beta03,beta13,beta04,sigma,sigma0,sigma1,sigma01,visit_num="fixed"){
  # Random seed
  set.seed(seed)
  # Generate id
  cohort.hypo = data.frame(id = paste0(615,1:N))
  # Simulate U & C 
  cohort.hypo %<>% mutate(U = rnorm(N,0,1)) %<>% 
    mutate(C = rnorm(N,0,1)) 
  # Estimate alpha0 such that mean education = 0.6
  alpha0 = sampling_alpha0(cohort.hypo$C,alpha1,mean_prob = 0.6)
  # Simulate education (0=HS+, 1=HS-)
  cohort.hypo %<>% mutate(educ = 1-rbinom(N,1, exp(alpha0+C*alpha1)/(1+exp(alpha0+C*alpha1)))) 
  # Simulate birth cohort
  cohort.hypo %<>% mutate(birth_year = round(runif(N, min=1919.5, max=1955.5))) %<>%
    mutate(baseline_age = 2010-birth_year)
  
  # Estimate gamma0t according to different t (age)
  gamma0t_list = data.frame(Age = census$Age,gamma0t = sampling_gamma0t(cohort.hypo$educ,cohort.hypo$U,gamma1,gamma2,census$cum_percentage))
  
  # Simulate survival prob & vital status (1=alive,0=dead)
  cohort.hypo = left_join(cohort.hypo,gamma0t_list,by = c("baseline_age" = "Age"))
  cohort.hypo %<>% mutate(survival_prob = exp(gamma0t+educ*gamma1+U*gamma2)/(1+exp(gamma0t+educ*gamma1+U*gamma2))) %<>%
    mutate(alive = rbinom(N,1, survival_prob))
  
  # Check survival prob by age
  # cohort.hypo %>% group_by(baseline_age) %>% summarise(total=n(),total_alive = sum(alive), mean = mean(alive)) %>% as.data.frame()
  
  # Alive id
  alive_id = (cohort.hypo %>% filter(alive==1) %>% select(id) %>% as.data.frame())$id
  # Sample id 
  final_id = sample(alive_id,n)
  # Create analytical data
  data = cohort.hypo %>% filter(id %in% final_id)  %>% select(id:baseline_age)
  # Check distribution in analytical data
  # data %>% summarise(mean_U = mean(U), mean_C = mean(C), mean_educ = mean(educ))
  # data %>% group_by(baseline_age) %>% summarise(n=n()) %>% as.data.frame()
 if(visit_num=="fixed"){
   # Visit number: maximum and minimum number of possible observations
   minv = 1
   maxv = 10
   # Simulate number of observations for each individual
   nv = rep(maxv,n)
 } else if(visit_num=="random"){
   # Visit number: maximum and minimum number of possible observations
   minv = 1
   maxv = 10
   # Simulate number of observations for each individual
   nv = round(runif(n,minv,maxv))
 }
  # Simulate individual visits  (don't have to be 1,2,3,4. can be 1,3,6,9. assume everybody has 1st obs)
  wave = as.numeric(unlist(sapply(nv, function(x) c(1, sort(sample(2:maxv, x-1, replace=FALSE))))))
  
  # Set up long format
  data_long = data.frame(id=rep(data$id, times=nv), wave=wave)
  data_long %<>% mutate(pe = ifelse(wave==1,0,1))
  # Simulate iid errors
  data_long %<>% mutate(err = rnorm(nrow(data_long),0,sigma))
  
  
  # Simulate (correlated) individual random effects for intercepts and slopes
  ## Covariance matrix 
  S1 = diag(c(sigma0, sigma1)) %*% matrix(c(1, sigma01, sigma01, 1), nrow=2) %*% diag(c(sigma0, sigma1))
  ## Simulate realization of individual random effects for intercept and slope
  U1 = mvrnorm(n, mu=c(0,0), Sigma=S1)
  
  data %<>% mutate(bi0 = U1[,1], bi1 = U1[,2])
  
  
  # Simulate Yij
  data_long = left_join(data_long,data,by='id')
  data_long %<>% mutate(t = wave-1, age = t + baseline_age) %<>%
    mutate(cohort = as.numeric(cut(baseline_age, c(55,60,65,70,75,80,85,90.1), right=FALSE))) %<>%
    mutate(Y = (beta00 + bi0) + beta01*educ + beta02*U + beta03*C + beta04*pe + # beta05[cohort] + 
             (beta10 + bi1 + beta11*educ + beta12*U + beta13*C)*age + err) 
  
  return(list(data=data,
              data_long=data_long,
              true_par = list(N=N,n=n,seed=seed,alpha1=alpha1,gamma1=gamma1,gamma2=gamma2,beta00=beta00,beta10=beta10,
                              beta01=beta01,beta11=beta11,beta02=beta02,beta12=beta12,beta03=beta03,beta13=beta13,
                              beta04=beta04,sigma=sigma,sigma0=sigma0,sigma1=sigma1,sigma01=sigma01,visit_num=visit_num,
                              alpha0=alpha0,gamma0t_list=gamma0t_list)))
}















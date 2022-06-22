
source('data_gen.R')


N = 50000
n = 2000
alpha1 = log(2)
gamma1 = -log(2)
gamma2 = -log(2)
beta00 = 0         # intercept 
beta10 = -0.05     # age effect 
beta01 = -0.05     # education baseline effect 
beta11 = -0.05     # education X age 
beta02 = -0.005    # U baseline effect
beta12 = -0.005    # U X age 
beta03 = -0.005    # C baseline effect
beta13 = -0.005    # C X age 
beta04 = 0.05      # practice effect- indicator (0,1,1,1,...)
sigma = sqrt(0.7)  # True error SD
sigma0  = sqrt(0.2)             # sd for random intercept
sigma1  = sqrt(0.005)           # sd for random slope
sigma01 = 0.01/sigma0/sigma1    # correlation between random intercept and slope. +ve: larger baseline cog, smaller rate of decline
visit_num='fixed'

B = 1000


r1 = r2 = r3 = r4 = list(time_C=data.frame(beta01=0,beta11=0,beta10=0,blage=0),
                         age_C=data.frame(beta01=0,beta11=0,beta10=0,blage=0),
                         agebl_C=data.frame(beta01=0,beta11=0,beta10=0,blage=0),
                         time_P=data.frame(beta01=0,beta11=0,beta10=0,blage=0),
                         age_P=data.frame(beta01=0,beta11=0,beta10=0,blage=0),
                         agebl_P=data.frame(beta01=0,beta11=0,beta10=0,blage=0),
                         time_CP=data.frame(beta01=0,beta11=0,beta10=0,blage=0),
                         age_CP=data.frame(beta01=0,beta11=0,beta10=0,blage=0),
                         agebl_CP=data.frame(beta01=0,beta11=0,beta10=0,blage=0))


for (i in 1:B) {
  print(paste("Iteration ",i,"/",B," starts!",sep = ""))
  time1 = Sys.time()
  seed = i
  # Scenario 1: base model/no bias
  d1 = data_gen(N=N,n=n,seed=seed,alpha1=0,gamma1=gamma1,gamma2=0,beta00=beta00,beta10=beta10,
                beta01=beta01,beta11=beta11,beta02=beta02,beta12=beta12,beta03=0,beta13=0,
                beta04=beta04,sigma=sigma,sigma0=sigma0,sigma1=sigma1,sigma01=sigma01,visit_num=visit_num)
  
  mod1 = lmer(Y ~ t*educ + baseline_age + C + (1+t|id), data=d1$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod2 = lmer(Y ~ age*educ+ C + (1+age|id), data=d1$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod3 = lmer(Y ~ age*educ + baseline_age + C + (1+age|id), data=d1$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  
  mod4 = lmer(Y ~ t*educ + baseline_age + pe + (1+t|id), data=d1$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod5 = lmer(Y ~ age*educ + pe + (1+age|id), data=d1$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod6 = lmer(Y ~ age*educ + baseline_age + pe + (1+age|id), data=d1$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  
  mod7 = lmer(Y ~ t*educ + baseline_age + C + pe + (1+t|id), data=d1$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod8 = lmer(Y ~ age*educ+ C + pe + (1+age|id), data=d1$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod9 = lmer(Y ~ age*educ + baseline_age + C + pe + (1+age|id), data=d1$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  
  r1$time_C[i,] = summary(mod1)$coef[c('educ','t:educ','t','baseline_age'),'Estimate']
  r1$age_C[i,] = c(summary(mod2)$coef[c('educ','age:educ','age'),'Estimate'],0)
  r1$agebl_C[i,] = summary(mod3)$coef[c('educ','age:educ','age','baseline_age'),'Estimate']
  
  r1$time_P[i,] = summary(mod4)$coef[c('educ','t:educ','t','baseline_age'),'Estimate']
  r1$age_P[i,] = c(summary(mod5)$coef[c('educ','age:educ','age'),'Estimate'],0)
  r1$agebl_P[i,] = summary(mod6)$coef[c('educ','age:educ','age','baseline_age'),'Estimate']
  
  r1$time_CP[i,] = summary(mod7)$coef[c('educ','t:educ','t','baseline_age'),'Estimate']
  r1$age_CP[i,] = c(summary(mod8)$coef[c('educ','age:educ','age'),'Estimate'],0)
  r1$agebl_CP[i,] = summary(mod9)$coef[c('educ','age:educ','age','baseline_age'),'Estimate']
  
  
  
  
  # Scenario 2: selection bias
  d2 = data_gen(N=N,n=n,seed=seed,alpha1=0,gamma1=gamma1,gamma2=gamma2,beta00=beta00,beta10=beta10,
                beta01=beta01,beta11=beta11,beta02=beta02,beta12=beta12,beta03=0,beta13=0,
                beta04=beta04,sigma=sigma,sigma0=sigma0,sigma1=sigma1,sigma01=sigma01,visit_num=visit_num)
  
  mod1 = lmer(Y ~ t*educ + baseline_age + C + (1+t|id), data=d2$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod2 = lmer(Y ~ age*educ+ C + (1+age|id), data=d2$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod3 = lmer(Y ~ age*educ + baseline_age + C + (1+age|id), data=d2$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  
  mod4 = lmer(Y ~ t*educ + baseline_age + pe + (1+t|id), data=d2$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod5 = lmer(Y ~ age*educ + pe + (1+age|id), data=d2$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod6 = lmer(Y ~ age*educ + baseline_age + pe + (1+age|id), data=d2$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  
  mod7 = lmer(Y ~ t*educ + baseline_age + C + pe + (1+t|id), data=d2$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod8 = lmer(Y ~ age*educ+ C + pe + (1+age|id), data=d2$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  mod9 = lmer(Y ~ age*educ + baseline_age + C + pe + (1+age|id), data=d2$data_long, REML = F,control = lmerControl(optimizer ="Nelder_Mead"))
  
  r2$time_C[i,] = summary(mod1)$coef[c('educ','t:educ','t','baseline_age'),'Estimate']
  r2$age_C[i,] = c(summary(mod2)$coef[c('educ','age:educ','age'),'Estimate'],0)
  r2$agebl_C[i,] = summary(mod3)$coef[c('educ','age:educ','age','baseline_age'),'Estimate']
  
  r2$time_P[i,] = summary(mod4)$coef[c('educ','t:educ','t','baseline_age'),'Estimate']
  r2$age_P[i,] = c(summary(mod5)$coef[c('educ','age:educ','age'),'Estimate'],0)
  r2$agebl_P[i,] = summary(mod6)$coef[c('educ','age:educ','age','baseline_age'),'Estimate']
  
  r2$time_CP[i,] = summary(mod7)$coef[c('educ','t:educ','t','baseline_age'),'Estimate']
  r2$age_CP[i,] = c(summary(mod8)$coef[c('educ','age:educ','age'),'Estimate'],0)
  r2$agebl_CP[i,] = summary(mod9)$coef[c('educ','age:educ','age','baseline_age'),'Estimate']
  
  
  time2 = Sys.time()
  print(paste("End of iteration ",i,'. Time used = ',round(as.numeric(time2-time1),2),' min.',sep=''))
  
}

result = list(beta01=beta01,beta11=beta11,beta10=beta10,r1=r1,r2=r2)

save(result,file = "result.RData")





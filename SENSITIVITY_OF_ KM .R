



#rm(list=ls())


library(survival)
library(km.ci)


gen.func <- function(n, s, t, M, METHOD){
  ##### Function to generate n failure times
  ##### Exponential distribution with s, survival after t months
  ##### Median follow-up time = M
  ##### Time grid [0,T]
  #s=0.95;n=246;t=60;M=100;METHOD='rothman'
  
  lambda  <- - log(s)/t                         ### parameter of the exponential function
  temps    <- rexp(n,lambda)                    ### we generate n random values according to the law exp
  status  <- rep(1,length.out=n)                ### line identity
  c       <- runif(n,0,2*M)                     ### we assign n random values according to the uniform law
  status  <- as.numeric(temps<=c)               ### if event, we give the value 1, otherwise the value 0
  temps   <- pmin(temps,c)                      
  ### we keep the smallest value between t_i and c_i
  EST.1<-data.frame(temps,status)               ### we create a data frame wich 
  EST<-Surv(temps,status)
  EST<-survfit(EST~1)
  EST<-km.ci(survi = EST,conf.level = .95,method = METHOD)
  
  
  temps<-summary(EST)$time                      ### times column
  
  if (any(temps<=t)){
    
    to <- max(temps[temps<=t])                      ### t0 is the time immediately above t
    
    su<-summary(EST)$surv[temps==to]                ###  survival rate at t_0
    I1<-summary(EST)$lower[temps==to]               ###  lower limit of the confidence interval for the estimation of real survival on
    I2<-summary(EST)$upper[temps==to]               ###  upper limit of the confidence interval for the estimation of real survival su
  }else su<-I1<-I2<-1
  
  cpt <- as.numeric(I1<=s & s<=I2)                ###  cpt = 1, if the real survival rate is in [I1, I2]
  cpt2<- as.numeric(s<=I1 | I2<=s)                ###  cpt2 = 1, if the real survival rate is not in [I1, I2]
  L=I2-I1                                         ###  length of the confidence interval
  u<-c(vrai=cpt,faux=cpt2,survie=su,inf=I1,sup=I2,longueur=L)  
  return(u)
}

simul<-function(N,n,s,t,M,METHOD){
  ### This function allow to create a table, N row and 6 colomn,
  ### repeate the previous function N times.
  
  w<-c(0,0,0,0,0,0)                  ###   null vector             
  for (i in 1:N) {
    k <-gen.func(n,s,t,M,METHOD)         
    w<-rbind(w,k)                   ### allows to combine the different vectors generate by the function gen.func
  }
  w<-w[-1,]                        ### remove the first line (Null line)
  return(w)
}

tableau.fonct<-function(N,n,s,t,MEDIANE,METHOD){
  ### for each value of the median follow-up,
  ### we create an array containing N replication of survival data,
  ### we calculate the mean of each column
  
  h<-array(NA,dim=c(length(MEDIANE),5,2),dimnames = list(MEDIANE,c("couv","S","IC_inf","IC_sup","precision"),c("moyenne","sd")))
  i<-1
  for (M in MEDIANE) {
    tab<-simul(N,n,s,t,M,METHOD)     ### creating a table
    h[i,1,1]<-mean(tab[,1], na.rm = TRUE)        ### coverage rate for follow-up median=M
    h[i,2,1]<-mean(tab[,3], na.rm = TRUE)
    h[i,3,1]<-mean(tab[,4], na.rm = TRUE)     
    h[i,4,1]<-mean(tab[,5], na.rm = TRUE)    
    h[i,5,1]<-mean(tab[,6], na.rm = TRUE)    ### Length of the interval for median of follow-up = M
    
    h[i,1,2]<-sd(tab[,1], na.rm = TRUE)      ### coverage rate for follow-up median=M
    h[i,2,2]<-sd(tab[,3], na.rm = TRUE)
    h[i,3,2]<-sd(tab[,4], na.rm = TRUE)     
    h[i,4,2]<-sd(tab[,5], na.rm = TRUE)    
    h[i,5,2]<-sd(tab[,6], na.rm = TRUE)      ### Length of the interval for median of follow-up = M
    i<-i+1 
  }
  return(h)
}


DONNEE2105<-tableau.fonct(N=10000,n=246,s=0.10,t=60,MEDIANE=seq(0,160,1),METHOD='loglog')      ### Data simulation
save(DONNEE2105,file="D:/program_R/program11/Data2105.Rdata")

DONNEE2205<-tableau.fonct(N=10000,n=246,s=0.50,t=60,MEDIANE=seq(0,160,1),METHOD='loglog')      ### Data simulation
save(DONNEE2205,file="D:/program_R/program11/Data2205.Rdata")


DONNEE2305<-tableau.fonct(N=10000,n=246,s=0.05,t=60,MEDIANE=seq(0,160,1),METHOD='loglog')      ### Data simulation
save(DONNEE2305,file="D:/program_R/program11/Data2305.Rdata")

DONNEE02<-tableau.fonct(N=10000,n=246,s=0.95,t=30,MEDIANE=seq(0,100,1),METHOD='loglog')      ### Data simulation
save(DONNEE02,file="D:/program_R/program11/Data30.Rdata")


DONNEE03<-tableau.fonct(N=10000,n=246,s=0.95,t=45,MEDIANE=seq(0,100,1),METHOD='loglog')      ### Data simulation
save(DONNEE03,file="D:/program_R/program11/Data45.Rdata")


DONNEE04<-tableau.fonct(N=10000,n=246,s=0.95,t=50,MEDIANE=seq(0,100,1),METHOD='loglog')      ### Data simulation
save(DONNEE04,file="D:/program_R/program11/Data50.Rdata")


DONNEE05<-tableau.fonct(N=10000,n=1000,s=0.95,t=60,MEDIANE=seq(0,160,1),METHOD='loglog')      ### Data simulation
save(DONNEE05,file="D:/program_R/program11/Data1000.Rdata")

DONNEE1005<-tableau.fonct(N=10000,n=246,s=0.95,t=2,MEDIANE=seq(0,8,0.1),METHOD='loglog')      ### Data simulation
save(DONNEE1005,file="D:/program_R/program11/Data1005.Rdata")



DONNEE_jr3<-tableau.fonct(N=10000,n=246,s=0.95,t=2,MEDIANE=seq(0,8,0.2),METHOD='loglog')      ### Data simulation
save(DONNEE_jr3,file="D:/program_R/program11/Data_jr3.Rdata")


DONNEE_jrs<-tableau.fonct(N=10000,n=7072 ,s=0.972,t=2.1,MEDIANE=seq(0,5,0.1),METHOD='loglog')      ### Data simulation
save(DONNEE_jrs,file="D:/program_R/program11/Data_jrs.Rdata")

DONNEE_jrs4<-tableau.fonct(N=10000,n=246 ,s=0.972,t=2.1,MEDIANE=seq(0,5,0.1),METHOD='loglog')      ### Data simulation
save(DONNEE_jrs4,file="D:/program_R/program11/Data_jrs4.Rdata")


 DONNEE_PETO<-tableau.fonct(N=10000,n=246,s=0.95,t=60,MEDIANE=seq(0,170,1),METHOD='peto')          ### Data simulation
save(DONNEE_PETO,file="D:/program_R/program11/Data_PETO.Rdata")

DONNEE_LINEAR<-tableau.fonct(N=10000,n=246,s=0.95,t=60,MEDIANE=seq(0,170,1),METHOD='linear')      ### Data simulation
save(DONNEE_LINEAR,file="D:/program_R/program11/Data_LINEAR.Rdata")

DONNEE_LOG<-tableau.fonct(N=10000,n=246,s=0.95,t=60,MEDIANE=seq(0,170,1),METHOD='log')            ### Data simulation
save(DONNEE_LOG,file="D:/program_R/program11/Data1_LOG.Rdata")

DONNEE_LOGLOG<-tableau.fonct(N=10000,n=246,s=0.95,t=60,MEDIANE=seq(0,170,1),METHOD='loglog')      ### Data simulation
save(DONNEE_LOGLOG,file="D:/program_R/program11/Data_LOGLOG.Rdata")

DONNEE_ROTHMAN<-tableau.fonct(N=10000,n=246,s=0.95,t=60,MEDIANE=seq(0,170,1),METHOD='rothman')    ### Data simulation
save(DONNEE_ROTHMAN,file="D:/program_R/program11/Data_ROTHMAN.Rdata")


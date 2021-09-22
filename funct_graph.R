

rm(list = ls())

setwd("E:/INStech/Données")
micro=read.table("microscopy.txt",header=T)
summary(micro)
str(micro)
attach(micro)


traitement <- status

######### Infection###
model0 <- glm(infection~1)
model1<-glm(infection~relevel(traitement,ref = "DMSO")*(period + isolate)+microsporidies,family="binomial")
model.step <- step(model0, scope=list(upper=model1,lower=model0),direction = "forward",family="binomial")
summary(model.step)

best01 = glm(formula = infection ~ period + isolate + microsporidies + 
      relevel(traitement, ref = "DMSO") + isolate:relevel(traitement, 
                                                          ref = "DMSO"),binomial())
summary(best01)
library(glmulti)
mod <- glmulti(infection~status+period+isolate+microsporidies,data=micro,family=binomial())
Best_model= glm(infection~relevel(traitement, ref = "DMSO")+period+isolate+microsporidies+isolate:period+isolate:relevel(traitement, ref = "DMSO"),binomial())
summary(Best_model)


modell=MASS::glm.nb(formula = oocyst ~ isolate + period + microsporidies + 
               relevel(status, ref = "DMSO") + isolate:relevel(status, ref = "DMSO")) 
             init.theta = 0.207420713, link = log)
summary(modell)

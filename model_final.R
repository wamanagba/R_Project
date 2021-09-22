

############ Infection

Best_model<- glm(infection~period+isolate+relevel(trmt,ref = "DMSO")+microsporidies+relevel(trmt,ref = "DMSO"):isolate,binomial())
summary(Best_model)
glmmPQL3 <- glmmPQL(infection~period+relevel(status,ref = "DMSO")+microsporidies, random=~1|isolate,family =binomial())
summary(glmmPQL3)
confint.default(Best_model)
exp(confint.default(Best_model))
1-exp(confint.default(Best_model))

############oocysts
Best_model01=MASS::glm.nb(formula = oocyst ~ isolate + period + microsporidies + 
               relevel(status, ref = "DMSO") + isolate:relevel(status, ref = "DMSO"), 
             init.theta = 0.207420713, link = log)
summary(Best_model01)

confint.default(Best_model01)
exp(confint.default(Best_model01))
1-exp(confint.default(Best_model01))

############# gorgement
Best_model3 <- glm(y ~ relevel(traitements,ref ="DMSO")+period+especes + isolats,family =  quasibinomial)
test(Best_model3)
summary(Best_model3)
confint.default(Best_model3)
exp(confint.default(Best_model3))
1-exp(confint.default(Best_model3))
################# glmm
trt= traitements
library(MASS)
glmmPQL1 <- glmmPQL(y~ relevel(trt,ref ="DMSO")+period+especes, random=~1|isolats, family=quasibinomial, data=dta)

summary(glmmPQL1)
exp(confint.default(glmmPQL1))

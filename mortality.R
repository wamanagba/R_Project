

rm(list = ls())

setwd("E:/INStech/Données")
mrt=read.table("mortality.txt",header=T)
summary(mrt)
str(mrt)
attach(mrt)
micro <- na.omit(mrt)
View(micro)
View(P1_AZA)

P1 <- subset(mrt, isolate=="P1")
P2 <- subset(mrt, isolate=="P2")
P3 <- subset(mrt, isolate=="P3")
P4 <- subset(mrt, isolate=="P4")
P5 <- subset(mrt, isolate=="P5")
P6 <- subset(mrt, isolate=="P6")


table01 <- table(trmt,isolate)
write.xlsx (table01, file = "F:/INStech/Texte/mortality/tab01.xlsx")
write.xlsx (micro, file = "F:/INStech/Texte/mortality/micro.xlsx")

d <- subset(d,treatment!=x[cpt])

DMSO_CTRL <- subset(micro,trmt!="RF")
DMSO_CTRL <- subset(DMSO_CTRL,trmt!="PUF")
View(DMSO_CTRL)
fit <- survfit(Surv(dpi, status) ~ trmt, data = DMSO_CTRL)
x11();ggsurvplot(fit,pval = T, data = DMSO_CTRL)
library(survival)
library("survminer")
require("survival")
fit <- survfit(Surv(dpi, status) ~ period, data = micro)
x11();ggsurvplot(fit,pval = T, data = micro)

setwd("E:/INStech/Texte/mortality")

x11();ggsurvplot(
  fit, 
  data = micro, 
  size = 1,                 # change line size
  palette = c(1,2,3,4,5,6,7),# custom color palettes
  #conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  #legend.labs = c("P1"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
)
#########################

fit0 <- survfit(Surv(dpi, status) ~ isolate, data = micro)
x11();ggsurvplot(fit0, data = micro,surv.median.line= "hv",pval = T,palette = c(1,2,3,4,5,6,7))
tmt=treatment
fit1 <- survfit(Surv(dpi, status) ~ trmt, data = P1)
x11();ggsurvplot(fit1, data = P1,surv.median.line= "hv",pval = T,palette = c(1,2,3,4,5,6,7))

surv_funct <- function(d,i){
  x <- c('AZA', 'DE','PA','PUF','RF')
  x= x[-i]
  for (cpt in 1:4){
    d <- subset(d,trmt!=x[cpt])
  }
  fit <- survfit(Surv(dpi, status) ~ trmt, data = d)
  d=ggsurvplot(fit, data = d,pval = T,legend = c(0.81, 0.78))
  return(d)
}
surv_plot = function(G){
  x=list()
  x[[1]]= surv_funct(G,1)
  x[[2]]= surv_funct(G,2)
  x[[3]]= surv_funct(G,3)
  x[[4]]= surv_funct(G,4)
  x[[5]]= surv_funct(G,5)
  
  xx=arrange_ggsurvplots ( x , print  =  T ,cex = 0.1 , ncol  =  2,nrow = 3,label_size=8,title = "isolate P6")
  return(xx)
}


surv_plot(P1)
surv_plot(P2)
surv_plot(P3)
surv_plot(P4)
surv_plot(P5)
surv_plot(P6)

####################




########  Modeles

mod0 <- survreg(Surv(dpi,status)~ relevel(treatment,ref="DMSO"))
mod1 <- survreg(Surv(dpi,status)~ relevel(treatment,ref="DMSO")*(period+isolate))
model.step <- step(mod0, scope=list(upper=mod1,lower=mod0),direction = "forward")

summary(model.step)


library(survival)
mod0 <- coxph(Surv(dpi,status)~ relevel(trmt,ref="DMSO"))
mod1 <- coxph(Surv(dpi,status)~ relevel(trmt,ref="DMSO")*(period+isolate))
model.step <- step(mod0, scope=list(upper=mod1,lower=mod0),direction = "forward")

model.final <- coxph(Surv(dpi, status) ~ relevel(trmt, ref = "CTRL") + isolate)
summary(model.final)


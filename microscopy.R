
rm(list = ls())

setwd("E:/INStech/Données")
micro=read.table("microscopy.txt",header=T)
summary(micro)
str(micro)
attach(micro)
micro <- na.omit(micro)
p1 <- subset(micro,isolate=="P1")
p2 <- subset(micro,isolate=="P2")
p3 <- subset(micro,isolate=="P3")
p4 <- subset(micro,isolate=="P4")
p5 <- subset(micro,isolate=="P5")
p6 <- subset(micro,isolate=="P6")

micro2 <-subset(micro,oocyst>0);View(micro2)
p12 <- subset(micro2,isolate=="P1")
p22 <- subset(micro2,isolate=="P2")
p32 <- subset(micro2,isolate=="P3")
p42 <- subset(micro2,isolate=="P4")
p52 <- subset(micro2,isolate=="P5")
p62 <- subset(micro2,isolate=="P6")
library(tidyverse)
library(car)
library(Rmisc)
library(multcomp)
library(survminer,quietly = TRUE)
library(Rmisc)



        #################### Quelques figures #############
##########################################################################
ooc <- summarySE(micro2,measurevar="oocyst",groupvars=c("status","period","isolate"))
library(xlsx)
write.xlsx (ooc, file = "E:/INStech/Texte/microscopy/oocyst.xlsx")

ooc_P1 <- summarySE(p12,measurevar="oocyst",groupvars=c("status","period"))
write.xlsx (ooc_P1, file = "F:/INStech/Texte/microscopy/ooc_P1.xlsx")

ooc_P2 <- summarySE(p22,measurevar="oocyst",groupvars=c("status","period"))
write.xlsx (ooc_P2, file = "F:/INStech/Texte/microscopy/ooc_P2.xlsx")

ooc_P3 <- summarySE(p32,measurevar="oocyst",groupvars=c("status","period"))
write.xlsx (ooc_P3, file = "F:/INStech/Texte/microscopy/ooc_P3.xlsx")

ooc_P4 <- summarySE(p42,measurevar="oocyst",groupvars=c("status","period"))
write.xlsx (ooc_P4, file = "F:/INStech/Texte/microscopy/ooc_P4.xlsx")

ooc_P5 <- summarySE(p52,measurevar="oocyst",groupvars=c("status","period"))
write.xlsx (ooc_P5, file = "F:/INStech/Texte/microscopy/ooc_P5.xlsx")

ooc_P6 <- summarySE(p62,measurevar="oocyst",groupvars=c("status","period"))
write.xlsx (ooc_P6, file = "F:/INStech/Texte/microscopy/ooc_P6.xlsx")

inf <- summarySE(micro,measurevar="infection",groupvars=c("status","period"))
write.xlsx (inf, file = "F:/INStech/Texte/microscopy/infection.xlsx")

inf_P1 <- summarySE(p1,measurevar="infection",groupvars=c("status","period"))
write.xlsx (inf_P1, file = "F:/INStech/Texte/microscopy/inf_P1.xlsx")

inf_P2 <- summarySE(p2,measurevar="infection",groupvars=c("status","period"))
write.xlsx (inf_P2, file = "F:/INStech/Texte/microscopy/inf_P2.xlsx")

inf_P3 <- summarySE(p3,measurevar="infection",groupvars=c("status","period"))
write.xlsx (inf_P3, file = "F:/INStech/Texte/microscopy/inf_P3.xlsx")

inf_P4 <- summarySE(p4,measurevar="infection",groupvars=c("status","period"))
write.xlsx (inf_P4, file = "F:/INStech/Texte/microscopy/inf_P4.xlsx")

inf_P5 <- summarySE(p5,measurevar="infection",groupvars=c("status","period"))
write.xlsx (inf_P5, file = "F:/INStech/Texte/microscopy/inf_P5.xlsx")

inf_P6 <- summarySE(p6,measurevar="infection",groupvars=c("status","period"))
write.xlsx (inf_P6, file = "F:/INStech/Texte/microscopy/inf_P6.xlsx")

figure_inf.oocy_1 <-function(inf,ooc){
  p <- position_dodge(0.1) 
  x <-ggplot(inf, aes(x=status, y=infection, colour=period, group=period)) + 
      geom_errorbar(aes(ymin=infection-ci, ymax=infection+ci), width=.1, position=p) +
      geom_line(position=p, size=2) +
      geom_point(position=p, size=1)+
      ylab("Taux d'infection")+
      scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
      scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
      theme_classic()

  y <-ggplot(ooc, aes(x=status, y=oocyst, colour=period, group=period)) + 
          geom_errorbar(aes(ymin=oocyst-ci, ymax=oocyst+ci), width=.1, position=p) +
          geom_line(position=p, size=2) +
          geom_point(position=p, size=1)+
          ylab("Charge parasitaire")+
          scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
          scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
          theme_classic()

  library(cowplot)
  xy <- plot_grid(x, y,labels = c("Infection","oocyst"), ncol = 2, nrow = 1)
  return(xy)
}
figure_inf.oocy_2 <- function(inf,ooc){
  
    y <- ggplot(ooc, aes(x=period, y=oocyst, fill=status)) + 
            geom_bar(stat = "identity", color="black", position=position_dodge())+
            geom_errorbar(aes(ymin=oocyst, ymax=oocyst+se),width=.2,  position=position_dodge(.9))+
            xlab("Periode")+
            ylab("Charge parasitaire")+
            coord_cartesian(ylim = c(0, 5))+
            scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
            theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
            theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
            theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
            theme(axis.line = element_line(colour = "black"))+
            theme(legend.position = c(0.75, 0.8))+
            theme(legend.title = element_blank())+
            theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))
    
  x <- ggplot(inf, aes(x=period, y=infection, fill=status)) + 
    geom_bar(stat = "identity", color="black", position=position_dodge())+
    geom_errorbar(aes(ymin=infection, ymax=infection+se),width=.2,  position=position_dodge(.9))+
    xlab("Periode")+
    ylab("Taux d'infection")+
    #coord_cartesian(ylim = c(0, 1.25))+
    scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
    theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
    theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
    theme(axis.line = element_line(colour = "black"))+
    theme(legend.position = c(0.75, 0.8))+
    theme(legend.title = element_blank())+
    theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))
    
  xy <- plot_grid(x, y,labels = c("Infection","oocyst"), ncol = 2, nrow = 1)
  return(xy)
  }

setwd("E:/INStech/Texte/microscopy")

x11();figure_inf.oocy_1(inf,ooc)
x11();figure_inf.oocy_1(inf_P1,ooc_P1)
x11();figure_inf.oocy_1(inf_P2,ooc_P2)
x11();figure_inf.oocy_1(inf_P3,ooc_P3)
x11();figure_inf.oocy_1(inf_P4,ooc_P4)
x11();figure_inf.oocy_1(inf_P5,ooc_P5)
x11();figure_inf.oocy_1(inf_P6,ooc_P6)

x11();figure_inf.oocy_2(inf,ooc)
x11();figure_inf.oocy_2(inf_P1,ooc_P1)
x11();figure_inf.oocy_2(inf_P2,ooc_P2)
x11();figure_inf.oocy_2(inf_P3,ooc_P3)
x11();figure_inf.oocy_2(inf_P4,ooc_P4)
x11();figure_inf.oocy_2(inf_P5,ooc_P5)
x11();figure_inf.oocy_2(inf_P6,ooc_P6)


 ################## Modele GLM ################
x <- status

######### Infection###

model1<-glm(infection~status*period * isolate+microsporidies,family="binomial")
summary(model1)
model2<-glm(infection~ relevel(x,ref = "DMSO")*(period +isolate)+microsporidies,family="binomial")
anova(model1,model2,test="Chi")
summary(model2)
x11();gplots::textplot( capture.output(summary(model2)), valign="top",cex = 0.5)

model3<-glm(infection~ status*isolate+period+microsporidies,family="binomial")
anova(model2,model3,test="Chi")
summary(model3)
model4<-glm(infection~ status*period+isolate+microsporidies,family="binomial")
anova(model2,model4,test="Chi")


####### Oocyst###
x <- status
mod1<-glm(oocyst~status*period*isolate+microsporidies+infection,family="poisson")
summary(mod1)
mod2<-glm(oocyst~relevel(x,ref = "DMSO")*(period+isolate)+microsporidies,family="poisson")
anova(mod1,mod2,test="Chi")

summary(mod2)
mod3<-glm(oocyst~relevel(x,ref = "DMSO")*period+isolate+microsporidies,family="poisson")
summary(mod3)


md1<-glm(microsporidies~status*period*isolate+infection,family="poisson")
summary(md1)
md2<-glm(microsporidies~status*isolate+infection+period,family="poisson")
anova(md1,md2,test="Chi")

x <- status
model1<-MASS::glm.nb(oocyst~relevel(x,ref = "DMSO")*period+isolate+microsporidies)
summary(model1)
library(gplots)
x11();gplots::textplot( capture.output(summary(model1)), valign="top",cex = 0.7)
OR(model1)
exp(model1$coefficients)

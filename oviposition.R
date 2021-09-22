

rm(list=ls())

#setwd("F:/INStech/Texte/oviposition/word")
setwd("E:/INStech/Données")
op=read.table("oviposition2.txt",header=T)
summary(op)
View(op)
attach(op)


library(dplyr)
dt <- dplyr::mutate(op,
                    Jours=case_when (dpi ==3 ~ "Jour_3",
                                     dpi ==4 ~ "Jour_4",
                                     dpi ==5 ~ "Jour_5",
                                     dpi ==6 ~ "Jour_6",
                                     dpi ==7 ~ "Jour_7",
                                     
                                     TRUE ~ as.character(dpi))) 

dt01 <- dplyr::mutate(dt,
                    isolat=case_when (carrier =="P1" ~ "GRP1",
                                      carrier =="P4" ~ "GRP1",
                                      carrier =="P5" ~ "GRP1",
                                      carrier =="P2" ~ "P2",
                                      carrier =="P3" ~ "P3",
                                     
                                     TRUE ~ as.character(carrier)))

grp00<-dt %>% 
  mutate(total_oeufs = as.numeric(eggs)) %>% 
  group_by(carrier,Jours) %>% 
  dplyr::summarise(
    total_oeufs = sum(total_oeufs)
  ) %>% 
  arrange((carrier))
data2 <- reshape::cast(grp00, carrier ~ Jours)
gg <- Rmisc::summarySE(dt,measurevar="eggs",groupvars=c("carrier", "Jours"))
gg01 <- Rmisc::summarySE(dt,measurevar="eggs",groupvars=c("carrier", "status"))
gg02 <- Rmisc::summarySE(dt01,measurevar="eggs",groupvars=c("isolat", "status"))

grp<-dt %>% 
  mutate(total_oeufs = as.numeric(eggs)) %>% 
  group_by(status,Jours) %>% 
  dplyr::summarise(
    total_oeufs = sum(total_oeufs)
    #Nb_moustiques = n()
  ) %>% 
  arrange((status))
data1 <- reshape::cast(grp, status ~ Jours)

grppp<-dt %>% 
  mutate(total_oeufs = as.numeric(eggs)) %>% 
  group_by(status) %>% 
  dplyr::summarise(
    total_oeufs = sum(total_oeufs),
    Nb_moustiques = n()
  ) %>% 
  arrange((status))
gpp <- Rmisc::summarySE(dt,measurevar="eggs",groupvars=c("status"))
x2 <- ggplot(gpp, aes(x=status, y=eggs, fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=eggs, ymax=eggs+se),width=.2,  position=position_dodge(.9))+
  xlab("Traitements")+
  ylab("Nombre d'oeufs pondus par moustiques")+
  #coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  #theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=11,colour="black"),axis.title=element_text(size=11))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.7))+
  theme(legend.title = element_blank())+
  theme_minimal()+
  #theme(legend.position = "none")+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


x3 <- ggplot(gg, aes(x=carrier, y=eggs, fill=Jours)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=eggs, ymax=eggs+se),width=.2,  position=position_dodge(.9))+
  xlab("Isolats")+
  ylab("Nombre d'oeufs pondus par moustiques")+
  #coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  #theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=11,colour="black"),axis.title=element_text(size=11))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.7))+
  theme(legend.title = element_blank())+
  theme_minimal()+
  #theme(legend.position = "none")+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))



x4 <- ggplot(gg02, aes(x=isolat, y=eggs, fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=eggs, ymax=eggs+se),width=.2,  position=position_dodge(.9))+
  xlab("Isolats")+
  ylab("Nombre d'oeufs pondus par moustiques")+
  #coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  #theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=11,colour="black"),axis.title=element_text(size=11))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.7))+
  theme(legend.title = element_blank())+
  theme_minimal()+
  #theme(legend.position = "none")+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

library(xlsx)
write.xlsx (data2, file = "F:/INStech/Texte/oviposition/word/df1.xlsx")
write.xlsx (data1, file = "F:/INStech/Texte/oviposition/word/df2.xlsx")
write.xlsx (oegg, file = "F:/INStech/Texte/oviposition/word/moyen.xlsx")
write.xlsx (gpp, file = "F:/INStech/Texte/oviposition/word/moyen_traitement.xlsx")
write.xlsx (gg, file = "F:/INStech/Texte/oviposition/word/isolat_jours.xlsx")
write.xlsx (gg01, file = "F:/INStech/Texte/oviposition/word/isolat_traitement.xlsx")
write.xlsx (gg02, file = "F:/INStech/Texte/oviposition/word/isolat_traitement02.xlsx")


setwd("F:/INStech/Texte/oviposition/word")

library(ggplot2)
x11();ggplot(op, aes(x=carrier, y=eggs, fill=carrier, colour=carrier))+
  geom_boxplot(alpha=0.5, outlier.alpha=0)+
  geom_jitter()+
  xlab("Isolats")+
  ylab("Nombres d'Oufs pondus")

x11();ggplot(op, aes(x=status, y=eggs, fill=status, colour=status))+
  geom_boxplot(alpha=0.5, outlier.alpha=0)+
  geom_jitter()+
  xlab("traitements")+
  ylab("Nombres d'Oufs pondus")



  

q <- ggplot(dt, aes(x=Jours, y=eggs)) + 
  geom_boxplot()
x11();q + geom_jitter(shape=16, position=position_jitter(0.2))+
       xlab("La date de ponte")+
       ylab("Nombres d'Oufs pondus")

summary(op)
test <- function(model){
  Dr <- model$deviance
  D0 <-model$null.deviance
  return((1-Dr/D0)*100)
}

model0 <- glm(eggs~status*carrier+dpi, family = poisson())
test(model)
summary(model)
mode1 <- MASS::glm.nb(eggs~relevel(x,ref = "DMSO")*carrier+dpi)
test(model)

##########
model2 <- MASS::glm.nb(eggs~relevel(x,ref = "DMSO")+carrier+dpi)
summary(model2)
############


anova(mode1,mode2)
model1 <- glm(eggs~status*(carrier+dpi), family = quasipoisson())
test(model1)
summary(model2)

model2 <- glm(eggs~status*carrier+dpi, family = quasipoisson())
model2.2 <- glm(eggs~status*dpi+carrier, family = quasipoisson())

test(model2)
summary(model2.2)
x11();gplots::textplot( capture.output(summary(model2)), valign="top",cex = 0.7)

anova(model1,model2,test="Chi") ## pas de difference statistiquement significative
model3 <- glm(eggs~status+carrier+dpi, family = quasipoisson())

anova(model2,model3,test="Chi") # Pas de difference
summary(model3)
test(model3)
x<- status
model3 <- glm(eggs~relevel(x,ref = "DMSO")+carrier+dpi, family = quasipoisson())
summary(model3)

x11();gplots::textplot( capture.output(summary(model3)), valign="top",cex = 0.7)






period_traitement <- function(op){
  
  tpp<-op %>% 
    # Création de deux nouvelles variables
    mutate(total_eggs = as.numeric(eggs)) %>% 
    #mutate(total_expose = as.numeric(exposes)) %>% 
    # on regroupe suivant le traitement et l'espèce
    group_by(status,carrier) %>% 
    dplyr::summarise(
      total_eggs = sum(eggs),
      #expose = sum(total_expose),
      
      Nb_gobelets = n()
    ) %>% 
    # Tri: croissant
    arrange((status))
  
  
  #View(grp4)
  library(Hmisc)
  xx1<-binconf(x= tpp1$total_eggs, n=tpp1$Nb_gobelets, alpha=0.05,
               method=c("exact"),
               include.x=T, include.n=T, return.df=T)
  
  names(xx1) <- c("total_gorge","expose","taux","Lower","Upper")
  
  tt1 <- merge(tpp1, xx1, by = c("total_gorge","expose"))
  #t1 <- t1[,-5]
  
  
  oegg <- Rmisc::summarySE(dt,measurevar="eggs",groupvars=c("status", "Jours"))
  
  tt1 <- arrange(tt1,traitements)
  return(tt1)
}
x1 <- ggplot(oegg, aes(x=status, y=eggs, fill=Jours)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=eggs, ymax=eggs+se),width=.2,  position=position_dodge(.9))+
  xlab("Traitements")+
  ylab("Nombre d'oeufs par moustiques")+
  #coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  #theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=11,colour="black"),axis.title=element_text(size=11))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.7))+
  theme(legend.title = element_blank())+
  theme_minimal()+
  #theme(legend.position = "none")+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

oegg <- Rmisc::summarySE(dt,measurevar="eggs",groupvars=c("status", "Jours"))
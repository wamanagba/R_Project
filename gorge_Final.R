###############                          ###############
###############    Traitement des donnes ###############
###############                          ###############
########################################################

rm(list=ls())
library(dplyr)


setwd("E:/INStech/Données")
f1=read.table("f1.txt",header=T)

#View(t1)
summary(f1)




func <- function(f){
  data <- c()
  for (cpt in 1:nrow(f)){
    d <- f1[cpt,]
    isolat     <- as.character(d[1])
    traitement <- as.character(d[2])
    s          <- "S"
    gorge      <- as.numeric(d[3])
    espose     <- as.numeric(d[5])
    non_gorge  <- as.numeric(d[5]-d[3])
    period     <- as.character(d[7])
    
    m  <- "M"
    gorge.m   <- as.numeric(d[4])
    espose.m  <- as.numeric(d[6])
    non_gorge.m  <- as.numeric(d[6]-d[4])
    
    d<- rbind(c(isolat,traitement,s,gorge,non_gorge,espose,period),
              c(isolat,traitement,m,gorge.m,non_gorge.m,espose.m,period))
    data <- rbind(data,d)
  }
  return(data.frame(data)) 
}

dta <- func(f1)

#library(openxlsx)
#write.xlsx(dta,file ="F:/INStech/Program_R/dta0.xlsx", sheetName = "sheet0" )
setwd("E:/INStech/Données")
dta=read.table("dta0.txt",header=T)
summary(dta)
str(dta)
dta <- na.omit(dta)
p1 <- subset(dta,isolats=="P1")
p2 <- subset(dta,isolats=="P2")
p3 <- subset(dta,isolats=="P3")
p4 <- subset(dta,isolats=="P4")
p5 <- subset(dta,isolats=="P5")
p6 <- subset(dta,isolats=="P6")


###############################################################
###################    Les fonctions         ##################
###################                          ##################
###############################################################
Espece_traitement <- function(p){
  
  tp1<-p %>% 
    # Création de deux nouvelles variables
    mutate(total_gorge = as.numeric(gorges)) %>% 
    mutate(total_expose = as.numeric(exposes)) %>% 
    # on regroupe suivant le traitement et l'espèce
    group_by(traitements,especes) %>% 
    dplyr::summarise(
      total_gorge = sum(total_gorge),
      expose = sum(total_expose),
      
      Nb_gobelets = n()
    ) %>% 
    # Tri: croissant
    arrange((traitements))
  
  
  #View(grp4)
  library(Hmisc)
  x1<-binconf(x= tp1$total_gorge, n=tp1$expose, alpha=0.05,
              method=c("wilson"),
              include.x=T, include.n=T, return.df=T)
  
  names(x1) <- c("total_gorge","expose","taux","Lower","Upper")
  
  t1 <- merge(tp1, x1, by = c("total_gorge","expose"))
  #t1 <- t1[,-5]
  t1 <- arrange(t1,traitements)
  return(t1)
  
}

period_traitement <- function(p){
  
  tpp1<-p %>% 
    # Création de deux nouvelles variables
    mutate(total_gorge = as.numeric(gorges)) %>% 
    mutate(total_expose = as.numeric(exposes)) %>% 
    # on regroupe suivant le traitement et l'espèce
    group_by(traitements,period) %>% 
    dplyr::summarise(
      total_gorge = sum(total_gorge),
      expose = sum(total_expose),
      
      Nb_gobelets = n()
    ) %>% 
    # Tri: croissant
    arrange((traitements))
  
  
  #View(grp4)
  library(Hmisc)
  xx1<-binconf(x= tpp1$total_gorge, n=tpp1$expose, alpha=0.05,
               method=c("wilson"),
               include.x=T, include.n=T, return.df=T)
  
  names(xx1) <- c("total_gorge","expose","taux","Lower","Upper")
  
  tt1 <- merge(tpp1, xx1, by = c("total_gorge","expose"))
  #t1 <- t1[,-5]
  tt1 <- arrange(tt1,traitements)
  return(tt1)
}

Isolat_traitement <- function(P){
  
  grp3<-P %>% 
    # Création de deux nouvelles variables
    mutate(total_gorge = as.numeric(gorges)) %>% 
    mutate(total_expose = as.numeric(exposes)) %>% 
    # on regroupe suivant le traitement et l'espèce
    group_by(traitements,isolats) %>% 
    dplyr::summarise(
      total_gorge = sum(total_gorge),
      expose = sum(total_expose),
      
      Nombre_de_gobelet = n()
    ) %>% 
    # Tri: croissant
    arrange((traitements))
  
  
  #View(grp1)
  library(Hmisc)
  Y3<-binconf(x= grp3$total_gorge, n=grp3$expose, alpha=0.05,
              method=c("wilson"),
              include.x=T, include.n=T, return.df=T)
  
  names(Y3) <- c("total_gorge","expose","taux","Lower","Upper")
  
  tab3 <- merge(grp3, Y3, by = c("total_gorge","expose"))
  tab3 <- tab3[,-5]
  tab3 <- arrange(tab3,traitements)
  return(tab3)
}
Isolat_espece <- function(P){
  grp4<-P %>% 
    # Création de deux nouvelles variables
    mutate(total_gorge = as.numeric(gorges)) %>% 
    mutate(total_expose = as.numeric(exposes)) %>% 
    # on regroupe suivant le traitement et l'espèce
    group_by(isolats,especes) %>% 
    dplyr::summarise(
      total_gorge = sum(total_gorge),
      expose = sum(total_expose),
      
      Nombre_de_gobelet = n()
    ) %>% 
    # Tri: croissant
    arrange((isolats))
  
  
  #View(grp4)
  library(Hmisc)
  Y4<-binconf(x= grp4$total_gorge, n=grp4$expose, alpha=0.05,
              method=c("wilson"),
              include.x=T, include.n=T, return.df=T)
  
  names(Y4) <- c("total_gorge","expose","taux","Lower","Upper")
  
  tab4 <- merge(grp4, Y4, by = c("total_gorge","expose"))
  tab4 <- tab4[,-5]
  tab4 <- arrange(tab4,isolats)
}
Traitement <-function(P){
  grp0<-P %>% 
    # Création de deux nouvelles variables
    mutate(total_gorge = as.numeric(gorges)) %>% 
    mutate(total_expose = as.numeric(exposes)) %>% 
    # Spécification du niveau d'agrégation
    group_by(traitements) %>% 
    dplyr::summarise(
      total_gorge = sum(total_gorge),
      expose = sum(total_expose),
      #Taux_de_gorgement= total_gorge/expose,
      Nombre_de_gobelet =n()
    ) %>% 
    # Tri: croissant
    arrange((traitements))
  
  
  Y0<-binconf(x= grp0$total_gorge, n=grp0$expose, alpha=0.05,
              method=c("wilson"),
              include.x=T, include.n=T, return.df=T)
  #View(Y1)
  names(Y0) <- c("total_gorge","expose","taux","Lower","Upper")
  #View(Y1)
  
  tab0 <- merge(grp0, Y0, by = c("total_gorge","expose"))
  tab0 <- tab0[,-4]
  tab0 <- arrange(tab0,traitements)
}

#########################################################
###################     Graphiques       ################
###################                      ################
#########################################################
setwd("E:/INStech/Texte/oviposition/word/gorgement")

# 1- Taux de gorgement en fonftion de la period et du traitement.
library(dplyr)

tab2 <- period_traitement(dta)

x11();ggplot(tab2, aes(x=period, y=taux, fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("Période")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.9, 0.6))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


library(tidyverse)
p <- position_dodge(0.1) 

# 2 Taux de gorgement en fonction des traitements(a period et Isolat constant)
library(tidyverse)
p <- position_dodge(0.1)
tab0 <-Traitement(dta)

x11();ggplot(tab0, aes(x=traitements, y=taux), colour=traitements)+#, group=espece)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c(1,2,3,4,5,6,7)))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7))+
  theme_classic()

x11();ggplot(tab0, aes(x=traitements, y=taux,fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab(" Traitements")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


# 3- Taux de gorgement en fonction traitement et de l'especes( a period constant)

tab1 <- Espece_traitement(dta)

x11();ggplot(tab1, aes(x=traitements, y=taux, colour=especes, group=especes)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()
x11();ggplot(tab1, aes(x=especes, y=taux, fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("Espèce")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

# 4- Taux de gorgement en fonction traitement et de la periode
tab3 <- period_traitement(dta)
x11();ggplot(tab3, aes(x=traitements, y=taux, colour=period, group=period)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()

# 5 - taux de gorgement en fonction du traitement et de l'isolat
tab4 <- Isolat_traitement(dta)
x11();ggplot(tab4, aes(x=isolats, y=taux, fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("Isolat")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  xlab("Isolats")+
  theme(legend.position = c(0.75, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))



################### Traitement ~ Especes #################

# 1-  taux de gorgement en fonction du traitement et de l'espece __ isolat =P1 ###
tr_is.P1 <- Espece_traitement(p1)
x11();ggplot(tr_is.P1, aes(x=traitements, y=taux, colour=especes, group=especes)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()
x11();ggplot(tr_is.P1, aes(x=especes, y=taux, fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("Espèce")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


# 2-  taux de gorgement en fonction du traitement et de l'espece __ isolat =P2 ###
tr_is.P2 <- Espece_traitement(p2)
x11();ggplot(tr_is.P2, aes(x=traitements, y=taux, colour=especes, group=especes)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()
x11();ggplot(tr_is.P2, aes(x=especes, y=taux, fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("Espèce")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


# 3-  taux de gorgement en fonction du traitement et de l'espece __ isolat =P3 ###
tr_is.P3 <- Espece_traitement(p3)
x11();ggplot(tr_is.P3, aes(x=traitements, y=taux, colour=especes, group=especes)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()
x11();ggplot(tr_is.P3, aes(x=especes, y=taux, fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("Espèce")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

# 4-  taux de gorgement en fonction du traitement et de l'espece __ isolat =P4 ###
tr_is.P4 <- Espece_traitement(p4)
x11();ggplot(tr_is.P4, aes(x=traitements, y=taux, colour=especes, group=especes)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()
x11();ggplot(tr_is.P4, aes(x=especes, y=taux, fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("Espèce")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

# 5-  taux de gorgement en fonction du traitement et de l'espece __ isolat =P5 ###
tr_is.P5 <- Espece_traitement(p5)
x11();ggplot(tr_is.P5, aes(x=traitements, y=taux, colour=especes, group=especes)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()
x11();ggplot(tr_is.P5, aes(x=especes, y=taux, fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("Espèce")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

# 6-  taux de gorgement en fonction du traitement et de l'espece __ isolat =P6 ###
tr_is.P6 <- Espece_traitement(p6)
x11();ggplot(tr_is.P6, aes(x=traitements, y=taux, colour=especes, group=especes)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()
x11();ggplot(tr_is.P6, aes(x=especes, y=taux, fill=traitements)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("Espèce")+
  ylab("Taux de gorgement")+
  coord_cartesian(ylim = c(0, 1.25))+
  scale_fill_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

################### Traitement ~ period #################
# 1- Taux de gorgement en fonction traitement et de la periode isolat = P1
tr_pe.p1 <- period_traitement(p1)
x11();ggplot(tr_pe.p1, aes(x=traitements, y=taux, colour=period, group=period)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()


# 2- Taux de gorgement en fonction traitement et de la periode isolat = P2
tr_pe.p2 <- period_traitement(p2)
x11();ggplot(tr_pe.p2, aes(x=traitements, y=taux, colour=period, group=period)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()

# 3- Taux de gorgement en fonction traitement et de la periode isolat = P3
tr_pe.p3 <- period_traitement(p3)
x11();ggplot(tr_pe.p3, aes(x=traitements, y=taux, colour=period, group=period)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()

# 4- Taux de gorgement en fonction traitement et de la periode isolat = P4
tr_pe.p4 <- period_traitement(p4)
x11();ggplot(tr_pe.p4, aes(x=traitements, y=taux, colour=period, group=period)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()

# 5- Taux de gorgement en fonction traitement et de la periode,pour l'isolat = P5
tr_pe.p5 <- period_traitement(p5)
x11();ggplot(tr_pe.p5, aes(x=traitements, y=taux, colour=period, group=period)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()

# 6- Taux de gorgement en fonction traitement et de la periode,pour l'isolat = P6
tr_pe.p6 <- period_traitement(p6)
x11();ggplot(tr_pe.p6, aes(x=traitements, y=taux, colour=period, group=period)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1, position=p) +
  ylab("Taux de gorgement")+
  geom_line(position=p, size=2) +
  geom_point(position=p, size=1)+
  scale_colour_manual(values=c(c("#1E90FF", "#D02090", "#FFFFFF")))+
  scale_fill_manual(values=c("#1E90FF", "#D02090", "#FFFFFF"))+
  theme_classic()

#########################################################
############# Exportation des tableaux ##################
#########################################################
library(xlsx)
tab2 <- period_traitement(dta)
write.xlsx (tab2, file = "F:/INStech/Texte/oviposition/word/tr_pe.data.xlsx")

tab0 <-Traitement(dta)
write.xlsx (tab2, file = "F:/INStech/Texte/oviposition/word/tratement.xlsx")

tab1 <- Espece_traitement(dta)
write.xlsx (tab1, file = "F:/INStech/Texte/oviposition/word/tr_esp.data.xlsx")

tab4 <- Isolat_traitement(dta)
write.xlsx (tab4, file = "F:/INStech/Texte/oviposition/word/Iso_tr.data.xlsx")

tr_is.P1 <- Espece_traitement(p1)
write.xlsx (tr_is.P1, file = "F:/INStech/Texte/oviposition/word/tr_is.P1.xlsx")

tr_is.P2 <- Espece_traitement(p2)
write.xlsx (tr_is.P2, file = "F:/INStech/Texte/oviposition/word/tr_is.P2.xlsx")

tr_is.P3 <- Espece_traitement(p3)
write.xlsx (tr_is.P3, file = "F:/INStech/Texte/oviposition/word/tr_is.P3.xlsx")

tr_is.P4 <- Espece_traitement(p4)
write.xlsx (tr_is.P4, file = "F:/INStech/Texte/oviposition/word/tr_is.P4.xlsx")

tr_is.P5 <- Espece_traitement(p5)
write.xlsx (tr_is.P5, file = "F:/INStech/Texte/oviposition/word/tr_is.P5.xlsx")

tr_is.P6 <- Espece_traitement(p6)
write.xlsx (tr_is.P6, file = "F:/INStech/Texte/oviposition/word/tr_is.P6.xlsx")

tr_pe.p1 <- period_traitement(p1)
write.xlsx (tr_pe.p1, file = "F:/INStech/Texte/oviposition/word/tr_pe.p1.xlsx")

tr_pe.p2 <- period_traitement(p2)
write.xlsx (tr_pe.p2, file = "F:/INStech/Texte/oviposition/word/tr_pe.p2.xlsx")

tr_pe.p3 <- period_traitement(p3)
write.xlsx (tr_pe.p3, file = "F:/INStech/Texte/oviposition/word/tr_pe.p3.xlsx")

tr_pe.p4 <- period_traitement(p4)
write.xlsx (tr_pe.p4, file = "F:/INStech/Texte/oviposition/word/tr_pe.p4.xlsx")

tr_pe.p5 <- period_traitement(p5)
write.xlsx (tr_pe.p5, file = "F:/INStech/Texte/oviposition/word/tr_pe.p5.xlsx")

tr_pe.p6 <- period_traitement(p6)
write.xlsx (tr_pe.p6, file = "F:/INStech/Texte/oviposition/word/tr_pe.p6.xlsx")


###########################################################################
############                                                 ##############
#+++++++++++               GLM model                         ##############
############                                                 ##############
###########################################################################
attach(dta)
view(dta)
x <- as.factor(traitements)
y <- cbind(gorges,non_gorges)

test <- function(model){
  Dr <- model$deviance
  D0 <-model$null.deviance
  return((1-Dr/D0)*100)
}
model1 <- glm(y ~ relevel(x,ref="Ctrl")*period* especes+isolats  ,family=binomial)
summary(model1) # 1501.1/77 est largement superieure a 1 

model2 <- glm(y ~ traitements*period*especes + isolats,family =  quasibinomial)
summary(model2) ## L'interaction n'est pas statistiquement significative
                ##entre les variables
test(model2)
model3 <- glm(y ~ relevel(traitements,ref ="DMSO")+period+especes + isolats,family =  quasibinomial)
test(model3)
summary(model3)

anova(model2,model3,test="Chi")
model4 <- glm(y ~ traitements+especes + isolats,family =  quasibinomial)
anova(model3,model4,test="Chi")
summary(model5)
model5 <- glm(y ~ traitements*especes + isolats,family =  quasibinomial)
model6 <- glm(y ~ traitements* period +especes + isolats,family =  quasibinomial)

#################################
str(dta$traitements)
str(dta$isolats)
barplot(table(dta$gorges,dta$traitements))

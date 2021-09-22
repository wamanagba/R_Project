
###################  Article ###########################

#------------ s=0.95 -----------------------

# ===========........Mean of survival rate........=======
d<- DONNEE_LOGLOG
mediane<-seq(0,170,1)
xx <- c(mediane,rev(mediane))
yy <- c((d[,2,1]+1.96*d[,2,2]/100), rev((d[,2,1]-1.96*d[,2,2]/100)))
x11();plot   (xx, yy, type = "n", xlab = "Median of follow up", ylab = "mean of survival rate")
polygon(xx,yy,col = "gray", border = "black")
points(mediane,d[,2,1],pch=".",cex=2)
text(34,.98,"median of follow-up = 30",srt=90)
abline(v=30,lty=2)
legend("topright",lty=1,legend = c("limit of confidence interval "),col ="black")


# ==== === ......la couverture ....====== ====
d<- DONNEE_LOGLOG
xx <- c(mediane,rev(mediane))
yy <- c((d[,1,1]+1.96*d[,1,2]/100), rev((d[,1,1]-1.96*d[,1,2]/100)))
x11();plot   (xx, yy, type = "n", xlab = "Median of follow-up", ylab = "coverage of the confidence interval",ylim=c(0.9,1))
polygon(xx,yy,col = "gray", border = "black")
points(mediane,d[,1,1], pch=".",cex=3)
abline(v=38,lwd=1,lty=2)
text(42,.917,"median of follow-up = 38",srt=90)
abline(h=.946,lty=2,lwd=1)
text(12,.95," lower CI bound",srt=360)
#text(12,.95," lower CI bound",srt=360)
abline(h=.956,lty=2,lwd=1)
text(12,.96," upper CI bound",srt=360)
legend("topright",lty=1,lwd =0.1 ,legend = c("limit of confidence interval "))

d<- DONNEE_LOGLOG
xx <- c(mediane,rev(mediane))
yy <- c((d[,1,1]+1.96*d[,1,2]/100), rev((d[,1,1]-1.96*d[,1,2]/100)))
x11();plot   (xx, yy, type = "l", xlab = "Median of follow up", ylab = "coverage of the confidence interval")
polygon(xx,yy,col = "gray", border = "black ")
points(mediane,d[,1,1], pch=".",cex=2)
abline(v=38,lwd=1,lty=2)
text(33,.4,"median of follow-up = 38",srt=90)
abline(h=.95,lty=2,lwd=0.1)
text(12,.968," 0.95",srt=360)
legend("bottomright",lty=1,lwd =0.1 ,legend = c("limit of confidence interval"))

# ======= long_interval -------------

d<- DONNEE_LOGLOG
xx <- c(mediane,rev(mediane))
yy <- c((d[,5,1]+1.96*d[,5,2]/100), rev((d[,5,1]-1.96*d[,5,2]/100)))
x11();plot   (xx, yy, type = "n", xlab = "median of follow-up", ylab = "length of the confidence interval")
polygon(xx,yy,col = "gray")
points(mediane,d[,5,1], pch=".",cex=2)
abline(v=30,lwd=1,lty=2)
text(34,.06,"median of follow-up = 34",srt=90)
abline(h=.03,lwd=1,lty=2)
text(110,.033,"length of the confidence interval = 0.03",srt=360)
abline(h=0.09,lwd=1,lty=2)
#text(85,0.015,'Estimation for s=0.95',col="blue")
text(110,.093,"length of the confidence interval = 0.09",srt=360)
legend("topright",lty=1,lwd =0.1,legend = c("limit of confidence interval"))


#===================== box-plot ===============================
#--------------------- survie=95% --------------------------
me95_15<-simul(N=10000,n=246,s=.95,t=60,M=15,METHOD = 'loglog')
me95_38<-simul(N=10000,n=246,s=.95,t=60,M=38,METHOD = 'loglog')
me95_60<-simul(N=10000,n=246,s=.95,t=60,M=60,METHOD = 'loglog')
me95_120<-simul(N=10000,n=246,s=.95,t=60,M=120,METHOD = 'loglog')
me95_170<-simul(N=10000,n=246,s=.95,t=60,M=170,METHOD = 'loglog')
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
x11();boxplot(me95_15[,3],me95_38[,3],me95_60[,3],me95_120[,3],me95_170[,3],
              
              col = c('red','green','blue',"lightblue","lightpink"),
              outline = FALSE,
              #main='', #sub='Scénario 1',
              xlab='Médiane de suivi',ylab='Taux de survie' ,
              names=c("me=15","me=38","me=60","me=120","me=170")
              
);text(0.5,.952,"0.95",srt=360,col = 2);abline(h=0.95,col=2,lty=2)

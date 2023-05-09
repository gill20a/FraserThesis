setwd("/Users/mollyfraser/Dropbox/Molly_Thesis")
data <- read.csv("rawfluxdata.csv")
head(data)

N <- data[c(1:37),]
dim(N)
O<- data[c(38:73),]

n_worms<-N$Worm==TRUE
n_no_worms<-N$Worm==FALSE
o_worms<-O$Worm==TRUE
o_no_worms<-O$Worm==FALSE

## STD ERR ##

(sd(N[n_worms, "FCO2_DRY_umol_m2_sec"], na.rm=T)/sqrt(18))
(sd(N[n_no_worms, "FCO2_DRY_umol_m2_sec"], na.rm=T)/sqrt(18))
(sd(O[o_worms, "FCO2_DRY_umol_m2_sec"], na.rm=T)/sqrt(18))
(sd(O[o_no_worms, "FCO2_DRY_umol_m2_sec"], na.rm=T)/sqrt(18))

## FLUX SUMMARY ##
tiff('plot1.tiff', units="in", width=8, height=8, res=300)

par(mfrow=c(1,1))
par(mar=c(2,5,2,3))
NorwayRows<-data$Substrate=="N"
plot1<-plot(data[NorwayRows,"Worm"], data[NorwayRows, "FCO2_DRY_umol_m2_sec"],
     col="blue", xlim=c(-0.25,1.25), xaxt="n",
     ylab=expression(paste("Respiration (", u, "mol ",CO[2]," ",m^-2," ",sec^-1,") ", sep="")), xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data[NorwayRows, "FCO2_DRY_umol_m2_sec"],data[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
OakRows<-data$Substrate=="O"
points(data[OakRows,"Worm"], data[OakRows, "FCO2_DRY_umol_m2_sec"],
       col="red")
Oak_Means<-tapply(data[OakRows, "FCO2_DRY_umol_m2_sec"],data[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", c("Norway Maple", "Oak", "p < 0.001"), col=c("blue", "red", "black"),
       pch=1, bty="n")
dev.off(4)

## LINEAR MODELS ##

colnames(data)
library(nlme)
fit<-lm(FCO2_DRY_umol_m2_sec~Substrate*Worm*DOY_round, data=data)
summary(fit)
anova(fit)

fit<-lm(FCO2_DRY_umol_m2_sec~Substrate*Worm, data=data)
summary(fit)
anova(fit)

fit = lme(FCO2_DRY_umol_m2_sec~Substrate*Worm,random=~1|DOY_round,data=data, na.action=na.omit)
summary(fit)
anova(fit)

## percent CN std err ##
data5 <- read.csv("CN_ratio.csv")
head(data5)
N <- data5[c(1:6),]
dim(N)
O<- data5[c(7:13),]

n_worms<-N$Worm==TRUE
n_no_worms<-N$Worm==FALSE
o_worms<-O$Worm==TRUE
o_no_worms<-O$Worm==FALSE

(sd(N[n_worms, "PercentN"], na.rm=T)/sqrt(3))
(sd(N[n_no_worms, "PercentN"], na.rm=T)/sqrt(3))
(sd(O[o_worms, "PercentN"], na.rm=T)/sqrt(3))
(sd(O[o_no_worms, "PercentN"], na.rm=T)/sqrt(3))

(sd(N[n_worms, "PercentC"], na.rm=T)/sqrt(3))
(sd(N[n_no_worms, "PercentC"], na.rm=T)/sqrt(3))
(sd(O[o_worms, "PercentC"], na.rm=T)/sqrt(3))
(sd(O[o_no_worms, "PercentC"], na.rm=T)/sqrt(3))

(sd(N[n_worms, "CN"], na.rm=T)/sqrt(3))
(sd(N[n_no_worms, "CN"], na.rm=T)/sqrt(3))
(sd(O[o_worms, "CN"], na.rm=T)/sqrt(3))
(sd(O[o_no_worms, "CN"], na.rm=T)/sqrt(3))

fit = lm(data5$CN~data5$Worm)
anova(fit)
summary(fit)

fit = lm(data5$PercentC~data5$Worm)
anova(fit)
summary(fit)

fit = lm(data5$PercentN~data5$Worm)
anova(fit)
summary(fit)

fit = lm(data$FCO2_DRY_umol_m2_sec~data$Worm)
anova(fit)
summary(fit)







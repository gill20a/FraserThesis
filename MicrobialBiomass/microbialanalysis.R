setwd("/Users/mollyfraser/Dropbox/Molly_Thesis")
data1 <- read.csv("TOC_newnumbers.csv")
library(ggplot2)
library(plyr)
data1<-data1[-(13:17),]
head(data1)

NorwayRows<-subset(data1, data1$Sample_substrate=="N")
dim(NorwayRows)
OakRows<-subset(data1, data1$Sample_substrate=="O")
dim(OakRows)
n_worms<-NorwayRows$Worm==TRUE
n_no_worms<-NorwayRows$Worm==FALSE
o_worms<-OakRows$Worm==TRUE
o_no_worms<-OakRows$Worm==FALSE

## STD ERR ## 

(sd(NorwayRows[n_worms, "MIBC_ugC"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_no_worms, "MIBC_ugC"], na.rm=T)/sqrt(3))

(sd(NorwayRows[n_worms, "MicrobialBiomassN_ugN"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_no_worms, "MicrobialBiomassN_ugN"], na.rm=T)/sqrt(3))

(sd(NorwayRows[n_worms, "MIB_CN"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_no_worms, "MIB_CN"], na.rm=T)/sqrt(3))

(sd(OakRows[o_worms, "MIBC_ugC"], na.rm=T)/sqrt(3))
(sd(OakRows[o_no_worms, "MIBC_ugC"], na.rm=T)/sqrt(3))

(sd(OakRows[o_worms, "MicrobialBiomassN_ugN"], na.rm=T)/sqrt(3))
(sd(OakRows[o_no_worms, "MicrobialBiomassN_ugN"], na.rm=T)/sqrt(3))

(sd(OakRows[o_worms, "MIB_CN"], na.rm=T)/sqrt(3))
(sd(OakRows[o_no_worms, "MIB_CN"], na.rm=T)/sqrt(3))

## SUMMARY FIGURES ##
par(mfrow=c(3,1))
par(mar=c(2,5,2,3))

NorwayRows<-data1$Sample_substrate=="N"
OakRows<-data1$Sample_substrate=="O"

## MIB C ## 
tiff('mibCN.tiff', units="in", width=8, height=8, res=300)
par(mfrow=c(3,1))
par(mar=c(4,5,2,3))

plot(data1[NorwayRows,"Worm"], data1[NorwayRows, "MIBC_ugC"], 
     col="blue", ylim=c(0,700), xlim=c(-0.25,1.25), xaxt="n",
     ylab="MIBC (ugC)", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data1[NorwayRows, "MIBC_ugC"],data1[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

points(data1[OakRows,"Worm"], data1[OakRows, "MIBC_ugC"],
       col="red")
Oak_Means<-tapply(data1[OakRows, "MIBC_ugC"],data1[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", c("Norway Maple", "Oak"), col=c("blue", "red"), 
       pch=1, bty="n")

## MIB N ## 
plot(data1[NorwayRows,"Worm"], data1[NorwayRows, "MicrobialBiomassN_ugN"], 
     col="blue", ylim=c(-30,80), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="MicrobialBiomassN (ugN)", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data1[NorwayRows, "MicrobialBiomassN_ugN"],data1[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

points(data1[OakRows,"Worm"], data1[OakRows, "MicrobialBiomassN_ugN"], 
       col="red")
Oak_Means<-tapply(data1[OakRows, "MicrobialBiomassN_ugN"],data1[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")

## MIB CN ##
######################
par(mfrow=c(1,1))
plot(data1[NorwayRows,"Worm"], data1[NorwayRows, "MIB_CN"], 
     col="blue", ylim=c(0,15), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="MIB C:N", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data1[NorwayRows, "MIB_CN"],data1[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

points(data1[OakRows,"Worm"], data1[OakRows, "MIB_CN"], 
       col="red")
Oak_Means<-tapply(data1[OakRows, "MIB_CN"],data1[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", "p < 0.1", col="black", 
       pch=1, bty="n")
dev.off()

## respiration ~ MIB ##
tiff('mibresp.tiff', units="in", width=8, height=8, res=300)
par(mfrow=c(3,1))
par(mar=c(4,5,2,3))

NorwayRows<-subset(data1, data1$Sample_substrate=="N")
dim(NorwayRows)
OakRows<-subset(data1, data1$Sample_substrate=="O")
reg<-lm(data1$Avg_mgC~data1$MIBC_ugC)
summary(reg)

NorwayRows<-data1$Sample_substrate=="N"
OakRows<-data1$Sample_substrate=="O"
plot(data1[NorwayRows,"MIBC_ugC"], data1[NorwayRows, "Avg_mgC"], 
     col="blue",
     ylab=expression(paste("", u, "mol ",CO[2]," ",m^-2," ",sec^-1,"", sep="")), xlab="MIBC (ug C)", ylim=c(0,300), xlim=c(0,400))
points(data1[OakRows,"MIBC_ugC"], data1[OakRows, "Avg_mgC"], 
       col="red")
legend("topleft", c("Norway Maple", "Oak", "p < 0.1"), col=c("blue", "red", "black"), 
       pch=1, bty="n")
abline(reg)

fit= lm(data1[NorwayRows, "Avg_mgC"]~data1[NorwayRows,"MIBC_ugC"])
summary(fit)
fit=lm(data1[OakRows, "Avg_mgC"]~data1[OakRows, "MIBC_ugC"])
summary(fit)

## MIBN
NorwayRows<-subset(data1, data1$Sample_substrate=="N")
dim(NorwayRows)
OakRows<-subset(data1, data1$Sample_substrate=="O")
reg<-lm(data1$Avg_mgC~data1$MicrobialBiomassN_ugN)
summary(reg)

NorwayRows<-data1$Sample_substrate=="N"
OakRows<-data1$Sample_substrate=="O"

plot(data1[NorwayRows,"MicrobialBiomassN_ugN"], data1[NorwayRows, "Avg_mgC"], 
     col="blue",
     ylab=expression(paste("", u, "mol ",CO[2]," ",m^-2," ",sec^-1,"", sep="")), xlab="MIBN (ug N)", ylim=c(0,300), xlim=c(0,80))
legend("topleft", c("Norway Maple", "Oak"), col=c("blue", "red", "black"), 
       pch=1, bty="n")
points(data1[OakRows,"MicrobialBiomassN_ugN"], data1[OakRows, "Avg_mgC"], 
       col="red")
abline(reg)

fit= lm(data1[NorwayRows, "Avg_mgC"]~data1[NorwayRows,"MicrobialBiomassN_ugN"])
summary(fit)
fit=lm(data1[OakRows, "Avg_mgC"]~data1[OakRows, "MicrobialBiomassN_ugN"])
summary(fit)

### MIB CN
NorwayRows<-subset(data1, data1$Sample_substrate=="N")
dim(NorwayRows)
OakRows<-subset(data1, data1$Sample_substrate=="O")
reg<-lm(data1$Avg_mgC~data1$MIB_CN)
summary(reg)

NorwayRows<-data1$Sample_substrate=="N"
OakRows<-data1$Sample_substrate=="O"

plot(data1[NorwayRows,"MIB_CN"], data1[NorwayRows, "Avg_mgC"], 
     col="blue",
     ylab=expression(paste("", u, "mol ",CO[2]," ",m^-2," ",sec^-1,"", sep="")), xlab="MIB C:N (ug)", ylim=c(0,300), xlim=c(0,10))
points(data1[OakRows,"MIB_CN"], data1[OakRows, "Avg_mgC"], 
       col="red")
legend("topleft", c("Norway Maple", "Oak"), col=c("blue", "red", "black"), 
       pch=1, bty="n")
abline(reg)

dev.off(2)

fit= lm(data1[NorwayRows, "Avg_mgC"]~data1[NorwayRows,"MIB_CN"])
summary(fit)
fit=lm(data1[OakRows, "Avg_mgC"]~data1[OakRows, "MIB_CN"])
summary(fit)

fit=lm(data1$Avg_mgC~data1$MIB_CN)
summary(fit)

fit=lm(data1$Avg_mgC~data1$MIBC_ugC)
summary(fit)

fit=lm(data1$Avg_mgC~data1$MicrobialBiomassN_ugN)
summary(fit)


head(data1)
fit = lm(data1$MIBC_ugC~data1$Worm)
anova(fit)
summary(fit)


fit = lm(data1$MicrobialBiomassN_ugN~data1$Worm)
anova(fit)
summary(fit)

fit = lm(data1$MIB_CN~data1$Worm)
anova(fit)
summary(fit)


setwd("/Users/mollyfraser/Dropbox/Molly_Thesis")
data2 <- read.csv("density_fractionation.csv")
head(data2)
library(ggplot2)

par(mfrow=c(3,1))
par(mar=c(2,5,2,3))

#soil metrics
data6 <- read.csv("CN_ratio.csv")
print(data6)

N <- data6[c(1:6),]
O<- data6[c(7:12),]

n_worms<-N$Worm==TRUE
length(o_worms)
n_no_worms<-N$Worm==FALSE
o_worms<-O$Worm==TRUE
o_no_worms<-O$Worm==FALSE

par(mfrow=c(1,1))
plot(data6[NorwayRows,"Worm"], data6[NorwayRows, "PercentC"],
     col="blue", xlim=c(-0.25,1.25), xaxt="n",
     ylab="C Flux (umolC/m^2/sec)", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data6[NorwayRows, "PercentC"],data6[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(data6[OakRows,"Worm"], data6[OakRows, "PercentC"],
       col="red")
Oak_Means<-tapply(data6[OakRows, "PercentC"],data6[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", c("Norway Maple", "Oak", "p < 0.001"), col=c("blue", "red", "black"),
       pch=1, bty="n")

## LF
colnames(data2)
NorwayRows<-data2$Substrate=="N"
plot(data2[NorwayRows,"Worms"], data2[NorwayRows, "LF_percent"], 
     col="blue", ylim=c(0,25), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="% Light Fraction", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data2[NorwayRows, "LF_percent"],data2[NorwayRows,"Worms"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data2$Substrate=="O" 
points(data2[OakRows,"Worms"], data2[OakRows, "LF_percent"],
       col="red")
Oak_Means<-tapply(data2[OakRows, "LF_percent"],data2[OakRows,"Worms"], mean, na.rm=T)
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", c("Norway Maple", "Oak"), col=c("blue", "red"), 
       pch=1, bty="n")

fit = lm(data2$LF_percent~data2$Substrate*data2$Worms) 
anova(fit)
summary(fit)

fit = lm(data2$LF_percent~data2$Worms)
anova(fit)

## Sand
NorwayRows<-data2$Substrate=="N"
plot(data2[NorwayRows,"Worms"], data2[NorwayRows, "Sand_percent"], 
     col="blue", ylim=c(0,70), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="% Sand Fraction", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data2[NorwayRows, "Sand_percent"],data2[NorwayRows,"Worms"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data1$Sample_substrate=="O"
points(data2[OakRows,"Worms"], data2[OakRows, "Sand_percent"],
       col="red")
Oak_Means<-tapply(data2[OakRows, "Sand_percent"],data2[OakRows,"Worms"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", c("Norway Maple", "Oak"), col=c("blue", "red"), 
       pch=1, bty="n")

fit = lm(data2$Sand_percent~data2$Substrate*data2$Worms) 
anova(fit)
summary(fit)

fit = lm(data2$Sand_percent~data2$Worms)
anova(fit)
summary(fit)

## Clay
NorwayRows<-data2$Substrate=="N"
plot(data2[NorwayRows,"Worms"], data2[NorwayRows, "Clay_percent"], 
     col="blue", ylim=c(0,70), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="% Clay Fraction", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data2[NorwayRows, "Clay_percent"],data2[NorwayRows,"Worms"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data2$Substrate=="O"
points(data2[OakRows,"Worms"], data2[OakRows, "Clay_percent"],
       col="red")
Oak_Means<-tapply(data2[OakRows, "Clay_percent"],data2[OakRows,"Worms"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", c("Norway Maple", "Oak"), col=c("blue", "red"), 
       pch=1, bty="n")

fit = lm(data2$Clay_percent~data2$Substrate*data2$Worms) 
anova(fit)
summary(fit)

fit = lm(data2$Clay_percent~data2$Worms)
anova(fit)
summary(fit)

# fraction plots
worm<-data2$Worms==T
no_worm<-data2$Worms==F

LF_worms_frac<-tapply(data2[worm, "LF_percent"], data2[worm, "Substrate"], mean, na.rm=T)
Sand_worms_frac<-tapply(data2[worm, "Sand_percent"], data2[worm, "Substrate"], mean, na.rm=T)
Clay_worms_frac<-tapply(data2[worm, "Clay_percent"], data2[worm, "Substrate"], mean, na.rm=T)

LF_no_worms_frac<-tapply(data2[no_worm, "LF_percent"], data2[no_worm, "Substrate"], mean, na.rm=T)
Sand_no_worms_frac<-tapply(data2[no_worm, "Sand_percent"], data2[no_worm, "Substrate"], mean, na.rm=T)
Clay_no_worms_frac<-tapply(data2[no_worm, "Clay_percent"], data2[no_worm, "Substrate"], mean, na.rm=T)

fractions <- rbind(LF_worms_frac, Sand_worms_frac, Clay_worms_frac, LF_no_worms_frac, Sand_no_worms_frac, Clay_no_worms_frac)
worm_fractions <- rbind(LF_worms_frac, Sand_worms_frac, Clay_worms_frac)
noworm_fractions <- rbind(LF_no_worms_frac, Sand_no_worms_frac, Clay_no_worms_frac)
tiff('fraction.tiff', units="in", width=8, height=8, res=300)
par(mfrow=c(1,2)+0.02)
par(mar=c(5,5,2,3))

as.matrix(O_worm_frac <- worm_fractions[,2])
as.matrix(O_noworm_frac <- noworm_fractions[,2])
as.matrix(N_worm_frac<- worm_fractions[,1])
as.matrix(N_noworm_frac <- noworm_fractions[,1])

n_frac<-cbind(N_noworm_frac, N_worm_frac)
o_frac<-cbind(O_noworm_frac, O_worm_frac)
barplot(n_frac, col=c("blue", "#89CFF0", "#7393B3"), xaxt="n", xlab="Norway Maple")
axis(1, at=c(.7,1.9), labels=c("No Worm", "Worm"))
mtext("+", side=1, adj=0.77, line=-2, font=2, cex=1.5, col="white")
box()
barplot(o_frac, yaxt="n", col=c("red", "#FA5F55", "#AA4A44"), xaxt="n", xlab="Oak")
axis(1, at=c(.7,1.9), labels=c("No Worm", "Worm"))
mtext("+", side=1, adj=0.77, line=-3, font=2, cex=1.5, col="white")
box()
dev.off()
#### Percent change significance testing 
setwd("/Users/mollyfraser/Dropbox/Molly_Thesis")
data4 <- read.csv("percentchange.csv")
head(data4)

t.test(data4$C_N, data4$C_N_n, paired=FALSE) 
t.test(data4$PercentN, data4$PercentN_n, paired=FALSE) 
t.test(data4$PercentC, data4$PercentC_n, paired=FALSE) 
t.test(data4$C_resp, data4$C_resp_n, paired=FALSE) 
t.test(data4$MIBN, data4$MIBN_n, paired=FALSE) 
t.test(data4$MIB_CN, data4$MIB_CN_n, paired=FALSE) 
t.test(data4$MIBC, data4$MIBC_n, paired=FALSE) 
t.test(data4$LF_percent, data4$LF_percent_n, paired=FALSE) 
t.test(data4$Sand_percent, data4$Sand_percent_n, paired=FALSE) 
t.test(data4$Clay_percent, data4$Clay_percent_n, paired=FALSE) 

t.test(data4$LF_C, data4$LF_C_n, paired=FALSE) 
t.test(data4$LF_N, data4$LF_N_n, paired=FALSE) 
t.test(data4$LF_CN, data4$LF_CN_n, paired=FALSE) 
t.test(data4$S_N, data4$S_N_n, paired=FALSE) 
t.test(data4$S_C, data4$S_C_n, paired=FALSE)
t.test(data4$S_CN, data4$S_CN_n, paired=FALSE) 
t.test(data4$Clay_N, data4$Clay_N_n, paired=FALSE) 
t.test(data4$Clay_C, data4$Clay_C_n, paired=FALSE) 
t.test(data4$Clay_CN, data4$Clay_CN_n, paired=FALSE) 


##### fraction carbon
data3<-read.csv("fraction_CN_final.csv")
head(data3)
par(mfrow=c(3,1))
par(mar=c(2,5,2,3))

# LF C N ## 
NorwayRows<-data3$Sample_substrate=="N"
plot(data3[NorwayRows,"Worm"], data3[NorwayRows, "LF_C"], 
     col="blue", ylim=c(0,200), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="LF C", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data3[NorwayRows, "LF_C"],data3[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data3$Sample_substrate=="O"
points(data3[OakRows,"Worm"], data3[OakRows, "LF_C"],
       col="red")
Oak_Means<-tapply(data3[OakRows, "LF_C"],data3[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", c("Norway Maple", "Oak"), col=c("blue", "red"), 
       pch=1, bty="n")

fit = lm(data3$LF_C~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

fit = lm(data3$LF_C~data3$Worm) 
anova(fit)
summary(fit)

NorwayRows<-data3$Sample_substrate=="N"
plot(data3[NorwayRows,"Worm"], data3[NorwayRows, "LF_N"], 
     col="blue", ylim=c(0,10), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="LF N", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data3[NorwayRows, "LF_N"],data3[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data3$Sample_substrate=="O"
points(data3[OakRows,"Worm"], data3[OakRows, "LF_N"],
       col="red")
Oak_Means<-tapply(data3[OakRows, "LF_N"],data3[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")

fit = lm(data3$LF_N~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

fit = lm(data3$LF_N~data3$Worm) 
anova(fit)
summary(fit)

NorwayRows<-data3$Sample_substrate=="N"
plot(data3[NorwayRows,"Worm"], data3[NorwayRows, "LF_CN"], 
     col="blue", ylim=c(0,30), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="LF CN", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data3[NorwayRows, "LF_CN"],data3[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data3$Sample_substrate=="O"
points(data3[OakRows,"Worm"], data3[OakRows, "LF_CN"],
       col="red")
Oak_Means<-tapply(data3[OakRows, "LF_CN"],data3[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")


fit = lm(data3$LF_CN~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

fit = lm(data3$LF_CN~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

## Sand CN ##
par(mfrow=c(3,1))
par(mar=c(2,5,2,3))
NorwayRows<-data3$Sample_substrate=="N"
plot(data3[NorwayRows,"Worm"], data3[NorwayRows, "S_C"], 
     col="blue", ylim=c(0,300), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="S C", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data3[NorwayRows, "S_C"],data3[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data3$Sample_substrate=="O"
points(data3[OakRows,"Worm"], data3[OakRows, "S_C"],
       col="red")
Oak_Means<-tapply(data3[OakRows, "S_C"],data3[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", c("Norway Maple", "Oak"), col=c("blue", "red"), 
       pch=1, bty="n")

fit = lm(data3$S_C~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

fit = lm(data3$S_C~data3$Worm) 
anova(fit)
summary(fit)

NorwayRows<-data3$Sample_substrate=="N"
plot(data3[NorwayRows,"Worm"], data3[NorwayRows, "S_N"], 
     col="blue", ylim=c(0,15), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="S N", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data3[NorwayRows, "S_N"],data3[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data3$Sample_substrate=="O"
points(data3[OakRows,"Worm"], data3[OakRows, "S_N"],
       col="red")
Oak_Means<-tapply(data3[OakRows, "S_N"],data3[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")

fit = lm(data3$S_N~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

fit = lm(data3$S_N~data3$Worm) 
anova(fit)
summary(fit)

NorwayRows<-data3$Sample_substrate=="N"
plot(data3[NorwayRows,"Worm"], data3[NorwayRows, "S_CN"], 
     col="blue", ylim=c(0,50), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="S CN", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data3[NorwayRows, "S_CN"],data3[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data3$Sample_substrate=="O"
points(data3[OakRows,"Worm"], data3[OakRows, "S_CN"],
       col="red")
Oak_Means<-tapply(data3[OakRows, "S_CN"],data3[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")


fit = lm(data3$S_CN~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

fit = lm(data3$S_CN~data3$Worm) 
anova(fit)
summary(fit)

## Clay CN ## 
par(mfrow=c(3,1))
par(mar=c(2,5,2,3))

tiff('clayc.tiff', units="in", width=8, height=8, res=300)
par(mfrow=c(1,1))
NorwayRows<-data3$Sample_substrate=="N"
plot(data3[NorwayRows,"Worm"], data3[NorwayRows, "Clay_C"], 
     col="blue", ylim=c(0,200), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="Clay C (ug)", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data3[NorwayRows, "Clay_C"],data3[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data3$Sample_substrate=="O"
points(data3[OakRows,"Worm"], data3[OakRows, "Clay_C"],
       col="red")
Oak_Means<-tapply(data3[OakRows, "Clay_C"],data3[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")
legend("topleft", c("Norway Maple", "Oak"), col=c("blue", "red"), 
       pch=1, bty="n")
dev.off(2)
fit = lm(data3$Clay_C~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

fit = lm(data3$Clay_C~data3$Worm) 
anova(fit)
summary(fit)

NorwayRows<-data3$Sample_substrate=="N"
plot(data3[NorwayRows,"Worm"], data3[NorwayRows, "Clay_N"], 
     col="blue", ylim=c(0,10), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="Clay N", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data3[NorwayRows, "Clay_N"],data3[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data3$Sample_substrate=="O"
points(data3[OakRows,"Worm"], data3[OakRows, "Clay_N"],
       col="red")
Oak_Means<-tapply(data3[OakRows, "Clay_N"],data3[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")

fit = lm(data3$Clay_N~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

fit = lm(data3$Clay_N~data3$Worm) 
anova(fit)
summary(fit)

NorwayRows<-data3$Sample_substrate=="N"
plot(data3[NorwayRows,"Worm"], data3[NorwayRows, "Clay_CN"], 
     col="blue", ylim=c(0,50), xlim=c(-0.25,1.25), xaxt="n", 
     ylab="Clay CN", xlab="")
axis(1, at=c(0,1), labels=c("No Worm", "Worm"))
Norway_Means<-tapply(data3[NorwayRows, "Clay_CN"],data3[NorwayRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")

OakRows<-data3$Sample_substrate=="O"
points(data3[OakRows,"Worm"], data3[OakRows, "Clay_CN"],
       col="red")
Oak_Means<-tapply(data3[OakRows, "Clay_CN"],data3[OakRows,"Worm"], mean, na.rm=T)
points(c(0,1), Norway_Means, col="blue", pch=16, cex=2, type="b")
points(c(0,1), Oak_Means, col="red", pch=16, cex=2, type="b")

fit = lm(data3$Clay_CN~data3$Sample_substrate*data3$Worm) 
anova(fit)
summary(fit)

fit = lm(data3$Clay_CN~data3$Worm)
anova(fit)
summary(fit)

fit = lm(data3$Clay_CN~data3$Sample_substrate)
anova(fit)
summary(fit)

## STD ERR ##
head(data2)
NorwayRows<-subset(data2, data2$Substrate=="N")
dim(NorwayRows)
OakRows<-subset(data2, data2$Substrate=="O")
dim(OakRows)
n_worms = NorwayRows$Worm == T
n_no_worms = NorwayRows$Worm == F
o_worms = OakRows$Worm == T
o_no_worms = OakRows$Worm == F

(sd(data2[n_worms, "LF_percent"], na.rm=T)/sqrt(3))
(sd(data2[n_worms, "Sand_percent"], na.rm=T)/sqrt(3))
(sd(data2[n_worms, "Clay_percent"], na.rm=T)/sqrt(3))

(sd(data2[n_no_worms, "LF_percent"], na.rm=T)/sqrt(3))
(sd(data2[n_no_worms, "Sand_percent"], na.rm=T)/sqrt(3))
(sd(data2[n_no_worms, "Clay_percent"], na.rm=T)/sqrt(3))

(sd(data2[o_worms, "LF_percent"], na.rm=T)/sqrt(3))
(sd(data2[o_worms, "Sand_percent"], na.rm=T)/sqrt(3))
(sd(data2[o_worms, "Clay_percent"], na.rm=T)/sqrt(3))

(sd(data2[o_no_worms, "LF_percent"], na.rm=T)/sqrt(3))
(sd(data2[o_no_worms, "Sand_percent"], na.rm=T)/sqrt(3))
(sd(data2[o_no_worms, "Clay_percent"], na.rm=T)/sqrt(3))

head(data3)

NorwayRows<-subset(data3, data3$Sample_substrate=="N")
OakRows<-subset(data3, data3$Sample_substrate=="O")
n_worms = NorwayRows$Worm == TRUE
n_no_worms = NorwayRows$Worm == FALSE
o_worms = OakRows$Worm == TRUE
o_no_worms = OakRows$Worm == FALSE

(sd(NorwayRows[n_worms, "LF_C"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_worms, "LF_N"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_worms, "LF_CN"], na.rm=T)/sqrt(3))

(sd(NorwayRows[n_no_worms, "LF_C"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_no_worms, "LF_N"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_no_worms, "LF_CN"], na.rm=T)/sqrt(3))

(sd(OakRows[o_worms, "LF_C"], na.rm=T)/sqrt(3))
(sd(OakRows[o_worms, "LF_N"], na.rm=T)/sqrt(3))
(sd(OakRows[o_worms, "LF_CN"], na.rm=T)/sqrt(3))

(sd(OakRows[o_no_worms, "LF_C"], na.rm=T)/sqrt(3))
(sd(OakRows[o_no_worms, "LF_N"], na.rm=T)/sqrt(3))
(sd(OakRows[o_no_worms, "LF_CN"], na.rm=T)/sqrt(3))


(sd(NorwayRows[n_worms, "S_C"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_worms, "S_N"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_worms, "S_CN"], na.rm=T)/sqrt(3))

(sd(NorwayRows[n_no_worms, "S_C"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_no_worms, "S_N"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_no_worms, "S_CN"], na.rm=T)/sqrt(3))

(sd(OakRows[o_worms, "S_C"], na.rm=T)/sqrt(3))
(sd(OakRows[o_worms, "S_N"], na.rm=T)/sqrt(3))
(sd(OakRows[o_worms, "S_CN"], na.rm=T)/sqrt(3))

(sd(OakRows[o_no_worms, "S_C"], na.rm=T)/sqrt(3))
(sd(OakRows[o_no_worms, "S_N"], na.rm=T)/sqrt(3))
(sd(OakRows[o_no_worms, "S_CN"], na.rm=T)/sqrt(3))

(sd(NorwayRows[n_worms, "Clay_C"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_worms, "Clay_N"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_worms, "Clay_CN"], na.rm=T)/sqrt(3))

(sd(NorwayRows[n_no_worms, "Clay_C"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_no_worms, "Clay_N"], na.rm=T)/sqrt(3))
(sd(NorwayRows[n_no_worms, "Clay_CN"], na.rm=T)/sqrt(3))

(sd(OakRows[o_worms, "Clay_C"], na.rm=T)/sqrt(3))
(sd(OakRows[o_worms, "Clay_N"], na.rm=T)/sqrt(3))
(sd(OakRows[o_worms, "Clay_CN"], na.rm=T)/sqrt(3))

(sd(OakRows[o_no_worms, "Clay_C"], na.rm=T)/sqrt(3))
(sd(OakRows[o_no_worms, "Clay_N"], na.rm=T)/sqrt(3))
(sd(OakRows[o_no_worms, "Clay_CN"], na.rm=T)/sqrt(3))


head(data2)
fit = lm(data2$LF_percent~data2$Worms)
anova(fit)
summary(fit)

fit = lm(data2$Sand_percent~data2$Worms)
anova(fit)
summary(fit)

fit = lm(data2$Clay_percent~data2$Worms)
anova(fit)
summary(fit)

head(data3)
fit = lm(data3$LF_C~data3$Worm)
anova(fit)
summary(fit)

fit = lm(data3$LF_N~data3$Worm)
anova(fit)
summary(fit)

fit = lm(data3$LF_CN~data3$Worm)
anova(fit)
summary(fit)

fit = lm(data3$S_C~data3$Worm)
anova(fit)
summary(fit)

fit = lm(data3$S_N~data3$Worm)
anova(fit)
summary(fit)

fit = lm(data3$S_CN~data3$Worm)
anova(fit)
summary(fit)

fit = lm(data3$Clay_C~data3$Worm)
anova(fit)
summary(fit)

fit = lm(data3$Clay_N~data3$Worm)
anova(fit)
summary(fit)

fit = lm(data3$Clay_CN~data3$Worm)
anova(fit)
summary(fit)

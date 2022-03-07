library(popbio);
library(popdemo);
library(ggplot2);
library(export);
library(tidyr);
library(xlsx);
library(afex);

setwd("C:/Users/evane/OneDrive/Documents/KU Leuven/Master's Thesis/Data & Analysis")


# NEW MATRIX CONSTRUCTION #

x <- c('N1','N2','J1','J2','J3','A')

A08.09 <- matrix(c(0,0,0,0,0,0.169,0.6,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0.9,0.877), byrow=TRUE, ncol=6);
rownames(A08.09) <- x;
colnames(A08.09) <- x;
A08.09;

A09.10 <- matrix(c(0,0,0,0,0,0.212,0.909,0,0,0,0,0,0,0.667,0,0,0,0,0,0,0.824,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0.879), byrow=TRUE, ncol=6);
rownames(A09.10) <- x;
colnames(A09.10) <- x;
A09.10;

A10.11 <- matrix(c(0,0,0,0,0,0.145,0.857,0,0,0,0,0,0,0.8,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0.957), byrow=TRUE, ncol=6);
rownames(A10.11) <- x;
colnames(A10.11) <- x;
A10.11;

A11.12 <- matrix(c(0,0,0,0,0,0.1,0.6,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0.957), byrow=TRUE, ncol=6);
rownames(A11.12) <- x;
colnames(A11.12) <- x;
A11.12;

A12.13 <- matrix(c(0,0,0,0,0,0.185,1,0,0,0,0,0,0,0.667,0,0,0,0,0,0,0.917,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0.901), byrow=TRUE, ncol=6);
rownames(A12.13) <- x;
colnames(A12.13) <- x;
A12.13;

A13.14 <- matrix(c(0,0,0,0,0,0.24,1,0,0,0,0,0,0,0.714,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0.933), byrow=TRUE, ncol=6);
rownames(A13.14) <- x;
colnames(A13.14) <- x;
A13.14;

A14.15 <- matrix(c(0,0,0,0,0,0.154,0.944,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0.949), byrow=TRUE, ncol=6);
rownames(A14.15) <- x;
colnames(A14.15) <- x;
A14.15;

A15.16 <- matrix(c(0,0,0,0,0,0.141,0.75,0,0,0,0,0,0,0.941,0,0,0,0,0,0,0.867,0,0,0,0,0,0,1,0,0,0,0,0,0,0.75,0.941), byrow=TRUE, ncol=6);
rownames(A15.16) <- x;
colnames(A15.16) <- x;
A15.16;

A16.17 <- matrix(c(0,0,0,0,0,0.301,0.833,0,0,0,0,0,0,0.778,0,0,0,0,0,0,0.813,0,0,0,0,0,0,0.846,0,0,0,0,0,0,1,0.928), byrow=TRUE, ncol=6);
rownames(A16.17) <- x;
colnames(A16.17) <- x;
A16.17;

A17.18 <- matrix(c(0,0,0,0,0,0.085,0.84,0,0,0,0,0,0,0.8,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0.866), byrow=TRUE, ncol=6);
rownames(A17.18) <- x;
colnames(A17.18) <- x;
A17.18;

A18.19 <- matrix(c(0,0,0,0,0,0.134,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0.951), byrow=TRUE, ncol=6);
rownames(A18.19) <- x;
colnames(A18.19) <- x;
A18.19;

Av <- matrix(c(0,0,0,0,0,0.17,0.849,0,0,0,0,0,0,0.852,0,0,0,0,0,0,0.947,0,0,0,0,0,0,0.986,0,0,0,0,0,0,0.968,0.922), byrow=TRUE, ncol=6);
rownames(Av) <- x;
colnames(Av) <- x;
Av;


# POPULATION GROWTH RATES #

lambda(A08.09);
lambda(A09.10);
lambda(A10.11);
lambda(A11.12);
lambda(A12.13);
lambda(A13.14);
lambda(A14.15);
lambda(A15.16);
lambda(A16.17);
lambda(A17.18);
lambda(A18.19);

lambda(Av);

# sTOCHASTIC POPULATION GROWTH RATE #

gelada <- list(A08.09,A09.10,A10.11,A11.12,A12.13,A13.14,A14.15,A15.16,A16.17,A17.18,A18.19);
gelada;

sgr <- stoch.growth.rate(gelada)
sgr;
exp(sgr$approx);
exp(sgr$sim);


# INITIAL POPULATION VECTORS #

n08.09 <- matrix(c(5,17,4,11,10,65), byrow=TRUE, ncol=1)
rownames(n08.09) <- x;

n09.10 <- matrix(c(11,3,17,4,11,66), byrow=TRUE, ncol=1)
rownames(n09.10) <- x;

n10.11 <- matrix(c(14,10,2,14,4,69), byrow=TRUE, ncol=1)
rownames(n10.11) <- x;

n11.12 <- matrix(c(10,12,8,2,14,70), byrow=TRUE, ncol=1)
rownames(n11.12) <- x;

n12.13 <- matrix(c(7,6,12,8,2,81), byrow=TRUE, ncol=1)
rownames(n12.13) <- x;

n13.14 <- matrix(c(15,7,4,11,8,75), byrow=TRUE, ncol=1)
rownames(n13.14) <- x;

n14.15 <- matrix(c(18,15,5,4,11,78), byrow=TRUE, ncol=1)
rownames(n14.15) <- x;

n15.16 <- matrix(c(12,17,15,5,4,85), byrow=TRUE, ncol=1)
rownames(n15.16) <- x;

n16.17 <- matrix(c(12,9,16,13,5,83), byrow=TRUE, ncol=1)
rownames(n16.17) <- x;

n17.18 <- matrix(c(25,10,7,13,11,82), byrow=TRUE, ncol=1)
rownames(n17.18) <- x;

n18.19 <- matrix(c(7,21,8,7,13,82), byrow=TRUE, ncol=1)
rownames(n18.19) <- x;



# BASIC STATISTICS #

net.reproductive.rate(Av);
stable.stage(Av);
reproductive.value(Av);
fundamental.matrix(Av);
generation.time(Av);

ss08.09 <- data.frame(stable.stage(A08.09))
kd08.09 <- data.frame(KeyfitzD(A08.09,n08.09))
cd08.09 <- data.frame(CohenD(A08.09,n08.09))

ss09.10 <- data.frame(stable.stage(A09.10))
kd09.10 <- data.frame(KeyfitzD(A09.10,n09.10))
cd09.10 <- data.frame(CohenD(A09.10,n09.10))

ss10.11 <- data.frame(stable.stage(A10.11))
kd10.11 <- data.frame(KeyfitzD(A10.11,n10.11))
cd10.11 <- data.frame(CohenD(A10.11,n10.11))

ss11.12 <- data.frame(stable.stage(A11.12))
kd11.12 <- data.frame(KeyfitzD(A11.12,n11.12))
cd11.12 <- data.frame(CohenD(A11.12,n11.12))

ss12.13 <- data.frame(stable.stage(A12.13))
kd12.13 <- data.frame(KeyfitzD(A12.13,n12.13))
cd12.13 <- data.frame(CohenD(A12.13,n12.13))

ss13.14 <- data.frame(stable.stage(A13.14))
kd13.14 <- data.frame(KeyfitzD(A13.14,n13.14))
cd13.14 <- data.frame(CohenD(A13.14,n13.14))

ss14.15 <- data.frame(stable.stage(A14.15))
kd14.15 <- data.frame(KeyfitzD(A14.15,n14.15))
cd14.15 <- data.frame(CohenD(A14.15,n14.15))

ss15.16 <- data.frame(stable.stage(A15.16))
kd15.16 <- data.frame(KeyfitzD(A15.16,n15.16))
cd15.16 <- data.frame(CohenD(A15.16,n15.16))

ss16.17 <- data.frame(stable.stage(A16.17))
kd16.17 <- data.frame(KeyfitzD(A16.17,n16.17))
cd16.17 <- data.frame(CohenD(A16.17,n16.17))

ss17.18 <- data.frame(stable.stage(A17.18))
kd17.18 <- data.frame(KeyfitzD(A17.18,n17.18))
cd17.18 <- data.frame(CohenD(A17.18,n17.18))

ss18.19 <- data.frame(stable.stage(A18.19))
kd18.19 <- data.frame(KeyfitzD(A18.19,n18.19))
cd18.19 <- data.frame(CohenD(A18.19,n18.19))

ssfull <- data.frame(cbind(ss07.08,ss08.09,ss09.10,ss10.11,ss11.12,ss12.13,ss13.14,ss14.15,ss15.16,ss16.17,ss17.18,ss18.19))
ssfull2 <- data.frame(t(ssfull))
colnames(ssfull2) <- x;

kdfull <- data.frame(cbind(kd07.08,kd08.09,kd09.10,kd10.11,kd11.12,kd12.13,kd13.14,kd14.15,kd15.16,kd16.17,kd17.18,kd18.19))
kdfull2 <- data.frame(t(kdfull))

cdfull <- data.frame(cbind(cd07.08,cd08.09,cd09.10,cd10.11,cd11.12,cd12.13,cd13.14,cd14.15,cd15.16,cd16.17,cd17.18,cd18.19))
cdfull2 <- data.frame(t(cdfull))

stablestage <- data.frame(Year=c('A07.08','A08.09','A09.10','A10.11','A11.12','A12.13','A13.14','A14.15','A15.16','A16.17','A17.18','A18.19'),
                          N=ssfull2$N1, N2=ssfull2$N2, J1=ssfull2$J1, J2=ssfull2$J2, J3=ssfull2$J3, A=ssfull2$A, KeyfitzDelta=kdfull2$t.kdfull., CohensD=cdfull2$t.cdfull.)



# SENSITIVITY ANALYSIS #

stoch.sens <- stoch.sens(gelada, tlimit = 1000)
stoch.sens;
image2(stoch.sens$sensitivities, border="black", mar=c(1,5,5,1), box.offset=0.1, text.cex=1.1, srt=1);
title(main="Stochastic", line=1, adj=0.6)

ave.sens <- sensitivity(Av, zero=TRUE)
image2(ave.sens, border="black", mar=c(1,5,5,1), box.offset=0.1, text.cex=1.1, srt=1);
title(main="Average", line=1, adj=0.6)


par(mfrow= c(3,4))
sens08.09 <- sensitivity(A08.09, zero=TRUE);
image2(sens08.09, border="black", mar=c(1,5,5,1), box.offset=0.1, text.cex=1.1, srt=1);
title(main="A08.09", line=1, adj=0.6)

sens09.10 <- sensitivity(A09.10, zero=TRUE);
image2(sens09.10, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A09.10", line=1, adj=0.6)

sens10.11 <- sensitivity(A10.11, zero=TRUE);
image2(sens10.11, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A10.11", line=1, adj=0.6)

sens11.12 <- sensitivity(A11.12, zero=TRUE);
image2(sens11.12, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A11.12", line=1, adj=0.6)

sens12.13 <- sensitivity(A12.13, zero=TRUE);
image2(sens12.13, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A12.13", line=1, adj=0.6)

sens13.14 <- sensitivity(A13.14, zero=TRUE);
image2(sens13.14, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A13.14", line=1, adj=0.6)

sens14.15 <- sensitivity(A14.15, zero=TRUE);
image2(sens14.15, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A14.15", line=1, adj=0.6)

sens15.16 <- sensitivity(A15.16, zero=TRUE);
image2(sens15.16, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A15.16", line=1, adj=0.6)

sens16.17 <- sensitivity(A16.17, zero=TRUE);
image2(sens16.17, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A16.17", line=1, adj=0.6)

sens17.18 <- sensitivity(A17.18, zero=TRUE);
image2(sens17.18, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A17.18", line=1, adj=0.6)

sens18.19 <- sensitivity(A18.19, zero=TRUE);
image2(sens18.19, border="black", mar=c(1,5,5,1), box.offset=.1, text.cex=1.1, srt=1);
title(main="A18.19", line=1, adj=0.6)


## DECOMPOSED Sn AND F SENSITIVITIES ##

vr.ave <- list(Fa=0.182, Sn1=0.928, Sn2=0.849, Sj1=0.852, Sj2=0.947,
               Sj3=0.986, Sj4=0.968, Sa=0.922)
matrix.ave <- expression(
  0, 0, 0, 0, 0, Sn1*Fa,
  Sn2, 0, 0, 0, 0, 0,
  0, Sj1, 0, 0, 0, 0,
  0, 0, Sj2, 0, 0, 0,
  0, 0, 0, Sj3, 0, 0,
  0, 0, 0, 0, Sj4, Sa)
vitalsens(matrix.ave, vr.ave)

sensitivities <- read.csv('sensitivities.csv', header=TRUE, row.names = 1)

ggplot(sensitivities, aes(x=row.names(sensitivities)), y=Sensitivity) +
  geom_bar(stat="identity", aes(y=Sensitivity), fill="steelblue") + theme_classic() +
  theme(plot.margin=margin(10,10,14,14), axis.text=element_text(face="plain", size=12, color="black"), 
        axis.title.x=element_text(face="bold", size=14, vjust=-4), axis.title.y=element_text(face="bold", size=14, vjust=4)) +
  scale_x_discrete(name="Vital Rate", limits=c("Fa","Sn1","Sn2","Sn","Sj1","Sj2","Sj3","Sj4","Sj","Sa")) + 
  scale_y_continuous(name="Sensitivity", limits=c(0,0.7), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7))

## below graph to illustrate Pfister's hypothesis
ggplot(sensitivities, aes(x=row.names(sensitivities)), y=Variance) +
  geom_bar(stat="identity", aes(y=Variance), fill="steelblue") + theme_classic() +
  theme(plot.margin=margin(10,10,14,14), axis.text=element_text(face="plain", size=12, color="black"), 
        axis.title.x=element_text(face="bold", size=14, vjust=-4), axis.title.y=element_text(face="bold", size=14, vjust=4)) +
  scale_x_discrete(name="Vital Rate", limits=c("Fa","Sn","Sj","Sa")) + 
  scale_y_continuous(name="Variance", limits=c(0,0.008), breaks=c(0.002,0.004,0.006,0.008))



# AGGREGATE INTO A SINGLE MATRIX WITH COLUMNS AS VITAL RATES #

v08.09 <- as.vector(A08.09)
v09.10 <- as.vector(A09.10)
v10.11 <- as.vector(A10.11)
v11.12 <- as.vector(A11.12)
v11.12 <- as.vector(A11.12)
v12.13 <- as.vector(A12.13)
v13.14 <- as.vector(A13.14)
v14.15 <- as.vector(A14.15)
v15.16 <- as.vector(A15.16)
v16.17 <- as.vector(A16.17)
v17.18 <- as.vector(A17.18)
v18.19 <- as.vector(A18.19)

vrates <- rbind(v08.09, v09.10, v10.11, v11.12, v12.13, v13.14, v14.15, v15.16, v16.17, v17.18, v18.19)
vrates <- vrates[,-c(1,3,4,5,6,7,8,10,11,12,13,14,15,17,18,19,20,21,22,24,25,26,27,28,29,31,32,33,34,35)]
colnames(vrates) <- c("Sn2","Sj1","Sj2","Sj3","Sj4","Sa")
vrates.decomp <- read.csv('vrates.decomp.csv', header=TRUE, row.names = 1)
vrates <- cbind(vrates, vrates.decomp)

write.csv(vrates, "C:/Users/evane/OneDrive/Documents/KU Leuven/Master's Thesis/Data & Analysis/vrates.csv")



# ASSESS COVARIANCE #

vrates.df <- as.data.frame(vrates)
vrates_shapiro <- lapply(vrates.df, shapiro.test) # all are normally distributed #

vrate_cov <- cov(vrates, method = "pearson")
vrate_cor <- cov2cor(vrate_cov)

# strong: Fa/Sj (-0.701)
# moderate: Sn1/Sa (0.473), Fa/Sn (0.380), Sa/Sj1 (0.404)
# all others are weak



# CLIMATE INTERPOLATION #

#### Step1_Rainfalls_interpolation
library(simputation)
library(climwin)
set.seed(100) 
raw.climate <- read.csv("raw.climate.csv")
climate<-raw.climate
climate <- climate[,-c(6,8)]

climate[,1] <- as.Date(as.character(climate[,1]), format = "%d/%m/%Y")
climate[,5] <- as.numeric(as.character(climate[,5])) #I have reduced three lines to a single one
climate[,6] <- as.numeric(as.character(climate[,6]))
climate[,7] <- as.numeric(as.character(climate[,7]))

climate$Min.T[climate$Min.T > 19] <- NA
climate$Max.T[climate$Max.T > 35] <- NA

climate.imp_lm<-impute_lm(climate, Rainfall.mm  ~ Date)
climate.imp_proxy<-impute_proxy(climate, Rainfall.mm  ~ Date)
climate.imp_rf<-impute_rf(climate, Rainfall.mm  ~ Date)
climate.imp_cart<-impute_cart(climate, Rainfall.mm  ~ Date)
climate_compare<-cbind(climate$Rainfall.mm , climate.imp_lm$Rainfall.mm , climate.imp_proxy$Rainfall.mm ,climate.imp_rf$Rainfall.mm ,climate.imp_cart$Rainfall.mm )
colnames(climate_compare) <- c("Observed", "LM", "Proxy", "RF", "CART")
cc.df<-data.frame(climate_compare)
par(mfrow = c(2,2))
plot(cc.df$Observed, cc.df$LM)
plot(cc.df$Observed, cc.df$Proxy)
plot(cc.df$Observed, cc.df$RF)
plot(cc.df$Observed, cc.df$CART)

summary(cc.df)
#   Observed           LM              Proxy               RF               CART      
#Min.   :  0.0   Min.   :  0.000   Min.   :    0.0   Min.   :  0.000   Min.   :  0.0  
#1st Qu.:  0.0   1st Qu.:  0.000   1st Qu.:    0.0   1st Qu.:  0.000   1st Qu.:  0.0  
#Median :  0.1   Median :  0.600   Median :    0.6   Median :  0.200   Median :  0.6  
#Mean   :  5.4   Mean   :  5.378   Mean   : 1512.4   Mean   :  5.462   Mean   :  5.4  
#3rd Qu.:  6.0   3rd Qu.:  5.600   3rd Qu.:   10.5   3rd Qu.:  6.000   3rd Qu.:  5.4  
#Max.   :140.0   Max.   :140.000   Max.   :18104.0   Max.   :140.000   Max.   :140.0  
#NA's   :499                                                                          

# from plot and summary, I can clearly exclude the Proxy method. LM, RF and CART give very similar results. 


p_obs<-ggplot(cc.df, aes(x=Observed)) + geom_density() + geom_density(color="darkblue", fill="lightblue", linetype="dashed") 
p_LM<-ggplot(cc.df, aes(x=LM)) + geom_density() + geom_density(color="darkblue", fill="red", linetype="dashed") 
p_RF<-ggplot(cc.df, aes(x=RF)) + geom_density() + geom_density(color="darkblue", fill="green", linetype="dashed") 
p_CART<-ggplot(cc.df, aes(x=CART)) + geom_density() + geom_density(color="darkblue", fill="grey", linetype="dashed") 
library(gridExtra)
grid.arrange(p_obs, p_LM, p_RF, p_CART, nrow = 2)
#From this results I may clearly chose the RF (Random Forest) method, but it allows some negative values (which is clearly impossible in reality)
#Therefore, I chose to use the CART method
#To quote the CART method, Breiman L (1984) Classification and regression trees. The Wadsworth and Brooks-Cole statisticsprobability series. Chapman #& Hall.

climate.full<-impute_cart(climate, Rainfall.mm  + Min.T + Max.T ~ Date)
colnames(climate.full) <- c("Date", "Mos", "Year", "Cum.Mos", "Max.T_imp", "Min.T_imp", "Rainfall.mm_imp", "Notes")
climate.full$Mean.T<-(climate.full$Max.T_imp+climate.full$Min.T_imp)/2


# CONVERTING VITAL RATES INTO BINARY LISTS #

vrates.mod <- read.csv("vrates.mod.csv", header=TRUE, row.names = 1)

cnt2bin <- function(data, suc, fail) {
  
  xvars <- names(data)[names(data)!=suc & names(data)!=fail]
  list <- lapply(xvars, function(z) with(data, rep(get(z), get(suc)+get(fail))))
  names(list) <- xvars
  df <- as.data.frame(list)
  with(data,data.frame(bin=rep(rep(c(1,0),nrow(data)),c(rbind(get(suc),get(fail)))),
                       df))
}

sn1.surv.long <- vrates.mod[,c(1,2,5,6)]
sn1.surv.long <- cnt2bin(sn1.surv.long, "n1.surv", "n1.not_surv")

sn2.surv.long <- vrates.mod[,c(1,2,9,10)]
sn2.surv.long <- cnt2bin(sn2.surv.long, "n2.surv", "n2.not_surv")

sn.all.surv.long <- vrates.mod[,c(1,2,40,41)]
sn.all.surv.long <- cnt2bin(sn.all.surv.long, "n.all.surv", "n.all.not_surv")

sj1.surv.long <- vrates.mod[,c(1,2,13,14)]
sj1.surv.long <- cnt2bin(sj1.surv.long, "j1.surv", "j1.not_surv")

sj2.surv.long <- vrates.mod[,c(1,2,17,18)]
sj2.surv.long <- cnt2bin(sj2.surv.long, "j2.surv", "j2.not_surv")

sj3.surv.long <- vrates.mod[,c(1,2,21,22)]
sj3.surv.long <- cnt2bin(sj3.surv.long, "j3.surv", "j3.not_surv")

sj4.surv.long <- vrates.mod[,c(1,2,25,26)]
sj4.surv.long <- cnt2bin(sj4.surv.long, "j4.surv", "j4.not_surv")

sj.all.surv.long <- vrates.mod[,c(1,2,36,37)]
sj.all.surv.long <- cnt2bin(sj.all.surv.long, "j.all.surv", "j.all.not_surv")

sa.surv.long <- vrates.mod[,c(1,2,29,30)]
sa.surv.long <- cnt2bin(sa.surv.long, "a.surv", "a.not_surv")

fa.long <- vrates.mod[,c(1,2,32,33)]
fa.long <- cnt2bin(fa.long, "a.repro", "a.not_repro")


# MOVING WINDOW CLIMATE ANALYSIS #
#	missing nearly 4 months in 2016 due to evacuation (August to December)
# precludes analysis of A15.16 and A16.17

vrates <- read.csv("vrates.df")
write.xlsx(vrates.mod, file="vrates_mod.xlsx")
vrates.mod <- read.csv("vrates.mod.csv")
# null models should use a binomial GLMM with vital rates represented as binary lists and year as a random factor

# Adult Survival #

library(lme4)
null.sa <- glmer(bin ~ 1 + (1|Year), family=binomial, data=sa.surv.long, nAGQ = 0)

rain.sa <- slidingwin(xvar = list(Rainfall.mm_imp=climate.full$Rainfall.mm_imp),
                      cdate = climate.full$Date, bdate = sa.surv.long$Date,
                      baseline = null.sa, range = c(24, 0), cinterval = "month",
                      type = "absolute", refday = c(31, 12),
                      stat = c("mean"), func = "lin")
head(rain.sa[[1]]$Dataset)
#here there was a singularity problem (i.e., parameters are on the boundary of the feasible parameter space: variances of one or more linear #combinations of effects are (close to) zero)
# however, a test for singularity rejected this hypothesis
#isSingular(null.sa, tol = 1e-4)
#FALSE

rain.sa.rand <- randwin(repeats = 100, xvar = list(Rainfall.mm_imp=climate.full$Rainfall.mm_imp),
                        cdate = climate.full$Date, bdate = sa.surv.long$Date,
                        baseline = null.sa, range = c(24, 0), cinterval = "month",
                        type = "absolute", refday = c(31, 12),
                        stat = c("mean"), func = "lin", cmissing ="method2", k=3)
pvalue(dataset = rain.sa[[1]]$Dataset, datasetrand = rain.sa.rand[[1]], metric = "C", sample.size = 9) # p=0.495
plotall(dataset = rain.sa[[1]]$Dataset, datasetrand = rain.sa.rand[[1]],
        bestmodel = rain.sa[[1]]$BestModel, bestmodeldata = rain.sa[[1]]$BestModelData)
# 90% of models within 95% confidence

ggplot(rain.sa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))

ggplot(rain.sa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = ModelBeta)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "darkred", name = "Model ??") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))

temp.sa <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                      cdate = climate.full$Date, bdate = sa.surv.long$Date,
                      baseline = null.sa, range = c(24, 0), cinterval = "month",
                      type = "absolute", refday = c(31, 12),
                      stat = "mean", func = "lin")
head(temp.sa[[1]]$Dataset)

temp.sa.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                        cdate = climate.full$Date, bdate = sa.surv.long$Date,
                        baseline = null.sa, range = c(24, 0), cinterval = "month",
                        type = "absolute", refday = c(31, 12),
                        stat = "mean", func = "lin")
pvalue(dataset = temp.sa[[1]]$Dataset, datasetrand = temp.sa.rand[[1]], metric = "C", sample.size = 9) # p=0.429
plotall(dataset = temp.sa[[1]]$Dataset, datasetrand = temp.sa.rand[[1]],
        bestmodel = temp.sa[[1]]$BestModel, bestmodeldata = temp.sa[[1]]$BestModelData)
# 85% of models within 95% confidence

ggplot(temp.sa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))


ggplot(temp.sa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = ModelBeta)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "darkred", name = "Model ??") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))



# Infant Survival #

null.sn1 <- glmer.nb(bin ~ 1 + (1|Year), family=binomial, data=sn1.surv.long, nAGQ = 0)

rain.sn1 <- slidingwin(xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                       cdate = climate.full$Date, bdate = sn.surv.long$Date,
                       baseline = null.sn, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = c("mean"), func = "lin")
head(rain.sn1[[1]]$Dataset)

rain.sn1.rand <- randwin(repeats = 100, xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                         cdate = climate.full$Date, bdate = sn.surv.long$Date,
                         baseline = null.sn, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = c("mean"), func = "lin")
pvalue(dataset = rain.sn1[[1]]$Dataset, datasetrand = rain.sn1.rand[[1]], metric = "C", sample.size = 9) # p=0.561
plotall(dataset = rain.sn1[[1]]$Dataset, datasetrand = rain.sn1.rand[[1]],
        bestmodel = rain.sn1[[1]]$BestModel, bestmodeldata = rain.sn1[[1]]$BestModelData)
# 95% of models within 95% confidence


temp.sn1 <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                       cdate = climate.full$Date, bdate = sn.surv.long$Date,
                       baseline = null.sn1, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = "mean", func = "lin")
head(temp.sn1[[1]]$Dataset)

temp.sn1.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                         cdate = climate.full$Date, bdate = sn.surv.long$Date,
                         baseline = null.sn1, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = "mean", func = "lin")
pvalue(dataset = temp.sn1[[1]]$Dataset, datasetrand = temp.sn1.rand[[1]], metric = "C", sample.size = 9) # p=0.561
plotall(dataset = temp.sn1[[1]]$Dataset, datasetrand = temp.sn1.rand[[1]],
        bestmodel = temp.sn1[[1]]$BestModel, bestmodeldata = temp.sn1[[1]]$BestModelData)
# 95% of models within 95% confidence



null.sn2 <- glmer(bin ~ 1 + (1|Year), family=binomial, data=sn2.surv.long, nAGQ = 0)

rain.sn2 <- slidingwin(xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                       cdate = climate.full$Date, bdate = sn2.surv.long$Date,
                       baseline = null.sn2, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = c("mean"), func = "lin")
head(rain.sn2[[1]]$Dataset)

rain.sn2.rand <- randwin(repeats = 100, xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                         cdate = climate.full$Date, bdate = sn2.surv.long$Date,
                         baseline = null.sn2, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = c("mean"), func = "lin", cmissing ="method2")
pvalue(dataset = rain.sn2[[1]]$Dataset, datasetrand = rain.sn2.rand[[1]], metric = "C", sample.size = 9) # p=0.450
plotall(dataset = rain.sn2[[1]]$Dataset, datasetrand = rain.sn2.rand[[1]],
        bestmodel = rain.sn2[[1]]$BestModel, bestmodeldata = rain.sn2[[1]]$BestModelData)
# 84% of models within 95% confidence

ggplot(rain.sn2[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))

ggplot(rain.sn2[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = ModelBeta)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "darkred", name = "Model ??") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))


temp.sn2 <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                       cdate = climate.full$Date, bdate = sn2.surv.long$Date,
                       baseline = null.sn2, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = "mean", func = "lin")
head(temp.sn2[[1]]$Dataset)

temp.sn2.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                         cdate = climate.full$Date, bdate = sn2.surv.long$Date,
                         baseline = null.sn2, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = "mean", func = "lin")
pvalue(dataset = temp.sn2[[1]]$Dataset, datasetrand = temp.sn2.rand[[1]], metric = "C", sample.size = 9) # p=0.586
plotall(dataset = temp.sn2[[1]]$Dataset, datasetrand = temp.sn2.rand[[1]],
        bestmodel = temp.sn2[[1]]$BestModel, bestmodeldata = temp.sn2[[1]]$BestModelData)
# 93% of models within 95% confidence


null.sn_all <- glmer.nb(bin ~ 1 + (1|Year), family=binomial, data=sn.all.surv.long, nAGQ = 0)

rain.sn_all <- slidingwin(xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                          cdate = climate.full$Date, bdate = sn.all.surv.long$Date,
                          baseline = null.sn_all, range = c(24, 0), cinterval = "month",
                          type = "absolute", refday = c(31, 12),
                          stat = c("mean"), func = "lin")
head(rain.sn_all[[1]]$Dataset)

rain.sn_all.rand <- randwin(repeats = 100, xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                            cdate = climate.full$Date, bdate = sn.all.surv.long$Date,
                            baseline = null.sn_all, range = c(24, 0), cinterval = "month",
                            type = "absolute", refday = c(31, 12),
                            stat = c("mean"), func = "lin", cmissing ="method2")
pvalue(dataset = rain.sn_all[[1]]$Dataset, datasetrand = rain.sn_all.rand[[1]], metric = "C", sample.size = 9) # p=0.554
plotall(dataset = rain.sn_all[[1]]$Dataset, datasetrand = rain.sn_all.rand[[1]],
        bestmodel = rain.sn_all[[1]]$BestModel, bestmodeldata = rain.sn_all[[1]]$BestModelData)
# 95% of models within 95% confidence


temp.sn_all <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                          cdate = climate.full$Date, bdate = sn.all.surv.long$Date,
                          baseline = null.sn_all, range = c(24, 0), cinterval = "month",
                          type = "absolute", refday = c(31, 12),
                          stat = "mean", func = "lin")
head(temp.sn_all[[1]]$Dataset)

temp.sn_all.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                            cdate = climate.full$Date, bdate = sn.all.surv.long$Date,
                            baseline = null.sn_all, range = c(24, 0), cinterval = "month",
                            type = "absolute", refday = c(31, 12),
                            stat = "mean", func = "lin", cmissing ="method2")
pvalue(dataset = temp.sn_all[[1]]$Dataset, datasetrand = temp.sn_all.rand[[1]], metric = "C", sample.size = 9) # p=0.561
plotall(dataset = temp.sn_all[[1]]$Dataset, datasetrand = temp.sn_all.rand[[1]],
        bestmodel = temp.sn_all[[1]]$BestModel, bestmodeldata = temp.sn_all[[1]]$BestModelData)
# 95% of models within 95% confidence



# Juvenile Survival #

null.sj1 <- glmer.nb(bin ~ 1 + (1|Year), family=binomial, data=sj1.surv.long, nAGQ = 0)

rain.sj1 <- slidingwin(xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                       cdate = climate.full$Date, bdate = sj1.surv.long$Date,
                       baseline = null.sj1, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = c("mean"), func = "lin")
head(rain.sj1[[1]]$Dataset)

rain.sj1.rand <- randwin(repeats = 100, xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                         cdate = climate.full$Date, bdate = sj1.surv.long$Date,
                         baseline = null.sj1, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = c("mean"), func = "lin", cmissing ="method2")
pvalue(dataset = rain.sj1[[1]]$Dataset, datasetrand = rain.sj1.rand[[1]], metric = "C", sample.size = 9) # p=0.553
plotall(dataset = rain.sj1[[1]]$Dataset, datasetrand = rain.sj1.rand[[1]],
        bestmodel = rain.sj1[[1]]$BestModel, bestmodeldata = rain.sj1[[1]]$BestModelData)
# 94% of models within 95% confidence



temp.sj1 <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                       cdate = climate.full$Date, bdate = sj1.surv.long$Date,
                       baseline = null.sj1, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = "mean", func = "lin")
head(temp.sj1[[1]]$Dataset)

temp.sj1.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                         cdate = climate.full$Date, bdate = sj1.surv.long$Date,
                         baseline = null.sj1, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = "mean", func = "lin", cmissing ="method2")
pvalue(dataset = temp.sj1[[1]]$Dataset, datasetrand = temp.sj1.rand[[1]], metric = "C", sample.size = 9) # p=0.549
plotall(dataset = temp.sj1[[1]]$Dataset, datasetrand = temp.sj1.rand[[1]],
        bestmodel = temp.sj1[[1]]$BestModel, bestmodeldata = temp.sj1[[1]]$BestModelData)
# 94% of models within 95% confidence



null.sj2 <- glmer.nb(bin ~ 1 + (1|Year), family=binomial, data=sj2.surv.long, nAGQ = 0)

rain.sj2 <- slidingwin(xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                       cdate = climate.full$Date, bdate = sj2.surv.long$Date,
                       baseline = null.sj2, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = c("mean"), func = "lin")
head(rain.sj2[[1]]$Dataset)

rain.sj2.rand <- randwin(repeats = 100, xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                         cdate = climate.full$Date, bdate = sj2.surv.long$Date,
                         baseline = null.sj2, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = c("mean"), func = "lin", cmissing ="method2")
pvalue(dataset = rain.sj2[[1]]$Dataset, datasetrand = rain.sj2.rand[[1]], metric = "C", sample.size = 9) # p=0.561
plotall(dataset = rain.sj2[[1]]$Dataset, datasetrand = rain.sj2.rand[[1]],
        bestmodel = rain.sj2[[1]]$BestModel, bestmodeldata = rain.sj2[[1]]$BestModelData)
# 95% of models within 95% confidence



temp.sj2 <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                       cdate = climate.full$Date, bdate = sj2.surv.long$Date,
                       baseline = null.sj2, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = "mean", func = "lin")
head(temp.sj2[[1]]$Dataset)

temp.sj2.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                         cdate = climate.full$Date, bdate = sj2.surv.long$Date,
                         baseline = null.sj2, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = "mean", func = "lin")
pvalue(dataset = temp.sj2[[1]]$Dataset, datasetrand = temp.sj2.rand[[1]], metric = "C", sample.size = 9) # p=0.557
plotall(dataset = temp.sj2[[1]]$Dataset, datasetrand = temp.sj2.rand[[1]],
        bestmodel = temp.sj2[[1]]$BestModel, bestmodeldata = temp.sj2[[1]]$BestModelData)
# 95% of models within 95% confidence



#Not_RUN
null.sj3 <- glmer(bin ~ 1 + (1|Year), family=binomial, data=sj3.surv.long, nAGQ = 0)  ## no deaths in any years means no detectable variation

rain.sj3 <- slidingwin(xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                       cdate = climate.full$Date, bdate = sj3.surv.long$Date,
                       baseline = null.sj3, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = c("mean"), func = "lin")
head(rain.sj3[[1]]$Dataset)

rain.sj3.rand <- randwin(repeats = 100, xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                         cdate = climate.full$Date, bdate = sj3.surv.long$Date,
                         baseline = null.sj3, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = c("mean"), func = "lin", cmissing ="method2")
pvalue(dataset = rain.sj3[[1]]$Dataset, datasetrand = rain.sj3.rand[[1]], metric = "C", sample.size = 9) # p=0.561
plotall(dataset = rain.sj3[[1]]$Dataset, datasetrand = rain.sj3.rand[[1]],
        bestmodel = rain.sj3[[1]]$BestModel, bestmodeldata = rain.sj3[[1]]$BestModelData)
# 95% of models within 95% confidence


temp.sj3 <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                       cdate = climate.full$Date, bdate = sj3.surv.long$Date,
                       baseline = null.sj3, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = "mean", func = "lin")
head(temp.sj3[[1]]$Dataset)

temp.sj3.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                         cdate = climate.full$Date, bdate = sj3.surv.long$Date,
                         baseline = null.sj3, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = "mean", func = "lin")
pvalue(dataset = temp.sj3[[1]]$Dataset, datasetrand = temp.sj3.rand[[1]], metric = "C", sample.size = 9) # p=0.561
plotall(dataset = temp.sj3[[1]]$Dataset, datasetrand = temp.sj3.rand[[1]],
        bestmodel = temp.sj3[[1]]$BestModel, bestmodeldata = temp.sj3[[1]]$BestModelData)
# 95% of models within 95% confidence



null.sj4 <- glmer.nb(bin ~ 1 + (1|Year), family=binomial, data=sj4.surv.long, nAGQ = 0)

rain.sj4 <- slidingwin(xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                       cdate = climate.full$Date, bdate = sj4.surv.long$Date,
                       baseline = null.sj4, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = c("mean"), func = "lin")
head(rain.sj4[[1]]$Dataset)

rain.sj4.rand <- randwin(repeats = 100, xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                         cdate = climate.full$Date, bdate = sj4.surv.long$Date,
                         baseline = null.sj4, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = c("mean"), func = "lin", cmissing ="method2")
pvalue(dataset = rain.sj4[[1]]$Dataset, datasetrand = rain.sj4.rand[[1]], metric = "C", sample.size = 9) # p=0.561
plotall(dataset = rain.sj4[[1]]$Dataset, datasetrand = rain.sj4.rand[[1]],
        bestmodel = rain.sj4[[1]]$BestModel, bestmodeldata = rain.sj4[[1]]$BestModelData)
# 95% of models within 95% confidence


temp.sj4 <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                       cdate = climate.full$Date, bdate = sj5.surv.long$Date,
                       baseline = null.sj4, range = c(24, 0), cinterval = "month",
                       type = "absolute", refday = c(31, 12),
                       stat = "mean", func = "lin")
head(temp.sj4[[1]]$Dataset)

temp.sj4.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                         cdate = climate.full$Date, bdate = sj4.surv.long$Date,
                         baseline = null.sj4, range = c(24, 0), cinterval = "month",
                         type = "absolute", refday = c(31, 12),
                         stat = "mean", func = "lin", cmissing ="method2")
pvalue(dataset = temp.sj4[[1]]$Dataset, datasetrand = temp.sj4.rand[[1]], metric = "C", sample.size = 9) # p=0.561
plotall(dataset = temp.sj4[[1]]$Dataset, datasetrand = temp.sj4.rand[[1]],
        bestmodel = temp.sj4[[1]]$BestModel, bestmodeldata = temp.sj4[[1]]$BestModelData)
# 95% of models within 95% confidence



null.sj_all <- glmer(bin ~ 1 + (1|Year), family=binomial, data=sj.all.surv.long, control=glmerControl(nAGQ0initStep=FALSE))

rain.sj_all <- slidingwin(xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                          cdate = climate.full$Date, bdate = sj.all.surv.long$Date,
                          baseline = null.sj_all, range = c(24, 0), cinterval = "month",
                          type = "absolute", refday = c(31, 12),
                          stat = c("mean"), func = "lin")
head(rain.sj_all[[1]]$Dataset)

rain.sj_all.rand <- randwin(repeats = 100, xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                            cdate = climate.full$Date, bdate = sj.all.surv.long$Date,
                            baseline = null.sj_all, range = c(24, 0), cinterval = "month",
                            type = "absolute", refday = c(31, 12),
                            stat = c("mean"), func = "lin", cmissing ="method2")
pvalue(dataset = rain.sj_all[[1]]$Dataset, datasetrand = rain.sj_all.rand[[1]], metric = "C", sample.size = 9) # p=0.558
plotall(dataset = rain.sj_all[[1]]$Dataset, datasetrand = rain.sj_all.rand[[1]],
        bestmodel = rain.sj_all[[1]]$BestModel, bestmodeldata = rain.sj_all[[1]]$BestModelData)
# 95% of models within 95% confidence


temp.sj_all <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                          cdate = climate.full$Date, bdate = sj.all.surv.long$Date,
                          baseline = null.sj_all, range = c(24, 0), cinterval = "month",
                          type = "absolute", refday = c(31, 12),
                          stat = "mean", func = "lin")
head(temp.sj_all[[1]]$Dataset)

temp.sj_all.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                            cdate = climate.full$Date, bdate = sj.all.surv.long$Date,
                            baseline = null.sj_all, range = c(24, 0), cinterval = "month",
                            type = "absolute", refday = c(31, 12),
                            stat = "mean", func = "lin", cmissing ="method2")
pvalue(dataset = temp.sj_all[[1]]$Dataset, datasetrand = temp.sj_all.rand[[1]], metric = "C", sample.size = 9) # p=0.064
plotall(dataset = temp.sj_all[[1]]$Dataset, datasetrand = temp.sj_all.rand[[1]],
        bestmodel = temp.sj_all[[1]]$BestModel, bestmodeldata = temp.sj_all[[1]]$BestModelData)
# 38% of models within 95% confidence

ggplot(temp.sj_all[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))

ggplot(temp.sj_all[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = ModelBeta)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "darkred", name = "Model ??") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))



# Fecundity #


null.fa <- glmer(bin ~ 1 + (1|Year), family=binomial, data=fa.long, nAGQ = 0)

rain.fa <- slidingwin(xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                      cdate = climate.full$Date, bdate = fa.long$Date,
                      baseline = null.fa, range = c(24, 0), cinterval = "month",
                      type = "absolute", refday = c(31, 12),
                      stat = c("mean"), func = "lin")
head(rain.fa[[1]]$Dataset)

rain.fa.rand <- randwin(repeats = 100, xvar = list(Rainfall = climate.full$Rainfall.mm_imp),
                        cdate = climate.full$Date, bdate = fa.long$Date,
                        baseline = null.fa, range = c(24, 0), cinterval = "month",
                        type = "absolute", refday = c(31, 12),
                        stat = c("mean"), func = "lin", cmissing ="method2")
pvalue(dataset = rain.fa[[1]]$Dataset, datasetrand = rain.fa.rand[[1]], metric = "C", sample.size = 9) # p=0.473
plotall(dataset = rain.fa[[1]]$Dataset, datasetrand = rain.fa.rand[[1]],
        bestmodel = rain.fa[[1]]$BestModel, bestmodeldata = rain.fa[[1]]$BestModelData)
# 88% of models within 95% confidence

ggplot(rain.fa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))

ggplot(rain.fa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = ModelBeta)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "darkred", name = "Model ??") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))


temp.fa <- slidingwin(xvar = list(Temp = climate.full$Mean.T),
                      cdate = climate.full$Date, bdate = fa.long$Date,
                      baseline = null.fa, range = c(24, 0), cinterval = "month",
                      type = "absolute", refday = c(31, 12),
                      stat = "mean", func = "lin")
head(temp.fa[[1]]$Dataset)

temp.fa.rand <- randwin(repeats = 100, xvar = list(Temp = climate.full$Mean.T),
                        cdate = climate.full$Date, bdate = fa.long$Date,
                        baseline = null.fa, range = c(24, 0), cinterval = "month",
                        type = "absolute", refday = c(31, 12),
                        stat = "mean", func = "lin", cmissing ="method2")
pvalue(dataset = temp.fa[[1]]$Dataset, datasetrand = temp.fa.rand[[1]], metric = "C", sample.size = 9) # p=0.445
plotall(dataset = temp.fa[[1]]$Dataset, datasetrand = temp.fa.rand[[1]],
        bestmodel = temp.fa[[1]]$BestModel, bestmodeldata = temp.fa[[1]]$BestModelData)
# 86% of models within 95% confidence

ggplot(temp.fa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))

ggplot(temp.fa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = ModelBeta)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "darkred", name = "Model ??") + 
  theme_classic() + theme(legend.position = c(0.8,0.275)) + 
  xlab("Window Close (months)") + ylab("Window Open (months)") + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24))


# CRUDE CLIMATE ANALYSIS #

temp.month <- aggregate(cbind(Mean.T, Max.T_imp, Min.T_imp) ~ Cum.Mos, data = climate.full, mean)
plot(data=temp.month, Mean.T~Cum.Mos, xaxp=c(0,170,85))
plot(data=temp.month, Max.T_imp~Cum.Mos, xaxp=c(0,170,85))
plot(data=temp.month, Min.T_imp~Cum.Mos, xaxp=c(0,170,85))



# CLIMATE PLOTS #

install.packages("gridExtra")
library(gtable)
library(grid)


# Fecundity #

p1 <- ggplot(rain.fa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + theme_classic() + 
  theme(legend.position = c(0.9,0.4), legend.key.size=unit(0.4, "cm"), legend.title=element_text(size=10), legend.text=element_text(size=8),
        axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24)) + 
  ggtitle("Rainfall") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

p2 <- ggplot(temp.fa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + theme_classic() + 
  theme(legend.position = c(0.9,0.4), legend.key.size=unit(0.4, "cm"), legend.title=element_text(size=10), legend.text=element_text(size=8),
        axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24)) +
  ggtitle("Temperature") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

grid.arrange(p1, p2, nrow=1, left = textGrob("Window Open (months)", rot = 90, vjust = 0.5, hjust=0.45), bottom = textGrob("Window Close (months)"))


# Temperature #

p3 <- ggplot(temp.sj_all[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc", limits = c(-10,2.5)) + theme_classic() + 
  theme(legend.position = c(0.9,0.4), legend.key.size=unit(0.4, "cm"), legend.title=element_text(size=10), legend.text=element_text(size=8),
        axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24)) + 
  ggtitle("Juvenile") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

p4 <- ggplot(temp.sa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + theme_classic() + 
  theme(legend.position = c(0.9,0.4), legend.key.size=unit(0.4, "cm"), legend.title=element_text(size=10), legend.text=element_text(size=8),
        axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24)) + 
  ggtitle("Adult") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))


grid.arrange(p3, p4, nrow=1, left = textGrob("Window Open (months)", rot = 90, vjust = 0.5, hjust=0.45), bottom = textGrob("Window Close (months)"))


# Rainfall #

p5 <- ggplot(rain.sn2[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + theme_classic() + 
  theme(legend.position = c(0.9,0.4), legend.key.size=unit(0.4, "cm"), legend.title=element_text(size=10), legend.text=element_text(size=8),
        axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24)) + 
  ggtitle("Year 2 Infant") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

p6 <- ggplot(rain.sa[[1]]$Dataset, aes(WindowClose, WindowOpen, fill = deltaAICc)) + 
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue4", mid = "white", high = "black", name = "?? AICc") + theme_classic() + 
  theme(legend.position = c(0.9,0.4), legend.key.size=unit(0.4, "cm"), legend.title=element_text(size=10), legend.text=element_text(size=8),
        axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_x_discrete(limits = c(0,6,12,18,24)) + scale_y_discrete(limits = c(0,6,12,18,24)) +
  ggtitle("Adult") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

grid.arrange(p5, p6, nrow=1, left = textGrob("Window Open (months)", rot = 90, vjust = 0.5, hjust=0.45), bottom = textGrob("Window Close (months)"))


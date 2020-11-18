#######P300 Bayesian Analysis of Latency and Amplitude######
#install.packages('rjags')
#install.packages('BEST')
setwd("D:/Luehr_Onedrive_UVic/OneDrive for Business/Data Analysis/P300L")
library(rjags)
library(BEST)
library(Hmisc)

donnes_Latency <- read.csv('P300_Latency.csv', header = T)
donnes_Amplitude <- read.csv('P300_Amplitude.csv', header = T)

####ANOVA####
#install.packages("ez")
library(ez)

#Convert to Factor
donnes_Latency$Frequency <- as.factor(donnes_Latency$Frequency)
donnes_Amplitude$Frequency <- as.factor(donnes_Amplitude$Frequency)

donnes_Latency$Subject <- as.factor(donnes_Latency$Subject)
donnes_Amplitude$Subject <- as.factor(donnes_Amplitude$Subject)

#Test Assumptions
hist(donnes_Latency$Latency)
hist(donnes_Amplitude$Amplitude)

lat_anova <- ezANOVA(donnes_Latency, dv = Latency, wid = Subject, within = Frequency)
lat_anova

amp_anova <- ezANOVA(donnes_Amplitude, dv = Amplitude, wid = Subject, within = Frequency)
amp_anova
#Rerun with better display output for Post-hoc
require(nlme)         ## for lme()
require(multcomp)  ## for multiple comparison stuff

Aov.mod <- aov(Amplitude ~ Frequency + Subject + Error(Subject/Frequency), data = donnes_Amplitude)
Lme.mod <- lme(Amplitude ~ Frequency + Subject, random = ~1 | Subject/Frequency, data = donnes_Amplitude)

summary(Aov.mod)
anova(Lme.mod)

summary(Lme.mod)
summary(glht(Lme.mod, linfct=mcp(V="Tukey")))

Aov.mod <- aov(Latency ~ Frequency + Subject + Error(Subject/Frequency), data = donnes_Latency)
Lme.mod <- lme(Latency ~ Frequency + Subject, random = ~1 | Subject/Frequency, data = donnes_Latency)

summary(Aov.mod)
anova(Lme.mod)

summary(Lme.mod)
summary(glht(Lme.mod, linfct=mcp(V="Tukey")))

#Basic plot with some 95% CI
lat_an_plot <- ezPlot(donnes_Latency, dv = Latency, wid = Subject, within = Frequency, x = Frequency)
lat_an_plot

amp_an_plot <- ezPlot(donnes_Amplitude, dv = Amplitude, wid = Subject, within = Frequency, x = Frequency)
amp_an_plot

#####BAYESIAN ANOVA####
#Potential Packages
#install.packages("bayesm")
#install.packages("BayesFactor")
library(bayesm)
library(BayesFactor)

#Reminder of traditional rmANOVA values
summary(aov(donnes_Latency$Latency ~ donnes_Latency$Frequency + Error(donnes_Latency$Subject/donnes_Latency$Frequency)), data=donnes_Latency)
latbf = anovaBF(Latency ~ Frequency + Subject, data = donnes_Latency)
latbf
plot(latbf)

plot(latbf[3:4]/latbf[1])
#Frequency + Subject and F+S with interaction model heavily preferred over purely frequency model

#Some clean output bits
#Requires a working TEX (latex) installation
library(Hmisc); library(Formula)
latex(summary(aov(donnes_Latency$Latency ~ donnes_Latency$Frequency + Error(donnes_Latency$Subject/donnes_Latency$Frequency)), data=donnes_Latency))



########################################################################################################################################
####Amplitude####
########################################################################################################################################
summary(aov(donnes_Amplitude$Amplitude ~ donnes_Amplitude$Frequency + Error(donnes_Amplitude$Subject/donnes_Amplitude$Frequency)), data=donnes_Amplitude)
ampbf = anovaBF(Amplitude ~ Frequency + Subject, data = donnes_Amplitude)
ampbf
plot(ampbf)

plot(latbf[3:4]/latbf[1])

####Amplitude by Trial#####

abtdonnes <- read.csv('P300_Amplitude_TBT.csv', header = T) 

#Factor required columns
abtdonnes$Frequency <- as.factor(abtdonnes$Frequency)
abtdonnes$Trial <- as.factor(abtdonnes$Trial)

#summary(aov(abtdonnes$Amplitude ~ abtdonnes$Trial + abtdonnes$Frequency + Error(abtdonnes$Trial/abtdonnes$Frequency)), data=abtdonnes)
abtbf = anovaBF(Amplitude ~ Frequency + Trial, data = abtdonnes)
abtbf
plot(abtbf)

plot(abtbf[3:4]/abtbf[2])

#Trial does not have a significant effect on amplitude.

?BayesFactor

#Truncated data set#
tdon <- abtdonnes[ which(as.numeric(abtdonnes$Trial) <= 5), ]

tdonbf = anovaBF(Amplitude ~ Frequency + Trial, data = tdon)
tdonbf
plot(tdonbf)

plot(tdonbf[3:4]/tdonbf[2])

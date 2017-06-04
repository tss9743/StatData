library(gdata)
library(stats)
library(reshape2)
library(car)
library(sn)


#data set greater than 3
loc <- c(rep("DN",16), rep("SG",20), rep("HL",20))
seas <- c(rep("DRY",54), rep("WET",58))
DO <- c(5.6,5.3,5.3,4.9,3.7,6,5.6,5.5,5.7,6.0,5.5,5.4,3.6,5.9,5.5,5.4,
        5.5,5,4.3,4.2,3.9,3.6,3.6,3.3,5.5,
        5.5,5,4.3,4.2,3.9,3.5,3.1,3.6,5.2,
        5.5,4,5.4,5.8,5.4,6,5.5,5.7,5.9,4.6,5.3,3.5,5.4,
        5.7,5.1,6,5.4,5.7,5.9,4.2,
        6.6,6.2,5.7,5.4,4.7,6.2,6.1,5.9,6.3,6.1,5.6,5.3,4.7,6.4,6,6,
        5.8,5.7,5.1,4.6,4.4,3.2,4.1,3.2,4.2,4.2,6.1,
        5.7,5.7,4.9,4.6,4.6,3.2,4.7,3.3,4.4,3.9,6.2,
        6,5.1,5.9,5.8,5.8,6.4,6.2,6.1,6.2,5.7,
        6.1,5,6.1,6.3,5.9,6.3,6.1,6.1,6.2,5)
YR <- c(rep("2009",8), rep("2010", 8),rep("2009",10), rep("2010", 9),rep("2009",10), rep("2010", 10),rep("2009",8), rep("2010", 8),rep("2009",11), rep("2010", 10),rep("2009",10), rep("2010", 10))
mydata <- data.frame(loc, seas, YR, DO)
h1 = hist(DO, breaks = 144)

#Trying to turn my explanatory variables to not sting so i can use the ":" command
loc = mydata$loc[!is.na(mydata$DO)]
seas = mydata$seas[!is.na(mydata$DO)]
YR = mydata$YR[!is.na(mydata$DO)]
DO = mydata$DO[!is.na(mydata$DO)]

#Fit the linear model
myfit <- lm(DO ~ loc+seas+YR+loc*seas+loc*YR+seas*YR+loc*seas*YR, mydata)
myfitSN <- selm(DO ~ loc+seas+YR+loc*seas+loc*YR+seas*YR+loc*seas*YR, family = "SN", method = "MLE")


#data set less than 3
location <- c(rep("SG",14), rep("HL",4),rep("SG",10), rep("HL",4))
season <- c(rep("DRY",18), rep("WET",14))
Diss <- c(1.5,0,0.2,1.7,2.4,0,0.2,1.6,0,0.2,1.7,2.4,0,0.1,0.8,1.7,0.8,1.7,0,0.6,2.6,0,0.4,0,0.6,2.9,0,0.5,1.7,2.5,1.7,2.5)
Year <- c(rep("2009",7), rep("2010", 7),rep("2009",2), rep("2010", 2),rep("2009",5), rep("2010", 5),rep("2009",2), rep("2010", 2))
mydata2 <- data.frame(location, season, Year, Diss)
h2 = hist(Diss, breaks = 144)

location = mydata2$location[!is.na(mydata2$Diss)]
season = mydata2$season[!is.na(mydata2$Diss)]
Year = mydata2$Year[!is.na(mydata2$Diss)]
Diss = mydata2$Diss[!is.na(mydata2$Diss)]

myfit2 <- lm(Diss ~ location+season+Year+location*season+location*Year+season*Year+location*season*Year, mydata2)

summary(myfit)
aov.out <- aov(myfit)
summary(aov.out)

summary(myfit2)
aov.out2 <- aov(myfit2)
summary(aov.out2)

#standardized residuals
stdRes1 = rstandard(myfit)
stdRes2 = rstandard(myfit2)
stdRes <- c(stdRes1, stdRes2)
#qqplot
qqPlot(stdRes1)
qqPlot(stdRes2)
qqPlot(stdRes)

#Normal Plot on the histogram
#xfit<-seq(min(DO),max(DO),length=40) 
#yfit<-dnorm(xfit,mean=mean(DO),sd=sd(DO)) 
#yfit <- yfit*diff(h1$mids[1:2])*6*length(DO) 
#lines(xfit, yfit, col="blue", lwd=2)

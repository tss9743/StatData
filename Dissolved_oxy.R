library(stats)
library(reshape2)
library(car)

loc <- c(rep("DN",16), rep("SG",32), rep("HL",24))
seas <- c(rep("DRY",72), rep("WET",72))
DO <- c(5.6,5.3,5.3,4.9,3.7,6,5.6,5.5,5.7,6.0,5.5,5.4,3.6,5.9,5.5,5.4,
        5.5,5,4.3,4.2,3.9,1.5,0,3.6,0.2,1.7,2.4,0,0.2,3.6,3.3,5.5,
        5.5,5,4.3,4.2,3.9,1.6,0,3.5,0.2,1.7,2.4,0,0.1,3.1,3.6,5.2,
        5.5,4,5.4,5.8,5.4,6,5.5,5.7,5.9,0.8,1.7,4.6,5.3,3.5,5.4,
        5.7,5.1,6,5.4,5.7,5.9,0.8,1.7,4.2,
        6.6,6.2,5.7,5.4,4.7,6.2,6.1,5.9,6.3,6.1,5.6,5.3,4.7,6.4,6,6,
        5.8,5.7,5.1,4.6,4.4,3.2,0,4.1,0.6,2.6,3.2,0,0.4,4.2,4.2,6.1,
        5.7,5.7,4.9,4.6,4.6,3.2,0,4.7,0.6,2.9,3.3,0,0.5,4.4,3.9,6.2,
        NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
        6.1,5,6.1,6.3,5.9,6.3,6.1,6.1,6.2,1.7,2.5,5)
mydata <- data.frame(loc, seas, DO)

#ACTUAL DATA FRAME...... needed the NA's to make the repeats, now taking them out
location = mydata$loc[!is.na(mydata$DO)]
season = mydata$seas[!is.na(mydata$DO)]
Dissolve = mydata$DO[!is.na(mydata$DO)]
mydataclean <-data.frame(location, season, Dissolve)

#Fit the linear model
myfit <- lm(Dissolve ~ location+season+location*season, mydataclean)


#interaction term
#mydataclean$location:season


#plots
plot(mydataclean$location, mydataclean$Dissolve, xlab = "Location",ylab = "Dissolve")
plot(mydataclean$season, mydataclean$Dissolve, xlab = "Season",ylab = "Dissolve")
plot(mydataclean$location:season, mydataclean$Dissolve, xlab = "Interaction", ylab = "Dissolved Oxygen")

#plot of the avg responses at each treatment combination
interaction.plot(location, season, Dissolve)


#standardized residuals
stdRes = rstandard(myfit)

#standard deviation and mean of std res
sd(stdRes) #will be 1 because we have standardized it
mean(stdRes)  #close to zero becasue standardized
#This does not mean we have a normal fit. though we have (0,1)

#Test of normality
shapiro.test(stdRes)
#pvalue is really low so data not following normality, also, df of residual is really high(126), cant claim normality

#histogram plot of standardized res
h <- hist(stdRes)
#this is skewed to the negative side

#plot the normal curve on the histogram
xfit<-seq(min(stdRes),max(stdRes),length=40) 
yfit<-dnorm(xfit,mean=mean(stdRes),sd=sd(stdRes)) 
yfit <- yfit*diff(h$mids[1:2])*length(stdRes) 
lines(xfit, yfit, col="blue", lwd=2)

#Actual vs predicted surface curve
plot(myfit$fitted.values, mydataclean$Dissolve)
abline(lm(mydataclean$Dissolve ~ myfit$fitted.values))
SumOfFit = summary(lm(mydataclean$Dissolve ~ myfit$fitted.values))

#Residual plot
plot(mydataclean$location, myfit$residuals, xlab = "Location", ylab = "Dissolved Oxygen Residuals")
plot(mydataclean$season, myfit$residuals, xlab = "Season", ylab = "Dissolved Oxygen Residuals")
plot(mydataclean$location:season, myfit$residuals, xlab = "Interaction", ylab = "Dissolved Oxygen Residuals")


#Bartlett test (extremely sensitive to normality. Since our data is not normal, Levenes test might be better)
bartlett.test(Dissolve ~ location, mydataclean)
bartlett.test(Dissolve ~ season, mydataclean)

#trying to do bartlett by interaction
x <- data.frame(location, season, location:season, Dissolve)
bartlett.test(Dissolve ~ location.season, x)


#Levenes test
leveneTest(Dissolve ~ location, mydataclean)
leveneTest(Dissolve ~ season, mydataclean)

#trying to do Levene by interaction
leveneTest(Dissolve ~ location.season, x)

#ANOVA TABLE
aov.out = aov(myfit)
summary(aov.out)

#Tukeys test
TukeyHSD(aov.out)

#plot residuals
plot(myfit)
#qqplot: the dots are way below the 45degree lin, showing negative skewness of the std res
#to check if balanced design : (replications(Dissolve ~ location*season, data = mydataclean))
#counting how many of the stdRes are greater than 0 : length(stdRes[stdRes > 0])
        
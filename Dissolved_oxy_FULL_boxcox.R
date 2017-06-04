library(MASS)
library(car)
library(stats)
library(ggplot2)
library(lattice)


loc <- c(rep("DN",16), rep("SG",32), rep("HL",24))
seas <- c(rep("DRY",72), rep("WET",72))
DO <- c(5.6,5.3,5.3,4.9,3.7,6,5.6,5.5,5.7,6.0,5.5,5.4,3.6,5.9,5.5,5.4,
        5.5,5,4.3,4.2,3.9,1.5,0.01,3.6,0.2,1.7,2.4,0.01,0.2,3.6,3.3,5.5,
        5.5,5,4.3,4.2,3.9,1.6,0.01,3.5,0.2,1.7,2.4,0.01,0.1,3.1,3.6,5.2,
        5.5,4,5.4,5.8,5.4,6,5.5,5.7,5.9,0.8,1.7,4.6,5.3,3.5,5.4,
        5.7,5.1,6,5.4,5.7,5.9,0.8,1.7,4.2,
        6.6,6.2,5.7,5.4,4.7,6.2,6.1,5.9,6.3,6.1,5.6,5.3,4.7,6.4,6,6,
        5.8,5.7,5.1,4.6,4.4,3.2,0.01,4.1,0.6,2.6,3.2,0.01,0.4,4.2,4.2,6.1,
        5.7,5.7,4.9,4.6,4.6,3.2,0.01,4.7,0.6,2.9,3.3,0.01,0.5,4.4,3.9,6.2,
        6,5.1,5.9,5.8,5.8,6.4,6.2,6.1,6.2,1.7,2.5,5.7,
        6.1,5,6.1,6.3,5.9,6.3,6.1,6.1,6.2,1.7,2.5,5)


mydata <- data.frame(loc, seas, DO)



#ACTUAL DATA FRAME...... needed the NA's to make the repeats, now taking them out
location = mydata$loc[!is.na(mydata$DO)]
season = mydata$seas[!is.na(mydata$DO)]
Dissolve = mydata$DO[!is.na(mydata$DO)]
mydataclean <-data.frame(location, season, Dissolve)

y <- boxcox(Dissolve ~ location+season+location*season, data = mydata, lambda = seq(-3, 3, length = 10))
plot <- data.frame(y$y, y$x)
lambda = approx(x = plot[,1], y = plot[,2], xout = max(plot[,1]))

BoxDO = (Dissolve)^(lambda$y)

myfit <- lm(BoxDO ~ location+season+location*season, mydata)
stdRes <- rstandard(myfit)

h <- hist(stdRes)

#plot the normal curve on the histogram
xfit<-seq(min(stdRes),max(stdRes),length=40) 
yfit<-dnorm(xfit,mean=mean(stdRes),sd=sd(stdRes)) 
yfit <- yfit*diff(h$mids[1:2])*length(stdRes) 
lines(xfit, yfit, col="blue", lwd=2)

qqPlot(stdRes)
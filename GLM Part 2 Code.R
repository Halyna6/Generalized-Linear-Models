### PROJECT 2 ###
#################

rm(list=ls())

# Environment
setwd("C:/Users/admin/Dropbox/KU_Leuven/GLM/GLM_GroupProject")

packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}
packages(c("lattice", "car","reshape2", "GGally", "mgcv", "splines", "readr", "psych", "ggplot2", "lattice"))

#import data 
library(readr)
Air_data <- read_delim("C:/Users/admin/Dropbox/KU_Leuven/GLM/GLM_GroupProject/Air_data.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

attach(Air_data)
head(Air_data)

# data structure
str(Air_data)
summary(Air_data)

library(xtable)
xtable(summary(Air_data))

#################
##### Descriptive
  # (absolute) correlation
    panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- abs(cor(x, y))
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste0(prefix, txt)
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = 1.5)
    }
    panel.hist <- function(x, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5) )
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
    }
    pairs(Air_data [], pch = 21, upper.panel = panel.cor, diag.panel = panel.hist)
    
    ggcorr(Air_data, palette = "RdBu", label = TRUE)
    
cor(Air_data)
summary(Air_data)
#vif
oz.VIF <- diag(solve(cor(Air_data)))
oz.VIF

plot(Ozone, Temp)
hist(Ozone)
boxplot(Ozone)
hist(Temp)
boxplot(Temp)

#month of the year
medmonth<-data.frame(aggregate(Air_data[,1], by=list(Air_data$Month), median))
plot(Month, Ozone, xlab = "Month of the Year", ylab = "Ozone Concentration")
lines(medmonth[,1], medmonth[,2], lwd=2)
#observations per month
sum(Month==5) #24
sum(Month==6) #9
sum(Month==7) #26
sum(Month==8) #23
sum(Month==9) #29

#day of the month
meanday<-data.frame(aggregate(Air_data[,1], by=list(Air_data$Day), mean))
plot(Day, Ozone, xlab="Day of the Month", ylab="Ozone Concentration")
lines(meanday[,1], meanday[,2], lwd=2)
    
### Ozone ~ Temp
    # https://datascienceplus.com/cubic-and-smoothing-splines-in-r/
    TempLim <- range(Temp)
    Air_data$ID <- 1: 111
    
    ggplot(Air_data, aes(Temp, Ozone) ) +
      geom_point() +
      stat_smooth() +
      ggtitle ("Relationship between Ozone and Temperature") + theme_light() +
      geom_text(aes(label=ifelse(Ozone>125,as.character(ID),'')),hjust=0,vjust=0) +
      geom_text(aes(label=ifelse(ID==23,as.character(ID),'')),hjust=0,vjust=0)
    # the scatter plot suggest a non-linear relationship between variables
    # possible outliers: 23, 34, 77.
    library(base)
    cook1<-cookd(fit1)
    cook2<-cookd(fit2)
    #cut off 4/n, 0.036
    plot(rownames(Air_data), cook2, xlab = "Observation ID Number", ylab = "Cook's Distance")
    text(rownames(Air_data), cook2,labels=ifelse(cook2>0.036, rownames(Air_data), NA))
    
    
    
    # remove outliers
      detach(Air_data)
      Air_reduced <- Air_data[-c (23, 34, 77),]
      attach(Air_reduced)
      
      ggplot(Air_reduced, aes(Temp, Ozone) ) +
        geom_point() +
        stat_smooth() +
        ggtitle ("Relationship between Ozone and Temperature") + theme_light() +
        geom_text(aes(label=ifelse(Ozone>125,as.character(ID),'')),hjust=0,vjust=0) +
        geom_text(aes(label=ifelse(ID==23,as.character(ID),'')),hjust=0,vjust=0)

#################
# Polynomials
    ggplot(Air_reduced, aes(x = Temp, y = Ozone)) +
      geom_point(color = "black") +
      stat_smooth(method = "lm", formula = y ~ poly(x, 3), aes(color = "2nd"), se = FALSE) +
      stat_smooth(method = "lm", formula = y ~ poly(x, 4), aes(color = "3rd"), se = FALSE) +
      stat_smooth(method = "lm", formula = y ~ poly(x, 5), aes(color = "4th"), se = FALSE) +
      stat_smooth(method = "lm", formula = y ~ poly(x, 6), aes(color = "5th"), se = FALSE)+ 
      scale_colour_manual(name = "Polynomial Degree", 
                          breaks = c("2nd", "3rd", "4th", "5th"),
                          values=c("blue", "purple","red", "green"))+
      theme_light() + ggtitle ("Polynomial Fittings")
      # best fitting: degree 4 
      
      par(mfrow = c (1,4))
      
      #degree 2
      orthpoly <- poly(Air_reduced$Temp, order=2)
      Air_reduced$xo1 <- orthpoly[,1]
      Air_reduced$xo2 <- orthpoly[,2]

      polymodel1 <- lm(Ozone ~ xo1 + xo2, data=Air_reduced)
      Air_reduced$fitted1 <- fitted(polymodel1)

      Air_reduced = Air_reduced[order(Air_reduced$Temp), ]
      plot(Air_reduced$Temp,Air_reduced$Ozone,xlab="Temp",ylab="Ozone",
           cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3,
           main="Polynomial of degree 2", xlim = c(57,97), ylim = c(0,150))
      lines(Air_reduced$Temp, Air_reduced$fitted1,col="blue",lwd=3)
      Temp2<- Temp^2
     
#checking AIC deg=2 versus deg=4
      fit1<- lm(Ozone~Temp+Temp2)
      x<- seq(50, 100, by = .1)
      plot(Temp,Ozone, main="Polynomial regression model",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(x, summary(fit1)$coefficients[1] + summary(fit1)$coefficients[2]*x +
              summary(fit1)$coefficients[3]*x^2, lwd=2)
      n<-nrow(ozdata)
      polyaic<- n*log(summary(fit1)$sigma^2) + 2*(summary(fit1)$df[1]+1)
      polyaic
      
      Temp3<-Temp^3
      Temp4<-Temp^4
      fit2<-lm(Ozone~Temp+Temp2+Temp3+Temp4)
      polyaic<- n*log(summary(fit2)$sigma^2) + 2*(summary(fit2)$df[1]+1)
      polyaic
      
    #degree 3
      orthpoly <- poly(Air_reduced$Temp, order=3)
      Air_reduced$xo1 <- orthpoly[,1]
      Air_reduced$xo2 <- orthpoly[,2]
      Air_reduced$xo3 <- orthpoly[,3]

      polymodel1 <- lm(Ozone ~ xo1 + xo2 + xo3, data=Air_reduced)
      Air_reduced$fitted1 <- fitted(polymodel1)

      Air_reduced = Air_reduced[order(Air_reduced$Temp), ]
      plot(Air_reduced$Temp,Air_reduced$Ozone,xlab="Temp",ylab="Ozone",
           cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3,
           main="Polynomial of degree 3", xlim = c(57,97), ylim = c(0,150))
      lines(Air_reduced$Temp, Air_reduced$fitted1,col="blue",lwd=3)

    # degree 4
      orthpoly <- poly(Air_reduced$Temp, order=4)
      Air_reduced$xo1 <- orthpoly[,1]
      Air_reduced$xo2 <- orthpoly[,2]
      Air_reduced$xo3 <- orthpoly[,3]
      Air_reduced$xo4 <- orthpoly[,4]

      polymodel2 <- lm(Ozone ~ xo1 + xo2 + xo3 + xo4, data=Air_reduced)
      Air_reduced$fitted2 <- fitted(polymodel2)

      Air_reduced = Air_reduced[order(Air_reduced$Temp), ]
      plot(Air_reduced$Temp,Air_reduced$Ozone,xlab="Temp",ylab="Ozone",
           cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3,
           main="Polynomial of degree 4", xlim = c(57,97), ylim = c(0,150))
      lines(Air_reduced$Temp, Air_reduced$fitted2,col="blue",lwd=3)

    # degree 5
      orthpoly <- poly(Air_reduced$Temp, order=5)
      Air_reduced$xo1 <- orthpoly[,1]
      Air_reduced$xo2 <- orthpoly[,2]
      Air_reduced$xo3 <- orthpoly[,3]
      Air_reduced$xo4 <- orthpoly[,4]
      Air_reduced$xo5 <- orthpoly[,5]

      polymodel3 <- lm(Ozone ~ xo1 + xo2 + xo3 + xo4 + xo5, data=Air_reduced)
      Air_reduced$fitted3 <- fitted(polymodel3)

      Air_reduced = Air_reduced[order(Air_reduced$Temp), ]
      plot(Air_reduced$Temp,Air_reduced$Ozone,xlab="Temp",ylab="Ozone",
           cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3,
           main="Polynomial of degree 5", xlim = c(57,97), ylim = c(0,150))
      lines(Air_reduced$Temp, Air_reduced$fitted3,col="blue",lwd=3)

    # degree 6
      orthpoly <- poly(Air_reduced$Temp, order=6)
      Air_reduced$xo1 <- orthpoly[,1]
      Air_reduced$xo2 <- orthpoly[,2]
      Air_reduced$xo3 <- orthpoly[,3]
      Air_reduced$xo4 <- orthpoly[,4]
      Air_reduced$xo5 <- orthpoly[,5]
      Air_reduced$xo6 <- orthpoly[,6]

      polymodel4 <- lm(Ozone ~ xo1 + xo2 + xo3 + xo4 + xo5 + xo6, data=Air_reduced)
      Air_reduced$fitted4 <- fitted(polymodel4)

      Air_reduced = Air_reduced[order(Air_reduced$Temp), ]
      plot(Air_reduced$Temp,Air_reduced$Ozone,xlab="Temp",ylab="Ozone",
           cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3,
           main="Polynomial of degree 6", xlim = c(57,97), ylim = c(0,150))
      lines(Air_reduced$Temp, Air_reduced$fitted4,col="blue",lwd=3)
 
##############################           
# Polynomial Spline - Truncated Power Series (d=degree(smoothness), k=knots(flexibility))
      rm(list=ls())
      Air_data <- read_delim("C:/Users/admin/Dropbox/KU_Leuven/GLM/GLM_GroupProject/Air_data.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)
      Air_reduced <- Air_data[-c (23, 34, 77),]
      attach(Air_reduced)

      par(mfrow = c (3,3))
      
    # knots = 3, splinegrad = 2
      knots <- c(57,77,97)
      b1<- (Temp-knots[2])^2*(Temp > knots[2])
      Temp2<- Temp^2
      
      fit3.2<- lm(Ozone~Temp+Temp2+b1)
      
      temp_eval <- seq(57,97,length=108)
      b_eval <- matrix(0,108,4)
      b_eval[,1] <- rep(1,108)
      b_eval[,2] <- temp_eval
      b_eval[,3] <- temp_eval^2
      b_eval[,4] <- (temp_eval-knots[2])^2*(temp_eval > knots[2])
      
      fitted3.2 <- round(b_eval%*%coef(fit3.2),5)
      
      plot(Temp,Ozone, main="TP splines: d=2,k=3 (AIC:609.9536)",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(temp_eval,fitted3.2,lwd=2, col="blue")
      
      # AIC
        n <- length(Ozone)
        tp3.2aic<- n*log(summary(fit3.2)$sigma^2) + 2*(summary(fit3.2)$df[1]+1)
        tp3.2aic
        # 609.9536
        
      # knots = 5, splinegrad = 2 +++ best fit +++
      knots <- c(57,67,77,87,97)
      b1<- (Temp-knots[2])^2*(Temp > knots[2])
      b2<- (Temp-knots[3])^2*(Temp > knots[3])
      b3<- (Temp-knots[4])^2*(Temp > knots[4])
      
      Temp2<- Temp^2
      
      fit5.2<- lm(Ozone~Temp+Temp2+b1+b2+b3)
      
      temp_eval <- seq(57,97,length=108)
      b_eval <- matrix(0,108,6)
      b_eval[,1] <- rep(1,108)
      b_eval[,2] <- temp_eval
      b_eval[,3] <- temp_eval^2
      b_eval[,4] <- (temp_eval-knots[2])^2*(temp_eval > knots[2])
      b_eval[,5] <- (temp_eval-knots[3])^2*(temp_eval > knots[3])
      b_eval[,6] <- (temp_eval-knots[4])^2*(temp_eval > knots[4])
      
      fitted5.2 <- round(b_eval%*%coef(fit5.2),5)
      
      plot(Temp,Ozone, main="TP splines: d=2, k=5, (AIC: 592.7144) ",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(temp_eval,fitted5.2,lwd=2, col="blue")
      
      # AIC
        n <- length(Ozone)
        tp5.2aic<- n*log(summary(fit5.2)$sigma^2) + 2*(summary(fit5.2)$df[1]+1)
        tp5.2aic
        # 592.7144
        
      # knots = 9, splinegrad = 2
      knots <- c(57,62,67,72,77,82,87,92,97)
      b1<- (Temp-knots[2])^2*(Temp > knots[2])
      b2<- (Temp-knots[3])^2*(Temp > knots[3])
      b3<- (Temp-knots[4])^2*(Temp > knots[4])
      b4<- (Temp-knots[5])^2*(Temp > knots[5])
      b5<- (Temp-knots[6])^2*(Temp > knots[6])
      b6<- (Temp-knots[7])^2*(Temp > knots[7])
      b7<- (Temp-knots[8])^2*(Temp > knots[8])
      
      Temp2<- Temp^2
      
      fit9.2<- lm(Ozone~Temp+Temp2+b1+b2+b3+b4+b5+b6+b7)
      
      temp_eval <- seq(57,97,length=108)
      b_eval <- matrix(0,108,10)
      b_eval[,1] <- rep(1,108)
      b_eval[,2] <- temp_eval
      b_eval[,3] <- temp_eval^2
      b_eval[,4] <- (temp_eval-knots[2])^2*(temp_eval > knots[2])
      b_eval[,5] <- (temp_eval-knots[3])^2*(temp_eval > knots[3])
      b_eval[,6] <- (temp_eval-knots[4])^2*(temp_eval > knots[4])
      b_eval[,7] <- (temp_eval-knots[5])^2*(temp_eval > knots[5])
      b_eval[,8] <- (temp_eval-knots[6])^2*(temp_eval > knots[6])
      b_eval[,9] <- (temp_eval-knots[7])^2*(temp_eval > knots[7])
      b_eval[,10] <- (temp_eval-knots[8])^2*(temp_eval > knots[8])
      
      fitted9.2 <- round(b_eval%*%coef(fit9.2),5)
      
      plot(Temp,Ozone, main="TP splines: d=2, k=9, (AIC:600.5719)",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(temp_eval,fitted9.2,lwd=2, col="blue")
      
      # AIC
        n <- length(Ozone)
        tp9.2aic<- n*log(summary(fit9.2)$sigma^2) + 2*(summary(fit9.2)$df[1]+1)
        tp9.2aic
        # 600.5719
      
    # knots = 3, splinegrad = 3
      knots <- c(57,77,97)
      Temp2<- Temp^2
      Temp3 <- Temp^3
      
      b1<- (Temp-knots[2])^3*(Temp > knots[2])
      
      fit3.3<- lm(Ozone~Temp+Temp2+Temp3+b1)
      
      temp_eval <- seq(57,97,length=108)
      b_eval <- matrix(0,108,5)
      b_eval[,1] <- rep(1,108)
      b_eval[,2] <- temp_eval
      b_eval[,3] <- temp_eval^2
      b_eval[,4] <- temp_eval^3
      b_eval[,5] <- (temp_eval-knots[2])^3*(temp_eval > knots[2])

      fitted3.3 <- round(b_eval%*%coef(fit3.3),5)
      
      plot(Temp, Ozone, main="TP splines: d=3, k=3, (AIC: 594.756)",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(temp_eval,fitted3.3,lwd=2, col="purple")
      
      # AIC
        n <- length(Ozone)
        tp3.3aic<- n*log(summary(fit3.3)$sigma^2) + 2*(summary(fit3.3)$df[1]+1)
        tp3.3aic
        # 594.756
        
      # knots = 5, splinegrad = 3 
      knots <- c(57,67,77,87,97)
      b1<- (Temp-knots[2])^3*(Temp > knots[2])
      b2<- (Temp-knots[3])^3*(Temp > knots[3])
      b3<- (Temp-knots[4])^3*(Temp > knots[4])
      
      Temp2<- Temp^2
      Temp3 <- Temp^3
      
      fit5.3<- lm(Ozone~Temp+Temp2+Temp3+b1+b2+b3)
      
      temp_eval <- seq(57,97,length=108)
      b_eval <- matrix(0,108,7)
      b_eval[,1] <- rep(1,108)
      b_eval[,2] <- temp_eval
      b_eval[,3] <- temp_eval^2
      b_eval[,4] <- temp_eval^3
      b_eval[,5] <- (temp_eval-knots[2])^3*(temp_eval > knots[2])
      b_eval[,6] <- (temp_eval-knots[3])^3*(temp_eval > knots[3])
      b_eval[,7] <- (temp_eval-knots[4])^3*(temp_eval > knots[4])
      
      fitted5.3 <- round(b_eval%*%coef(fit5.3),5)
      
      plot(Temp,Ozone, main="TP splines: d=3, k=5, (AIC:596.2891) ",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(temp_eval,fitted5.3,lwd=2, col="purple")
      
      # AIC
        n <- length(Ozone)
        tp5.3aic<- n*log(summary(fit5.3)$sigma^2) + 2*(summary(fit5.3)$df[1]+1)
        tp5.3aic
        # 596.2891
        
        # knots = 9, splinegrad = 3
      knots <- c(57,62,67,72,77,82,87,92,97)
      b1<- (Temp-knots[2])^3*(Temp > knots[2])
      b2<- (Temp-knots[3])^3*(Temp > knots[3])
      b3<- (Temp-knots[4])^3*(Temp > knots[4])
      b4<- (Temp-knots[5])^3*(Temp > knots[5])
      b5<- (Temp-knots[6])^3*(Temp > knots[6])
      b6<- (Temp-knots[7])^3*(Temp > knots[7])
      b7<- (Temp-knots[8])^3*(Temp > knots[8])
      
      Temp2<- Temp^2
      Temp3 <- Temp^3
      
      fit9.3<- lm(Ozone~Temp+Temp2+Temp3+b1+b2+b3+b4+b5+b6+b7)
      
      temp_eval <- seq(57,97,length=108)
      b_eval <- matrix(0,108,11)
      b_eval[,1] <- rep(1,108)
      b_eval[,2] <- temp_eval
      b_eval[,3] <- temp_eval^2
      b_eval[,4] <- temp_eval^3
      b_eval[,5] <- (temp_eval-knots[2])^3*(temp_eval > knots[2])
      b_eval[,6] <- (temp_eval-knots[3])^3*(temp_eval > knots[3])
      b_eval[,7] <- (temp_eval-knots[4])^3*(temp_eval > knots[4])
      b_eval[,8] <- (temp_eval-knots[5])^3*(temp_eval > knots[5])
      b_eval[,9] <- (temp_eval-knots[6])^3*(temp_eval > knots[6])
      b_eval[,10] <- (temp_eval-knots[7])^3*(temp_eval > knots[7])
      b_eval[,11] <- (temp_eval-knots[8])^3*(temp_eval > knots[8])
      
      fitted9.3 <- round(b_eval%*%coef(fit9.3),5)
      
      plot(Temp,Ozone, main="TP splines: d=3, k= 9, (AIC: 605.1476) ",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(temp_eval,fitted9.3,lwd=2, col="purple")
      
      # AIC
        n <- length(Ozone)
        tp9.3aic<- n*log(summary(fit9.3)$sigma^2) + 2*(summary(fit9.3)$df[1]+1)
        tp9.3aic
        # 605.1476
    

    # knots = 3, splinegrad = 4
      knots <- c(57,77,97)
      Temp2<- Temp^2
      Temp3 <- Temp^3
      Temp4 <- Temp^4

      b1<- (Temp-knots[2])^4*(Temp > knots[2])

      fit4.3<- lm(Ozone~Temp+Temp2+Temp3+Temp4+b1)

      temp_eval <- seq(57,97,length=108)
      b_eval <- matrix(0,108,6)
      b_eval[,1] <- rep(1,108)
      b_eval[,2] <- temp_eval
      b_eval[,3] <- temp_eval^2
      b_eval[,4] <- temp_eval^3
      b_eval[,5] <- temp_eval^4
      b_eval[,6] <- (temp_eval-knots[2])^4*(temp_eval > knots[2])

      fitted4.3 <- round(b_eval%*%coef(fit4.3),5)

      plot(Temp, Ozone, main="TP splines: d=4, k=3, (AIC = 593.244)",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(temp_eval,fitted3.3,lwd=2, col="red")

      # AIC
        n <- length(Ozone)
        tp4.3aic<- n*log(summary(fit4.3)$sigma^2) + 2*(summary(fit4.3)$df[1]+1)
        tp4.3aic
        # 593.244

    # knots = 5, splinegrad = 4
      knots <- c(57,67,77, 87,97)
      Temp2<- Temp^2
      Temp3 <- Temp^3
      Temp4 <- Temp^4

      b1<- (Temp-knots[2])^4*(Temp > knots[2])
      b2<- (Temp-knots[3])^4*(Temp > knots[3])
      b3<- (Temp-knots[4])^4*(Temp > knots[4])

      fit4.5<- lm(Ozone~Temp+Temp2+Temp3+Temp4+b1+b2+b3)

      temp_eval <- seq(57,97,length=108)
      b_eval <- matrix(0,108,8)
      b_eval[,1] <- rep(1,108)
      b_eval[,2] <- temp_eval
      b_eval[,3] <- temp_eval^2
      b_eval[,4] <- temp_eval^3
      b_eval[,5] <- temp_eval^4
      b_eval[,6] <- (temp_eval-knots[2])^4*(temp_eval > knots[2])
      b_eval[,7] <- (temp_eval-knots[3])^4*(temp_eval > knots[3])
      b_eval[,8] <- (temp_eval-knots[4])^4*(temp_eval > knots[4])

      fitted4.5 <- round(b_eval%*%coef(fit4.5),5)

      plot(Temp, Ozone, main="TP splines: d=4, k=5, (AIC: 599.2603)",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(temp_eval,fitted3.3,lwd=2, col="red")

      # AIC
        n <- length(Ozone)
        tp4.5aic<- n*log(summary(fit4.5)$sigma^2) + 2*(summary(fit4.5)$df[1]+1)
        tp4.5aic
        # 599.2603

    # knots = 9, splinegrad = 4
      knots <- c(57, 62, 67, 72, 77, 82, 87, 92, 97)
      Temp2<- Temp^2
      Temp3 <- Temp^3
      Temp4 <- Temp^4

      b1<- (Temp-knots[2])^4*(Temp > knots[2])
      b2<- (Temp-knots[3])^4*(Temp > knots[3])
      b3<- (Temp-knots[4])^4*(Temp > knots[4])
      b4<- (Temp-knots[5])^4*(Temp > knots[5])
      b5<- (Temp-knots[7])^4*(Temp > knots[7])
      b6<- (Temp-knots[8])^4*(Temp > knots[8])

      fit4.9<- lm(Ozone~Temp+Temp2+Temp3+Temp4+b1+b2+b3+b4+b5+b6)
      summary(fit4.9)

      temp_eval <- seq(57,97,length=108)
      b_eval <- matrix(0,108,12)
      b_eval[,1] <- rep(1,108)
      b_eval[,2] <- temp_eval
      b_eval[,3] <- temp_eval^2
      b_eval[,4] <- temp_eval^3
      b_eval[,5] <- temp_eval^4
      b_eval[,6] <- (temp_eval-knots[2])^4*(temp_eval > knots[2])
      b_eval[,7] <- (temp_eval-knots[3])^4*(temp_eval > knots[3])
      b_eval[,8] <- (temp_eval-knots[4])^4*(temp_eval > knots[4])
      b_eval[,9] <- (temp_eval-knots[5])^4*(temp_eval > knots[5])
      b_eval[,10] <- (temp_eval-knots[6])^4*(temp_eval > knots[6])
      b_eval[,11] <- (temp_eval-knots[7])^4*(temp_eval > knots[7])
      b_eval[,12] <- (temp_eval-knots[8])^4*(temp_eval > knots[8])

      fitted4.9 <- round(b_eval%*%coef(fit4.9),5)

      plot(Temp, Ozone, main="TP splines: d=4, k=9, (AIC = 607.3286)",
           cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
      lines(temp_eval,fitted3.3,lwd=2, col="red")

      # AIC
        n <- length(Ozone)
        tp4.5aic<- n*log(summary(fit4.9)$sigma^2) + 2*(summary(fit4.9)$df[1]+1)
        tp4.5aic
        # 607.3286

#################
# B-splines
par(mfrow = c(2,3))
  # degree = 2, 3 knots
    nrknots <- 3
    minx <- min(Temp)-0.001
    maxx <- max(Temp)+0.001
    step <- (maxx-minx)/(nrknots-1)
    inner.knots <- seq(minx,maxx,length=nrknots)
    knots <- seq(minx-2*step,maxx+2*step,by=step)
    
    xseq <- seq(minx,maxx,length=500)
    
    B <- spline.des(knots=knots,Temp,ord=3)$design
        ?spline.des # ord = number of coefficients in each piecewise polynomial segment, thus a cubic spline has order 4
    Bfit2.3 <- spline.des(knots=knots,xseq,ord=3)$design
    
    betahat <- solve(t(B)%*%B)%*%t(B)%*%Ozone
    fitted <- Bfit2.3%*%betahat
    
    plot(Temp, Ozone,main="B-splines: d=2, k=3, (AIC:605.8777)",
         cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
    lines(xseq,fitted,lwd=2,col="blue")

    #calculate AIC
      n <- nrow(Air_reduced)
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      bspline2.3aic <- n*log(sigma2) + 2*(df+1)
      bspline2.3aic
      # 605.8777

  # degree = 2, knots = 5 ++++ BEST FIT +++
    nrknots <- 5
    minx <- min(Temp)-0.001
    maxx <- max(Temp)+0.001
    step <- (maxx-minx)/(nrknots-1)
    inner.knots <- seq(minx,maxx,length=nrknots)
    knots <- seq(minx-2*step,maxx+2*step,by=step)
    
    xseq <- seq(minx,maxx,length=500)
    
    B <- spline.des(knots=knots,Temp,ord=3)$design
    Bfit2.5 <- spline.des(knots=knots,xseq,ord=3, outer.ok = TRUE)$design
    
    betahat <- solve(t(B)%*%B)%*%t(B)%*%Ozone
    fitted <- Bfit2.5%*%betahat
    
    plot(Temp, Ozone,main="B-splines: d=2, k=5, AIC(586.5409)",
         cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
    lines(xseq,fitted,lwd=2,col="blue")
    
    #calculate AIC
      n <- nrow(Air_reduced)
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      bspline2.5aic <- n*log(sigma2) + 2*(df+1)
      bspline2.5aic
      # 586.5409
      
  # degree = 2, knots = 9
    nrknots <- 9
    minx <- min(Temp)-0.001
    maxx <- max(Temp)+0.001
    step <- (maxx-minx)/(nrknots-1)
    inner.knots <- seq(minx,maxx,length=nrknots)
    knots <- seq(minx-2*step,maxx+2*step,by=step)
    
    xseq <- seq(minx,maxx,length=500)
    
    B <- spline.des(knots=knots,Temp,ord=3)$design
    Bfit2.9 <- spline.des(knots=knots,xseq,ord=3,outer.ok=T)$design
    
    betahat <- solve(t(B)%*%B)%*%t(B)%*%Ozone
    fitted <- Bfit2.9%*%betahat
    
    plot(Temp, Ozone,main="B-splines, d=2, k=9, (AIC:592.0785)",
         cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
    lines(xseq,fitted,lwd=2,col="blue")

    #calculate AIC
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      bspline2.9aic <- n*log(sigma2) + 2*(df+1)
      bspline2.9aic
      # 592.0785
      
   # degree = 3, knots = 3
    nrknots <- 3
    minx <- min(Temp)-0.001
    maxx <- max(Temp)+0.001
    step <- (maxx-minx)/(nrknots-1)
    inner.knots <- seq(minx,maxx,length=nrknots)
    knots <- seq(minx-3*step,maxx+3*step,by=step)
    
    xseq <- seq(minx,maxx,length=500)
    
    B <- spline.des(knots=knots,Temp,ord=4,outer.ok = TRUE)$design
        ?spline.des
    Bfit3.3 <- spline.des(knots=knots,xseq,ord=4,outer.ok = TRUE)$design
    
    betahat <- solve(t(B)%*%B)%*%t(B)%*%Ozone
    fitted <- Bfit3.3%*%betahat
    
    plot(Temp, Ozone,main="B-splines, d=3, k=3(AIC:589.6366)",
         cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
    lines(xseq,fitted,lwd=2,col="purple")

    #calculate AIC
      n <- nrow(Air_reduced)
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      bspline3.3aic <- n*log(sigma2) + 2*(df+1)
      bspline3.3aic
      # 589.6366

  # degree = 3, knots = 5 +++ best fit +++
    nrknots <- 5
    minx <- min(Temp)-0.001
    maxx <- max(Temp)+0.001
    step <- (maxx-minx)/(nrknots-1)
    inner.knots <- seq(minx,maxx,length=nrknots)
    knots <- seq(minx-3*step,maxx+3*step,by=step)
    
    xseq <- seq(minx,maxx,length=500)
    
    B <- spline.des(knots=knots,Temp,ord=4,outer.ok = TRUE)$design
    Bfit3.5 <- spline.des(knots=knots,xseq,ord=4, outer.ok = TRUE)$design
    
    betahat <- solve(t(B)%*%B)%*%t(B)%*%Ozone
    fitted <- Bfit3.5%*%betahat
    
    plot(Temp, Ozone,main="B-splines, d=3, k=5, (AIC:589.0521)",
         cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
    lines(xseq,fitted,lwd=2,col="purple")
    
    #calculate AIC
      n <- nrow(Air_reduced)
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      bspline3.5aic <- n*log(sigma2) + 2*(df+1)
      bspline3.5aic
      # 589.0521
      
  # degree = 3, knots = 9
    nrknots <- 9
    minx <- min(Temp)-0.001
    maxx <- max(Temp)+0.001
    step <- (maxx-minx)/(nrknots-1)
    inner.knots <- seq(minx,maxx,length=nrknots)
    knots <- seq(minx-3*step,maxx+3*step,by=step)
    
    xseq <- seq(minx,maxx,length=500)
    
    B <- spline.des(knots=knots,Temp,ord=4,outer.ok=T)$design
    Bfit3.9 <- spline.des(knots=knots,xseq,ord=4,outer.ok=T)$design
    
    betahat <- solve(t(B)%*%B)%*%t(B)%*%Ozone
    fitted <- Bfit3.9%*%betahat
    
    plot(Temp, Ozone,main="B-splines, d=3, k=9, (AIC:593.5464)",
         cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
    lines(xseq,fitted,lwd=2,col="purple")

    #calculate AIC
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      bspline3.9aic <- n*log(sigma2) + 2*(df+1)
      bspline3.9aic
      # 593.5464
      
  #  # degree = 4, knots = 3
  #   nrknots <- 3
  #   minx <- min(Temp)-0.001
  #   maxx <- max(Temp)+0.001
  #   step <- (maxx-minx)/(nrknots-1)
  #   inner.knots <- seq(minx,maxx,length=nrknots)
  #   knots <- seq(minx-3*step,maxx+3*step,by=step)
  #   
  #   xseq <- seq(minx,maxx,length=500)
  #   
  #   B <- spline.des(knots=knots,Temp,ord=5,outer.ok = TRUE)$design
  #       ?spline.des
  #   Bfit4.3 <- spline.des(knots=knots,xseq,ord=5,outer.ok = TRUE)$design
  #   
  #   betahat <- solve(t(B)%*%B)%*%t(B)%*%Ozone
  #   fitted <- Bfit4.3%*%betahat
  #   
  #   plot(Temp, Ozone,main="B-splines, degree = 4, knots = 3",
  #        cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
  #   lines(xseq,fitted,lwd=2)
  # 
  #   #calculate AIC
  #     n <- nrow(Air_reduced)
  #     S<- B%*%solve(t(B)%*%B)%*%t(B)
  #     fit <- as.vector(S%*%Ozone)
  #     diags <- diag(S)
  #     df <- sum(diags)
  #     sigma2 <- sum((Ozone-fit)^2)/n
  #     bspline4.3aic <- n*log(sigma2) + 2*(df+1)
  #     bspline4.3aic
  #     # 600.6986
  # 
  # # degree = 4, knots = 5
  #   nrknots <- 5
  #   minx <- min(Temp)-0.001
  #   maxx <- max(Temp)+0.001
  #   step <- (maxx-minx)/(nrknots-1)
  #   inner.knots <- seq(minx,maxx,length=nrknots)
  #   knots <- seq(minx-3*step,maxx+3*step,by=step)
  #   
  #   xseq <- seq(minx,maxx,length=500)
  #   
  #   B <- spline.des(knots=knots,Temp,ord=5,outer.ok = TRUE)$design
  #   Bfit4.5 <- spline.des(knots=knots,xseq,ord=5, outer.ok = TRUE)$design
  #   
  #   betahat <- solve(t(B)%*%B)%*%t(B)%*%Ozone
  #   fitted <- Bfit4.5%*%betahat
  #   
  #   plot(Temp, Ozone,main="B-splines, degree = 4, knots = 5",
  #        cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
  #   lines(xseq,fitted,lwd=2)
  #   
  #   #calculate AIC ||| *** best fitting setting *** |||
  #     n <- nrow(Air_reduced)
  #     S<- B%*%solve(t(B)%*%B)%*%t(B)
  #     fit <- as.vector(S%*%Ozone)
  #     diags <- diag(S)
  #     df <- sum(diags)
  #     sigma2 <- sum((Ozone-fit)^2)/n
  #     bspline4.5aic <- n*log(sigma2) + 2*(df+1)
  #     bspline4.5aic
  #     # 587.4351
  #     
  # # degree = 4, knots = 9
  #   nrknots <- 9
  #   minx <- min(Temp)-0.001
  #   maxx <- max(Temp)+0.001
  #   step <- (maxx-minx)/(nrknots-1)
  #   inner.knots <- seq(minx,maxx,length=nrknots)
  #   knots <- seq(minx-4*step,maxx+4*step,by=step) # ???
  #   
  #   xseq <- seq(minx,maxx,length=500)
  #   
  #   B <- spline.des(knots=knots,Temp,ord=5,outer.ok=T)$design
  #   Bfit4.9 <- spline.des(knots=knots,xseq,ord=5,outer.ok=T)$design
  #   
  #   betahat <- solve(t(B)%*%B)%*%t(B)%*%Ozone
  #   fitted <- Bfit4.9%*%betahat
  #   
  #   plot(Temp, Ozone,main="B-splines, degree = 4, knots = 9",
  #        cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
  #   lines(xseq,fitted,lwd=2)
  # 
  #   #calculate AIC
  #     S<- B%*%solve(t(B)%*%B)%*%t(B)
  #     fit <- as.vector(S%*%Ozone)
  #     diags <- diag(S)
  #     df <- sum(diags)
  #     sigma2 <- sum((Ozone-fit)^2)/n
  #     bspline4.9aic <- n*log(sigma2) + 2*(df+1)
  #     bspline4.9aic
  #     # 594.5097

# P-splines 
###########
      
      rm(list=ls())
      Air_data <- read_delim("C:/Users/admin/Dropbox/KU_Leuven/GLM/GLM_GroupProject/Air_data.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)
      Air_reduced <- Air_data[-c (23, 34, 77),]
      attach(Air_reduced)
      rm(Air_data)
 
# 3 knots      
nrknots <- 3
minx <- min(Temp)-0.001
maxx <- max(Temp)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)
xseq_k3 <- seq(minx+0.001,maxx-0.001,length=108)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1)){
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}

D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots){
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}

K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Temp,ord=4)$design
lambda <- seq(0.0005,0.05,length=250)

gcv <- rep(0,length(lambda))
aic <- rep(0,length(lambda))
bic <- rep(0,length(lambda))

n <- nrow(Air_reduced)

require(psych)
S <- B%*%solve(t(B)%*%B)%*%t(B)
fit <- as.vector(S%*%Ozone)
sigma2 <- sum((Ozone-fit)^2)/(n-tr(S))
for(i in 1:length(lambda)){
        S <- B%*%solve(t(B)%*%B + lambda[i]*K2)%*%t(B)
         diags <- diag(S)
         trs <- mean(diags)
         df <- sum(diags)
         fit <- as.vector(S%*%Ozone)
         gcv[i] <- mean(((Ozone-fit)/(1-trs))^2)
         #  aic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + 2*df
         #  bic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + log(n)*df
         sigma2 <- sum((Ozone-fit)^2)/n
         aic[i] <- n*log(sigma2) + 2*(df+1)
         bic[i] <- n*log(sigma2) + log(n)*(df+1)
}

 par(mfrow=c(1,2))
 plot(lambda,gcv,type="l",lwd=2,
            xlab="lambda",ylab="GCV",main="3 knots",
            cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
 plot(lambda,aic,type="l",lwd=2,main="3 knots",
            xlab="lambda",ylab="AIC",
            cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
 lambdamingcv <- lambda[which(gcv==min(gcv))]
 lambdaminaic <- lambda[which(aic==min(aic))]
 lambdaminbic <- lambda[which(bic==min(bic))]
 lambdamingcv;lambdaminaic;lambdaminbic    
 
 # Fit model
optlambda <- lambdaminaic
xseq_k3 <- seq(minx+0.001,maxx-0.001,length=108)

P <- spline.des(knots=knots,Temp,ord=4, outer.ok = TRUE)$design
Pfit <- spline.des(knots=knots,xseq_k3,ord=4, outer.ok = TRUE)$design

betahat <- solve(t(P)%*%P + optlambda*K2)%*%t(P)%*%Ozone
fitted_k3 <- Pfit%*%betahat

# Graphical representation
k3 <- ggplot(data = Air_reduced, 
         aes(x=Temp, y=Ozone, colour='Original data points')) + 
  geom_point() + scale_colour_hue(h = c(180,0)) + 
  geom_line(aes(x=xseq_k3, y=fitted_k3),colour='black')+
  labs(x="Temperature (Fahrenheit)",y="Ozone") +
  scale_colour_manual("", breaks = c("Data",  "fitted_k3"),values = c("green", "black")) +
  theme_light()
k3

    #calculate AIC
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      pspline_k3 <- n*log(sigma2) + 2*(df+1)
      pspline_k3
      # 589.6366

#5knots 
nrknots <- 5
minx <- min(Temp)-0.001
maxx <- max(Temp)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)
xseq_k5 <- seq(minx+0.001,maxx-0.001,length=108)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1)){
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}

D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots){
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}

K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Temp,ord=4)$design
lambda <- seq(0.0005,0.05,length=250)

gcv <- rep(0,length(lambda))
aic <- rep(0,length(lambda))
bic <- rep(0,length(lambda))

n <- nrow(Air_reduced)

require(psych)
S <- B%*%solve(t(B)%*%B)%*%t(B)
fit <- as.vector(S%*%Ozone)
sigma2 <- sum((Ozone-fit)^2)/(n-tr(S))
for(i in 1:length(lambda)){
        S <- B%*%solve(t(B)%*%B + lambda[i]*K2)%*%t(B)
         diags <- diag(S)
         trs <- mean(diags)
         df <- sum(diags)
         fit <- as.vector(S%*%Ozone)
         gcv[i] <- mean(((Ozone-fit)/(1-trs))^2)
         #  aic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + 2*df
         #  bic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + log(n)*df
         sigma2 <- sum((Ozone-fit)^2)/n
         aic[i] <- n*log(sigma2) + 2*(df+1)
         bic[i] <- n*log(sigma2) + log(n)*(df+1)
}

 par(mfrow=c(1,2))
 plot(lambda,gcv,type="l",lwd=2,
            xlab="lambda",ylab="GCV",main="5 knots",
            cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
 plot(lambda,aic,type="l",lwd=2,main="5 knots",
            xlab="lambda",ylab="AIC",
            cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
 lambdamingcv <- lambda[which(gcv==min(gcv))]
 lambdaminaic <- lambda[which(aic==min(aic))]
 lambdaminbic <- lambda[which(bic==min(bic))]
 lambdamingcv;lambdaminaic;lambdaminbic

  # Fit model
optlambda <- lambdaminaic
xseq_k5 <- seq(minx+0.001,maxx-0.001,length=108)

P <- spline.des(knots=knots,Temp,ord=4, outer.ok = TRUE)$design
Pfit <- spline.des(knots=knots,xseq_k5,ord=4, outer.ok = TRUE)$design

betahat <- solve(t(P)%*%P + optlambda*K2)%*%t(P)%*%Ozone
fitted_k5 <- Pfit%*%betahat

# Graphical representation
k5 <- ggplot(data = Air_reduced, 
         aes(x=Temp, y=Ozone, colour='Original data points')) + 
  geom_point() + scale_colour_hue(h = c(180,0)) + 
  geom_line(aes(x=xseq_k5, y=fitted_k5),colour='black')+
  labs(x="Temperature (Fahrenheit)",y="Ozone") +
  scale_colour_manual("", breaks = c("Data",  "fitted_k5"),values = c("green", "black")) +
  theme_light()
k5

    #calculate AIC
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      pspline_k5 <- n*log(sigma2) + 2*(df+1)
      pspline_k5
      #589.0521

#8 knots 
nrknots <- 8
minx <- min(Temp)-0.001
maxx <- max(Temp)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)
xseq_k8_k8 <- seq(minx+0.001,maxx-0.001,length=108)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1)){
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}

D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots){
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}

K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Temp,ord=4)$design
lambda <- seq(0.0005,0.05,length=250)

gcv <- rep(0,length(lambda))
aic <- rep(0,length(lambda))
bic <- rep(0,length(lambda))

n <- nrow(Air_reduced)

require(psych)
S <- B%*%solve(t(B)%*%B)%*%t(B)
fit <- as.vector(S%*%Ozone)
sigma2 <- sum((Ozone-fit)^2)/(n-tr(S))
for(i in 1:length(lambda)){
        S <- B%*%solve(t(B)%*%B + lambda[i]*K2)%*%t(B)
         diags <- diag(S)
         trs <- mean(diags)
         df <- sum(diags)
         fit <- as.vector(S%*%Ozone)
         gcv[i] <- mean(((Ozone-fit)/(1-trs))^2)
         #  aic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + 2*df
         #  bic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + log(n)*df
         sigma2 <- sum((Ozone-fit)^2)/n
         aic[i] <- n*log(sigma2) + 2*(df+1)
         bic[i] <- n*log(sigma2) + log(n)*(df+1)
}

 par(mfrow=c(1,2))
 plot(lambda,gcv,type="l",lwd=2,
            xlab="lambda",ylab="GCV",main="8 knots",
            cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
 plot(lambda,aic,type="l",lwd=2,main="8 knots",
            xlab="lambda",ylab="AIC",
            cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
 lambdamingcv <- lambda[which(gcv==min(gcv))]
 lambdaminaic <- lambda[which(aic==min(aic))]
 lambdaminbic <- lambda[which(bic==min(bic))]
 lambdamingcv;lambdaminaic;lambdaminbic

  # Fit model
optlambda <- lambdaminaic
xseq_k8 <- seq(minx+0.001,maxx-0.001,length=108)

P <- spline.des(knots=knots,Temp,ord=4, outer.ok = TRUE)$design
Pfit <- spline.des(knots=knots,xseq_k8,ord=4, outer.ok = TRUE)$design

betahat <- solve(t(P)%*%P + optlambda*K2)%*%t(P)%*%Ozone
fitted_k8 <- Pfit%*%betahat

# Graphical representation
k8 <- ggplot(data = Air_reduced, 
         aes(x=Temp, y=Ozone, colour='Original data points')) + 
  geom_point() + scale_colour_hue(h = c(180,0)) + 
  geom_line(aes(x=xseq_k8, y=fitted_k8),colour='black')+
  labs(x="Temperature (Fahrenheit)",y="Ozone") +
  scale_colour_manual("", breaks = c("Data",  "fitted_k8"),values = c("green", "black")) +
  theme_light()
k8

    #calculate AIC
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      pspline_k8 <- n*log(sigma2) + 2*(df+1)
      pspline_k8
      # 593.1127

#20knots 
nrknots <- 20
minx <- min(Temp)-0.001
maxx <- max(Temp)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)
xseq_k20 <- seq(minx+0.001,maxx-0.001,length=108)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1)){
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}

D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots){
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}

K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Temp,ord=4)$design
lambda <- seq(0.0005,0.05,length=250)

gcv <- rep(0,length(lambda))
aic <- rep(0,length(lambda))
bic <- rep(0,length(lambda))

n <- nrow(Air_reduced)

require(psych)
S <- B%*%solve(t(B)%*%B)%*%t(B)
fit <- as.vector(S%*%Ozone)
sigma2 <- sum((Ozone-fit)^2)/(n-tr(S))
for(i in 1:length(lambda)){
        S <- B%*%solve(t(B)%*%B + lambda[i]*K2)%*%t(B)
         diags <- diag(S)
         trs <- mean(diags)
         df <- sum(diags)
         fit <- as.vector(S%*%Ozone)
         gcv[i] <- mean(((Ozone-fit)/(1-trs))^2)
         #  aic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + 2*df
         #  bic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + log(n)*df
         sigma2 <- sum((Ozone-fit)^2)/n
         aic[i] <- n*log(sigma2) + 2*(df+1)
         bic[i] <- n*log(sigma2) + log(n)*(df+1)
}

 par(mfrow=c(1,2))
 plot(lambda,gcv,type="l",lwd=2,
            xlab="lambda",ylab="GCV",main="20 knots",
            cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
 plot(lambda,aic,type="l",lwd=2,main="20 knots",
            xlab="lambda",ylab="AIC",
            cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
 lambdamingcv <- lambda[which(gcv==min(gcv))]
 lambdaminaic <- lambda[which(aic==min(aic))]
 lambdaminbic <- lambda[which(bic==min(bic))]
 lambdamingcv;lambdaminaic;lambdaminbic

  # Fit model
optlambda <- lambdaminaic
xseq_k20 <- seq(minx+0.001,maxx-0.001,length=108)

P <- spline.des(knots=knots,Temp,ord=4, outer.ok = TRUE)$design
Pfit <- spline.des(knots=knots,xseq_k20,ord=4, outer.ok = TRUE)$design

betahat <- solve(t(P)%*%P + optlambda*K2)%*%t(P)%*%Ozone
fitted_k20 <- Pfit%*%betahat

# Graphical representation
k20 <- ggplot(data = Air_reduced, 
         aes(x=Temp, y=Ozone, colour='Original data points')) + 
  geom_point() + scale_colour_hue(h = c(180,0)) + 
  geom_line(aes(x=xseq_k20, y=fitted_k20),colour='black')+
  labs(x="Temperature (Fahrenheit)",y="Ozone") +
  scale_colour_manual("", breaks = c("Data",  "fitted_k20"),values = c("green", "black")) +
  theme_light()
k20

    #calculate AIC
      S<- B%*%solve(t(B)%*%B)%*%t(B)
      fit <- as.vector(S%*%Ozone)
      diags <- diag(S)
      df <- sum(diags)
      sigma2 <- sum((Ozone-fit)^2)/n
      pspline_k20 <- n*log(sigma2) + 2*(df+1)
      pspline_k20
      # 593.4693

    # combined ggplot
    ggplot(Air_reduced, aes(x = Temp, y = Ozone)) +
      geom_point(color = "black") +
      geom_line(data=Air_reduced,aes(x=xseq_k3, y=fitted_k3,colour='3 knots'),size = 1) +
      geom_line(data=Air_reduced,aes(x=xseq_k5, y=fitted_k5,colour='5 knots'),size = 1) +
      geom_line(data=Air_reduced,aes(x=xseq_k8, y=fitted_k8,colour='8 knots'),size = 1) +
      geom_line(data=Air_reduced,aes(x=xseq_k20,y=fitted_k20,colour='20 knots'),size = 1) +
      scale_colour_manual(name = "Cubic P-splines", 
                          breaks = c("3 knots", "5 knots", "8 knots", "20 knots"),
                          values=c("blue", "purple","red", "green"))+
      theme_light() + ggtitle ("Cubic P-splines Fittings")

########
## GAM 
    
oz.gam1 <- gam(Ozone~s(Temp, bs="bs",k=5)+s(Solar.R, bs="bs",k=5)+s(Wind, bs="bs",k=5)+factor(Month)+factor(Day),sp=c(0,0,0), data=Air_reduced)
summary(oz.gam1)

#drop Day
oz.gam2 <- gam(Ozone~s(Temp, bs="bs",k=5)+s(Solar.R, bs="bs",k=5)+s(Wind, bs="bs",k=5)+factor(Month),sp=c(0,0,0), data=Air_reduced)
summary(oz.gam2)

#log(ozone)
logoz<-log(Ozone)
oz.gam3 <- gam(logoz~s(Temp, bs="bs",k=5)+s(Solar.R, bs="bs",k=5)+s(Wind, bs="bs",k=5)+factor(Month),sp=c(0,0,0), data=Air_reduced)
summary(oz.gam3)

#just continuous because Month not sig for both
oz.gam4 <- gam(Ozone~s(Temp, bs="bs",k=5)+s(Solar.R, bs="bs",k=5)+s(Wind, bs="bs",k=5),sp=c(0,0,0), data=Air_reduced)
summary(oz.gam4)
logoz<-log(Ozone)
oz.gam5 <- gam(logoz~s(Temp, bs="bs",k=5)+s(Solar.R, bs="bs",k=5)+s(Wind, bs="bs",k=5),sp=c(0,0,0), data=Air_reduced)
summary(oz.gam5)


#plots for model diagnostics
plot(oz.gam4,residuals=FALSE,cex=1.3,col="red",shade=TRUE, scale=0)
plot(oz.gam4,residuals=TRUE,cex=2.5,col="red",shade=TRUE,
     main="Partial residuals for Ozone model",cex.main=1.5,cex.lab=1.3)
plot(oz.gam5,residuals=FALSE,cex=1.3,col="red",shade=TRUE, scale=0)
plot(oz.gam5,residuals=TRUE,cex=2.5,col="red",shade=TRUE,
     main="Partial residuals for Ozone model",cex.main=1.5,cex.lab=1.3)

par(mfrow=c(1,1))
gam.check(oz.gam4,col="blue")
gam.check(oz.gam5,col='blue')

#get diagnostic plots separately
type <- "deviance"
resid <- residuals(oz.gam4, type = type)
linpred <- napredict(oz.gam4$na.action, oz.gam4$linear.predictors)
observed.y <- napredict(oz.gam4$na.action, oz.gam4$y)

qq.gam(oz.gam4, rep = 0, level = 0.9, type = type, rl.col = 2,
       pch=20,main="QQ plot",col="blue")
qq.gam(oz.gam4, rep = 0, level = 0.9, type = type, rl.col = 2,
       pch=20,main="QQ plot",col="blue")
hist(resid, xlab = "Residuals",
     main = "Histogram of residuals",col="blue")
plot(linpred, resid, main = "Resids vs. linear pred.", 
     xlab = "linear predictor", ylab = "residuals",col="blue")
plot(fitted(oz.gam4), observed.y, xlab = "Fitted Values", 
     ylab = "Response", main = "Response vs. Fitted Values",col="blue")
    

type <- "deviance"
resid <- residuals(oz.gam5, type = type)
linpred <- napredict(oz.gam5$na.action, oz.gam5$linear.predictors)
observed.y <- napredict(oz.gam5$na.action, oz.gam5$y)

qq.gam(oz.gam5, rep = 0, level = 0.9, type = type, rl.col = 2,
       pch=20,main="QQ plot",col="blue")
qq.gam(oz.gam5, rep = 0, level = 0.9, type = type, rl.col = 2,
       pch=20,main="QQ plot",col="blue")
hist(resid, xlab = "Residuals",
     main = "Histogram of residuals",col="blue")
plot(linpred, resid, main = "Resids vs. linear pred.", 
     xlab = "linear predictor", ylab = "residuals",col="blue")
plot(fitted(oz.gam5), observed.y, xlab = "Fitted Values", 
     ylab = "Response", main = "Response vs. Fitted Values",col="blue")

##########################################
##################################logistic

#make response binary

oz.bin <- ifelse(Ozone>= 70, 1, 0)
oz.bin

#fit the binomial model
binoz <- gam(oz.bin~s(Temp, bs="bs",k=5),s(Solar.R, bs="bs",k=5),s(Wind, bs="bs",k=5),family=binomial(link="logit"))
summary(binoz)
#remove insig
binoz <- gam(oz.bin~s(Temp, bs="bs",k=5),family=binomial(link="logit"))
summary(binoz)


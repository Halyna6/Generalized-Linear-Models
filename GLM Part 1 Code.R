elderly <- read.table(file=file.choose(),header=T)
attach(elderly)

elderly$nvisit = (ofp + ofnp + opp + opnp + emer + hosp)
elderly$numchron <- elderly$numchron - mean(elderly$numchron)
elderly$age <- elderly$age - mean(elderly$age)
elderly$faminc <- elderly$faminc - mean(elderly$faminc)

elderly <- subset(elderly, select = c(numchron, adldiff, age, gender, married, faminc, employed, privins, medicaid, nvisit))
attach(elderly)
##########################
### DESCRIPTIVE STATS ###
##########################

library(GGally)
library(graphics)
library(ggplot2)
library(reshape2)
library(xtable)
library(stargazer)
library(psych)

#descriptives
describe(elderly)

#cor-matrix
ggpairs(elderly)

#relationship with nvisit
par(mfrow = c(3,3))
for(v in 1:9){
  plot(elderly$nvisit ~ elderly[,v], xlab = names(elderly[v]) , ylab = "Total visits",
       main = names(elderly[v]), pch = 19, col = "lightblue4")
}  

#distributions
d = melt(elderly[,c(1:10)], id.vars = 'nvisit')
ggplot(d, aes(y = nvisit, x = value))+
  geom_point() +
  theme_bw() +
  facet_wrap(~variable, scales = 'free')


######################
### OVERDISPERSION ###
######################

# overdispersion? first glimpse:  YES, residual deviance is 10 times as large compared to degrees of freedom 
# overdispersion means that var(Y)> E(Y)
mean(elderly$nvisit) # E(nvisit) = 9.476
var(elderly$nvisit) #195.5846 

dev.new(width=6, height=4)

hist(elderly$nvisit, nclas=100, col="gainsboro", probability = TRUE,
     xlab="nvisit",ylab="",main="",ylim=c(0,0.14),
     cex.lab=1.5,cex.axis=1.3 )

##Poisson empty model
poismodel0 <- glm(nvisit ~ 1, family = poisson(link = 'log'), data = elderly) 
summary(poismodel0)
lines(0:120,dpois(0:120,exp(2.24876)),col="mediumvioletred",lwd=3)

##Negative binomial empty model
library(MASS)
ngbinmodel0 <- glm.nb(nvisit ~ 1,data = elderly) 
summary(ngbinmodel0)
nbmu <- exp(summary(ngbinmodel0)$coefficients[,1])
nbsize <- summary(ngbinmodel0)$theta

nbp <- 1-nbmu/(nbmu+nbsize)

ngbindmft <- dnbinom(0:120, prob=nbp, size=nbsize, log = FALSE)
lines(0:120,ngbindmft,col="aquamarine3",lwd=3)

##Quassipoisson empty model
#https://www.r-bloggers.com/generate-quasi-poisson-distribution-variable/
rqpois <- function(n, mu, theta) {
  rnbinom(n = n, mu = mu, size = mu/(theta-1))
}

#var(Y) = θμ, where θ is the quasi-Poisson overdispersion parameter,

quasipoiss0 <- glm(nvisit ~ 1, family=quasipoisson, data = elderly) 
summary(quasipoiss0)
qpmu <- exp(summary(quasipoiss0)$coefficients[,1])
qptheta<- 20.6412

qpbindmft <- rqpois(0:120, mu=qpmu, theta=qptheta)
lines(0:120,qpbindmft,col="orange",lwd=3)

#the predicted counts line does not fit the data well at all, the distribution should go in line with the blue/orange line


############################
### FREQUENTIST: POISSON ###
############################

###Model selection with main effects only #####
poisson.full_main<- glm(nvisit~numchron + adldiff+ age+ gender+ married+ faminc + employed+ privins + medicaid, family = poisson(link = 'log'), data = elderly)
summary(poisson.full_main)

## Chisq
drop1(poisson.full_main, data = elderly,family = poisson(link = 'log'), test = "Chisq")
# Lowest AIC when nothing is dropped, keep full model
POISS.final.v1 <- glm(nvisit~numchron + adldiff+ age+ gender+ married+ faminc + employed+ privins + medicaid, family = poisson(link = 'log'), data = elderly)
summary(POISS.final.v1)
#AIC: 6864

## Likelihood ratio test
poiss.empty <- glm(nvisit ~ 1, family = poisson(link = 'log'), data = elderly)
add1(poiss.empty, ~ . + numchron + adldiff+ age+ gender+ married+ faminc + employed+ privins + medicaid, family = poisson(link = 'log'), data = elderly, test = "LRT")
# add numchron, because result in lowest AIC
LRT1 <- glm(nvisit ~ numchron, family = poisson(link = 'log'),data = elderly)
add1(LRT1, ~ . + adldiff+ age+ gender+ married+ faminc + employed+ privins + medicaid, family = poisson(link = 'log'), data = elderly, test = "LRT")
# add privins, because result in lowest AIC
LRT2 <- glm(nvisit ~ numchron + privins, family = poisson(link = 'log'), data = elderly)
add1(LRT2, ~ . + adldiff+ age+ gender+ married+ faminc+ employed + medicaid, family = poisson(link = 'log'), data = elderly, test = "LRT")
# add employed, because result in lowest AIC
LRT3 <- glm(nvisit ~ numchron + privins + employed, family = poisson(link = 'log'), data = elderly)
add1(LRT3, ~ . + adldiff+ age+ married+ faminc+ gender + medicaid, family = poisson(link = 'log'), data = elderly, test = "LRT")
# add age, because result in lowest AIC
LRT4 <- glm(nvisit ~ numchron + privins+ employed+age, family = poisson(link = 'log'), data = elderly)
add1(LRT4, ~ . + adldiff+ married+ faminc+ gender + medicaid, family = poisson(link = 'log'), data = elderly, test = "LRT")
# add adldiff, because result in lowest AIC
LRT5 <- glm(nvisit ~ numchron + privins+ employed + age + adldiff, family = poisson(link = 'log'), data = elderly)
add1(LRT5, ~ . + married+ gender + medicaid + faminc, family = poisson(link = 'log'), data = elderly, test = "LRT")
# add faminc, because result in lowest AIC
LRT6 <- glm(nvisit ~ numchron + privins+ employed + age + adldiff + faminc, family = poisson(link = 'log'), data = elderly)
add1(LRT6, ~ . + married + gender + medicaid, family = poisson(link = 'log'), data = elderly, test = "LRT")
# add married, because result in lowest AIC
LRT7 <- glm(nvisit ~ numchron + privins + employed + age + adldiff + faminc + married, family = poisson(link = 'log'), data = elderly)
add1(LRT7, ~ . + gender + medicaid, family = poisson(link = 'log'), data = elderly, test = "LRT")
# add gender, because result in lowest AIC
LRT8 <- glm(nvisit ~ numchron + privins + employed + age + adldiff + faminc + married + gender, family = poisson(link = 'log'), data = elderly)
add1(LRT8, ~ . + medicaid, family = poisson(link = 'log'), data = elderly, test = "LRT")
# add medicaid, because result in lowest AIC
LRT9 <- glm(nvisit ~ numchron + privins + employed + age + adldiff + faminc + married + gender + medicaid, family = poisson(link = 'log'), data = elderly)
# nothing is added, because full model was reached...
POISS.final.v2 <- glm(nvisit ~ numchron + privins + employed + age + adldiff + faminc + married + gender + medicaid, family = poisson(link = 'log'), data = elderly)
summary(POISS.final.v2)
# Rule of thumb is STILL violated!! (residual deviance is NOT almost equal to the df)
#AIC: 6864

##Stepwise procedure
step(poisson.full_main, direction = "both")
POISS.final.v3 <- glm(formula = nvisit ~ numchron + adldiff + age + gender + married + 
                        faminc + employed + privins + medicaid, family = poisson(link = "log"), 
                      data = elderly)
#AIC: 6864, however, this is also just the full model...

###Model selection with interactions and quadratic effects #####
poisson.full_all <- glm(nvisit ~ numchron + factor(adldiff) + age + factor(gender)
                       +  faminc +  factor(employed) + factor(married) 
                       + factor(privins) + factor(medicaid) + numchron:factor(adldiff)
                       + numchron:age + numchron:factor(employed) + numchron:factor(gender)
                       + numchron:factor(married)+ numchron:factor(medicaid) + numchron:factor(privins) + numchron:faminc
                       + factor(adldiff):age + factor(adldiff):factor(gender) + factor(adldiff):factor(married) 
                       + factor(adldiff):factor(employed) + factor(adldiff):faminc + factor(adldiff):factor(privins) + factor(adldiff):factor(medicaid) 
                       + age:factor(gender) + age:factor(married) + age:faminc + age:factor(privins) + age:factor(employed)+ age:factor(medicaid) 
                       + factor(gender):factor(married) + factor(gender):faminc + factor(gender):factor(employed) + factor(gender):factor(privins)
                       + factor(gender):factor(medicaid) 
                       + faminc:factor(married) +faminc:factor(employed)+ faminc:factor(privins)+faminc:factor(medicaid)
                       + factor(employed):factor(married) + factor(employed):factor(medicaid) + factor(employed):factor(privins) 
                       + factor(married):factor(privins) +factor(married):factor(medicaid) 
                       + factor(medicaid):factor(privins)
                       + age^2 + faminc^2 + numchron^2,
                       family = poisson(link = 'log'), data = elderly)

step(poisson.full_all)
POISS.final.v4 <-  glm(formula = nvisit ~ numchron + factor(adldiff) + age + factor(gender) + 
                            faminc + factor(employed) + factor(married) + factor(privins) + 
                            factor(medicaid) + numchron:factor(adldiff) + numchron:age + 
                            numchron:factor(employed) + numchron:factor(privins) + factor(adldiff):age + 
                            factor(adldiff):factor(gender) + factor(adldiff):factor(employed) + 
                            factor(adldiff):factor(medicaid) + age:factor(gender) + age:factor(married) + 
                            age:faminc + age:factor(privins) + age:factor(medicaid) + 
                            factor(gender):factor(married) + factor(gender):faminc + 
                            factor(gender):factor(privins) + factor(gender):factor(medicaid) + 
                            faminc:factor(married) + faminc:factor(medicaid) + factor(employed):factor(married) + 
                            factor(employed):factor(privins) + factor(married):factor(medicaid) + 
                            factor(privins):factor(medicaid), family = poisson(link = "log"), 
                          data = elderly)
summary(POISS.final.v4)
#AIC: 6165


######################################
### FREQUENTIST: NEGATIVE BINOMIAL ###
######################################
library(MASS)

###Model selection with main effects only #####
nb.full_main <- glm.nb(nvisit ~numchron + adldiff+ age+ gender+ married+ faminc + employed+ privins + medicaid, data = elderly)
summary(nb.full_main)

# Chisq
drop1(nb.full_main, data = elderly, test = "Chisq")
# drop faminc because result in lowest AIC
d1 <- glm.nb(nvisit ~numchron + adldiff+ age+ gender+ married + employed+ privins + medicaid, data = elderly)
drop1(d1, data = elderly, test = "Chisq")
# drop medicaid because result in lowest AIC
d2 <- glm.nb(nvisit ~numchron + adldiff+ age+ gender+ married + employed+ privins, data = elderly)
drop1(d2, data = elderly, test = "Chisq")
# Lowest AIC when nothing is dropped
NB.final.v1 = glm.nb(nvisit ~numchron + adldiff+ age+ gender+ married + employed+ privins, data = elderly)
summary(NB.final.v1)
#AIC: 3241.4


# Likelihood ratio test
nb.null <- glm.nb(nvisit ~ 1, data = elderly)
add1(nb.null, ~ . + numchron + adldiff+ age+ gender+ married+ faminc + employed+ privins + medicaid, data = elderly, test = "LRT")
# add numchron, because result in lowest AIC
LRT1 <- glm.nb(nvisit ~ numchron, data = elderly)
add1(LRT1,  ~ . + adldiff+ age+ gender+ married+ faminc + employed+ privins + medicaid, data = elderly, test = "LRT")
# add employed, because result in lowest AIC
LRT2 <- glm.nb(nvisit ~ numchron + employed, data = elderly)
add1(LRT2, ~ . + adldiff+ age + gender + married + faminc + privins + medicaid, data = elderly, test = "LRT")
# add privins, because result in lowest AIC
LRT3 <- glm.nb(nvisit ~ numchron + employed + privins, data = elderly)
add1(LRT3, ~ . + adldiff+ age+ gender+ married+ faminc + medicaid, data = elderly, test = "LRT")
# add age, because result in lowest AIC
LRT4 <- glm.nb(nvisit ~ numchron + employed + privins + age, data = elderly)
add1(LRT4, ~ . + adldiff+ gender+ married+ faminc + medicaid, data = elderly, test = "LRT")
# nothing is added, because no effect is significant
NB.final.v2 <- glm.nb(nvisit ~ numchron + employed + privins + age, data = elderly)
summary(NB.final.v2)

# Rule of thumb is not violated (residual deviance is almost equal to the df)
# model is not same as previous, because stopped when no longer significant. 
#AIC: 3240

step(nb.full_main, direction = "both")
NB.final.v3 <- glm.nb(nvisit ~ numchron + age + gender + married + employed + privins, data = elderly)
summary(NB.final.v3)
#AIC: 3240.9
#almost the same as v1, only without adldiff

###Model selection with interactions and quadratic effects #####
ngbin.full_all <- glm.nb(nvisit ~ numchron + factor(adldiff) + age + factor(gender)
                        +  faminc +  factor(employed) + factor(married) 
                        + factor(privins) + factor(medicaid) + numchron:factor(adldiff)
                        + numchron:age + numchron:factor(employed) + numchron:factor(gender)
                        + numchron:factor(married)+ numchron:factor(medicaid) + numchron:factor(privins) + numchron:faminc
                        + factor(adldiff):age + factor(adldiff):factor(gender) + factor(adldiff):factor(married) 
                        + factor(adldiff):factor(employed) + factor(adldiff):faminc + factor(adldiff):factor(privins) + factor(adldiff):factor(medicaid) 
                        + age:factor(gender) + age:factor(married) + age:faminc + age:factor(privins) + age:factor(employed)+ age:factor(medicaid) 
                        + factor(gender):factor(married) + factor(gender):faminc + factor(gender):factor(employed) + factor(gender):factor(privins)
                        + factor(gender):factor(medicaid) 
                        + faminc:factor(married) +faminc:factor(employed)+ faminc:factor(privins)+faminc:factor(medicaid)
                        + factor(employed):factor(married) + factor(employed):factor(medicaid) + factor(employed):factor(privins) 
                        + factor(married):factor(privins) +factor(married):factor(medicaid) 
                        + factor(medicaid):factor(privins)
                        + age^2 + faminc^2 + numchron^2, data = elderly, maxit = 500)
summary(ngbin.full_v2)
step(ngbin.full_v2, direction = "both")

NB.final.v4 <-  glm.nb(formula = nvisit ~ numchron + factor(adldiff) + age 
                           + factor(gender) + faminc + factor(employed) + factor(married)
                           + factor(privins) + factor(medicaid) + numchron:factor(employed)
                           + numchron:factor(privins) + factor(adldiff):age
                           + factor(adldiff):factor(employed) + age:factor(gender) 
                           + factor(gender):factor(married) + factor(gender):factor(privins)
                           + factor(gender):factor(medicaid) + factor(employed):factor(married)
                           + factor(married):factor(medicaid) + factor(privins):factor(medicaid),
                           data = elderly, maxit = 500)
summary(NB.final.v4)
# Rule of thumb is not violated (residual deviance is almost equal to the df)
#3207.1


##################################
### FREQUENTIST: QUASI-POISSON ###
##################################
quasi.full_main <- glm(nvisit ~numchron + adldiff+ age+ gender+ married+ faminc + employed+ privins + medicaid, family = quasipoisson, data = elderly)
summary(quasi.full_main)
# Variable selection based on the AIC is not possible

### Final model based on negative binomial (take effects found in binomial)
##Main effects only
#based on model NB.final.v1
QP.final.v1 <- glm(nvisit ~numchron + adldiff+ age+ gender+ married + employed+ privins, family = quasipoisson, data = elderly)
summary(QP.final.v1)
# Rule of thumb is violated (residual deviance is bigger than df)

#based on model NB.final.v2
QP.final.v2 <- glm(nvisit ~ numchron + employed + privins + age, family = quasipoisson, data = elderly)
summary(QP.final.v2)
# Rule of thumb is violated (residual deviance is bigger than df)

#based on model NB.final.v3
QP.final.v3 <- glm(nvisit ~ numchron + age + gender + married + employed + privins, family = quasipoisson, data = elderly)
summary(QP.final.v3)
# Rule of thumb is violated (residual deviance is bigger than df)

##Main effects + interactions
#based on model NB.final.v4
QP.final.v4 <- glm(formula = nvisit ~ numchron + factor(adldiff) + age 
                   + factor(gender) + faminc + factor(employed) + factor(married)
                   + factor(privins) + factor(medicaid) + numchron:factor(employed)
                   + numchron:factor(privins) + factor(adldiff):age
                   + factor(adldiff):factor(employed) + age:factor(gender) 
                   + factor(gender):factor(married) + factor(gender):factor(privins)
                   + factor(gender):factor(medicaid) + factor(employed):factor(married)
                   + factor(married):factor(medicaid) + factor(privins):factor(medicaid),
                     family = quasipoisson(link = "log"), data = elderly)
summary(QP.final.v4)
# Rule of thumb is violated (residual deviance is bigger than df)
# many non significant effects -> pick v1

# Sandwich estimator
#install.packages('lmtest')
library(lmtest)
#install.packages('sandwich')
library(sandwich)
coeftest(quasi.full_main, vcov = sandwich)
coeftest(QP.final.v1, vcov = sandwich)
coeftest(QP.final.v2, vcov. = sandwich)

######################
### MODEL ADEQUACY ###
######################

#Define final chosen models:
final.model.poisson <- POISS.final.v1
final.model.ngbin <- NB.final.v2
final.model.quasi <- QP.final.v1

# Poisson
win.graph()
par(mfrow=c(1,4))
plot(final.model.poisson)
win.graph()
par(mfrow=c(2,2))
plot(final.model.poisson)

# Quasi-poisson
win.graph()
par(mfrow=c(2,2))
plot(final.model.quasi)

# Negative binomial
win.graph()
par(mfrow=c(2,2))
plot(final.model.ngbin)

#-----------------#
# Goodness of fit #
#-----------------#

# Deviance chi-squared test
# Poisson
dev.poisson <- summary(final.model.poisson)$deviance
dev.poisson
df.poisson <- summary(final.model.poisson)$df.residual
1-pchisq(dev.poisson, df.poisson)

# Quasi-Poisson
dev.quasi <- summary(final.model.quasi)$deviance
dev.quasi
df.quasi <- summary(final.model.quasi)$df.residual
1-pchisq(dev.quasi, df.quasi)

# Negative binomial
dev.nb <- summary(final.model.ngbin)$deviance
dev.nb
df.nb <- summary(final.model.ngbin)$df.residual
1-pchisq(dev.nb, df.nb)

# Deviance residuals
# Poisson
win.graph()
r.dev <- residuals(final.model.poisson, type = "deviance")
summary(r.dev)
plot(elderly$age,r.dev,xlab="Age",ylab="Deviance residual",
     cex.lab=1.5,cex.axis=1.3, main="Poisson model")
loess.dev <- loess(r.dev~elderly$age)
lo.pred <- predict(loess.dev, se=T)

ordertot <- order(elderly$age)
lines(elderly$age[ordertot],lo.pred$fit[ordertot],col="blue",lwd=3)
lines(elderly$age[ordertot],lo.pred$fit[ordertot]+2*lo.pred$s[ordertot], lty=2,col="red") #rough & ready CI
lines(elderly$age[ordertot],lo.pred$fit[ordertot]-2*lo.pred$s[ordertot], lty=2,col="red")
# A trend in the smoothed line -> No good model

# Quasi-poisson
win.graph()
r.dev <- residuals(final.model.quasi, type = "deviance")
summary(r.dev)
plot(elderly$age,r.dev,xlab="Age",ylab="Deviance residual", main= "Quasi-Poisson model",
     cex.lab=1.5,cex.axis=1.3)
loess.dev <- loess(r.dev~elderly$age)
lo.pred <- predict(loess.dev, se=T)

ordertot <- order(elderly$age)
lines(elderly$age[ordertot],lo.pred$fit[ordertot],col="blue",lwd=3)
lines(elderly$age[ordertot],lo.pred$fit[ordertot]+2*lo.pred$s[ordertot], lty=2,col="red") #rough & ready CI
lines(elderly$age[ordertot],lo.pred$fit[ordertot]-2*lo.pred$s[ordertot], lty=2,col="red")
# A trend in the smoothed line -> No good model

# Negative binomial
win.graph()
r.dev <- residuals(final.model.ngbin, type = "deviance")
summary(r.dev)
plot(elderly$age,r.dev,xlab="Age",ylab="Deviance residual", main="Negative Binomial model"
     ,cex.lab=1.5,cex.axis=1.3)
loess.dev <- loess(r.dev~elderly$age)
lo.pred <- predict(loess.dev, se=T)

ordertot <- order(elderly$age)
lines(elderly$age[ordertot],lo.pred$fit[ordertot],col="blue",lwd=3)
lines(elderly$age[ordertot],lo.pred$fit[ordertot]+2*lo.pred$s[ordertot], lty=2,col="red") #rough & ready CI
lines(elderly$age[ordertot],lo.pred$fit[ordertot]-2*lo.pred$s[ordertot], lty=2,col="red")
# A trend in the smoothed line -> No good model

#----------------------------#
# Diagnostic for influential #
#----------------------------#

# Poisson
win.graph()

N <- length(elderly$nvisit); id <- 1:N
hat.road <- hatvalues(final.model.poisson)
rstudent.road <- rstudent(final.model.poisson)
plot(hat.road, rstudent.road, xlab="Hat values", ylab="Studentized residuals", main="Poisson model")
dffits.road <- dffits(final.model.poisson)
plot(id, dffits.road, xlab="Obs. number", ylab="Dffits", type= "l", main="Poisson model")
cov.road <- covratio(final.model.poisson)
plot(id, cov.road, xlab="Obs. number", ylab="Covariance ratio", type = "l", main="Poisson model")
cook.road <- cooks.distance(final.model.poisson)
#identify points
cutoff <- 4/((nrow(elderly)-length(final.model.poisson$coefficients)-2))
plot(final.model.poisson, which=4, cook.levels=cutoff, main="Poisson model")

# Quasi-poisson
win.graph()

N <- length(elderly$nvisit); id <- 1:N
hat.road <- hatvalues(final.model.quasi)
rstudent.road <- rstudent(final.model.quasi)
plot(hat.road, rstudent.road, xlab="Hat values", ylab="Studentized residuals", main="Quasi-Poisson model")
dffits.road <- dffits(final.model.quasi)
plot(id, dffits.road, xlab="Obs. number", ylab="Dffits", type = "l", main="Quasi-Poisson model")
cov.road <- covratio(final.model.quasi)
plot(id, cov.road, xlab="Obs. number", ylab="Covariance ratio", type = "l", main="Quasi-Poisson model")
cook.road <- cooks.distance(final.model.quasi)
#identify points
cutoff <- 4/((nrow(elderly)-length(final.model.quasi$coefficients)-2))
plot(final.model.quasi, which=4, cook.levels=cutoff, main="Quasi-Poisson model")

# Negative binomial model
win.graph()

N <- length(elderly$nvisit); id <- 1:N
hat.road <- hatvalues(final.model.ngbin)
rstudent.road <- rstudent(final.model.ngbin)
plot(hat.road, rstudent.road, xlab="Hat values", ylab="Studentized residuals", main="Negative Binomial model")
dffits.road <- dffits(final.model.ngbin)
plot(id, dffits.road, type = 'l', xlab="Obs. number", ylab="Dffits",main="Negative Binomial model")
cov.road <- covratio(final.model.ngbin)
plot(id, cov.road, xlab="Obs. number" , ylab="Covariance ratio", type="l", main="Negative Binomial model")
cook.road <- cooks.distance(final.model.ngbin)
#identify points
cutoff <- 4/((nrow(elderly)-length(final.model.ngbin$coefficients)-2))
plot(final.model.ngbin, which=4, cook.levels=cutoff, main="Negative Binomial model")


##################
### Prediction ###
##################

win.graph()
plot(elderly$nvisit,final.model.poisson$fitted.values,xlab="nvisit", ylab="Fitted values", main="Poisson")
win.graph()
plot(elderly$nvisit,final.model.ngbin$fitted.values, xlab="nvisit", ylab="Fitted values", main="Negative Binomial")
win.graph()
plot(elderly$nvisit,final.model.quasi$fitted.values, xlab="nvisit", ylab="Fitted values", main= "Quasi-Poisson")


win.graph()
plot(age,nvisit)
predictedvalues<-predict(final.model.poisson, type="response", se.fit = TRUE)
ci <- matrix( c(predictedvalues$fit + 1.96 * predictedvalues$se.fit, predictedvalues$fit - 1.96 * 
                  predictedvalues$se.fit), ncol=2 ) 

colnames(ci, do.NULL = TRUE, prefix = "col")
colnames(ci) <- c("upr", "lwr")
datafull<- cbind(ci, elderly)
head(datafull)
detach(elderly)
attach(datafull)
lines(age,fitted.values(final.model.poisson), col="red")
lines(age,lwr, col="blue")
lines(age,upr, col="blue")


################
### Bayesian ###
################

#install.packages('MCMCpack')
library(MCMCpack)
library(coda)

#remove influential points
elderly_clean <- elderly[-c(95,157,177,444),]

#fit model
poisson.bayes <- MCMCpoisson(nvisit~numchron + adldiff+ age+ gender+ married+ faminc + employed+ privins + medicaid, data=elderly)
summary(poisson.bayes)

plot(poisson.bayes, col = "lightblue4")

negbin.bayes <-  MCMCnegbinChange(nvisit ~ numchron + employed + privins + age, data = elderly, mcmc=10000, burnin = 1000, m=1, rho.step = 0.1)
plot(negbin.bayes, col = "lightblue4")
summary(negbin.bayes)

negbin.bayes.v1 <-  MCMCnegbin(nvisit ~ numchron + employed + privins + age, data = elderly_clean, mcmc=10000, burnin = 2000)
plot(negbin.bayes.v1, col = "lightblue4")
summary(negbin.bayes.v1)


negbin.bayes.v2 <- MCMCnegbinChange(formula = nvisit ~ numchron + factor(adldiff) + age 
                              + factor(gender) + faminc + factor(employed) + factor(married)
                              + factor(privins) + factor(medicaid) + numchron:factor(employed)
                              + numchron:factor(privins) + factor(adldiff):age
                              + factor(adldiff):factor(employed) + age:factor(gender) 
                              + factor(gender):factor(married) + factor(gender):factor(privins)
                              + factor(gender):factor(medicaid) + factor(employed):factor(married)
                              + factor(married):factor(medicaid) + factor(privins):factor(medicaid),
                              data = elderly, mcmc=10000, burnin = 1000, m=2, rho.step = 0.1)
summary(negbin.bayes.v2)

plot(negbin.bayes.v2, col = "lightblue4")

#https://www.r-bloggers.com/a-simple-intro-to-bayesian-change-point-analysis/

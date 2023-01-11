install.packages("tree")
install.packages("leaps")
install.packages("glmnet")
install.packages("gbm")
install.packages("Metrics")
install.packages("olsrr")

library(car)
library(corrplot)
library(tidyverse)
library(caret)
library(tree)
theme_set(theme_classic())
library(leaps)
library(glmnet)
library(randomForest)
library(gbm)
library(e1071)
library(caret)
library(hydroGOF)
library(Metrics)
library(olsrr)

#randomizing the experiment
set.seed(23)
runseq <- 1:12
randomseq <- sample(runseq)
randomseq
set.seed(23)
factorcol <- c("1","2","3","4","5","6","7","8","9","10","11")
randomfactorcol <- sort(sample(factorcol,6))
randomfactorcol

#reading the data
library(readxl)
D = read_excel("regdata.xlsx")
attach(D)

#analyzing the data
names(D)
dim(D)
str(D)
data <- as.data.frame(D)
str(data)
plot(data)
cor(data)


#linear regression with all predictors
par(mfrow=c(2,2))
lm_model1 <- lm(Y1 ~ A, data = data)
summary(lm_model1)
Anova(lm_model1, type = "II")

#INTERACTION PLOT
par(mfrow=c(1,1))
interaction.plot(x.factor     = data$A,
                 trace.factor = data$D,
                 response     = data$Y1,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

#Hamada- Wu stepwise regression method

##Step 1
mA <- lm(Y1 ~ A + A:B + A:C + A:D + A:E + A:F, data = data)
kA <- ols_step_both_p(mA, pent = 0.1, prem = 0.3, details =TRUE)
kA

mB <- lm(Y1 ~ B + B:A + B:C + B:D + B:E + B:F, data = data)
kB <- ols_step_both_p(mB, pent = 0.1, prem = 0.3, details =TRUE)
kB

mC <- lm(Y1 ~ C + C:A + C:B + C:D + C:E + C:F, data = data)
kC <- ols_step_both_p(mC, pent = 0.1, prem = 0.3, details =TRUE)
kC

mD <- lm(Y1 ~ D + D:A + D:B + D:C + D:E + D:F, data = data)
kD <- ols_step_both_p(mD, pent = 0.1, prem = 0.3, details =TRUE)
kD

mE <- lm(Y1 ~ E + E:A + E:B + E:C + E:D + E:F, data = data)
kE <- ols_step_both_p(mE, pent = 0.1, prem = 0.3, details =TRUE)
kE

mF <- lm(Y1 ~ F + F:A + F:B + F:C + F:D + F:E, data = data)
kF <- ols_step_both_p(mF, pent = 0.1, prem = 0.3, details =TRUE)
kF

##Step 2
m2 <- lm(Y1 ~ A + B + C + C:B + D + E + F:A + F:D, data = data)
k2 <- ols_step_both_p(m2, pent = 0.1, prem = 0.3, details =TRUE)
k2


##Step 3
m4 <- lm(Y1 ~ A + A:B + A:C + A:D + A:E + A:F, data = data)
k4 <- ols_step_both_p(m4, pent = 0.1, prem = 0.3, details =TRUE)
k4


test.data <- c(4.5, 1.8, 3, 1.25, 1, -1 )
test.data <- as.data.frame(test.data)
str(test.data)
pred <- predict(lm_model1, newdata= test.data )

fxn = function(z){
  z = as.data.frame( t(z) )
  colnames(z) = colnames(data)[-1]
  predict(lm_model1, newdata=z)
}
optim(c(0,0), fxn, control=list(fnscale=-1))
?optim

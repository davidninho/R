# Quantile Regression in R - Inflation at risk

#Instalar paquetes
# install.packages("quantreg")
library(quantreg)
# install.packages("SparseM")
library(SparseM)
#Leer datos excel
library(readxl)

#Cargar los datos
rfp<- read_excel("D:/Escritorio/BANREP/Inflation at risk/Bases/log x100/base log x 100.xlsx")
attach(rfp)

#Transformar los datos en serie temporal
rfp.ts<-ts(rfp, frequency=4, start=c(2004,1))
rfp.ts

# Define variables
Y <- cbind(exp_infl)
X <- cbind(pib, infl_act, tasa_cam, pre_petro, con_fin)

# Estadística descriptiva (opcional)
summary(Y)
summary(X)

# Regresión MCO (OLS)
olsreg <- lm(Y ~ X, data=rfp.ts)
summary(olsreg)

#Regresiones cuantiles (a percentiles 25, 50 y 75)
quantreg25 <- rq(Y ~ X, data=rfp.ts, tau=0.25)
summary(quantreg25)

quantreg50 <- rq(Y ~ X, data=rfp.ts, tau=0.5)
summary(quantreg50)

quantreg75 <- rq(Y ~ X, data=rfp.ts, tau=0.75)
summary(quantreg75)

# Se realiza una regresión cuantil simultanea
quantreg2575 <- rq(Y ~ X, data=rfp.ts, tau=c(0.25, 0.75))
summary(quantreg2575)

# Test ANOVA para diferencias entre coeficientes
anova(quantreg25, quantreg75)

# Se grafican los datos
quantreg.all <- rq(Y ~ X, tau = seq(0.05, 0.95, by = 0.05), data=rfp.ts)
quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)

#Regresiones Cuantiles en las colas

quantreg1 <- rq(Y ~ X, data=rfp.ts, tau=0.01)
summary(quantreg1)

quantreg3 <- rq(Y ~ X, data=rfp.ts, tau=0.03)
summary(quantreg3)

quantreg5 <- rq(Y ~ X, data=rfp.ts, tau=0.05)
summary(quantreg5)

quantreg95 <- rq(Y ~ X, data=rfp.ts, tau=0.95)
summary(quantreg95)

quantreg97 <- rq(Y ~ X, data=rfp.ts, tau=0.97)
summary(quantreg97)

quantreg99 <- rq(Y ~ X, data=rfp.ts, tau=0.99)
summary(quantreg99)

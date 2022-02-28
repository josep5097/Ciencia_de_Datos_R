# Modelo Probit

# Librerias ====
library(openxlsx)
library(foreign)
library(forecast)
library(gmodels)
library(lmtest)
library(ResourceSelection)
library(ROCR)

#cargar la base de datos ====
DB_1 <- read.xlsx("Data/laboral.xlsx")
attach(DB_1)

names(DB_1)
DB_2 <- DB_1[c("inlf","nwifeinc",
               "educ","exper","expersq",
               "age","kidsge6","kidslt6")]

attach(DB_2)

mprobit <- glm(inlf~.,
               family = binomial("probit"),
               data = DB_2)
summary(mprobit)

newdata <- data.frame(nwifeinc = c(17.5),
                      educ = c(14),
                      exper = c(15),
                      expersq = c(225),
                      age = c(31),
                      kidsge6 = c(1),
                      kidslt6 = c(2))

f1 <- predict(mprobit, newdata, type = "response")
f1

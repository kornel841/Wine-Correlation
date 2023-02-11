install.packages("tidyverse")
install.packages("caret")
install.packages("MASS")
install.packages("lmtest")
install.packages("olsrr")
install.packages("broom")
install.packages("ggpubr")
install.packages("ggplot2")
library(dplyr)
library(caret)
library(tidyverse)
library(MASS)
library(lmtest)
library(olsrr)
library(broom)
library(ggpubr)

data = read.csv("WineQT.csv", header = T)
df <- data.frame(data)
for (x_name in colnames(df)){
  for (y_name in colnames(df)){
    X = df[,x_name]
    Y = df[,y_name]
    print(paste0(y_name, "/", x_name, " corelation: ", cor.test(Y,X)[4]))
  }
  print("")
}

plot(data$density,data$fixed.acidity)
#najwieksza korelacja wyszla miedzy density (gestosc) a fixed acidity (kwasowość)

summary(data$fixed.acidity)
summary(data$density)
#podstawowe dane dla x i y 

cor.test(data$density, data$fixed.acidity)
#korelacja jest widoczna ale nie doskonała 

data2 = Edited

#podzial zbioru danych na dane do trenowania i testowania (60 % do 40%)
set.seed(123)
training.samples <- data2$Density %>%
  createDataPartition(p = 0.6, list = FALSE)
train.data  <- data2[training.samples, ]
test.data <- data2[-training.samples, ]

#model regresji liniowej 
model <- lm(fixed.acidity ~ density, data = train.data)
summary(model)

#wykres kwantyl-kwantyl; wizualny test dla przyjecia "normalnosci zbioru"
qqnorm(model$residuals)
qqline(model$residuals , col = "steelblue", lwd = 2) 

#test shapiro wilk dla rozkladu danych 
shapiro.test(model$residuals) #p value mniejsze od 0.05; rozklad nie jest normalny

#sprawdzanie wizualne homoskedastyczności; wariancja bledow nie jest stala w czasie 
plot(model$fitted.values , model$residuals)

#test statystyczny 
bptest(model) #p-value > 0.05 errors have constant variance

#auto-korelacja reszt
plot(model$residuals)

#test Durbina-Watsona
dwtest(model,  alternative = c("two.sided"))
#DW=1.6 => lies between  0 and 2, it implies positive autocorrelation.

#Calculating mean of errors
mean(model$residuals)

#influential point detection
#wizualnie
plot(model , 4)
#statystycznie
model_dm <- augment(model)
#sprawdzanie highest Cook's distance (czyli miara stopnia zmiany współczynników regresji, gdyby dany przypadek pominąć w obliczeniach współczynników)
#im większa wartość tym bardziej dana obserwacja wpływa na wartości współczynnika regresji
max(model_dm$.cooksd)
#odleglosc cook'a nie jest wieksza od 0.05 wiec nie ma influential point w tym zbiorze 

#

library(MuMIn)
library(tidyverse)
library(broom)

data("ChickWeight")

fit1 <- lm(weight ~ Time + Diet, data = ChickWeight)

tidy(fit1)

# calculamos para Dieta 4 Tiempo 4

(8.75 * 4) + 10.92 + (0 * 16.17) + (0 * 36.5) + (1 * 30.23)

## Incluir una interacción

fit2a <- lm(weight ~ Time + Time:Diet, data = ChickWeight)
fit2b <- glm(weight ~ Time + Time:Diet, data = ChickWeight)
fit2SinT <- glm(weight ~ Time:Diet, data = ChickWeight)
tidy(fit2)


## Diagnositicos con broom

Test <- augment(fit2)


# Esto
fit3 <- lm(weight ~ Time * Diet, data = ChickWeight)

# Es equivalente a esto

fit3 <- lm(weight ~ Time + Diet + Time:Diet, data = ChickWeight)


### GLM Familia Gamma

fit2g <- glm(weight ~ Time + Time:Diet, family = Gamma, data = ChickWeight)

DF <- data.frame(Time = 3, Diet = as.factor(1))

DF <- data.frame(Time = 4, Diet = as.factor(4))

### Glm Familia poisson

fit2p <- glm(weight ~ Time + Time:Diet, family = poisson, data = ChickWeight)

Data <- ChickWeight

Data$PredL <- predict(fit2, newdata = Data)
Data$PredP <- predict(fit2p, newdata = Data, type = "response")
Data$PredG <- predict(fit2g, newdata = Data, type = "response")

Data$ResidL <- Data$PredL - Data$weight
Data$ResidP <- Data$PredP - Data$weight
Data$ResidG <- Data$PredG - Data$weight

sqrt(sum(Data$ResidP^2))
sqrt(sum(Data$ResidG^2))
  
predict(fit2g, newdata = DF, type = "response")
predict(fit2p, newdata = DF, type = "response")
predict(fit2, newdata = DF, type = "response")

## Modelo binomial

train <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/CursoMultiPres/Capitulo_3/train.csv") %>% 
  filter(Embarked == "S")
###

FitBin <- glm(Survived ~ Fare + Sex, data = train)

FitBin2 <- glm(Survived ~ Fare + Sex, data = train, family = binomial)


DF <- data.frame(Fare = c(100, 100), Sex = c("male", "female"))

predict(FitBin2, newdata = DF, type = "response") 



set.seed(2020)
X1 <- rnorm(100, mean = 90, sd = 10)
X1 <- 1:100*X1

set.seed(2020)
X2 <- rnorm(100, mean = 50, sd = 10)
X2 <-X2*seq(0, -100, length.out = 100)

y = -3.4*X2 + 2.5*X1 + 25 + rnorm(100, mean = 0, sd = 20)


lm(y~ X1 + X2)

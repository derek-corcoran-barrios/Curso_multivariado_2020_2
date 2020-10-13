library(tidyverse)
library(MuMIn)
library(caret)
library(broom)

data("mtcars")
  
index <- sample(1:nrow(mtcars), size = round(nrow(mtcars)/2))

Train <- mtcars[index, ]
Test <- mtcars[-index, ]

Modelo <- lm(mpg ~ hp + I(hp^2), data = Train)

Modelo2 <- lm(mpg ~ hp + I(hp^2) + I(hp^3) + I(hp^4), data = Train)


broom::tidy(Modelo)

broom::glance(Modelo)
  

Test$Pred <- predict(Modelo, Test)

Test <- Test %>% 
  mutate(resid = mpg - Pred) %>% 
  dplyr::select(hp, mpg, Pred, resid)


## Para ver el poder predictivo

postResample(pred = Test$Pred, obs = Test$mpg)


# Para calcular AICc

AICc(Modelo)
AICc(Modelo2)


## Poder predictivo, vs Poder explicativo


DF <- data.frame(Predictivo = rep(NA, 100), Explicativo = rep(NA, 100))

for(i in 1:100){
  index <- sample(1:nrow(mtcars), size = round(nrow(mtcars)/2))
  
  Train <- mtcars[index, ]
  Test <- mtcars[-index, ]
  
  
  Modelo2 <- lm(mpg ~ hp + I(hp^2) + I(hp^3) + I(hp^4), data = Train)
  
  Train$Pred <- predict(Modelo2, Train)
  Test$Pred <- predict(Modelo2, Test)
  
  ## Para ver el poder predictivo
  
  
  DF$Predictivo[i] <- postResample(pred = Test$Pred, obs = Test$mpg)[2]
  DF$Explicativo[i] <- postResample(pred = Train$Pred, obs = Train$mpg)[2]
}

DF <- DF %>% pivot_longer(cols = everything(), names_to = "Tipo", values_to = "R2")


ggplot(DF, aes(x = Tipo, y = R2)) + geom_boxplot()
  


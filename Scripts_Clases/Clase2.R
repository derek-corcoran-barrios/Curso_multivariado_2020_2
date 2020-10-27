  library(tidyverse)
  library(MuMIn)
  library(broom)
  library(caret)
  
  data("mtcars")
  
  ## Generamos nuestos modelos
  
  fit1 <- lm(mpg ~ carb + cyl, data = mtcars)
  fit2 <- lm(mpg ~ cyl + wt, data = mtcars)
  fit3 <- lm(mpg ~ am + qsec + wt, data = mtcars)
  fit4 <- lm(mpg ~ carb + cyl + wt, data = mtcars)
  fit5 <- lm(mpg ~ am + carb + cyl + qsec + wt, data = mtcars)
  fit6 <- lm(mpg ~ am + carb + cyl + hp + qsec, data = mtcars)
  fit7 <- lm(mpg ~ carb, data = mtcars)
  fit8 <- lm(mpg ~ am + I(am^2) + carb + I(carb^2) + cyl + I(cyl^2) + hp + I(hp^2) + qsec, data = mtcars)
  
  ## Los ponemos en una lista
  
  Modelos <- list(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)
  
  ## Generamos la tabla de selección de modelos
  
SelectAIC <- model.sel(Modelos)
SelectBIC <- model.sel(Modelos, rank = "BIC")
  
SelectedAIC <- subset(SelectAIC, delta <= 2)
SelectedBIC <- subset(SelectBIC, delta <= 2)
  
  ## Obtener el mejor modelo
  
  BestModel <- get.models(Select, 1)[[1]]
  
  
  
  ############### opcion promediar modelos
  
  S <- as.data.frame(Selected)
  S <- as.data.frame(Selected) %>% select(cyl, weight)
  
  
  
  
  ### Calculo full
  
  S_full <- S
  
  S_full <- S_full %>% 
    mutate(cyl = ifelse(is.na(cyl), 0, cyl)) %>% 
    mutate(Theta_i = cyl*weight) %>% 
    summarise(Theta = sum(Theta_i))
  
  
  ## Calculo subset
  
  
  S_Subset <- S
  
  S_Subset <- S_Subset %>% 
    filter(!is.na(cyl)) %>% 
    mutate(Theta_i = cyl*weight) %>% 
    summarise(Theta = sum(Theta_i)/sum(weight)) 
  
  
  ### Calculo del promedio de modelos con MuIn
  
  Modelo_Promedio <- model.avg(Select, subset = delta <= 2, fit = T)


####################################################
#############Multicolinearidad######################
####################################################


ggplot(mtcars, aes(x = qsec, y = mpg)) + 
  geom_smooth(method = "lm") +
  geom_point() + 
  theme_bw()



Model1 <- lm(mpg ~ qsec, data = mtcars)
Model2 <- lm(mpg ~ qsec + hp, data = mtcars)
Model3 <- lm(mpg ~ hp, data = mtcars)



DF <- data.frame(qsec = seq(min(mtcars$qsec), max(mtcars$qsec), length.out = 20),
                 hp = mean(mtcars$hp))


DF$Pred1 <- predict(Model1, newdata = DF)
DF$Pred2 <- predict(Model2, newdata = DF)


DF <- DF %>% pivot_longer(cols = starts_with("Pred"), names_to = "Modelo", values_to = "MPG")


ggplot(DF, aes(x = qsec, y = MPG)) + geom_path(aes(color = Modelo)) + geom_point(data = mtcars, aes(x = qsec, y = mpg)) + theme_bw()

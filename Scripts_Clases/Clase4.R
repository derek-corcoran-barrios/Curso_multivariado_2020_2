library(lme4)
library(caret)
library(broom.mixed)

library(broom)
library(tidyverse)
library(MuMIn)

data("CO2")

ggplot(CO2, aes(x = conc, y = uptake)) + 
  geom_point(aes(shape = Treatment, color = Type)) +
  geom_path(aes(color = Type, lty =Treatment, group = Plant)) +
  theme_bw()


## MOdelo sin variables aleatorias

mod1 <- lm(uptake ~ Type * Treatment + I(log(conc)) + conc, data = CO2)

## MOdelo con variables aleatorias
mod2 <- lmer(uptake ~ Type * Treatment + I(log(conc)) + conc + (1 | Plant), data = CO2)

### Ver el modelo

glance(mod1)

tidy(mod1)


## Ver modelo 2

broom.mixed::glance(mod2)

broom.mixed::tidy(mod2)


### FAQ Ben Bolker (http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html)


### Selección automática de modelos

options(na.action = "na.fail")

Max.Vars <- floor(nrow(CO2)/10)

Seleccion <- dredge(mod2, m.lim = c(0, Max.Vars), rank = "BIC")


## Obtener el mejor modelo

BestModel <- get.models(Seleccion, 1)[[1]]

## Mas especificaciones

data("Cement")


GlobalMod <- lm(y ~ ., data = Cement)


smat <- abs(cor(Cement[, -1])) <= 0.7

smat[!lower.tri(smat)] <- NA

Max.Vars <- floor(nrow(Cement)/10)

Selected1 <- dredge(GlobalMod, subset = smat, m.lim = c(0, Max.Vars))



### Crossvalidation con caret #####
###################################

set.seed(707)

ctrl <- trainControl(method = "cv", number = 5)

Modelo <- train(mpg ~ hp, data = mtcars, method = "lm", trControl = ctrl)

ModeloFinal <- Modelo$finalModel

glance(ModeloFinal)
tidy(ModeloFinal)

DF <- Modelo$resample
DF <- DF %>% select(Rsquared, Resample)

## Seleccion de modelos Con CV

form1 <- "mpg ~ hp"
form2 <- "mpg ~ hp + I(hp^2)"
form3 <- "mpg ~ hp + I(hp^2) + I(hp^3)"
form4 <- "mpg ~ hp + I(hp^2) + I(hp^3) + I(hp^4)"
form5 <- "mpg ~ hp + I(hp^2) + I(hp^3) + I(hp^4) + I(hp^5)"
form6 <- "mpg ~ hp + I(hp^2) + I(hp^3) + I(hp^4) + I(hp^5) + I(hp^6)"
forms <- list(form1, form2, form3, form4, form5, form6)



set.seed(707)

ctrl <- trainControl(method = "cv", number = 5)

K = (2:7)


Tests <- forms %>% map(~train(as.formula(.x), 
                              data = mtcars, method = "lm", 
                              trControl = ctrl)) %>% 
  map(~as.data.frame(.x$resample)) %>% 
  map(~select(.x, Rsquared)) %>% 
  map(~summarise_all(.x, funs(mean, 
                              sd), na.rm = T)) %>% map2(.y = forms, 
                                                         ~mutate(.x, model = .y)) %>% 
  reduce(bind_rows) %>% mutate(K = K) %>% 
  arrange(desc(mean))


library(glmnet)
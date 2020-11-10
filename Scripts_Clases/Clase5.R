library(glmnet)
library(tidyverse)
library(caret)
library(broom)

  data("mtcars")
Fit <- lm(mpg ~ wt, data = mtcars)

tidy(Fit)

Preds <- augment(Fit)

SSE <- Preds %>% mutate(SE = .resid^2) %>% summarise(SSE = sum(SE))


### Preparandonos para hacer glmnet

# Primero que haremos es preparar nuestra función

x <- model.matrix(mpg ~., data = mtcars)
x <- x[,-1]

y <- mtcars$mpg


set.seed(707)
ridge <- cv.glmnet(x, y, alpha = 0, nfolds = 10)


Mejor_Lasso <- glmnet(x, y, alpha = 1, lambda = lasso$lambda.min)

Mejor_Ridge <- glmnet(x, y, alpha = 0, lambda = ridge$lambda.min)


tidy(Mejor_Lasso)

tidy(Mejor_Ridge)

### Usando caret

library(caret)
set.seed(707)
Index <- createDataPartition(mtcars$mpg, p = 0.8, list = F)
Train <- mtcars[Index, ]
Test <- mtcars[-Index, ]

set.seed(707)
Folds <- createFolds(Train$mpg, k = 5, list = F)

x <- model.matrix(mpg ~ ., data = Train)
x <- x[, -1]
y <- Train$mpg


lasso <- cv.glmnet(x, y, alpha = 1, foldid = Folds)
ridge <- cv.glmnet(x, y, alpha = 0, foldid = Folds)
ElasticNet <- cv.glmnet(x, y, alpha = 0.5, foldid = Folds)


Mejor_Lasso <- glmnet(x, y, alpha = 1, lambda = lasso$lambda.min)
Mejor_Ridge <- glmnet(x, y, alpha = 0, lambda = ridge$lambda.min)
Mejor_Elastic <- glmnet(x, y, alpha = 0.5, lambda = ElasticNet$lambda.min)


NewX <- model.matrix(mpg ~., data = Test)
NewX <- NewX[,-1]

NewY <- Test$mpg


DF <- tibble(Modelo = c("Lasso", "Ridge", "Elastic net"),
             Alpha = c(1,0,0.5), 
             Lambda = c(Mejor_Lasso$lambda, Mejor_Ridge$lambda, Mejor_Elastic$lambda),
             RMSE = c(caret::RMSE(pred =  predict(Mejor_Lasso, NewX), 
                                  obs = NewY),
                      caret::RMSE(pred =  predict(Mejor_Ridge, NewX), 
                                  obs = NewY),
                      caret::RMSE(pred =  predict(Mejor_Elastic, NewX), 
                                  obs = NewY)
                      )
)


##

train <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/CursoMultiPres/Capitulo_3/train.csv") %>% 
  select(-Cabin)
train <- train[complete.cases(train),]

x <- model.matrix(Survived ~ ., data = train)
x <- x[, -1]
y <- train$Survived
set.seed(2020)
Fit <- cv.glmnet(x, y, nfolds = 10, family = "binomial", alpha = 1)


mejor.Fit <- glmnet(x, y, family = "binomial", alpha = 1, lambda = Fit$lambda.1se)

tidy(mejor.Fit)








####---- Test suite 1----####
library(MARS)
set.seed(123)
#### Using data mtcars
data("mtcars")
#### Initialize mars.control object with an even Mmax value 6
mc = mars.control(Mmax=6)
#### Fit a mars model, regressing the variable 'mpg' on all other variables
mod = mars(formula=mpg~., data=mtcars, control=mc)
#### Use different functions of class mars
print(mod)
summary(mod)
plot(mod)
anova(mod)
### To use predict function, divide the data into test and train set
train_size <- floor(0.75*nrow(mtcars))
train <- mtcars[1:train_size,]
test <- mtcars[train_size+1:(nrow(mtcars)-train_size),]
#### Fit a mars model, regressing the variable 'mpg' on all other variables only using the train set
mod2 <- mars(formula=mpg~., data=train, control=mc)
#### Use predict function to predict mpg
preds <- predict(mod2, test)
#### Actual value of mpg
actual <- test$mpg
#### Performance of the prediction can be learn by calculating RMSE (mean squared error)
sqrt(mean((actual-preds)^2))

####---- Test suite 2----####
library(MARS)
set.seed(123)
#### Using data USArrests
data("USArrests")
#### Fit a mars model, regressing the variable 'Murder' on all other variables
#### default mars.control is used
mod <- mars(formula = Murder~., data = USArrests, control = mars.control())
#### Use different functions of class mars
print(mod)
summary(mod)
plot(mod)
anova(mod)
### To use predict function, divide the data into test and train set
train_size <- floor(0.75*nrow(USArrests))
train <- USArrests[1:train_size,]
test <- USArrests[train_size+1:(nrow(USArrests)-train_size),]
#### Fit a mars model, regressing the variable 'Murder' on all other variables only using the train set
mod2 <- mars(formula = Murder~., data = train, control = mars.control())
#### Use predict function to predict Murder
preds <- predict(mod2, test)
#### Actual Murder value
actual <- test$Murder
#### Performance of the prediction can be learn by calculating RMSE (mean squared error)
sqrt(mean((actual-preds)^2))

####---- Test suite 3----####
library(MARS)
set.seed(123)
#### Using data LifeCycleSavings
data("LifeCycleSavings")
#### Fit a mars model, regressing the variable 'sr' on all other variables
#### mars.control is initialized with even number of Mmax value 4
mod <- mars(formula = sr~., data = LifeCycleSavings, control = mars.control(Mmax = 4, d = 3, trace = FALSE))
#### Use different functions of class mars
print(mod)
summary(mod)
plot(mod)
anova(mod)
### To use predict function, divide the data into test and train set
train_size <- floor(0.75*nrow(LifeCycleSavings))
train <- LifeCycleSavings[1:train_size,]
test <- LifeCycleSavings[train_size+1:(nrow(LifeCycleSavings)-train_size),]
#### Fit a mars model, regressing the variable 'sr' on all other variables only using the train set
mod2 <- mars(formula = sr~., data = train, control = mars.control(Mmax = 4, d = 3, trace = FALSE))
#### Use predict function to predict sr
preds <- predict.mars(mod2, test)
#### Actual sr value
actual <- test$sr
#### Performance of the prediction can be learn by calculating RMSE (mean squared error)
sqrt(mean((actual-preds)^2))

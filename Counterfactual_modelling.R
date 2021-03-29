##*******************************************************
##                                                   ****
##   COVID-19 Mortality of ages 80+ in England       ####
##                                                   ****
##*******************************************************

## IFoA ICAT Health and Care working group: John Ng and Scott Reid

## Purpose:
# Construct models predicting expected mortality of age 80+, under the counterfactual scenario 
# where the environment after 8th Dec 2020 is taken to be the same as before, including no vaccination.

## Disclaimer:
# The views expressed in this publication are those of invited contributors and not
# necessarily those of the Institute and Faculty of Actuaries. The Institute and Faculty of Actuaries do
# not endorse any of the views stated, nor any claims or representations made in this publication and
# accept no responsibility or liability to any person for loss or damage suffered as a consequence of
# their placing reliance upon any view, claim or representation made in this publication. The information
# and expressions of opinion contained in this publication are not intended to be a comprehensive
# study, nor to provide actuarial advice or advice of any nature and should not be treated as a substitute
# for specific advice concerning individual situations. On no account may any part of this publication be
# reproduced without the written permission of the Institute and Faculty of Actuaries.

##*******************************************************
##                                                   ****
##   1. Setup                                        ####
##                                                   ****
##*******************************************************

## Version 1.1

## Disable scientific notation (for example e+09) in R
options(scipen = 999)

##****************************************************
##  ~~ 1.1 Library                                ####
##****************************************************

## load the required packages
library(glmnet)

##*******************************************************
##                                                   ****
##   2. Data                                         ####
##                                                   ****
##*******************************************************

## read ONS mortality data
DF_ONSmortality <- read.csv("data_ons_registeredmortality.csv", stringsAsFactors = FALSE)


##*******************************************************
##                                                   ****
##   3. Modelling of ONS mortality                   ####
##                                                   ****
##*******************************************************

##******************************************************
##  ~~ 3.1 Lasso Regression of ONS mortality (R5)   ####
##******************************************************

## Construction of Model R5

## Split data into train, test and predict
DF_ONSmortality_train <- DF_ONSmortality [1:8,]
DF_ONSmortality_test <- DF_ONSmortality [9:12,]
DF_ONSmortality_predict <- DF_ONSmortality [13:nrow(DF_ONSmortality),]

## Select columns for modelling
selectcols_ONSmortality_lasso = c("Age30to39", 
                                  "Age40to49", 
                                  "Age50to59",
                                  "Age60to69", 
                                  "Age80andabove")

DF_ONSmortality_train_lasso <- DF_ONSmortality_train[,selectcols_ONSmortality_lasso]
DF_ONSmortality_test_lasso <- DF_ONSmortality_test[,selectcols_ONSmortality_lasso]
DF_ONSmortality_predict_lasso <- DF_ONSmortality_predict[,selectcols_ONSmortality_lasso]

## Reproducibility
set.seed(123) 

## Target variable (80+ mortality) and model matrix
y_ONSmortality_lasso <- DF_ONSmortality_train_lasso$Age80andabove
x_ONSmortality_train_lasso <- model.matrix(Age80andabove ~.,  DF_ONSmortality_train_lasso)[,-1]
x_ONSmortality_test_lasso <- model.matrix(Age80andabove ~.,  DF_ONSmortality_test_lasso)[,-1]
x_ONSmortality_predict_lasso <- model.matrix(Age80andabove ~.,  DF_ONSmortality_predict_lasso)[,-1]

## Lasso regularised model
cv.lasso <- cv.glmnet(x_ONSmortality_train_lasso, y_ONSmortality_lasso, alpha = 1, family = "gaussian")
model_ONSmortality_lasso <- glmnet(x_ONSmortality_train_lasso, y_ONSmortality_lasso, alpha = 1, family = "gaussian", intercept = FALSE, lambda = cv.lasso$lambda.1se)
coef(model_ONSmortality_lasso)

## Train MAPE
DF_ONSmortality_output_train_lasso <- cbind(DF_ONSmortality_train_lasso$Age80andabove, predict(newx=x_ONSmortality_train_lasso, model_ONSmortality_lasso))
sum(abs(predict(newx=x_ONSmortality_train_lasso, model_ONSmortality_lasso)-DF_ONSmortality_train_lasso$Age80andabove)/DF_ONSmortality_train_lasso$Age80andabove)/nrow(DF_ONSmortality_train_lasso)

## Test MAPE
DF_ONSmortality_output_test_lasso <- cbind(DF_ONSmortality_test_lasso$Age80andabove, predict(newx=x_ONSmortality_test_lasso, model_ONSmortality_lasso))
sum(abs(predict(newx=x_ONSmortality_test_lasso, model_ONSmortality_lasso)-DF_ONSmortality_test_lasso$Age80andabove)/DF_ONSmortality_test_lasso$Age80andabove)/nrow(DF_ONSmortality_test_lasso)

## Predict
predict(newx=x_ONSmortality_predict_lasso, model_ONSmortality_lasso)

##******************************************************
##  ~~ 3.2 Gompertz Network of ONS mortality (R4)   ####
##******************************************************

## Construction of Model R4

## Reproducibility
set.seed(123) 

## Average of Gompertz slope
alpha = 0.107

## Gompertz projections
DF_ONSmortality$Gomp_Age30to39 <- DF_ONSmortality$Age30to39*exp(alpha*55)
DF_ONSmortality$Gomp_Age40to49 <- DF_ONSmortality$Age40to49*exp(alpha*45)
DF_ONSmortality$Gomp_Age50to59 <- DF_ONSmortality$Age50to59*exp(alpha*35)
DF_ONSmortality$Gomp_Age60to69 <- DF_ONSmortality$Age60to69*exp(alpha*25)

## Split data into train, test and predict
DF_ONSmortality_train_GN <- DF_ONSmortality[1:8,]
DF_ONSmortality_test_GN <- DF_ONSmortality[9:12,]
DF_ONSmortality_predict_GN <- DF_ONSmortality[13:nrow(DF_ONSmortality),]

## Select columns for modelling
selectcols_ONSmortality_GN = c("Gomp_Age30to39", 
                                  "Gomp_Age40to49", 
                                  "Gomp_Age50to59",
                                  "Gomp_Age60to69", 
                                  "Age80andabove")

DF_ONSmortality_train_GN <- DF_ONSmortality_train_GN[,selectcols_ONSmortality_GN]
DF_ONSmortality_test_GN <- DF_ONSmortality_test_GN[,selectcols_ONSmortality_GN]
DF_ONSmortality_predict_GN <- DF_ONSmortality_predict_GN[,selectcols_ONSmortality_GN]

## Reproducibility
set.seed(123) 

## Target variable (80+ mortality) and model matrix
y_ONSmortality_GN <- DF_ONSmortality_train_GN$Age80andabove
x_ONSmortality_train_GN <- model.matrix(Age80andabove ~.,  DF_ONSmortality_train_GN)[,-1]
x_ONSmortality_test_GN <- model.matrix(Age80andabove ~.,  DF_ONSmortality_test_GN)[,-1]
x_ONSmortality_predict_GN <- model.matrix(Age80andabove ~.,  DF_ONSmortality_predict_GN)[,-1]

## Gompertz Network model
cv.GN <- cv.glmnet(x_ONSmortality_train_GN, y_ONSmortality_GN, alpha = 1, family = "gaussian")
model_ONSmortality_GN <- glmnet(x_ONSmortality_train_GN, y_ONSmortality_GN, alpha = 1, family = "gaussian", intercept = FALSE, lambda = cv.GN$lambda.1se)
coef(model_ONSmortality_GN)

## Train MAPE
DF_ONSmortality_output_train_GN <- cbind(DF_ONSmortality_train_GN$Age80andabove, predict(newx=x_ONSmortality_train_GN, model_ONSmortality_GN))
sum(abs(predict(newx=x_ONSmortality_train_GN, model_ONSmortality_GN)-DF_ONSmortality_train_GN$Age80andabove)/DF_ONSmortality_train_GN$Age80andabove)/nrow(DF_ONSmortality_train_GN)

## Test MAPE
DF_ONSmortality_output_test_GN <- cbind(DF_ONSmortality_test_GN$Age80andabove, predict(newx=x_ONSmortality_test_GN, model_ONSmortality_GN))
sum(abs(predict(newx=x_ONSmortality_test_GN, model_ONSmortality_GN)-DF_ONSmortality_test_GN$Age80andabove)/DF_ONSmortality_test_GN$Age80andabove)/nrow(DF_ONSmortality_test_GN)

## Predict
predict(newx=x_ONSmortality_predict_GN, model_ONSmortality_GN)

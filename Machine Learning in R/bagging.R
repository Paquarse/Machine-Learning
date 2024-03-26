
########## Bagging 

library(ipred)

bag <- bagging(y ~., data = fin2_train, nbagg = 25, method = "standard",
               coob = TRUE, control = rpart.control(minsize = 50, cp = 0.01, maxdepth = 4,
                                                    minbucket = 15))

### forêt aléatoire

library(randomForest)

rf <- randomForest(formula = y ~., data = fin2_train, ntree = 500, mtry = sqrt(ncol(fin_data2)-1))
rf
hist(rf$oob.times)

vip(rf)
#ou
rf$importance
varImpPlot(rf)

## Performance : Boosting
library(ada)
boos_ada <- ada(x = fin2_train[,-8], y = fin2_train[,8], test.x = fin2_test[,-8], test.y = fin2_test[,8],
                loss = "exponential", type = "discrete", iter = 2000, nu = 1)

plot(boos_ada, kappa = F, test = F, tflag = F)
summary(boos_ada)


# Gradien boostique avec xbg
## Etape 0
library(xgboost)

dtrain <- subset(fin2_train, select = c(-y))
ytrain <- subset(fin2_train, select = c(y))
ytrain$y <- as.numeric(ytrain$y) - 1

dtest <- subset(fin2_test, select = c(-y))
ytest <- subset(fin2_test, select = c(y))
ytest$y <- as.numeric(ytest$y) - 1

# Format Matriciel 

ddtrain <- xgb.DMatrix(data = data.matrix(dtrain), label = ytrain$y)
ddtest <- xgb.DMatrix(data = data.matrix(dtest), label = ytest$y)

watchlist <- list(train = ddtrain, test = ddtest)

# validation croisée

xgbcv <- xgb.cv(params = list(objective = "binary:logistic", eta = 0.1,
                              max.depth = 6, nthread = 4), ddtrain, nrounds = 1000,
                nfold = 5, watchlist = watchlist, early_stopping_rounds = 10
                )


# Creation du modèle

num_round = 31 

  













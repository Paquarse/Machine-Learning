##########################################################################
## COURS  : INTRODUCTION AU MACHINE LEARNING : apprentissage supervise  ##
## DATE   : septembre-novembre 2022                                     ##
## MASTER : MAS-PPE                                                     ##
## AUTEUR : D. DELAUNAY                                                 ##
##########################################################################

###################################################
## 1. Installation et chargement des librairies  ##
###################################################



library(descr)
library(ggplot2)
library(psych)
library(Hmisc)
library(gmodels) # Cross Tables [CrossTable()]
library(ggmosaic) # Mosaic plot with ggplot [geom_mosaic()]
library(corrplot) # Correlation plot [corrplot()]
library(ggpubr) # Arranging ggplots together [ggarrange()]
library(cowplot) # Arranging ggplots together [plot_grid()]
library(caret) # ML [train(), confusionMatrix(), createDataPartition(), varImp(), trainControl()]
library(ROCR) # Model performance [performance(), prediction()]
library(plotROC) # ROC Curve with ggplot [geom_roc()]
library(pROC) # AUC computation [auc()]
library(PRROC) # AUPR computation [pr.curve()]
library(rpart) # Decision trees [rpart(), plotcp(), prune()]
library(rpart.plot) # Decision trees plotting [rpart.plot()]
library(ranger) # Optimized Random Forest [ranger()]
#library(lightGBM) # Light GBM [lgb.train()]
library(xgboost) # XGBoost [xgb.DMatrix(), xgb.train()]
library(MLmetrics) # Custom metrics (F1 score for example)
library(tidyverse) # Data manipulation
library(forcats)
library(tidyr)
library(ggplot2)
library(corrplot)
library(cowplot)
library(e1071)
library(glmnet)
library(rattle)
library(randomForest)
library(plotly)
library(FactoMineR) 
library(factoextra) 
library(class) 
library(dplyr)
library(Metrics)
library(rlang)

######################################################
## 2. PREPARATION DES DONNEES POUR LA MODELISATION  ##
######################################################

## 2.1 Preparation des predicteurs
## -------------------------------

## On part de la table fin_DATA issue du prg ML_Exploration_Traitement.R
## ---------------------------------------------------------------------
fin_data2 = fin_data %>% select(-age)

## Transforme chaque variable caractere en facteur
## -----------------------------------------------
fin_data2$cl_age <- factor(fin_data2$cl_age)
levels(fin_data2$cl_age)

fin_data2$cl_campaign <- factor(fin_data2$cl_campaign)
levels(fin_data2$cl_campaign)

fin_data2$cl_pdays <- factor(fin_data2$cl_pdays)
levels(fin_data2$cl_pdays)

fin_data2$cl_previous <- factor(fin_data2$cl_previous)
levels(fin_data2$cl_previous)

fin_data2$contact <- factor(fin_data2$contact)
levels(fin_data2$contact)

fin_data2$poutcome <- factor(fin_data2$poutcome)
levels(fin_data2$poutcome)

fin_data2$day_of_week <- factor(fin_data2$day_of_week)
levels(fin_data2$day_of_week)

# Changer la modalite de reference
# --------------------------------
fin_data2$cl_age <- relevel(fin_data2$cl_age,"(01) <= 30ans")
fin_data2$cl_campaign <- relevel(fin_data2$cl_campaign,"5 appels et +")
fin_data2$cl_pdays <- relevel(fin_data2$cl_pdays,"0")
fin_data2$cl_previous <- relevel(fin_data2$cl_previous,"2+")
fin_data2$cl_job <- relevel(fin_data2$cl_job,"blue-collar,entrepreneur")
fin_data2$cl_marital <- relevel(fin_data2$cl_marital,"single")
fin_data2$cl_month <- relevel(fin_data2$cl_month,"(05)may")
fin_data2$cl_education <- relevel(fin_data2$cl_education,"basic.6y,basic.9y")
fin_data2$contact <- relevel(fin_data2$contact,"telephone")
fin_data2$poutcome <- relevel(fin_data2$poutcome,"nonexistent")
fin_data2$day_of_week <- relevel(fin_data2$day_of_week,"(01)mon")


## 2.2 Partitionnement de la base en TRAIN et TEST
## -----------------------------------------------

## A) Echantillonnage al?atoire avec la fonction createDataPartition()
## -------------------------------------------------------------------
set.seed(1234)
ind = createDataPartition(fin_data2$y,
                          times = 1,
                          p = 0.75,
                          list = F)
fin2_train = fin_data2[ind, ]
fin2_test = fin_data2[-ind, ]

# Verification de la representativite des ecrm(hantillons

Hmisc::describe(fin2_train$y)
Hmisc::describe(fin2_test$y)

## B) Autre solution : echantillonnage aleatoire stratifie
## -------------------------------------------------------
set.seed(1234)
train.idx <- createDataPartition(y = fin_data2$y, p = 0.75, list = FALSE)
fin2_train <- fin_data2[train.idx,] # creation du jeu de donn?es "Train" 
fin2_test <- fin_data2[-train.idx,] # creation du jeu de donn?es "Test"
Hmisc::describe(fin2_train$y)
Hmisc::describe(fin2_test$y)

res <- glm(formula = y ~., data =  fin2_train, family = "binomial")

# Arbre de décision 

arbre <- rpart(formula = y ~., data = fin2_train, method = "class", parms = list(split = "gini"))
# control : pour spécifier les contraintes
fancyRpartPlot(arbre, digits = 2, palettes = c("Purples", "Oranges"))

rpart.plot(arbre)

arbre2 <- rpart(formula = y ~., data = fin2_train, method = "class", 
                parms = list(split = "gini"), 
                control = rpart.control(minsplit = 50), maxdepth = 4)

rpart.plot(arbre2)
fancyRpartPlot(arbre2, digits = 2, palettes = c("Purples", "Oranges"))

# critere de complexite
summary(arbre2, digits = 3)

#variable importance
library(vip)
vip(arbre2)

#prune : pour faire l'élagage de l'abre







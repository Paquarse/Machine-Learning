#############################################################
##  COURS : INTRODUCTION AU MARCHINE LEARNING              ##
##  Seance: ANALYSE EXPLORATOIRE ET TRAITEMENT DES DONNEES ##
##  Date  : septembre-novembre 2022                        ##
#############################################################

# #################
# 1.INTRODUCTION
# #################

## 1.1 Chargement des librairies
## -----------------------------
library(gmodels) # Cross Tables [CrossTable()]
library(ggmosaic) # Mosaic plot with ggplot [geom_mosaic()]
library(corrplot) # Correlation plot [corrplot()]
library(ggpubr) # Arranging ggplots together [ggarrange()]
library(cowplot) # Arranging ggplots together [plot_grid()]
library(caret) # ML [train(), confusionMatrix(), createDataPartition(), varImp(), trainControl()]
library(tidyverse) # Data manipulation
library(Matrix) # Sparse and Dense Matrix Classes and Methods
library(ROCR) # 


## 1.2 Import du fichier .CSV
## ---------------------------
fin_data = read.csv(file = "finance.csv",
                    sep = ";",
                    stringsAsFactors = F)

## 1.3 Description de la base
## --------------------------
dim(fin_data)
# 4119 obs et 21 var
names(fin_data)

CrossTable(fin_data$y)
# no : 3668 (89.1%) / yes : 451 (10.9%)

fin_data = fin_data %>% 
  mutate(y = factor(if_else(y == "yes", "1", "0"), 
                    levels = c("0", "1")))
# no --> 0 et yes --> 1

#fin_data = fin_data %>% 
#  mutate(y = factor(if_else(y == "yes", "1", "0"), 
#                   levels = c("0", "1"), labels = c("No", "Yes"))) : mon perso

head(fin_data)
sum(is.na(fin_data))
# Aucune valeur manquante sur les variables
sum(fin_data == "unknown")
# 1230 "unknown" dans la base

fin_data %>% 
  summarise_all(list(~sum(. == "unknown"))) %>% 
  gather(key = "variable", value = "nr_unknown") %>% 
  arrange(-nr_unknown)
# 6 variables concernees par les "unknown" : defaut (803), education (167), housing (105),
# loan (105), job (39) et marital (11) 


## ##################################################
## 2. ANALYSE EXPLORATOIRE ET TRAITEMENT DES DONNEES
## ##################################################

## Theme par defaut pour ggplot
## ----------------------------
theme_set(theme_bw())

## Parametres par defaut pour les graphes en mosaic
## ------------------------------------------------
mosaic_theme = theme(axis.text.x = element_text(angle = 90,
                                                hjust = 1,
                                                vjust = 0.5),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())

### 2.1 ANALYSE UNIVARIEE
### ---------------------

#### Age
#### --- 
summary(fin_data$age)
# etendue : 18 ? 88 ans. moyenne = 40 ans

fin_data %>% 
  ggplot() +
  aes(x = age) +
  geom_bar() +
  geom_vline(xintercept = c(30, 55), 
             col = "red",
             linetype = "dashed") +
  facet_grid(y ~ .,
             scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 90, 5))

fin_data %>% 
  mutate(elder55 = if_else(age > 55, "1", "0")) %>% 
  group_by(y) %>% 
  add_count(nr_y = n()) %>% 
  group_by(elder55, y) %>% 
  summarise(abs_freq = n(),
            relative_freq = round(100*n()/first(nr_y), 2))

fin_data = fin_data %>% 
  mutate(cl_age = if_else(age > 55, "(03) > 55 ans", if_else(age > 30, "(02) 31-55ans", "(01) <= 30ans")))

# ---------------
# Fonction pour generer des tableaux croises avec des parametres par defaut
fun_crosstable = function(df, var1, var2){
  # df: dataframe containing both columns to cross
  # var1, var2: columns to cross together.
  CrossTable(df[, var1], df[, var2],
             prop.r = T,
             prop.c = F,
             prop.t = F,
             prop.chisq = F,
             dnn = c(var1, var2))
}

# ---------------

fun_crosstable(fin_data, "cl_age", "y")

#### Job
#### ---
table(fin_data$job)
fun_crosstable(fin_data, "job", "y")

fin_data = fin_data %>% 
  filter(job != "unknown")
# suppression de 39 individus

fin_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(y, job), fill = y)) +
  mosaic_theme +
  xlab("Job") +
  ylab(" ")

fin_data$cl_job <- fct_collapse(fin_data$job,
                                "student,retired,unemployed" = c("student", "retired","unemployed"),
                                "housemaid, self-employed,services" = c("self-employed","housemaid","services"),
                                "blue-collar,entrepreneur" = c("blue-collar","entrepreneur"))

fun_crosstable(fin_data, "cl_job", "y")

# suppression de la variable JOB
fin_data = fin_data %>% 
  select(-job)


#### Marital (Situation maritale)
#### ----------------------------
fun_crosstable(fin_data, "marital", "y")

fin_data = fin_data %>% 
  filter(marital != "unknown")
# suppression de 11 individus

fin_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(y, marital), fill = y)) +
  mosaic_theme +
  xlab("Marital status") +
  ylab("")

fin_data$cl_marital <- fct_collapse(fin_data$marital,
                                    "divorced,married" = c("divorced", "married"))

fun_crosstable(fin_data, "cl_marital", "y")

# suppression de la variable JOB
fin_data = fin_data %>% 
  select(-marital)


#### Education (niveau d'education)
#### ------------------------------
fun_crosstable(fin_data, "education", "y")

# suppression de l'individu illettre
fin_data = fin_data %>% 
  filter(education != "illiterate")

fin_data = fin_data %>% 
  mutate(education = recode(education, "unknown" = "university.degree"))

fin_data$cl_education <- fct_collapse(fin_data$education,
                                      "basic.6y,basic.9y" = c("basic.6y", "basic.9y"))

fun_crosstable(fin_data, "cl_education", "y")

# suppression de la variable EDUCATION
fin_data = fin_data %>% 
  select(-education)

fin_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(y, cl_education), fill = y)) +
  mosaic_theme +
  xlab("Education") +
  ylab(" ")


#### Default (defaut de remboursement de credit)
#### -------------------------------------------
fun_crosstable(fin_data, "default", "y")

fin_data = fin_data %>% select(-default)

#### Housing (pret immobilier en cours)
#### ----------------------------------
fun_crosstable(fin_data, "housing", "y")

chisq.test(fin_data$housing, fin_data$y)

fin_data = fin_data %>% 
  select(-housing)

#### Loan (pret personnel en cours)
#### ------------------------------
fun_crosstable(fin_data, "loan", "y")

chisq.test(fin_data$loan, fin_data$y)

fin_data = fin_data %>% 
  select(-loan)

#### Contact (canal de contact avec le client)
#### -----------------------------------------
fun_crosstable(fin_data, "contact", "y")
# on conserve la variable en l'etat

#### Month (mois de la campagne marketing)
#### -------------------------------------
month_recode = c("jan" = "(01)jan",
                 "feb" = "(02)feb",
                 "mar" = "(03)mar",
                 "apr" = "(04)apr",
                 "may" = "(05)may",
                 "jun" = "(06)jun",
                 "jul" = "(07)jul",
                 "aug" = "(08)aug",
                 "sep" = "(09)sep",
                 "oct" = "(10)oct",
                 "nov" = "(11)nov",
                 "dec" = "(12)dec")

fin_data = fin_data %>% 
  mutate(month = recode(month, !!!month_recode))

fun_crosstable(fin_data, "month", "y")

fin_data %>% 
  ggplot() +
  aes(x = month, y = ..count../nrow(fin_data), fill = y) +
  geom_bar() +
  ylab("relative frequency")

fin_data$cl_month <- fct_collapse(fin_data$month,
                                  "(08)aug,(11)nov" = c("(08)aug", "(11)nov"),
                                  "(03)mar,(09)sep,(10)oct,(12)dec" = c("(03)mar","(09)sep","(10)oct","(12)dec"))

fun_crosstable(fin_data, "cl_month", "y")

# suppression de la variable MONTH
fin_data = fin_data %>% 
  select(-month)

fin_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(y, cl_month), fill = y)) +
  mosaic_theme +
  xlab("Month") +
  ylab(" ")


#### Day of the week (jour du contact avec le client)
#### ------------------------------------------------
day_recode = c("mon" = "(01)mon",
               "tue" = "(02)tue",
               "wed" = "(03)wed",
               "thu" = "(04)thu",
               "fri" = "(05)fri")

fin_data = fin_data %>% 
  mutate(day_of_week = recode(day_of_week, !!!day_recode))

fun_crosstable(fin_data, "day_of_week", "y")
# On conserve la variable en l'etat

#### Duration (duree du dernier contact avec le client)
#### --------------------------------------------------
fin_data = fin_data %>% 
  select(-duration)

#### Campaign (nombre de fois o? un client a ete contacte pendant la campagne)
#### -------------------------------------------------------------------------
fin_data %>% 
  ggplot() +
  aes(x = campaign) +
  geom_bar() +
  facet_grid(y ~ .,
             scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 50, 5))

fun_crosstable(fin_data, "campaign", "y")

prop_row = fun_crosstable(fin_data, "campaign", "y")$prop.row %>% 
  as.data.frame() %>% 
  filter(y == 1)
prop_row %>% 
  ggplot() +
  aes(x = x,
      y = Freq) +
  geom_point() +
  geom_hline(yintercept = 0.09, col = "red")

fin_data = fin_data %>% 
  mutate(cl_campaign = if_else(campaign > 4, "5 appels et +", "< =4 appels"))

fun_crosstable(fin_data, "cl_campaign", "y")

# suppression de la variable CAMPAIGN
fin_data = fin_data %>% 
  select(-campaign)


#### Pdays (est-ce que le client a ete deja ete contacte lors d'une precedente campagne)
#### -----------------------------------------------------------------------------------
table(fin_data$pdays)

fin_data = fin_data %>% 
  mutate(cl_pdays = if_else(pdays == 999, "0", "1")) %>% 
  select(-pdays)

fun_crosstable(fin_data, "cl_pdays", "y")


#### Previous (nombre de contacts sur les campagnes precedentes)
#### -----------------------------------------------------------
table(fin_data$previous)

fin_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(previous), fill = y)) +
  mosaic_theme +
  xlab("Previous") +
  ylab(" ")

fin_data = fin_data %>% 
  mutate(cl_previous = if_else(previous >=  2, "2+", if_else(previous == 1, "1", "0")))

fun_crosstable(fin_data, "cl_previous", "y")

# suppression de la variable PREVIOUS
fin_data = fin_data %>% 
  select(-previous)

#### Poutcome (resultat des sollicitations precedentes)
#### --------------------------------------------------
fun_crosstable(fin_data, "poutcome", "y")
# On conserve la variable en l'etat
fun_crosstable(fin_data, "poutcome", "cl_previous")


### 2.2 ANALYSE BIVARIEE
### --------------------

## Analyse des 5 variables socio-economiques
## -----------------------------------------
fin_data %>% 
  select(emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed) %>% 
  cor() %>% 
  corrplot(method = "number",
           type = "upper",
           tl.cex = 0.8,
           tl.srt = 45,
           tl.col = "black")

# Suppression de EMP.VAR.RATE (fortement correlee aux autres var socio-economiques)
# ---------------------------------------------------------------------------------
fin_data = fin_data %>% 
  select(-emp.var.rate)

## Matrice de correlation sur les 4 var socio-economiques
## -----------------------------------------------------
fin_data %>% 
  select(cons.price.idx, cons.conf.idx, euribor3m, nr.employed) %>% 
  cor() %>% 
  corrplot(method = "number",
           type = "full",
           tl.cex = 0.8,
           tl.srt = 45,
           tl.col = "black")

## On splite les features Caractere des autres features (numerique ou facteur) pour generer
## les tests appropries : ANOVA pour les var numeriques et TEST DU CHI2 pour les var discretes
## -------------------------------------------------------------------------------------------
fin_data_x_dbl = fin_data %>%
  select_if(~{is.double(.)})

fin_data_x_chr = fin_data %>%
  select_if(~is.character(.) | is.factor(.))

fin_data_x_chr = fin_data_x_chr %>% 
  select(-y)

## Analyse ANOVA 
## -------------
summary(aov(cons.price.idx ~ fin_data$y,
            data = fin_data_x_dbl))
summary(aov(cons.conf.idx ~ fin_data$y,
            data = fin_data_x_dbl))
summary(aov(euribor3m ~ fin_data$y,
            data = fin_data_x_dbl))
summary(aov(nr.employed ~ fin_data$y,
            data = fin_data_x_dbl))

## Calcul du V de Cramer entre y et les var discretes
## -------------------------------------------------
cramer = data.frame(NA, ncol(fin_data_x_chr), 3)

for (i in (1:ncol(fin_data_x_chr))){
  tab = table(fin_data_x_chr[, i], fin_data$y)
  chisq_results = chisq.test(tab)
  cramer[i, 1] = names(fin_data_x_chr)[i]
  cramer[i, 2] = round(sqrt(chisq_results$statistic/(nrow(fin_data_x_chr))), 3)
  cramer[i, 3] = signif(chisq_results$p.value, 3)
}
colnames(cramer) = c("variable", "cramerv", "pvalue_chi2")
cramer

## Representation graphique du V de Cramer entre y et les var discretes
## --------------------------------------------------------------------
cramer %>% 
  arrange(-cramerv) %>% 
  ggplot() +
  aes(x = reorder(variable, -cramerv),
      y = cramerv,
      fill = -cramerv) +
  geom_bar(stat = "identity",
           show.legend = F) +
  xlab("Variable") +
  ylab("Cramer's V") +
  ggtitle("Cramer's V against the explained response") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_fill_gradient(high = "pink",
                      low = "darkred")

## Calcul du V de Cramer entre les var discretes
## ---------------------------------------------
cramer = matrix(NA, ncol(fin_data_x_chr), ncol(fin_data_x_chr))

for (i in (1:ncol(fin_data_x_chr))){
  for (j in (1:ncol(fin_data_x_chr))){
    tab = table(fin_data_x_chr[, i], fin_data_x_chr[, j])
    chisq_results = chisq.test(tab)
    cramer[i, j] = sqrt(chisq_results$statistic/(nrow(fin_data_x_chr) * (min(dim(tab)) -1)))
  }
}

cramer = round(cramer, 3)
colnames(cramer) = colnames(fin_data_x_chr)
rownames(cramer) = colnames(fin_data_x_chr)
corrplot(cramer,
         method = "shade",
         type = "upper",
         diag = F,
         tl.srt = 45, 
         tl.col = "black",
         tl.cex = 0.6, 
         addCoef.col = "darkgreen", 
         addCoefasPercent = T)

# EN CONCLUSION ----------------------------------------------------------------------
# Variables supprimees    : default (manque de variabilite), housing (manque d'info),
#                           loan (manque d'info) et emp.var.rate (pb de correlation)
# Variables dichotomisees : previous, month
# Variables discretisees  : age, pdays, campaign
# Variables retraitees    : job, marital, education
# Variables en l'etat     : contact, poutcome, day_of_week
# Variables continues conservees malgre leur lien fort : nr.employed avec euribor3m 
# Variables discretes conservees malgre leur lien fort : poutcome avec cl_pdays
# ------------------------------------------------------------------------------------


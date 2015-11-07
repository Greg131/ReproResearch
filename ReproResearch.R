# ----------------------------------------------------------------
# Reproductible Research
# Comment permettre la comprehension, validation, réutilisation par des tiers ?
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# Week 1
# ----------------------------------------------------------------

setwd("~/Dropbox/Datascience/Universite_Johns-Hopkins/Recherche_reproductible/ReproResearch")

# Steps in Data analysis : 

# 1 - Define the question
# Peut-on determiner si des emails sont des SPAMS ou non?
# Et dans un contexte d'analyse de données :
# Peut-on utiliser des caractéristiques quqntitatives d'email 
# pour les classifier en SPAMS / HAM
# 2 - Define the ideal data set
# 3 - Determine what data your can access
# 4 - Obtain the data 
# 5 - Clean the data 

library(kernlab)
data(spam)

# 6 - Exploratory data analysis
# Look at summary of the data
# Check missing data
# Make exploratory plots
# Perfoprm exploratory analysis

str(spam[, 1:5])
summary(spam)
str(spam)
nrow(spam)

# A data frame with 4601 observations and 58 variables.
# The first 48 variables contain the frequency of the variable name 
# (e.g., business) in the e-mail. 
# If the variable name starts with num (e.g., num650) the 
# it indicates the frequency of the corresponding number (e.g., 650). 
# The variables 49-54 indicate 
# the frequency of the characters ‘;’, ‘(’, ‘[’, ‘!’, ‘\$’, and ‘\#’. 
# The variables 55-57 contain the average, longest and total run-length 
# of capital letters. 
# Variable 58 indicates the type of the mail and is either "nonspam" or "spam", 
# i.e. unsolicited commercial e-mail.
  
# Perform the subsampling
# Séparation du Data set en Training set et Test set de façon aléatoire
set.seed(3435)
?rbinom()
trainIndicator = rbinom(4601, size = 1, prob = 0.5) 
table(trainIndicator)
## trainIndicator vecteur de 0 et 1
## 0 1
## 2314 2287

# Training set
trainSpam = spam[trainIndicator == 1, ]
# Test set
testSpam = spam[trainIndicator == 0, ]                                                                                                                                                                                                                                                                                                      
                                       
names(trainSpam)                                                                                                                                                                                                                                                               
head(trainSpam)
str(trainSpam)
table(trainSpam$type)

barplot(table(trainSpam$type), col = "wheat2", main = "Répartition SPAM / HAM")

summary(trainSpam$capitalAve) # average nb of cap letters by word ? by type
mean(trainSpam$capitalAve)
sd(trainSpam$capitalAve)

boxplot(trainSpam$capitalAve, col = "steelblue")
boxplot(capitalAve ~ type, data = trainSpam , col = "red") 
boxplot(log10(capitalAve) ~ type, data = trainSpam , col = "red") 
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve) ~ trainSpam$type)

summary(trainSpam$business)
boxplot(log10(business +1) ~ type, data = trainSpam , col = "red") 
summary(trainSpam$charDollar)
boxplot(log10(charDollar +1) ~ type, data = trainSpam , col = "red") 


# Relationship between predictors
plot(log10(trainSpam[, 1:4] + 1))
plot(log10(trainSpam[, 1:2] + 1))
plot(log10(trainSpam[, 7:8] + 1))
plot(log10(trainSpam[, c(7,18)] + 1))
plot(log10(trainSpam[, c(7,18,3)] + 1))
plot(log10(trainSpam[, c(55,56,57)] + 1))
# Evidement nombre total de lettre majuscules 
# et suite de maj la plus lonque sont correllées

# Clustering
?dist # matrice de distance par defaut euclidienne entre les lignes
?t() # matrice transpose
# L'idée est ici de voir comment les features sont "proches"
hCluster = hclust(dist(t(trainSpam[, 1:57]))) 
plot(hCluster)

# Et si on faisait du unsupervised learning sur les lignes (les mails) au lieu des features
set.seed(3435)
?sample()
echantillon100Indicator <- sample(1:nrow(trainSpam),100)
echantillon100 <- trainSpam[echantillon100Indicator, ]
hCluster = hclust(dist(echantillon100[, 1:57]))
plot(hCluster)

clusters4 <- cutree(hCluster, k=4)
clusters10 <- cutree(hCluster, k=10)
clusters20 <- cutree(hCluster, k=20)
echantillon100$clusters4 <- clusters4
echantillon100$clusters10 <- clusters10
echantillon100$clusters20 <- clusters20

par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1)) #  initialiser

plot(echantillon100[, c(58,59,60,61)])
plot(jitter(echantillon100[, 59]),echantillon100[, 58])


#------------------------------------------------
hCluster = hclust(dist(t(trainSpam[, 1:54]))) 
plot(hCluster)

hCluster = hclust(dist(t(log10(trainSpam[, 1:54]+1)))) 
plot(hCluster)

# 7 Statistical prediction/modeling

levels(trainSpam$type)
# Transformer le type (de type factor) en 0 pour HAM et 1 pour SPAM
trainSpam$numType = as.numeric(trainSpam$type) - 1 

# Fonction de coût
costFunction = function(x, y) sum(x != (y > 0.5)) 
cvError = rep(NA, 55)
library(boot)

?reformulate()
lmFormula = reformulate(names(trainSpam)[2], response = "numType") 
# construit une formule numType tild address

for (i in 1:55) {
        lmFormula = reformulate(names(trainSpam)[i], response = "numType") 
        glmFit = glm(lmFormula, family = "binomial", data = trainSpam) 
        cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

## Which predictor has minimum cross-validated error?
## Attention en fit il s'agit de "Test Error"
names(trainSpam)[which.min(cvError)]
## [1] "charDollar"

## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)
## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam) 
predictedSpam = rep("nonspam", dim(testSpam)[1]) # taille 2314
## Classify as `spam' for those with prob > 0.5
## ERROR predictedSpam[predictionModel$fitted > 0.5] = "spam"
predictedSpam[predictionTest > 0.5] = "spam"

# 8 - Interpret Results

## Classification table
table(testSpam$type)
table(predictedSpam, testSpam$type)

## predictedSpam nonspam spam
## nonspam 1361 460
## spam 46 447
## Error rate
(46 + 460)/(1361 + 460 + 46 + 447)
## [1] 0.218669

## En fait se lit :
## true negatives 1361
## true positives 447
## False positives (predit positiofs mais neg en vrai) 46
## False negatives (pred neg mais positif en vrai) 460
 

## Precision : True Pos / Predicted Pos  ou TP/(TP+FP)
Precision <- 447/(447+46)
## 91%

## Recall : True Pos / Actual Pos  ou TP/(TP+FN)
Recall <- 447/(447+460)
## 49%

## F1 Score : 2 PR/(P+R)
F1 <- 2*Precision*Recall/(Precision+Recall)
F1
## 0.64

##--------------------------------------------------------
## Etape Statistical prediction/modeling plus sérieuse
set.seed(3435)
## Taille du DataSet
nrow(spam)
## Training set 60%, Cross validation set 20%, Test set 20%
bound1 <- floor((nrow(spam)/5)*3)         # 60% 
bound2 <- floor((nrow(spam)/5)*4)         # 80% 

RandomlysortedSet <- spam[sample(nrow(spam)), ]

# Training set environ 60%
TrainingSet <- RandomlysortedSet[1:bound1, ] 
# Cross salidation set environ 20%
CrossValidationSet <- RandomlysortedSet[(bound1+1):bound2, ]                                                                                                                                                                                                                                                                                                     
# Test set envitron 20%
TestSet <- RandomlysortedSet[(bound2+1):nrow(spam), ]  

TrainingSet$numType = as.numeric(TrainingSet$type) - 1 
CrossValidationSet$numType = as.numeric(CrossValidationSet$type) - 1 

costFunction = function(x, y) sum(x != (y > 0.5)) 
TrainError = rep(NA, 55)
TestError = rep(NA, 55)
CVError = rep(NA, 55)

library(boot)
?glm()

for (i in 1:55) {
        lmFormula = reformulate(names(TrainingSet)[i], response = "numType") 
        glmFit = glm(lmFormula, family = "binomial", data = TrainingSet) 
        TrainError[i] = costFunction(TrainingSet$numType,glmFit$fitted)
        predictionCV = predict(glmFit,CrossValidationSet)
        CVError[i] = costFunction(CrossValidationSet$numType,predictionCV)
}

names(TrainingSet)[which.min(TrainError)]
# [1] "charDollar"
names(TrainingSet)[which.min(CVError)]
# [1] "charDollar"

# Visulisation Bias et variance, ...
# par(mfrow = c(2,1), mar = c(5.1,4.1,4.1,2.1)) #  initialiser
par(mfrow = c(2,1), mar = c(2,4.1,4.1,2.1)) 

plot(TrainError, col = "blue")
plot(CVError, col = "red")

#----------------------------------------------------------
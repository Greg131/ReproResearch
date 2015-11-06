setwd("~/Dropbox/Datascience/Universite_Johns-Hopkins/Recherche_reproductible/ReproResearch")


library(kernlab)
data(spam)
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
set.seed(3435)
?rbinom()
trainIndicator = rbinom(4601, size = 1, prob = 0.5) 
table(trainIndicator)
## trainIndicator 
## 0 1
## 2314 2287

# Training set
trainSpam = spam[trainIndicator == 1, ]
# Test set
testSpam = spam[trainIndicator == 0, ]                                                                                                                                                                                                                                                                                                      
                                       
str(trainSpam)
names(trainSpam)                                                                                                                                                                                                                                                               
head(trainSpam)
table(trainSpam$type)
barplot(table(trainSpam$type), col = "wheat2", main = "Répartition SPAM / non SPAM")


summary(trainSpam$capitalAve) # average nb of cap letters by word ? by type
mean(trainSpam$capitalAve)
sd(trainSpam$capitalAve)

boxplot(trainSpam$capitalAve, col = "steelblue")
boxplot(capitalAve ~ type, data = trainSpam , col = "red") 
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

# Relationship between predictors
plot(log10(trainSpam[, 1:4] + 1))

# Clustering
hCluster = hclust(dist(t(trainSpam[, 1:57]))) 
plot(hCluster)

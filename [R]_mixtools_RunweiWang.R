#### 1. Preparation
library(mixtools)
library(tidyverse)
library(data.table)
library(ggplot2)
library(caret)


#### 2. Load the dataset into the R
wine = fread("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header = F)
names(wine) = c("Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity", "Hue", "OD280/OD315_of_diluted_wines", "Proline")
pred = cbind(wine$Total_phenols, rep(1, 178))

#### 3. Pattern of the selected variables
ggplot(wine, aes(x = Color_intensity)) + geom_density() + ggtitle("Density plot of response 'Color_intensity'") + theme(plot.title = element_text(hjust = 0.5))

ggplot(wine, aes(x = Total_phenols)) + geom_density() + ggtitle("Density plot of predictor 'Total_phenols'") + theme(plot.title = element_text(hjust = 0.5))

ggplot(wine, aes(x = Class)) + geom_histogram() + ggtitle("Histogram of the ground truth class 'Class'") + theme(plot.title = element_text(hjust = 0.5))

#### 4. Model fitting
winereg <- regmixEM(wine$Color_intensity, 
                    pred, 
                    lambda = c(0.3370787, 0.3314607, 0.3314607),
                    beta = matrix(c(2.6478173, 2.3281364, 3.2570108, -3.3001476, 2.9323563, -4.4052074), nrow = 2, ncol = 3), 
                    sigma = sqrt(c(5.3676108, .41571224, .92670911)), 
                    addintercept = FALSE, 
                    k = 3)

#### 5. Results of model fitting
summary(winereg)


#### 6. Clustering
post = winereg$posterior
post = as.data.frame(post)
names(post) = c('Class_1', 'Class_2', 'Class_3')


#### 7. Classification
res = cbind(post, wine$Class)
res = as.data.frame(res)
names(res) = c('Class_1', 'Class_2', 'Class_3', 'Class_true')
med = group_by(res, Class_true) %>%
summarise(med1 = median(Class_1), 
med2 = median(Class_2), 
med3 = median(Class_3))

final = data.frame()
for (i in 1:178)
{
  final[i,1] = which.max(post[i,])
}
final[,2] = final[,1]-1
final[,2][final[,2] == 0] = 3

post1 = cbind(post, rep(0,178))
for (i in 1:178)
{
  post1[i,4] = max(post[i,])
}
post2 = as.data.frame(post1)
res = cbind(post2, final, wine$Class, rep(0,178))
res[,8][res[,6] == res[,7]] = 1
names(res) = c('C1', 'C2', 'C3', 'Max', 'Cluster', 'Class_pred', 'Class_true', 'match')
clustering = group_by(res, Class_true) %>%
summarise(Right_percentage = sum(match)/n())



#### 8. Accuracy of the classification
x = factor(res$Class_pred)
y = factor(res$Class_true)
z = confusionMatrix(x, y)
z$table








library(ISLR)
require(tree)
data(package="ISLR")
carseats<-Carseats

# Take cleaned data from previous steps, then apply here
adult.cmplt <- tem_data
ratio = sample(1:nrow(adult.cmplt), size = 0.80*nrow(adult.cmplt))
training_data = adult.cmplt[ratio,] #Test dataset 20% of total
testing_data = adult.cmplt[-ratio,] #Train dataset 80% of total

#regression model  creation on all predictors:
regression_glm_fit<- glm(income~., family = binomial(link='logit'), data = training_data)
summary(regression_glm_fit)
# creating prediction of income on all predictors
regression_glm_all_predictors <-  predict(regression_glm_fit , testing_data, type = "response")

hist(regression_glm_all_predictors)
hist(regression_glm_all_predictors[testing_data$income], breaks=20, ,col='yellow',xlab = 'testing-data-target',main="Histogram of testing-prediction(target)")
table(predicted= regression_glm_all_predictors>0.5,actual= testing_data$income)

predicted_income <- rep('<=50K', length(regression_glm_all_predictors))
predicted_income[regression_glm_all_predictors>=.5] <- '>50K'
# confusion matrix 
table(predicted_income, actual_income=testing_data$income)
#confusionMatrix( regression_glm_all_predictors, testing_data$income )

## decision tree: its confusion matrix is working
library(rpart)
total_tree <- rpart(income~., data=tem_data)
plot(total_tree)
text(total_tree, pretty = 0)
#decision tree model
tree_model<- rpart(income~., data=training_data)
plot(tree_model)
text(tree_model, pretty = 0)
tree_predict<- predict(tree_model, testing_data, type = "class")
confusionMatrix(tree_predict,  testing_data$income, positive = '2') # 86% accuracy

# Support Vector Machine
svm.model<- svm(income~., data = training_data,kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, testing_data)
confusionMatrix(testing_data$income, svm.predict)

# Random Forest
library(randomForest)
rf.model<- randomForest(income~., data = training_data, importance=TRUE, keep.forest=TRUE)
rf.predict <- predict(rf.model, testing_data)
confusionMatrix(testing_data$income, rf.predict) 


#### for checking k=1 is accurat or not
mydata <- tem_data
d1 <- model.matrix(~.-1, data=mydata)
set.seed(20) 
result.kmean = kmeans(d1, 2, nstart = 50, iter.max = 15) 
result.kmean.mm <- table(tem_data$income, result.kmean$cluster)
purity.kmean <- sum(apply(result.kmean.mm, 2, max)) / nrow(mydata)

#some good plots for learning, please check.
qplot(income, data = adult.cmplt, fill = occupation) + facet_grid (. ~ occupation)
qplot(income, data = adult.cmplt, fill = education) + facet_grid (. ~ education)

qplot(income, data = adult.cmplt, fill = relationship) + facet_grid (. ~ race)
qplot(income, data = adult.cmplt, fill = age) + facet_grid (. ~ age)

qplot(income, data = adult.cmplt, fill = marital.status) + facet_grid (. ~ marital.status)

#cluster
total_data <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/final-project/adult222.csv",header = T)
cluster_data <- total_data

cluster_data[cluster_data == "?"] <- NA 
cluster_data <- na.omit(cluster_data)

mydata1 <- cluster_data[,c('age', "capital.gain" ,  "capital.loss" ,  "hours.per.week")] 
#Determine number of clusters
wssplot <- function(data1, nc=100, seed=1234){
  wss <- (nrow(data1))*sum(apply(data1,2,var))
  for (i in 2:nc) wss[i] <- sum(kmeans(data1,
                                       centers=i)$withinss)
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  }
wssplot(mydata1)
#=============================================
# K-Means Cluster Analysis
cl_data <- cluster_data 

cl_data[["educational.num"]]=NULL
cl_data[["fnlwgt"]]=NULL
cl_data[cl_data == "?"] <- NA 
cl_data <- na.omit(cl_data)

data_prepare <- cl_data
#age, workclass, race, gender, capital-gain, capital-loss, hours-per-week, 
data_prepare$gender <- factor(cl_data$gender, levels=c("Male","Female"), labels=c(1,0))
# Amer-Indian-Eskimo=1 Asian-Pac-Islander=2 Black=3  White=4 Other=5
data_prepare$race <- factor(cl_data$race, levels=c(" Amer-Indian-Eskimo","Asian-Pac-Islander",'Black','White','Other'), labels=c(1,2,3,4,5))
#Federal-gov=0 Local-gov=1 Never-worked=2 Private=3 Self-emp-inc=4 Self-emp-not-inc=5 State-gov=6 Without-pay=7
data_prepare$workclass <- factor(cl_data$workclass, levels=c('Federal-gov','Local-gov','Never-worked','Private','Self-emp-in','Self-emp-not-inc','State-gov','Without-pay'), labels=c(0,1,2,3,4,5,6,7))
data_prepare$income <- factor(cl_data$income, levels=c("<=50K",">50K"), labels=c(1,2))
data_prepare <- na.omit(data_prepare)

new_data <- data_prepare[,c('age', 'workclass', 'race', 'gender','capital.gain', 'capital.loss','hours.per.week')]
new_data <- na.omit(new_data)

my_clu_fit <- kmeans(new_data, 2)
my_clu_fit
str(my_clu_fit)
plot(data_prepare, col=my_clu_fit$cluster)


data_cluster <- kmeans(mydata1, 2)



fit <- kmeans(mydata1, 4)
autoplot(fit,mydata1, frame=TRUE)
str(fit)
plot(mydata1,col=fit$cluster,pos=4,cex=.5)

# how to choose k
k <- list()
for (i in 1:4){
  k[[i]] <- kmeans(data_Scaled, i)
}

betweenSS_totSS <- list()
for (i in 1:4){
  betweenSS_totSS[[i]] <- k[[i]]$betweenss/ k[[i]]$totss
}
betweenSS_totSS
plot(1:4, betweenSS_totSS, type='b', ylab="k[[i]]$betweenss/ k[[i]]$totss", xlab='cluster')

# hirarcical clustering:
d <- dist(mydata1)
fitH <- hclust(d, 'ward.D2')
par(cex=.2)
plot(fitH)
par(cex=1)

###
normalization_z_score <- function(x){
  return( (x-mean(x))/sd(x)   )
}

before_normalized_data <- cluster_data[,c('age', "capital.gain" ,  "capital.loss" ,  "hours.per.week")] 
after_normalized_data <- as.data.frame(lapply(normal_data  , normalization_z_score))

summary(before_normalized_data) #After Normalization

summary(after_normalized_data)


#Roc curve for final exam:
adult.cmplt <- tem_data
ratio = sample(1:nrow(adult.cmplt), size = 0.80*nrow(adult.cmplt))
training_data = adult.cmplt[ratio,] #Test dataset 20% of total
testing_data = adult.cmplt[-ratio,] #Train dataset 80% of total
#Liner-regression
regression_glm_fit<- glm(income~., family = binomial(link='logit'), data = training_data)
regression_glm_all_predictors <-  predict(regression_glm_fit , testing_data, type = "response")
predicted_income <- rep('<=50K', length(regression_glm_all_predictors))
predicted_income[regression_glm_all_predictors>=.5] <- '>50K'
#confusion matrix 
table(predicted_income, actual_income=testing_data$income)
pr1 <- prediction(regression_glm_all_predictors, testing_data$income)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
dd1 <- data.frame(FP = prf1@x.values[[1]], TP = prf1@y.values[[1]])
#Random-forest
library(randomForest)
rf.model<- randomForest(income~., data = training_data, importance=TRUE, keep.forest=TRUE)
rf.predict <- predict(rf.model, testing_data,type = 'prob')
confusionMatrix(testing_data$income, rf.predict) 
pr3 <- prediction(rf.predict[,2], testing_data$income)
prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
dd3 <- data.frame(FP = prf3@x.values[[1]], TP = prf3@y.values[[1]])
# plot:
ggplot() + 
  geom_line(data = dd, aes(x = FP, y = TP, color = 'Logistic Regression'))+
  geom_line(data = dd3, aes(x = FP, y = TP, color = 'Random Forest'))+
  scale_colour_manual(name = 'Classifier', values = c('Logistic Regression'='#E69F00', 'Random Forest'='#56B4E9'))

  






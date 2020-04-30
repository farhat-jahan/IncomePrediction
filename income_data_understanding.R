
adult_data <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/final-project/adult222.csv",header = T)
tem_data <- adult_data
tem_data[["educational.num"]]=NULL
tem_data[["fnlwgt"]]=NULL

#data cleaning
tem_data[adult == "?"] <- NA
is.na(tem_data) = data=='?'
is.na(tem_data) = tem_data==' ?'
tem_data = na.omit(tem_data)


# box plot to know outlier in age who works in old age:
outlier_age <- tem_data
summary(outlier_age$age)
ggplot(mapping = aes(x = factor(0), y = outlier_age$age),
       data = outlier_age) + 
  geom_boxplot() +
  stat_summary(fun.y = match.fun(mean),geom ='point',colour="red", size=3,
               position = position_dodge2(width = 0.75)) +
  coord_cartesian(ylim = c(10, 100)) +
  scale_y_continuous(breaks = seq(10, 100, 5)) +
  ylab("Age") +
  xlab("") +  
  ggtitle("Box Plot of Age") +
  scale_x_discrete(breaks = NULL)

#how many hours per week  people works
hr_week_data <- tem_data
ggplot(mapping=aes(x = factor(0), y = hr_week_data$hours.per.week),
       data = hr_week_data) + 
  geom_boxplot(col='#bd00ff') +
  stat_summary(fun.y = match.fun(mean), 
               geom = "point", 
               color = "red",
               shape = 19,
               cex = 2) +
  coord_cartesian(ylim = c(30, 50)) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(30, 50, 1)) +
  ylab("Hours per Week") +
  xlab("") +  
  ggtitle("Box plot of Hours per Week")

# workign people region
region <- tem_data
db.adult <- region
db.adult$native_region <- factor(db.adult$native.country, 
                                 levels = 
                                   names(sort(table(db.adult$native.country),                                                 decreasing = TRUE)))

ggplot(db.adult, 
       aes(x = db.adult$native.country, fill = db.adult$native.country)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1) +
  labs(x = "Region", 
       y = "",
       fill = "Regions") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) 

#Education higher qualification count:
db.adult <- tem_data
db.adult$education <- factor(db.adult$education, levels=names(sort(table(db.adult$education),                                                    decreasing = TRUE)))

ggplot(db.adult, 
       aes(x = db.adult$education, fill = db.adult$education)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Education", 
       y = "",
       fill = "Education") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# percentage of marotal stataus count
db.adult <- tem_data
db.adult$marital_status <- factor(db.adult$marital.status, levels = names(sort(table(db.adult$marital.status),                                               decreasing = TRUE)))

ggplot(db.adult, 
       aes(x = db.adult$marital.status, fill = db.adult$marital.status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Marital Status", 
       y = "",
       fill = "Marital Status") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))
# count as per relationship
summary(db.adult$relationship)
db.adult$relationship <- factor(db.adult$relationship, 
                                levels = 
                                  names(sort(table(db.adult$relationship),                                                 decreasing = TRUE)))

ggplot(db.adult, 
       aes(x = db.adult$relationship, fill = db.adult$relationship)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1) +
  labs(x = "Relationship", 
       y = "",
       fill = "Relationship") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))
#occupation percentage
summary(db.adult$occupation)
db.adult$occupation <- factor(db.adult$occupation, 
                              levels = 
                                names(sort(table(db.adult$occupation),                                                   decreasing = TRUE)))

ggplot(db.adult,
       aes(x = db.adult$occupation, fill = db.adult$occupation)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ),
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Occupation",
       y = "Percentage",
       fill = "Occupation") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))


is.na(data) = data=='?'
is.na(data) = data==' ?'
data = na.omit(data)


#Outliers, cap-gain cap-loss
ggplot(mapping = aes(x = income, y = capital.loss),
       data = subset(db.adult, db.adult$capital.loss > 0)) + 
  geom_boxplot(col='#77ab59') +
  stat_summary(fun.y = match.fun(mean), 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(0, 3000))+
  scale_y_continuous(breaks = seq(0, 3000, 200)) +
  labs(x = "Income", 
       y = "Capital Loss") +
  ggtitle("Box Plot of Nonzero Capital Loss by Income")

#Outliers, cap-gain cap-gain
ggplot(mapping = aes(x = income, y = capital.gain),
       data = subset(db.adult, db.adult$capital.gain > 0)) + 
  geom_boxplot(col='#4682b4') +
  stat_summary(fun.y = match.fun(mean), 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(0, 30000)) +
  scale_y_continuous(breaks = seq(0, 30000, 1500)) +
  labs(x = "Income", 
       y = "Capital Gain") +
  ggtitle("Box Plot of Nonzero Capital Gain by Income") 

#Outliers, age$
ggplot(mapping = aes(x = income, y = age),
       data = subset(db.adult, db.adult$age > 0)) + 
  geom_boxplot(col='#9F455F') +
  stat_summary(fun.y = match.fun(mean), 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(10, 100))+
  scale_y_continuous(breaks = seq(10, 100, 5)) +
  labs(x = "Income", 
       y = "Age") +
  ggtitle("Box Plot of Age by Income")

##Outliers hour-per week
ggplot(mapping = aes(x = income, y = hours.per.week),
       data = subset(db.adult, db.adult$hours.per.week>0)) + 
  geom_boxplot(col='#CABA1A') +
  stat_summary(fun.y = match.fun(mean), 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(10,60))+
  scale_y_continuous(breaks = seq(10, 60, 5)) +
  labs(x = "Income", 
       y = "Hours Per Week") +
  ggtitle("Box Plot of Hours Per Week")
### check relation with density plot to decide which will be good for prediction

# age and income are corelated
corelation_data <- tem_data
ggplot(data = corelation_data, aes(corelation_data$age, fill = corelation_data$income)) + 
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 95, 5))
# h-w-week and income are corelated
ggplot(data = corelation_data, aes(corelation_data$hours.per.week, fill = corelation_data$income)) + 
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(10, 60, 5))
# capital gain and income are corelated
ggplot(data = corelation_data, aes(corelation_data$capital.gain, fill = corelation_data$income)) + 
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 30000, 1500))
# capital loss and income are corelated
ggplot(data = corelation_data, aes(corelation_data$capital.loss, fill = corelation_data$income)) + 
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 3000, 200))


# correlation of all data:
library(psych)
pairs.panels(tem_data, pch=3)

#density plots for each attribut, to check better Outliers## Normal density graph
y <- as.factor(db.adult$income)
x <- db.adult$age
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot='density', scales=scales)
  
######## decision tree
library(rattle)

library(rpart)
library(rpart.plot)

## normalization not require
gsub("[^[:alnum:][:blank:]+?&/\\-]", "", tem_data)
normalization_z_score <- function(x){
  return( (x-mean(x))/sd(x)   )
}
normalized_data_2 <- as.data.frame(lapply( tem_data[,1], normalization_z_score))

summary(normalized_data_2)

######=======


income <- tem_data
model1<-rpart(income~., data = income)
prp(model1, type =2, extra = 4, main = "Probabilities Per Class")
asRules(model1)

library(caret)
library(lattice)
index<-createDataPartition(income$income, p=0.8, list =F)
Train<-income[index,]
Test<-income[-index,]



















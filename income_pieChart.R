adult_data <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/final-project/adult222.csv",header = T)
tem_data <- adult_data

counttable <- table(tem_data$income)

data_lebels <- c("great 50", "less 50")
data_lebels <- paste(data_lebels,"\n",counttable)
# pie chart is created based on 'dataset' attribute which is response a variable.
pie(counttable, labels = data_lebels,main = "Count of dataset attribute",
    col = rainbow(length(data_lebels)))
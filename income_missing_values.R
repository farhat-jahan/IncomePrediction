###--- BEFORE DISCRETIZATION AND DIMNTION REDUCTION FOLLOW THESE STEPS ###---

adult_data <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/final-project/adult222.csv",header = T)
tem_data <- adult_data

#Checking missing values.
missing_values <- table(is.na(tem_data))
missing_attributes_count<- sapply(tem_data, function(x) sum(is.na(x)))

#missing value graph:
missig_g1 <- vis_miss(tem_data)
missing_g2 <-  gg_miss_var(tem_data)

#checking percentage missing
percentage_missing <- pct_miss(tem_data)
percentage_complete <- pct_complete(tem_data)

#removing unwanted columns
tem_data[["educational.num"]]=NULL
tem_data[["fnlwgt"]]=NULL

#cleaning unwanted data.
tem_data[adult == "?"] <- NA
is.na(tem_data) = data=='?'
is.na(tem_data) = tem_data==' ?'
tem_data = na.omit(tem_data)

missmap(tem_data, y.at = 1, y.label = "", legend = FALSE, col = c("red", "#779c74"))


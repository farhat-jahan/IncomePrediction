adult_data <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/final-project/adult222.csv",header = T)
tem_data <- adult_data


#Discretization of some data:
barplot(table(tem_data$age), main = "Frequency of Age", col= "#BA960E", space =.9, angle=50, density = 30)
#We divide values of age to 4 levels: Young (17~25), Middle (26~45), Senior (46~65), and Old (66~90).
tem_data[["age"]] <- cut(tem_data[["age"]], c(0,25,45,65,100),labels = c("Young", "Middle", "Senior", "Old"), ordered=TRUE)
barplot(table(tem_data$age), main = "Frequency of Age", col= "#BA960E", space =.9, angle=50, density = 15)
#We divide values of hr_per_week to 4 levels: Part-time (0~25), Full-time (25~40), Over-time (40~60), and Too-much (60~168) by the following R codes:
barplot(table(tem_data$hours.per.week), main = "Frequency of hour per week",col= "red", space =.9, angle=50, density = 30);
tem_data[["hours.per.week"]] <- cut(tem_data[["hours.per.week"]], c(0,25,40,60,168), labels = c("Part-time", "Full-time", "Over-time", "Too-much"), ordered=TRUE)
#We divide values of capital_gain to 3 levels: None (0), Low (0 < median of the values greater zero < max) and High (>=max) by the following R codes:
barplot(table(tem_data$capital.gain), main = "Frequency of capital gain",col= "#0B4C5F", space =.9, angle=50, density = 30)
tem_data[["capital.gain"]] <- cut(tem_data[["capital.gain"]], c(-Inf,0, median(tem_data[["capital.gain"]][tem_data[["capital.gain"]]>0]),Inf), labels = c("None", "Low", "High"), ordered=TRUE)
#We divide values of capital_loss to 3 levels: None (0), Low (0 < median of the values greater zero < max) and High (>=max) by the following R codes:
barplot(table(tem_data$capital.loss), main = "Frequency of capital loss",col= "#FF8000", space =.9, angle=50, density = 30)
tem_data[["capital.loss"]] <- cut(tem_data[["capital.loss"]], c(-Inf,0, median(tem_data[["capital.loss"]][tem_data[["capital.loss"]]>0]),Inf), labels = c("None", "Low", "High"), ordered=TRUE)

#recheck:cleaning all inwanted data
tem_data[tem_data == "?"] <- NA 
#tem_data[tem_data == ".\"] <- NA

missing_values <- table(is.na(tem_data))
missing_attributes_count<- sapply(tem_data, function(x) sum(is.na(x)))

tem_data <- na.omit(tem_data)
data <- tem_data

missmap(data, y.at = 1, y.label = "", legend = FALSE, col = c("yellow", "#779c74"))

#The read.table() command will convert character variables to factors as default, so we firstly convert the nominal attributes that we want to modify to the character format:
data$workclass = as.character(data$workclass)
data$occupation = as.character(data$occupation)
data$native.country = as.character(data$native.country)
data$education = as.character(data$education)
data$race = as.character(data$race)
data$marital.status = as.character(data$marital.status)

#Combining some columns, which can be work together. in dim-reduction steps
table(data$workclass)
#We saw that “Never-worked” and “Without-Pay” are both very small groups, and they are likely very similar, so we will combine them to form a “Not-Working” category. The following is R codes to do the task:
data$workclass = gsub("^Without-pay","Not-Working",data$workclass)
data$workclass = gsub("^Never-worked","Not-Working", data$workclassr)
#We also combine “Local-gov” and “State-gov” groups to “Other-gov” group and “Self-emp-inc” and “Self-emp-not-inc” groups to “Self-employed” group:
data$workclass = gsub("^Local-gov","Other-gov",data$workclass)
data$workclass = gsub("^State-gov","Other-gov",data$workclass) 
data$workclass = gsub("^Self-emp-inc","Self-employed",data$workclass)
data$workclass = gsub("^Self-emp-not-inc","Self-employed", data$workclass)
#The result of the reducing number of levels of “type_employer” attribute is below:
table(data$workclass)

table(data$education)
#We combine “Preschool”, “1st-4th”, “5th-6th”, “7th-8th”, “9th”, “10th”, “11th” and 
#“12th” groups to “Dropout” group, “Assoc-acdm” and “Assoc-voc” groups to “Associates” group, 
#“HS-grad” and “Some-college” groups to “HS-Graduate” group
data$education = gsub("^Preschool","Dropout",data$education)
data$education = gsub("^1st-4th","Dropout",data$education)
data$education = gsub("^5th-6th","Dropout",data$education)
data$education = gsub("^7th-8th","Dropout",data$education)
data$education = gsub("^9th","Dropout",data$education)
data$education = gsub("^10th","Dropout",data$education)
data$education = gsub("^11th","Dropout",data$education)
data$education = gsub("^12th","Dropout",data$education)
data$education = gsub("^Assoc-acdm","Associates",data$education)
data$education = gsub("^Assoc-voc","Associates",data$education)
data$education = gsub("^Bachelors","Bachelors",data$education)
data$education = gsub("^HS-grad","HS-graduate",data$education)
data$education = gsub("^Some-college","Colleges",data$education)
table(data$education)

table(data$marital.status)
#We combine two groups “Married-AF-spouse” and “Married-civ-spouse” to group “Married” and 
#three groups “Married-spouse-absent”,
#“Separated” and “Divorced” to group “Not-married”:
data$marital.status = gsub("^Married-AF-spouse","Married",data$marital.status)
data$marital.status = gsub("^Married-civ-spouse","Married",data$marital.status)
data$marital.status = gsub("^Married-spouse-absent","Not-married",data$marital.status)
data$marital.status = gsub("^Separated","Not-married",data$marital.status)
data$marital.status = gsub("^Divorced","Not-married",data$marital.status)
table(data$marital.status)

table(data$occupation)
#We reduce the number of levels of the attributes as below:
data$occupation = gsub("^Adm-clerical","Admin",data$occupation)
data$occupation = gsub("^Armed-Forces","Military",data$occupation)
data$occupation = gsub("^Craft-repair","Blue-Collar",data$occupation)
data$occupation = gsub("^Exec-managerial","White-Collar", data$occupation)
data$occupation = gsub("^Farming-fishing","Blue-Collar", data$occupation)
data$occupation = gsub("^Handlers-cleaners","Blue-Collar", data$occupation)
data$occupation = gsub("^Machine-op-inspct","Blue-Collar", data$occupation)
data$occupation = gsub("^Other-service","Service",data$occupation)
data$occupation = gsub("^Priv-house-serv","Service",data$occupation)
data$occupation = gsub("^Prof-specialty","Professional", data$occupation)
data$occupation = gsub("^Protective-serv","Other-Occupations", data$occupation)
data$occupation = gsub("^Sales","Sales",data$occupation)
data$occupation = gsub("^Tech-support","Other-Occupations", data$occupation)
data$occupation = gsub("^Transport-moving","Blue-Collar", data$occupation)
table(data$occupation)


table(data$native.country)
#The result of the reducing number of levels of “native.country” attribute is below:
data$native.country[data$native.country=="Cambodia"] = "SE-Asia"
data$native.country[data$native.country=="Canada"] = "British-Commonwealth"   
data$native.country[data$native.country=="China"] = "China"      
data$native.country[data$native.country=="Columbia"] = "South-America"   
data$native.country[data$native.country=="Cuba"] = "Other"       
data$native.country[data$native.country=="Dominican-Republic"] = "Latin-America"
data$native.country[data$native.country=="Ecuador"] = "South-America"    
data$native.country[data$native.country=="El-Salvador"] = "South-America"
data$native.country[data$native.country=="England"] = "British-Commonwealth"
data$native.country[data$native.country=="France"] = "Euro_1"
data$native.country[data$native.country=="Germany"] = "Euro_1"
data$native.country[data$native.country=="Greece"] = "Euro_2"
data$native.country[data$native.country=="Guatemala"] = "Latin-America"
data$native.country[data$native.country=="Haiti"] = "Latin-America"
data$native.country[data$native.country=="Holand-Netherlands"] = "Euro_1"
data$native.country[data$native.country=="Honduras"] = "Latin-America"
data$native.country[data$native.country=="Hong"] = "China"
data$native.country[data$native.country=="Hungary"] = "Euro_2"
data$native.country[data$native.country=="India"] = "British-Commonwealth"
data$native.country[data$native.country=="Iran"] = "Other"
data$native.country[data$native.country=="Ireland"] = "British-Commonwealth"
data$native.country[data$native.country=="Italy"] = "Euro_1"
data$native.country[data$native.country=="Jamaica"] = "Latin-America"
data$native.country[data$native.country=="Japan"] = "Other"
data$native.country[data$native.country=="Laos"] = "SE-Asia"
data$native.country[data$native.country=="Mexico"] = "Latin-America"
data$native.country[data$native.country=="Nicaragua"] = "Latin-America"
data$native.country[data$native.country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
data$native.country[data$native.country=="Peru"] = "South-America"
data$native.country[data$native.country=="Philippines"] = "SE-Asia"
data$native.country[data$native.country=="Poland"] = "Euro_2"
data$native.country[data$native.country=="Portugal"] = "Euro_2"
data$native.country[data$native.country=="Puerto-Rico"] = "Latin-America"
data$native.country[data$native.country=="Scotland"] = "British-Commonwealth"
data$native.country[data$native.country=="South"] = "Euro_2"
data$native.country[data$native.country=="Taiwan"] = "China"
data$native.country[data$native.country=="Thailand"] = "SE-Asia"
data$native.country[data$native.country=="Trinadad&Tobago"] = "Latin-America"
data$native.country[data$native.country=="United-States"] = "United-States"
data$native.country[data$native.country=="Vietnam"] = "SE-Asia"
data$native.country[data$native.country=="Yugoslavia"] = "Euro_2"
table(data$native.country)
#Convert attributes back to factors, with attribute “income”, the data is converted to 1 (<=50K) and 2 (>50K):
data$marital.status = factor(data$marital.status)
data$education = factor(data$education)
data$native.country = factor(data$native.country)
data$workclass = factor(data$workclass)
data$occupation = factor(data$occupation)
data$race = factor(data$race)
data$gender = factor(data$gender)
data$relationship = factor(data$relationship)
data$income = as.factor(ifelse(data$income==data$income[1],1,2))

tem_data <- data
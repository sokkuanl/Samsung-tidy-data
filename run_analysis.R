#Read training data
features <- read.table("features.txt")
activitylabels <- read.table("activity_labels.txt")
subject_train <- read.table("./train/subject_train.txt")
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")

#Assign column names to training data 
colnames(activitylabels) <- c("activityId", "activityType")
colnames(subject_train) <- "subjectId"
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityId"

#Create training data set 
data_train <- cbind (subject_train, x_train, y_train)

#Read test data 
subject_test <- read.table("./test/subject_test.txt")
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")

#Assign column names to test data 
colnames(subject_test) <- "subjectId"
colnames(x_test) <- features[,2]
colnames(y_test) <- "activityId"

#Create test data set 
data_test <- cbind (subject_test, x_test, y_test)

#Merge training and test data set
finaldata <- rbind(data_train, data_test)

#Create a vector for the column names of finaldata, to select the mean and SD columns
colNames <- colnames(finaldata)

#2. Extract only the measurements on the mean and standard deviation for each measurement
mean_SD <- (grepl("activityId", colNames) | grepl("subjectId", colNames) | grepl("-mean..", colNames) & !grepl("-meanFreq..", colNames) & !grepl("mean..-", colNames) | grepl("-std..", colNames) & !grepl("-std()..-", colNames))

#Subset finaldata table based on the mean_SD vector 
finaldata <- finaldata[mean_SD == TRUE]

# 3. Use descriptive activity names to name the activities in the data set
# Merge the finaldata set with the acitivitylabels table to include descriptive activity names

finaldata <- merge(finaldata, activitylabels, by = "activityId", all.x = TRUE)

#Update the colNames vector to include the new column names after merge 
colNames <- colnames(finaldata)

# 4. Appropriately label the data set with descriptive activity names.
# Cleaning up the variable names

for(i in 1:length(colNames)){
  colNames[i] = gsub("\\()", "", colNames[i]) 
  colNames[i] = gsub("^(t)", "time", colNames[i]) 
  colNames[i] = gsub("^(f)", "freq", colNames[i]) 
}

# Reassigning the new descriptive column names to the finaldata set
colnames(finaldata) <- colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
#Create a new table without activityType column

finaldata_noactivity <- select(finaldata, -activityType)

# Summarize the finaldata_noactivity table to include just the mean of each variable for each activity and each subject
tidydata <- aggregate(finaldata_noactivity[,names(finaldata_noactivity) != c('activityId','subjectId')], by = list (activityId = finaldata_noactivity$activityId,subjectId = finaldata_noactivity$subjectId),mean)

# Merge the tidydata with activity type to include descriptive acitvity names
tidydata <- merge(tidydata, activityType, by ='activityId', all.x=TRUE)

# Export the tidydata set 
write.table(tidydata, './tidydata.txt',row.names=FALSE,sep='\t')



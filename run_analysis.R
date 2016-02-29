#############################################################################################################
#############################################################################################################
###
###  Getting and cleaning data - Course project
###  
###  Owner: Elisenda Vila Jofre
###
#############################################################################################################
#############################################################################################################

#Steps:
#1:Merges the training and the test sets to create one data set.
#2:Extracts only the measurements on the mean and standard deviation for each measurement.
#3:Uses descriptive activity names to name the activities in the data set
#4:Appropriately labels the data set with descriptive variable names.
#5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject


library(data.table)
######################
### Download data
######################

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = url, destfile = "Data.zip", method = "curl")
unzip("Data.zip")

#######################
### Read data
#######################

### These are the column names
labels<-read.table("./UCI HAR Dataset/activity_labels.txt",col.names = c("activity","activity_name"))
features<-read.table("./UCI HAR Dataset/features.txt",col.names = c("pos","varname"))


test_users<-read.table("./UCI HAR Dataset/test/subject_test.txt",col.names = c("subject_num"))
test_xdata<-read.table("./UCI HAR Dataset/test/X_test.txt",col.names = features$varname )
## the names are not copied properly
colnames(test_xdata)<-features$varname
test_ydata<-read.table("./UCI HAR Dataset/test/y_test.txt",col.names = c("activity"))

train_users<-read.table("./UCI HAR Dataset/train/subject_train.txt",col.names = c("subject_num"))
train_xdata<-read.table("./UCI HAR Dataset/train/X_train.txt",col.names = features$varname )
## the names are not copied properly
colnames(train_xdata)<-features$varname
train_ydata<-read.table("./UCI HAR Dataset/train/y_train.txt",col.names = c("activity"))

### Merge training and test data sets
data_all<-rbind(test_xdata,train_xdata)


### Keep only the measurements on the mean and standard deviation for each measurement.
features_to_extract <- grep("mean|std", features$varname)
data_meansd<- data_all[, features_to_extract]

### Use the descriptive activity names to name the activities in the data set

recoded<-""
for(i in 1:nrow(labels)){
  recoded<-paste0(recoded,"'",labels[i,1],"' = '",labels[i,2],"'")
  if(i < nrow(labels) ){
    recoded<- paste(recoded,";")
  }
  
}
library(car)

test_ydata$activity<-recode(test_ydata$activity,recoded,as.factor.result=TRUE)
train_ydata$activity<-recode(train_ydata$activity,recoded,as.factor.result=TRUE)

### Merge all data
final_data<-cbind(data_meansd, rbind(test_ydata,train_ydata),rbind(test_users,train_users))

### Create a second, independent tidy data set with the average of each variable for each activity and each subject
DT <- data.table(final_data)
tidy<-DT[,lapply(.SD,mean),by="activity,subject_num"]


### Write the tidy data in a file
write.table(tidy, file = "./tidy_data.txt", row.names=FALSE)
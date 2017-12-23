#Create a R Script that does the following:
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Readig the data into variables

y_train <- read.table("train/y_train.txt", header= FALSE)
y_test <- read.table("test/y_test.txt", header=FALSE)

features <- read.table("features.txt", header = FALSE)
activity_labels <- read.table("activity_labels.txt", header=FALSE)


subject_train <- read.table("train/subject_train.txt", header=FALSE)
subject_test <- read.table("test/subject_test.txt", header = FALSE)

X_train <- read.table("train/X_train.txt", header=FALSE)
X_test <- read.table("test/X_test.txt", header = FALSE)

#Merge the data row wise

dsubj <- rbind(subject_train, subject_test)
dact<- rbind(y_train, y_test)

dact[, 1] <- activity_labels[dact[, 1], 2]

names(dact) <- "activity"

dfeat<- rbind(X_train, X_test)

#name variables

names(dsubj) <- c("subject")
names(dact) <- c("activity")
names(dfeat) <- features$V2

#Merge tje columns to get the consolidated data

tempData <- cbind(dsubj,dact)

data <- cbind(tempData,dfeat)

#Extract only the measurements on the mean and std.dev

dataFeaturesMeanStd <- features$V2[grep("mean\\(\\)|std\\(\\)",features$V2)] #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

selectedNames <- c("subject","activity", as.character(dataFeaturesMeanStd))
data<- subset(data,select=selectedNames) 

#Use Descriptive Activity Name

names(data) <- gsub("^t","time",names(data))
names(data) <- gsub("^f","frequency",names(data))
names(data) <- gsub("Acc","Accelerometer",names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))

#Create tidy data set

Data2<-aggregate(. ~subject + activity, data, mean)
Data2<-Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)














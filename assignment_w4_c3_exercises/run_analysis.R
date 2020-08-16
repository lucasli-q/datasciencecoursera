##downloading zip file as a temporary file
temp<-tempfile(fileext=".zip")
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,temp)

##visualizing files' names
unzip(temp,list=T)

##downloading files of interest
features<-read.table(unz(temp,"UCI HAR Dataset/features.txt"), header=F)
features<-as.character(features[,2])
activitylabels<-read.table(unz(temp,"UCI HAR Dataset/activity_labels.txt"), header=F)
activitylabels<-as.character(activitylabels[,2])
trainX<-read.table(unz(temp,"UCI HAR Dataset/train/X_train.txt"), header=F)
trainY<-read.table(unz(temp,"UCI HAR Dataset/train/y_train.txt"), header=F)
trainsubject<-read.table(unz(temp,"UCI HAR Dataset/train/subject_train.txt"), header=F)
testX<-read.table(unz(temp,"UCI HAR Dataset/test/X_test.txt"), header=F)
testY<-read.table(unz(temp,"UCI HAR Dataset/test/y_test.txt"),header=F)
testsubject<-read.table(unz(temp,"UCI HAR Dataset/test/subject_test.txt"),header=F)

##removing the temp file
unlink(temp)

##exploring how data is sorted
str(features)
str(activitylabels)
str(trainX)
str(trainY)
str(trainsubject)
str(testX)
str(testY)
str(testsubject)

##joining train data into one df
train<-data.frame(trainsubject, trainY, trainX)
##joining test data into one df
test<-data.frame(testsubject,testY,testX)

##renaming variables using features vector
names(train)<-c(c("subject","activity"),features)
head(names(train))
names(test)<-c(c("subject","activity"),features)
head(names(test))

##merging "train" and "test" dfs
fulldata<-rbind(train,test)

##selecting only mean and std variables
head(colnames(fulldata))
meanstddata<-fulldata[,which(colnames(fulldata) %in% c("subject","activity",grep("mean()|std()",colnames(fulldata),value=TRUE)))]

##substituting numbers of activities by their respective labels
meanstddata[,"activity"]<-gsub(1, activitylabels[1],meanstddata[,"activity"])
meanstddata[,"activity"]<-gsub(2, activitylabels[2],meanstddata[,"activity"])
meanstddata[,"activity"]<-gsub(3, activitylabels[3],meanstddata[,"activity"])
meanstddata[,"activity"]<-gsub(4, activitylabels[4],meanstddata[,"activity"])
meanstddata[,"activity"]<-gsub(5, activitylabels[5],meanstddata[,"activity"])
meanstddata[,"activity"]<-gsub(6, activitylabels[6],meanstddata[,"activity"])

##checking if the activity numbers were substituted by activitylabels
meanstddata[,"activity"]<-as.factor(meanstddata[,"activity"])
str(meanstddata[,"activity"])
summary(meanstddata[,"activity"])

##giving variables with descriptive names
names(meanstddata)
names(meanstddata)<-gsub("^t", "time", names(meanstddata))
names(meanstddata)<-gsub("^f", "frequency", names(meanstddata))
names(meanstddata)<-gsub("Acc", "accelerometer", names(meanstddata))
names(meanstddata)<-gsub("Gyro", "gyroscope", names(meanstddata))
names(meanstddata)<-gsub("Mag", "magnitude", names(meanstddata))
names(meanstddata)<-gsub("BodyBody", "body", names(meanstddata))

##aggregating by subject and activity and calculating means
tidydata<-aggregate(.~subject+activity, meanstddata,mean)
head(tidydata)
str(tidydata)

##exporting data
write.table(tidydata,"tidydata.txt",row.names=FALSE)

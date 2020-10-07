#Parts in this script are not in the same order as the assignment text.
#This order is only the way I organized the tasks required to complete the requirements of the assignment.

#Part1: Data load
testmaindata<-read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
trainmaindata<-read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
subjecttest<-read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
subjecttrain<-read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
testactivities<-read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
trainactivities<-read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
activitylabels<-read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
featureslabels<-read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")

#Part2: Descriptive variable names setting: lower cases; no dots, underscores or white spaces (I remove other special characters as well); descriptive variables; character variables to factor variables.                    
                         
mainvariablesnames<-featureslabels[,2]  
mainvariablesnames<-tolower(mainvariablesnames)                            #to lower cases
mainvariablesnames<-sub("^f","frequency",mainvariablesnames)               #Descriptive names: t's to time
mainvariablesnames<-sub("^t", "time",mainvariablesnames)                                     # and f's to frequency
mainvariablesnames<-gsub("-","", mainvariablesnames)                       #removing special characters                
mainvariablesnames<-gsub("\\(","", mainvariablesnames)                     #removing special characters
mainvariablesnames<-gsub("\\)","", mainvariablesnames)                     #removing special characters
mainvariablesnames<-gsub(",","", mainvariablesnames)                       ##removing special characters
names(testmaindata)<-mainvariablesnames
names(trainmaindata)<-mainvariablesnames
names(subjecttest)<-c("subjectid")
names(subjecttrain)<-c("subjectid")
names(activitylabels)<-c("code","activity")
#there was no reason to run code to remove spaces, dots or underscores from variable names since those characters are not present
#character variables will be change to factor variables in Part 4 for convenience

#Part3: Merging Train and Test data sets
testactivitieslabeled<-merge(testactivities, activitylabels,by.x = 1, by.y = 1)
trainactivitieslabeled<-merge(trainactivities,activitylabels,by.x = 1, by.y = 1)
test<-cbind(testmaindata,subjecttest,testactivitieslabeled[2])
train<-cbind(trainmaindata,subjecttrain, trainactivitieslabeled[2])
mergeddata<- rbind(test,train)

#Part 4: Extracting only the measurements on the mean and standard deviation for each measurement
meancols<-names(mergeddata)[grep("mean", names(mergeddata))]
stdcols<-names(mergeddata)[grep("std", names(mergeddata))]
colstoselect<-c(meancols,stdcols, "subjectid", "activity")
extracteddata<-mergeddata[,colstoselect]
extracteddata$subjectid<-as.factor(extracteddata$subjectid)               #character variables to factor variables
extracteddata$activity<-as.factor(extracteddata$activity)                 #character variables to factor variables
str(extracteddata)
View(extracteddata)

#Part5: create a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
step5dataset<-aggregate(select(extracteddata, -(subjectid:activity)), by=list(subjectid=extracteddata$subjectid,activity=extracteddata$activity), mean)
step5dataset<-arrange(seconddataset, subjectid, activity)
View(step5dataset)

#Part 6: Data export as .txt file
write.table(step5dataset, file = "./step5.txt", row.names = FALSE)


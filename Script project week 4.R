############################################
### Getting and Cleanning Data Couse Project
###   Autor: Manuel Terán Melgarejo
###   Date:  8 mMay 2016
############################################

library(dplyr)

###########################################
## Load files
###########################################
activity<-read.table("activity_labels.txt")
features<-read.table("features.txt")
subject.test<-read.table("subject_test.txt")
x.test<-read.table("X_test.txt")
y.test<-read.table("y_test.txt")
subject.train<-read.table("subject_train.txt")
x.train<-read.table("X_train.txt")
y.train<-read.table("y_train.txt")


###########################################
## Transform test files
###########################################
names(x.test)<-features$V2

## Assign labels
x.test.1<-cbind(x.test,Act.Cve = y.test$V1)
x.test.1<-cbind(x.test.1,sub.cve = subject.test$V1)
x.test.1<-left_join(x.test.1,activity, by = c("Act.Cve" = "V1"))
x.test.1<-cbind(x.test.1,type = "test")
x.test.1<-x.test.1[,c(565,563:564,1:561)]
colnames(x.test.1)[2:3] <- c("subject","activity")

##Select mean and std variables
ww.test<-grep("mean()",features$V2)
ww.test<-ww.test+3
xx.test<-grep("std()",features$V2)
xx.test<-xx.test+3
zz.test<-c(ww.test,xx.test)

## cerate test file (x.test.out) with mean and std variables and lables for descriptive variables
x.test.out <- x.test.1[,c(1:3,sort(zz.test))]


###########################################
## Transform train files
###########################################
names(x.train)<-features$V2

x.train.1<-cbind(x.train,Act.Cve = y.train$V1)
x.train.1<-cbind(x.train.1,sub.cve = subject.train$V1)
x.train.1<-left_join(x.train.1,activity, by = c("Act.Cve" = "V1"))
x.train.1<-cbind(x.train.1,type = "train")
x.train.1<-x.train.1[,c(565,563:564,1:561)]
colnames(x.train.1)[2:3] <- c("subject","activity")

##Select mean and std variables
ww.train<-grep("mean()",features$V2)
ww.train<-ww.train+3
xx.train<-grep("std()",features$V2)
xx.train<-xx.train+3
zz.train<-c(ww.train,xx.train)

## cerate test file (x.test.out) with mean and std variables and lables for descriptive variables
x.train.out <- x.train.1[,c(1:3,sort(zz.train))]


###############################################################
## Combines test file (x.test.out) and train file (x,train.out)
###############################################################
x.out<-rbind(x.test.out,x.train.out)


##############################################################
## Data set group by activity and subject
##############################################################

group<-group_by(x.out,subject,activity)
group.summarize<-summarise(group,
                           `tBodyAcc-mean()-X` = mean(`tBodyAcc-mean()-X`),
                           `tBodyAcc-mean()-Y` = mean(`tBodyAcc-mean()-Y`),
                           `tBodyAcc-mean()-Z` = mean(`tBodyAcc-mean()-Z`),
                           `tBodyAcc-std()-X` = mean (`tBodyAcc-std()-X`),
                           `tBodyAcc-std()-Y` = mean (`tBodyAcc-std()-Y`),
                           `tBodyAcc-std()-Z`= mean (`tBodyAcc-std()-Z`),
                           `tGravityAcc-mean()-X` = mean (`tGravityAcc-mean()-X`),
                           `tGravityAcc-mean()-Y` = mean (`tGravityAcc-mean()-Y`),
                           `tGravityAcc-mean()-Z` = mean (`tGravityAcc-mean()-Z`),
                           `tGravityAcc-std()-X`= mean (`tGravityAcc-std()-X`),
                           `tGravityAcc-std()-Y`= mean (`tGravityAcc-std()-Y`),
                           `tGravityAcc-std()-Z`= mean (`tGravityAcc-std()-Z`),
                           `tBodyAccJerk-mean()-X`= mean (`tBodyAccJerk-mean()-X`),
                           `tBodyAccJerk-mean()-Y`= mean (`tBodyAccJerk-mean()-Y`),
                           `tBodyAccJerk-mean()-Z`= mean (`tBodyAccJerk-mean()-Z`),
                           `tBodyAccJerk-std()-X`= mean (`tBodyAccJerk-std()-X`),
                           `tBodyAccJerk-sdt()-Y`= mean (`tBodyAccJerk-std()-Y`),
                           `tBodyAccJerk-sdt()-Z`= mean (`tBodyAccJerk-std()-Z`),
                           `tBodyGyro-mean()-X` =mean (`tBodyGyro-mean()-X`),
                           `tBodyGyro-mean()-Y` =mean (`tBodyGyro-mean()-Y`),
                           `tBodyGyro-mean()-Z`= mean (`tBodyGyro-mean()-Z`),
                           `tBodyGyro-std()-X` =mean (`tBodyGyro-std()-X`),
                           `tBodyGyro-std()-Y` =mean (`tBodyGyro-std()-Y`),
                           `tBodyGyro-std()-Z`= mean (`tBodyGyro-std()-Z`),
                           `tBodyGyroJerk-mean()-X` = mean (`tBodyGyroJerk-mean()-X`),
                           `tBodyGyroJerk-mean()-Y` = mean (`tBodyGyroJerk-mean()-Y`),
                           `tBodyGyroJerk-mean()-Z` = mean (`tBodyGyroJerk-mean()-X`),
                           `tBodyGyroJerk-std()-X` = mean (`tBodyGyroJerk-std()-X`),
                           `tBodyGyroJerk-sdt()-Y` = mean (`tBodyGyroJerk-std()-Y`),
                           `tBodyGyroJerk-std()-Z` = mean (`tBodyGyroJerk-std()-X`),
                           `tBodyAccMag-mean()`= mean (`tBodyAccMag-mean()`),
                           `tBodyAccMag-std()`= mean (`tBodyAccMag-std()`),
                           `tGravityAccMag-mean()` = mean(`tGravityAccMag-mean()`),
                           `tGravityAccMag-std()`= mean(`tGravityAccMag-std()`),
                           `tBodyAccJerkMag-mean()`= mean(`tBodyAccJerkMag-mean()`),
                           `tBodyAccJerkMag-std()`= mean(`tBodyAccJerkMag-std()`),
                           `tBodyGyroMag-mean()` = mean (`tBodyGyroMag-mean()`),
                           `tBodyGyroMag-std()` = mean (`tBodyGyroMag-std()`),
                           `tBodyGyroJerkMag-mean()`= mean (`tBodyGyroJerkMag-mean()`),
                           `tBodyGyroJerkMag-std()`= mean (`tBodyGyroJerkMag-std()`),
                           `fBodyAcc-mean()-X`=mean(`fBodyAcc-mean()-X`),
                           `fBodyAcc-mean()-Y`=mean(`fBodyAcc-mean()-Y`),
                           `fBodyAcc-mean()-Z`=mean(`fBodyAcc-mean()-Z`),
                           `fBodyAcc-std()-X`=mean(`fBodyAcc-std()-X`),
                           `fBodyAcc-std()-Y`=mean(`fBodyAcc-std()-Y`),
                           `fBodyAcc-std()-Z`=mean(`fBodyAcc-std()-Z`),
                           `fBodyAcc-meanFreq()-X`= mean (`fBodyAcc-meanFreq()-X`),
                           `fBodyAcc-meanFreq()-Y`= mean (`fBodyAcc-meanFreq()-Y`),
                           `fBodyAcc-meanFreq()-Z`= mean (`fBodyAcc-meanFreq()-Z`),
                           `fBodyAccJerk-mean()-X`= mean (`fBodyAccJerk-mean()-X`),
                           `fBodyAccJerk-mean()-Y`= mean (`fBodyAccJerk-mean()-Y`),
                           `fBodyAccJerk-mean()-Z`= mean (`fBodyAccJerk-mean()-Z`),
                           `fBodyAccJerk-std()-X`= mean (`fBodyAccJerk-std()-X`),
                           `fBodyAccJerk-std()-Y`= mean (`fBodyAccJerk-std()-Y`),
                           `fBodyAccJerk-std()-Z`= mean (`fBodyAccJerk-std()-Z`),
                           `fBodyAccJerk-meanFreq()-X`= mean (`fBodyAccJerk-meanFreq()-X`),
                           `fBodyAccJerk-meanFreq()-Y`= mean (`fBodyAccJerk-meanFreq()-Y`),
                           `fBodyAccJerk-meanFreq()-Z`= mean (`fBodyAccJerk-meanFreq()-Z`),
                           `fBodyGyro-mean()-X`= mean (`fBodyGyro-mean()-X`),
                           `fBodyGyro-mean()-Y`= mean (`fBodyGyro-mean()-Y`),
                           `fBodyGyro-mean()-Z`= mean (`fBodyGyro-mean()-Z`),
                           `fBodyGyro-std()-X`= mean (`fBodyGyro-std()-X`),
                           `fBodyGyro-std()-Y`= mean (`fBodyGyro-std()-Y`),
                           `fBodyGyro-std()-Z`= mean (`fBodyGyro-std()-Z`),
                           `fBodyGyro-meanFreq()-X`= mean (`fBodyGyro-meanFreq()-X`),
                           `fBodyGyro-meanFreq()-Y`= mean (`fBodyGyro-meanFreq()-Y`),
                           `fBodyGyro-meanFreq()-Z`= mean (`fBodyGyro-meanFreq()-Z`),
                           `fBodyAccMag-mean()`=mean(`fBodyAccMag-mean()`),
                           `fBodyAccMag-std()`= mean(`fBodyAccMag-std()`),
                           `fBodyAccMag-meanFreq()`= mean (`fBodyAccMag-meanFreq()`),
                           `fBodyBodyAccJerkMag-mean()`= mean (`fBodyBodyAccJerkMag-mean()`),
                           `fBodyBodyAccJerkMag-std()`= mean (`fBodyBodyAccJerkMag-std()`),
                           `fBodyBodyAccJerkMag-meanFreq()`= mean (`fBodyBodyAccJerkMag-meanFreq()`),
                           `fBodyBodyGyroMag-mean()`= mean (`fBodyBodyGyroMag-mean()`),
                           `fBodyBodyGyroMag-std()`= mean (`fBodyBodyGyroMag-std()`),
                           `fBodyBodyGyroMag-meanFreq()`= mean (`fBodyBodyGyroMag-meanFreq()`),
                           `fBodyBodyGyroJerkMag-mean()`= mean (`fBodyBodyGyroJerkMag-mean()`),
                           `fBodyBodyGyroJerkMag-std()`= mean (`fBodyBodyGyroJerkMag-std()`),
                           `fBodyBodyGyroJerkMag-meanFreq()`= mean (`fBodyBodyGyroJerkMag-meanFreq()`)
                           )

#####################################################################
## writel tables to scv file (meredataset.csv and groupsummarize.csv)
#####################################################################

write.table(x.out,"./mergedataset.csv")

write.table(group.summarize,"./groupsummarize")



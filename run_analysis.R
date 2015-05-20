
setwd("C:/Users/andrew/Documents/Gettingcleaningdata_coursera/project_data/UCI HAR Dataset")

###submit code after here:
X_train<-read.table("X_train.txt")
y_train<-read.table("y_train.txt")
X_test<-read.table("X_test.txt")
y_test<-read.table("y_test.txt")
subject_test<-read.table("subject_test.txt")
subject_train<-read.table("subject_train.txt")
feature_labs<-read.table("features.txt")
activity_labels<-read.table("activity_labels.txt")


####################1. merges the test and training datasets###########################

#check that feature_labs match test & training column names

feature_labs$V3<-paste0("V",feature_labs$V1)

sum(!(names(X_train)==feature_labs$V3))
sum(!(names(X_test)==feature_labs$V3))

#change column names to feature names

names(X_train)<-feature_labs$V2
names(X_test)<-feature_labs$V2


X_train2<-cbind(activitycode=y_train$V1,X_train)
X_train2<-cbind(subject=subject_train$V1,X_train2)
X_test2<-cbind(activitycode=y_test$V1,X_test)
X_test2<-cbind(subject=subject_test$V1,X_test2)

#test that column names are equivalent
sum(!(names(X_train2)==names(X_test2)))

#merge test and training sets
complete<-rbind(X_test2,X_train2)

############2. Extract only mean and standard deviation on measurements###############
#Note: featuresinfo.txt explains that each signal had estimated mean() and std().
#I did not extract the meanFreq() or the angle() means

#subset data that only contains mean() or sd()
complete2<-complete[,grepl("mean\\(\\)|std\\(\\)",names(complete))]

#add subject and activity labels back onto complete2
complete2<-cbind(complete[,1:2],complete2)


#####################3. Name the activities with descriptive names##########################################

#clean up labels
activity_labels$V2<-gsub("_","",activity_labels$V2)
activity_labels$V2<-tolower(activity_labels$V2)
names(activity_labels)<-c("activitycode","activity")

#merge activity_labels and complete2 data frame to add column with activity names
complete3<-merge(activity_labels,complete2,all=TRUE)


####################4. Label dataset with descriptive data names#####################################

#Clean up column names
new_names<-gsub("-","_",names(complete3))
new_names<-gsub("\\(|\\)","",new_names)
names(complete3)<-new_names


#########################5. Create a second tidy data set with averages####################

#test for NA's
sum(is.na(complete3))

#calculate subject by activity averages using aggregate
project.data<-aggregate(complete3[,-c(1:3)],list(subject=complete3$subject,activity=complete3$activity),FUN=mean)

#write new table of means to a text file
write.table(project.data,file="project.data.txt",row.name=FALSE)

#Upload text file into R and view
data <- read.table("project.data.txt", header = TRUE)
View(data)



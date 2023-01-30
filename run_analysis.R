setwd("C:/Users/imrel/OneDrive/Documents/02_MUNKA/01_Coursera/01_FundationR/03_CleaningData/04_Week4/UCI HAR Dataset")

#load necessary packages
library(dplyr)

#------------------------------------------------------------
# 1. Merges the training and the test sets to create one data set.
#------------------------------------------------------------


#reading training data sets
        #it assumes that working directory is already set to the folder of "UCI HAR Dataset"
X_train<-read.table("./train/X_train.txt")
y_train<-read.table("./train/y_train.txt")
subject_train<-read.table("./train/subject_train.txt")
#reading test data sets
#it assumes that working directory is already set to the folder of "UCI HAR Dataset"

X_test<-read.table("./test/X_test.txt")
y_test<-read.table("./test/y_test.txt")
subject_test<-read.table("./test/subject_test.txt")

#merging training and test data sets

testdata<-cbind(subject_test,y_test,X_test)
#adding status flag in order to indicate that these are coming from test
testdata$status<-"test"


traindata<-cbind(subject_train,y_train,X_train)
#adding status flag in order to indicate that these are coming from train
traindata$status<-"train"

#rbind test and train data sets
merged_traintest_data<-rbind(testdata,traindata)

colnames(merged_traintest_data)[1]<-"Subject_ID"
colnames(merged_traintest_data)[2]<-"Activity_Label"
#------------------------------------------------------------
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#------------------------------------------------------------
#read features table
features<-read.table("./features.txt")

#add 2 rows to the beginning and 1 row to the end in order to the have same dim as the one of train and test data sets
features<-add_row(features, V1=0, V2="id1", .before=1)
features<-add_row(features, V1=-1, V2="id2", .before=1)
features<-add_row(features, V1=564,V2="Status")

#find all the mean and standard deviation variables in features table (mean() and std())
targetcol<-grep("\\bmean()\\b|\\bstd()\\b", features$V2)
targetcol_name <-features$V2[grep("\\bmean()\\b|\\bstd()\\b", features$V2)]
#extract relevant columns
subdata<-select(merged_traintest_data, 1:2, targetcol)
#------------------------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set
#------------------------------------------------------------
#read activity table
activity<-read.table("./activity_labels.txt")
#replace activity label code with descriptive names
subdata$Activity_Label<-sapply(subdata$Activity_Label, function(x) activity$V2[match(x,activity$V1)])

#------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names. 
#------------------------------------------------------------
#replace V&number colnames with variable names based on feature
colnames(subdata)[3:68]<-targetcol_name

#replace t with time
colnames(subdata)[3:68]<-sub("^t","time",colnames(subdata)[3:68])
#replace f with frequency
colnames(subdata)[3:68]<-sub("^f","frequency",colnames(subdata)[3:68])

#------------------------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#------------------------------------------------------------
#create tidy data for variable means grouped by subject id and activity label
tidy_mean_data<-group_by(subdata,Subject_ID,Activity_Label) %>% summarise(across(everything(),list(mean)))

#export result table
write.table(tidy_mean_data,"tidy_data.txt",row.names = FALSE)

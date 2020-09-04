# checking for the working Directory
getwd()

# creating folder name and assgned to the argument filename

filename <- "Week_4_Assignment"               

# using the below function downloading the zip file to the directory
if(!file.exists(filename)){
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  quiz_week_4 <- "/Week_4_Assignment/"
  download.file(fileurl,destfile = "quiz_week_4.zip")
}
# unzipping the file 
file_name_1 <- "quiz_week_4.zip"
if(!file.exists(filename)){
  unzip(file_name_1)
}

# importing the dplyr library for Data Manipulation

library(dplyr)

# importing activity and features datasets and stored in the variabes

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",  col.names = c("n" , "activity_name"))
features <- read.table("./UCI HAR Dataset/features.txt",  col.names = c("s.no" , "features_name"))


# importing test data and assigning to the variables

subject_test  <- read.table("./UCI HAR Dataset/test/subject_test.txt",  col.names = c( "subject"))
x_test  <- read.table("./UCI HAR Dataset/test/X_test.txt",  col.names = features$features_name)
y_test  <- read.table("./UCI HAR Dataset/test/y_test.txt",  col.names = "n")

# importing train data and assigning to the variables

subject_train  <- read.table("./UCI HAR Dataset/train/subject_train.txt",  col.names = c( "subject"))
x_train  <- read.table("./UCI HAR Dataset/train/X_train.txt",  col.names = features$features_name)
y_train  <- read.table("./UCI HAR Dataset/train/y_train.txt",  col.names = "n")


# step 1 : Mergeing the subject , train, test datasets to create one new dataframe 
#          Result is to a variable df

subject_merge <- rbind(subject_train , subject_test)
X <- rbind(x_train , x_test)
Y <- rbind(y_train , y_test)

df <- cbind(subject_merge, X, Y)
df
names(df)

# step:2
# Extracts only the measurements on the mean and standard deviation for each measurement 
# result is stored in the variable  Tidy_data

Tidy_data <- df %>% select(subject , n , contains("mean") , contains("std"))
dim(Tidy_data)


# Step 3: Uses descriptive activity names to name the activities in the data set

Tidy_data$n <- activity_labels[Tidy_data$n, 2]


# step 4:

# Appropriately labels the data set with descriptive variable names
# following acronyms can be replaced:

# Acc as Accelerometer , Gyro as Gyroscope , BodyBody as Body
# Mag as Magnitude , ^t as Time , tBody as TimeBody 
# -mean() as Mean , -std as STD, -freq() as Frequency, angle as Angle
# gravity as Gravity
# using gsub() function replaces all matches of a string

names(Tidy_data)[2] = "activity"
names(Tidy_data)<-gsub("Acc", "Accelerometer", names(Tidy_data))
names(Tidy_data)<-gsub("Gyro", "Gyroscope", names(Tidy_data))
names(Tidy_data)<-gsub("BodyBody", "Body", names(Tidy_data))
names(Tidy_data)<-gsub("Mag", "Magnitude", names(Tidy_data))
names(Tidy_data)<-gsub("^t", "Time", names(Tidy_data))
names(Tidy_data)<-gsub("^f", "Frequency", names(Tidy_data))
names(Tidy_data)<-gsub("tBody", "TimeBody", names(Tidy_data))
names(Tidy_data)<-gsub("-mean()", "Mean", names(Tidy_data), ignore.case = TRUE)
names(Tidy_data)<-gsub("-std()", "STD", names(Tidy_data), ignore.case = TRUE)
names(Tidy_data)<-gsub("-freq()", "Frequency", names(Tidy_data), ignore.case = TRUE)
names(Tidy_data)<-gsub("angle", "Angle", names(Tidy_data))
names(Tidy_data)<-gsub("gravity", "Gravity", names(Tidy_data))

# step 5:
# From the data set in step 4, creates a second, independent TidyData set 
# with the average of each variable for each activity and each subject.

# created a TidyData from Tidy_data grrouped from subject and activity and 
# summarised by the function mean then writed into table called the TidyData.txt

TidyData <- Tidy_data %>%                        
  group_by(subject , activity) %>%
  summarise_all(funs(mean))
write.table(TidyData, "TidyData.txt", row.name=FALSE)

# checking the structure and dim of data using the str() and dim()  
str(TidyData)
dim(TidyData)

# viewing the TidyData that is ready for analysis
View(TidyData)



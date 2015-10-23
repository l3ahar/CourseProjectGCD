# Getting and Cleaning Data
# Course Project
# OCT 2015

################ Reading data from URL and Making desirable data set ##################
# Unzipping the file into temp file and directory
tf <- tempfile() 
td<-tempdir()
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,tf,mode="wb")
UCIds<-unzip(tf,exdir=td)  # This will give path to different spaces in the folder

#Reading "subjects" to put them for subjects in Test data
sbjTest<-read.table(UCIds[14])
sbjTest<-as.vector(sbjTest)

# Getting data from addresses in the UCIds
TestSet<-read.table(UCIds[15])
TestLabels<-read.table(UCIds[16])
Test<-cbind(TestLabels,sbjTest,TestSet)



#Reading "features" names to put them for column names in Test data
fts<-read.table(UCIds[2])
ftsn<-fts$V2
ftsn<-as.character(ftsn)
names(Test)<-c("Labels","Subject",ftsn)



# By following the same process for training set:

#Reading "subjects" to put them for subjects in Training data
sbjTrain<-read.table(UCIds[26])
sbjTrain<-as.vector(sbjTrain)

TrainSet<-read.table(UCIds[27])
TrainLabels<-read.table(UCIds[28])
Train<-cbind(TrainLabels,sbjTrain,TrainSet)
names(Train)<-c("Labels","Subject",ftsn)


###################### STEP 1 #######################
# Merges the training and the test sets to create one data set #

mergedData<- rbind(Test,Train)

###################### STEP 2 #######################
# Extracts only the measurements on the mean and standard deviation for each measurement #
# The problem statement for this step was a bit vague for me. Based on my final perception, it requires only the columns of the dataset which have either "mean" or "std" in their neme.

n<-names(mergedData)  # n is a variable containing all the names for the features

# Now by using "grepl" specific patterns can be seen in n (For this section "mean" and "std")
con1<-grepl("mean",n,ignore.case= TRUE) # 1st Condition (features with "mean" in their name)
con2<-grepl("std",n,ignore.case = TRUE) # 2nd Condition (features with "std" in their name)
con<-con1 | con2                         # Final condition is the combination of those two conditions

# "dataMeanStd is the data frame required by step 2 of the project statement
dataMeanStd<-mergedData[,con]
dataMeanStd<-cbind(mergedData$Labels,mergedData$Subject,dataMeanStd)
names(dataMeanStd)[1:2]<-c("Labels","Subject")
###################### STEP 3 #######################
# Uses descriptive activity names to name the activities in the data set #
labs<-read.table(UCIds[1])        #Activitly Labels
labs<-labs$V2                     # By reading activity labels we will have a matrix with two columns. We need the second column (V2)

# Now we define activity labels as factors and chenge the levels of the labels in dataset to descriptive names
labs<- as.factor(labs)
dataMeanStd$Labels<- as.factor(dataMeanStd$Labels)
levels(dataMeanStd$Labels)<-labs
names(dataMeanStd)[1]<-"Activity"          # Previously the first column was called "Labels". Now we change it to "Activity"

###################### STEP 4 #######################
# Appropriately labels the data set with descriptive variable names #
varNames<-names(dataMeanStd)  #Variable names
# By using "gsub" command we would be able to recognize patterns in "varNames" and replace them
#changing all the t's will make trouble. We only need to change t's that are at the beginning of the strings. By looking at the names we can see that they start either with "tB" or "tG"
varNames<-gsub("tB","Time B",varNames) 
varNames<-gsub("tG","Time G",varNames)
# Acc stands for Accelaration
varNames<-gsub("Acc"," Accelaraion ",varNames)
# Like the process I went through to change t to time, we change f to frequency.
varNames<-gsub("fB","Frequency B",varNames)
#Mag stands for Magnitude
varNames<-gsub("Mag","Magnitude",varNames)
# std stands for standard deviation
varNames<-gsub("std"," Standard Deviation ",varNames)
# Freq in meanFreq stands for frequency. If we change all the "freq" terms we'll encounter problem. So I replace the whole "meanFreq" expression.
varNames<-gsub("meanFreq","Mean Frequency",varNames)
#Some other small changes:
varNames<-gsub("BodyBody","Body",varNames)
varNames<-gsub("JerkMagnitude","Jerk Magnitude",varNames)
varNames<-gsub("()","",varNames)
varNames<-gsub("BodyGyro","Body Gyro",varNames)
names(dataMeanStd)<-varNames

###################### STEP 5 #######################
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#For this part I used reshape2 package.
#By "Melting" and "Casting" I could perform getting the mean of each measurement

#Melting Data Frame
library(reshape2)
dataMelt<-melt(dataMeanStd,id = c("Activity","Subject"))

#Casting Data Frames
dataCast<-dcast(dataMelt,Activity~Subject+variable,mean)

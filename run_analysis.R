##This R Script reads in the test and training data,puts it together
##and outputs with two different dataframes:
##training_and_test_data and aggregate_data.

##NOTE: You do also need the "feattures.txt" file for the labels

##these get output as training_and_test_data.csv
##and aggregate_data.csv to the working directory

##read in all of the necessary data
testx<-read.table("Course3/UCI HAR Dataset/test/X_test.txt")
testy<-read.table("Course3/UCI HAR Dataset/test/Y_test.txt")
trainx<-read.table("Course3/UCI HAR Dataset/train/X_train.txt")
trainy<-read.table("Course3/UCI HAR Dataset/train/Y_train.txt")
cnames<-read.table("Course3/UCI HAR Dataset/features.txt")

##I only want the columns with mean and std in their title.
keep<-(grep("mean|std", cnames$V2))
##but I don't want the meanFreq, so I am going to remove those from the
##names
rem1<-(grep("meanFreq", cnames$V2))
keep<-setdiff(keep,rem1)

##now I have the exact coumns that I need!
cnames2<-cnames$V2[keep]

##only keeping the columns that correspond to the columns that I want
##and replaceing the non-named colums with the correct names.
testx2<-testx[,keep]
trainx2<-trainx[,keep]
names(testx2)<-cnames2
names(trainx2)<-cnames2

#Now I need to fix the activities and add it to the data sets
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING

testy[testy=="1"]<-"Walking"
testy[testy=='2']<-"Walking Upstairs"
testy[testy=='3']<-"Walking Downstairs"
testy[testy=='4']<-"Sitting"
testy[testy=='5']<-"Standing"
testy[testy=='6']<-"Laying"

trainy[trainy=="1"]<-"Walking"
trainy[trainy=='2']<-"Walking Upstairs"
trainy[trainy=='3']<-"Walking Downstairs"
trainy[trainy=='4']<-"Sitting"
trainy[trainy=='5']<-"Standing"
trainy[trainy=='6']<-"Laying"

##add the activity column to the data sets

testx2$Activity<-testy$V1
trainx2$Activity<-trainy$V1

##put the activity column in the front

testx3<-testx2[,c(dim(testx2)[2],1:(dim(testx2)[2]-1))]
trainx3<-trainx2[,c(dim(trainx2)[2],1:(dim(trainx2)[2]-1))]

#I finally get to put them together!
totdat<-rbind(trainx3, testx3)

##the second data set is easier, as the summzarize_all function
##helps us out with that
agg1<-totdat%>%
  group_by(Activity)%>%
  summarize_all(list(average=mean))

###Now the yucky part. I wanted good names for the labels,
##so this whole next part is fixing the names of the columns

holder1<-names(totdat)

egad1<-str_replace(holder1, "f","")
egad2<-str_replace(egad1, "t","")
holder1<-egad2[which(!is.na(str_match(egad2,"mean")))]
holder1<-paste("Mean of", holder1)
holder1<-str_replace(holder1, "-mean\\(\\)", "")
holder1<-str_replace(holder1, "-", " Axis ")

#mean fixed, put back in the list
egad2[which(!is.na(str_match(egad2,"mean")))]<-holder1

##now for std
holder1<-egad2[which(!is.na(str_match(egad2,"std")))]
holder1<-paste("Standard Deviation of", holder1)
holder1<-str_replace(holder1, "-std\\(\\)", "")
holder1<-str_replace(holder1, "-", " Axis ")

##str fixed
egad2[which(!is.na(str_match(egad2,"std")))]<-holder1

#some of my earlier code removed the "t" from std, so I gotta fix that
holder1<-egad2[which(!is.na(str_match(egad2,"sd")))]
holder1<-paste("Standard Deviation of", holder1)
holder1<-str_replace(holder1, "-sd()", "")
holder1<-str_replace(holder1, "-", " Axis ")
holder1<-str_replace(holder1,"\\(\\)","")

#all names fixed!
egad2[which(!is.na(str_match(egad2,"sd")))]<-holder1

#now I can put them back onto the dataset
names(totdat)<-egad2

#and, while I am at it, I can fix the names of the agg data set
##they are pretty much the same, I am just adding "averge of"
##to the beginning
aggnames<-paste("Average of", names(totdat))

aggnames[1]<-"Activity"
names(agg1)<-aggnames
names(totdat)[1]<-"Activity"

##write them out and we are done!
write.table(totdat,"training_and_test_data.txt", row.names=FALSE)
write.table(agg1,"aggregate_data.txt", row.names=FALSE)


run_analysis <- function(){

# running this script, you can read what it is doing looking at console, thanks to "message" function
    
##################################################
        # PART 0 - SETTING UP FOR PROJECT
##################################################

# get working directory and setting path
message("This script is creating tiny data from measurements of 6 different activities using sensors inside Samsung smartphone Galaxy SII...")
message("My working dir")    
print(getwd())
    path <- "./Getting_and_Cleaning_Data/getting_project"

# check if new directory already exists (dir's name is "course name/project" and dowload zip file in it
    if(!file.exists(path)){dir.create(path)}

# download zip file from URL indicated in the project and unzip it
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
message("Downloading data from URL...")
    #download.file(url, destfile = "./Getting_and_Cleaning_Data/getting_project/dataZip.zip")
    pathFile <- file.path(path, "dataZip.zip")    
    unzip(zipfile = pathFile, exdir = path)

# read all the files inside the unzipped dir, creating a list (recursive = TRUE expand ALL subdirectories present in the main ones)
    pathFileZipped <- file.path(path, "UCI HAR Dataset")
message("Unzipping data...")
    listFiles <- list.files(pathFileZipped, recursive = TRUE)
    print(listFiles)

###########################################
        # PART 1 - MERGING DATA
###########################################

# reading desired datas into new variables (Inertial files are not necessary). They are "test" and "train" inside 2 different directories
message("reading variables from different files...")
    FeaturesTrain <- read.table(file.path(pathFileZipped, "train", "X_train.txt"),header = FALSE)
    FeaturesTest  <- read.table(file.path(pathFileZipped, "test" , "X_test.txt" ),header = FALSE)
    
    ActivityTrain <- read.table(file.path(pathFileZipped, "train", "y_train.txt"),header = FALSE)
    ActivityTest  <- read.table(file.path(pathFileZipped, "test" , "y_test.txt" ),header = FALSE)
    
    SubjectTrain <- read.table(file.path(pathFileZipped, "train", "subject_train.txt"),header = FALSE)
    SubjectTest  <- read.table(file.path(pathFileZipped, "test" , "subject_test.txt"),header = FALSE)

# creating a new data frame by merging previous data by row with "rbind" function
message("creating 3 different databases...")
    Features <- rbind(FeaturesTrain, FeaturesTest)
    Activity <- rbind(ActivityTrain, ActivityTest)
    Subject  <- rbind(SubjectTrain, SubjectTest)

# simplify variables' name. "features"' names are present in "features.txt" file, in the "V2" column
message("changing variables' names with something more comprehensible...")
    names(Activity) <- c("activity")    
    names(Subject)  <- c("subject")
    featuresNames   <- read.table(file.path(pathFileZipped, "features.txt"), header = FALSE)
    names(Features) <- featuresNames$V2

# final steps, global merging by columns "cbind"
message("creating a single and useful data base...")
    almostData <- cbind(Subject, Activity)
    Data <- cbind(Features, almostData)

###################################################################################
    # PART 2 - EXTRACTING MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT
###################################################################################

# "mean" and "standard deviation" are inside "featuresNames", so i use grep function to subset
message("extracting MEAN and STANDARD DEVIATION columns...")
    subfeaturesNames <- featuresNames$V2[grep("mean\\(\\)|std\\(\\)", featuresNames$V2)]

# now, I subset merged data.frame "Data" using previous variable + "subject" + "activity"
    names <- c(as.character(subfeaturesNames), "subject", "activity")
    Data <- subset(Data, select=names)

########################################################################################
    # PART 3 - Uses descriptive activity names to name the activities in the data set
########################################################################################

# reading the 6 activity names (like WALKING - WALKING UPSTAIRS...) and label merged data.frame "Data" with them (instead of 1 2 3 4 5 6)
message("changing activities' names inside the database: WALKING, WALKING_UPSTAIRS...")
    activityLabels <- read.table(file.path(pathFileZipped, "activity_labels.txt"),header = FALSE)
    #print(activityLabels)
    Data$activity <- factor(Data$activity, labels = activityLabels[,2])
    #head(Data$activity, 10)

########################################################################################
# PART 4 - Appropriately labels the data set with descriptive variable names
########################################################################################
message("make more readable features: Time, Gyroscope, frequency...")
    names(Data)
# in the "features_info.txt" file there is the explanation of the meaning of different features:
#   - "t"       stay for "time",            so it will be replaced by "time"
#   - "Acc"     stay for "Accelerometer"    so it will be replaced by "Accelerometer"
#   - "Gyro"    stay for "Gyroscope"        so it will be replaced by "Gyroscope" 
#   - "f"       stay for "frequency"        so it will be replaced by "frequency"
#   - "Mag"     stay for "Magnitude         so it will be replaced by "Magnitude"
#   - "BodyBody"stay for "body"             so it will be replaced by "Body"
# I subset them using "gsub" function
    names(Data) <- gsub("^t",       "time",          names(Data))
    names(Data) <- gsub("Acc",      "Accelerometer", names(Data))
    names(Data) <- gsub("Gyro",     "Gyroscope",     names(Data))
    names(Data) <- gsub("^f",       "frequency",     names(Data))
    names(Data) <- gsub("Mag",      "Magnitude",     names(Data))
    names(Data) <- gsub("BodyBody", "Body",          names(Data))
    
    # checking Data's names
    print(names(Data))

############################################################################################
# PART 5 - From the data set in step 4, creates a second, independent tidy data set with 
#          the average of each variable for each activity and each subject.
############################################################################################

# using "plyr" package and function "aggregate" I can summarize using "mean"
message("creating new file tidydata.txt where tidy data are stored (no columns' name)...")
    library(plyr);
    secondData <- aggregate(. ~subject + activity, Data, mean)
    secondData <- secondData[order(secondData$subject, secondData$activity),]
    # checking structure
    print(head(secondData))
    pathTiny <- file.path(path, "tidydata.txt")    
    write.table(secondData, file = pathTiny, row.name=FALSE)
message("FINISHED!")
message("To read the data, according to forum's tips, use")
message("data <- read.table(file_path, header = TRUE)")
message("where file_path is the complete path of the tidydata.txt file")
}

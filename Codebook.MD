# ORIGINAL DATA CODEBOOK
## Raw data zip file
Data used in this project are found in the following URL:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

## Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

# MY SCRIPT'S CODEBOOK
Here you'll find description of the tinydata.txt table.
## ID vars
- col(1) subject: The participant ("subject") ID
- col(2) activity: The label of the activity performed when the corresponding measurements were taken. See 
activity_labels.txt 
file inside raw data file for a better description.

## FEATURES vars
Colums from 3 to 68: these are the features obtained from original data (see features.txt file inside raw data folder) only about MEAN and STD value.
Better names are now shown:
- "time" replaces "t"
- "Accelerometer" replacs "Acc"
- "Gyroscope" replaces "Gyro"
- "frequency" replaces "f"
- "Magnitude" replaces "Mag"
- "Body" replaces "BodyBody"
Units of measurements are the same of the original raw data

## HOW THE SCRIPT WORKS
The script's code is described with a lot of comments, so every step is clear. Also, during script's execution, on console you'll read what step the script is currently performing.
Here a brief description.
- starting from Working Directory, it checks if new folder is already present: if not, it will be created.
- download raw data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip and unzip it into a new folder "UCI HAR Dataset"
- for this project, "Inertial Signals" are not used
- creating variables starting from raw data, using 6 files ("X" and "y" train, "X" and "y" test, "subject" train and test
- creating 3 differents df merged by rows (rbind): "Features" "Activity" "Subject"
- using new names, taken from featuresNames$V2
- creating last bug "Data" df, by columns
- using regular expression MEAN and STD to subset the "Data" df
- transforming the original number linked to Activity (1,2,3,4,5,6) with the correct name (WALKING, ...) and using them in the df "Data"
- as said in the above paragraph, features' name are replaced with something more comprehensible
- using "plyr" package to calculate the MEAN for each subject and each activity

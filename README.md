# README
zsomogyvari  
Saturday, April 25, 2015  

# README file for run_analysis.R

The code "run_analysis.R" 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



##Loading in the required libraries:

```r
library(data.table)
library(dplyr)
library(tidyr)
```

## Reading in the files containing relevant data

```r
subject_train   <- read.table("train/subject_train.txt")        ##Subjects
y_train         <- read.table("train/y_train.txt")              ##Activities
X_train         <- read.table("train/X_train.txt")              ##Data
                  
subject_test   <- read.table("test/subject_test.txt")           ##Subjects
y_test         <- read.table("test/y_test.txt")                 ##Activities
X_test         <- read.table("test/X_test.txt")                 ##Data
```

## Putting together the data in one tbl_df file
As only the measurements on the mean and standard deviation for each measurement are needed, only the corresponding columns of the data files are used.
From the code books "feature.txt" and "features_info.txt" the following variables are identified as the means and standard deviations for each measurement (the numbers are column numbers in X_train.txt and X_test.txt):

* 1 tBodyAcc-mean()-X
* 2 tBodyAcc-mean()-Y
* 3 tBodyAcc-mean()-Z
* 4 tBodyAcc-std()-X
* 5 tBodyAcc-std()-Y
* 6 tBodyAcc-std()-Z
* 121 tBodyGyro-mean()-X
* 122 tBodyGyro-mean()-Y
* 123 tBodyGyro-mean()-Z
* 124 tBodyGyro-std()-X
* 125 tBodyGyro-std()-Y
* 126 tBodyGyro-std()-Z


```r
Activity_data <- cbind(subject_train, y_train, select(X_train, c(1:6, 121:126))) %>%
                 rbind(
                 cbind(subject_test, y_test, select(X_test, c(1:6, 121:126)))
                        )
Activity_data <- tbl_df(Activity_data)
```

## Setting the column names

```r
colnames(Activity_data) <- c(
        "Subject",
        "Activity",
        "Body_acceleration_mean_X",
        "Body_acceleration_mean_Y",
        "Body_acceleration_mean_Z",
        "Body_acceleration_stdev_X",
        "Body_acceleration_stdev_Y",
        "Body_acceleration_stdev_Z",
        "Body_angularVelocity_mean_X",
        "Body_angularVelocity_mean_Y",
        "Body_angularVelocity_mean_Z",
        "Body_angularVelocity_stdev_X",
        "Body_angularVelocity_stdev_Y",
        "Body_angularVelocity_stdev_Z"
        )
```

## Replacing activity numbers with descriptive activity names

```r
Activity_data$Activity <-
        Activity_data$Activity %>% 
        replace(Activity_data$Activity == 1, "WALKING") %>% 
        replace(Activity_data$Activity == 2, "WALKING_UPSTAIRS") %>% 
        replace(Activity_data$Activity == 3, "WALKING_DOWNSTAIRS") %>% 
        replace(Activity_data$Activity == 4, "SITTING") %>% 
        replace(Activity_data$Activity == 5, "STANDING") %>% 
        replace(Activity_data$Activity == 6, "LAYING")
```

The structure of the resultant file:

```r
str(Activity_data)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	10299 obs. of  14 variables:
##  $ Subject                     : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Activity                    : chr  "STANDING" "STANDING" "STANDING" "STANDING" ...
##  $ Body_acceleration_mean_X    : num  0.289 0.278 0.28 0.279 0.277 ...
##  $ Body_acceleration_mean_Y    : num  -0.0203 -0.0164 -0.0195 -0.0262 -0.0166 ...
##  $ Body_acceleration_mean_Z    : num  -0.133 -0.124 -0.113 -0.123 -0.115 ...
##  $ Body_acceleration_stdev_X   : num  -0.995 -0.998 -0.995 -0.996 -0.998 ...
##  $ Body_acceleration_stdev_Y   : num  -0.983 -0.975 -0.967 -0.983 -0.981 ...
##  $ Body_acceleration_stdev_Z   : num  -0.914 -0.96 -0.979 -0.991 -0.99 ...
##  $ Body_angularVelocity_mean_X : num  -0.0061 -0.0161 -0.0317 -0.0434 -0.034 ...
##  $ Body_angularVelocity_mean_Y : num  -0.0314 -0.0839 -0.1023 -0.0914 -0.0747 ...
##  $ Body_angularVelocity_mean_Z : num  0.1077 0.1006 0.0961 0.0855 0.0774 ...
##  $ Body_angularVelocity_stdev_X: num  -0.985 -0.983 -0.976 -0.991 -0.985 ...
##  $ Body_angularVelocity_stdev_Y: num  -0.977 -0.989 -0.994 -0.992 -0.992 ...
##  $ Body_angularVelocity_stdev_Z: num  -0.992 -0.989 -0.986 -0.988 -0.987 ...
```

## Creating a second, independent tidy data set with the average of each variable for each activity and each subject

```r
Activity_data_mean <- summarise(group_by(Activity_data, Subject, Activity), mean(Body_acceleration_mean_X)) %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_acceleration_mean_Y)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_acceleration_mean_Z)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_acceleration_stdev_X)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_acceleration_stdev_Y)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_acceleration_stdev_Z)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_angularVelocity_mean_X)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_angularVelocity_mean_Y)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_angularVelocity_mean_Z)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_angularVelocity_stdev_X)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_angularVelocity_stdev_Y)))  %>%
   inner_join(summarise(group_by(Activity_data, Subject, Activity), mean(Body_angularVelocity_stdev_Z))) 
```

The structure of the resultant file:

```r
str(Activity_data_mean)
```

```
## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	180 obs. of  14 variables:
##  $ Subject                           : int  1 1 1 1 1 1 2 2 2 2 ...
##  $ Activity                          : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...
##  $ mean(Body_acceleration_mean_X)    : num  0.222 0.261 0.279 0.277 0.289 ...
##  $ mean(Body_acceleration_mean_Y)    : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
##  $ mean(Body_acceleration_mean_Z)    : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
##  $ mean(Body_acceleration_stdev_X)   : num  -0.928 -0.977 -0.996 -0.284 0.03 ...
##  $ mean(Body_acceleration_stdev_Y)   : num  -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...
##  $ mean(Body_acceleration_stdev_Z)   : num  -0.826 -0.94 -0.98 -0.26 -0.23 ...
##  $ mean(Body_angularVelocity_mean_X) : num  -0.0166 -0.0454 -0.024 -0.0418 -0.0351 ...
##  $ mean(Body_angularVelocity_mean_Y) : num  -0.0645 -0.0919 -0.0594 -0.0695 -0.0909 ...
##  $ mean(Body_angularVelocity_mean_Z) : num  0.1487 0.0629 0.0748 0.0849 0.0901 ...
##  $ mean(Body_angularVelocity_stdev_X): num  -0.874 -0.977 -0.987 -0.474 -0.458 ...
##  $ mean(Body_angularVelocity_stdev_Y): num  -0.9511 -0.9665 -0.9877 -0.0546 -0.1263 ...
##  $ mean(Body_angularVelocity_stdev_Z): num  -0.908 -0.941 -0.981 -0.344 -0.125 ...
##  - attr(*, "vars")=List of 1
##   ..$ : symbol Subject
```

## Creating output file

```r
write.table(Activity_data_mean, file = "Average_variables.txt",  row.name=FALSE)
```



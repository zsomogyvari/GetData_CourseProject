library(dplyr)
library(tidyr)

## Reading in the files containing relevant data
subject_train   <- read.table("train/subject_train.txt")        ##Subjects
y_train         <- read.table("train/y_train.txt")              ##Activities
X_train         <- read.table("train/X_train.txt")              ##Data
                  
subject_test   <- read.table("test/subject_test.txt")           ##Subjects
y_test         <- read.table("test/y_test.txt")                 ##Activities
X_test         <- read.table("test/X_test.txt")                 ##Data

## Putting together the data in one tbl_df file
Activity_data <- cbind(subject_train, y_train, select(X_train, c(1:6, 121:126))) %>%
                 rbind(
                 cbind(subject_test, y_test, select(X_test, c(1:6, 121:126)))
                        )
Activity_data <- tbl_df(Activity_data)

## Setting the column names
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

## Replacing activity numbers with descriptive activity names
Activity_data$Activity <-
        Activity_data$Activity %>% 
        replace(Activity_data$Activity == 1, "WALKING") %>% 
        replace(Activity_data$Activity == 2, "WALKING_UPSTAIRS") %>% 
        replace(Activity_data$Activity == 3, "WALKING_DOWNSTAIRS") %>% 
        replace(Activity_data$Activity == 4, "SITTING") %>% 
        replace(Activity_data$Activity == 5, "STANDING") %>% 
        replace(Activity_data$Activity == 6, "LAYING")

## Creating a second, independent tidy data set with the average of each variable for each activity and each subject
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

## Creating output file
write.table(Activity_data_mean, file = "Average_variables.txt",  row.name=FALSE)
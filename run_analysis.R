run_analysis <- function() {
  library(dplyr)

  # Read data labels
  data_labels <- read.table("getdata-projectfiles-UCI HAR Dataset//features.txt")

  # Format data labels into a list
  data_labels <- as.character( paste(data_labels[,2],data_labels[,1] ) )
  
  # Remove - ( ) and space from data labels
  data_labels <- gsub(" ","", data_labels)
  data_labels <- gsub("-","", data_labels)
  data_labels <- gsub("\\(","", data_labels)
  data_labels <- gsub("\\)","", data_labels)
  data_labels <- gsub("\t","",data_labels)

  # Read activity labels
  activity_labels <- read.table("getdata-projectfiles-UCI HAR Dataset//activity_labels.txt")

  # Read training and test data
  train_data <- read.table("getdata-projectfiles-UCI HAR Dataset/train//X_train.txt")
  test_data <- read.table("getdata-projectfiles-UCI HAR Dataset/test//X_test.txt")

  # Read training and test activity labels
  train_activity <- read.table("getdata-projectfiles-UCI HAR Dataset/train//y_train.txt")
  test_activity <- read.table("getdata-projectfiles-UCI HAR Dataset/test//y_test.txt")

  # Read training and test subject labels
  train_subject <- read.table("getdata-projectfiles-UCI HAR Dataset/train//subject_train.txt")
  test_subject <- read.table("getdata-projectfiles-UCI HAR Dataset/test//subject_test.txt")

  # Merge training and test data
  data <- rbind(train_data, test_data)
  activity <- rbind(train_activity, test_activity)
  subject <- rbind(train_subject, test_subject)

  # Properly name all data columns
  names(data) <- data_labels

  # Adjust activity labels into factor
  activity[1] <- factor(activity[[1]], labels = activity_labels[[2]])

  # Name activity and subject column names and add columns to data
  data <- cbind(subject, activity, data)
  names(data)[1:2] <- c("Subject_id","Activity")

  # Extract all mean and standard deviation columns
  data <- select(data, Subject_id, Activity, contains("mean"), contains("std"))

  # Remove unwanted data
  rm("train_data","test_data","train_activity","test_activity", "train_subject","test_subject")

  # Group data
  grouped_data <- group_by(data,Subject_id,Activity)
  
  # Initialize summary data table
  summarised_data <- summarise(grouped_data)
  
  # Generate summary data table for each variable then column bind the mean to summarised_data
  for (i in 3:ncol(grouped_data)) {
    # Replace name of column to be summarised
    names(grouped_data)[i] <- "ivariable"
    # Summarise, obtaining mean of ivariable
    summarised_variable <- summarise(grouped_data, mean(ivariable))
    
    # column bind to summarised_data
    summarised_data <- cbind( summarised_data, summarised_variable[,3])
    
    # Adjust names of variable to original variable name
    names(grouped_data)[i] <- names(data)[i]
    names(summarised_data)[i] <- names(data)[i]
  }
  
  # output file
  write.table(summarised_data, file = "output.txt")

  # return data
  data
}
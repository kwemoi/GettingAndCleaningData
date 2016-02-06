run_analysis <- function()
{
    print('================================================================================')
    print('RUNNING ASSIGNMENT PROJECT')
    print('--------------------------------------------------------------------------------')
    print('1) Initializing libraries ...')
    library(dplyr)

    print('2) Reading data ...')
    # This first line will likely take a few seconds. Be patient!
    datapath <- "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/"
    testpath <- paste(datapath, "test/", sep = "")
    trainpath <- paste(datapath, "train/", sep = "")
    
    # Read activity labels
    activity_labels <- read_data(datapath, "activity_labels.txt")
    names(activity_labels) <- c("Id", "ActivityLabel")
    # Read activity labels
    features <- read_data(datapath, "features.txt")
    names(features) <- c("Id", "FeatureName")
    
    # Read test data
    testdata <- read_dataset(testpath, 
                             "subject_test.txt", "y_test.txt", "X_test.txt")
    # Read train data
    traindata <- read_dataset(trainpath, 
                              "subject_train.txt", "y_train.txt", "X_train.txt")

    print('--------------------------------------------------------------------------------')
    print('1. Merge the training and the test sets to create one data set.')
    print('--------------------------------------------------------------------------------')

    # row-bind test and train data
    alldata <- rbind(testdata, traindata)
    dim(alldata)
    
    print("Done.")
    
    print('--------------------------------------------------------------------------------')
    print('2. Extract only the measurements on the mean and standard deviation for each measurement.')
    print('--------------------------------------------------------------------------------')
    
    # Locate mean() and std() features
    grepString <- paste("[Mm]ean\\(\\)", 
                        "[Ss]td\\(\\)", 
                        sep = "|")
    feats2use <- features[ grep(grepString, features$FeatureName), ]
    # Build a vector with the actual columns to extract
    feats2useIds <- c(c(1, 2), feats2use$Id + 2)
    
    # Extract the appropriate features
    dataextract <- alldata[, feats2useIds]
    dim(dataextract)
    
    print("Done.")
    
    print('--------------------------------------------------------------------------------')
    print('3. Use descriptive activity names to name the activities in the data set.')
    print('--------------------------------------------------------------------------------')
    # Print a grouped summary by activity ids
    summarise( group_by( dataextract, y ), n() )

    # Factorize y-column
    dataextract$y <- as.factor(dataextract$y)
    # Reset factor y-column levels to the activity labels!
    #   levels(dataextract$y) <- activity_labels[,2]
    dataextract$y <- sapply(dataextract$y, FUN = lookUpVect, activity_labels)
    
    # Print a grouped summary by activity names
    summarise( group_by( dataextract, y ), n() )
    
    print("Done.")

    print('--------------------------------------------------------------------------------')
    print('4. Appropriately labels the data set with descriptive variable names.')
    print('--------------------------------------------------------------------------------')

    # Prepare the new features names    
    feats2use$FeatureName <- gsub("\\.", "", 
                                  make.names(feats2use$FeatureName) )
    # Build the full new features names
    newFeatNames <- c(c("Subject", "Activity"), 
                      feats2use$FeatureName)
    # Set the new features names
    names(dataextract) <- newFeatNames
    
    # Print a grouped summary (by activity names) with new features names
    summarise( group_by( dataextract, Activity ), n() )
    
    print("Done.")

    print('--------------------------------------------------------------------------------')
    print('5. From the data set in step 4, creates a second, independent tidy data set')
    print('   with the average of each variable for each activity and each subject.')
    print('--------------------------------------------------------------------------------')

    # Make a funs mean(..) function list
    funs <- funs(mean(., na.rm = TRUE))

    # Build the independent tidy data set 
    tidy_data <- dataextract %>% 
                 group_by(Subject, Activity) %>% 
                 summarise_each(funs(mean))

    # Build new features names with a '.mean' sufix
    newTidyFeatNames <- c(c("Subject", "Activity"), 
                          paste(feats2use$FeatureName, '.mean', sep = "") )
    # Set the new tidy features names
    names(tidy_data) <- newTidyFeatNames
    
    # Save the tidy data.frame
    tidy_filename <- "tidy_data.txt"
    write.table(tidy_data, file = tidy_filename, append = FALSE, 
                sep = " ", dec = ".", 
                col.names = TRUE, row.names = FALSE)
    print(paste("File", tidy_filename, "was written."))
    # Build and save the tidy variable-names
    tidy_vars_filename <- "tidy_variables.txt"
    vars <- names(tidy_data)
    varnames <- data.frame(id = 1:length(vars), variable = vars)
    write.table(varnames, file = tidy_vars_filename, append = FALSE, 
                quote = FALSE, 
                col.names = FALSE, row.names = FALSE)
    print(paste("File", tidy_vars_filename, "was written."))
    
    print("Done.")
    
    
    print("")
    print("")
    print("PROJECT COMPLETED!")
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
}

# Read data set files (subject, y, and x files)
read_dataset <- function(datapath, subset_file, y_file, x_file)
{
    print(paste("Reading dataset files, from ", datapath, " (please, wait) ..."))
    subset_data <- read_data(datapath, subset_file)
    names(subset_data) = c("subject")
    y_data <- read_data(datapath, y_file)
    names(y_data) = c("y")
    x_data <- read_data(datapath, x_file)
    
    dataset <- cbind(subset_data, y_data, x_data)
    print("")
    print(dim(dataset))
    print("")
    
    dataset
}
    
# Read data file
read_data <- function(datapath, filename)
{
    print(paste("Reading data file", filename, " ..."))
    data <- read.table(paste(datapath, filename, sep = ""), 
                       header = FALSE, sep = "", dec = ".")
    print(dim(data))

    data
}

# Substitude a numerical (ID) value with a lookup literal
lookUpVect <- function(id, lookup_table)
{
    entry <- lookup_table[lookup_table[,1] == as.integer(id), ]
    if (nrow(entry) > 0)
        return(entry[1, 2])
    NULL
}


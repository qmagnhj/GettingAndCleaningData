library(dplyr)

# Define general file names 
fname_activitylables <- "activity_labels.txt"
fname_features       <- "features.txt"


# constructDataset is a function that reads the test or train data - 
# depending on the type parameter - and returns the data as data frame.
constructDataset <- function (type) {
    
    # Check if type is "test" or "train", otherwise stop execution
    if(!(type %in% c("train","test"))) { 
        stop("type must be either \"test\" or \"train\".")
    }
    
    print(paste0("Constructing dataset for ", type))
    
    XfileToRead   <- paste0("./", type, "/X_", type, ".txt")
    YfileToRead   <- paste0("./", type, "/Y_", type, ".txt")
    SubfileToRead <- paste0("./", type, "/subject_", type, ".txt")
    
    # Get the variable names from the features file and clean them up 
    dfnames <- readLines(fname_features)
    dfnames <- sapply(strsplit(dfnames, " "), function(x) x[2])
    dfnames <- make.names(dfnames)
    dfnames <- gsub("\\.+","\\.",dfnames)    # replace multiple . with one .
    dfnames <- gsub("\\.$","",dfnames)       # remove . at end of names
    
    
    # Read data from the X file as fixed width format file into data frame
    print(paste0("Reading file: ",XfileToRead))
    df <- read.fwf(file = XfileToRead, 
                   widths = rep(16, 561),
                   col.names = dfnames)
    
    # Insert the subject ID into the dataframe
    
    print("Adding Subjects to dataset")
    Subjects <- readLines(SubfileToRead)
    df$Subject <- as.integer(Subjects)
    
    # Read data from the Y file and the activitiey lables file, then add
    # the activity labels 
    
    print("Adding activities to dataset")
    
    Yvalues <- data.frame(Activity = readLines(YfileToRead), 
                          stringsAsFactors = FALSE)
    YLabels <- readLines(fname_activitylables)
    YLabels <- as.data.frame(do.call(rbind,strsplit(YLabels, " ")), 
                             stringsAsFactors = FALSE)
    names(YLabels) <- c("Activity","Label")
    YAll <- left_join(Yvalues, YLabels)
    
    # Add the Y values (in the form of the text labels) and set the levels of 
    # the Activity factor align with the original activity numbers
    df$Activity <- YAll$Label
    levels(df$Activity) <- YLabels[,2]
    
    print("Dataset constructed")
    
    # Reorder columns so Subject and Activity are in the rightmost columns
    df<-select(df, Subject, Activity, everything())
    df
}


################################################################################
# 1.Merges the training and the test sets to create one data set.

totalDf <- rbind(constructDataset("train"),constructDataset("test"))


################################################################################
# 2.Extracts only the measurements on the mean and standard deviation for 
# each measurement. 

MeanAndStdDf <- select(totalDf, Subject, Activity, 
                       matches("\\.(mean|std)(\\..)?$"))


################################################################################
# 5.From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.


# Get the mean by lapply:ing over the columns and sapply:ing over a split
# on a combination of subject and activity (column 1 and 2)
tmp <- lapply(MeanAndStdDf[,c(-1,-2)], function(x) {
    sapply(split(x,list(MeanAndStdDf[,2],MeanAndStdDf[,1])), mean)})

# Combine the resulting list into a data frame (each list element from tmp 
# a becomes a column)
Averages <- as.data.frame(do.call(cbind,tmp))

# Add text string "Mean." to the variable names to indicate that these
# variables are now means (averages) of the previous variables
names(Averages) <- paste0("Mean.",names(Averages))



# The above procedure creates row names of the form Activity.Subject in text
# format (not the factor), so the activity name is easy to grab from that
tmp<-sapply(strsplit(row.names(Averages), "\\."), function(x) c(x[1],x[2]))

# Insert the Subjects and Activities and rearrange so they are in the
# rightmost columns
Averages$Subject <- as.integer(tmp[2,])
Averages$Activity <- tmp[1,]
Averages <- select(Averages, Subject, Activity, everything())

write.table(Averages,"Averages.txt",row.names = F)
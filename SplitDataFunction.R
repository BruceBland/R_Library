#####################################################
#
# SplitData function - Create a sample of a dataset
#
# returns training and test set in a list object
#
# Example Use: 
# ListOfDataFrames <- SplitData(df,PercentageToSplit=80)
# print(ListOfDataFrames[[1]]) # print training set
#
#
# Create by : Bruce Bland
#
#####################################################

# Split function
SplitData <- function(DataFrame,PercentageToSplit)
{
  # Will add index column
  DataFrame$x <- seq(1,nrow(DataFrame))
  
  ## 70% of the sample size
  smp_size <- floor(0.70 * nrow(DataFrame))
  
  # Create sampling index
  train_ind <- sample(DataFrame$x, size = smp_size)
  
  # Now split te data into training and test
  train <- DataFrame[train_ind, ]
  test <- DataFrame[-train_ind, ]
  
  # Return list of data frames
  ReturnList <- list(train,test)
  
  return(ReturnList)
}

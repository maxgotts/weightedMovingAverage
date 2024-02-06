


gauss <- function(x, center, width) return(exp(-0.5*((x - center)/width)^2)) # Gaussian kernel
rect <- function(x, center, width) return(as.numeric(abs(x-center)<width/2)) # Rectangular kernel for simple moving average
hann <- function(x, center, width) return((1/width) * cos(pi*(x-center)/width)^2  * rect(x, center, width)) # Hann kernel



weightedMovingAverage <- function(dat, axes, width, kernel=gauss, alpha=0.05, spine=NULL) {
  dat <- as.data.frame(dat) # Convert the given data to be a dataframe
  z <- qnorm((1-alpha/2)) # Calculate the z-score given the given FDR alpha
  x.name <- as.character(axes$x) # Get the name of the x-axis, over which we will do the moving average
  y.name <- as.character(axes$y) # Get the name of the y-axis, that which will be averaged
  by.names <- as.character(unlist(axes)[setdiff(names(axes), c("x","y"))]) # Get the remaining names of the axes used to split the dataset
  spineNULL <- FALSE # Record that spine was not NULL
  if (is.null(spine)) {
    spineNULL <- TRUE # Record that spine was set to NULL
    spine <- seq(from=min(dat[,x.name]), to=max(dat[,x.name]), length.out=1000) # If none are provided, generate a new x-axis which will form the backbone of the average
  }
  
  if (length(by.names)==0) { # If there are no splits provided in axes
    if (nrow(dat)==0) { # If empty dataset has been provided, return an empty dataframe
      wma.dat <- as.data.frame(matrix(NA,nrow=0,ncol=5))
      colnames(wma.dat) <- c(y.name,"SD","Neff","SE")
      return(wma.dat)
    }
    wma.dat <- data.frame(x=spine) # Create main dataframe filled with the spine
    colnames(wma.dat) <- x.name # Name the new dataframe as the x-axis
    for (xIndex in 1:length(spine)) { # Iterate through steps in spine
      center <- spine[xIndex] # Center of kernel for step in spine
      unstd.kernel.value <- kernel(dat[,x.name], center, width) # Calculate kernel's values over data set 
      kernel.value <- unstd.kernel.value/sum(unstd.kernel.value) # Standardize kernel's values
      Average <- sum(kernel.value*dat[,y.name]) # Evaluate convolution over provided dataset to get weighted average
      SumOfSquares <- sum(kernel.value^2) # Get sum-of-squares of kernel's values
      Neff <- 1/SumOfSquares # Calculate effective N of the kernel based on sum-of-squares
      SD <- sqrt((sum(kernel.value*(dat[,y.name]-Average)^2))/(1-SumOfSquares)) # Get standard deviation of convolution
      SE <- z*SD/sqrt(Neff) # Calculate standard error using standard deviation, z-score, and effective N
      wma.dat[xIndex, c(y.name, "SD", "Neff", "SE")] <- c(Average, SD, Neff, SE) # Add relevant values to main dataframe
    }
  } else { 
    # Generate a dataframe of all combinations of split columns' values
    unique.values <- list() # Initialize list
    for (by.axis in by.names) unique.values[[by.axis]] <- unique(dat[,by.axis]) # Add unique values to the list for each split column
    rows.of.values <- do.call(expand.grid, unique.values) # Use expand.grid to create dataframe of all combinations
    
    # Recursively generate new sets of rows based on subsets of dataset
    wma.dat <- as.data.frame(matrix(NA,nrow=0,ncol=5+length(by.names))) # Create main dataframe with correct width and zero length
    for (row in 1:nrow(rows.of.values)) { # Iterate through expand.grid dataframe
      sub <- dat # Initialize subset of provided dataset, starting with the full dataset
      for (col in 1:length(by.names)) { # For each split value...
        sub <- sub[sub[,by.names[col]]==rows.of.values[row,col],] # ...subset the sub dataset by that value
      }
      newspine <- spine # New spine that will be sent to the recursive step below
      if (spineNULL) newspine <- NULL # If spine was originally NULL, let it continue to be so
      wma.add <- weightedMovingAverage(sub, list(x=x.name, y=y.name),
                                       width, kernel=kernel, alpha=alpha,
                                       spine=newspine
                ) # Perform the recursive step with all values the same except the axes to create a to-add dataframe
      wma.add[,by.names] <- rows.of.values[row,by.names] # Add split column values by name
      wma.dat <- rbind(wma.dat, wma.add) # Add the recursive to-add dataframe to the main dataframe
    }
  }
  return(wma.dat) # Return the main dataframe
}
wma <- ck <- cK <- WMA <- weighted.moving.average <- confidentKernel <- confident.kernel <- confidentKernels <- weightedMovingAverage # Add alternative function names for confidentKernels


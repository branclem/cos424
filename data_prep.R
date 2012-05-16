
# Choose a permanent test/train and validation set so that we all
# can agree not to use the validation data. Write it to a file.
# I've already checked it into the repository, so nobody should be
# running this function anymore.
create.data <- function() {
    # Read in the initial data from Stata format.
    library(foreign);
    raw.data <- read.dta(file='alcohol_data.dta');  

    # Shuffle and renumber the data.
    raw.data.shuffled <- raw.data[sample(nrow(raw.data)), ];
    raw.data.final <- data.frame(raw.data.shuffled, row.names=NULL);

    # Split into two pieces.
    data <- raw.data.final[1:8999,];
    validation.set <- raw.data.final[9000:10904, ];
    validation.set <- data.frame(validation.set, row.names=NULL);

    # Write the data.
    save(data, file='alcohol_data.rda');
    save(validation.set, file='alcohol_validation.rda');
}


# Return the data factored into integer form and with N/As
# omitted.
get.data.numeric <- function() {

    # Read the data.
    load('alcohol_data.rda');

    # Convert all fields to numeric.
    to.remove = NULL;
    for (i in 3:483) {
        data[,i] <- as.numeric(data[,i]);
    }

    # Omit any columns that don't have the response variable.
    data <- data[!is.na(data$C1),]
    data <- data.frame(data, row.names=NULL);

    # Prep the ethnicity data.
    data[is.na(data$G3A),]$G3A <- 0;
    data[is.na(data$G3B),]$G3B <- 0;
    data[is.na(data$G3C),]$G3C <- 0;
    data[is.na(data$G3D),]$G3D <- 0;
    data[is.na(data$G3E),]$G3E <- 0;

    # Remove any columns with less than 7000, the others have
    # N/A values replaced by the mean of the column.
    for (i in 3:483) {
        not.na <- data[!is.na(data[,i]),i];
        num.not.na <- nrow(data[!is.na(data[,i]),]);
	  if (num.not.na < 7000) {
            cat(i, ':', names(data)[i], ':', num.not.na, '\n');
            flush.console();
            to.remove <- c(to.remove, i);
	  }
	  else {
            data[is.na(data[,i]), i] <- round(mean(not.na));
	  }
    }

    cat(to.remove, '\n');
    data <- data[, -to.remove];

    return(data);

}


get.validation.numeric <- function(data) {

    # Read the data.
    load('alcohol_validation.rda');

    # Convert all fields to numeric.
    to.remove = NULL;
    for (i in 3:483) {
        validation.set[,i] <- as.numeric(validation.set[,i]);
    }

    # Omit any columns that don't have the response variable.
    validation.set <- validation.set[!is.na(validation.set$C1),]
    validation.set <- data.frame(validation.set, row.names=NULL);

    # Prep the ethnicity data.
    validation.set[is.na(validation.set$G3A),]$G3A <- 0;
    validation.set[is.na(validation.set$G3B),]$G3B <- 0;
    validation.set[is.na(validation.set$G3C),]$G3C <- 0;
    validation.set[is.na(validation.set$G3D),]$G3D <- 0;
    validation.set[is.na(validation.set$G3E),]$G3E <- 0;

    # Match features to input data.
    validation.set <- validation.set[colnames(validation.set) %in% colnames(data)];

    # Do mean thingy.
    for (i in 3:400) {
        not.na <- validation.set[!is.na(validation.set[,i]),i];
        validation.set[is.na(validation.set[,i]), i] <- round(mean(not.na));
    }

    return(validation.set);

}

# Return the data factored into integer form and with N/As
# omitted.
get.data.factor <- function(data=NULL) {

    # If they already have the numerical data they can pass
    # it in, otherwise read it ourselves
    if (is.null(data)) {
	    data <- get.data.numeric();
    }

    for (i in 3:400) {
        data[,i] <- as.ordered(data[,i]);
        if (length(levels(data[,i])) == 2 ) {
            data[,i] <- as.factor(as.numeric(data[,i]));
        }
    }

    data$C1 <- as.numeric(data$C1);

    return(data);

}



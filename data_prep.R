
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

    # Prep the data, converting relevant fields to numeric
    # and omitting N/A values.
    data$A2 <- as.numeric(data$A2);
    data$A3 <- as.numeric(data$A3);
    data$A5 <- as.numeric(data$A5);
    data$D3A <- as.numeric(data$D3A);
    data$D3B <- as.numeric(data$D3B);
    data$F5 <- as.numeric(data$F5);
    data$G14 <- as.numeric(data$G14);
    data$G15 <- as.numeric(data$G15);

    data$C1 <- as.numeric(data$C1);

    data.omitted <- data[!is.na(data$A1),]
    data.omitted <- data.omitted[!is.na(data.omitted$A2),]
    data.omitted <- data.omitted[!is.na(data.omitted$A3),]
    data.omitted <- data.omitted[!is.na(data.omitted$A5),]
    data.omitted <- data.omitted[!is.na(data.omitted$D3A),]
    data.omitted <- data.omitted[!is.na(data.omitted$D3B),]
    data.omitted <- data.omitted[!is.na(data.omitted$F5),]
    data.omitted <- data.omitted[!is.na(data.omitted$G14),]
    data.omitted <- data.omitted[!is.na(data.omitted$G15),]

    data.omitted<-data.omitted[!is.na(data.omitted$C1),]

    data.omitted<-data.frame(data.omitted, row.names=NULL);

    return(data.omitted);

}


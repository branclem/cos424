
# Return a list L with two elements:
# L#data  is the data for training and testing.
# L#validation.set is the final validation set.
get.data <- function() {

    # Read in the dataset.
    library(foreign);
    raw.data <- read.dta(file="alcohol_data.dta");  

# TODO: shuffle the data first.

    # Set aside a validation set.
    validation.set <- raw.data[9000:10904, ];
    data <- raw.data[1:8999,];

    # Prep the data, converting relevant fields to numeric
    # and omitting N/A values.
    data$A2 <- as.numeric(data$A2);
    data$A5 <- as.numeric(data$A5);
    data$D3A <- as.numeric(data$D3A);
    data$D3B <- as.numeric(data$D3B);
    data$C1 <- as.numeric(data$C1);

    data.omitted<-data[!is.na(data$A1),]
    data.omitted<-data.omitted[!is.na(data.omitted$A2),]
    data.omitted<-data.omitted[!is.na(data.omitted$A5),]
    data.omitted<-data.omitted[!is.na(data.omitted$D3A),]
    data.omitted<-data.omitted[!is.na(data.omitted$D3B),]
    data.omitted<-data.omitted[!is.na(data.omitted$C1),]

    data.omitted<-data.frame(data.omitted, row.names=NULL);

    list(data=data.omitted, validation.set=validation.set);

}


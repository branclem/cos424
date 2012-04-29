
# Read in the dataset.
library(foreign);
library(mixtools);
raw.data <- read.dta(file="alcohol_data.dta");

head(raw.data);
validation.set<-raw.data[9000:10904, ];
data<-raw.data[1:8999,];
#data$A2[grep('female', data$A2)]<-0;

data$A2<-sub("[[:graph:][:space:]]*\\(", "", data$A2);
data$A2<-sub("\\)[[:graph:][:space:]]*", "", data$A2);
data$A2<-as.numeric(data$A2);

data$A5<-sub("[[:graph:][:space:]]*\\(", "", data$A5);
data$A5<-sub("\\)[[:graph:][:space:]]*", "", data$A5);
data$A5<-as.numeric(data$A5);

data$D3A<-sub("[[:graph:][:space:]]*\\(", "", data$D3A);
data$D3A<-sub("\\)[[:graph:][:space:]]*", "", data$D3A);
data$D3A<-as.numeric(data$D3A);

data$D3B<-sub("[[:graph:][:space:]]*\\(", "", data$D3B);
data$D3B<-sub("\\)[[:graph:][:space:]]*", "", data$D3B);
data$D3B<-as.numeric(data$D3A);

data$C1<-sub("[[:graph:][:space:]]*\\(", "", data$C1);
data$C1<-sub("\\)[[:graph:][:space:]]*", "", data$C1);
data$C1<-as.numeric(data$C1);

#na.action = na.omit
#we need to skip NA values

data.omitted<-data[!is.na(data$A1),]
data.omitted<-data[!is.na(data$A2),]
data.omitted<-data[!is.na(data$A5),]
data.omitted<-data[!is.na(data$D3A),]
data.omitted<-data[!is.na(data$D3B),]
data.omitted<-data[!is.na(data$C1),]

fit<-glm(C1~A1+A2+A5+D3A+D3B, gaussian, data.omitted)
summary(fit)

plot.new()
points(y=fit$fitted.values,x = data.omitted$A1  col='red')
points(y=data.omitted$C1, x=data.omitted$A1, col='green')
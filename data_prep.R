
# Read in the dataset.
library(foreign);
library(mixtools);
raw.data <- read.dta(file="alcohol_data.dta");

head(raw.data);
validation.set<-raw.data[9000:10904, ];
data<-raw.data[1:8999,];

data$A2<-as.numeric(data$A2);
data$A5<-as.numeric(data$A5);
data$D3A<-as.numeric(data$D3A);
data$D3B<-as.numeric(data$D3B);
data$C1<-as.numeric(data$C1);

#na.action = na.omit
#we need to skip NA values

data.omitted<-data[!is.na(data$A1),]
data.omitted<-data.omitted[!is.na(data.omitted$A2),]
data.omitted<-data.omitted[!is.na(data.omitted$A5),]
data.omitted<-data.omitted[!is.na(data.omitted$D3A),]
data.omitted<-data.omitted[!is.na(data.omitted$D3B),]
data.omitted<-data.omitted[!is.na(data.omitted$C1),]

fit.linear<-lm(C1~A1+A2+A5+D3A+D3B, data.omitted, na.action=na.omit)
fit.glm<-glm(C1~A1+A2+A5+D3A+D3B, gaussian, data.omitted, na.action=na.omit)
summary(fit.glm)


plot.new()
plot(y=fit.linear$fitted.values, x = data.omitted$A1,  col='red')
points(y=data.omitted$C1, x=data.omitted$A1, col='green')
points(y=fit.glm$fitted.values, x = data.omitted$A1,  col='orange')


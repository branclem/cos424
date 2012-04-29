
source('data_prep.R');
source('validation.R');

D <- get.data();
data <- D$data;

fit.linear<-lm(C1~A1+A2+A5+D3A+D3B, data, na.action=na.omit)
fit.glm<-glm(C1~A1+A2+A5+D3A+D3B, gaussian, data, na.action=na.omit)
summary(fit.glm)

#plot(y=fit.linear$fitted.values, x = data$A1,  col='red')
#points(y=data.omitted$C1, x=data$A1, col='green')
#points(y=fit.glm$fitted.values, x = data$A1,  col='orange')

# Run it on this stuff.
source('validation.R');
N <- 1000;
actual <- data$C1[1:N];
pred <- cross.validate(5,
           data[1:N,],
           function(data) { return(glm(C1~A1+A2+A5+D3A+D3B, gaussian, data=data)) },
	     function(model, new.data) { return(predict(model, newdata=new.data)) } );
rounded.pred <- round(pred);
mean(abs(pred - actual));
mean(abs(rounded.pred - actual));
mean(abs(rep(1, N) - actual));



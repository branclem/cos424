
source('data_prep.R');
source('validation.R');

data <- get.data.numeric();

# test
fit.glm<-glm(C1~A1+A2+A3+A5+D3A+D3B+F5+G14+G15, poisson, data, na.action=na.omit)
summary(fit.glm)

#exp(predict(fit.glm, newdata=data[1:N,]));

# Run it on this stuff.
print(nrow(data));
N <- 3000;
actual <- data$C1[1:N];
pred <- cross.validate(5,
           data[1:N,],
           function(data) { return(glm(C1~A1+A2+A3+A5+D3A+D3B+F5+G14+G15, poisson, data=data)) },
	     function(model, new.data) {return(predict(model, newdata=new.data)) } );
pred <- exp(pred);

rounded.pred <- round(pred);
rounded.pred[rounded.pred == 0] = 1;
rounded.pred[rounded.pred > 6] = 6;

# constrain predictions to be within valid range.
floor.pred <- floor(pred);
floor.pred[floor.pred == 0] = 1;
floor.pred[floor.pred > 6] = 6;

cat('Base predictions: ', mean(abs(pred - actual)), '\n');
cat('Rounded predictions: ', mean(abs(rounded.pred - actual)), '\n');
cat('Floored predictions: ', mean(abs(floor.pred - actual)), '\n');
cat('All 1: ', mean(abs(rep(1, N) - actual)), '\n');



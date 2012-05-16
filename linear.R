
library(MASS);
library(rms);
library(ggplot2);

source('data_prep.R');
source('validation.R');

test.glm <- function(data, formula, family, floor=T, folds=NULL) {

    cat('Running glm -', family$family, '- on', nrow(data), 'rows of data.\n');
    N <- nrow(data);
    actual <- data$C1[1:N];
    pred <- cross.validate(5,
               data[1:N,],
               function(data) { return(glm(formula, family, data=data)) },
	         function(model, new.data) {return(predict(model, newdata=new.data)) },
		   folds=folds,
               verbose=T );
    pred <- family$linkinv(pred);

    # Try different rounding modes to predict integer values.
    rounded.pred <- round(pred);
    rounded.pred[rounded.pred == 0] = 1;
    rounded.pred[rounded.pred > 6] = 6;

    floor.pred <- floor(pred);
    floor.pred[floor.pred <= 0] = 1;
    floor.pred[floor.pred > 6] = 6;

    cat('Base predictions: avg offset =', mean(abs(pred - actual)), '\n');
    cat('Rounded predictions: avg offset =', mean(abs(rounded.pred - actual)), ',', 100*length(which(rounded.pred == actual)) / length(actual), '%\n');
    cat('Floored predictions: avg offset =', mean(abs(floor.pred - actual)), ',', 100*length(which(floor.pred == actual)) / length(actual), '%\n');
    cat('All 1: avg offset =', mean(abs(rep(1, N) - actual)), ',', 100*length(which(1 == actual)) / length(actual), '%\n');

    if(floor) {
	    r <- list();
	    r$dev <- mean(abs(floor.pred - actual));
	    r$pct <- 100*length(which(floor.pred == actual)) / length(actual);
	    return(r);
    }
    else {
	    r <- list();
	    r$dev <- mean(abs(rounded.pred - actual));
	    r$pct <- 100*length(which(rounded.pred == actual)) / length(actual);
	    return(r);
    }

}

test.step.glm <- function(data, formula, family, folds=NULL) {

    cat('Stepping glm -', family$family, '- on', nrow(data), 'rows of data.\n');
    N <- nrow(data);
    actual <- data$C1[1:N];
    pred <- cross.validate(5,
               data[1:N,],
               function(data) { return(step(glm(C1~A2, family, data=data), scope=formula, k=log(nrow(data)))) },
	         function(model, new.data) {return(predict(model, newdata=new.data)) },
               folds=folds,
               verbose=T );
    pred <- family$linkinv(pred);

    # Try different rounding modes to predict integer values.
    rounded.pred <- round(pred);
    rounded.pred[rounded.pred == 0] = 1;
    rounded.pred[rounded.pred > 6] = 6;

    floor.pred <- floor(pred);
    floor.pred[floor.pred <= 0] = 1;
    floor.pred[floor.pred > 6] = 6;

    cat('Base predictions: avg offset =', mean(abs(pred - actual)), '\n');
    cat('Rounded predictions: avg offset =', mean(abs(rounded.pred - actual)), ',', 100*length(which(rounded.pred == actual)) / length(actual), '%\n');
    cat('Floored predictions: avg offset =', mean(abs(floor.pred - actual)), ',', 100*length(which(floor.pred == actual)) / length(actual), '%\n');
    cat('All 1: avg offset =', mean(abs(rep(1, N) - actual)), ',', 100*length(which(1 == actual)) / length(actual), '%\n');

    r <- list();
    r$dev <- mean(abs(floor.pred - actual));
    r$pct <- 100*length(which(floor.pred == actual)) / length(actual);
    return(r);

}

test.glmnet <- function(data, formula, family, folds=NULL) {

    cat('Running glmnet -', family, '- on', nrow(data), 'rows of data.\n');
    N <- nrow(data);
    actual <- data$C1[1:N];
    pred <- cross.validate(5,
               data[1:N,],
               function(data) {
			mm <- model.matrix(formula, data=data);
		      fit.glmnet <- glmnet(x=mm, y=data$C1, family);
			pred <- predict(fit.glmnet,newx=mm);
			wm <- which.min(colMeans(abs(floor(pred - data$C1))));
			s <- fit.glmnet$lambda[wm];
			s <- 0.2;
			return(glmnet(x=mm, y=data$C1, family, lambda=s))
		   },
	         function(model, new.data) {
			mm <- model.matrix(formula, data=new.data);
			return(predict(model, newx=mm));
		   },
               folds=folds,
               verbose=T );
#    pred <- family$linkinv(pred);

	pred;

    # Try different rounding modes to predict integer values.
    rounded.pred <- round(pred);
    rounded.pred[rounded.pred == 0] = 1;
    rounded.pred[rounded.pred > 6] = 6;

    floor.pred <- floor(pred);
    floor.pred[floor.pred <= 0] = 1;
    floor.pred[floor.pred > 6] = 6;

    cat('Base predictions: avg offset =', mean(abs(pred - actual)), '\n');
    cat('Rounded predictions: avg offset =', mean(abs(rounded.pred - actual)), ',', 100*length(which(rounded.pred == actual)) / length(actual), '%\n');
    cat('Floored predictions: avg offset =', mean(abs(floor.pred - actual)), ',', 100*length(which(floor.pred == actual)) / length(actual), '%\n');
    cat('All 1: avg offset =', mean(abs(rep(1, N) - actual)), ',', 100*length(which(1 == actual)) / length(actual), '%\n');

    r <- list();
    r$dev <- mean(abs(floor.pred - actual));
    r$pct <- 100*length(which(floor.pred == actual)) / length(actual);
    return(r);
}

test.polr <- function(data, formula, folds=NULL) {

    data$C1 <- as.factor(data$C1);

    # Run it on this stuff.
    cat('Running polr on', nrow(data), 'rows of data.\n');    N <- nrow(data);
    N <- nrow(data);
    actual <- as.numeric(data$C1[1:N]);
    pred <- cross.validate(5,
               data[1:N,],
               function(data) { return(polr(formula, data=data, method='logistic')) },
               function(model, new.data) {return(predict(model, newdata=new.data)) },
               folds=folds,
               verbose=F );
    pred <- as.numeric(pred);

    cat('Predictions: avg offset =', mean(abs(pred - actual)), ',', 100*length(which(pred == actual)) / length(actual), '%\n');
    cat('All 1: avg offset =', mean(abs(rep(1, N) - actual)), ',', 100*length(which(1 == actual)) / length(actual), '%\n');

    data$C1 <- as.numeric(data$C1);

    r <- list();
    r$dev <- mean(abs(pred - actual));
    r$pct <- 100*length(which(pred == actual)) / length(actual);
    return(r);

}

test.lrm <- function(data, formula, folds=NULL) {

    data$C1 <- as.factor(data$C1);

    # Run it on this stuff.
    cat('Running lrm on', nrow(data), 'rows of data.\n');    N <- nrow(data);
    N <- nrow(data);
    actual <- as.numeric(data$C1[1:N]);
    pred <- cross.validate(5,
               data[1:N,],
               function(data) { return(lrm(formula, data=data)) },
               function(model, new.data) {return(predict(model, newdata=new.data, type="mean")) },
               folds=folds,
               verbose=T );
    cat(pred, '\n');
    pred <- as.numeric(pred);

    rounded.pred <- round(pred);
    rounded.pred[rounded.pred == 0] = 1;
    rounded.pred[rounded.pred > 6] = 6;

    floor.pred <- floor(pred);
    floor.pred[floor.pred <= 0] = 1;
    floor.pred[floor.pred > 6] = 6;

    cat('Base predictions: avg offset =', mean(abs(pred - actual)), '\n');
    cat('Rounded predictions: avg offset =', mean(abs(rounded.pred - actual)), ',', 100*length(which(rounded.pred == actual)) / length(actual), '%\n');
    cat('Floored predictions: avg offset =', mean(abs(floor.pred - actual)), ',', 100*length(which(floor.pred == actual)) / length(actual), '%\n');
    cat('All 1: avg offset =', mean(abs(rep(1, N) - actual)), ',', 100*length(which(1 == actual)) / length(actual), '%\n');

    r <- list();
    r$dev <- mean(abs(floor.pred - actual));
    r$pct <- 100*length(which(floor.pred == actual)) / length(actual);
    return(r);

}

test.lrm.mle <- function(data, formula, folds=NULL) {

    data$C1 <- as.factor(data$C1);

    # Run it on this stuff.
    cat('Running lrm on', nrow(data), 'rows of data.\n');
    N <- nrow(data);
    actual <- as.numeric(data$C1[1:N]);
    pred <- cross.validate(5,
               data[1:N,],
               function(data) { return(lrm(formula, data=data)) },
               function(model, new.data) {
			p <- predict(model, newdata=new.data, type="fitted.ind");
			return(apply(p, 1, which.max));
		   },
               folds=folds,
               verbose=T );
    pred <- as.numeric(pred);

    cat('Base predictions: avg offset =', mean(abs(pred - actual)), '\n');
    cat('All 1: avg offset =', mean(abs(rep(1, N) - actual)), ',', 100*length(which(1 == actual)) / length(actual), '%\n');

    r <- list();
    r$dev <- mean(abs(pred - actual));
    r$pct <- 100*length(which(pred == actual)) / length(actual);
    return(r);

}

build.regularized.plot <- function() {

	library(glmnet);

	train <- data[1:6000,];
	test <- data[6001:8965,];
	form <- formula4;

	mm <- model.matrix(form, data=train);
	jim <- seq(from=0.0001, to=1, by=0.0001);
	fit.glmnet <- glmnet(x=mm, y=train$C1, family='poisson');

	coef(fit.glmnet, s=0.01) # extract coefficients at a single value of lambda

	mm <- model.matrix(form, data=test);
	pred <- predict(fit.glmnet,newx=mm);
	pred <- exp(pred);
	round.pred <- round(pred);
	round.pred[round.pred <= 0] <- 1;
	round.pred[round.pred > 6] <- 6;
	floor.pred <- floor(pred);
	floor.pred[floor.pred <= 0] <- 1;
	floor.pred[floor.pred > 6] <- 6;

	plot(x=fit.glmnet$lambda, y=colMeans(abs(floor(pred) - test$C1)), ylim=c(0,1), type="l");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(round(pred) - test$C1)), col="red");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(pred - test$C1)), col="green");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(floor.pred - test$C1)), ylim=c(0,1), col="grey");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(round.pred - test$C1)), col="pink");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(pred - test$C1)), col="yellow");

	pred <- as.vector(predict(fit.glmnet, newx=mm, s=0.2));

	fit.glm <- glm(form, data=train, family=poisson());
	pred2 <- as.vector(predict(fit.glm, newdata=test));
	pred2 <- exp(pred2);
	floor.pred2 <- floor(pred2);
	floor.pred2[floor.pred2 <= 0] <- 1;
	floor.pred2[floor.pred2 > 6] <- 6;
	lines(x=fit.glmnet$lambda, y=rep(mean(abs(floor.pred2 - test$C1)), length(fit.glmnet$lambda)), col="orange");
	lines(x=fit.glmnet$lambda, y=rep(mean(abs(pred2 - test$C1)), length(fit.glmnet$lambda)), col="brown");
}

# THINGS TO TRY:
# 1) Interaction Terms - takes too long to run for all, not very helpful!
# 2) Regularized Regression (glmnet) - interacts poorly with flooring, which
#    produces bad results.
# 3) AIC (step) - doesn't seem to help much... same results as full model
# 4) Model all terms as ordered factors? - not a big change

# To-do:
# - Generate results for levels 1-5 with each algorithm
#	- Gaussian linear regression
#	- Poisson linear regression
#	- Ordered logistic (use MLE, not mean)
#
# - floor linear regression for <= 4, round for 5
#
# - do each of these with inputs as numbers and factors
# - have them all use the same exact folds
#
# - Overlay a line for constant case?
# - show %s in another graph
#
# - things that did not work well
#   - regularized linear regression (poor flooring interaction)
#   - AIC
#   - interaction terms

######################
# Generate results
######################

# Break the variables into categories.
demographic <- 'A1+A2+A3+A4+A5+A6+A7A+A7B+A7C+A7D+G1+G2+G3A+G3B+G3C+G3D+G3E+G4';
background <- 'G5A+G5B+G6+G7+G8+G12A+G13+G14+G15+G16+G17+G18';
student.life <- 'A8A+A8B+A8C+A8D+A8E+A8F+A8G+A8H+A8I+F1+F2+F3+F4+F5+F6A+F6B+F6C+F6D+F6E+F6F+F6G+F6H+F6I+F7+F8';
personal <- 'E1A+E1B+E1C+E1D+E1E+E1F+E1G+E1H+E1I+E1J+E1K+E1L+E1M+E1N+E1O+E1P+E1Q+E1R+E1S+E1T+E2A+E2B+E2C+E2D+E2E+E2F+E2G+E2H+E2I+E3+E11+E12+E13+E14+E15A+E15B+E15C+E15D+E15E+E15F+E15G+E15H+E16+E17+E26+E27C+E27D+E27E+E28A+E28B+E29+E30';
policy.views <- 'B1+B2+B3+B4+B5+B6A+B6B+B6C+B6D+B6E+B6F+B6G+B7A+B7B+B7C+B7D+B7E+B8+B11+B12+B15A+B15B+B15C+B15D+B15E+B15F+B15G+B17A+B17B+B18A+B18B+B18C+B18D+B18E+B19+B20A+B20B+B20C+B20D+B21A+B21B+B21C+B21D+B21E+B21F+B21G+B21H+B21I+B22+B23A+B23B+B23C+B23D+B23E+B23F+B23G+B23H';
other.students <- 'D1A+D1B+D1C+D1D+D1E+D1F+D1G+D1H+D1I+D2A+D2B+D3A+D3B+D4A+D4B+D5A+D5B+D5C+D5D+D5E+D5F+D5G+D5H+D6+D7';
personal.alcohol.related <- 'E23+E24+E25+E27A+E27B+E28C+G9+G10+G11';
alcohol.use <- 'C2+C7+C8+C9+C10+C17A+C17B+C17C+C17D+C17E+C17F+C17G+C17H+C17I+C17J+C17K+C17L+C18A+C18B+C18C+C18D+C18E+C18F+C19A+C19B+C19C+C19D+C19E+C19F+C19G+C19H+C20A+C20B+C20C+C20D+C20E+C20F+C20G+C20H+C20I+C20J+C20K+C20L+C20M+C21+C22A+C22B+C22C+C22D+C22E+C22F+C22G+C22H+C22I+C22J+C22K+C22L+C22M+C22N+C22O+C22P+C22Q+C22R+C22S';

# Construct formulae based on these categories.
formula1 = as.formula(paste('C1~', demographic, '+',
                                   background));
formula2 = as.formula(paste('C1~', demographic, '+',
                                   background, '+',
                                   student.life, '+',
                                   personal));
formula3 = as.formula(paste('C1~', demographic, '+',
                                   background, '+',
                                   student.life, '+',
                                   personal, '+',
                                   policy.views, '+',
                                   other.students));
formula4 = as.formula(paste('C1~', demographic, '+',
                                   background, '+',
                                   student.life, '+',
                                   personal, '+',
                                   policy.views, '+',
                                   other.students, '+',
                                   personal.alcohol.related));
formula5 = as.formula(paste('C1~', demographic, '+',
                                   background, '+',
                                   student.life, '+',
                                   personal, '+',
                                   policy.views, '+',
                                   other.students, '+',
                                   personal.alcohol.related, '+',
					     alcohol.use));


# Read in the numerical data.
data <- get.data.numeric();

# Generate a fixed set of folds that everyone will use.
folds <- sample(rep(1:5, length=nrow(data)));

options(stringsAsFactors = FALSE);
results <- data.frame(Algorithm=character(40), 'Features Used'=character(40), 'Input Mode'=character(40), Error=numeric(40), pct=numeric(40), stringsAsFactors=F);

N <- nrow(data);
pred <- rep(1, N);
actual <- as.numeric(data$C1[1:N]);
baseline <- mean(abs(pred - actual));
base.pct <- 100*length(which(pred == actual)) / length(actual);
results[1,] <- c('Constant', 'Feature Class 1', 'Numeric', baseline, base.pct);
results[2,] <- c('Constant', 'Feature Class <= 2', 'Numeric', baseline, base.pct);
results[3,] <- c('Constant', 'Feature Class <= 3', 'Numeric', baseline, base.pct);
results[4,] <- c('Constant', 'Feature Class <= 4', 'Numeric', baseline, base.pct);
results[5,] <- c('Constant', 'Feature Class <= 5', 'Numeric', baseline, base.pct);

cat('Category 1\n');
results[6,] <- c('GLM - Gaussian', 'Feature Class 1', 'Numeric', test.glm(data, formula1, gaussian(), folds=folds));
results[7,] <- c('GLM - Poisson', 'Feature Class 1', 'Numeric', test.glm(data, formula1, poisson(), folds=folds));
results[8,] <- c('Ordered Logistic', 'Feature Class 1', 'Numeric', test.lrm.mle(data, formula1, folds=folds));

cat('Categories 1, 2\n');
results[9,] <- c('GLM - Gaussian', 'Feature Class <= 2', 'Numeric', test.glm(data, formula2, gaussian(), folds=folds));
results[10,] <- c('GLM - Poisson', 'Feature Class <= 2', 'Numeric', test.glm(data, formula2, poisson(), folds=folds));
results[11,] <- c('Ordered Logistic', 'Feature Class <= 2', 'Numeric', test.lrm.mle(data, formula2, folds=folds));

cat('Categories 1, 2, 3\n');
results[12,] <- c('GLM - Gaussian', 'Feature Class <= 3', 'Numeric', test.glm(data, formula3, gaussian(), folds=folds));
results[13,] <- c('GLM - Poisson', 'Feature Class <= 3', 'Numeric', test.glm(data, formula3, poisson(), folds=folds));
results[14,] <- c('Ordered Logistic', 'Feature Class <= 3', 'Numeric', test.lrm.mle(data, formula3, folds=folds));

cat('Categories 1, 2, 3, 4\n');
results[15,] <- c('GLM - Gaussian', 'Feature Class <= 4', 'Numeric', test.glm(data, formula4, gaussian(), folds=folds));
results[16,] <- c('GLM - Poisson', 'Feature Class <= 4', 'Numeric', test.glm(data, formula4, poisson(), folds=folds));
results[17,] <- c('Ordered Logistic', 'Feature Class <= 4', 'Numeric', test.lrm.mle(data, formula4, folds=folds));

cat('Categories 1, 2, 3, 4, 5\n');
results[18,] <- c('GLM - Gaussian', 'Feature Class <= 5', 'Numeric', test.glm(data, formula5, gaussian(), folds=folds, floor=F));
results[19,] <- c('GLM - Poisson', 'Feature Class <= 5', 'Numeric', test.glm(data, formula5, poisson(), folds=folds, floor=F));
results[20,] <- c('Ordered Logistic', 'Feature Class <= 5', 'Numeric', test.lrm.mle(data, formula5, folds=folds));

# Now get the data as factors.
data.f <- get.data.factor(data);

pred <- rep(1, N);
actual <- as.numeric(data$C1[1:N]);
baseline <- mean(abs(pred - actual));
base.pct <- 100*length(which(pred == actual)) / length(actual);
results[21,] <- c('Constant', 'Feature Class 1', 'Factor', baseline, base.pct);
results[22,] <- c('Constant', 'Feature Class <= 2', 'Factor', baseline, base.pct);
results[23,] <- c('Constant', 'Feature Class <= 3', 'Factor', baseline, base.pct);
results[24,] <- c('Constant', 'Feature Class <= 4', 'Factor', baseline, base.pct);
results[25,] <- c('Constant', 'Feature Class <= 5', 'Factor', baseline, base.pct);

cat('Category 1\n');
results[26,] <- c('GLM - Gaussian', 'Feature Class 1', 'Factor', test.glm(data.f, formula1, gaussian(), folds=folds));
results[27,] <- c('GLM - Poisson', 'Feature Class 1', 'Factor', test.glm(data.f, formula1, poisson(), folds=folds));
results[28,] <- c('Ordered Logistic', 'Feature Class 1', 'Factor', test.lrm.mle(data.f, formula1, folds=folds));

cat('Categories 1, 2\n');
results[29,] <- c('GLM - Gaussian', 'Feature Class <= 2', 'Factor', test.glm(data.f, formula2, gaussian(), folds=folds));
results[30,] <- c('GLM - Poisson', 'Feature Class <= 2', 'Factor', test.glm(data.f, formula2, poisson(), folds=folds));
results[31,] <- c('Ordered Logistic', 'Feature Class <= 2', 'Factor', test.lrm.mle(data.f, formula2, folds=folds));

cat('Categories 1, 2, 3\n');
results[32,] <- c('GLM - Gaussian', 'Feature Class <= 3', 'Factor', test.glm(data.f, formula3, gaussian(), folds=folds));
results[33,] <- c('GLM - Poisson', 'Feature Class <= 3', 'Factor', test.glm(data.f, formula3, poisson(), folds=folds));
results[34,] <- c('Ordered Logistic', 'Feature Class <= 3', 'Factor', test.lrm.mle(data.f, formula3, folds=folds));

cat('Categories 1, 2, 3, 4\n');
results[35,] <- c('GLM - Gaussian', 'Feature Class <= 4', 'Factor', test.glm(data.f, formula4, gaussian(), folds=folds));
results[36,] <- c('GLM - Poisson', 'Feature Class <= 4', 'Factor', test.glm(data.f, formula4, poisson(), folds=folds));
results[37,] <- c('Ordered Logistic', 'Feature Class <= 4', 'Factor', test.lrm.mle(data.f, formula4, folds=folds));

cat('Categories 1, 2, 3, 4, 5\n');
results[38,] <- c('GLM - Gaussian', 'Feature Class <= 5', 'Factor', test.glm(data.f, formula5, gaussian(), folds=folds, floor=F));
results[39,] <- c('GLM - Poisson', 'Feature Class <= 5', 'Factor', test.glm(data.f, formula5, poisson(), folds=folds, floor=F));
results[40,] <- c('Ordered Logistic', 'Feature Class <= 5', 'Factor', test.lrm.mle(data.f, formula5, folds=folds));


# All done!
results$Error <- as.numeric(results$Error);
results$pct <- as.numeric(results$pct);

ggplot(results[1:20,], aes(x=Features.Used, y=Error, fill=Features.Used)) +
	geom_bar() + facet_grid(. ~ Algorithm) +
	scale_y_continuous(name='Mean Error') +
	scale_x_discrete(name=NA) +
	scale_fill_discrete(name='Features Used') +
	opts(axis.text.x = theme_blank(), axis.title.x = theme_blank(), axis.ticks = theme_blank());


ggplot(results[1:20,], aes(x=Features.Used, y=pct, fill=Features.Used)) +
	geom_bar() + facet_grid(. ~ Algorithm) +
	scale_y_continuous(name='Percentage Accuracy') +
	scale_x_discrete(name=NA) +
#	scale_fill_discrete(name='Features Used') +
	opts(legend.position="none", axis.text.x = theme_blank(), axis.title.x = theme_blank(), axis.ticks = theme_blank());


data <- get.data.numeric();
validation.set <- get.validation.numeric(data);

lrm.fit <- lrm(formula5, data=data);
p <- predict(lrm.fit, newdata=validation.set, type="fitted.ind");
pred <- apply(p, 1, which.max);
actual <- validation.set$C1;

dev <- mean(abs(pred - actual));
pct <- 100*length(which(pred == actual)) / length(actual);
cat(pct, '%, mean =', dev, '\n');



library(MASS);

source('data_prep.R');
source('validation.R');

data <- get.data.numeric();

test.glm <- function(data, formula, family) {

    # Fit on the whole dataset just as a sanity check
#    fit.glm <- glm(formula, poisson, data);
#    summary(fit.glm);

    cat('Running glm -', family$family, '- on', nrow(data), 'rows of data.\n');
    N <- nrow(data);
    actual <- data$C1[1:N];
    pred <- cross.validate(5,
               data[1:N,],
               function(data) { return(glm(formula, family, data=data)) },
	         function(model, new.data) {return(predict(model, newdata=new.data)) },
               verbose=F );
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

}

test.polr <- function(data, formula) {

    data$C1 <- as.factor(data$C1);

    # Sanity check
#    fit.polr <- polr(formula, data=data);
#    summary(fit.polr);

    # Run it on this stuff.
    cat('Running polr on', nrow(data), 'rows of data.\n');    N <- nrow(data);
    actual <- as.numeric(data$C1[1:N]);
    pred <- cross.validate(5,
               data[1:N,],
               function(data) { return(polr(formula, data=data)) },
               function(model, new.data) {return(predict(model, newdata=new.data)) },
               verbose=F );
    pred <- as.numeric(pred);

    cat('Predictions: avg offset =', mean(abs(pred - actual)), ',', 100*length(which(pred == actual)) / length(actual), '%\n');
    cat('All 1: avg offset =', mean(abs(rep(1, N) - actual)), ',', 100*length(which(1 == actual)) / length(actual), '%\n');

    data$C1 <- as.numeric(data$C1);

}

formula = C1~A1+A2+A3+A4+A5+A6+A7A+A7B+A7C+A7D+A8A+A8B+A8C+A8D+A8E+A8F+A8G+A8H+A8I+B1+B2+B3+B4+B5+B6A+B6B+B6C+B6D+B6E+B6F+B6G+B7A+B7B+B7C+B7D+B7E+B8+B11+B12+B15A+B15B+B15C+B15D+B15E+B15F+B15G+B17A+B17B+B18A+B18B+B18C+B18D+B18E+B19+B20A+B20B+B20C+B20D+B21A+B21B+B21C+B21D+B21E+B21F+B21G+B21H+B21I+B22+B23A+B23B+B23C+B23D+B23E+B23F+B23G+B23H+D1A+D1B+D1C+D1D+D1E+D1F+D1G+D1H+D1I+D2A+D2B+D3A+D3B+D4A+D4B+D5A+D5B+D5C+D5D+D5E+D5F+D5G+D5H+D6+D7+E1A+E1B+E1C+E1D+E1E+E1F+E1G+E1H+E1I+E1J+E1K+E1L+E1M+E1N+E1O+E1P+E1Q+E1R+E1S+E1T+E2A+E2B+E3+E11+E12+E13+E14+E15A+E15B+E15C+E15D+E15E+E15F+E15G+E15H+E16+E17+E23+E24+E25+E26+E27A+E27B+E27C+E27D+E27E+E28A+E28B+E28C+E29+E30+F1+F2+F3+F4+F5+F6A+F6B+F6C+F6D+F6E+F6F+F6G+F6H+F6I+F7+F8+G1+G2+G3A+G3B+G3C+G3D+G3E+G4+G5A+G5B+G6+G7+G8+G13+G14+G15+G16+G17+G18;
test.glm(data, formula, gaussian());
test.glm(data, formula, poisson());
test.polr(data, formula);


#fit.glm.bare <- glm(C1~A1, poisson, data, na.action=na.omit);
#fit.glm.stepped <- step(fit.glm.bare, scope=C1~(A1+A2+A3+A4+A5+A6+A7A+A7B+A7C+A7D+D3A+D3B+F1+F2+F3+F4+F5+F6A+F6B+F6C+F6D+F6E+F6F+F6G+F6H+F6I+F7+F8+G14+G15)^2);
#summary(fit.glm);
#summary(fit.glm.stepped);


library(MASS);
library(ggplot2);

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

    return(mean(abs(floor.pred - actual)));

}

test.polr <- function(data, formula) {

    data$C1 <- as.factor(data$C1);

    # Sanity check
#    fit.polr <- polr(formula, data=data);
#    summary(fit.polr);

    # Run it on this stuff.
    cat('Running polr on', nrow(data), 'rows of data.\n');    N <- nrow(data);
    N <- nrow(data);
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

    return(mean(abs(pred - actual)));

}

# Break the variables into categories.
demographic <- 'A1+A2+A3+A4+A5+A6+A7A+A7B+A7C+A7D+G1+G2+G3A+G3B+G3C+G3D+G3E+G4';
background <- 'G5A+G5B+G6+G7+G8+G12A+G13+G14+G15+G16+G17+G18';
student.life <- 'A8A+A8B+A8C+A8D+A8E+A8F+A8G+A8H+A8I+F1+F2+F3+F4+F5+F6A+F6B+F6C+F6D+F6E+F6F+F6G+F6H+F6I+F7+F8';
personal <- 'E1A+E1B+E1C+E1D+E1E+E1F+E1G+E1H+E1I+E1J+E1K+E1L+E1M+E1N+E1O+E1P+E1Q+E1R+E1S+E1T+E2A+E2B+E2C+E2D+E2E+E2F+E2G+E2H+E2I+E3+E11+E12+E13+E14+E15A+E15B+E15C+E15D+E15E+E15F+E15G+E15H+E16+E17+E26+E27C+E27D+E27E+E28A+E28B+E29+E30';
policy.views <- 'B1+B2+B3+B4+B5+B6A+B6B+B6C+B6D+B6E+B6F+B6G+B7A+B7B+B7C+B7D+B7E+B8+B11+B12+B15A+B15B+B15C+B15D+B15E+B15F+B15G+B17A+B17B+B18A+B18B+B18C+B18D+B18E+B19+B20A+B20B+B20C+B20D+B21A+B21B+B21C+B21D+B21E+B21F+B21G+B21H+B21I+B22+B23A+B23B+B23C+B23D+B23E+B23F+B23G+B23H';
other.students <- 'D1A+D1B+D1C+D1D+D1E+D1F+D1G+D1H+D1I+D2A+D2B+D3A+D3B+D4A+D4B+D5A+D5B+D5C+D5D+D5E+D5F+D5G+D5H+D6+D7';
personal.alcohol.related <- 'E23+E24+E25+E27A+E27B+E28C+G9+G10+G11';
alcohol.use <- 'C2+C7+C8+C9+C10+C17A+C17B+C17C+C17D+C17E+C17F+C17G+C17H+C17I+C17J+C17K+C17L+C18A+C18B+C18C+C18D+C18E+C18F+C19A+C19B+C19C+C19D+C19E+C19F+C19G+C19H+C20A+C20B+C20C+C20D+C20E+C20F+C20G+C20H+C20I+C20J+C20K+C20L+C20M+C21+C22A+C22B+C22C+C22D+C22E+C22F+C22G+C22H+C22I+C22J+C22K+C22L+C22M+C22N+C22O+C22P+C22Q+C22R+C22S';


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

    fit.glm <- glm(formula4, poisson, data);
    summary(fit.glm);

options(stringsAsFactors = FALSE);
results <- data.frame(Algorithm=character(16), 'Features Used'=character(16), Error=numeric(16), stringsAsFactors=F);

N <- nrow(data);
baseline <- mean(abs(rep(1, N) - as.numeric(data$C1[1:N])));
results[1,] <- c('Constant', 'Feature Class 1', baseline);
results[2,] <- c('Constant', 'Feature Class <= 2', baseline);
results[3,] <- c('Constant', 'Feature Class <= 3', baseline);
results[4,] <- c('Constant', 'Feature Class <= 4', baseline);

cat('Category 1\n');
results[5,] <- c('GLM - Gaussian', 'Feature Class 1', test.glm(data, formula1, gaussian()));
results[6,] <- c('GLM - Poisson', 'Feature Class 1', test.glm(data, formula1, poisson()));
results[7,] <- c('Ordered Logistic', 'Feature Class 1', test.polr(data, formula1));

cat('Categories 1, 2\n');
results[8,] <- c('GLM - Gaussian', 'Feature Class <= 2', test.glm(data, formula2, gaussian()));
results[9,] <- c('GLM - Poisson', 'Feature Class <= 2', test.glm(data, formula2, poisson()));
results[10,] <- c('Ordered Logistic', 'Feature Class <= 2', test.polr(data, formula2));

cat('Categories 1, 2, 3\n');
results[11,] <- c('GLM - Gaussian', 'Feature Class <= 3', test.glm(data, formula3, gaussian()));
results[12,] <- c('GLM - Poisson', 'Feature Class <= 3', test.glm(data, formula3, poisson()));
results[13,] <- c('Ordered Logistic', 'Feature Class <= 3', test.polr(data, formula3));

cat('Categories 1, 2, 3, 4\n');
results[14,] <- c('GLM - Gaussian', 'Feature Class <= 4', test.glm(data, formula4, gaussian()));
results[15,] <- c('GLM - Poisson', 'Feature Class <= 4', test.glm(data, formula4, poisson()));
results[16,] <- c('Ordered Logistic', 'Feature Class <= 4', test.polr(data, formula4));

results$Error <- as.numeric(results$Error);

ggplot(results, aes(x=Features.Used, y=Error, fill=Features.Used)) +
	geom_bar() + facet_grid(. ~ Algorithm) +
	scale_y_continuous(name='Mean Error') +
	scale_x_discrete(name=NA) +
	scale_fill_discrete(name='Features Used') +
	opts(title="Predictive Error for GLM and Ordered Logistic Regression", axis.text.x = theme_blank(), axis.title.x = theme_blank(), axis.ticks = theme_blank());

#fit.glm.bare <- glm(C1~A1, poisson, data, na.action=na.omit);
#fit.glm.stepped <- step(fit.glm.bare, scope=C1~(A1+A2+A3+A4+A5+A6+A7A+A7B+A7C+A7D+D3A+D3B+F1+F2+F3+F4+F5+F6A+F6B+F6C+F6D+F6E+F6F+F6G+F6H+F6I+F7+F8+G14+G15)^2);
#summary(fit.glm);
#summary(fit.glm.stepped);


# This is a revised version of Section 6.7.1 of "Introduction to Statistical 
# Learning" by James et al. In this version, the number of PC components that
# CV chooses is pulled out programmatically, rather than being determined
# by visual inspection of the validation plot.

# load data
library(ISLR)
data(Hitters)
x = model.matrix(Salary ~ ., Hitters)[, -1]
y = Hitters$Salary

# perform PC regression with CV
library(pls)
set.seed(2)
pcr.fit = pcr(Salary ~ ., data = Hitters, scale = TRUE, 
              validation = "CV")
summary(pcr.fit)

# get indices for training set
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

# perform PC regression with CV on just the training set
set.seed(1)
pcr.fit = pcr(Salary ~ ., data = Hitters, subset = train, 
              scale = TRUE, validation = "CV")

# plot the CV error
validationplot(pcr.fit, val.type = "MSEP")


cverr <- MSEP(pcrfit)$val[2,,]
imin <- which.min(cverr)
if (imin > 1) {
    predictions <- c(predict(pcrfit, data.frame(X_test_mat), ncomp = imin - 1))
    MSE_mat[i, 2 + length(rat) + 3] <- MSE(predictions, signal_test)
    support_mat[i, 2 + length(rat) + 3] <- p
} else {
    MSE_mat[i, 2 + length(rat) + 3] <- MSE_mat[i, 1]
    support_mat[i, 2 + length(rat) + 3] <- 0
}
}

#' Build Logistic Regression Model
#'
#' This function loads a clean dataset as a matrix. It generates a set seed to randomly sample the data (without replacement)
#' to split said data into a train and prediction subsample. It builds a logistic regression model based upon the trained subsample.
#'
#' @param inputfile Path to the input file
#' @return A logistic regression model (glm) of the input file
#' @export
buildmodel <- function(inputfile) {
  file_csv <- read.csv(inputfile)
  set.seed(1335)
  nr <- nrow(file_csv)
  index <- sample(1:nr, size=ceiling(length(file_csv$y)/(100/99.99)),replace=FALSE)
  data.train <- file_csv[index,]
  data.pred <- file_csv[-index,]

  #NOTE: DIDN'T INCLUDE YFAC OR YNUM IN CLEAN DATA
  logitmodel <- glm(as.factor(y) ~ (.),
                    data=data.train, family = binomial("logit"),
                    control = list(maxit = 50))
  logitmodel
}


#' Build Predicted Intervals
#'
#' This builds a predicted interval based on the glm of our trained dataset.
#'
#' @param inputfile Path to the input file
#' @return A logistic regression model (glm) of the input file
#' @export
pred_CI <- function(inputfile) {
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

  pred <- predict(logitmodel, newdata=bank.pred, type = "response", se.fit = TRUE)

  fit = pred$fit
  upr = pred$fit + 1.96 * pred$se.fit
  lwr = pred$fit - 1.96 * pred$se.fit

  predictedint <- as.data.frame(cbind(fit, lwr, upr))
  predictedint
}

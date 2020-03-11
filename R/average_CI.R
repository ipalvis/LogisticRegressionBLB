#' Confidence Interval Averages
#'
#' Finds 20 sets of average confidence intervals based upon the trained subsample of data.
#'
#' @param inputfile Path to the input file
#'
#' @return A list of 20 average confidence intervals based upon the trained subsample
#' @export
average_CI <- function(inputfile) {
  cl = parallel:makeCluster(4)
  B = 10
  singleBoots<-function(i){
    # factor y
    bank$yfac = as.factor(bank$y)
    # numeric y
    bank$ynum = ifelse(bank$y == "yes", 1, 0)
    index <- sample(x = seq_len(n), size = n, replace = TRUE)
    bank.train <- bank[index,]
    # parameter estimation
    calc_ci(bank.train)
  }
  parallel::clusterEvalQ(cl, {
    #library(tidyverse)
    #library(scales)
    alpha = 0.05
    n = 1e3
    bank = read.csv("bank-additional.csv", sep=";")

    calc_ci<-function(bank){
      logitmodel<-glm(as.factor(y) ~ (.),
                      data=bank, family = binomial("logit"),
                      control = list(maxit = 50))
      gbenf = length(summary(logitmodel)$coefficients[,1]) # g value
      res = benfmult = qt(1-alpha/gbenf, nrow(bank)-(gbenf-1))
      resList = summary(logitmodel)$coefficients %>%
        as.data.frame() %>%
        select(Estimate, Std.Error = "Std. Error") %>%
        rownames_to_column("Variable") %>%
        mutate(CI.Lwr = Estimate - Std.Error*benfmult,
               CI.Upr = Estimate + Std.Error*benfmult) %>%
        mutate(Is.Zero = 0  > CI.Lwr & 0 < CI.Upr)%>% as.data.frame()
      data.frame(lower=resList$CI.Lwr , upper=resList$CI.Upr)
    }
  })
  cis = parallel::parSapply(cl, seq_len(B),singleBoots)
  parallel::stopCluster(cl)
  cis
}


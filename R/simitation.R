#' analyze.simstudy.chisq.test.gf
#'
#' @description This function analyzes the results of a simulated chi-squared test of goodness of fit.
#'
#' @param test.statistics.chisq.test.gf A list containing summary information
#' for fitting chi squared tests of goodness of fit. The structure is in the form
#' returned by the function \code{simitation::sim.chisq.test.gf()}.
#' @param conf.level A numeric value between 0 and 1 representing the confidence
#' level (1 - significance level). Default is 0.95.
#' @param the.quantiles A numeric vector of values between 0 and 1. Summary
#' statistics to analyze the tests will return the specified quantiles. Default
#' values are c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975).
#' @return A list containing the following elements:
#' \itemize{
#'   \item stat.summary: Summary statistics for the test statistics.
#'   \item p.value.summary: Proportions of tests that rejected and did not
#'   reject the null hypothesis.
#' }
#' @export

analyze.simstudy.chisq.test.gf <- function(test.statistics.chisq.test.gf, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  stat.summary <- internal.quantiles.mean.sd(x = test.statistics.chisq.test.gf$statistic, the.quantiles = the.quantiles)

  p.value.summary <- data.table::data.table(
    reject.proportion = mean(test.statistics.chisq.test.gf$p.value < 1 - conf.level),
    non.reject.proportion = mean(test.statistics.chisq.test.gf$p.value >= 1 - conf.level)
  )

  res <- list(stat.summary = stat.summary, p.value.summary = p.value.summary)

  return(res)
}


#' sim.chisq.test.gf
#'
#' @description Perform a chi-squared test of goodness of fit across one or more experiments.
#'
#' @param simdat.chisq.gf Data for use in chi squared tests of goodness of fit across one or more experiments. The structure should be in the form returned by the function simitation::sim.chisq.gf().
#' @param hypothesized.probs A vector of hypothesized probabilities corresponding to the values in the column specified by value.name. If the values include c("B", "A", "C"), then a probability vector of c(0.5, 0.3, 0.2) would associate a value of 0.5 with "A", 0.3 with "B", and 0.2 with "C".
#' @param correct Logical. For details, refer to the chisq.test documentation.
#' @param experiment.name A character value providing the name of the column identifying the experiment.
#' @param value.name A character value providing the name of the column identifying the values.
#' @return A data.table or data.frame with the results of the chi-squared tests.
#' @export

sim.chisq.test.gf <- function(simdat.chisq.gf, hypothesized.probs = NULL, correct = TRUE, experiment.name = "experiment", value.name = "x") {

  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }

  is.dt <- data.table::is.data.table(simdat.chisq.gf)

  data.table::setDT(simdat.chisq.gf)

  res <- simdat.chisq.gf[, internal.chisq.test.gf(x = get(value.name), hypothesized.probs = hypothesized.probs), by = experiment.name]

  if(!is.dt) {
    data.table::setDF(simdat.chisq.gf)
    data.table::setDF(res)
  }

  return(res)
}




#' analyze.simstudy.chisq.test.ind
#'
#' @description This function analyzes the results of a simulated chi-squared test of independence.
#'
#' @param test.statistics.chisq.test.ind A list containing summary information
#' for fitting chi squared tests of independence. The structure is in the form
#' returned by the function \code{simitation::sim.chisq.test.ind()}.
#' @param conf.level A numeric value between 0 and 1 representing the confidence
#' level (1 - significance level). Default is 0.95.
#' @param the.quantiles A numeric vector of values between 0 and 1. Summary
#' statistics to analyze the tests will return the specified quantiles. Default
#' values are c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975).
#' @return A list containing the following elements:
#' \itemize{
#'   \item stat.summary: Summary statistics for the test statistics.
#'   \item p.value.summary: Proportions of tests that rejected and did not
#'   reject the null hypothesis.
#' }
#' @export

analyze.simstudy.chisq.test.ind <- function(test.statistics.chisq.test.ind, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  stat.summary <- internal.quantiles.mean.sd(x = test.statistics.chisq.test.ind$statistic, the.quantiles = the.quantiles)

  p.value.summary <- data.table::data.table(
    reject.proportion = mean(test.statistics.chisq.test.ind$p.value < 1 - conf.level),
    non.reject.proportion = mean(test.statistics.chisq.test.ind$p.value >= 1 - conf.level)
  )

  res <- list(stat.summary = stat.summary, p.value.summary = p.value.summary)

  return(res)
}



#' Analyze Simulated Linear Regression Models
#'
#' @description This function analyzes the results of simulated linear regression models, providing
#' various summary statistics about the model coefficients, fit, and other aspects.
#'
#' @param the.coefs A data frame or data.table containing the summary table of estimated
#' coefficients from repeated linear regression models. It should be structured like the output
#' of \code{simitation::sim.statistics.lm$the.coefs()}.
#' @param summary.stats A data frame or data.table containing the summary statistics from
#' repeated linear regression models, similar to \code{simitation::sim.statistics.lm$summary.stats()}.
#' @param conf.level A numeric value for the confidence level (1 - significance level). Default is 0.95.
#' @param the.quantiles Numeric vector of quantile values for which statistics are required.
#' @param coef.name Column name in 'the.coefs' that has input variable names of the regression model.
#' @param estimate.name Column name in 'the.coefs' for estimated coefficients of the regression model.
#' @param lm.p.name Column name in 'the.coefs' for p-values of coefficient tests.
#' @param f.p.name Column name in 'summary.stats' for the F-test p-value.
#' @return A list with several summary statistics for the linear regression model.
#' @export
#'

analyze.simstudy.lm <- function(the.coefs, summary.stats, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), coef.name = "Coefficient", estimate.name = "Estimate", lm.p.name = "Pr(>|t|)", f.p.name = "f.pvalue"){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt.the.coefs <- data.table::is.data.table(the.coefs)
  data.table::setDT(the.coefs)

  is.dt.summary.stats <- data.table::is.data.table(summary.stats)
  data.table::setDT(summary.stats)

  lm.estimate.summary <- the.coefs[, internal.quantiles.mean.sd(x = get(estimate.name), the.quantiles = the.quantiles), by = coef.name]

  lm.p.summary <- the.coefs[, .(reject.proportion = mean(get(lm.p.name) < 1 - conf.level), non.reject.proportion = mean(get(lm.p.name) >= 1 - conf.level)), by = coef.name]

  sigma.summary <- data.table(stat = "sigma", internal.quantiles.mean.sd(x = summary.stats$sigma, the.quantiles = the.quantiles, na.rm = T))

  rse.summary <- data.table(stat = "rse", internal.quantiles.mean.sd(x = summary.stats$rse, the.quantiles = the.quantiles, na.rm = T))

  r.squared.summary <- data.table(stat = "r.squared", internal.quantiles.mean.sd(x = summary.stats$r.squared, the.quantiles = the.quantiles, na.rm = T))

  adj.r.squared.summary <- data.table(stat = "adj.r.squared", internal.quantiles.mean.sd(x = summary.stats$adj.r.squared, the.quantiles = the.quantiles, na.rm = T))

  fstatistic.summary <- data.table(stat = "fstatistic", internal.quantiles.mean.sd(x = summary.stats$fstatistic, the.quantiles = the.quantiles, na.rm = T))

  lm.stats.summary <- rbindlist(l = list(sigma.summary, rse.summary, r.squared.summary, adj.r.squared.summary, fstatistic.summary), fill = T)

  fstatistic.p.summary <- summary.stats[, .(reject.proportion = mean(get(f.p.name) < 1 - conf.level), non.reject.proportion = mean(get(f.p.name) >= 1 - conf.level))]


  res <- list(lm.estimate.summary = lm.estimate.summary, lm.p.summary = lm.p.summary, lm.stats.summary = lm.stats.summary, fstatistic.p.summary = fstatistic.p.summary)

  if(is.dt.the.coefs == F){
    data.table::setDF(the.coefs)
  }
  if(is.dt.summary.stats == F){
    data.table::setDF(summary.stats)
  }


  return(res)
}

#' Analyze Simulated Logistic Regression Models
#'
#' @description This function analyzes the results of simulated logistic regression models, providing
#' various summary statistics about the model coefficients, fit, and other aspects.
#'
#' @param the.coefs A data frame or data.table containing the summary table of estimated
#' coefficients from repeated logistic regression models. It should be structured like the output
#' of \code{simitation::sim.statistics.logistic$the.coefs()}.
#' @param  summary.stats  A data.frame or data.table object of the summary statistics of repeated logistic regression models.  Structure is in the form returned by the function simitation::sim.statistics.logistic$summary.stats().
#' @param conf.level  A numeric value between 0 and 1 representing the confidence level (1 - significance level).
#' @param  the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.
#' @param  coef.name  A character value specifying the column of the.coefs that contains the names of the input variables of the logistic regression model.
#' @param estimate.name  A character value specifying the column of the.coefs that contains the estimated coefficients of the logistic regression model.
#' @param logistic.p.name  A character value specifying the column of the.coefs that contains the p-values for the tests of the estimated coefficients of the logistic regression model.
#' @return A list with several summary statistics for the logistic regression model.
#' @export
#'

analyze.simstudy.logistic <- function(the.coefs, summary.stats, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), coef.name = "Coefficient", estimate.name = "Estimate", logistic.p.name = "Pr(>|z|)"){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  coef.copy <- data.table::copy(the.coefs)
  summary.stats.copy <- data.table::copy(summary.stats)

  data.table::setDT(coef.copy)
  data.table::setDT(summary.stats.copy)

  logistic.estimate.summary <- coef.copy[, internal.quantiles.mean.sd(x = get(estimate.name), the.quantiles = the.quantiles, na.rm = T), by = coef.name]

  logistic.p.summary <- coef.copy[, .(reject.proportion = mean(get(logistic.p.name) < 1 - conf.level), non.reject.proportion = mean(get(logistic.p.name) >= 1 - conf.level)), by = coef.name]


  deviance.summary <- data.table(stat = "deviance", internal.quantiles.mean.sd(x = summary.stats.copy$deviance, the.quantiles = the.quantiles, na.rm = T))

  aic.summary <- data.table(stat = "aic", internal.quantiles.mean.sd(x = summary.stats.copy$aic, the.quantiles = the.quantiles, na.rm = T))

  df.residual.summary <- data.table(stat = "df.residual", internal.quantiles.mean.sd(x = summary.stats.copy$df.residual, the.quantiles = the.quantiles, na.rm = T))

  null.deviance.summary <- data.table(stat = "null.deviance", internal.quantiles.mean.sd(x = summary.stats.copy$null.deviance, the.quantiles = the.quantiles, na.rm = T))

  df.null.summary <- data.table(stat = "df.null", internal.quantiles.mean.sd(x = summary.stats.copy$df.null, the.quantiles = the.quantiles, na.rm = T))

  iter.summary <- data.table(stat = "iter", internal.quantiles.mean.sd(x = summary.stats.copy$iter, the.quantiles = the.quantiles, na.rm = T))

  dispersion.summary <- data.table(stat = "dispersion", internal.quantiles.mean.sd(x = summary.stats.copy$dispersion, the.quantiles = the.quantiles, na.rm = T))

  logistic.stats.summary <- rbindlist(l = list(deviance.summary, aic.summary, df.residual.summary, null.deviance.summary, df.null.summary, iter.summary, dispersion.summary), fill = T)

  res <- list(logistic.estimate.summary = logistic.estimate.summary, logistic.p.summary = logistic.p.summary, logistic.stats.summary = logistic.stats.summary)

  return(res)
}

#' Analyze Simulated Proportion Tests
#'
#' @description This function analyzes the results of simulated tests for proportions, providing
#' various summary statistics about the test statistics, estimates, and confidence intervals.
#'
#' @param test.statistics.prop A data frame or data.table containing the summary table of
#' estimated coefficients from repeated proportion tests. Expected structure is similar to the output
#' of \code{simitation::sim.prop.test()}.
#' @param alternative A character string specifying the alternative hypothesis. Must be one of
#' "two.sided", "less", or "greater". Default is "two.sided".
#' @param conf.level A numeric value between 0 and 1 representing the confidence level. Default is 0.95.
#' @param the.quantiles A numeric vector of values between 0 and 1. The function will return the
#' specified quantiles for summary statistics.
#' @return A list containing various summary statistics for the proportion test.
#' @export
#'

analyze.simstudy.prop <- function(test.statistics.prop, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)){

  value.two.sided <- "two.sided"
  value.less <- "less"
  value.greater <- "greater"

  alternative <- alternative[1]

  if(!(alternative %in% c(value.less, value.greater))){
    alternative <- value.two.sided
  }

  stat.summary <- internal.quantiles.mean.sd(x = test.statistics.prop$statistic, the.quantiles = the.quantiles)

  estimate.summary <- internal.quantiles.mean.sd(x = test.statistics.prop$estimate, the.quantiles = the.quantiles)

  p.value.summary <- data.frame(reject.proportion = mean(test.statistics.prop$p.value < 1 - conf.level), non.reject.proportion = mean(test.statistics.prop$p.value >= 1 - conf.level))

  if(alternative == value.two.sided){

    ci.range.summary <- internal.quantiles.mean.sd(x = test.statistics.prop$upper.ci - test.statistics.prop$lower.ci, the.quantiles = the.quantiles)

    ci.proportion.above.null.summary <- internal.quantiles.mean.sd(x = (test.statistics.prop$upper.ci - test.statistics.prop$null.value) / (test.statistics.prop$upper.ci - test.statistics.prop$lower.ci), the.quantiles = the.quantiles)

    ci.proportion.below.null.summary <- internal.quantiles.mean.sd(x = (test.statistics.prop$null.value - test.statistics.prop$lower.ci) / (test.statistics.prop$upper.ci - test.statistics.prop$lower.ci), the.quantiles = the.quantiles)
  }

  if(alternative == value.greater){
    ci.limit.summary <- internal.quantiles.mean.sd(x = test.statistics.prop$lower.ci, the.quantiles = the.quantiles)
  }
  if(alternative == value.less){
    ci.limit.summary <- internal.quantiles.mean.sd(x = test.statistics.prop$upper.ci, the.quantiles = the.quantiles)
  }


  res <- list(estimate.summary = estimate.summary, stat.summary = stat.summary, p.value.summary = p.value.summary)

  if(alternative == value.two.sided){
    res$ci.range.summary <- ci.range.summary
    res$ci.proportion.above.null.summary <- ci.proportion.above.null.summary
    res$ci.proportion.below.null.summary <- ci.proportion.below.null.summary
  }

  if(alternative != value.two.sided){
    res$ci.limit.summary <- ci.limit.summary
  }

  return(res)

}

#' analyze.simstudy.prop2
#'
#' @description analyze.simstudy.prop2

#' @param test.statistics.prop2  Summary information for fitting two-sample tests of proportions.  Structure is in the form returned by the function simitation::sim.prop2.test().

#' @param  alternative  See help(prop.test).

#' @param  conf.level  See help(prop.test).

#' @param  the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.
#'
#' @export


analyze.simstudy.prop2 <- function(test.statistics.prop2, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)){

  value.two.sided <- "two.sided"
  value.less <- "less"
  value.greater <- "greater"

  alternative <- alternative[1]

  if(!(alternative %in% c(value.less, value.greater))){
    alternative <- value.two.sided
  }

  stat.summary <- internal.quantiles.mean.sd(x = test.statistics.prop2$statistic, the.quantiles = the.quantiles)

  estimate.summary <- internal.quantiles.mean.sd(x = test.statistics.prop2$estimate, the.quantiles = the.quantiles)

  p.value.summary <- data.frame(reject.proportion = mean(test.statistics.prop2$p.value < 1 - conf.level), non.reject.proportion = mean(test.statistics.prop2$p.value >= 1 - conf.level))

  df.summary <- internal.quantiles.mean.sd(x = test.statistics.prop2$df, the.quantiles = the.quantiles)

  if(alternative == value.two.sided){

    ci.range.summary <- internal.quantiles.mean.sd(x = test.statistics.prop2$upper.ci - test.statistics.prop2$lower.ci, the.quantiles = the.quantiles)

    ci.proportion.above.null.summary <- internal.quantiles.mean.sd(x = (test.statistics.prop2$upper.ci - test.statistics.prop2$null.value) / (test.statistics.prop2$upper.ci - test.statistics.prop2$lower.ci), the.quantiles = the.quantiles)

    ci.proportion.below.null.summary <- internal.quantiles.mean.sd(x = (test.statistics.prop2$null.value - test.statistics.prop2$lower.ci) / (test.statistics.prop2$upper.ci - test.statistics.prop2$lower.ci), the.quantiles = the.quantiles)
  }
  if(alternative == value.greater){
    ci.limit.summary <- internal.quantiles.mean.sd(x = test.statistics.prop2$lower.ci, the.quantiles = the.quantiles)
  }
  if(alternative == value.less){
    ci.limit.summary <- internal.quantiles.mean.sd(x = test.statistics.prop2$upper.ci, the.quantiles = the.quantiles)
  }


  res <- list(estimate.summary = estimate.summary, stat.summary = stat.summary, p.value.summary = p.value.summary, df.summary = df.summary)

  if(alternative == value.two.sided){
    res$ci.range.summary <- ci.range.summary
    res$ci.proportion.above.null.summary <- ci.proportion.above.null.summary
    res$ci.proportion.below.null.summary <- ci.proportion.below.null.summary
  }

  if(alternative != value.two.sided){
    res$ci.limit.summary <- ci.limit.summary
  }

  return(res)
}

#' analyze.simstudy.t


#' @description analyze.simstudy.t

#' @param test.statistics.t  Summary information for fitting one-sample t tests.  Structure is in the form returned by the function simitation::sim.t.test().

#' @param  alternative  See help(t.test).

#' @param  conf.level  See help(t.test)

#' @param  the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.
#'
#' @export

analyze.simstudy.t <- function(test.statistics.t, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)){

  value.two.sided <- "two.sided"
  value.less <- "less"
  value.greater <- "greater"

  alternative <- alternative[1]

  if(!(alternative %in% c(value.less, value.greater))){
    alternative <- value.two.sided
  }

  stat.summary <- internal.quantiles.mean.sd(x = test.statistics.t$statistic, the.quantiles = the.quantiles)

  estimate.summary <- internal.quantiles.mean.sd(x = test.statistics.t$estimate, the.quantiles = the.quantiles)

  p.value.summary <- data.frame(reject.proportion = mean(test.statistics.t$p.value < 1 - conf.level), non.reject.proportion = mean(test.statistics.t$p.value >= 1 - conf.level))

  if(alternative == value.two.sided){
    ci.range.summary <- internal.quantiles.mean.sd(x = test.statistics.t$upper.ci - test.statistics.t$lower.ci, the.quantiles = the.quantiles)

    ci.proportion.above.null.summary <- internal.quantiles.mean.sd(x = (test.statistics.t$upper.ci - test.statistics.t$null.value) / (test.statistics.t$upper.ci - test.statistics.t$lower.ci), the.quantiles = the.quantiles)

    ci.proportion.below.null.summary <- internal.quantiles.mean.sd(x = (test.statistics.t$null.value - test.statistics.t$lower.ci) / (test.statistics.t$upper.ci - test.statistics.t$lower.ci), the.quantiles = the.quantiles)
  }
  if(alternative == value.greater){
    ci.limit.summary <- internal.quantiles.mean.sd(x = test.statistics.t$lower.ci, the.quantiles = the.quantiles)
  }
  if(alternative == value.less){
    ci.limit.summary <- internal.quantiles.mean.sd(x = test.statistics.t$upper.ci, the.quantiles = the.quantiles)
  }

  res <- list(estimate.summary = estimate.summary, stat.summary = stat.summary, p.value.summary = p.value.summary)

  if(alternative == value.two.sided){
    res$ci.range.summary <- ci.range.summary
    res$ci.proportion.above.null.summary <- ci.proportion.above.null.summary
    res$ci.proportion.below.null.summary <- ci.proportion.below.null.summary
  }

  if(alternative != value.two.sided){
    res$ci.limit.summary <- ci.limit.summary
  }

  return(res)
}


#' analyze.simstudy.t2


#' @description analyze.simstudy.t2

#' @param  test.statistics.t2  Summary information for fitting two-sample t tests.  Structure is in the form returned by the function simitation::sim.t2.test().

#' @param  alternative  See help(t.test).

#' @param  conf.level  See help(t.test)

#' @param  the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.
#'
#' @export

analyze.simstudy.t2 <- function(test.statistics.t2, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)){

  value.two.sided <- "two.sided"
  value.less <- "less"
  value.greater <- "greater"

  alternative <- alternative[1]

  if(!(alternative %in% c(value.less, value.greater))){
    alternative <- value.two.sided
  }

  stat.summary <- internal.quantiles.mean.sd(x = test.statistics.t2$statistic, the.quantiles = the.quantiles)

  estimate.summary <- internal.quantiles.mean.sd(x = test.statistics.t2$estimate, the.quantiles = the.quantiles)

  df.summary <- internal.quantiles.mean.sd(x = test.statistics.t2$df, the.quantiles = the.quantiles)

  p.value.summary <- data.frame(reject.proportion = mean(test.statistics.t2$p.value < 1 - conf.level), non.reject.proportion = mean(test.statistics.t2$p.value >= 1 - conf.level))

  if(alternative == value.two.sided){

    ci.range.summary <- internal.quantiles.mean.sd(x = test.statistics.t2$upper.ci - test.statistics.t2$lower.ci, the.quantiles = the.quantiles)

    ci.proportion.above.null.summary <- internal.quantiles.mean.sd(x = (test.statistics.t2$upper.ci - test.statistics.t2$null.value) / (test.statistics.t2$upper.ci - test.statistics.t2$lower.ci), the.quantiles = the.quantiles)

    ci.proportion.below.null.summary <- internal.quantiles.mean.sd(x = (test.statistics.t2$null.value - test.statistics.t2$lower.ci) / (test.statistics.t2$upper.ci - test.statistics.t2$lower.ci), the.quantiles = the.quantiles)
  }

  if(alternative == value.greater){
    ci.limit.summary <- internal.quantiles.mean.sd(x = test.statistics.t2$lower.ci, the.quantiles = the.quantiles)
  }
  if(alternative == value.less){
    ci.limit.summary <- internal.quantiles.mean.sd(x = test.statistics.t2$upper.ci, the.quantiles = the.quantiles)
  }

  res <- list(estimate.summary = estimate.summary, stat.summary = stat.summary, df.summary = df.summary, p.value.summary = p.value.summary)

  if(alternative == value.two.sided){
    res$ci.range.summary <- ci.range.summary
    res$ci.proportion.above.null.summary <- ci.proportion.above.null.summary
    res$ci.proportion.below.null.summary <- ci.proportion.below.null.summary
  }

  if(alternative != value.two.sided){
    res$ci.limit.summary <- ci.limit.summary
  }

  return(res)
}

#' sim.chisq.gf
#'
#' @description Simulate data for chi-squared tests of goodness of fit across experiments.
#'
#' @param n A numeric value indicating the number of observations in each experiment.
#' @param values A numeric vector specifying the possible values (sample space).
#' @param prob A numeric vector of probabilities corresponding to the values for simulation.
#'        If not provided, equal probabilities are assumed for all values.
#' @param num.experiments An integer indicating the number of simulated experiments to conduct.
#' @param experiment.name A character string specifying the column name for identifying each experiment in the output.
#' @param value.name A character string specifying the column name for the simulated values in the output.
#' @param seed An integer specifying the seed for reproducibility. Default is 91.
#' @param vstr A character string indicating the RNG version for reproducibility.
#'        Default is "3.6". For more details, refer to \code{\link[RNGversion]{set.seed}}.
#'
#' @return A `data.table` containing the simulated experiments with specified column names.
#' @export
sim.chisq.gf <- function(n, values, prob = NULL, num.experiments = 1,
                         experiment.name = "experiment", value.name = "x",
                         seed = 91, vstr = "3.6") {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  RNGversion(vstr = vstr)
  set.seed(seed = seed)

  n <- max(1, floor(n), na.rm = TRUE)
  num.experiments <- max(1, floor(num.experiments), na.rm = TRUE)

  num.values <- length(values)

  if (num.values == 0) {
    stop("Error: Must specify values to build the sample.")
  }

  if (is.null(prob) || length(prob) == 0) {
    prob <- rep.int(x = 1/num.values, times = num.values)
  }

  if (length(prob) != num.values) {
    stop("Error: length(prob) should equal length(values)")
  }

  x <- sample(x = values, size = n * num.experiments, replace = TRUE, prob = prob)

  dat <- data.table::data.table(a = rep.int(x = 1:num.experiments, times = n), b = x)

  data.table::setorderv(x = dat, cols = "a", order = 1L)

  names(dat) <- c(experiment.name, value.name)

  return(dat)
}

#' sim.chisq.ind
#'
#' @description sim.chisq.ind


#' @param n A vector of sample sizes for the different groups.

#' @param values A vector of values specifying the sample space.

#' @param  probs A matrix of probabilities used to simulate the values in each group.  The rows of the probs matrix correspond to the groups, while the columns correspond to the values.

#' @param  num.experiments A numeric value representing the number of simulated experiments.

#' @param  experiment.name A character value providing the name for the column identifying the experiment.

#' @param group.name A character value providing the name of the column of the group labels.

#' @param  group.values A vector of unique values that identify the different groups, e.g. c("x", "y", "z").  If NULL, then values "x1", "x2", ..., "xk" will be assigned for the k groups specified.

#' @param value.name A character value providing the name for the simulated values.

#' @param seed A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param vstr A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).

#' @export
#'
#'
sim.chisq.ind <- function(n, values, probs, num.experiments = 2, experiment.name = "experiment", group.name = "group", group.values = NULL, value.name = "value", seed = 8272, vstr = 3.6){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  RNGversion(vstr = vstr)
  set.seed(seed = seed)

  n <- pmax(1, floor(n), na.rm = TRUE)
  num.experiments <- max(1, floor(num.experiments), na.rm = TRUE)

  num.values <- length(values)
  num.groups <- length(n)

  if(num.values == 0){
    stop("Error: Must specify values to build the sample.")
  }

  if(nrow(probs) == 0){
    stop(sprintf("Error: Must specify probs as a numeric matrix of probabilities for the groups"))
  }
  if(ncol(probs) != num.values){
    stop("Error: ncol(probs) should equal length(values).")
  }
  if(nrow(probs) != num.groups){
    stop("Error: nrow(probs) should equal length(n).")
  }

  if(is.null(group.values)){
    group.values <- sprintf("x%d", 1:num.groups)
  }

  list.dat <- list()
  for(i in 1:num.groups){
    list.dat[[i]] <- data.table::data.table(a = rep.int(x = 1:num.experiments, times = n[i]), b = group.values[i], c = sample(x = values, size = n[i] * num.experiments, replace = TRUE, prob = probs[i,]))
  }

  dat <- data.table::rbindlist(l = list.dat)
  data.table::setorderv(x = dat, cols = c("a", "b"))

  names(dat) <- c(experiment.name, group.name, value.name)

  return(dat)
}



#' sim.chisq.test.ind
#'
#' @description sim.chisq.test.ind

#' @param simdat.chisq.ind Data for use in chi squared tests of independence across one or more experiments.  Structure is in the form returned by the function simitation::sim.chisq.ind().

#' @param correct See help(chisq.test).

#' @param experiment.name A character value providing the name of the column identifying the experiment.

#' @param  group.name A character value providing the name of the column of the group labels.

#' @param  value.name A character value providing the name of the column identifying the values.
#'
#' @export

sim.chisq.test.ind <- function(simdat.chisq.ind, correct = T, experiment.name = "experiment", group.name = "variable", value.name = "value"){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt <- data.table::is.data.table(simdat.chisq.ind)
  data.table::setDT(simdat.chisq.ind)

  res <- simdat.chisq.ind[, internal.chisq.test.ind(the.data = .SD, group.name = group.name, value.name = value.name, correct = correct), by = experiment.name]

  if(!is.dt){
    data.table::setDF(simdat.chisq.ind)
    data.table::setDF(res)
  }

  return(res)
}



#' sim.prop
#'
#' @description sim.prop

#' @param  n A numeric value for the number of observations in each experiment.

#' @param  p A numeric value for the probability of success.

#' @param num.experiments A numeric value representing the number of simulated experiments.

#' @param experiment.name A character value providing the name for the column identifying the experiment.

#' @param value.name A character value providing the name for the simulated values.

#' @param seed A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param vstr A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export

sim.prop <- function(n, p = 0.5, num.experiments = 1, experiment.name = "experiment", value.name = "x", seed = 2470, vstr = 3.6){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  RNGversion(vstr = vstr)
  set.seed(seed = seed)
  n <- max(1, floor(n), na.rm = TRUE)
  num.experiments <- max(1, floor(num.experiments), na.rm = TRUE)

  if(p < 0 | p > 1){
    stop("Error:  p must be a number between 0 and 1.")
  }

  x <- sample(0:1, size = n * num.experiments, replace = TRUE, prob = c(1-p, p))
  experiment <- sort(rep.int(1:num.experiments, n), decreasing = FALSE)
  dat <- data.table::data.table(experiment = experiment, x = x)
  names(dat) = c(experiment.name, value.name)

  return(dat)
}


#' sim.prop.test
#'
#' @description sim.prop.test


#' @param simdat.prop Data for use in one-sample proportions tests across one or more experiments.  Structure is in the form returned by the function simitation::sim.prop().

#' @param p See help(prop.test).

#' @param alternative See help(prop.test).

#' @param conf.level See help(prop.test).

#' @param correct See help(prop.test).

#' @param experiment.name A character value providing the name of the column identifying the experiment.

#' @param value.name A character value providing the name of the column identifying the values.
#'
#' @export


sim.prop.test <- function(simdat.prop, p = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = TRUE, experiment.name = "experiment", value.name = "x"){
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt <- data.table::is.data.table(simdat.prop)
  data.table::setDT(simdat.prop)
  res <- simdat.prop[, internal.prop.test(x = get(value.name), p = p, alternative = alternative, conf.level = conf.level, correct = correct), by = experiment.name]

  if(!is.dt){
    data.table::setDF(simdat.prop)
    data.table::setDF(res)
  }

  return(res)
}

#' sim.prop2.test
#'
#' @description sim.prop2.test

#' @param simdat.prop2 Data for use in two-sample proportions tests across one or more experiments.  Structure is in the form returned by the function simitation::sim.prop2().

#' @param p See help(prop.test).

#' @param alternative See help(prop.test).

#' @param conf.level See help(prop.test).

#' @param correct See help(prop.test).

#' @param experiment.name A character value providing the name of the column identifying the experiment.

#' @param  group.name A character value providing the name of the column of the group labels.

#' @param  x.value A character value providing a label for the first group in the two-sample t test in the column of data named by group.name.

#' @param y.value A character value providing a label for the second group in the two-sample t test in the column of data named by group.name.

#' @param value.name A character value providing the name of the column identifying the values.
#'
#' @export



sim.prop2.test <- function(simdat.prop2, p = NULL,  alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = TRUE, experiment.name = "experiment", group.name = "group", x.value = "x", y.value = "y", value.name = "value"){
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt <- data.table::is.data.table(simdat.prop2)
  data.table::setDT(simdat.prop2)
  res <- simdat.prop2[, internal.prop2.test(x = get(value.name)[get(group.name) == x.value], y = get(value.name)[get(group.name) == y.value], p = p, alternative = alternative, conf.level = conf.level, correct = correct), by = experiment.name]

  if(!is.dt){
    data.table::setDF(simdat.prop2)
    data.table::setDF(res)
  }

  return(res)
}



#' sim.statistic.lm

#' @description sim.statistic.lm
#'
#' @param simdat Data for use in multivariable regression models across one or more experiments.  Structure is in the form returned by the function simitation::simulation.steps().

#' @param the.formula A formula object or character value specifying the formula for the regression model.

#' @param grouping.variables A character vector of column names from simdat on which to group the data.  The intended regression model will be fit in groups based on this selection.
#'
#' @export

sim.statistics.lm <- function(simdat, the.formula, grouping.variables){
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt <- data.table::is.data.table(simdat)
  data.table::setDT(simdat)

  the.coefs <- simdat[, internal.statistics.one.lm(the.data = .SD, the.formula = the.formula)$coef.table, by = grouping.variables]
  summary.stats <- simdat[, internal.statistics.one.lm(the.data = .SD, the.formula = the.formula)$summary.stats, by = grouping.variables]

  if(!is.dt){
    data.table::setDF(simdat)
  }

  return(list(the.coefs = the.coefs, summary.stats = summary.stats))
}



#' sim.statistics.logistic

#' @description sim.statistics.logistic

#' @param simdat Data for use in multivariable regression models across one or more experiments.  Structure is in the form returned by the function simitation::simulation.steps().

#' @param the.formula A formula object or character value specifying the formula for the regression model.

#' @param grouping.variables A character vector of column names from simdat on which to group the data.  The intended regression model will be fit in groups based on this selection.
#'
#' @export

sim.statistics.logistic <- function(simdat, the.formula, grouping.variables){
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt <- data.table::is.data.table(simdat)
  data.table::setDT(simdat)
  the.coefs <- simdat[, internal.statistics.one.logistic(the.data = .SD, the.formula = the.formula)$coef.table, by = grouping.variables]
  summary.stats <- simdat[, internal.statistics.one.logistic(the.data = .SD, the.formula = the.formula)$summary.stats, by = grouping.variables]

  if(!is.dt){
    data.table::setDF(simdat)
  }

  return(list(the.coefs = the.coefs, summary.stats = summary.stats))
}


#' sim.t

#' @description sim.t

#' @param n A numeric value for the number of observations in each experiment.

#' @param mean A numeric value for the expected value of the data to be simulated.

#' @param sd A numeric value for the standard deviation of the data to be simulated.

#' @param num.experiments A numeric value representing the number of simulated experiments.

#' @param  experiment.name A character value providing the name for the column identifying the experiment.

#' @param value.name A character value providing the name for the simulated values.

#' @param seed A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export

sim.t <- function(n, mean = 0, sd = 1, num.experiments = 1, experiment.name = "experiment", value.name = "x", seed = 7261, vstr = 3.6){

  dat = sim.norm(n.values = n, mean.values = mean, sd.values = sd, num.experiments = num.experiments, variable.names = value.name, seed = seed, vstr = vstr)[, c("experiment", "value")]

  names(dat) <- c(experiment.name, value.name)

  return(dat)
}


#' sim.prop2
#'
#' @description sim.prop2

#' @param  nx A numeric value for the number of observations in the x group for each experiment.

#' @param  ny A numeric value for the number of observations in the y group for each experiment.

#' @param  px A numeric value for the probability of success in the x group.

#' @param  py A numeric value for the probability of success in the y group.

#' @param  num.experiments A numeric value representing the number of simulated experiments.

#' @param  experiment.name A character value providing the name for the column identifying the experiment.

#' @param  group.name A character value providing the name of the column of the group labels.

#' @param  x.value A character value specifying the label used for data in the x group (in the column labled by the group.name parameter).

#' @param  y.value A character value specifying the label used for data in the y group (in the column labled by the group.name parameter).

#' @param  value.name A character value specifying the name of the column that contains the value of the simulated data.

#' @param  seed A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export


sim.prop2 <- function(nx, ny, px = 0.5, py = 0.5, num.experiments = 1, experiment.name = "experiment", group.name = "group", x.value = "x", y.value = "y", value.name = "value", seed = 3471, vstr = 3.6){
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  RNGversion(vstr = vstr)
  set.seed(seed = seed)

  nx <- max(1, floor(nx), na.rm = TRUE)
  ny <- max(1, floor(ny), na.rm = TRUE)
  num.experiments <- max(1, floor(num.experiments), na.rm = TRUE)

  if(px < 0 | px > 1){
    stop("Error:  px must be a number between 0 and 1.")
  }
  if(py < 0 | py > 1){
    stop("Error:  py must be a number between 0 and 1.")
  }

  x <- sample(0:1, size = nx * num.experiments, replace = TRUE, prob = c(1-px, px))
  y <- sample(0:1, size = ny * num.experiments, replace = TRUE, prob = c(1-py, py))

  x.dat <- data.table::data.table(experiment = rep.int(1:num.experiments, nx), variable = x.value, value = x)
  y.dat <- data.table::data.table(experiment = rep.int(1:num.experiments, ny), variable = y.value, value = y)

  res <- data.table::rbindlist(list(x.dat, y.dat), fill = TRUE)
  data.table::setorderv(res, "experiment", order = 1L)
  names(res) <- c(experiment.name, group.name, value.name)

  return(res)
}





#' sim.t.test
#'
#' @description sim.t.test
#' @import data.table
#'
#' @param  simdat.t Data for use in one-sample t tests across one or more experiments.  Structure is in the form returned by the function simitation::sim.t().

#' @param  alternative See help(t.test).

#' @param  mu See help(t.test)

#' @param  paired See help(t.test)

#' @param  var.equal See help(t.test)

#' @param  conf.level See help(t.test)

#' @param  experiment.name A character value providing the name of the column identifying the experiment.

#' @param  value.name A character value providing the name of the column identifying the values.
#'
#' @export


sim.t.test <- function(simdat.t, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, experiment.name = "experiment", value.name = "x"){
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt <- data.table::is.data.table(simdat.t)
  data.table::setDT(simdat.t)
  res <- simdat.t[, internal.t.test(x = get(value.name), alternative = alternative, mu = mu, paired = paired, var.equal = var.equal, conf.level = conf.level), by = experiment.name]

  if(!is.dt){
    data.table::setDF(simdat.t)
    data.table::setDF(res)
  }

  return(res)
}



#' sim.t2
#'
#' @description sim.t2

#' @param  nx A numeric value for the number of observations in the x group for each experiment.

#' @param  ny A numeric value for the number of observations in the y group for each experiment.

#' @param  meanx A numeric value for the expected value of the x group used in the simulation.

#' @param  meany A numeric value for the expected value of the y group used in the simulation.

#' @param  sdx A numeric value for the standard deviation of the x group used in the simulation.

#' @param  sdy A numeric value for the standard deviation of the y group used in the simulation.

#' @param  num.experiments A numeric value representing the number of simulated experiments.

#' @param  experiment.name A character value providing the name for the column identifying the experiment.

#' @param  group.name A character value providing the name of the column of the group labels.

#' @param  x.value A character value specifying the label used for data in the x group (in the column labled by the group.name parameter).

#' @param  y.value A character value specifying the label used for data in the y group (in the column labled by the group.name parameter).

#' @param  value.name A character value specifying the name of the column that contains the value of the simulated data.

#' @param  seed A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export


sim.t2 <- function(nx, ny, meanx = 0, meany = 1, sdx = 1, sdy = 1, num.experiments = 1, experiment.name = "experiment", group.name = "group", x.value = "x", y.value = "y", value.name = "value", seed = 3471, vstr = 3.6){

  res <- sim.norm(n.values = c(nx,ny),mean.values = c(meanx, meany), sd.values = c(sdx, sdy), num.experiments = num.experiments, variable.names = c(x.value, y.value), seed = seed, vstr = vstr)

  names(res) <- c(experiment.name, group.name, value.name)

  return(res)
}


#' sim.t2.test
#'
#' @description sim.t2.test

#' @param  simdat.t2 Data for use in two-sample t tests across one or more experiments.  Structure is in the form returned by the function simitation::sim.t2().

#' @param  alternative See help(t.test).

#' @param  mu See help(t.test)

#' @param  paired See help(t.test)

#' @param  var.equal See help(t.test)

#' @param  conf.level See help(t.test)

#' @param  experiment.name A character value providing the name for the column identifying the experiment.

#' @param  group.name A character value providing the name of the column of the group labels.

#' @param  x.value A character value providing a label for the first group in the two-sample t test in the column of data named by group.name.

#' @param  y.value A character value providing a label for the second group in the two-sample t test in the column of data named by group.name.

#' @param  value.name A character value providing the name of the column of the values.
#'
#' @export


sim.t2.test <- function(simdat.t2, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, experiment.name = "experiment", group.name = "group", x.value = "x", y.value = "y", value.name = "value"){
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt <- data.table::is.data.table(simdat.t2)
  data.table::setDT(simdat.t2)
  res <- simdat.t2[, internal.t2.test(x = get(value.name)[get(group.name) == x.value], y = get(value.name)[get(group.name) == y.value], alternative = alternative, mu = mu, paired = paired, var.equal = var.equal, conf.level = conf.level), by = experiment.name]

  if(!is.dt){
    data.table::setDF(simdat.t2)
    data.table::setDF(res)
  }

  return(res)
}



#' simstudy.chisq.test.gf
#'
#' @description simstudy.chisq.test.gf
#'
#' @param  n A numeric value for the number of observations in each experiment.
#' @param  values A vector of values specifying the sample space.
#' @param  actual.probs A vector of probabilities used to simulate the values.
#' @param  hypothesized.probs A vector of hypothesized probabilities for the values.
#' @param num.experiments A numeric value representing the number of simulated experiments.
#' @param conf.level A numeric value between 0 and 1 representing the confidence level (1 - significance level).
#' @param  correct See help(chisq.test).
#' @param  the.quantiles A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.
#' @param experiment.name A character value providing the name for the column identifying the experiment.
#' @param  value.name A character value providing the name for the simulated values.
#' @param  seed A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).
#' @param vstr A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#' @export

simstudy.chisq.test.gf <- function(n, values, actual.probs, hypothesized.probs = NULL, num.experiments = 1, conf.level = 0.95, correct = T, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", value.name = "x", seed = 7261, vstr = 3.6){

  simdat <- sim.chisq.gf(n = n, values = values, prob = actual.probs, num.experiments = num.experiments, value.name = value.name, experiment.name = experiment.name, seed = seed, vstr = vstr)

  test.statistics <- sim.chisq.test.gf(simdat.chisq.gf = simdat, hypothesized.probs = hypothesized.probs, experiment.name = experiment.name, value.name = value.name, correct = correct)

  ## analyze.simstudy.chisq.test.gf.R
  sim.analysis <- analyze.simstudy.chisq.test.gf(test.statistics.chisq.test.gf =  test.statistics, conf.level = conf.level, the.quantiles = the.quantiles)

  return(list(simdat = simdat, test.statistics = test.statistics, sim.analysis = sim.analysis))
}


#' simstudy.chisq.test.ind

#' @description simstudy.chisq.test.ind

#' @param n A vector of sample sizes for the different groups.

#' @param  values A vector of values specifying the sample space.

#' @param  probs A matrix of probabilities used to simulate the values in each group.  The rows of the probs matrix correspond to the groups, while the columns correspond to the values.

#' @param num.experiments A numeric value representing the number of simulated experiments.

#' @param conf.level A numeric value between 0 and 1 representing the confidence level (1 - significance level).

#' @param  correct See help(chisq.test).

#' @param  the.quantiles A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param experiment.name A character value providing the name for the column identifying the experiment.

#' @param  group.name A character value providing the name of the column of the group labels.

#' @param group.values A vector of unique values that identify the different groups, e.g. c("x", "y", "z").  If NULL, then values "x1", "x2", ..., "xk" will be assigned for the k groups specified.

#' @param  value.name A character value providing the name for the simulated values.

#' @param seed A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export


simstudy.chisq.test.ind <- function(n, values, probs, num.experiments = 1, conf.level = 0.95, correct = T, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", group.name = "group", group.values = NULL, value.name = "value", seed = 403, vstr = 3.6){

  simdat.chisq.ind <- sim.chisq.ind(n = n, values = values, probs = probs, num.experiments = num.experiments, experiment.name = experiment.name, group.name = group.name, group.values = group.values, value.name = value.name, seed = seed, vstr = vstr)

  test.statistics.chisq.test.ind <- sim.chisq.test.ind(simdat.chisq.ind = simdat.chisq.ind, experiment.name = experiment.name, group.name = group.name, value.name = value.name, correct = correct)

  sim.analysis.chisq.test.ind <- analyze.simstudy.chisq.test.ind(test.statistics.chisq.test.ind = test.statistics.chisq.test.ind, conf.level = conf.level, the.quantiles = the.quantiles)

  return(list(simdat.chisq.ind = simdat.chisq.ind, test.statistics.chisq.test.ind = test.statistics.chisq.test.ind, sim.analysis = sim.analysis.chisq.test.ind))
}

#' simstudy.lm

#' @description simstudy.lm

#' @param  the.steps A character vector of variables to simulate.  The variables are simulated in the order specified.  Later variables can be generated to depend on earlier variables.  The possible specifications include:

#' Normal "X ~ N(100, 5)" with the mean and SD.

#' Uniform "X ~ U(0, 100)" with the minimum and maximum.

#' Poisson "X ~ Poisson(3)" with the mean.

#' Binary "X ~ Binary(0.5)" with the probability of success.

#' Binomial "X ~ Bin(10, 0.2)" with the number of trials and probability of success.

#' Categorical "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))" with the values in the first set of parentheses and their respective probabilities in the second.

#' Regression "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"

#' Linear Regression  "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))".  Note that the error term may be specified symbolically with any of the above distributions.

#' @param  n  A numeric value for the number of observations in each experiment.

#' @param  num.experiments  A numeric value representing the number of simulated experiments.

#' @param  the.formula  A formula object or character value specifying the formula for the regression model.

#' @param  conf.level  A numeric value between 0 and 1 representing the confidence level (1 - significance level).

#' @param  the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param  experiment.name  A character value providing the name for the column identifying the experiment.

#' @param  step.split  A character value that separates the name of the variable to be simulated (left side) from its distribution (right side).  Using the.steps = "X ~ N(0,1)" with step.split = "~" will generate a variable named X from a standard Normal distribution.

#' @param  coef.name  A character value specifying the column of the.coefs that contains the names of the input variables of the linear regression model.

#' @param  estimate.name  A character value specifying the column of the.coefs that contains the estimated coefficients of the linear regression model.

#' @param  lm.p.name  A character value specifying the column of the.coefs that contains the p-values for the tests of the estimated coefficients of the linear regression model.

#' @param  f.p.name  A character value specifying the column of summary.stats that contains the p-value for the linear regression model's F test.

#' @param  seed  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr  A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export


simstudy.lm <- function(the.steps, n, num.experiments, the.formula, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", step.split = "~", coef.name = "Coefficient", estimate.name = "Estimate", lm.p.name = "Pr(>|t|)", f.p.name = "f.pvalue", seed = 41, vstr = 3.6){

  simdat.lm <- simulation.steps(the.steps = the.steps, n = n, num.experiments = num.experiments, experiment.name = experiment.name, step.split = step.split, seed = seed, vstr = vstr)

  statistics.lm <- sim.statistics.lm(simdat = simdat.lm, the.formula = the.formula, grouping.variables = experiment.name)

  sim.analysis <- analyze.simstudy.lm(the.coefs = statistics.lm$the.coefs, summary.stats = statistics.lm$summary.stats, conf.level = conf.level, the.quantiles = the.quantiles, coef.name = coef.name, estimate.name = estimate.name, lm.p.name = lm.p.name, f.p.name = f.p.name)

  return(list(the.steps = the.steps, simdat = simdat.lm, statistics = statistics.lm, sim.analysis = sim.analysis))
}



#' simstudy.logistic
#'
#' @description simstudy.logistic

#' @param  the.steps  A character vector of variables to simulate.  The variables are simulated in the order specified.  Later variables can be generated to depend on earlier variables.  The possible specifications include:
#' Normal  "X ~ N(100, 5)" with the mean and SD.

#' Uniform  "X ~ U(0, 100)" with the minimum and maximum.

#' Poisson  "X ~ Poisson(3)" with the mean.

#' Binary  "X ~ Binary(0.5)" with the probability of success.

#' Binomial  "X ~ Bin(10, 0.2)" with the number of trials and probability of success.

#' Categorical  "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))" with the values in the first set of parentheses and their respective probabilities in the second.

#' Logistic Regression  "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"

#' Linear Regression  "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))".  Note that the error term may be specified symbolically with any of the above distributions.

#' @param  n  A numeric value for the number of observations in each experiment.

#' @param  num.experiments  A numeric value representing the number of simulated experiments.

#' @param  the.formula  A formula object or character value specifying the formula for the regression model.

#' @param  conf.level  A numeric value between 0 and 1 representing the confidence level (1 - significance level).

#' @param  the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param  experiment.name  A character value providing the name for the column identifying the experiment.

#' @param  step.split  A character value that separates the name of the variable to be simulated (left side) from its distribution (right side).  Using the.steps = "X ~ N(0,1)" with step.split = "~" will generate a variable named X from a standard Normal distribution.

#' @param  coef.name  A character value specifying the column of the.coefs that contains the names of the input variables of the linear regression model.

#' @param  estimate.name  A character value specifying the column of the.coefs that contains the estimated coefficients of the linear regression model.

#' @param  logistic.p.name  A character value specifying the column of the.coefs that contains the p-values for the tests of the estimated coefficients of the logistic regression model.

#' @param  seed  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr  A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export


simstudy.logistic <- function(the.steps, n, num.experiments, the.formula, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", step.split = "~", coef.name = "Coefficient", estimate.name = "Estimate", logistic.p.name = "Pr(>|z|)", seed = 39, vstr = 3.6){

  simdat <- simulation.steps(the.steps = the.steps, n = n, num.experiments = num.experiments, experiment.name = experiment.name, step.split = step.split, seed = seed, vstr = vstr)

  statistics <- sim.statistics.logistic(simdat = simdat, the.formula = the.formula, grouping.variables = experiment.name)

  sim.analysis <- analyze.simstudy.logistic(the.coefs = statistics$the.coefs, summary.stats = statistics$summary.stats, conf.level = conf.level, the.quantiles = the.quantiles, coef.name = coef.name, estimate.name = estimate.name, logistic.p.name = logistic.p.name)

  return(list(the.steps = the.steps, simdat = simdat, statistics = statistics, sim.analysis = sim.analysis))
}



#' simstudy.prop

#' @description simstudy.prop

#' @param  n  A numeric value for the number of observations in each experiment.

#' @param  p.actual A numeric value for the actual probability of success.

#' @param  p.hypothesized  A numeric value for the hypothesized probability of success.

#' @param  num.experiments  A numeric value representing the number of simulated experiments.

#' @param  alternative  See help(prop.test).

#' @param  conf.level  See help(prop.test).

#' @param  correct  See help(prop.test).

#' @param  the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param  experiment.name  A character value providing the name for the column identifying the experiment.

#' @param  value.name  A character value providing the name for the simulated values.

#' @param  seed  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr  A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export


simstudy.prop <- function(n, p.actual = 0.5, p.hypothesized = 0.5, num.experiments = 1, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = T, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", value.name = "x", seed = 7261, vstr = 3.6){

  value.two.sided <- "two.sided"
  value.less <- "less"
  value.greater <- "greater"

  alternative <- alternative[1]

  if(!(alternative %in% c(value.less, value.greater))){
    alternative <- value.two.sided
  }

  simdat.prop <- sim.prop(n = n, p = p.actual, num.experiments = num.experiments, seed = seed, vstr = vstr, experiment.name = experiment.name, value.name = value.name)

  # sim.prop.test
  test.statistics.prop <- sim.prop.test(simdat.prop = simdat.prop, value.name = value.name, experiment.name = experiment.name, alternative = alternative, p = p.hypothesized, conf.level = conf.level, correct = correct)

  # analyze.simstudy.prop
  sim.analysis.prop <- analyze.simstudy.prop(test.statistics.prop = test.statistics.prop, conf.level = conf.level, alternative = alternative, the.quantiles = the.quantiles)

  return(list(simdat.prop = simdat.prop, test.statistics.prop = test.statistics.prop, sim.analysis.prop = sim.analysis.prop))
}


#' simstudy.prop2
#'
#' @description simstudy.prop2

#' @param  nx  A numeric value for the number of observations in the x group for each experiment.

#' @param  ny  A numeric value for the number of observations in the y group for each experiment.

#' @param  px  A numeric value for the probability of success in the x group.

#' @param  py  A numeric value for the probability of success in the y group.

#' @param num.experiments  A numeric value representing the number of simulated experiments.

#' @param p  See help(prop.test).

#' @param alternative  See help(prop.test).

#' @param conf.level  See help(prop.test).

#' @param correct  See help(prop.test).

#' @param the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param experiment.name  A character value providing the name for the column identifying the experiment.

#' @param group.name  A character value providing the name of the column of the group labels.

#' @param x.value  A character value specifying the label used for data in the x group (in the column labled by the group.name parameter).

#' @param  y.value  A character value specifying the label used for data in the y group (in the column labled by the group.name parameter).

#' @param  value.name  A character value specifying the name of the column that contains the value of the simulated data.

#' @param  seed  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param vstr  A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export


simstudy.prop2 <- function(nx, ny, px, py, num.experiments, p = NULL,  alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = TRUE, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", group.name = "group", x.value = "x", y.value = "y", value.name = "value", seed = 920173, vstr = 3.6){

  value.two.sided <- "two.sided"
  value.less <- "less"
  value.greater <- "greater"

  alternative <- alternative[1]

  if(!(alternative %in% c(value.less, value.greater))){
    alternative <- value.two.sided
  }

  simdat.prop2 <- sim.prop2(nx = nx, ny = ny, px = px, py = py, num.experiments = num.experiments, experiment.name = experiment.name, group.name = group.name, x.value = x.value, y.value = y.value, value.name = value.name, seed = seed, vstr = vstr)

  # sim.prop2.test
  test.statistics.prop2 <- sim.prop2.test(simdat.prop2 = simdat.prop2, group.name = group.name, x.value = x.value, y.value = y.value, value.name = value.name, p = p, alternative = alternative, conf.level = conf.level, correct = correct, experiment.name = experiment.name)

  # analyze.simstudy.prop2
  sim.analysis.prop2 <- analyze.simstudy.prop2(test.statistics.prop2 = test.statistics.prop2, alternative = alternative, conf.level = conf.level, the.quantiles = the.quantiles)

  return(list(simdat.prop2 = simdat.prop2, test.statistics.prop2 = test.statistics.prop2, sim.analysis.prop2 = sim.analysis.prop2))
}


#' simstudy.t
#'
#' @description simstudy.t

#' @param  n  A numeric value for the number of observations in each experiment.

#' @param  mean  A numeric value for the expected value of the data to be simulated.

#' @param  sd  A numeric value for the standard deviation of the data to be simulated.

#' @param  num.experiments  A numeric value representing the number of simulated experiments.

#' @param  alternative  See help(t.test).

#' @param  mu  See help(t.test)

#' @param  conf.level  See help(t.test)

#' @param  value.name  A character value providing the name for the simulated values.

#' @param  experiment.name  A character value providing the name for the column identifying the experiment.

#' @param  the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param  seed  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr  A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export


simstudy.t <- function(n, mean = 0, sd = 1, num.experiments = 1, alternative = c("two.sided", "less", "greater"), mu = 0, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", value.name = "x", seed = 7261, vstr = 3.6){
  value.two.sided <- "two.sided"
  value.less <- "less"
  value.greater <- "greater"

  alternative <- alternative[1]

  if(!(alternative %in% c(value.less, value.greater))){
    alternative <- value.two.sided
  }

  simdat.t <- sim.t(n = n, num.experiments = num.experiments, mean = mean, sd = sd, experiment.name = experiment.name, value.name = value.name, seed = seed, vstr = vstr)

  test.statistics.t <- sim.t.test(simdat.t = simdat.t, alternative = alternative, mu = mu, conf.level = conf.level, experiment.name = experiment.name, value.name = value.name)

  sim.analysis.t <- analyze.simstudy.t(test.statistics.t = test.statistics.t, conf.level = conf.level, the.quantiles = the.quantiles, alternative = alternative)

  return(list(simdat.t = simdat.t, test.statistics.t = test.statistics.t, sim.analysis.t = sim.analysis.t))
}


#' simstudy.t2
#'
#' @description simstudy.t2
#'
#' @param  nx  A numeric value for the number of observations in the x group for each experiment.

#' @param  ny  A numeric value for the number of observations in the y group for each experiment.

#' @param  meanx  A numeric value for the expected value of the x group used in the simulation.

#' @param  meany  A numeric value for the expected value of the y group used in the simulation.

#' @param  sdx  A numeric value for the standard deviation of the x group used in the simulation.

#' @param  sdy  A numeric value for the standard deviation of the y group used in the simulation.

#' @param  num.experiments  A numeric value representing the number of simulated experiments.

#' @param  alternative  See help(t.test).

#' @param  mu  See help(t.test)

#' @param  conf.level  See help(t.test)

#' @param  experiment.name  A character value providing the name for the column identifying the experiment.

#' @param  group.name  A character value providing the name of the column of the group labels.

#' @param  x.value  A character value specifying the label used for data in the x group (in the column labled by the group.name parameter).

#' @param  y.value  A character value specifying the label used for data in the y group (in the column labled by the group.name parameter).

#' @param  value.name  A character value specifying the name of the column that contains the value of the simulated data.

#' @param  the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param  seed  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr  A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#' @param  var.equal A logical indicating whether to treat the two variances as being equal. If TRUE, then a pooled variance is used to estimate the variance, otherwise the variances are estimated separately. See help(t.test).
#'
#' @export

simstudy.t2 <- function(nx, ny, meanx = 0, meany = 1, sdx = 1, sdy = 1, num.experiments = 1, alternative = c("two.sided", "less", "greater"), mu = 0, var.equal = FALSE, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", group.name = "group", x.value = "x", y.value = "y", value.name = "value", seed = 3471, vstr = 3.6){

  value.two.sided <- "two.sided"
  value.less <- "less"
  value.greater <- "greater"

  alternative <- alternative[1]

  if(!(alternative %in% c(value.less, value.greater))){
    alternative <- value.two.sided
  }

  simdat.t2 <- sim.t2(nx = nx, ny = ny, meanx = meanx, meany = meany, sdx = sdx, sdy = sdy, num.experiments = num.experiments, x.value = x.value, y.value = y.value, seed = seed, vstr = vstr, experiment.name = experiment.name, group.name = group.name, value.name = value.name)

  test.statistics.t2 <- sim.t2.test(simdat.t2 = simdat.t2, alternative = alternative, mu = mu, paired = F, var.equal = var.equal, conf.level = conf.level, group.name = group.name, x.value = x.value, y.value = y.value, value.name = value.name, experiment.name = experiment.name)

  sim.analysis.t2 <- analyze.simstudy.t2(test.statistics.t2 = test.statistics.t2, alternative = alternative, conf.level = conf.level, the.quantiles = the.quantiles)

  return(list(simdat.t2 = simdat.t2, test.statistics.t2 = test.statistics.t2, sim.analysis.t2 = sim.analysis.t2))

}


#' simulation.steps
#'
#' @description simulation.steps


#' @param  the.steps  A character vector of variables to simulate.  The variables are simulated in the order specified.  Later variables can be generated to depend on earlier variables.  The possible specifications include:

## Normal:  "X ~ N(100, 5)" with the mean and SD.

## Uniform:  "X ~ U(0, 100)" with the minimum and maximum.

## Poisson:  "X ~ Poisson(3)" with the mean.

## Binary:  "X ~ Binary(0.5)" with the probability of success.

## Binomial:  "X ~ Bin(10, 0.2)" with the number of trials and probability of success.

## Categorical:  "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))" with the values in the first set of parentheses and their respective probabilities in the second.

## Logistic Regression:  "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"

## Linear Regression:  "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))".  Note that the error term may be specified symbolically with any of the above distributions.

#' @param  n  A numeric value for the number of observations in each experiment.

#' @param  num.experiments  A numeric value representing the number of simulated experiments.

#'
#' @param  experiment.name  A character value providing the name for the column identifying the experiment.

#' @param  step.split  A character value that separates the name of the variable to be simulated (left side) from its distribution (right side).  Using the.steps = "X ~ N(0,1)" with step.split = "~" will generate a variable named X from a standard Normal distribution.

#' @param  seed  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param  vstr  A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#'
#' @export

simulation.steps <- function(the.steps, n, num.experiments = 1, experiment.name = "experiment", step.split = "~", seed = 62, vstr = 3.6){

  n <- pmax(1, floor(n[1]))

  num.experiments <- pmax(1, floor(num.experiments))

  num.steps <- length(the.steps)

  if(num.steps < 1){
    stop("Error:  the.steps hould specify a character vector with the sequence of steps to build the simulation.")
  }
  RNGversion(vstr = vstr)
  set.seed(seed = seed)

  dat <- NULL
  for(i in 1:num.steps){
    dat <- identify.distribution(dat = dat, the.step = the.steps[i], n = n, num.experiments = num.experiments)
  }

  setDT(dat)

  dat[, eval(experiment.name) := rep.int(x = 1:num.experiments, times = n)]

  setorderv(x = dat, cols = experiment.name, order = 1L)

  setcolorder(x = dat, neworder = experiment.name)

  return(dat[])
}

#' Internal function for One-sample t-test
#'
#' @description Computes the one-sample t-test for the given data.
#'
#' @param x A numeric vector.
#' @param alternative A character string specifying the alternative hypothesis. One of \code{"two.sided"}, \code{"less"}, or \code{"greater"}. Default is \code{"two.sided"}.
#' @param mu A number indicating the true value of the mean (or difference in means if you are performing a two-sample test). Default is 0.
#' @param paired A logical indicating whether you want a paired t-test. Default is FALSE.
#' @param var.equal A logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used. Default is FALSE.
#' @param conf.level A numeric value between 0 and 1 indicating the confidence level for the interval estimate of the mean. Default is 0.95.
#' @return A data frame with test results.

internal.t.test <- function(x, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95){
  the.test <- t.test(x = x, alternative = alternative, mu = mu, conf.level = conf.level, paired = paired, var.equal = var.equal)

  res <- data.frame(statistic = the.test$statistic, df = the.test$parameter, p.value = the.test$p.value, lower.ci = the.test$conf.int[1], upper.ci = the.test$conf.int[2], estimate = the.test$estimate, null.value = the.test$null.value, alternative = the.test$alternative, method = the.test$method)

  return(res)
}





#' Internal function for Simulation for Binary Data
#'
#' @description This function is designed to generate binary data based on the provided formula.
#' It is an internal function and is not meant for end-users.
#'
#' @param the.formula A character string specifying the distribution function, e.g., "binary(0.5)".
#' @param the.variable A character string naming the variable in the generated data.
#' @param n An integer specifying the number of data points to generate for each experiment.
#' @param num.experiments An integer specifying the number of experiments to simulate. Default is 1.
#'
#' @return A data frame with simulated binary values based on the given formula.



buildsim.binary <- function(the.formula, the.variable, n, num.experiments = 1){
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]

  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. binary(0.5).")
  }

  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. binary(0.5).")
  }

  closing.parens <- which(the.chars == ")")
  last.closing.paren <- closing.parens[length(closing.parens)]

  if(last.closing.paren < first.open.paren){
    stop("Error:  Distribution must end with a closing ), e.g. binary(0.5)")
  }

  intermediate.text <- paste(the.chars[(first.open.paren + 1):(last.closing.paren-1)], collapse = "")


  success.prob <- eval(parse(text = intermediate.text))

  if(success.prob > 1 | success.prob < 0){
    stop("The value inside the parentheses must be a probability between 0 and 1, e.g. binary(0.5) for a 0.5 chance of success.")
  }

  the.values <- sample(x = c(TRUE, FALSE), size = n * num.experiments, replace = T, prob = c(success.prob, 1 - success.prob))

  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)

  return(res)
}

#' Internal function for Simulation for Binomial Data
#'
#' @description This internal function is designed to generate binomial data based on the
#' provided formula. It is not intended for direct usage by end-users.
#'
#' @param the.formula A character string specifying the distribution function, e.g., "Bin(10, 0.5)".
#' @param the.variable A character string naming the variable in the generated data.
#' @param n An integer specifying the number of data points to generate for each experiment.
#' @param num.experiments An integer specifying the number of experiments to simulate. Default is 1.
#'
#' @return A data frame with simulated binomial values based on the given formula.
#'


buildsim.binomial <- function(the.formula, the.variable, n, num.experiments = 1){
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]

  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. Bin(10, 0.5).")
  }

  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. Bin(10, 0.5).")
  }

  closing.parens <- which(the.chars == ")")
  last.closing.paren <- closing.parens[length(closing.parens)]

  if(last.closing.paren < first.open.paren){
    stop("Error:  Distribution must end with a closing ), e.g. Bin(10, 0.5)")
  }

  intermediate.text <- paste(the.chars[(first.open.paren + 1):(last.closing.paren-1)], collapse = "")

  the.pieces <- trimws(strsplit(x = intermediate.text, split = ",")[[1]])

  num.trials <- floor(as.numeric(the.pieces[1]))
  success.prob <- as.numeric(the.pieces[2])

  if(success.prob > 1 | success.prob < 0){
    stop("The value inside the parentheses must be a probability between 0 and 1, e.g. binary(0.5) for a 0.5 chance of success.")
  }

  if(num.trials < 0){
    num.trials <- 1
  }

  the.values <- rbinom(n = n * num.experiments, size = num.trials, prob = success.prob)

  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)

  return(res)
}


#' Internal function for Simulation for Linear Regression Data
#'
#' @description This internal function is designed to generate data based on a linear regression model
#' specified by the provided formula. It is not intended for direct usage by end-users.
#' @import data.table
#' @param dat A data.frame or data.table containing the variables referenced in the.formula.
#' @param the.formula A character string specifying the linear regression function, e.g., "lm(0.5 * X + 1.2 * Y + N(0,2))".
#' @param the.variable A character string naming the variable in the generated data.
#' @param n An integer specifying the number of data points to generate for each experiment.
#' @param num.experiments An integer specifying the number of experiments to simulate. Default is 1.
#'
#' @return A data frame with simulated linear regression values based on the given formula.
#'



buildsim.lm <- function(dat, the.formula, the.variable, n, num.experiments = 1){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt <- data.table::is.data.table(dat)
  data.table::setDT(dat)

  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]

  closed.parens <- which(the.chars == ")")
  last.closing.paren <- closed.parens[length(closed.parens)]

  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. lm(0.5 * X + 1.2 * Y + N(0,2)).")
  }

  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. logistic(0.5 * X + 1.2 * Y).")
  }

  if(last.closing.paren < first.open.paren){
    stop("Error:  Distribution must end with a closing ), e.g. logistic(0.5 * X + 1.2 * Y)")
  }

  rhs <- paste(the.chars[(first.open.paren + 1):(last.closing.paren-1)], collapse = "")

  pieces.rhs <- trimws(strsplit(x = rhs, split = "+", fixed = T)[[1]])

  num.pieces.rhs <- length(pieces.rhs)

  if(num.pieces.rhs < 2){
    stop("Error:  The right side of the linear regression formula must include fixed terms and a specification of the residual error, such as lm(0.5 * X + 1.2 * Y + N(0,1))")
  }

  fixed.component <- paste(pieces.rhs[1:(num.pieces.rhs - 1)], collapse = " + ")

  bx <- dat[, eval(parse(text = fixed.component))]

  the.residuals <- identify.distribution(dat = dat, the.step = sprintf("residual ~ %s", pieces.rhs[num.pieces.rhs]), n = n, num.experiments = num.experiments)$residual

  the.values <- bx + the.residuals

  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)

  if(!is.dt){
    data.table::setDF(dat)
  }

  return(res)
}


#' Internal function for Simulation for Logistic Regression Data
#'
#' @description This internal function is designed to generate data based on a logistic regression model
#' specified by the provided formula. It is not intended for direct usage by end-users.
#'
#' @param dat A data.frame or data.table containing the variables referenced in the.formula.
#' @param the.formula A character string specifying the logistic regression function, e.g., "logistic(0.5 * X + 1.2 * Y)".
#' @param the.variable A character string naming the variable in the generated data.
#' @param n An integer specifying the number of data points to generate for each experiment.
#' @param num.experiments An integer specifying the number of experiments to simulate. Default is 1.
#'
#' @return A data frame with simulated logistic regression values based on the given formula.
#'


buildsim.logistic <- function(dat, the.formula, the.variable, n, num.experiments = 1){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  is.dt <- data.table::is.data.table(dat)
  data.table::setDT(dat)

  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]

  closed.parens <- which(the.chars == ")")
  last.closing.paren <- closed.parens[length(closed.parens)]

  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. logistic(0.5 * X + 1.2 * Y).")
  }

  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. logistic(0.5 * X + 1.2 * Y).")
  }

  if(last.closing.paren < first.open.paren){
    stop("Error:  Distribution must end with a closing ), e.g. logistic(0.5 * X + 1.2 * Y)")
  }

  rhs <- trimws(paste(the.chars[(first.open.paren + 1):(last.closing.paren-1)], collapse = ""))

  bx <- dat[, eval(parse(text = rhs))]

  the.values <- runif(n = dat[, .N]) < (exp(bx)/(1+exp(bx)))

  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)

  if(!is.dt){
    data.table::setDF(dat)
  }

  return(res)
}

#' Internal Simulation for Normally Distributed Data
#'
#' @description This internal function generates data based on a normal distribution specified
#' by the provided formula. It is not intended for direct usage by end-users.
#'
#' @param the.formula A character string specifying the normal distribution function, e.g., "N(0,1)".
#' @param the.variable A character string naming the variable in the generated data.
#' @param n An integer specifying the number of data points to generate for each experiment.
#' @param num.experiments An integer specifying the number of experiments to simulate.
#'
#' @return A data frame with simulated normally distributed values based on the given formula.
#'


buildsim.normal <- function(the.formula, the.variable, n, num.experiments){

  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]

  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. N(0,1).")
  }

  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. N(0,1).")
  }

  open.parens <- which(the.chars == ")")
  last.open.paren <- open.parens[length(open.parens)]

  first.comma <- first.open.paren + which(x = the.chars[(first.open.paren + 1):(last.open.paren - 1)] == ",")[1]

  if(first.comma == first.open.paren + 1){
    stop("The Normal function must be specified like N(0, 1), filling in numbers for the mean and SD.")
  }

  the.mean <- as.numeric(trimws(paste(the.chars[(first.open.paren + 1):(first.comma -1)], collapse = "")))
  the.sd <- as.numeric(trimws(paste(the.chars[(first.comma + 1):(last.open.paren -1)], collapse = "")))

  the.values <- rnorm(n = n * num.experiments, mean = the.mean, sd = the.sd)

  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)

  return(res)
}


#' Internal function for Simulation for Poisson Distributed Data
#'
#' @description This internal function generates data based on a Poisson distribution specified
#' by the provided formula. It is not intended for direct usage by end-users.
#'
#' @param the.formula A character string specifying the Poisson distribution function, e.g., "poisson(3)".
#' @param the.variable A character string naming the variable in the generated data.
#' @param n An integer specifying the number of data points to generate for each experiment.
#' @param num.experiments An integer specifying the number of experiments to simulate.
#'
#' @return A data frame with simulated Poisson distributed values based on the given formula.
#'


buildsim.poisson <- function(the.formula, the.variable, n, num.experiments){
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]

  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. poisson(3).")
  }

  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. poisson(3).")
  }

  close.parens <- which(the.chars == ")")
  last.close.paren <- close.parens[length(close.parens)]

  the.mean <- as.numeric(trimws(paste(the.chars[(first.open.paren + 1):(last.close.paren -1)], collapse = "")))

  the.values <- rpois(n = n * num.experiments, lambda = the.mean)

  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)

  return(res)
}


#' Internal function for Sampling Function
#'
#' @description This internal function generates samples based on the specified distributions and probabilities.
#' It is not intended for direct usage by end-users.
#'
#' @param the.formula A character string specifying the sampling formula, e.g., "sample(('Red', 'Green', 'Blue'), (0.5, 0.3, 0.2))".
#' @param the.variable A character string naming the variable in the generated data.
#' @param n An integer specifying the number of data points to generate for each experiment.
#' @param num.experiments An integer specifying the number of experiments to simulate.
#' @param value.split A character used to split the values in the sample.
#' @param symbol.open.paren A character specifying the opening parenthesis.
#' @param symbol.close.paren A character specifying the closing parenthesis.
#'
#' @return A data frame with sampled values based on the given formula.

buildsim.sample <- function(the.formula, the.variable, n, num.experiments, value.split = ",", symbol.open.paren = "(", symbol.close.paren = ")"){

  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  open.parens <- which(the.chars == symbol.open.paren)

  if(length(open.parens) < 3){
    stop("The sample function must be specified like sample(('Red', 'Green', 'Blue'), (0.5, 0.3, 0.2)), filling in possible values for the first set of parentheses and probabilities for the second set.  There must be at least 3 open paren characters ( in the statement.")
  }

  num.open.parens <- cumsum(the.chars == symbol.open.paren) - cumsum(the.chars == symbol.close.paren)
  open.1 <- which(num.open.parens == 1)
  open.2 <- which(num.open.parens == 2)

  begin.x <- min(open.2)
  end.x <- min(open.1[open.1 > begin.x])

  x <- eval(parse(text = sprintf("c%s", substring(text = the.formula, first = begin.x, last = end.x))))

  begin.prob <- min(open.2[open.2 > end.x])
  end.prob <- min(open.1[open.1 > begin.prob])
  prob <- eval(parse(text = sprintf("c%s", substring(text = the.formula, first = begin.prob, last = end.prob))))

  the.values <- sample(x = x, size = n * num.experiments, replace = T, prob = prob)

  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)

  return(res)
}


#' Internal function for Simulation for Uniform Distributed Data
#'
#' @description This internal function generates data based on a Uniform distribution specified
#' by the provided formula. It is not intended for direct usage by end-users.
#'
#' @param the.formula A character string specifying the Uniform distribution function, e.g., "U(0,1)".
#' @param the.variable A character string naming the variable in the generated data.
#' @param n An integer specifying the number of data points to generate for each experiment.
#' @param num.experiments An integer specifying the number of experiments to simulate.
#'
#' @return A data frame with simulated Uniform distributed values based on the given formula.

buildsim.uniform <- function(the.formula, the.variable, n, num.experiments = 1){
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]

  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. U(0,1).")
  }

  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. U(0,1).")
  }

  open.parens <- which(the.chars == ")")
  last.open.paren <- open.parens[length(open.parens)]

  first.comma <- first.open.paren + which(x = the.chars[(first.open.paren + 1):(last.open.paren - 1)] == ",")[1]

  if(first.comma == first.open.paren + 1){
    stop("The Uniform function must be specified like U(0, 1), filling in numbers for the min and max.")
  }

  the.min <- as.numeric(trimws(paste(the.chars[(first.open.paren + 1):(first.comma -1)], collapse = "")))
  the.max <- as.numeric(trimws(paste(the.chars[(first.comma + 1):(last.open.paren -1)], collapse = "")))


  the.values <- runif(n = n * num.experiments, min = the.min, max = the.max)

  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)

  return(res)
}

#' Internal function for Distribution Identification
#'
#' @description This internal function identifies the type of distribution based on a given formula and simulates
#' data accordingly. It is not intended for direct usage by end-users.
#'
#' @param dat Optional data table for generating data.
#' @param the.step A character string specifying the formula for simulation.
#' @param n An integer specifying the number of data points to generate.
#' @param num.experiments An integer specifying the number of experiments to simulate.
#' @param step.split A character indicating the delimiter for splitting the step formula.
#' @param value.split A character used to split values in certain distributions.
#'
#' @return A data table with simulated values based on the identified distribution.


identify.distribution <- function(dat = NULL, the.step, n, num.experiments, step.split = "~", value.split = ","){
  pieces.step <- trimws(strsplit(x = the.step, split = step.split)[[1]])

  the.variable <- pieces.step[1]
  the.formula <- pieces.step[2]
  the.chars <- strsplit(x = the.formula, split = "")[[1]]

  first.open.paren <- which(the.chars == "(")[1]

  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. N(0,1).")
  }

  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. N(0,1).")
  }

  the.function <- tolower(paste(the.chars[1:(first.open.paren - 1)], collapse = ""))

  if(the.function %in% c("n", "norm", "normal")){
    sim.variable <- buildsim.normal(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("u", "unif", "uniform")){
    sim.variable <- buildsim.uniform(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("pois", "poisson")){
    sim.variable <- buildsim.poisson(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("binary")){
    sim.variable <- buildsim.binary(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("bin", "binomial")){
    sim.variable <- buildsim.binomial(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("sample", "categorical")){
    sim.variable <- buildsim.sample(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments, value.split = value.split)
  }
  if(the.function %in% c("logistic")){
    sim.variable <- buildsim.logistic(dat = dat, the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("lm", "linear.regression")){
    sim.variable <- buildsim.lm(dat = dat, the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }

  if(!is.null(dat)){
    res <- data.table::data.table(dat, sim.variable)
  }
  if(is.null(dat)){
    res <- sim.variable
  }

  return(res)
}

#' Internal function for Chi-Squared Test of Goodness of Fit
#'
#' @description Computes the chi-squared test for the given categorical data.
#'
#' @param x A categorical variable.
#' @param hypothesized.probs Hypothesized probabilities for each level of x.
#' @param correct A logical indicating if a continuity correction should be applied.
#' @return A data frame containing the test statistic, degrees of freedom, and p-value.

internal.chisq.test.gf <- function(x, hypothesized.probs = NULL, correct = TRUE){

  tab.unsorted <- table(x)

  tab <- tab.unsorted[order(names(tab.unsorted))]

  if(is.null(hypothesized.probs)){
    hypothesized.probs <- rep.int(x = 1/length(tab), times = length(tab))
  }
  the.test <- chisq.test(x = tab, correct = correct, p = hypothesized.probs)

  res <- data.frame(statistic = the.test$statistic, df = the.test$parameter, p.value = the.test$p.value)

  return(res)
}


#' Internal function for Chi-Squared Test of Independence
#'
#' @description Computes the chi-squared test for the given data.
#'
#' @param the.data The data table.
#' @param group.name Group variable name.
#' @param value.name Value variable name.
#' @param correct A logical indicating if a continuity correction should be applied.
#' @return A data frame containing the test statistic, degrees of freedom, and p-value.

internal.chisq.test.ind <- function(the.data, group.name, value.name, correct = TRUE){

  data.table::setDT(the.data)
  tab <- table(the.data[, base::get(value.name)], the.data[, base::get(group.name)])

  the.test <- chisq.test(x = tab, correct = correct, p = hypothesized.probs)

  res <- data.frame(statistic = the.test$statistic, df = the.test$parameter, p.value = the.test$p.value)

  return(res)
}


#' Internal function for One-sample Proportions Test
#'
#' @description Computes the test for proportion for the given binary data.
#'
#' @param x A binary variable.
#' @param p Null hypothesis value for proportion. Default is NULL.
#' @param alternative A character string specifying the alternative hypothesis. One of \code{"two.sided"}, \code{"less"}, or \code{"greater"}. Default is \code{"two.sided"}.
#' @param conf.level A numeric value between 0 and 1 indicating the confidence level for the interval estimate of the proportion. Default is 0.95.
#' @param correct A logical indicating if Yates' continuity correction should be applied for the test. Default is TRUE.
#' @return A data frame with test results.

internal.prop.test <- function(x, p = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = TRUE){

  the.test <- prop.test(x = sum(x, na.rm = T), n = length(!is.na(x)), p = p, alternative = alternative, conf.level = conf.level, correct = correct)

  res <- data.frame(statistic = the.test$statistic, df = the.test$parameter, p.value = the.test$p.value, lower.ci = the.test$conf.int[1], upper.ci = the.test$conf.int[2], estimate = the.test$estimate, null.value = the.test$null.value, alternative = the.test$alternative, method = the.test$method)

  return(res)
}


#' Internal function for Two-sample Proportions Test
#'
#' @description Computes the test for proportions for two given binary variables.
#'
#' @param x First binary variable.
#' @param y Second binary variable.
#' @param p Null hypothesis value for proportion. Default is NULL.
#' @param alternative A character string specifying the alternative hypothesis. One of \code{"two.sided"}, \code{"less"}, or \code{"greater"}. Default is \code{"two.sided"}.
#' @param conf.level A numeric value between 0 and 1 indicating the confidence level for the interval estimate of the proportion. Default is 0.95.
#' @param correct A logical indicating if Yates' continuity correction should be applied for the test. Default is TRUE.
#' @param na.rm A logical indicating if NA values should be removed. Default is TRUE.
#' @return A data frame with test results.

internal.prop2.test <- function(x, y, p = NULL,  alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = TRUE, na.rm = T){

  the.test <- prop.test(x = c(sum(x, na.rm = na.rm), sum(y, na.rm = na.rm)), n = c(sum(!is.na(x)), sum(!is.na(y))), p = p, alternative = alternative, conf.level = conf.level, correct = correct)

  if(is.null(the.test$null.value)){
    the.test$null.value <- 0
  }

  res <- data.frame(statistic = the.test$statistic, df = the.test$parameter, p.value = the.test$p.value, lower.ci = the.test$conf.int[1], upper.ci = the.test$conf.int[2], estimate = the.test$estimate[1] - the.test$estimate[2], x.estimate = the.test$estimate[1], y.estimate = the.test$estimate[2], null.value = the.test$null.value, alternative = the.test$alternative, method = the.test$method)

  return(res)
}


#' Internal function for Quantile, Mean, and Standard Deviation Calculation
#'
#' @description Computes the specified quantiles, mean, and standard deviation for the given data.
#'
#' @param x A numeric vector.
#' @param the.quantiles A numeric vector of quantile values.
#' @param na.rm A logical indicating if missing values should be removed.
#' @return A data table with summary statistics.

internal.quantiles.mean.sd <- function(x, the.quantiles, na.rm = T){

  if(is.null(the.quantiles) | length(the.quantiles) == 0){
    the.quantiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  }
  stat.summary <- data.table::as.data.table(x = t(quantile(x = x, probs = the.quantiles, na.rm = na.rm)))

  names(stat.summary) <- gsub(pattern = "%", replacement = "", x = sprintf("q.%s", names(stat.summary)), fixed = T)

  stat.summary[, eval("mean") := mean(x, na.rm = na.rm)]
  stat.summary[, eval("st.error") := sd(x, na.rm = na.rm)]

  return(stat.summary[])
}



#' Internal function for Summary Statistics of Linear Model
#'
#' @description Computes the summary statistics for the linear model fit on the given data.
#'
#' @param the.data The data table.
#' @param the.formula A formula specifying the linear model.

internal.statistics.one.lm <- function(the.data, the.formula){

  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }

  mod <- lm(formula = the.formula, data = the.data)

  summary.mod <- summary(mod)
  coef.table <- data.table::as.data.table(x = summary.mod$coefficients, keep.rownames = TRUE)
  names(coef.table)[names(coef.table) == "rn"] <- "Coefficient"

  fstats <- data.table::as.data.table(t(summary.mod$fstatistic))
  fstats$p <- pf(q = fstats$value, df1 = fstats$numdf, df2 = fstats$dendf, lower.tail = FALSE)

  rse <- sqrt(sum(summary.mod$residuals^2) / summary.mod$df[2])

  summary.stats <- data.table::data.table(sigma = summary.mod$sigma, df = summary.mod$df[2], rse = rse, r.squared = summary.mod$r.squared, adj.r.squared = summary.mod$adj.r.squared, fstatistic = fstats$value, f.numdf = fstats$numdf, f.dendf = fstats$dendf, f.pvalue = fstats$p)

  res <- list(coef.table = coef.table, summary.stats = summary.stats)
}



#' Internal function for Summary Statistics of Linear Model
#'
#' @description Computes the summary statistics for the linear model fit on the given data.
#'
#' @param the.data The data table.
#' @param the.formula A formula specifying the linear model.
#' @return A list containing the coefficient table and summary statistics.

internal.statistics.onelm <- function(the.data, the.formula){

  mod <- lm(formula = the.formula, data = the.data)

  summary.mod <- summary(mod)
  coef.table <- data.table::as.data.table(x = summary.mod$coefficients, keep.rownames = T)
  names(coef.table)[names(coef.table) == "rn"] <- "Coefficient"

  fstats <- as.data.table(t(summary.mod$fstatistic))
  fstats$p = pf(q = fstats$value, df1 = fstats$numdf, df2 = fstats$dendf, lower.tail = F)

  rse <- sqrt(sum(summary.mod$residuals^2) / summary.mod$df[2])

  summary.stats <- data.table(sigma = summary.mod$sigma, df = summary.mod$df[2], rse = rse, r.squared = summary.mod$r.squared, adj.r.squared = summary.mod$adj.r.squared, fstatistic = fstats$value, f.numdf = fstats$numdf, f.dendf = fstats$dendf, f.pvalue = fstats$p)

  res <- list(coef.table = coef.table, summary.stats = summary.stats)
}


#' Internal function for Summary Statistics of Logistic Regression
#'
#' @description Computes the summary statistics for the logistic regression fit on the given data.
#'
#' @param the.data The data table.
#' @param the.formula A formula specifying the logistic regression model.
#' @return A list containing the coefficient table and summary statistics.


internal.statistics.one.logistic <- function(the.data, the.formula){
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }

  mod <- glm(formula = the.formula, family = "binomial", data = the.data)

  summary.mod <- summary(mod)
  coef.table <- data.table::as.data.table(x = summary.mod$coefficients, keep.rownames = TRUE)
  names(coef.table)[names(coef.table) == "rn"] <- "Coefficient"

  summary.stats <- data.table::data.table(deviance = summary.mod$deviance, aic = summary.mod$aic, df.residual = summary.mod$df.residual, null.deviance = summary.mod$null.deviance, df.null = summary.mod$df.null, iter = summary.mod$iter, dispersion = summary.mod$dispersion)

  res <- list(coef.table = coef.table, summary.stats = summary.stats)

}






#' Internal function for Two-sample t-test
#'
#' @description Computes the two-sample t-test for the given data.
#'
#' @param x First numeric vector.
#' @param y Second numeric vector.
#' @param alternative A character string specifying the alternative hypothesis. One of \code{"two.sided"}, \code{"less"}, or \code{"greater"}. Default is \code{"two.sided"}.
#' @param mu A number indicating the true value of the mean difference (relevant if \code{paired = TRUE}). Default is 0.
#' @param paired A logical indicating whether you want a paired t-test. Default is FALSE.
#' @param var.equal A logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used. Default is FALSE.
#' @param conf.level A numeric value between 0 and 1 indicating the confidence level for the interval estimate of the mean difference. Default is 0.95.
#' @return A data frame with test results.

internal.t2.test <- function(x, y, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95){
  the.test <- t.test(x = x, y = y, alternative = alternative, mu = mu, paired = paired, var.equal = var.equal, conf.level = conf.level)

  res <- data.frame(statistic = the.test$statistic, df = the.test$parameter, p.value = the.test$p.value, lower.ci = the.test$conf.int[1], upper.ci = the.test$conf.int[2], estimate = the.test$estimate[1] - the.test$estimate[2], x.estimate = the.test$estimate[1], y.estimate = the.test$estimate[2], null.value = the.test$null.value, alternative = the.test$alternative, method = the.test$method)

  return(res)
}


#' Internal function for Quantiles Calculation
#'
#' @description Computes the specified quantiles for the given data.
#'
#' @param x A numeric vector.
#' @param probs A numeric vector of quantile values.
#' @param na.rm A logical indicating if missing values should be removed.
#' @return A data table with quantile values.

quantile.dt <- function(x, probs, na.rm = TRUE){
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }

  y <- quantile(x = x, probs = probs, na.rm = na.rm)

  res <- data.table::as.data.table(t(as.matrix(y)))
  names(res) <- gsub(pattern = "%", replacement = "", x = sprintf("q.%s", names(res)), fixed = TRUE)

  return(res)
}


#' Internal function for Normal Distribution Simulation
#'
#' @description Simulates data from normal distributions given specified parameters.
#'
#' @param n.values A numeric vector indicating the number of values to be simulated for each normal distribution.
#' @param mean.values A numeric vector indicating the mean values for each normal distribution.
#' @param sd.values A numeric vector indicating the standard deviation values for each normal distribution.
#' @param num.experiments A single integer indicating the number of experiments to simulate. Default is 1.
#' @param variable.names A character vector with names for the variables. If NULL, default names "x1", "x2", ... will be used.
#' @param seed An integer to set as the seed for reproducibility. Default is 1978.
#' @param vstr A character string specifying the RNG version. Default is "3.6".
#' @return A data.table containing the simulated data.
#'
#' Note: This function is intended for internal use and is not exported.

sim.norm <- function(n.values, mean.values, sd.values, num.experiments = 1, variable.names = NULL, seed = 1978, vstr = 3.6){

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required.")
  }

  n.values <- pmax(1, floor(n.values), na.rm = TRUE)

  num.experiments <- max(1, floor(num.experiments), na.rm = TRUE)

  if(sum(sd.values <= 0) > 0){
    stop("Error:  sd.values must be a vector of positive numbers.")
  }

  RNGversion(vstr = vstr)
  set.seed(seed = seed)

  num.variables <- length(n.values)

  if(is.null(variable.names)){
    variable.names <- sprintf("x%d", 1:num.variables)
  }
  list.values <- list()
  list.experiment <- list()
  list.variables <- list()
  for(i in 1:num.variables){
    list.values[[i]] <- rnorm(n = n.values[i] * num.experiments, mean = mean.values[i], sd = sd.values[i])
    list.experiment[[i]] <- rep.int(x = 1:num.experiments, times = n.values[i])
    list.variables[[i]] <- rep.int(x = variable.names[i], times = n.values[i] * num.experiments)
  }

  dat <- data.table::data.table(experiment = unlist(list.experiment), variable = unlist(list.variables), value = unlist(list.values))

  data.table::setorderv(x = dat, cols = "experiment", order = 1L)

  return(dat)
}

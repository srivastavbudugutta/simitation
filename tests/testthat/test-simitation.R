library(testthat)


# t-test full simulation study

describe("Testing analyze.simstudy.t function", {

  study.t <- simstudy.t(n = 25, mean = 0.3, sd = 1, num.experiments = 1000,
                        alternative = "greater", mu = 0, conf.level = 0.95,
                        the.quantiles = c(0.025, 0.975), experiment.name = "experiment",
                        value.name = "x", seed = 817)

  # Check if the function returns the expected components
  expect_named(study.t, c("simdat.t", "test.statistics.t", "sim.analysis.t"))

  # Check if "simdat.t" data frame has the right columns
  expect_equal(colnames(study.t$simdat.t), c("experiment", "x"))


  # Check if "test.statistics.t" data frame has the right columns
  expect_equal(colnames(study.t$test.statistics.t),
               c("experiment", "statistic", "df", "p.value", "lower.ci", "upper.ci",
                 "estimate", "null.value", "alternative", "method"))


  # Validate summaries in "sim.analysis.t"
  expect_true(all(names(study.t$sim.analysis.t) %in% c("estimate.summary",
                                                       "stat.summary",
                                                       "p.value.summary",
                                                       "ci.limit.summary")))

})


# two sample t-test Full simulation study


describe("Testing simstudy.t2 function", {

  study.t2 <- simstudy.t2(nx = 30, ny = 40, meanx = 0, meany = 0.2, sdx = 1,
                          sdy = 1,
                          num.experiments = 500, alternative = "less", mu = 0,
                          conf.level = 0.9,
                          the.quantiles = c(0.1, 0.5, 0.9),
                          experiment.name = "experiment_id",
                          group.name = "category", x.value = "a",
                          y.value = "b", value.name = "measurement", seed = 41)

  # Check if the function returns a list
  expect_true(is.list(study.t2))

  # Check if the list has the expected named components
  expect_named(study.t2, c("simdat.t2", "test.statistics.t2",
                           "sim.analysis.t2"))

  # Check if simdat.t2 is a data frame with the correct columns
  expect_s3_class(study.t2$simdat.t2, "data.frame")
  expect_equal(colnames(study.t2$simdat.t2), c("experiment_id", "category",
                                               "measurement"))

  # Check if test.statistics.t2 is a data frame with the correct columns
  expect_s3_class(study.t2$test.statistics.t2, "data.frame")
  expected_cols_t2 <- c("experiment_id", "statistic", "df", "p.value",
                        "lower.ci",
                        "upper.ci", "estimate", "x.estimate",
                        "y.estimate", "null.value",
                        "alternative", "method")
  expect_equal(colnames(study.t2$test.statistics.t2), expected_cols_t2)

  # Check for sim.analysis.t2 components
  expect_true(is.list(study.t2$sim.analysis.t2))
  expect_named(study.t2$sim.analysis.t2, c("estimate.summary", "stat.summary",
                                           "df.summary", "p.value.summary",
                                           "ci.limit.summary"))

  # Check the contents of sim.analysis.t2 sub-components except for p.value.summary
  sub_components <- c("estimate.summary", "stat.summary", "df.summary",
                      "ci.limit.summary")
  for (component in sub_components) {
    expect_s3_class(study.t2$sim.analysis.t2[[component]], "data.frame")
    expect_equal(colnames(study.t2$sim.analysis.t2[[component]]), c("q.10",
                                                                    "q.50",
                                                                    "q.90",
                                                                    "mean",
                                                                    "st.error"))
  }

  # Check for p.value.summary separately
  expect_s3_class(study.t2$sim.analysis.t2$p.value.summary, "data.frame")
  expect_equal(colnames(study.t2$sim.analysis.t2$p.value.summary),
               c("reject.proportion", "non.reject.proportion"))

})



# Testing one sample test of proportion test - Full simulation study


describe("Testing simstudy.prop function", {

  study.prop <- simstudy.prop(n = 30, p.actual = 0.42,
                              p.hypothesized = 0.5, num.experiments = 500,
                              alternative = "less", conf.level = 0.92,
                              correct = T,
                              the.quantiles = c(0.04, 0.5, 0.96),
                              experiment.name = "simulation_id",
                              value.name = "success", seed = 8001)


  # Checking the names of the list
  expected_list_names <- c("simdat.prop", "test.statistics.prop",
                           "sim.analysis.prop")
  expect_equal(names(study.prop), expected_list_names)

  # Check simdat.prop structure
  expect_s3_class(study.prop$simdat.prop, "data.frame")
  expect_equal(colnames(study.prop$simdat.prop), c("simulation_id", "success"))

  # Check test.statistics.prop structure
  expect_s3_class(study.prop$test.statistics.prop, "data.frame")
  expected_cols_test_stats <- c("simulation_id", "statistic", "df",
                                "p.value", "lower.ci",
                                "upper.ci", "estimate", "null.value",
                                "alternative", "method")
  expect_equal(colnames(study.prop$test.statistics.prop), expected_cols_test_stats)

  # Check sim.analysis.prop structure
  expected_sublist_names <- c("estimate.summary", "stat.summary",
                              "p.value.summary", "ci.limit.summary")
  expect_equal(names(study.prop$sim.analysis.prop), expected_sublist_names)

  expect_s3_class(study.prop$sim.analysis.prop$estimate.summary, "data.frame")
  expect_equal(colnames(study.prop$sim.analysis.prop$estimate.summary),
               c("q.4", "q.50", "q.96", "mean", "st.error"))


})


# Testing two sample test of difference of proportion test - Full simulation study

describe("Testing simstudy.prop2 function", {

  study.prop2 <- simstudy.prop2(nx = 30, ny = 40, px = 0.5, py = 0.55,
                                num.experiments = 1000,
                                p = NULL, alternative = "less",
                                conf.level = 0.95, correct = T,
                                the.quantiles = c(0.025, 0.5, 0.975),
                                experiment.name = "sim",
                                group.name = "treatment",
                                x.value = "treatment_1",
                                y.value = "treatment_2",
                                value.name = "correct_answer", seed = 904)


  # Check the main list elements
  expect_named(study.prop2, c("simdat.prop2",
                              "test.statistics.prop2", "sim.analysis.prop2"))

  # Check the structure of simdat.prop2
  expect_s3_class(study.prop2$simdat.prop2, "data.frame")
  expect_named(study.prop2$simdat.prop2, c("sim", "treatment", "correct_answer"))

  # Check the structure of test.statistics.prop2
  expect_s3_class(study.prop2$test.statistics.prop2, "data.frame")
  expect_named(study.prop2$test.statistics.prop2,
               c("sim", "statistic", "df", "p.value", "lower.ci",
                 "upper.ci", "estimate",
                 "x.estimate", "y.estimate", "null.value", "alternative",
                 "method"))

  # Check the structure of sim.analysis.prop2 and its sub-elements
  expect_named(study.prop2$sim.analysis.prop2,
               c("estimate.summary", "stat.summary", "p.value.summary",
                 "df.summary",
                 "ci.limit.summary"))

  # Checking the sub-elements of sim.analysis.prop2
  expect_s3_class(study.prop2$sim.analysis.prop2$estimate.summary,
                  "data.frame")
  expect_named(study.prop2$sim.analysis.prop2$estimate.summary,
               c("q.2.5", "q.50", "q.97.5", "mean", "st.error"))

  expect_s3_class(study.prop2$sim.analysis.prop2$stat.summary, "data.frame")
  expect_named(study.prop2$sim.analysis.prop2$stat.summary,
               c("q.2.5", "q.50", "q.97.5", "mean", "st.error"))

  expect_s3_class(study.prop2$sim.analysis.prop2$p.value.summary, "data.frame")
  expect_named(study.prop2$sim.analysis.prop2$p.value.summary,
               c("reject.proportion", "non.reject.proportion"))

  expect_s3_class(study.prop2$sim.analysis.prop2$df.summary, "data.frame")
  expect_named(study.prop2$sim.analysis.prop2$df.summary,
               c("q.2.5", "q.50", "q.97.5", "mean", "st.error"))

  expect_s3_class(study.prop2$sim.analysis.prop2$ci.limit.summary, "data.frame")
  expect_named(study.prop2$sim.analysis.prop2$ci.limit.summary,
               c("q.2.5", "q.50", "q.97.5", "mean", "st.error"))

})


# Testing X^2 test of the goodness of fit - full simulation study

describe("Testing simstudy.chisq.test.gf function", {

  study.chisq.gf <- simstudy.chisq.test.gf(n = 75,
                                           values = LETTERS[1:4],
                                           actual.probs = c(0.3, 0.3, 0.2, 0.2),
                                           hypothesized.probs =
                                             rep.int(x = 0.25, times = 4),
                                           num.experiments = 40,
                                           conf.level = 0.95,
                                           correct = F,
                                           the.quantiles = c(0.25, 0.75),
                                           experiment.name = "experiment_id",
                                           value.name = "classification",
                                           seed = 61)

  # Check the structure of study.chisq.gf
  expect_named(study.chisq.gf, c("simdat", "test.statistics", "sim.analysis"))

  # Validate the simdat dataframe
  expect_s3_class(study.chisq.gf$simdat, "data.frame")
  expect_named(study.chisq.gf$simdat, c("experiment_id", "classification"))

  # Check that simdat dataframe contains correct number of rows
  expect_equal(nrow(study.chisq.gf$simdat), 75 * 40)

  # Validate the test.statistics dataframe
  expect_s3_class(study.chisq.gf$test.statistics, "data.frame")
  expect_named(study.chisq.gf$test.statistics,
               c("experiment_id", "statistic", "df", "p.value"))

  # Check values within expected range
  expect_true(all(study.chisq.gf$test.statistics$statistic >= 0))
  expect_true(all(study.chisq.gf$test.statistics$p.value >= 0 & study.chisq.gf$test.statistics$p.value <= 1))


  # Check the structure of stat.summary
  expect_named(study.chisq.gf$sim.analysis$stat.summary,
               c("q.25", "q.75", "mean", "st.error"))

  # Check values within expected range
  expect_true(all(study.chisq.gf$sim.analysis$stat.summary$mean >= 0))
  expect_true(all(study.chisq.gf$sim.analysis$stat.summary$st.error >= 0))

  # Check the structure of p.value.summary
  expect_named(study.chisq.gf$sim.analysis$p.value.summary,
               c("reject.proportion", "non.reject.proportion"))

  # Validate proportions are between 0 and 1
  expect_equal(sum(study.chisq.gf$sim.analysis$p.value.summary$reject.proportion,
                   study.chisq.gf$sim.analysis$p.value.summary$non.reject.proportion), 1)


})


# Testing X^2 test of Independence - full simulation study

describe("Testing simstudy.chisq.test.ind function", {
  n <- c(50, 75, 100)
  values <- LETTERS[1:4]
  group.names <- sprintf("group_%d", 1:3)
  probs <- matrix(data = c(0.25, 0.25, 0.25, 0.25, 0.4, 0.3, 0.2, 0.1, 0.2, 0.4, 0.2,
                           0.2), nrow = length(n), byrow = T)

  # Suppress warnings and execute the function
  study.chisq.ind <- suppressWarnings({
    simstudy.chisq.test.ind(n = c(30, 35, 40), values = LETTERS[1:4],
                            probs = probs, num.experiments = 1000,
                            conf.level = 0.95, correct = T,
                            the.quantiles = c(0.025, 0.975),
                            experiment.name = "exp_id",
                            group.name = "treatment_group",
                            group.values = sprintf("group_%d", 1:3),
                            value.name = "category", seed = 77)
  })

  # Check the structure of study.chisq.ind
  expect_named(study.chisq.ind, c("simdat.chisq.ind", "test.statistics.chisq.test.ind", "sim.analysis"))

  # Check the structure and properties of each of the elements for simdat.chisq.ind
  expect_s3_class(study.chisq.ind$simdat.chisq.ind, "data.frame")
  expect_named(study.chisq.ind$simdat.chisq.ind, c("exp_id", "treatment_group", "category"))

  # For test.statistics.chisq.test.ind
  expect_s3_class(study.chisq.ind$test.statistics.chisq.test.ind, "data.frame")
  expect_named(study.chisq.ind$test.statistics.chisq.test.ind,
               c("exp_id", "statistic", "df", "p.value"))


  # For sim.analysis$stat.summary
  expect_s3_class(study.chisq.ind$sim.analysis$stat.summary, "data.frame")
  expect_named(study.chisq.ind$sim.analysis$stat.summary,
               c("q.2.5", "q.97.5", "mean", "st.error"))

  # For sim.analysis$p.value.summary
  expect_s3_class(study.chisq.ind$sim.analysis$p.value.summary, "data.frame")
  expect_named(study.chisq.ind$sim.analysis$p.value.summary,
               c("reject.proportion", "non.reject.proportion"))

  # Validate that reject and non.reject proportions sum to 1
  expect_equal(with(study.chisq.ind$sim.analysis$p.value.summary, reject.proportion + non.reject.proportion), 1)

  # Validate that reject and non.reject proportions are between 0 and 1
  expect_true(all(study.chisq.ind$sim.analysis$p.value.summary$reject.proportion >= 0 & study.chisq.ind$sim.analysis$p.value.summary$reject.proportion <= 1))
  expect_true(all(study.chisq.ind$sim.analysis$p.value.summary$non.reject.proportion >= 0 & study.chisq.ind$sim.analysis$p.value.summary$non.reject.proportion <= 1))


})


step.age <- "Age ~ N(45, 10)"
step.female <- "Female ~ binary(0.53)"
step.health.percentile <- "Health.Percentile ~ U(0,100)"
step.exercise.sessions <- "Exercise.Sessions ~ Poisson(2)"
step.diet <- "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))"
step.healthy.lifestyle <- "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"
step.weight <- "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))"
the.steps <- c(step.age, step.female, step.health.percentile, step.exercise.sessions,
               step.diet, step.healthy.lifestyle, step.weight)

# Multivariate studies - linear regression modelling full simulation study

describe("Testing simstudy.lm function", {

  study.lm <- simstudy.lm(the.steps = the.steps, n = 100,
                          num.experiments = 1000,
                          the.formula = Weight ~ Age + Female +
                            Health.Percentile + Exercise.Sessions + Healthy.Lifestyle,
                          conf.level = 0.95, the.quantiles = c(0.25, 0.75),
                          experiment.name = "sim", seed = 11)

  # Check the structure of the output
  expect_named(study.lm, c("the.steps", "simdat", "statistics", "sim.analysis"))

  # Check the structure of the.steps
  expect_true(is.vector(study.lm$the.steps))
  expect_equal(length(study.lm$the.steps), length(the.steps))

  # Check the structure of simdat
  expect_s3_class(study.lm$simdat, "data.table")
  expect_named(study.lm$simdat,
               c("sim", "Age", "Female", "Health.Percentile",
                 "Exercise.Sessions", "Diet", "Healthy.Lifestyle", "Weight"))

  # Check the structure of statistics
  expect_named(study.lm$statistics, c("the.coefs", "summary.stats"))
  expect_named(study.lm$statistics$the.coefs, c("sim", "Coefficient",
                                                "Estimate", "Std. Error",
                                                "t value", "Pr(>|t|)"))

  # Check the structure of sim.analysis
  expect_named(study.lm$sim.analysis, c("lm.estimate.summary", "lm.p.summary", "lm.stats.summary", "fstatistic.p.summary"))
  expect_named(study.lm$sim.analysis$lm.estimate.summary, c("Coefficient", "q.25", "q.75", "mean", "st.error"))
  expect_named(study.lm$sim.analysis$lm.p.summary, c("Coefficient", "reject.proportion", "non.reject.proportion"))
  expect_named(study.lm$sim.analysis$lm.stats.summary, c("stat", "q.25", "q.75", "mean", "st.error"))
  expect_named(study.lm$sim.analysis$fstatistic.p.summary, c("reject.proportion", "non.reject.proportion"))

  # Validate proportions in sim.analysis$lm.p.summary
  expect_true(all(study.lm$sim.analysis$lm.p.summary$reject.proportion >= 0 &
                    study.lm$sim.analysis$lm.p.summary$reject.proportion <= 1))
  expect_true(all(study.lm$sim.analysis$lm.p.summary$non.reject.proportion >= 0 &
                    study.lm$sim.analysis$lm.p.summary$non.reject.proportion <= 1))
})


# Multivariate studies - logistic regression modelling full simulation study

describe("Testing simstudy.logistic function", {

  # Execute the function for logistic regression
  study.logistic <- simstudy.logistic(the.steps = the.steps,
                                      n = 100,
                                      num.experiments = 1000,
                                      the.formula = Healthy.Lifestyle ~ Age +
                                        Female + Health.Percentile +
                                        Exercise.Sessions,
                                      conf.level = 0.95,
                                      the.quantiles = c(0.25, 0.75),
                                      experiment.name = "sim", seed = 11)

  # Check the structure of the output
  expect_named(study.logistic,
               c("the.steps", "simdat", "statistics", "sim.analysis"))

  # Check the structure of the.steps
  expect_true(is.vector(study.logistic$the.steps))
  expect_equal(length(study.logistic$the.steps), length(the.steps))

  # Check the structure of statistics
  expect_named(study.logistic$statistics,
               c("the.coefs", "summary.stats"))
  expect_named(study.logistic$statistics$the.coefs,
               c("sim", "Coefficient", "Estimate", "Std. Error", "z value", "Pr(>|z|)"))

  # Check the structure of sim.analysis
  expect_named(study.logistic$sim.analysis,
               c("logistic.estimate.summary", "logistic.p.summary", "logistic.stats.summary"))
  expect_named(study.logistic$sim.analysis$logistic.estimate.summary,
               c("Coefficient", "q.25", "q.75", "mean", "st.error"))
  expect_named(study.logistic$sim.analysis$logistic.p.summary,
               c("Coefficient", "reject.proportion", "non.reject.proportion"))
  expect_named(study.logistic$sim.analysis$logistic.stats.summary,
               c("stat", "q.25", "q.75", "mean", "st.error"))

  # Validate proportions in sim.analysis$logistic.p.summary
  expect_true(all(study.logistic$sim.analysis$logistic.p.summary$reject.proportion >= 0 &
                    study.logistic$sim.analysis$logistic.p.summary$reject.proportion <= 1))
  expect_true(all(study.logistic$sim.analysis$logistic.p.summary$non.reject.proportion >= 0 &
                    study.logistic$sim.analysis$logistic.p.summary$non.reject.proportion <= 1))
})


library(testthat)
num.experiments = 2
n = 30
mean = 0
sd = 1
seed = 7261
vstr = 3.6


n1 = 1
n2 = 2
mean1 = 0
mean2 = 1
sd1 = 1
sd2 = 1


n.values <- c(10, 10, 10)
mean.values <- c(0, 1, 2)
sd.values <- c(1,1,1)

# testing t-test functions generating data

describe("Testing sim.t function", {

  it("produces expected output", {
    num.experiments = 2
    n = 30
    seed = 2187

    # Generate the simulated data
    simdat.t <- sim.t(n = n, mean = 0.3, sd = 1, num.experiments = num.experiments, seed = seed)

    # Check the structure of the output
    expect_s3_class(simdat.t, "data.frame") # expect the output to be a data.frame

    # Since num.experiments is 2 and n is 30, you'd expect 60 rows
    expect_equal(nrow(simdat.t), n * num.experiments)
    expect_equal(ncol(simdat.t), 2) # expect 2 columns: experiment and x

    # Check the column names
    expect_equal(names(simdat.t), c("experiment", "x"))

  })

})

# t-test statistical testing

describe("Testing sim.t.test function", {

  test.dat <- sim.t(n = 30, mean = 0.3, sd = 1, num.experiments = 2, seed = 2187)
  test.statistics.t <- sim.t.test(simdat.t = test.dat, alternative = "greater", mu = 0, conf.level = 0.95, experiment.name = "experiment", value.name = "x")

  it("produces the expected column names", {
    expect_equal(colnames(test.statistics.t), c("experiment", "statistic", "df", "p.value", "lower.ci", "upper.ci", "estimate", "null.value", "alternative", "method"))
  })

  it("returns 'One Sample t-test' as method", {
    expect_equal(unique(test.statistics.t$method), "One Sample t-test")
  })

  it("returns 'greater' as alternative hypothesis", {
    expect_equal(unique(test.statistics.t$alternative), "greater")
  })

  it("returns 0 as null.value", {
    expect_equal(unique(test.statistics.t$null.value), 0)
  })

})

# t-test analyzing simulated data

describe("Testing analyze.simstudy.t function", {

  test.dat <- sim.t(n = 30, mean = 0.3, sd = 1, num.experiments = 2, seed = 2187)
  test.statistics.t <- sim.t.test(simdat.t = test.dat, alternative = "greater", mu = 0, conf.level = 0.95, experiment.name = "experiment", value.name = "x")
  analysis.t <- analyze.simstudy.t(test.statistics.t = test.statistics.t, conf.level = 0.95, alternative = "greater", the.quantiles = c(0.025, 0.25, 0.25, 0.5, 0.75, 0.975))

  it("produces expected structure in output", {
    expect_named(analysis.t, c("estimate.summary", "stat.summary", "p.value.summary", "ci.limit.summary"))
  })

  it("returns correct reject and non-reject proportions in p.value.summary", {
    expect_equal(analysis.t$p.value.summary$reject.proportion, 0.50)
    expect_equal(analysis.t$p.value.summary$non.reject.proportion, 0.50)
  })

})

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

# two sample t-test Generating Data

describe("Testing sim.t2 function", {

  simdat.t2 <- sim.t2(nx = 30, ny = 40, meanx = 0, meany = 0.2, sdx = 1,
                      sdy = 1,
                      num.experiments = 500,
                      experiment.name = "experiment", group.name = "group",
                      x.value = "x", y.value = "y",
                      value.name = "value", seed = 17)

  # Check if the function returns a data frame
  expect_s3_class(simdat.t2, "data.frame")

  # Check if the data frame has the right columns
  expect_equal(colnames(simdat.t2), c("experiment", "group", "value"))

  # Check if the number of rows matches expectation
  expected_rows <- (30 + 40) * 500
  expect_equal(nrow(simdat.t2), expected_rows)

  # checking mean value of 'value' column
  expect_equal(mean(simdat.t2$value), 0.12, tolerance = 0.05)
})


# two sample t-test Statistical testing
describe("Testing sim.t2.test function", {

  simdat.t2 <- sim.t2(nx = 30, ny = 40, meanx = 0, meany = 0.2, sdx = 1,
                      sdy = 1,
                      num.experiments = 500,
                      experiment.name = "experiment",
                      group.name = "group",
                      x.value = "x", y.value = "y",
                      value.name = "value", seed = 17)

  test.statistics.t2 <- sim.t2.test(simdat.t2 = simdat.t2, alternative = "less",
                                    mu = 0,
                                    conf.level = 0.9,
                                    experiment.name = "experiment",
                                    group.name = "group",
                                    x.value = "x",
                                    y.value = "y",
                                    value.name = "value")

  # Check if the function returns a data frame
  expect_s3_class(test.statistics.t2, "data.frame")

  # Check if the data frame has the right columns
  expected_cols <- c("experiment", "statistic", "df", "p.value", "lower.ci",
                     "upper.ci",
                     "estimate", "x.estimate", "y.estimate", "null.value",
                     "alternative", "method")
  expect_equal(colnames(test.statistics.t2), expected_cols)

  # Check if the 'alternative' column always has the value "less"
  expect_true(all(test.statistics.t2$alternative == "less"))

  # Check if 'method' column always has the value "Welch Two Sample t-test"
  expect_true(all(test.statistics.t2$method == "Welch Two Sample t-test"))
})


# two sample t-test Analyzing simulation study
describe("Testing analyze.simstudy.t2 function", {

  simdat.t2 <- sim.t2(nx = 30, ny = 40, meanx = 0, meany = 0.2, sdx = 1, sdy = 1,
                      num.experiments = 500, experiment.name = "experiment",
                      group.name = "group",
                      x.value = "x", y.value = "y", value.name = "value", seed = 17)

  test.statistics.t2 <- sim.t2.test(simdat.t2 = simdat.t2, alternative = "less", mu = 0,
                                    conf.level = 0.9, experiment.name = "experiment",
                                    group.name = "group",
                                    x.value = "x", y.value = "y",
                                    value.name = "value")

  analysis.t2 <- analyze.simstudy.t2(test.statistics.t2 = test.statistics.t2,
                                     alternative = "less",
                                     conf.level = 0.9,
                                     the.quantiles = c(0.25, 0.5, 0.75))

  # Check if the function returns a list
  expect_true(is.list(analysis.t2))


  # Check if the list has the expected named components
  expect_named(analysis.t2, c("estimate.summary", "stat.summary", "df.summary",
                              "p.value.summary", "ci.limit.summary"))

  # For each summary, check if it is a data frame and has specific columns:
  # Check for estimate.summary
  expect_s3_class(analysis.t2$estimate.summary, "data.frame")
  expect_equal(colnames(analysis.t2$estimate.summary), c("q.25", "q.50", "q.75",
                                                         "mean", "st.error"))

  # Check for stat.summary
  expect_s3_class(analysis.t2$stat.summary, "data.frame")
  expect_equal(colnames(analysis.t2$stat.summary), c("q.25", "q.50", "q.75",
                                                     "mean", "st.error"))

  # Check for df.summary
  expect_s3_class(analysis.t2$df.summary, "data.frame")
  expect_equal(colnames(analysis.t2$df.summary), c("q.25", "q.50", "q.75",
                                                   "mean", "st.error"))

  # Check for p.value.summary
  expect_s3_class(analysis.t2$p.value.summary, "data.frame")
  expect_equal(colnames(analysis.t2$p.value.summary), c("reject.proportion",
                                                        "non.reject.proportion"))

  # Check for ci.limit.summary
  expect_s3_class(analysis.t2$ci.limit.summary, "data.frame")
  expect_equal(colnames(analysis.t2$ci.limit.summary), c("q.25", "q.50",
                                                         "q.75", "mean",
                                                         "st.error"))

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

# Testing one sample test of proportion generating data


describe("Testing sim.prop function", {

  simdat.prop <- sim.prop(n = 30, p = 0.45, num.experiments = 1000,
                          experiment.name = "simulation_id",
                          value.name = "success", seed = 104)

  # Check if the result is a data frame
  expect_s3_class(simdat.prop, "data.frame")

  # Check if the data frame has the expected columns
  expect_equal(colnames(simdat.prop), c("simulation_id", "success"))

  # Check if the 'success' column contains only 0s and 1s, as it represents a success/failure variable
  expect_true(all(simdat.prop$success %in% c(0, 1)))

  # Check the unique values of 'simulation_id' to ensure the correct number of experiments
  expect_equal(length(unique(simdat.prop$simulation_id)), 1000)

  # check if the total number of rows matches expected
  # n multiplied by num.experiments
  expect_equal(nrow(simdat.prop), 30 * 1000)

})

# Testing one sample test of proportion - statistical testing


describe("Testing sim.prop.test function", {

  simdat.prop <- sim.prop(n = 30, p = 0.45, num.experiments = 1000,
                          experiment.name = "simulation_id",
                          value.name = "success", seed = 104)

  test.statistics.prop <- sim.prop.test(simdat.prop = simdat.prop, p = 0.5,
                                        alternative = "two.sided",
                                        conf.level = 0.99, correct = T,
                                        experiment.name = "simulation_id",
                                        value.name = "success")

  # Check if test.statistics.prop is a data frame
  expect_s3_class(test.statistics.prop, "data.frame")

  # Check the column names of test.statistics.prop
  expected_colnames <- c("simulation_id", "statistic", "df",
                         "p.value", "lower.ci", "upper.ci", "estimate",
                         "null.value", "alternative", "method")

  expect_equal(colnames(test.statistics.prop), expected_colnames)


})

# Testing one sample test of proportion test - Analyzing simulation data


describe("Testing analyze.simstudy.prop function", {

  simdat.prop <- sim.prop(n = 30, p = 0.45, num.experiments = 1000,
                          experiment.name = "simulation_id",
                          value.name = "success", seed = 104)

  test.statistics.prop <- sim.prop.test(simdat.prop = simdat.prop, p = 0.5,
                                        alternative = "two.sided",
                                        conf.level = 0.99, correct = T,
                                        experiment.name = "simulation_id",
                                        value.name = "success")

  analysis.prop <- analyze.simstudy.prop(test.statistics.prop = test.statistics.prop,
                                         alternative = "two.sided",
                                         conf.level = 0.99,
                                         the.quantiles = c(0.005, 0.995))

  # Checking the names of the list
  expected_list_names <- c("estimate.summary", "stat.summary", "p.value.summary",
                           "ci.range.summary", "ci.proportion.above.null.summary",
                           "ci.proportion.below.null.summary")

  expect_equal(names(analysis.prop), expected_list_names)

  # For each list element, checking its structure
  expect_s3_class(analysis.prop$estimate.summary, "data.frame")
  expect_equal(colnames(analysis.prop$estimate.summary),
               c("q.0.5", "q.99.5", "mean", "st.error"))

  expect_equal(nrow(analysis.prop$estimate.summary), 1)

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



# Testing two sample test of difference of proportion test - gathering data

describe("Testing sim.prop2 function", {

  simdat.prop2 <- sim.prop2(nx = 30, ny = 40, px = 0.5, py = 0.55,
                            num.experiments = 1000,
                            experiment.name = "sim", group.name = "treatment",
                            x.value = "group_1",
                            y.value = "group_2", value.name = "correct_answer",
                            seed = 3)

  # Check if simdat.prop2 is a data frame
  expect_s3_class(simdat.prop2, "data.frame")

  # Check column names
  expected_colnames <- c("sim", "treatment", "correct_answer")
  expect_equal(colnames(simdat.prop2), expected_colnames)

  # Check the range of 'correct_answer' column
  expect_true(all(simdat.prop2$correct_answer %in% c(0, 1)))

  # Check the number of rows
  expected_nrow <- (30 + 40) * 1000
  expect_equal(nrow(simdat.prop2), expected_nrow)

})


# Testing two sample test of difference of proportion test - statistical testing

describe("Testing sim.prop2.test function", {

  simdat.prop2 <- sim.prop2(nx = 30, ny = 40, px = 0.5, py = 0.55,
                            num.experiments = 1000,
                            experiment.name = "sim", group.name = "treatment",
                            x.value = "group_1",
                            y.value = "group_2", value.name = "correct_answer",
                            seed = 3)

  test.statistics.prop2 <- sim.prop2.test(simdat.prop2 = simdat.prop2, p = NULL,
                                          alternative = "less",
                                          conf.level = 0.95, correct = T,
                                          experiment.name = "sim",
                                          group.name = "treatment",
                                          x.value = "group_1",
                                          y.value = "group_2",
                                          value.name = "correct_answer")

  # Ensure it returns a data frame
  expect_s3_class(test.statistics.prop2, "data.frame")

  # Validate expected column names
  expected_colnames <- c("sim", "statistic", "df", "p.value", "lower.ci",
                         "upper.ci", "estimate",
                         "x.estimate", "y.estimate", "null.value",
                         "alternative", "method")
  expect_equal(colnames(test.statistics.prop2), expected_colnames)

  # Check the alternative values
  expect_equal(unique(test.statistics.prop2$alternative), "less")

  # Validate range for p.value column
  expect_true(all(test.statistics.prop2$p.value >= 0 & test.statistics.prop2$p.value <= 1))

  # Validate range for estimates
  expect_true(all(test.statistics.prop2$x.estimate >= 0 & test.statistics.prop2$x.estimate <= 1))
  expect_true(all(test.statistics.prop2$y.estimate >= 0 & test.statistics.prop2$y.estimate <= 1))



})


# Testing two sample test of difference of proportion test - analyzing simulation data

describe("Testing analyze.simstudy.prop2 function", {
  simdat.prop2 <- sim.prop2(nx = 30, ny = 40, px = 0.5, py = 0.55,
                            num.experiments = 1000,
                            experiment.name = "sim",
                            group.name = "treatment", x.value = "group_1",
                            y.value = "group_2", value.name = "correct_answer",
                            seed = 3)

  test.statistics.prop2 <- sim.prop2.test(simdat.prop2 = simdat.prop2,
                                          p = NULL, alternative = "less",
                                          conf.level = 0.95, correct = T,
                                          experiment.name = "sim",
                                          group.name = "treatment",
                                          x.value = "group_1",
                                          y.value = "group_2",
                                          value.name = "correct_answer")

  analysis.prop2 <- analyze.simstudy.prop2(test.statistics.prop2 =
                                             test.statistics.prop2,
                                           alternative = "less",
                                           conf.level = 0.95,
                                           the.quantiles =
                                             c(0.025, 0.1, 0.9, 0.975))


  # Check each of the elements in the list
  expect_named(analysis.prop2, c("estimate.summary", "stat.summary",
                                 "p.value.summary",
                                 "df.summary", "ci.limit.summary"))

  # Check the structure of each sub-element
  expect_s3_class(analysis.prop2$estimate.summary, "data.frame")
  expect_named(analysis.prop2$estimate.summary, c("q.2.5", "q.10",
                                                  "q.90", "q.97.5",
                                                  "mean", "st.error"))

  expect_s3_class(analysis.prop2$stat.summary, "data.frame")
  expect_named(analysis.prop2$stat.summary, c("q.2.5", "q.10",
                                              "q.90", "q.97.5",
                                              "mean", "st.error"))

  expect_s3_class(analysis.prop2$p.value.summary, "data.frame")
  expect_named(analysis.prop2$p.value.summary, c("reject.proportion",
                                                 "non.reject.proportion"))
  expect_true(all(analysis.prop2$p.value.summary$reject.proportion +
                    analysis.prop2$p.value.summary$non.reject.proportion == 1))

  expect_s3_class(analysis.prop2$df.summary, "data.frame")
  expect_named(analysis.prop2$df.summary, c("q.2.5", "q.10", "q.90", "q.97.5",
                                            "mean", "st.error"))

  expect_s3_class(analysis.prop2$ci.limit.summary, "data.frame")
  expect_named(analysis.prop2$ci.limit.summary, c("q.2.5", "q.10", "q.90",
                                                  "q.97.5", "mean", "st.error"))


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

# Testing X^2 test of the goodness of fit - generating data function


describe("sim.chisq.gf returns expected results", {

  simdat.chisq.gf <- sim.chisq.gf(n = 100, values = LETTERS[1:4],
                                  prob = c(0.4, 0.3, 0.2, 0.1),
                                  num.experiments = 200,
                                  experiment.name = "experiment_id",
                                  value.name = "classification",
                                  seed = 31)

  # Check that the returned object is a data.frame
  expect_s3_class(simdat.chisq.gf, "data.frame")

  # Check the column names of the data frame
  expect_named(simdat.chisq.gf, c("experiment_id", "classification"))

  # Check the number of rows in the data frame,  100 observations x 200 experiments
  expect_equal(nrow(simdat.chisq.gf), 20000)

  # Check the range of experiment_id
  expect_equal(range(simdat.chisq.gf$experiment_id), c(1, 200))

  # Check the unique classifications
  expect_setequal(unique(simdat.chisq.gf$classification), LETTERS[1:4])

  # Check the proportions of each classification
  proportions <- table(simdat.chisq.gf$classification) / nrow(simdat.chisq.gf)
  expect_true(all(abs(proportions - c(A = 0.4, B = 0.3, C = 0.2, D = 0.1)) < 0.05))

})


# Testing X^2 test of the goodness of fit - statistical testing

describe("Testing sim.chisq.test.gf function", {

  simdat.chisq.gf <- sim.chisq.gf(n = 100, values = LETTERS[1:4],
                                  prob = c(0.4, 0.3, 0.2, 0.1),
                                  num.experiments = 200,
                                  experiment.name = "experiment_id",
                                  value.name = "classification",
                                  seed = 31)

  test.statistics.chisq.test.gf <- sim.chisq.test.gf(simdat.chisq.gf = simdat.chisq.gf,
                                                     hypothesized.probs =
                                                       c(0.25, 0.3, 0.15, 0.3),
                                                     correct = F,
                                                     experiment.name = "experiment_id",
                                                     value.name = "classification")

  # Check that the returned object is a data.frame
  expect_s3_class(test.statistics.chisq.test.gf, "data.frame")

  expected_cols <- c("experiment_id", "statistic", "df", "p.value")
  expect_named(test.statistics.chisq.test.gf, expected_cols)

  # Check the range of experiment_id
  expect_equal(range(test.statistics.chisq.test.gf$experiment_id), c(1, 200))

  expect_equal(unique(test.statistics.chisq.test.gf$df), 3)

  # Validate that p-values are between 0 and 1
  expect_true(all(test.statistics.chisq.test.gf$p.value >= 0 & test.statistics.chisq.test.gf$p.value <= 1))

  # Validate that the X-squared values are non-negative
  expect_true(all(test.statistics.chisq.test.gf$`X-squared` >= 0))


})

# Testing X^2 test of the goodness of fit - Analyzing simulation data


describe("Testing analyze.simstudy.chisq.test.gf function", {

  simdat.chisq.gf <- sim.chisq.gf(n = 100, values = LETTERS[1:4],
                                  prob = c(0.4, 0.3, 0.2, 0.1),
                                  num.experiments = 200,
                                  experiment.name = "experiment_id",
                                  value.name = "classification",
                                  seed = 31)

  test.statistics.chisq.test.gf <- sim.chisq.test.gf(simdat.chisq.gf = simdat.chisq.gf,
                                                     hypothesized.probs =
                                                       c(0.25, 0.3, 0.15, 0.3),
                                                     correct = F,
                                                     experiment.name = "experiment_id",
                                                     value.name = "classification")

  analysis.chisq.gf <- analyze.simstudy.chisq.test.gf(test.statistics.chisq.test.gf =
                                                        test.statistics.chisq.test.gf,
                                                      conf.level = 0.95,
                                                      the.quantiles = c(0.25, 0.75))

  # Check the structure of analysis.chisq.gf
  expect_named(analysis.chisq.gf, c("stat.summary", "p.value.summary"))

  # Validate the stat.summary dataframe
  expect_s3_class(analysis.chisq.gf$stat.summary, "data.frame")
  expect_named(analysis.chisq.gf$stat.summary,
               c("q.25", "q.75", "mean", "st.error"))

  # Check values within expected range
  expect_true(all(analysis.chisq.gf$stat.summary$mean >= 0))
  expect_true(all(analysis.chisq.gf$stat.summary$st.error >= 0))

  # Validate the p.value.summary dataframe
  expect_s3_class(analysis.chisq.gf$p.value.summary, "data.frame")
  expect_named(analysis.chisq.gf$p.value.summary, c("reject.proportion",
                                                    "non.reject.proportion"))

  # Validate proportions are between 0 and 1
  expect_equal(sum(analysis.chisq.gf$p.value.summary$reject.proportion,
                   analysis.chisq.gf$p.value.summary$non.reject.proportion), 1)

  # Validate reject proportions
  expect_true(all(analysis.chisq.gf$p.value.summary$reject.proportion >= 0 &
                    analysis.chisq.gf$p.value.summary$reject.proportion <= 1))


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


# Testing X^2 test of Independence - generating data function

describe("sim.chisq.ind returns expected results", {

  n <- c(50, 75, 100)
  values <- LETTERS[1:4]
  group.names <- sprintf("group_%d", 1:3)
  probs <- matrix(data = c(0.25, 0.25, 0.25, 0.25, 0.4, 0.3, 0.2, 0.1, 0.2, 0.4, 0.2,
                           0.2), nrow = length(n), byrow = T)

  simdat.chisq.ind <- sim.chisq.ind(n = c(50, 75, 100),
                                    values = LETTERS[1:4],
                                    probs = probs,
                                    num.experiments = 500,
                                    experiment.name = "exp_id",
                                    group.name = "treatment_group",
                                    group.values = sprintf("group_%d", 1:3),
                                    value.name = "category",
                                    seed = 31)

  # Check the structure of simdat.chisq.ind
  expect_s3_class(simdat.chisq.ind, "data.frame")
  expect_named(simdat.chisq.ind, c("exp_id", "treatment_group", "category"))

  # Check that simdat.chisq.ind contains the correct number of rows
  expect_equal(nrow(simdat.chisq.ind), sum(n) * 500)

  # Validate that the exp_id, treatment_group, and category are correctly populated
  expect_true(all(simdat.chisq.ind$exp_id >= 1 & simdat.chisq.ind$exp_id <= 500))
  expect_setequal(unique(simdat.chisq.ind$treatment_group), group.names)
  expect_setequal(unique(simdat.chisq.ind$category), values)


})

# Testing X^2 test of Independence - statistical testing

describe("Testing sim.chisq.test.ind function", {

  n <- c(50, 75, 100)
  values <- LETTERS[1:4]
  group.names <- sprintf("group_%d", 1:3)
  probs <- matrix(data =
                    c(0.25, 0.25, 0.25, 0.25, 0.4, 0.3, 0.2, 0.1, 0.2, 0.4, 0.2, 0.2),
                  nrow = length(n), byrow = T)

  simdat.chisq.ind <- sim.chisq.ind(n = c(50, 75, 100),
                                    values = LETTERS[1:4],
                                    probs = probs,
                                    num.experiments = 500,
                                    experiment.name = "exp_id",
                                    group.name = "treatment_group",
                                    group.values = sprintf("group_%d", 1:3),
                                    value.name = "category",
                                    seed = 31)

  test.statistics.chisq.test.ind <- suppressWarnings({sim.chisq.test.ind(simdat.chisq.ind = simdat.chisq.ind,

                                                       correct = T,
                                                       experiment.name = "exp_id",
                                                       group.name = "treatment_group",
                                                       value.name = "category")})

  # Check the structure of test.statistics.chisq.test.ind
  expect_s3_class(test.statistics.chisq.test.ind, "data.frame")
  expect_named(test.statistics.chisq.test.ind, c("exp_id", "statistic", "df", "p.value"))

  # Check that test.statistics.chisq.test.ind contains the correct number of rows
  expect_equal(nrow(test.statistics.chisq.test.ind), 500)

  # Validate that the exp_id is correctly populated
  expect_true(all(test.statistics.chisq.test.ind$exp_id >= 1 & test.statistics.chisq.test.ind$exp_id <= 500))

  # Check that df is always 6
  expect_equal(unique(test.statistics.chisq.test.ind$df), 6)

  # Validate p.value is between 0 and 1
  expect_true(all(test.statistics.chisq.test.ind$p.value >= 0 & test.statistics.chisq.test.ind$p.value <= 1))


})

# Testing X^2 test of Independence - Analyzing simulation data

describe("Testing analyze.simstudy.chisq.test.ind function", {

  n <- c(50, 75, 100)
  values <- LETTERS[1:4]
  group.names <- sprintf("group_%d", 1:3)
  probs <- matrix(data = c(0.25, 0.25, 0.25, 0.25, 0.4, 0.3, 0.2, 0.1, 0.2, 0.4, 0.2,
                           0.2), nrow = length(n), byrow = T)

  simdat.chisq.ind <- sim.chisq.ind(n = c(50, 75, 100),
                                    values = LETTERS[1:4],
                                    probs = probs,
                                    num.experiments = 500,
                                    experiment.name = "exp_id",
                                    group.name = "treatment_group",
                                    group.values = sprintf("group_%d", 1:3),
                                    value.name = "category",
                                    seed = 31)

  test.statistics.chisq.test.ind <- suppressWarnings({sim.chisq.test.ind(simdat.chisq.ind = simdat.chisq.ind,
                                                       correct = T,
                                                       experiment.name = "exp_id",
                                                       group.name = "treatment_group",
                                                       value.name = "category")})
  analysis.chisq.ind <- analyze.simstudy.chisq.test.ind(test.statistics.chisq.test.ind = test.statistics.chisq.test.ind,
                                                        conf.level = 0.95,
                                                        the.quantiles = c(0.025, 0.975))

  # Check the structure of analysis.chisq.ind
  expect_named(analysis.chisq.ind, c("stat.summary", "p.value.summary"))

  # Check the structure of analysis.chisq.ind$stat.summary
  expect_s3_class(analysis.chisq.ind$stat.summary, "data.frame")
  expect_named(analysis.chisq.ind$stat.summary, c("q.2.5", "q.97.5", "mean", "st.error"))

  # Check the values are numerical and within expected ranges
  expect_true(all(is.numeric(analysis.chisq.ind$stat.summary$q.2.5)))
  expect_true(all(is.numeric(analysis.chisq.ind$stat.summary$q.97.5)))
  expect_true(all(is.numeric(analysis.chisq.ind$stat.summary$mean)))
  expect_true(all(is.numeric(analysis.chisq.ind$stat.summary$st.error)))

  # Check the structure of analysis.chisq.ind$p.value.summary
  expect_s3_class(analysis.chisq.ind$p.value.summary, "data.frame")
  expect_named(analysis.chisq.ind$p.value.summary, c("reject.proportion", "non.reject.proportion"))

  # Validate that reject and non.reject proportions sum to 1
  expect_equal(with(analysis.chisq.ind$p.value.summary, reject.proportion + non.reject.proportion), 1)

  # Validate that reject and non.reject proportions are between 0 and 1
  expect_true(all(analysis.chisq.ind$p.value.summary$reject.proportion >= 0 & analysis.chisq.ind$p.value.summary$reject.proportion <= 1))
  expect_true(all(analysis.chisq.ind$p.value.summary$non.reject.proportion >= 0 & analysis.chisq.ind$p.value.summary$non.reject.proportion <= 1))



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
# Multivariate studies - generating Multivariate data function


describe("Testing simulation.steps function", {

  simdat.multivariate <- simulation.steps(the.steps = the.steps, n = 50,
                                          num.experiments = 200,
                                          experiment.name = "sim", seed = 41)

  # Check the structure of simdat.multivariate
  expect_s3_class(simdat.multivariate, "data.frame")
  expect_named(simdat.multivariate,
               c("sim", "Age", "Female", "Health.Percentile", "Exercise.Sessions",
                 "Diet", "Healthy.Lifestyle", "Weight"))

  # Check Female column is binary
  expect_true(all(simdat.multivariate$Female %in% c(TRUE, FALSE)))

  # Check Health Percentile range
  expect_true(all(simdat.multivariate$Health.Percentile >= 0 & simdat.multivariate$Health.Percentile <= 100))

  # Check Exercise.Sessions is positive (Poisson distributed)
  expect_true(all(simdat.multivariate$Exercise.Sessions >= 0))

  # Check Diet values
  expect_true(all(simdat.multivariate$Diet %in% c('Light', 'Moderate', 'Heavy')))

  # Check Healthy.Lifestyle is binary
  expect_true(all(simdat.multivariate$Healthy.Lifestyle %in% c(TRUE, FALSE)))

  # Check the range of Weight values
  expect_true(all(simdat.multivariate$Weight >= 100 & simdat.multivariate$Weight <= 250))

})

# Multivariate studies - linear regression modelling

describe("Testing sim.statistics.lm function", {

  simdat.multivariate <- simulation.steps(the.steps = the.steps, n = 50,
                                          num.experiments = 200,
                                          experiment.name = "sim", seed = 41)

  stats.lm <- sim.statistics.lm(simdat = simdat.multivariate, the.formula = Weight ~
                                  Age + Female + Health.Percentile + Exercise.Sessions + Healthy.Lifestyle, grouping.variables = "sim")

  # Check the structure of the output
  expect_named(stats.lm, c("the.coefs", "summary.stats"))

  # Check the structure of the.coefs
  expect_s3_class(stats.lm$the.coefs, "data.frame")
  expect_named(stats.lm$the.coefs,
               c("sim", "Coefficient", "Estimate", "Std. Error", "t value", "Pr(>|t|)"))

  # Validate that there are 200 unique simulations (experiments) in the.coefs
  expect_equal(length(unique(stats.lm$the.coefs$sim)), 200)

  # Check range of Age coefficients
  expect_true(all(stats.lm$the.coefs$Estimate[stats.lm$the.coefs$Coefficient == "Age"] > -5 &
                    stats.lm$the.coefs$Estimate[stats.lm$the.coefs$Coefficient == "Age"] < 5))

  # Check structure of summary.stats
  expect_s3_class(stats.lm$summary.stats, "data.frame")
  expect_named(stats.lm$summary.stats,
               c("sim", "sigma", "df", "rse", "r.squared", "adj.r.squared",
                 "fstatistic", "f.numdf", "f.dendf", "f.pvalue"))

  # Validate that there are 200 unique simulations (experiments) in summary.stats
  expect_equal(length(unique(stats.lm$summary.stats$sim)), 200)

})

# Multivariate studies - linear regression analyzing simulation data

describe("Testing analyze.simstudy.lm function", {

  simdat.multivariate <- simulation.steps(the.steps = the.steps,
                                          n = 50, num.experiments = 200,
                                          experiment.name = "sim",
                                          seed = 41)


  stats.lm <- sim.statistics.lm(simdat = simdat.multivariate, the.formula = Weight ~
                                  Age + Female + Health.Percentile + Exercise.Sessions + Healthy.Lifestyle, grouping.variables = "sim")


  analysis.lm <- analyze.simstudy.lm(the.coefs = stats.lm$the.coefs,
                                     summary.stats = stats.lm$summary.stats,
                                     conf.level = 0.95, the.quantiles = c(0.25, 0.75),
                                     coef.name = "Coefficient",
                                     estimate.name = "Estimate",
                                     lm.p.name = "Pr(>|t|)",
                                     f.p.name = "f.pvalue")

  # Check the structure of the output
  expect_named(analysis.lm,
               c("lm.estimate.summary", "lm.p.summary", "lm.stats.summary",
                 "fstatistic.p.summary"))

  # Check the structure of lm.p.summary
  expect_s3_class(analysis.lm$lm.p.summary, "data.frame")
  expect_named(analysis.lm$lm.p.summary, c("Coefficient", "reject.proportion",
                                           "non.reject.proportion"))

  # Validate proportions in lm.p.summary
  expect_true(all(analysis.lm$lm.p.summary$reject.proportion >= 0 &
                    analysis.lm$lm.p.summary$reject.proportion <= 1))
  expect_true(all(analysis.lm$lm.p.summary$non.reject.proportion >= 0 &
                    analysis.lm$lm.p.summary$non.reject.proportion <= 1))

  # Check structure of lm.stats.summary
  expect_s3_class(analysis.lm$lm.stats.summary, "data.frame")
  expect_named(analysis.lm$lm.stats.summary, c("stat", "q.25", "q.75",
                                               "mean", "st.error"))


})

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

# Multivariate studies - logistic regression modelling


describe("Testing sim.statistics.logisticfunction", {
  simdat.multivariate <- simulation.steps(the.steps = the.steps,
                                          n = 50, num.experiments = 200,
                                          experiment.name = "sim", seed = 41)
  stats.logistic <- sim.statistics.logistic(simdat = simdat.multivariate,
                                            the.formula = Healthy.Lifestyle ~ Age +
                                              Female + Health.Percentile +
                                              Exercise.Sessions,
                                            grouping.variables = "sim")

  # Check the structure of the output
  expect_named(stats.logistic, c("the.coefs", "summary.stats"))

  # Check the structure of the.coefs
  expect_s3_class(stats.logistic$the.coefs, "data.table")
  expect_named(stats.logistic$the.coefs, c("sim", "Coefficient", "Estimate", "Std. Error", "z value", "Pr(>|z|)"))
  # As there are 5 predictors including the intercept
  expect_equal(nrow(stats.logistic$the.coefs), 200 * 5)

  # Check the structure of summary.stats
  expect_s3_class(stats.logistic$summary.stats, "data.table")
  expect_named(stats.logistic$summary.stats,
               c("sim", "deviance", "aic", "df.residual",
                 "null.deviance", "df.null", "iter", "dispersion"))
  # As there are 200 simulations
  expect_equal(nrow(stats.logistic$summary.stats), 200)

  # Validate some expected relationships in the data
  # Check if dispersion is always 1
  expect_true(all(stats.logistic$summary.stats$dispersion == 1))

  # The residual degrees of freedom should always be 45
  expect_true(all(stats.logistic$summary.stats$df.residual == 45))

  # The null degrees of freedom should always be 49
  expect_true(all(stats.logistic$summary.stats$df.null == 49))

  # The deviance should always be less than or equal to the null.deviance
  expect_true(all(stats.logistic$summary.stats$deviance <= stats.logistic$summary.stats$null.deviance))
})

# Multivariate studies - logistic regression analyzing simulation data


describe("Testing analyze.simstudy.logistic function", {

  simdat.multivariate <- simulation.steps(the.steps = the.steps,
                                          n = 50, num.experiments = 200,
                                          experiment.name = "sim", seed = 41)
  # Execute the function
  stats.logistic <- sim.statistics.logistic(simdat = simdat.multivariate,
                                            the.formula = Healthy.Lifestyle ~ Age +
                                              Female + Health.Percentile +
                                              Exercise.Sessions,
                                            grouping.variables = "sim")

  # Execute the function
  analysis.logistic <- analyze.simstudy.logistic(the.coefs = stats.logistic$the.coefs,
                                                 summary.stats =
                                                   stats.logistic$summary.stats,
                                                 conf.level = 0.95,
                                                 the.quantiles = c(0.1, 0.9))

  # Check the structure of the output
  expect_named(analysis.logistic, c("logistic.estimate.summary",
                                    "logistic.p.summary",
                                    "logistic.stats.summary"))

  # Validate logistic.estimate.summary
  expect_s3_class(analysis.logistic$logistic.estimate.summary, "data.table")
  expect_named(analysis.logistic$logistic.estimate.summary, c("Coefficient",
                                                              "q.10",
                                                              "q.90",
                                                              "mean",
                                                              "st.error"))

  # Validate logistic.p.summary
  expect_s3_class(analysis.logistic$logistic.p.summary, "data.table")
  expect_named(analysis.logistic$logistic.p.summary, c("Coefficient",
                                                       "reject.proportion",
                                                       "non.reject.proportion"))

  # Validate logistic.stats.summary
  expect_s3_class(analysis.logistic$logistic.stats.summary, "data.table")
  expect_named(analysis.logistic$logistic.stats.summary, c("stat",
                                                           "q.10",
                                                           "q.90",
                                                           "mean",
                                                           "st.error"))

  # Check specific values.
  # 5 coefficients
  expect_equal(nrow(analysis.logistic$logistic.estimate.summary), 5)
  # 5 p-values
  expect_equal(nrow(analysis.logistic$logistic.p.summary), 5)
  # 7 statistics
  expect_equal(nrow(analysis.logistic$logistic.stats.summary), 7)

  expect_true(all(analysis.logistic$logistic.p.summary$reject.proportion + analysis.logistic$logistic.p.summary$non.reject.proportion == 1))
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


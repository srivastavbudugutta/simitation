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

describe("Testing sim.t function", {

  it("produces expected output", {
    # Your provided settings
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


describe("sim.t.test function", {

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


describe("analyze.simstudy.t function", {

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












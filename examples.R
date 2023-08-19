# Set working directory to the source file's location.

rm(list = ls())
library(simitation)
# # Internal functions
# source("internal.statistics.one.lm.R")
# source("internal.quantiles.mean.sd.R")
# source("quantile.dt.R")
# source("internal.t.test.R")
# source("internal.t2.test.R")
# source("internal.prop.test.R")
# source("internal.prop2.test.R")
# source("internal.chisq.test.gf.R")
# source("internal.chisq.test.ind.R")
#
# # Exportable functions
# source("sim.norm.R")
# source("sim.t.R")
# source("sim.t2.R")
# source("sim.t.test.R")
# source("sim.t2.test.R")
# source("simstudy.prop.R")
# source("simstudy.prop2.R")
# source("simstudy.t.R")
# source("simstudy.t2.R")
# source("simstudy.lm.R")
# source("simstudy.logistic.R")
# source("analyze.simstudy.prop.R")
# source("analyze.simstudy.prop2.R")
# source("analyze.simstudy.t.R")
# source("analyze.simstudy.t2.R")
# source("analyze.simstudy.lm.R")
# source("analyze.simstudy.logistic.R")
# source("buildsim.binary.R")
# source("buildsim.lm.R")
# source("buildsim.logistic.R")
# source("buildsim.normal.R")
# source("buildsim.sample.R")
# source("buildsim.uniform.R")
# source("identify.distribution.R")
# source("sim.prop.R")
# source("sim.prop2.R")
# source("sim.prop.test.R")
# source("sim.prop2.test.R")
# source("sim.chisq.gf.R")
# source("sim.chisq.test.gf.R")
# source("analyze.simstudy.chisq.test.gf.R")
# source("simstudy.chisq.test.gf.R")
# source("sim.chisq.ind.R")
# source("sim.chisq.test.ind.R")
# source("analyze.simstudy.chisq.test.ind.R")
# source("simstudy.chisq.test.ind.R")
#
# source("simulation.steps.R")
# source("buildsim.poisson.R")
# source("sim.statistics.lm.R")
# source("sim.statistics.logistic.R")

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



## sim.t

simdat.t <- sim.t(n = n, mean = 0.3, sd = 1, num.experiments = num.experiments, variable.name = "x", seed = 2187)
print(simdat.t)


## sim.t.test

test.statistics.t <- sim.t.test(simdat.t = simdat.t, mu = 0, alternative = "less")
print(test.statistics.t)


## analyze.simstudy.t

analyze.simstudy.t(test.statistics.t = test.statistics.t, alternative = "less")

## simstudy.t

simstudy.t(n = n, mean = 0.3, sd = 1, mu = 0, num.experiments = 100, alternative = "less", seed = 89201)



## sim.t2

simdat.t2 <- sim.t2(nx = 30, ny = 30, meanx = 0, meany = 0.3, sdx = 1, sdy = 1, num.experiments = num.experiments, seed = 17, x.value = "x", y.value = "y")

print(simdat.t2)

## sim.t2.test

test.statistics.t2 <- sim.t2.test(simdat.t2 = simdat.t2, alternative = "two.sided")
print(test.statistics.t2)

## analyze.simstudy.t2

analyze.simstudy.t2(test.statistics.t2 = test.statistics.t2, alternative = "two.sided")

# simstudy.t2

simres.t2 <- simstudy.t2(nx = 30, ny = 30, meanx = 0, meany = 0.3, sdx = 1, sdy = 1, num.experiments = 2, seed = 92, alternative = "less")


## sim.p

simdat.prop <- sim.prop(n = 30, p = 0.55, num.experiments = 2, seed = 31)

# sim.prop.test
test.statistics.prop <- sim.prop.test(simdat.prop = simdat.prop, variable.name = "x", experiment.name = "experiment", alternative = "greater", p = 0.5)
print(test.statistics.prop)

# analyze.simstudy.prop
analyze.simstudy.prop(test.statistics.prop = test.statistics.prop, alternative = "greater")

# simstudy.prop
simstudy.prop(n = 30, p.actual = 0.55, p.hypothesized = 0.5, alternative = "r", num.experiments = 2, seed = 41)

## sim.p2

simdat.prop2 <- sim.prop2(nx = 30, ny = 35, px = 0.5, py = 0.55, num.experiments = num.experiments, seed = 31)

# sim.prop2.test
test.statistics.prop2 <- sim.prop2.test(simdat.prop2 = simdat.prop2, alternative = "less")
print(test.statistics.prop2)

# analyze.simstudy.prop2
analyze.simstudy.prop2(test.statistics.prop2 = test.statistics.prop2, alternative = "less")

## simstudy.prop2
simstudy.prop2(nx = 30, ny = 35, px = 0.5, py = 0.55, num.experiments = 2, alternative = "less")


## simdat.chisq.gf

simdat.chisq.gf <- sim.chisq.gf(n = 30, values = LETTERS[1:4], prob = c(0.4, 0.3, 0.2, 0.1), num.experiments = 20, seed = 31)

## sim.chisq.test.gf
test.statistics.chisq.test.gf <- sim.chisq.test.gf(simdat.chisq.gf = simdat.chisq.gf, hypothesized.probs = c(0.25, 0.3, 0.1, 0.35))

## analyze.simstudy.chisq.test.gf.R
analyze.simstudy.chisq.test.gf(test.statistics.chisq.test.gf = test.statistics.chisq.test.gf)

## simstudy.chisq.test.gf
simstudy.chisq.test.gf(n = 100, values = LETTERS[1:4], actual.probs = c(0.3, 0.3, 0.2, 0.2), hypothesized.probs = rep.int(x = 0.25, times = 4), num.experiments = 40)

## sim.chisq.ind

n <- c(50, 75, 100)
values <- LETTERS[1:4]
group.names <- sprintf("group_%d", 1:3)
probs <- matrix(data = c(0.25, 0.25, 0.25, 0.25, 0.4, 0.3, 0.2, 0.1, 0.2, 0.4, 0.2, 0.2), nrow = length(n), byrow = T)

simdat.chisq.ind <- sim.chisq.ind(n = n, values = values, probs = probs, num.experiments = 2, seed = 4000)

## sim.chisq.test.ind
test.statistics.chisq.test.ind <- sim.chisq.test.ind(simdat.chisq.ind = simdat.chisq.ind, variable.name = "variable", value.name = "value", experiment.name = "experiment")

## analyze.simstudy.chisq.test.ind
analyze.simstudy.chisq.test.ind(test.statistics.chisq.test.ind = test.statistics.chisq.test.ind)

simstudy.chisq.test.ind(n = n, probs = probs, values = values, num.experiments = 100, seed = 11)


## Create steps of simulation.
step.age <- "Age ~ N(45, 10)"
step.female <- "Female ~ binary(0.53)"
step.health.percentile <- "Health.Percentile ~ U(0,100)"
step.exercise.sessions <- "Exercise.Sessions ~ poisson(2)"

step.diet <- "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))"

step.healthy.lifestyle <- "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"
step.weight <- "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))"

the.steps <- c(step.age, step.female, step.health.percentile, step.exercise.sessions, step.diet, step.healthy.lifestyle, step.weight)


# Build simulated data from specification of the.steps.
simdat.lm <- simulation.steps(the.steps = the.steps, n = 100, num.experiments = num.experiments, seed = 17)

# Build linear regression statistics from simdat and a specification of the lm's formula.

statistics.lm <- sim.statistics.lm(simdat = simdat.lm, the.formula = Weight ~ Age + Female + Health.Percentile + Exercise.Sessions + Healthy.Lifestyle, grouping.variables = "experiment")

analyze.simstudy.lm(the.coefs = statistics.lm$the.coefs, summary.stats = statistics.lm$summary.stats)

simstudy.lm(the.steps = the.steps, n = 100, num.experiments = num.experiments, the.formula = Weight ~ Age + Female + Health.Percentile + Exercise.Sessions + Healthy.Lifestyle, seed = 92)






# Simulate data for a logistic regression based on symbolic steps:

simdat.logistic <- simulation.steps(the.steps = the.steps[1:(length(the.steps)-1)], n = 100, num.experiments = num.experiments, seed = 87)

# Build logistic regression statistics from simdat and a specification of the logistic regression's formula.

statistics.logistic <- sim.statistics.logistic(simdat = simdat.logistic, the.formula = Healthy.Lifestyle ~ Age + Female + Health.Percentile + Exercise.Sessions, grouping.variables = "experiment")

analyze.simstudy.logistic(the.coefs = statistics.logistic$the.coefs, summary.stats = statistics.logistic$summary.stats)

simstudy.logistic(the.steps = the.steps[1:(length(the.steps)-1)], n = 100, num.experiments = num.experiments, the.formula = Healthy.Lifestyle ~ Age + Female + Health.Percentile + Exercise.Sessions, seed = 222)


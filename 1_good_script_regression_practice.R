library(tidyverse)
library(haven)
library(apaTables)
my.data <- read_sav("Lecture 7 regression_example_data.sav")
apa.cor.table(my.data)
# USE CORRELATIONS TO PICK OUT SINGLE BEST PREDICTOR - IN THIS CASE, IT'S GMA!



# 1 - ANSWER EACH OF THE 3 QUESTIONS IN THE SLIDES USING A SINGLE REGRESSION BY EXAMINING SR2

con.results <- lm(jobperf ~ gma + con, data = my.data) # This gives you the b-weights (estimate)
summary(con.results)
apa.reg.table(con.results, filename = "con.results_RegressionTable.doc") # Fit = R-squared

ac.results <- lm(jobperf ~ gma + ac, data = my.data) # This gives you the b-weights (estimate)
summary(ac.results)
apa.reg.table(ac.results, filename = "ac.results_RegressionTable.doc") # Fit = R-squared

graph.results <- lm(jobperf ~ gma + graph, data = my.data) # This gives you the b-weights (estimate)
summary(graph.results)
apa.reg.table(graph.results, filename = "graph.results_RegressionTable.doc") # Fit = R-squared



# 2 - ANSWER EACH OF 3 QUESTIONS IN THE SLIDES USING TWO BLOCK REGRESSIONS:

block1 <- lm(jobperf ~ gma, data = my.data)
block2 <- lm(jobperf ~ gma + con, data = my.data)
apa.reg.table(block1, block2)
# Delta R2 tells you the semi-partial correlation of conscientiousness (.10**)

block3 <- lm(jobperf ~ gma, data = my.data)
block4 <- lm(jobperf ~ gma + ac, data = my.data)
apa.reg.table(block3, block4)
# Delta R2 tells you the semi-partial correlation of AC (.02**)

block5 <- lm(jobperf ~ gma, data = my.data)
block6 <- lm(jobperf ~ gma + graph, data = my.data)
apa.reg.table(block5, block6)
# Delta R2 tells you the semi-partial correlation of graphology (.00)



# 3 & 4 - IN THE END YOU WILL DECIDE TO USE GMA AND CON -> What is the CI & PI for predicted jobperf at the mean GMA and mean Con?

library(psych)
describe(my.data)
# GMA mean = 100
# CON mean = 120

x_axis_range <- data.frame(gma = c(100), con = c(120))
CI_data <- predict(con.results, newdata = x_axis_range, interval = "confidence", level = 0.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
print(CI_data)
# The predicted score for an individual with a GMA score of 100 and con score of 120 is 101, 95% CI [100.28, 101.72].

x_axis_range <- data.frame(gma = c(100), con = c(120))
PI_data <- predict(con.results, newdata = x_axis_range, interval = "prediction", level = 0.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
print(PI_data)
# The predicted score for an individual with a GMA score of 100 and con score of 120 is 101, 95% PI [84.87, 117.13].
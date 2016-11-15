library(tidyverse)
library(apaTables)
library(haven)

# 1 - LOAD THE DATA

my.data <- read_spss("Lecture 7 regression_example_data.sav")

apa.cor.table(my.data)

# make sure no curvilinear relationships...
psych::pairs.panels(as.data.frame(my.data))

con.results <- lm(jobperf ~ con, data=my.data)
summary(con.results)
apa.reg.table(con.results)

ac.results <- lm(jobperf ~ ac, data=my.data)
summary(ac.results)
apa.reg.table(ac.results)

graph.results <- lm(jobperf ~ graph, data = my.data)
summary(graph.results)
apa.reg.table(graph.results)

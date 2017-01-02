set.seed(614)
library(Matching)
library(ggplot2)

ate <- 2

control <- rnorm(1000, mean = 0, sd = .5)
treatment <- rnorm(1000, mean = 0 + ate, sd = .5)

control_df <- data.frame(group = "control", value = control)
treatment_df <- data.frame(group = "treatment", value = treatment)
observed <- rbind(control_df, treatment_df)

ggplot(data = observed, aes(x = value)) + geom_density(aes(fill = group, color = group), alpha = .5) + theme_minimal() + ggtitle("Visualizing Average Treatment effect", subtitle = "ATE = 2")

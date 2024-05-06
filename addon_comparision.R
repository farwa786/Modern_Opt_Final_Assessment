# Variability Analysis using Boxplots
boxplot(parsed_data1$best_mean, parsed_data2$best_mean, parsed_data3$best_mean,
        names = c("Pop Size 15", "Pop Size 20", "Pop Size 30"),
        main = "Variability of Fitness Scores",
        ylab = "Fitness Scores")


# Hypothesis Testing using ANOVA and Post-hoc Tests
fitness_combined <- c(parsed_data1$best_mean, parsed_data2$best_mean, parsed_data3$best_mean)
group_labels <- factor(rep(c("Pop Size 15", "Pop Size 20", "Pop Size 30"), each = nrow(parsed_data1)))

anova_result <- aov(fitness_combined ~ group_labels)
summary(anova_result)


if (summary(anova_result)[[1]]['Pr(>F)'][1] < 0.05) {
  post_hoc <- TukeyHSD(anova_result)
  print(post_hoc)
}

# Correlation Analysis (Example assuming mutation rates data is available)
# Assuming 'mutation_rates' is a vector of mutation rates corresponding to fitness values
mutation_rates <- c(runif(nrow(parsed_data1)), runif(nrow(parsed_data2)), runif(nrow(parsed_data3)))
cor.test(fitness_combined, mutation_rates, method = "pearson")

# Output the results
cat("ANOVA results:\n")
print(summary(anova_result))
cat("Post-hoc analysis if significant differences found:\n")
if (exists("post_hoc")) {
  print(post_hoc)
}


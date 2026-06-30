# ── 4.4 Pearson Correlation: Cluster Membership vs Average Points ─────────────

# Step 1: Encode cluster membership numerically (Elite=1, Mid-Table=2, Struggling=3)
team_data <- team_data %>%
  mutate(Cluster_Numeric = case_when(
    Cluster_Name == "Elite"      ~ 1,
    Cluster_Name == "Mid-Table"  ~ 2,
    Cluster_Name == "Struggling" ~ 3
  ))

# Step 2: Check normality of both variables (assumption of Pearson correlation)
shapiro.test(team_data$Cluster_Numeric)
shapiro.test(team_data$Avg_Pts)

# Step 3: Run Pearson Correlation
cor_result <- cor.test(team_data$Cluster_Numeric,
                       team_data$Avg_Pts,
                       method = "pearson")
print(cor_result)

# Step 4: Extract key values
cat("Pearson r:  ", round(cor_result$estimate, 4), "\n")
cat("p-value:    ", round(cor_result$p.value, 6), "\n")
cat("95% CI:     ", round(cor_result$conf.int[1], 4),
    "to",
    round(cor_result$conf.int[2], 4), "\n")
cat("R-squared:  ", round(cor_result$estimate^2, 4), "\n")

# Step 5: Scatterplot with correlation line
ggplot(team_data, aes(x = Cluster_Numeric, y = Avg_Pts)) +
  geom_jitter(width = 0.05, size = 2.5,
              aes(color = Cluster_Name), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE,
              color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Elite"      = "steelblue",
                                "Mid-Table"  = "darkorange",
                                "Struggling" = "red3")) +
  scale_x_continuous(breaks = c(1, 2, 3),
                     labels = c("Elite", "Mid-Table", "Struggling")) +
  labs(title    = "Pearson Correlation: Performance Group vs Average Points",
       subtitle = paste("r =", round(cor_result$estimate, 4),
                        "| p < 0.001"),
       x        = "Performance Group",
       y        = "Average Points Per Season",
       color    = "Performance Group") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "none")
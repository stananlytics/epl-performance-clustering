library(readxl)
library(tidyverse)
install.packages("corrplot")
library(corrplot)
install.packages("GGally")
library(GGally)
install.packages("ggthemes")
library(ggthemes)




epl <- epl_updated


epl <- epl %>%
  rename(Draws = D, Losses = L) %>%
  mutate(Wins = Pld - Draws - Losses)

# Aggregate by Team (across all seasons)
team_data <- epl %>%
  group_by(Team) %>%
  summarise(
    Seasons_Played = n(),
    Avg_Wins       = mean(Wins),
    Avg_Draws      = mean(Draws),
    Avg_Losses     = mean(Losses),
    Avg_GF         = mean(GF),
    Avg_GA         = mean(GA),
    Avg_GD         = mean(GD),
    Avg_Pts        = mean(Pts),
    Total_Wins     = sum(Wins),
    Total_Pts      = sum(Pts)
  ) %>%
  ungroup()

# Cluster Variables (team-level averages) 
# ── Cluster Variables (cleaned — no redundant variables) ────────────────────
cluster_vars <- team_data %>%
  select(Avg_Wins, Avg_Draws, Avg_Losses, Avg_GF, Avg_GA)

#each row rep one team
# Check number of rows = number of unique teams
nrow(team_data)

# View the actual team-level data
View(team_data)

# Or print it
print(team_data, n = Inf)


# Histograms for all clustering variables
# Compute per-variable stats first
var_stats <- cluster_vars %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(mean_val = mean(Value), sd_val = sd(Value), .groups = "drop")

# Plot
cluster_vars %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  left_join(var_stats, by = "Variable") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(aes(y = after_stat(density)),
                 fill = "steelblue", color = "white", bins = 20, alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_line(stat = "function",
            fun = function(x) dnorm(x, mean = unique(var_stats$mean_val), sd = unique(var_stats$sd_val)),
            color = "black", linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Distribution of Clustering Variables with Density Curves",
       subtitle = "Red = Actual density | Black dashed = Normal reference curve",
       x = "Value", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))



cor_matrix <- cor(cluster_vars)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black",
         title = "Correlation Matrix of Clustering Variables",
         mar = c(0,0,2,0))

# Scatterplots: Avg_Pts vs key predictors (team-level)
p1 <- ggplot(team_data, aes(x = Avg_Wins, y = Avg_Pts)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_text(aes(label = Team), size = 2.5, vjust = -0.8, check_overlap = TRUE) +
  labs(title = "Avg Points vs Avg Wins", x = "Avg Wins", y = "Avg Points") +
  theme_minimal()

p2 <- ggplot(team_data, aes(x = Avg_GD, y = Avg_Pts)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_text(aes(label = Team), size = 2.5, vjust = -0.8, check_overlap = TRUE) +
  labs(title = "Avg Points vs Avg Goal Difference", x = "Avg GD", y = "Avg Points") +
  theme_minimal()

p3 <- ggplot(team_data, aes(x = Avg_GF, y = Avg_GA)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_text(aes(label = Team), size = 2.5, vjust = -0.8, check_overlap = TRUE) +
  labs(title = "Avg Goals Scored vs Avg Goals Conceded", x = "Avg GF", y = "Avg GA") +
  theme_minimal()

gridExtra::grid.arrange(p1, p2, p3, ncol = 2)

# ── 3. MULTIVARIATE: Pairs Plot
ggpairs(cluster_vars,
        lower = list(continuous = wrap("points", alpha = 0.4, size = 0.8,
                                       color = "steelblue")),
        upper = list(continuous = wrap("cor", size = 3.5)),
        diag  = list(continuous = wrap("densityDiag", fill = "steelblue",
                                       alpha = 0.5)),
        title = "Pairwise Relationships Among Clustering Variables") +
  theme_minimal()

#  4. NORMALITY CHECK 
# Shapiro-Wilk on team-level Avg_Pts (n = 49, suitable for Shapiro-Wilk)
shapiro.test(team_data$Avg_Pts)

# Q-Q Plot
ggplot(team_data, aes(sample = Avg_Pts)) +
  stat_qq(color = "steelblue", size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "Q-Q Plot: Average League Points (Team Level)",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# ── 5. TEMPORAL TREND ───────────────────────────────────────────────────────
data %>%
  group_by(Season) %>%
  summarise(Avg_Pts = mean(Pts), SD_Pts = sd(Pts), .groups = "drop") %>%
  ggplot(aes(x = Season, y = Avg_Pts, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_ribbon(aes(ymin = Avg_Pts - SD_Pts, ymax = Avg_Pts + SD_Pts),
              alpha = 0.15, fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        plot.title    = element_text(face = "bold")) +
  labs(title    = "Average Points Per Season (2000–2022)",
       subtitle = "Shaded band = ±1 SD",
       y = "Avg Points", x = "Season")

# ── 6. SCALE CHECK & STANDARDIZATION ────────────────────────────────────────
# Check means and SDs — confirms standardization is needed before K-Means
sapply(cluster_vars, function(x) c(mean = mean(x), sd = sd(x)))

# Standardize (exclude Avg_Pts — it's the outcome, not a clustering input)
cluster_scaled <- scale(cluster_vars %>%
                          select(Avg_Wins, Avg_Draws, Avg_Losses,
                                 Avg_GF, Avg_GA))

# Attach team names as row names for reference during clustering
rownames(cluster_scaled) <- team_data$Team

# Preview
head(cluster_scaled)





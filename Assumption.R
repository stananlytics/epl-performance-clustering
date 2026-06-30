#K clustering asssumption
cov_mat  <- cov(cluster_vars)
cov_reg  <- cov_mat + diag(1e-6, ncol(cov_mat))   # nudge diagonal slightly

mahal <- mahalanobis(cluster_vars,
                     colMeans(cluster_vars),
                     cov_reg)

cutoff <- qchisq(0.999, df = ncol(cluster_vars))

outlier_teams <- team_data$Team[mahal > cutoff]
cat("Outlier teams:", outlier_teams, "\n")


#outlier detection
install.packages("knitr")
library(knitr)

mahal_table <- data.frame(
  Team         = team_data$Team,
  Mahalanobis  = round(mahal, 4),
  Outlier      = ifelse(mahal > cutoff, "Yes", "No")
) %>%
  arrange(desc(Mahalanobis))


mahal_table %>%
  kable(col.names = c("Team", "Mahalanobis Distance", "Outlier"),
        caption    = "Mahalanobis Distance per Team (Outlier Threshold p = 0.001)",
        align      = c("l", "r", "c"))

# Confirm standardization worked — all means ~0, all SDs ~1
round(apply(cluster_scaled, 2, mean), 4)
round(apply(cluster_scaled, 2, sd), 4)

# Visualize spread after scaling
as.data.frame(cluster_scaled) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red",
             linetype = "dashed") +
  labs(title = "Standardized Variables — Scale Check",
       subtitle = "All variables should be centered around 0",
       x = "Variable", y = "Standardized Value") +
  theme_minimal()

# Correlation matrix on the FINAL 5 variables only
cor_final <- cor(cluster_vars)
round(cor_final, 3)

corrplot(cor_final, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black",
         title = "Correlation Matrix — Final Clustering Variables",
         mar = c(0,0,2,0))

# Rule of thumb: concern if |r| > 0.85 between any two variables


# To detect if there is an actual cluster in the data geenerated 

install.packages("clustertend")
library(clustertend)

#  Step 1: Define the function first 
hopkins_manual <- function(data, n) {
  d  <- ncol(data)
  nr <- nrow(data)
  
  # Sample n points from data
  sample_pts <- data[sample(1:nr, n), ]
  
  # Generate n random points within data range
  random_pts <- matrix(
    apply(data, 2, function(col) runif(n, min(col), max(col))),
    nrow = n
  )
  
  # Nearest neighbour distances
  u_dist <- apply(random_pts, 1, function(rp) {
    min(apply(data, 1, function(dp) sqrt(sum((rp - dp)^2))))
  })
  
  w_dist <- apply(sample_pts, 1, function(sp) {
    dists <- apply(data, 1, function(dp) sqrt(sum((sp - dp)^2)))
    sort(dists)[2]
  })
  
  H <- sum(u_dist) / (sum(u_dist) + sum(w_dist))
  return(H)
}

# ── Step 2: Now run it at different n values ─────────────────────────────────
set.seed(123)
H_small  <- hopkins_manual(cluster_scaled, n = 5)
H_medium <- hopkins_manual(cluster_scaled, n = 10)
H_large  <- hopkins_manual(cluster_scaled, n = 15)

cat("Hopkins (n=5): ",  round(H_small,  4), "\n")
cat("Hopkins (n=10):", round(H_medium, 4), "\n")
cat("Hopkins (n=15):", round(H_large,  4), "\n")





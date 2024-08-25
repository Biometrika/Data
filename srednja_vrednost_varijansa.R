# Srednja vrednost-varijansa veza ---

library(ggplot2)
library(patchwork)

# Normalna raspodela ---
mean_values_normal <- seq(1, 10, length.out = 100)
variance_values_normal <- rep(1, length(mean_values_normal))
normal_dt <- data.frame(Mean = mean_values_normal,
                        Variance = variance_values_normal)

f1 <- ggplot(data = normal_dt,
       aes(x = Mean, y = Variance)) +
  geom_line(color = "#EF5350", size = 1.2) +
  labs(x = "Srednja vrednost", y = "Varijansa",
       title = "Normalna raspodela") +
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Poasonova raspodela ---
mean_values_poisson <- seq(1, 10, length.out = 100)
variance_values_poisson <- mean_values_poisson
poisson_dt <- data.frame(Mean = mean_values_poisson,
                         Variance = variance_values_poisson)

f2 <- ggplot(data = poisson_dt,
       aes(x = Mean, y = Variance)) +
  geom_line(color = "#0288D1", size = 1.2) +
  labs(x = "Srednja vrednost", y = "Varijansa",
       title = "Poasonova raspodela") +
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Binomna raspodela ---
n <- 10
p_values_binomial <- seq(0, 1, length.out = 100)
mean_values_binomial <- n * p_values_binomial
variance_values_binomial <- n * p_values_binomial * (1 - p_values_binomial)
binomial_dt <- data.frame(Mean = mean_values_binomial,
                          Variance = variance_values_binomial)

f3 <- ggplot(data = binomial_dt,
       aes(x = Mean, y = Variance)) +
  geom_line(color = "#7CB342", size = 1.2) +
  labs(x = "Srednja vrednost", y = "Varijansa",
       title = "Binomna raspodela") +
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

f1 + f2 + f3
f1 + f2

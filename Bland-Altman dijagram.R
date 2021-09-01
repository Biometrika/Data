# Bland-Altman dijagram ----

library(ggplot2)

dt <- data.frame(X = rnorm(120, 50, 5),
                 Y = rnorm(120, 40, 4))
head(dt, 3)

# srednja vrednost ---
dt$mean <- rowMeans(dt)
dt$mean

# razlika ---
dt$dif <- dt$X - dt$Y
dt$dif

# sredina razlika ---
mean_dif <- mean(dt$dif)
mean_dif

# interval poverenja ---
up_ci <- mean_dif + 1.96*sd(dt$dif)
lw_ci <- mean_dif - 1.96*sd(dt$dif)

# dijagram ---
ggplot(data = dt,
       aes(x = mean,
           y = dif)) +
  geom_point(size = 2,
             color = "#4DAF4A") +
  geom_hline(yintercept = mean_dif) +
  geom_hline(yintercept = lw_ci, color = "#E41A1C", linetype = "dashed", size = 1) +
  geom_hline(yintercept = up_ci, color = "#E41A1C", linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_y_continuous(breaks = seq(from = -100, to = 100, by = 5)) +
  labs(x = "Srednja vrednost",
       y = "Razlika izmedju vrednosti",
       title = "Bland-Altman dijagram") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_text(size = 8, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 8, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
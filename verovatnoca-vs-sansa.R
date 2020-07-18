# odnos verovatnoce i sanse ---

library(tibble)
library(ggplot2)

Verovatnoca <- seq(from = 0.001, to = 0.999, by = 0.001)
Sansa <- Verovatnoca/(1 - Verovatnoca)
dt <- data.frame(Verovatnoca,Sansa)
as_tibble(dt)

formula <- "italic(Sansa == frac(p, 1-p))"
ggplot(data = dt,
       aes(x = Verovatnoca,
           Sansa)) +
  geom_point(shape = 16,
             size = 2,
             color = "tomato",
             alpha = 0.6) +
  scale_x_continuous(breaks = seq(from = 0.0, to = 1.0, by = 0.1)) +
  scale_y_continuous(breaks = seq(from = 0.0, to = 1000, by = 100)) +
  annotate("text", x = 0.14, y = 960, label = formula, parse = TRUE, size = 7) +
  theme_light() +
  theme(axis.text.x = element_text(size = 12, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 12, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold")
        )
# Anscombe (1973) kvartet podaci ---
# Izvor podataka: Anscombe F.J. (1973). Graphs in statistical analysis".
# American Statistician. 27(1): 17-1. doi:10.1080/00031305.1973.10478966.

library(dplyr)
library(tibble)
library(ggplot2)

x <- c(10,8,13,9,11,14,6,4,12,7,5,
       10,8,13,9,11,14,6,4,12,7,5,
       10,8,13,9,11,14,6,4,12,7,5,
       8,8,8,8,8,8,8,19,8,8,8)
y <- c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68,
       9.14,8.14,8.74,8.77,9.26,8.10,6.13,3.10,9.13,7.26,4.74,
       7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73,
       6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.50,5.56,7.91,6.89)
grupa <- gl(4, 11, labels = c("I", "II", "III", "IV"))
anscombe <- data.frame(x,y,grupa)
as_tibble(anscombe)

# deskripcija ---
anscombe %>%
  group_by(grupa) %>%
  summarize(mean.x = mean(x),
            sd.x = sd(x),
            mean.y = mean(y),
            sd.y = sd(y)
            )

# korelacija ---
anscombe %>%
  group_by(grupa) %>%
  summarize(kor.tab = cor(x, y, method = "pearson")
            )

# cetri slucaja ---
rez <- list()
for (grupa.loop in 1:4){
     anscombe.lm <- lm(data = anscombe[grupa == grupa.loop],
                     formula = y ~ x)
     rez[[grupa.loop]] <- summary(anscombe.lm)
}

rez
rez[[1]]
rez[[2]]
rez[[3]]
rez[[4]]

# tackasti dijagram ---
ggplot(data = anscombe,
       aes(x = x,
           y = y,
           group = grupa)) +
  geom_point(shape = 1,
             size = 4) +
  stat_smooth(method = "lm",
              fullrange = TRUE,
              se = FALSE,
              color = "black",
              size = 0.5) +
  facet_wrap( ~ grupa) +
  theme_light() +
  labs(x = "X",
       y = "Y",
       title = "Anscombe (1973) kvartet podaci") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 12, hjust = 0.5),
        axis.text.y = element_text(size = 12, hjust = 0.5, angle = 90),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        strip.text = element_text(size = rel(2.0), face = "bold", color = "black"),
        strip.background = element_rect(fill = "gold", color = "black")
        )
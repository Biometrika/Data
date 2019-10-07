
# Referenca:
# Matejka J. & Fitzmaurice G. (2017):
# Same Stats, Different Graphs:
# Generating Datasets with Varied Appearance and
# Identical Statistics through Simulated Annealing
# http://dx.doi.org/10.1145/3025453.3025912

library(dplyr)
library(ggplot2)
library(datasauRus)
library(gridExtra)
head(datasaurus_dozen, 10)
datasaurus_dozen$dataset <- as.factor(datasaurus_dozen$dataset)
levels(datasaurus_dozen$dataset)

# korelaciona matrica ----
cor.mat <- datasaurus_dozen %>% 
  group_by(dataset) %>%
  summarize(correlation = cor(x, y)) %>%
  mutate(cor = round(correlation, 3))

ggplot(cor.mat,
       aes(x = dataset,
           y = cor,
           fill = dataset)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(-0.08, 0, 0.01)) +
  geom_text(aes(label = cor),
            hjust = -0.2,
            size = 4.5,
            color = "white") +
  labs(x = "Dataset(s)",
       y = "r") +
  coord_flip() +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11, hjust = 0.5, angle = 35),
        axis.title = element_text(size = 14, face = 2)
        )


# deskriptivna statistika ----
descr.mat <- datasaurus_dozen %>% 
  group_by(dataset) %>%
  summarise(mean.x = mean(x, na.rm = TRUE),
            mean.y = mean(y, na.rm = TRUE),
            sd.x = sd(x, na.rm = TRUE),
            sd.y = sd(y, na.rm = TRUE),
            se.x = sd(x, na.rm = TRUE)/sqrt(length(x)),
            se.y = sd(y, na.rm = TRUE)/sqrt(length(x))
            )
descr.mat


# tackasti dijagram ----
ggplot(datasaurus_dozen,
       aes(x = x,
           y = y)) +
  geom_point(shape = 1,
             color = "skyblue") +
  facet_wrap(. ~ dataset, ncol = 7) +
  theme_light() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(hjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11, angle = 90, hjust = 0.5),
        axis.title = element_text(size = 14, face = 2),
        strip.text = element_text(face = "bold", size = rel(1.2)),
        strip.background = element_rect(fill = "#EC6475", colour = "black", size = 0.5)
        )

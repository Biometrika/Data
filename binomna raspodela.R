
library(dplyr)
library(magrittr)
library(ggplot2)

# B(20, 0.5) ---
df <- data.frame(x = 0:20) %>%
  mutate(Verovatnoca = dbinom(x, size = 20, prob = 0.5))
ggplot(data = df,
       aes(x = x)) +
  geom_point(aes(y = Verovatnoca),
                 size = 4,
                 color = "tomato") +
  geom_linerange(aes(ymax = Verovatnoca,
                     ymin = 0),
                 size = 1.2,
                 color = "gold") +
  scale_y_continuous(breaks = seq(from = min(df$Verovatnoca),
                                  to = max(df$Verovatnoca),
                     by = 0.1)) +
  labs(title = "Binomna raspodela (n = 20, p = 0.5)") +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 11, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5, angle = 90),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
        )


# B(20, 0.7) ---
df <- data.frame(x = 0:20) %>%
  mutate(Verovatnoca = dbinom(x, size = 20, prob = 0.7))
ggplot(data = df,
       aes(x = x)) +
  geom_point(aes(y = Verovatnoca),
                 size = 4,
                 color = "tomato") +
  geom_linerange(aes(ymax = Verovatnoca,
                     ymin = 0),
                 size = 1.2,
                 color = "gold") +
  scale_y_continuous(breaks = seq(from = min(df$Verovatnoca),
                                  to = max(df$Verovatnoca),
                     by = 0.05)) +
  labs(title = "Binomna raspodela (n = 20, p = 0.7)") +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 11, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5, angle = 90),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
        )


# B(40, 0.5) ---
df <- data.frame(x = 0:40) %>%
  mutate(Verovatnoca = dbinom(x, size = 40, prob = 0.5))
ggplot(data = df,
       aes(x = x)) +
  geom_point(aes(y = Verovatnoca),
                 size = 4,
                 color = "tomato") +
  geom_linerange(aes(ymax = Verovatnoca,
                     ymin = 0),
                 size = 1.2,
                 color = "gold") +
  scale_y_continuous(breaks = seq(from = min(df$Verovatnoca),
                                  to = max(df$Verovatnoca),
                     by = 0.05)) +
  labs(title = "Binomna raspodela (n = 40, p = 0.5)") +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 11, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5, angle = 90),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
        )


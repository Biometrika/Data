library(ggplot2)

cor <- c(0.0,0.5,0.7,0.8,0.9,0.95,0.97,0.99,0.995,0.999)
vif <- c(1.00,1.33,1.96,2.78,5.26,10.26,16.92,50.25,100.0,500.0)
df <- data.frame(cor,vif)

ggplot(data = df,
       aes(x = cor,
           y = vif)) +
  geom_point(size = 2.0) +
  geom_line(size = 0.5) +
  geom_hline(yintercept = 10,
             color = "red") +
  scale_x_continuous(breaks = seq(from = 0.0, to = 1.0, by = 0.1)) +
  labs(x = "Pearson korelacioni koeficijent (r)",
       y = "Faktor inflacije varijanse (VIF)") +
  theme(text = element_text(size = 13),
        axis.text.x = element_text(size = 12, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 12, hjust = 0.5, vjust = 0.5, angle = 90),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        panel.grid.major = element_blank())
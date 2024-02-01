# Izvor podataka: Allard and Bradshaw (1964)

library(ggplot2)

tip <- c("Tip 1","Tip 1","Tip 1","Tip 1","Tip 2","Tip 2","Tip 2","Tip 2",
         "Tip 3","Tip 3","Tip 3","Tip 3","Tip 4","Tip 4","Tip 4","Tip 4",
         "Tip 5","Tip 5","Tip 5","Tip 5","Tip 6","Tip 6","Tip 6","Tip 6")
lok <- c("Y","Y","X","X","Y","Y","X","X",
         "Y","Y","X","X","Y","Y","X","X",
         "Y","Y","X","X","Y","Y","X","X")
gen <- c("A","B","A","B","A","B","A","B",
         "A","B","A","B","A","B","A","B",
         "A","B","A","B","A","B","A","B")
y <- c(3,1,4,2,2,1,4,3,3,2,4,1,2,3,4,1,1,3,4,2,1,2,4,3)
lok <- factor(lok, levels = c("Y", "X"), ordered = TRUE)
df <- data.frame(tip,lok,gen,y)
df

ggplot(data = df,
       aes(x = factor(lok),
           y = y)) +
  geom_point(shape = 16,
             size = 3.5,
             color = "tomato") +
  geom_line(aes(group = factor(gen)),
            color = "tomato") +
  facet_wrap(~ tip, ncol = 2) +
  labs(x = "Lokalitet",
       y = "Prinos zrna",
       title = "Allard and Bradshaw (1964)") +
  theme_light() +
  theme(axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        strip.background = element_rect(fill = "tomato", size = 0.2),
        strip.text = element_text(size = 12, face = "bold", colour = "white")
        )
library(MASS)
library(ggplot2)

corrdata <- function(samples = 200, r = 0){
  data <- mvrnorm(n = samples, mu = c(0, 0), Sigma = matrix(c(1,r,r,1), nrow = 2), empirical = TRUE)
  X <- data[,1]
  Y <- data[,2]
  data.frame(x = X, y = Y)
}

df <- data.frame()
for (i in c(1,0.8,0.5,0.2,0.0,-0.2,-0.5,-0.8,-1.0)){
  tmp = corrdata(200, i)
  tmp["corr"] = i
  df <- rbind(df, tmp)
}

ggplot(data = df,
       aes(x = x,
           y = y)) +
  geom_point(size = 2.0,
             shape = 1,
             color = "tomato") +
  facet_wrap(~ corr) +
  stat_smooth(method = "lm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_light() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 12, hjust = 0.5, vjust = 0.5, angle = 90),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        strip.background = element_rect(color = "black", fill = "NA", size = 0.5, linetype = "solid"),
        strip.text = element_text(size = 12, color = "tomato", face = "bold"))
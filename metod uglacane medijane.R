
# izvor podataka ---
# Gupta S., Loughman R. (2001). Current virulence of Pyrenophora teres
# on barley in Western Australia. Plant Disease 85:960-966.

library(ggplot2)
library(reshape2)

df <- matrix(c(6.0,4.3,5.8,5.0,4.9,6.1,4.0,4.7,
               5.2,5.0,5.5,5.0,6.4,6.9,4.0,3.5,
               7.7,6.3,7.3,5.0,6.9,6.4,5.0,5.2,
               7.6,7.1,7.9,8.0,7.1,7.4,7.2,6.5,
               2.1,2.0,1.9,2.0,1.9,2.0,4.9,4.0,
               1.8,1.8,1.9,1.9,2.1,2.0,2.3,1.9,
               2.3,2.1,2.3,2.8,2.9,2.8,2.9,2.6,
               2.3,2.0,2.0,2.0,2.3,2.2,7.8,6.8,
               3.6,2.7,2.7,2.0,3.6,3.6,7.4,6.4,
               4.7,4.6,4.8,4.0,4.9,5.0,3.9,4.2,
               5.1,4.1,5.0,8.0,4.7,5.0,4.6,4.7,
               4.5,3.6,4.5,4.7,4.2,4.1,4.6,4.4,
               2.8,2.7,3.5,4.0,3.8,3.8,3.0,2.6), ncol = 8, byrow = TRUE)
rownames(df) <- c("Cameo","Clipper","Betzes","LG30","LG10","LG16","LG32","LG8","LG31","LG17","LG22","LG33","LG34")
colnames(df) <- c("IG86","IG75","IG87","QNB851","IG88","IG84","IG48","IG78")

# metod uglacane medijane ---
mod.fit <- medpolish(df)
mod.fit$overall
mod.fit$row
mod.fit$col
mod.fit$residuals
round(mod.fit$residual,1)

mat <- as.matrix(mod.fit$residuals)
R <- as.matrix(mod.fit$row)
K <- as.matrix(mod.fit$col)
R.K <- tcrossprod(R, K)

plot(mod.fit, cex.axis = 0.8, mgp = c(1.8,0.6,0))
abline(lm(as.vector(mat) ~ as.vector(R.K)), col = "tomato", lwd = 2)
fit <- lm(as.vector(mat) ~ as.vector(R.K))
fit$coefficients[2]
p <- 1-fit$coefficients[2]*(mod.fit$overall)
p

# dijagram reziduala ---
mod.fit.melt <- melt(mod.fit$residuals)
colnames(mod.fit.melt) <- c("x","y","res")
ggplot(data = mod.fit.melt,
       aes(x = x,
           y = y)) +
  geom_tile(aes(fill = res)) +
  scale_fill_gradient2(low = "gold",
                       high = "tomato",
                       mid = "white",
                       midpoint = 0) +
  geom_text(aes(x = x,
                y = y,
                label = round(mod.fit.melt$res, 2)),
            size = 4.0) +
  labs(x = "",
       y = "",
       title = "Metod uglacane medijane / Gupta & Loughman (2001)") +
  coord_flip() +
  theme_light()



# PCA ---

library(pdfCluster)
library(ggplot2)
library(ggrepel)
library(ggfortify)
library(GGally)

data(oliveoil)
dt <- oliveoil
dt$tree <- 1:572
dt$linolenic <- NULL
dt$arachidic <- NULL
dt$eicosenoic <- NULL
head(dt, 3)

# scatterplot ---
ggpairs(dt,
  columns = c("palmitic", "palmitoleic", "stearic", "oleic", "linoleic"),
  ggplot2::aes(colour = macro.area))

# PCA ---
prcomp.out <- prcomp(dt[, 3:7])
summary(prcomp.out)

autoplot(prcomp.out,
         colour = "macro.area",
         loadings.colour = "red",
         loadings = TRUE,
         loadings.label = TRUE,
         loadings.label.size = 5,
         data = dt)

biplot.dt <- data.frame(PC1 = prcomp.out$x[, 1],
                        PC2 = prcomp.out$x[, 2],
                        id = dt$tree,
                        Legend = dt$macro.area)

# plot 1 ---
ggplot(biplot.dt,
       aes(x = PC1,
           y = PC2,
           label = id,
       col = Legend)) +
  geom_point(size = 3,
             alpha = 0.3) +
  stat_ellipse(size = 1) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.88,0.12))

# plot 2 ---
ggplot(biplot.dt,
       aes(x = PC1,
       y = PC2,
       label = id,
       col = Legend)) +
  geom_point(size = 3,
             alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.12,0.88)) +
  geom_text_repel(data = subset(biplot.dt,
                                  PC1 > 500 & PC2 > 200),
                  mapping = aes(label = id),
                  size = 3)

# plot 3 ---
ggplot(biplot.dt,
       aes(x = PC1,
       y = PC2,
       label = id,
       col = Legend)) +
  geom_point(size = 3,
             alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.12,0.88)) +
  geom_text_repel(data = subset(biplot.dt, Legend %in% "Sardinia"),
                  mapping = aes(label = id),
                  size = 3)
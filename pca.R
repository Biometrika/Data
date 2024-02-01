# PCA ---

library(pdfCluster)
library(ggplot2)
library(ggrepel)
library(ggfortify)
library(GGally)
library(MASS)
library(patchwork)

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

# classic ---
dt.scaled <- scale(dt[, 3:7])
princomp.classic <- princomp(dt.scaled)
summary(princomp.classic)
autoplot(princomp.classic,
         colour = "macro.area",
         loadings.colour = "red",
         loadings = TRUE,
         loadings.label = TRUE,
         loadings.label.size = 5,
         data = dt)

# robust (mcd estimator) ---
princomp.robust <- princomp(dt.scaled, covmat = cov.mcd(dt.scaled))
summary(princomp.robust)
autoplot(princomp.robust,
         colour = "macro.area",
         loadings.colour = "red",
         loadings = TRUE,
         loadings.label = TRUE,
         loadings.label.size = 5,
         data = dt)

biplot.dt <- data.frame(PC1 = princomp.classic$scores[, 1],
                        PC2 = princomp.classic$scores[, 2],
                        rPC1 = princomp.robust$scores[, 1],
                        rPC2 = princomp.robust$scores[, 2],
                        id = dt$tree,
                        Legend = dt$macro.area)

# plot 1 ---
p1a <- ggplot(biplot.dt,
              aes(x = PC1,
                  y = PC2,
                  label = id,
                  col = Legend)) +
         geom_point(size = 1.5,
                    alpha = 0.5) +
         stat_ellipse(size = 1) +
         scale_color_brewer(palette = "Set1") +
         scale_x_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         scale_y_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         labs(x = "PC1 (65.9%)",
              y = "PC2 (19.1%)") +
         theme(legend.position = c(0.88,0.88))

p1b <- ggplot(biplot.dt,
              aes(x = rPC1,
                  y = rPC2,
                  label = id,
                  col = Legend)) +
         geom_point(size = 1.5,
                    alpha = 0.5) +
         stat_ellipse(size = 1) +
         scale_color_brewer(palette = "Set1") +
         scale_x_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         scale_y_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         labs(x = "r-PC1 (66.9%)",
              y = "r-PC2 (16.3%)") +
         theme(legend.position = c(0.88,0.88))
p1a + p1b


# plot 2 ---
p2a <- ggplot(biplot.dt,
              aes(x = PC1,
                  y = PC2,
                  label = id,
                  col = Legend)) +
         geom_point(size = 1.5,
                    alpha = 0.5) +
         scale_color_brewer(palette = "Set1") +
         geom_text_repel(data = subset(biplot.dt,
                                       PC1 > -2.5 & PC2 > 1.0),
                         mapping = aes(label = id),
                         size = 2.5) +
         scale_x_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         scale_y_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         labs(x = "PC1 (65.9%)",
              y = "PC2 (19.1%)") +
         theme(legend.position = c(0.88,0.88))

p2b <- ggplot(biplot.dt,
              aes(x = rPC1,
                  y = rPC2,
                  label = id,
                  col = Legend)) +
         geom_point(size = 1.5,
                    alpha = 0.5) +
         scale_color_brewer(palette = "Set1") +
         geom_text_repel(data = subset(biplot.dt,
                                       PC1 > -2.5 & PC2 > 1.0),
                         mapping = aes(label = id),
                         size = 2.5) +
         scale_x_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         scale_y_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         labs(x = "r-PC1 (66.9%)",
              y = "r-PC2 (16.3%)") +
         theme(legend.position = c(0.88,0.88))
p2a + p2b


# plot 3 ---
p3a <- ggplot(biplot.dt,
              aes(x = PC1,
                  y = PC2,
                  label = id,
                  col = Legend)) +
         geom_point(size = 1.5,
                    alpha = 0.5) +
         scale_color_brewer(palette = "Set1") +
         geom_text_repel(data = subset(biplot.dt, Legend %in% "Sardinia"),
                         mapping = aes(label = id),
                         size = 2.5) +
         scale_x_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         scale_y_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         labs(x = "PC1 (65.9%)",
              y = "PC2 (19.1%)") +
         theme(legend.position = c(0.88,0.88))

p3b <- ggplot(biplot.dt,
              aes(x = rPC1,
                  y = rPC2,
                  label = id,
                  col = Legend)) +
         geom_point(size = 1.5,
                    alpha = 0.5) +
         scale_color_brewer(palette = "Set1") +
         geom_text_repel(data = subset(biplot.dt, Legend %in% "Sardinia"),
                         mapping = aes(label = id),
                         size = 2.5) +
         scale_x_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         scale_y_continuous(breaks = seq(from = -10, to = 10, by = 0.5)) +
         labs(x = "r-PC1 (66.9%)",
              y = "r-PC2 (16.3%)") +
         theme(legend.position = c(0.88,0.88))
p3a + p3b
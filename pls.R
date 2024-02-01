### PLS model with R

library(reshape2)
library(ggplot2)
library(pls)
library(ade4)
library(qgraph)

dtY <- read.csv(file = "yield.csv", header = TRUE)
dtX <- read.csv(file = "variables.csv", header = TRUE)
head(dtY, 3)
head(dtX, 3)

### associations among environmental variables
qgraph(cor(dtX[2:37], method = "pearson"),
       graph = "cor", layout = "spring",
       shape = "circle", posCol = "#E41A1C", negCol = "#4DAF4A",
       edge.labels = TRUE, edge.label.cex = 0.7,
       label.cex = 1.0, vsize = 5, vTrans = 150, labels = colnames(dtX[2:37]),
       label.scale = TRUE, label.cex = 0.7, details = TRUE,
       minimum = "sig", alpha = 0.05, bonf = TRUE, sampleSize = 20)

dtX.pca <- dudi.pca(dtX[2:37], scannf = FALSE, nf = 2)
s.corcircle(dtX.pca$co, xax = 1, yax = 2)

### heatmap of yield variation
ggplot(data = dtY, aes(x = gen, y = env)) +
  geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = rainbow(3)) +
  geom_text(aes(x = gen, y = env, label = round(yield, 1)), size = 2.8, color = "white") +
  labs(x = "", y = "") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5, angle = 25),
        axis.text.y = element_text(size = 9, hjust = 0.5, vjust = 0.5))

Z <- dtY
Z <- acast(Z, env ~ gen, value.var = "yield")

### double-centered matrix
Z <- sweep(Z, 1, rowMeans(Z))
Z <- sweep(Z, 2, colMeans(Z))
melt.Z <- melt(Z)
colnames(melt.Z) <- c("Env","Gen","Interaction")
head(melt.Z, 3)

### heatmap of genotype-by-environment effects
ggplot(data = melt.Z, aes(x = Gen, y = Env)) +
  geom_tile(aes(fill = Interaction)) +
  scale_fill_gradientn(colours = rainbow(3)) +
  geom_text(aes(x = Gen, y = Env, label = round(Interaction, 2)), size = 2.8, color = "white") +
  labs(x = "", y = "", title = "Double centered G-by-E matrix of yield") +
  coord_flip() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5, angle = 25),
        axis.text.y = element_text(size = 9, hjust = 0.5, vjust = 0.5))

### g-by-e interaction (AMMI-2) biplot
biplot(prcomp(Z, scale = FALSE),
       cex = 0.85, cex.axis = 0.75, mgp = c(1.8,0.6,0))

### PLS model
U <- dtX[2:37]
U <- scale(U)
m1 <- plsr(Z ~ U, ncomp = 2)
print(m1)
summary(m1)

### Y scores vs Y loadings
biplot(m1, which = "y", var.axes = TRUE, cex = 0.85, cex.axis = 0.75, mgp = c(1.8,0.6,0))

### X scores vs X loadings
biplot(m1, which = "x", var.axes = TRUE, cex = 0.85, cex.axis = 0.75, mgp = c(1.8,0.6,0))

### X scores and Y scores
biplot(m1, which = "scores", var.axes = TRUE, cex = 0.85, cex.axis = 0.75, mgp = c(1.8,0.6,0))

### X loadings vs Y loadings
biplot(m1, which = "loadings", var.axes = TRUE, cex = 0.85, cex.axis = 0.75, mgp = c(1.8,0.6,0))

### loadings
plot(m1, "loadings", xaxt = "n")
axis(1, at = 1:ncol(U), labels = colnames(U), las = 2)
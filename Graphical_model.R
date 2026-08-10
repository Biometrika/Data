# --- Gaussian undirected graphical model ---

rm(list=ls(all=TRUE))

library(huge)
library(MASS)
library(ggplot2)
library(igraph)
library(patchwork)

set.seed(2026)
n <- 1000
p <- 20
traits <- paste0("T", 1:p)

dt <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(dt) <- traits

# latentni faktori za generisanje klastera svojstava
F1 <- rnorm(n)
F2 <- rnorm(n)
F3 <- rnorm(n)

# --- klaster 1 (jaka veza)
dt[, 1:5] <- dt[, 1:5] + F1 * 2.5

# --- klaster 2 (umerena veza)
dt[, 6:10] <- dt[, 6:10] + F2 * 1.5

# klaster 3 (slaba veza)
dt[, 11:15] <- dt[, 11:15] + F3 * 0.8

# --- specifične zavisnosti između klastera ---
dt[, 1] <- dt[, 1] + dt[, 6] * 1.5
dt[, 16] <- dt[, 10] * 2.0 + rnorm(n)
dt[, 17] <- dt[, 3] * -1.8 + rnorm(n)
dt[, 18] <- dt[, 11] * 1.2 + rnorm(n)

# --- standardizacija podataka ---
dt <- scale(dt)

# --- Meinshausen-Buhlmann algoritam i selekcija ---
out.mb <- huge(dt, method = "mb")

# --- izbog optimalnog model pomoću StARS kriterijuma ---
out.select.stars <- huge.select(out.mb, criterion = "stars") # c("ric", "stars")
print(out.select.stars)

# --- vizuelizacija ---

# --- matrica susedstva ---
adj_matrix <- as.matrix(out.select.stars$refit)
rownames(adj_matrix) <- traits
colnames(adj_matrix) <- traits

# --- kreiranje "undirected" grafa ---
net <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

# --- fiksiranje koordinata cvorova (Fruchterman-Reingold layout) ---
set.seed(2026)
layout_coords <- layout_with_fr(net)

# --- podaci o cvorovima ---
nodes_dt <- data.frame(
  name = V(net)$name,
  x = layout_coords[, 1],
  y = layout_coords[, 2])

# --- broj veza ---
nodes_dt$broj_veza <- degree(net)

# --- prepoznavanje klastera (Louvain algoritam) ---
klaster <- cluster_louvain(net)
nodes_dt$louvain_klaster <- as.factor(membership(klaster))

# podaci o granama
edges <- as_data_frame(net, what = "edges")
edges_dt <- merge(edges, nodes_dt, by.x = "from", by.y = "name")
edges_dt <- merge(edges_dt, nodes_dt, by.x = "to", by.y = "name", suffixes = c("_from", "_to"))

# --- graf 1 ---
ggplot() +
  geom_segment(data = edges_dt, aes(x = x_from, y = y_from, xend = x_to, yend = y_to), color = "gray60", linewidth = 0.8) +
  geom_point(data = nodes_dt, aes(x = x, y = y), size = 12, color = "gold") +
  geom_text(data = nodes_dt, aes(x = x, y = y, label = name), fontface = "bold", size = 4, color = "black") +
  labs(title = "Osnovni LASSO model") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 9))

# --- graf 2 ---
ggplot() +
  geom_segment(data = edges_dt, aes(x = x_from, y = y_from, xend = x_to, yend = y_to), color = "gray60", linewidth = 0.8) +
  geom_point(data = nodes_dt, aes(x = x, y = y, color = broj_veza), size = 12) +
  geom_text(data = nodes_dt, aes(x = x, y = y, label = name), fontface = "bold", size = 4, color = "white") +
  scale_color_gradient(low = "lightblue", high = "midnightblue") +
  labs(title = "Broj veza", color = "Broj veza") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))

# --- graf 3 ---
ggplot() +
  geom_segment(data = edges_dt, aes(x = x_from, y = y_from, xend = x_to, yend = y_to), color = "gray60", linewidth = 0.8) +
  geom_point(data = nodes_dt, aes(x = x, y = y, color = louvain_klaster), size = 12) +
  geom_text(data = nodes_dt, aes(x = x, y = y, label = name), fontface = "bold", size = 4, color = "black") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Louvain algoritam", color = "Zajednica") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))

# --- PCA (loadings plot) ---

# --- PCA ---
pca_res <- prcomp(dt, center = TRUE, scale. = TRUE)

# --- koordinata za varijable ---
pca_var <- as.data.frame(pca_res$rotation[, 1:2])
pca_var$name <- rownames(pca_var)

# --- % varijanse ---
var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)
pc1_lab <- paste0("PC1 (", round(var_explained[1] * 100, 1), "%)")
pc2_lab <- paste0("PC2 (", round(var_explained[2] * 100, 1), "%)")

# --- graf 4 ---
ggplot(pca_var, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "gray80", linewidth = 0.4) +
  geom_text(aes(label = name), size = 2.5, vjust = -0.5, color = "black") +
  labs(title = "PCA (korelacije promenljivih)", x = pc1_lab, y = pc2_lab) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 9))
library(pls)
library(ggplot2)
library(ggrepel)
library(see)

# --- ucitavanje podataka ---
X_raw <- read.csv("clima.csv", header = TRUE, row.names = 1)
Y_raw <- read.csv("yield.csv", header = TRUE, row.names = 1)
zajednicki <- intersect(rownames(X_raw), rownames(Y_raw))
X_mat <- as.matrix(X_raw[zajednicki, ])
Y_mat <- as.matrix(Y_raw[zajednicki, ])

# --- dvostuko centriranja matrice prinosa ---
Y_expected <- outer(rowMeans(Y_mat), colMeans(Y_mat), "+") - mean(Y_mat)
Y_GE <- Y_mat - Y_expected

# --- standardizacija X i Y matrica ---
X_std <- scale(X_mat)
Y_std <- scale(Y_GE)

# --- PLS model ---
m1 <- plsr(Y_std ~ X_std,
           ncomp = 5,
           method = "kernelpls", # c("oscorespls")
           validation = "LOO",
           center = FALSE,
           scale = FALSE)

# --- odredjivanje optimalnog broja dimenzija PLS modela ---
val_data <- RMSEP(m1, estimate = "CV")
val_values <- val_data$val[1, , ]

if (is.matrix(val_values)) {
  rmsep_scores <- colMeans(val_values)
} else {
  rmsep_scores <- val_values
}

dt_viz <- data.frame(
  ncomp = 0:(length(rmsep_scores) - 1),
  RMSEP = rmsep_scores)

ggplot(data = dt_viz,
       aes(x = ncomp,
           y = RMSEP)) +
  geom_line(color = "gray70", linewidth = 1) +
  geom_point(aes(color = ncomp == 0), size = 4) +
  geom_text_repel(aes(label = round(RMSEP, 3)), size = 3, vjust = -1) +
  geom_point(data = dt_viz[dt_viz$RMSEP == min(dt_viz$RMSEP), ],
             color = "red", size = 5, shape = 1, stroke = 2) +
  scale_color_manual(values = c("#98C008FF", "#F85858FF"), guide = "none") +
  labs(x = "Broj dimenzija",
       y = "RMSEP",
       title = "Odredjivanje optimalnog broja PLS dimenzija") +
  scale_x_continuous(breaks = 0:max(dt_viz$ncomp)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

# --- procentualno ucesce klimatskih varijabli u dimenzijama PLS modela ---
get_percentages <- function(model, X_input) {
  scores_mat <- scores(model) 
  n_vars <- ncol(X_input)
  n_comps <- ncol(scores_mat)
  res <- matrix(NA, nrow = n_vars, ncol = n_comps)
  rownames(res) <- colnames(X_input)
  colnames(res) <- paste("Dim", 1:n_comps, sep="_")
  for (i in 1:n_comps) {
    t_score <- scores_mat[, i]
    corrs <- cor(X_input, t_score)
    res[, i] <- (corrs ^ 2) * 100
  }
  return(round(res, 2))
}

print("% varijanse u dimenzijama PLS modela")
print(get_percentages(m1, X_std))

print("% varijanse interakcije objasnjen dimenzijama PLS modela")
print(explvar(m1))

# --- vizuelizacija ---
dt_loc  <- data.frame(scores(m1)[, 1:2])
dt_clim <- data.frame(loadings(m1)[, 1:2])
dt_gen  <- data.frame(Yloadings(m1)[, 1:2])

colnames(dt_loc) = colnames(dt_clim) = colnames(dt_gen) <- c("Dim1", "Dim2")

dt_loc$Label  <- rownames(dt_loc)
dt_clim$Label <- rownames(dt_clim)
dt_gen$Label  <- rownames(dt_gen)

normalize_to_one <- function(df) {
  max_val <- max(abs(df[, 1:2]))
  df[, 1:2] <- df[, 1:2] / max_val
  return(df)
}

dt_loc  <- normalize_to_one(dt_loc)
dt_gen  <- normalize_to_one(dt_gen)
dt_clim <- normalize_to_one(dt_clim)

# --- PLS triplot ---
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
  geom_segment(data = dt_loc,
               aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", alpha = 0.6) +
  geom_text_repel(data = dt_loc,
                  aes(x = Dim1, y = Dim2, label = Label),
                  color = "black", fontface = "bold", size = 3) +
  geom_point(data = dt_clim, aes(x = Dim1, y = Dim2),
             color = "#F05B43FF", shape = 18, size = 3) +
  geom_text_repel(data = dt_clim, aes(x = Dim1, y = Dim2, label = Label),
                  color = "#F05B43FF", size = 3) +
  geom_point(data = dt_gen, aes(x = Dim1, y = Dim2),
             color = "#6D8325FF", size = 3) +
  geom_text_repel(data = dt_gen, aes(x = Dim1, y = Dim2, label = Label),
                  color = "#6D8325FF", size = 3) +
  scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.2)) +
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.2)) +
  labs(x = paste0("Dimension 1 (", round(explvar(m1)[1], 1), "%)"),
       y = paste0("Dimension 2 (", round(explvar(m1)[2], 1), "%)"),
       title = "PLS triplot") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

# --- vizuelizacija % objasnjene varijanse klimatskih varijabli ---
perc_dt <- get_percentages(m1, X_std)
dt_perc <- data.frame(
  Variable = rep(rownames(perc_dt), 2),
  Percentage = c(perc_dt[, 1], perc_dt[, 2]),
  Dimension = rep(c("Dimenzija 1", "Dimenzija 2"), each = nrow(perc_dt)))

ggplot(data = dt_perc,
       aes(x = reorder(Variable, Percentage),
           y = Percentage,
           fill = Dimension)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Dimension,
             scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("#FCCC4CFF", "#EB6154FF")) +
  labs(x = "Klimatska varijabla", 
       y = "% objasnjene varijanse") +
  theme(axis.text.y = element_text(size = 8))

# --- vizuelizacija loadings-a ---
load_mat <- loadings(m1)[, 1:2]
dt_load <- data.frame(
  Variable = rep(rownames(load_mat), 2),
  Loading = c(load_mat[, 1], load_mat[, 2]),
  Dimension = rep(c("Dimenzija 1", "Dimenzija 2"), each = nrow(load_mat)))

ggplot(data = dt_load,
       aes(x = reorder(Variable, Loading),
           y = Loading,
           fill = Dimension)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Dimension,
             scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("#F8D058FF", "#A8D038FF")) +
  labs(x = "Klimatska varijabla", 
       y = "Loading") +
  theme(axis.text.y = element_text(size = 8))
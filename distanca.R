library(dplyr)
library(reshape2)
library(ggplot2)
library(ggrepel)

data(iris)
head(iris, 3)
iris[1:4]

dist.iris <- dist((iris[1:4]), method = "euclidean")
hc.iris <- hclust(dist.iris, method = "complete")
plot(hc.iris, hang = -1, cex = 0.4)

subset(melt(dist.iris), value! = 0)
head(subset(melt(dist.iris), value! = 0), 3)

dist.iris.matrix <- as.matrix(dist.iris)
dist.iris.matrix[1:5, 1:5]
subset(melt(dist.iris.matrix), value!=0)
head(subset(melt(dist.iris.matrix), value!=0), 3)

distParallel <- subset(melt(dist.iris.matrix), value!=0)
colnames(distParallel) <- c("ii", "jj", "yy")
head(distParallel, 3)
distParallel$ID <- 1:22348
distParallel$pair <- paste(distParallel$ii, distParallel$jj, sep = "vs.")
range(distParallel$yy)

distParallel %>%
 filter(yy < 0.14) %>%
  ggplot(aes(x = ID,
             y = yy)) +
    geom_point(size = 4,
               color = "olivedrab3") +
    geom_text_repel(aes(x = ID,
                        y = yy,
                        label = pair),
                    size = 4) +
    labs(x = "Pair",
         y = "Distance") +
    theme(axis.text.x = element_text(size = 11, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_text(size = 18, face = "bold"),
          axis.title.y = element_text(size = 18, face = "bold"))
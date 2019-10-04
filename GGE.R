
rm(list=ls(all=TRUE))
ls()

library(GGEBiplots)
library(gge)
library(ggplot2)

df <- read.csv("d:/prinos.csv", header = TRUE, row.names = "gen")
head(df, 3)

# GGE model ---
m1 <- gge(as.matrix(df))

# bazicni biplot ---
GGEPlot(m1,
        colGen = "blue",
        colEnv = "red3",
        sizeGen = 4.5,
        sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )


# uporedjenje genotipova ---
CompareGens(m1,
            "G2", "G5",
            colGen = "blue",
            colEnv = "red3",
            sizeGen = 4.5,
            sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )


# diskriminativnost vs reprezentativnost ---
DiscRep(m1,
        colGen = "blue",
        colEnv = "red3",
        sizeGen = 4.5,
        sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )


# odnos izmedju sredina ---
EnvRelationship(m1,
                colGen = "blue",
                colEnv = "red3",
                sizeGen = 4.5,
                sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )


# test sredina ---
ExamineEnv(m1,
           "E5",
           colGen = "blue",
           colEnv = "red3",
           sizeGen = 4.5,
           sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )


# test genotip ---
ExamineGen(m1,
           "G3",
           colGen = "blue",
           colEnv = "red3",
           sizeGen = 4.5,
           sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )


# performans i stabilnost genotipova ---
MeanStability(m1,
              colGen = "blue",
              colEnv = "red3",
              sizeGen = 4.5,
              sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )


# rangiranje sredina ---
RankEnv(m1,
        colGen = "blue",
        colEnv = "red3",
        sizeGen = 4.5,
        sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )


# rangiranje sredina u odnosu na idealni genotip ---
RankGen(m1,
        colGen = "blue",
        colEnv = "red3",
        sizeGen = 4.5,
        sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )


# w-w-w biplot ---
WhichWon(m1,
         colGen = "blue",
         colEnv = "red3",
         sizeGen = 4.5,
         sizeEnv = 5.5) +
  theme_light() +
  labs(x = "PC 1 (30.7)",
       y = "PC 2 (24.5)",
       title = "",
       caption = "") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
        )

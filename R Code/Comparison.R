library(scales)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(effsize)
library(Boruta)
library(Hmisc)

# Read the data
Top <-
  read.csv("Top.csv", header = T, stringsAsFactors = FALSE)
Bottom <-
  read.csv("Bottom.csv", header = T, stringsAsFactors = FALSE)

# Merge top and bottom
MergedData <- rbind(Top, Bottom)

# Print some general information
for (i in names(Top))
{
  if (is.numeric(Top[[i]]))
  {
    print(paste("Top", i, sep = "_"))
    print(summary(Top[[i]]))
    print(paste("Bottom", i, sep = "_"))
    print(summary(Bottom[[i]]))
  }
}

# Draw the boxplot
Metrics <-
  c("AnswerCount", "CommentCount", "FavoriteCount", "ViewCount")
for (i in Metrics)
{
  PlotResult <-
    ggplot(MergedData, aes(
      x = Type, y = log(MergedData[[i]] + 1), fill = Type
    )) +
    geom_boxplot(lwd = 1.0) +
    scale_y_continuous(
      breaks = pretty_breaks(), limits = c(0, NA), name = paste("log(", i, "+1)", sep = "")
    ) +
    scale_x_discrete(name = "") +
    theme_bw() +
    theme(
      text = element_text(size = 50, face = "plain"),
      axis.title.y = element_text(
        size = 40, colour = "black", margin = margin(0, 15, 0, 0)
      ),
      legend.position = "none"
    ) +
    scale_fill_brewer(palette = "Blues")
  
  print(PlotResult)
  ggsave(
    plot = PlotResult,
    device = cairo_pdf,
    filename = file.path("../images", paste(i, "pdf", sep = ".")),
    width = 10,
    height = 9
  )
}

# Data Preprocessing
MergedData <-
  subset(
    MergedData, select = -c(
      PostID, Score, ViewCount, AnswerCount, CommentCount, FavoriteCount
    )
  )
MergedData$Type[MergedData$Type == "High-Quality"]  <- 1
MergedData$Type[MergedData$Type == "Low-Quality"]  <- 0
MergedData$Type <- as.numeric(as.character(MergedData$Type))

# Correlation Analysis
HmiscData <- as.matrix(MergedData[, -1])
v <- varclus(HmiscData, similarity = "spear")
plot(v)

# Number of anomalous value
SimilarityMatrix <- round(v$sim, 2)
sum(SimilarityMatrix >= 0.7 & SimilarityMatrix < 1) / 2
HmiscData <- subset(HmiscData, select = -c(Letters, AvgWordLen, FleschKincaid, GunningFog))
v <- varclus(HmiscData, similarity = "spear")
plot(v)

# Save the plot
setEPS()
postscript("../images/VariableDeduction.eps", width = 14, height = 14, pointsize = 28)
plot(varclus(HmiscData, similarity = "spear"))
dev.off()

# Remove correlated factors
MergedData <-
  subset(
    MergedData, select = -c(
      Letters, AvgWordLen, ARI, GunningFog
    )
  )

# Redundancy Analysis
redun(~ ., data = as.data.frame(MergedData[, -1]), r2 = 0.8, type = "adjusted", nk = 0)

# Remove redundant factors
MergedData <-
  subset(
    MergedData, select = -c(
      PosScore, FleschKincaid
    )
  )

# Boruta Algorithm
set.seed(2048)
BorutaResult <-
  Boruta(
    Type ~ ., data = MergedData, pValue = 0.01, maxRuns = 1000, doTrace = 2
  )
print(BorutaResult)
plot(BorutaResult)

# Plot ranking result
Ranking <- attStats(BorutaResult)
Ranking <- Ranking[with(Ranking, order(-medianImp)), ]
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
Ranking <-
  data.frame(Feature = row.names(Ranking),
             ZScore = normalize(Ranking$medianImp))
Ranking$Feature <-
  factor(Ranking$Feature, levels = Ranking$Feature[order(Ranking$ZScore)])

RankingResult <-
  ggplot(Ranking, aes(x = Feature, y = ZScore)) +
  geom_bar(stat = "identity", width = .65, fill = "slategray2") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_bw() +
  theme(
    text = element_text(size = 25, face = "plain"),
    axis.text.x = element_text(margin = margin(6, 0, 0, 0)),
    axis.text.y = element_text(margin = margin(0, 6, 0, 0)),
    axis.title.x = element_text(
      size = 25,
      colour = "black",
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      size = 25,
      colour = "black",
      margin = margin(0, 7, 0, 0)
    ),
    legend.position = "none"
  ) +
  labs(x = "Textual Features", y = "Z-Score (Importance)") +
  scale_fill_brewer(palette = "Blues") +
  coord_flip()
print(RankingResult)

ggsave(
  plot = RankingResult,
  device = cairo_pdf,
  filename = file.path("../images", "RankingResult.pdf"),
  width = 10,
  height = 10
)

setwd("C:/Users/eliot/OneDrive/Desktop/437-Project-2/Code")

library(ape)
library(MVA)
library(tidyverse)
library(mclust)
library(cluster)

Data <- read_csv('../Data/top2018.csv')
NumericData <- Data %>% select(4:16) %>% scale()

PrinComps <- prcomp(NumericData)
summary(PrinComps)
PrinComps

PCdat <- as.tibble(PrinComps$x[,1:2])

PCdat %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()

Data %>% 
  ggplot(aes(x = danceability, y = energy)) +
  geom_point()

# Hierarchical Clustering -------------------------------------------------

Dist <- dist(NumericData)

plot(hclust(Dist, method = "complete"))
plot(hclust(Dist, method = 'single'))
plot(hclust(Dist, method = 'average'))

Clusters <- cutree(hclust(Dist, method = "complete"), h = 5)
ClusterMerger <- tibble(HClusters = Clusters)
ClustDat <- cbind(NumericData, ClusterMerger)

ClustDat <- ClustDat %>%
  mutate(HClusters = factor(HClusters))

variables <- colnames(ClustDat)

for(i in 1:14){
  for(j in 1:14){
    ClustDat2 <- ClustDat[,c(i,j, 14)]
    colnames(ClustDat2) <- list("x", "y", "HClusters")

    plot <- ClustDat2 %>% 
      ggplot(aes(x = x,y = y, color = HClusters)) +
      labs(x = paste(variables[i]), y = paste(variables[j])) +
      geom_point()
    
    ggsave(plot, filename = paste("../Hierarchical_Graphics/HClust_Plot_", variables[i], "_", variables[j], ".png", sep = ''))
  }
}

ClustDat %>%
  ggplot(aes(x = , y = , label = rownames(PCdat), color = HClusters)) +
  geom_point() +
  labs(title = "Scatterplot of Points in PC space", subtitle = "colored by cluster when cutting at a height of 7", color = "Cluster")

SilhouetteHAC <- silhouette(cutree(hclust(Dist, method = 'average'), 7), Dist)

# K-Means Clustering ------------------------------------------------------

n <- nrow(NumericData)
wss <- rep(0, 40)
wss[1] <- (n - 1) * sum(sapply(NumericData, var))
for (i in 2:40){
  wss[i] <- sum(kmeans(NumericData,
                       centers = i)$withinss)
}
plot(1:40, wss, type = "b", xlab = "Number of groups",
     ylab = "Within groups sum of squares") # decided to use k = 15

set.seed(1234)
KMeanClusts <- tibble(KClusters = kmeans(NumericData, centers = 15)$cluster)
ClustDat <- bind_cols(NumericData, KMeanClusts)

for(i in 1:14){
  for(j in 1:14){
    ClustDat2 <- ClustDat[,c(i,j, 14)]
    colnames(ClustDat2) <- list("x", "y", "KClusters")
    
    plot <- ClustDat2 %>% 
      ggplot(aes(x = x,y = y, color = factor(KClusters))) +
      labs(x = paste(variables[i]), y = paste(variables[j])) +
      geom_point()
    
    ggsave(plot, filename = paste("../KMeans_Graphics/KClust_Plot_", variables[i], "_", variables[j], ".png", sep = ''))
  }
}

PCdat %>%
  ggplot(aes(x = PC1, y = PC2, color = factor(KClusters))) +
  geom_point() +
  labs(title = "Scatterplot of Points in PC space", subtitle = "from K-Means Clustering with 15 Clusters", color = "Cluster")

SilhouetteKMeans <- silhouette(kmeans(NumericData, centers = 15)$cluster, Dist)


# Model Based Clustering --------------------------------------------------

MClusters <- Mclust(NumericData)

MClusts <- tibble(MClusts = MClusters$classification)
ClustDat <- cbind(NumericData, MClusts)
ClustDat <- ClustDat %>%
  mutate(MClusts = factor(MClusts))

for(i in 1:14){
  for(j in 1:14){
    ClustDat2 <- ClustDat[,c(i,j, 14)]
    colnames(ClustDat2) <- list("x", "y", "MClusts")
    
    plot <- ClustDat2 %>% 
      ggplot(aes(x = x,y = y, color = MClusts)) +
      labs(x = paste(variables[i]), y = paste(variables[j])) +
      geom_point()
    
    ggsave(plot, filename = paste("../Model_Based_Graphics/MClust_Plot_", variables[i], "_", variables[j], ".png", sep = ''))
  }
}

PCdat %>%
  ggplot(aes(x = PC1, y = PC2, color = MClusts)) +
  geom_point() +
  labs(title = "Scatterplot of Points in PC space", subtitle = "from Model-Based Clustering", color = "Cluster")

SilhouetteModelBased <- silhouette(Mclust(NumericData)$classification, Dist)


# Comparing Methods -------------------------------------------------------

SumOfSquares = function(vector){
  placeholder <- vector(length = length(vector))
  for(i in 1:length(vector)){
    placeholder[i] <- sqrt((vector[i])^2)
  }
  print(sum(placeholder))
}

SumOfSquares(SilhouetteHAC[,3])
SumOfSquares(SilhouetteKMeans[,3])
SumOfSquares(SilhouetteModelBased[,3])

#loading the packages
library(tidyverse)
library('foreach')
library('doParallel')
library(microbenchmark)

#Load the Data
load("data/lingBinary.RData")


#Sourcing the SimilarityCoeff function using C++ for faster computation
library(Rcpp)
Rcpp::sourceCpp("SimilarityCPP.cpp")
source("SimilarityR.R")


#Defining parameters for the stability algorithm

#Maximum number of clusters to consider
k_max <- 10

#The number of subsamples
num_subsamples <- 100

#The sampling Ratio
m <- 0.7

#Initializing the empty Similarity_Matrix
S <- matrix(data = NA, nrow = num_subsamples, ncol = k_max-1)



#Algorithm for deciding stable number of centeres for the data, run in parallel

nCores <- 4 
# to set manually
#registerDoParallel(nCores) 

foreach (k = (2:k_max)) %dopar% {
  for (i in c(1:num_subsamples)) {
    #Making the pair of two samples from the data and ordering
    sub_1 <- sample_n(lingBinary, size = ceiling(m *nrow(lingBinary)))
    sub_1 <- sub_1[order(sub_1$ID), ]
    sub_2 <- sample_n(lingBinary, size = ceiling(m *nrow(lingBinary)))
    sub_2 <- sub_2[order(sub_2$ID), ]
    
    #Intersecting the two samples and ordering
    intersect <- intersect(sub_1, sub_2)
    intersect <- intersect[order(intersect$ID), ]
    
    #Applying k_means on the two sub-samples with k centers
    L_1_kmeans <- kmeans(x = sub_1[ , c(7:ncol(sub_1))], centers = k)
    L_2_kmeans <- kmeans(x = sub_2[ , c(7:ncol(sub_1))], centers = k)
    
    #Labeling the two subsampled data sets with the cluster it belongs 
    sub_1$L_1 <- as.factor(L_1_kmeans$cluster)
    sub_2$L_2 <- as.factor(L_2_kmeans$cluster)
    
    #Labeling the intersected dataset with the cluster it belonges in the first sub-sample and the second sub_sample
    intersect$L_1 <- sub_1[order(sub_1$ID), ] %>% filter(ID %in% intersect$ID) %>% select(L_1) 
    intersect$L_2 <- sub_2[order(sub_2$ID), ] %>% filter(ID %in% intersect$ID) %>% select(L_2) 
    
    #Making L_1 as vector from data frame
    L1_intersect <- as.numeric(as.vector(unlist(intersect[["L_1"]])))
    L2_intersect <- as.numeric(as.vector(unlist(intersect[["L_2"]])))
    #Assigning the similarity co-efficient
    S[i, k-1] = SimilarityCoeffCPP(L1_intersect, L2_intersect)
  }
}




#Writing the Similarity matrix for exporting
S_df <- as.data.frame(S)
colnames(S_df) <- paste0("k = ", c(2:k_max))
write.csv(x = S_df, file = "similarity_matrix_final.csv")

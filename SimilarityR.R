#Defining the function to Calculate the Similarity Co-efficient in R 
#with matrix storage method

SimilarityCoeff <- function(L_1, L_2){
  C_1 <- matrix(data = NA, nrow = length(L_1), ncol = length(L_1))
  C_2 <- matrix(data = NA, nrow = length(L_2), ncol = length(L_2))
  
  #Making C_1
  for (i in c(1:length(L_1))) {
    for (j in c(1:length(L_1))) {
      if(i != j & L_1[i]==L_1[j]){C_1[i, j] <- 1}
      else{C_1[i, j] <- 0}
    }
  }
  
  #Making C_2
  for (i in c(1:length(L_2))) {
    for (j in c(1:length(L_2))) {
      if(i != j & L_2[i]==L_2[j]){C_2[i, j] <- 1}
      else{C_2[i, j] <- 0}
    }
  }
  
  #Correlation between C_1 and C_2
  #Calculating dot product of C_1 and C_2
  C_1dotC_2 <- sum(as.vector(C_1) * as.vector(C_2))
  C_1dotC_1 <- sum(as.vector(C_1) * as.vector(C_1))
  C_2dotC_2 <- sum(as.vector(C_1) * as.vector(C_2))
  
  return(C_1dotC_2/sqrt(C_1dotC_1 * C_2dotC_2))
}
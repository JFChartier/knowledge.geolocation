#functions

#function to project query in latent semantic space
#'@param matrixV, a term-dim matrix m*k, of m terms previouslly modelleled with a SVD of k latent dimensions
#'@param sigularValues, the k singular values of the train SVD
#'@param newData, a new document-term matrix n*m to be projected in the latent space of k dimensions 
predictFromTrainSvdModel<-function(matrixV, singularValues, newData)
{
  call <- match.call()
  tsa <-  newData %*% matrixV %*% solve(diag((singularValues)))
  result <- list(docs_newspace = tsa)
  return (result)
}

#function to create a unit normed vector
normVector <- function(x) 
{
  if(sum(x)==0)
    return (x)
  else 
    return (x / sqrt(sum(x^2)))
  
}
#function to norm many vectors
normRowVectors<-function(m){
  t(apply(m, MARGIN = 1, FUN = function(x) normVector(x)))
}

#function to compute the dot product between 2 vectors
#note that when dot-product is applied to 2 unit vectors, it is the same as computing the cosine metric
dotProduct <- function(x, y) sum(x * y)

#function to build query
#'@param queryTokens, a vector of tokens
#'@param mySVD, results from irba SVD. 2 objects are used: v, d and the original feature space 
buildQuery<-function(queryTokens, mySVD){
  queryVector=quanteda::dfm(quanteda::tokens(paste(queryTokens, collapse = " "), what="fastestword"), tolower = FALSE) #quanteda::tokens(paste(queryTokens, sep = " "), what="fastestword")
  #str(queryVector)
  #create a dummy empty sparse matrix 
  dumSparseMatrix=Matrix::sparseMatrix(i = c(1), j = c(1))
  dumSparseMatrix=quanteda::as.dfm(dumSparseMatrix)
  dumSparseMatrix@Dimnames$features=mySVD$original.features
  #str(queryTokens)
  queryVector = quanteda::dfm_select(x=queryVector, pattern = dumSparseMatrix, case_insensitive = F, valuetype="fixed")
  #print("here2")
  #str(queryVector)
  #projecting query in latent space
  myReducedQueryVector = predictFromTrainSvdModel(matrixV = (mySVD$v), singularValues = mySVD$d,  newData = as.matrix(queryVector))
  myReducedQueryVector=normVector(myReducedQueryVector$docs_newspace)
  #str(myReducedQueryVector)
  myReducedQueryVector
}




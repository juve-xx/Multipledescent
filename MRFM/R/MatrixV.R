#'@title  MatrixV
#'
#'@description  a function that return matrix V in MRFM.
#'@param nu the fixed points obtained from getuniquesolution
#'@param Values  The output from getvalue.
#'@return a matrix V.
#'@export
MatrixV<-function(nu,Values){
  list_mu1=Values[[1]]
  list_mus=Values[[2]]
  list_psi=Values[[3]]
  lbd=Values[[4]]
  psi=Values[[5]]

  MN=L0(nu,Values)[1]
  MD=L0(nu,Values)[2]
  v1=c(list_mus^2,0)
  v2<-c(numeric(length(list_mu1)),1)
  v3=c(list_mu1^2,MN^2)/MD^2
  v4=c(nu[length(nu)]^2*list_mu1^2,1)/MD^2
  V=cbind(v1,v2,v3,v4)
  V
}


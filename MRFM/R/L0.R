#'@title  L0
#'
#'@description  a function that return MN and MD in MRFM.
#'@param nu the fixed points obtained from getuniquesolution
#'@param Values  The output from getvalue.
#'@return MN and MD.
#'@export
L0<-function(nu,Values){
  list_mu1=Values[[1]]
  list_mus=Values[[2]]
  list_psi=Values[[3]]
  lbd=Values[[4]]
  psi=Values[[5]]

  MN=sum(list_mu1^2*nu[-length(nu)])
  MD=-1+nu[length(nu)]*MN
  c(MN,MD)
}


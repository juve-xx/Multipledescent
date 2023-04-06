#'@title  syseq
#'
#'@description  The implicit functions. Note that we eliminate the imaginary part and give the equation which the fixed points are real numbers larger than 0.
#'
#'@details  It gives the implicit functions. syseq(m)=m gives the statinary fixed points.
#'
#'@param m, a vector with length equal to the number of activation functions+1.
#'@param Values The output from getvalue.
#'@return a vector
#'@export
syseq<-function(m,Values){
  list_mu1=Values[[1]]
  list_mus=Values[[2]]
  list_psi=Values[[3]]
  lbd=Values[[4]]
  psi=Values[[5]]
  if(length(list_psi)!=length(list_mu1)) {
    stop("The length of psiK and activations are not equal.")
  }
  f<-numeric(length(m))
  Hd=1+sum((list_mu1^2*m[-length(m)]))*m[length(m)]
  for (j in 1:(length(m)-1)){
    f[j]=list_psi[j]/(lbd^0.5+list_mus[j]^2*m[length(m)]+list_mu1[j]^2*m[length(m)]/Hd)
  }
  f[length(m)]=psi/(lbd^0.5+sum(list_mus^2*m[-length(m)])+sum(list_mu1^2*m[-length(m)])/Hd)
  f
}

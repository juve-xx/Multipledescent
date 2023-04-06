#'@title  MatrixH
#'
#'@description  a function that return matrix H in MRFM.
#'@param nu the fixed points obtained from getuniquesolution
#'@param Values  The output from getvalue.
#'@return a matrix H.
#'@export
MatrixH<-function(nu,Values){
  list_mu1=Values[[1]]
  list_mus=Values[[2]]
  list_psi=Values[[3]]
  lbd=Values[[4]]
  psi=Values[[5]]

  if(length(list_psi)!=length(list_mu1)) {
    stop("The length of psiK and activations are not equal.")
  }

  MN=L0(nu,Values)[1]
  MD=L0(nu,Values)[2]
  K=length(nu)-1
  H1=matrix(numeric(length(nu)*length(nu)),ncol=length(nu))
  H2=H1
  nuK_1=nu[K+1]
  listnu=nu[-length(nu)]
  for (j in 1:K){
    H2[j,j]=-nuK_1^2*list_mu1[j]^4/MD^2+list_psi[j]/listnu[j]^2
  }
  H2[K+1,K+1]=-MN^2/MD^2+psi/nuK_1^2
  if (K==1){
    H1[1,2]=-list_mu1[1]^2/MD^2-list_mus[1]^2
    H=H2+H1+t(H1)
  }
  else{
    for (j in 2:K){
      for (i in 1:(j-1)){
        H1[i,j]=-nuK_1^2*list_mu1[i]^2*list_mu1[j]^2/MD^2
      }
    }
    for (i in 1:K){
      H1[i,K+1]=-list_mu1[i]^2/MD^2-list_mus[i]^2
    }
    H=H2+H1+t(H1)
  }
  H
}


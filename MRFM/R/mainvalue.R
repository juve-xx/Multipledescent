#'@title  mainvalue
#'
#'@description  a function that ensemble all hyper parameters in MRFM.
#'
#'@details  It is a function that ensemble all hyper parameters in MRFM. Users may use this function to give a precise MRFM model.
#'@importFrom stats integrate
#'@param Values  The output from getvalue.
#'@return a number: theoretical value of the specific MRFM.
#'@examples f1<-function(x){x};  Values=getvalue(0.001,10/3,1,0.1,f1,psi1=1);mainvalue(Values)
#'@examples f1<-function(x){x};  Values=getvalue(0.001,10/3,1,0.1,f1,ReLU,psi1=1,psi2=1.5);mainvalue(Values)
#'@export
mainvalue<-function(Values){
  list_mu1=Values[[1]]
  list_mus=Values[[2]]
  list_psi=Values[[3]]
  lbd=Values[[4]]
  psi=Values[[5]]
  F1=Values[[6]]
  tau=Values[[7]]
  k=length(list_mu1)
  if (k!=length(list_psi)){
    stop('The number of activation functions should equal to the number of psi')
  }

  r0=getuniquesolution(Values)

  V=MatrixV(nu=r0,Values)
  H=MatrixH(nu=r0,Values)
  MD=L0(r0,Values)[2]
  L=t(V)%*%solve(H)%*%V
  abs(F1^2*(1/MD^2+L[3,4]+L[1,4])+tau^2*(L[2,3]+L[1,2]))
}

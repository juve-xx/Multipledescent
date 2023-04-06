#'@title  getvalue
#'
#'@description  a function that ensemble all hyper parameters in MRFM.
#'
#'@details  It is a function that ensemble all hyper parameters in MRFM. Users may use this function to give a precise MRFM model.
#'
#'@param lbd a real number >0, the tuning parameter of ridge regression.
#'@param psi  n/d, n is sample size and d is sample dimension.
#'@param F1  larger or equal to 0, the norm of beta.
#'@param tau  standard deviation of noise.
#'@param ...  Input all activation functions and N_j/d.
#'@return a list, contain all hyper parameters.
#'@importFrom stats  integrate
#'@examples f1<-function(x){x};  Values=getvalue(0.001,10/3,1,0.1,f1,psi1=1)
#'@examples f1<-function(x){x};  Values=getvalue(0.001,10/3,1,0.1,f1,psi1=1,ReLU,psi2=2)
#'@export
getvalue<-function(lbd,psi,F1,tau,...){
  list_mu1=c()
  list_mus=c()
  list_psi=c()
  for(arg in list(...)) {
    if (is.function(arg)){
      f0<-function(x){
        arg(x)*exp(-x^2/2)/sqrt(2*pi)
      }

      f1<-function(x){
        arg(x)*x*exp(-x^2/2)/sqrt(2*pi)
      }

      f2<-function(x){
        (arg(x))^2*exp(-x^2/2)/sqrt(2*pi)
      }

      mu0=as.numeric(integrate(f0,-10,10)$value)
      mu1=as.numeric(integrate(f1,-10,10)$value)
      mus=sqrt(as.numeric(integrate(f2,-10,10)$value)-mu0^2-mu1^2)
      list_mu1=c(list_mu1,mu1)
      list_mus=c(list_mus,mus)
    }
    else{list_psi=c(list_psi,arg)}
  }
  list(list_mu1,list_mus,list_psi,lbd,psi,F1,tau)
}




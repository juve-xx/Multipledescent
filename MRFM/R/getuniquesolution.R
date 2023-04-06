#'@title  getuniquesolution
#'
#'@description   function that solve the equation syseq(m)=m, and return m*1i.
#'@param Values The output from getvalue
#'@return a vector. which is the solution of the fixed points.
#'@examples f1<-function(x){x};  Values=getvalue(0.001,10/3,1,0.1,f1,psi1=1); getuniquesolution(Values)
#'@export


getuniquesolution<-function(Values){
  list_mu1=Values[[1]]
  list_mus=Values[[2]]
  list_psi=Values[[3]]
  lbd=Values[[4]]
  psi=Values[[5]]
  k=length(list_mu1)
  a=rep(0.5,k+1)
  fun=function(m){ syseq(m,Values = Values)}
  while(sum(abs(fun(a)-a))>1e-7){
    a=fun(a)
  }
  a*1i
}

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
# 







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
  F1^2*(1/MD^2+L[3,4]+L[1,4])+tau^2*(L[2,3]+L[1,2])
}















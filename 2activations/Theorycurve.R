syseq<-function(m,u=0.001,psi1=1/3,psi2=1/3,psi3=10/3,mu11=0.5,mu21=0.206621,mu1s=sqrt(1/4-1/(2*pi)),mu2s=sqrt(0.006645662)){
  f <- numeric(length(m))
  f[1]=psi1/(u^0.5+mu1s^2*m[3]+mu11^2*m[3]/(1+mu11^2*m[1]*m[3]+mu21^2*m[2]*m[3]))
  f[2]=psi2/(u^0.5+mu2s^2*m[3]+mu21^2*m[3]/(1+mu11^2*m[1]*m[3]+mu21^2*m[2]*m[3]))
  f[3]=psi3/(u^0.5+mu1s^2*m[1]+mu2s^2*m[2]+(mu11^2*m[1]+mu21^2*m[2])/(1+mu11^2*m[1]*m[3]+mu21^2*m[2]*m[3]))
  f
}



L0<-function(m1,m2,m3){
  HN=m1*mu11^2+m2*mu21^2
  HD=-1+m3*HN
  1/HD
}


MatrixH<-function(m1,m2,m3){
  HN=m1*mu11^2+m2*mu21^2
  HD=-1+m3*HN
  H1=c(-m3^2*mu11^4/HD^2+psi1/m1^2,-m3^2*mu11^2*mu21^2/HD^2,-mu11^2/HD^2-mu1s^2)
  H2=c(-m3^2*mu11^2*mu21^2/HD^2,-m3^2*mu21^4/HD^2+psi2/m2^2,-mu21^2/HD^2-mu2s^2)
  H3=c(-mu11^2/HD^2-mu1s^2,-mu21^2/HD^2-mu2s^2,-HN^2/HD^2+psi3/m3^2)
  H=cbind(H1,H2,H3)
  H
}



MatrixV<-function(m1,m2,m3){
  HN=m1*mu11^2+m2*mu21^2
  HD=-1+m3*HN
  v1=c(mu1s^2,mu2s^2,0)
  v2<-c(0,0,1)
  v3=c(mu11^2/HD^2,mu21^2/HD^2,HN^2/HD^2)
  v4=c(m3^2*mu11^2/HD^2,m3^2*mu21^2/HD^2,1/HD^2)
  V=cbind(v1,v2,v3,v4)
  V
}


a=rep(0.5,3)
ratio=0.5
TTT1=NULL
for (iter in P){
  psi1=iter/d
  psi2=round(iter)/d
  psi=psi1+psi2
  fun<-function(m){ syseq(m,u=lambda,psi1 = psi1,psi2 =psi2,psi3=psi3,mu11 = mu11,mu21 = mu21,mu1s=mu1s,mu2s=mu2s)}
  r0=BBsolve0(a,fun)
  r=r0*(1i)
  V=MatrixV(r[1],r[2],r[3])
  H=MatrixH(r[1],r[2],r[3])
  LC=L0(r[1],r[2],r[3])
  L=t(V)%*%solve(H)%*%V
  
  HN=r[1]*mu11^2+r[2]*mu21^2
  HD=-1+r[3]*HN
  
  TTT1<-c(TTT1,F1^2*(LC^2+L[3,4]+L[1,4])+tau^2*(L[2,3]+L[1,2]))
}


plot((1+1)*P/n,TTT1,type="l")





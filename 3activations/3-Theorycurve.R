syseq<-function(m,u,psi1,psi2,psi3,psi4,mu11,mu21,mu31,mu1s,mu2s,mu3s){
  f <- numeric(length(m))
  Hd=1+mu11^2*m[1]*m[4]+mu21^2*m[2]*m[4]+mu31^2*m[3]*m[4]
  f[1]=psi1/(u^0.5+mu1s^2*m[4]+mu11^2*m[4]/(Hd))
  f[2]=psi2/(u^0.5+mu2s^2*m[4]+mu21^2*m[4]/(Hd))
  f[3]=psi3/(u^0.5+mu3s^2*m[4]+mu31^2*m[4]/(Hd))
  # f[4]=psi4/(u^0.5+mu4s^2*m[4]+mu41^2*m[4]/(Hd))-m[4]
  f[4]=psi4/(u^0.5+mu1s^2*m[1]+mu2s^2*m[2]+mu3s^2*m[3]+(mu11^2*m[1]+mu21^2*m[2]+mu31^2*m[3])/(Hd))
  f
}


L0<-function(m1,m2,m3,m4){
  HN=m1*mu11^2+m2*mu21^2+m3*mu31^2
  HD=-1+m4*HN
  1/HD
}


MatrixH<-function(m1,m2,m3,m4){
  HN=m1*mu11^2+m2*mu21^2+m3*mu31^2
  HD=-1+m4*HN
  H1=c(-m4^2*mu11^4/HD^2+psi1/m1^2,-m4^2*mu11^2*mu21^2/HD^2,-m4^2*mu11^2*mu31^2/HD^2,-mu11^2/HD^2-mu1s^2)
  H2=c(-m4^2*mu11^2*mu21^2/HD^2,-m4^2*mu21^4/HD^2+psi2/m2^2,-m4^2*mu21^2*mu31^2/HD^2,-mu21^2/HD^2-mu2s^2)
  H3=c(-m4^2*mu11^2*mu31^2/HD^2,-m4^2*mu21^2*mu31^2/HD^2,-m4^2*mu31^4/HD^2+psi3/m3^2,-mu31^2/HD^2-mu3s^2)
  H4=c(-mu11^2/HD^2-mu1s^2,-mu21^2/HD^2-mu2s^2,-mu31^2/HD^2-mu3s^2,-HN^2/HD^2+psi4/m4^2)
  H=cbind(H1,H2,H3,H4)
  H
}


MatrixV<-function(m1,m2,m3,m4){
  HN=m1*mu11^2+m2*mu21^2+m3*mu31^2
  HD=-1+m4*HN
  v1=c(mu1s^2,mu2s^2,mu3s^2,0)
  v2<-c(0,0,0,1)
  v3=c(mu11^2/HD^2,mu21^2/HD^2,mu31^2/HD^2,HN^2/HD^2)
  v4=c(m4^2*mu11^2/HD^2,m4^2*mu21^2/HD^2,m4^2*mu31^2/HD^2,1/HD^2)
  V=cbind(v1,v2,v3,v4)
  V
}


a=c(0.5,0.5,0.5,0.5)
TTT1=NULL
for (iter in P){
  
  psi1=round(iter)/d
  psi2=round(iter)/d
  psi3=round(iter*3)/d
  psi=psi1+psi2+psi3
  fun=function(m){ syseq(m,u=lambda,psi1 = psi1,psi2 = psi2,psi3=psi3,psi4=psi4,mu11 = mu11,mu21=mu21,mu31=mu31,mu1s=mu1s,mu2s=mu2s,mu3s=mu3s)}
  r0=BBsolve0(a,fun)
  r=r0*(1i)
  
  V=MatrixV(r[1],r[2],r[3],r[4])
  H=MatrixH(r[1],r[2],r[3],r[4])
  LC=L0(r[1],r[2],r[3],r[4])
  L=t(V)%*%solve(H)%*%V
  

  TTT1<-c(TTT1,F1^2*(LC^2+L[3,4]+L[1,4])+tau^2*(L[2,3]+L[1,2]))
}

P0=5*P/n
plot(P0,TTT1,type="l")





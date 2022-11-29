syseq<-function(m,u,psi1,psi2,psi3,psi4,psi5,mu11,mu21,mu31,mu41,mu1s,mu2s,mu3s,mu4s){
  f <- numeric(length(m))
  Hd=1+mu11^2*m[1]*m[5]+mu21^2*m[2]*m[5]+mu31^2*m[3]*m[5]+mu41^2*m[4]*m[5]
  f[1]=psi1/(u^0.5+mu1s^2*m[5]+mu11^2*m[5]/(Hd))
  f[2]=psi2/(u^0.5+mu2s^2*m[5]+mu21^2*m[5]/(Hd))
  f[3]=psi3/(u^0.5+mu3s^2*m[5]+mu31^2*m[5]/(Hd))
  f[4]=psi4/(u^0.5+mu4s^2*m[5]+mu41^2*m[5]/(Hd))
  f[5]=psi5/(u^0.5+mu1s^2*m[1]+mu2s^2*m[2]+mu3s^2*m[3]+mu4s^2*m[4]+(mu11^2*m[1]+mu21^2*m[2]+mu31^2*m[3]+mu41^2*m[4])/(Hd))
  f
}



L0<-function(m1,m2,m3,m4,m5){
  HN=m1*mu11^2+m2*mu21^2+m3*mu31^2+m4*mu41^2
  HD=-1+m5*HN
  1/HD
}


MatrixH<-function(m1,m2,m3,m4,m5){
  HN=m1*mu11^2+m2*mu21^2+m3*mu31^2+m4*mu41^2
  HD=-1+m5*HN
  H1=c(-m5^2*mu11^4/HD^2+psi1/m1^2,-m5^2*mu11^2*mu21^2/HD^2,-m5^2*mu11^2*mu31^2/HD^2,-m5^2*mu11^2*mu41^2/HD^2,-mu11^2/HD^2-mu1s^2)
  H2=c(-m5^2*mu11^2*mu21^2/HD^2,-m5^2*mu21^4/HD^2+psi2/m2^2,-m5^2*mu21^2*mu31^2/HD^2,-m5^2*mu21^2*mu41^2/HD^2,-mu21^2/HD^2-mu2s^2)
  H3=c(-m5^2*mu11^2*mu31^2/HD^2,-m5^2*mu21^2*mu31^2/HD^2,-m5^2*mu31^4/HD^2+psi3/m3^2,-m5^2*mu31^2*mu41^2/HD^2,-mu31^2/HD^2-mu3s^2)
  H4=c(-m5^2*mu11^2*mu41^2/HD^2,-m5^2*mu21^2*mu41^2/HD^2,-m5^2*mu31^2*mu41^2/HD^2,-m5^2*mu41^4/HD^2+psi4/m4^2,-mu41^2/HD^2-mu4s^2)
  
  H5=c(-mu11^2/HD^2-mu1s^2,-mu21^2/HD^2-mu2s^2,-mu31^2/HD^2-mu3s^2,-mu41^2/HD^2-mu4s^2,-HN^2/HD^2+psi5/m5^2)
  H=cbind(H1,H2,H3,H4,H5)
  H
}


MatrixV<-function(m1,m2,m3,m4,m5){
  HN=m1*mu11^2+m2*mu21^2+m3*mu31^2+m4*mu41^2
  HD=-1+m5*HN
  v1=c(mu1s^2,mu2s^2,mu3s^2,mu4s^2,0)
  v2<-c(0,0,0,0,1)
  v3=c(mu11^2/HD^2,mu21^2/HD^2,mu31^2/HD^2,mu41^2/HD^2,HN^2/HD^2)
  v4=c(m5^2*mu11^2/HD^2,m5^2*mu21^2/HD^2,m5^2*mu31^2/HD^2,m5^2*mu41^2/HD^2,1/HD^2)
  V=cbind(v1,v2,v3,v4)
  V
}
# 
# MatrixH(0.2,0.9,1.8,1)
# MatrixV(0.2,0.9,1.8,1)
# L=t(V)%*%solve(H)%*%V
a=rep(0.5,5)
TTT1=NULL
for (iter in P){
  
  psi1=round(iter)/d
  psi2=round(iter)/d
  psi3=round(iter)/d
  psi4=round(iter*3)/d
  psi=psi1+psi2+psi3+psi4
  fun=function(m){ syseq(m,u=lambda,psi1 = psi1,psi2 = psi2,psi3=psi3,psi4=psi4,psi5=psi5,mu11 = mu11,mu21=mu21,mu31=mu31,mu41=mu41,mu1s=mu1s,mu2s=mu2s,mu3s=mu3s,mu4s=mu4s)}
  r0=BBsolve0(a,fun)
  r=r0*(1i)
  
  V=MatrixV(r[1],r[2],r[3],r[4],r[5])
  H=MatrixH(r[1],r[2],r[3],r[4],r[5])
  LC=L0(r[1],r[2],r[3],r[4],r[5])
  L=t(V)%*%solve(H)%*%V
  
  
  TTT1<-c(TTT1,F1^2*(LC^2+L[3,4]+L[1,4])+tau^2*(L[2,3]+L[1,2]))
}

P0=(1+1+1+3)*P/n
plot(P0,TTT1,type="l")





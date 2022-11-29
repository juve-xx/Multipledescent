d=300
n=1000
N1=810
N2=810
nT=700
lambda=0.0001
tau=0.1
F1=1


N=N1+N2
P=c(30,60,80,100,200,300,350,400,450,475,480,485,490,495,505,510, 515, 520 ,525,550,600,650,700,750,800,850,900,950,975,1000,1025,1050,1100,1200,1300,1400,1500,1600)


#sigmoid
# mu10=1/(sqrt(2*pi))
# mu20=0.5
# mu11=0.5
# mu21=0.206621
# mu1s=sqrt(1/4-1/(2*pi))
# mu2s=sqrt(0.006645662)


#ReLU'*0.1
# mu10=1/(sqrt(2*pi))
# mu11=0.5
# mu1s=sqrt(1/2-mu11^2-mu10^2)
# 
# 
# mu20=0.05*400
# mu21=0.035*400
# mu2s=sqrt(0.005*160000-mu21^2-mu20^2)

#ELU
# mu10=as.numeric(integrate(f10,-8,8)$value)
# mu11=as.numeric(integrate(f11,-8,8)$value)
# mu1s=sqrt(as.numeric(integrate(f12,-8,8)$value)-mu10^2-mu11^2)
# 
# 
# mu20=as.numeric(integrate(f20,-8,8)$value)
# mu21=as.numeric(integrate(f21,-8,8)$value)
# mu2s=sqrt(as.numeric(integrate(f22,-8,8)$value)-mu21^2-mu20^2)
# c(mu10,mu11,mu1s)
# 
# c(mu20,mu21,mu2s)

#FReLU
# mu10=0
# mu11=0.25
# mu1s=0.25
# 
# mu20=0
# mu21=0.02
# mu2s=0.02
# 
# c(mu10,mu11,mu1s)
# 
# c(mu20,mu21,mu2s)


mu0=mu10
mu1=mu11
mus=mu1s




psi3=n/d
psi=(N1+N2)/d
psi1=N1/d
psi2=N2/d


s1=s2=t1=t2=p=0
# s1=2.9
# s2=0.03
# t1=1.9
# t2=2.7
# p=1.2



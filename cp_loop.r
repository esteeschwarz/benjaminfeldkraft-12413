a1<-c("aa","bb","cc","dd","ee","kk","mm")
a2<-c("ff","gg","hh","aa","bb","kk","ll")
print(a1)
print(a2)
la<-(length(a1)+length(a2))/2
print(la)
k<-0
simv<-c()
sim1<-a1[k]

sim2<-c(append(simv,sim1))
sim3<-function(k) {print("k") 
print(a1[k])}
print(sim2)
for(k in la){
	ifelse(a1[k]==a2[k],sim3,sim1<-1)
	#sim1<-c(a1[sim])
print (sim1)
print("siblins")
print (sim2)
#sim2<-append(simv,sim1,after=k)
sim<-0
}
print(sim2)
i<-0
b<-
b1<-c(1:3)
b2<-c("aa","bb")
b3<-function(b) append(b1,b2,b)
b6<-b3(1:2)
b4<-append(b3(b),b2)
print(b3)
print(b4)
print (b6)
for (i in 1:3){
#  b3(b)
  print(i)
  b5<-b3(i)
  print(b5)
}

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
b<-1
b1<-c(1:3)
b2<-c("aa","bb")
b5<-c(b1,b2)
b3<-function(b) {
for (k in 1:b){
print (b1)
  
}
}
b3(4)
  #b6<-b3(1:2)
b4<-function(x) append(b3(x),b2,after=x)
f1<-function(f) c(b1,b3(b))
f1(2)
print(b1)
print(b2)
x1<-2
print(b3(x1))
x2<-2
print(b4(x2))
#print (b6)
for (i in 1:5){
#  b3(b)
  print(i)
  b6<-b4(i)
  print(b6)
}

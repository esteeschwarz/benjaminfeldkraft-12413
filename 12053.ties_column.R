#12053.corpusLX
#GR spanisch

d5<-c(1,2,3,4,5,6,7,8,9,1,2,3,4,6,7,1,2,3,6,7,8,3)
#d5<-c("doof","schlau","dd","ee","ee","doof","blöd")
d11<-d5[1:7]
d6<-unique(d5)
posd6<-
posd7<-
x<-1
pos<-1


for (x in pos:length(d6)){
#   print (x)
 # print(d6[x])
  for (l in pos:length(d5)){
     print(l)
    ifelse (d6[x]==d5[l],posd6[x]<-posd6[x]+1,posd6[x])
    
  }
}

print (d6)
print(posd6-posd7)
d8<-posd6-posd7
d9<-unique(d8)
posd8<-d8
posd9<-d8
x<-1
pos<-1
for (x in pos:length(d9)){
  # print (x)
  #print(d6[x])
  for (l in pos:length(d8)){
    # print(l)
    ifelse (d9[x]==d8[l],posd9[x]<-posd9[x]+1,posd9[x])
    
  }
}
print (d8)
print (d9)
print(posd9-posd8)
d10<-posd9-posd8

x<-1
pos<-1
print("ties")

for (x in pos:length(d10)){
     print (d10[x])
  
}
#normalisierung
d1<-3353
d2<-6416
d3<-35349
d4<-986992

100
100/100000*1000000
 # token/corpusgrösze*1mio
#  token/1mio wörter
#12055.loop essai
#try to compare two string arrays and put out siblings within
a1<-c("aa","bb","cc","dd","ee","kk","mm")
a2<-c("ff","gg","hh","aa","bb","kk","ll")
print(a1)
print(a2)
la<-(length(a1)+length(a2))/2
print(la)
k<-0
simv<-c()
sim1<-a1[k]
aempty<-c(1:la)
aempty<-aempty*0
print (aempty)
#sim2<-c(append(simv,sim1))
#sim3<-function(k) {print("k") 
#print(a1[k])}
#print(sim2)
pos1<-0
#loop over length of array
for(k in 1:la){
  #if string(a1)==string(a2) markiere position mit 1
	#ifelse(a1[k]==a2[k],pos1<-1,pos1<-0)
	#sim1<-c(a1[sim])
  #function: if marker!=0, 
  z(pos1,k)
  
    z<-function(x,posz){
    ifelse(a1[k]==a2[k],pos1<-1,pos1<-0)
    
    postrue<-aempty[posz]<-1
    posfalse<-aempty[posz]<-0
    ifelse(x!=0,postrue,posfalse)
  
#  aempty[posz]<-y
  print(aempty)
  
  }
  #call z with position, marker of position
pos1<-0
}



print(sim2)
print(aempty[pos1])

###############
##works halfway
i<-0
b<-5
b0<-c()
b1<-c(1:3)
b2<-c("aa","bb")
b5<-c(b1,b2)
lb5<-length(b5)

b3<-function(b) {
  b0<-c()
  
  #  c<-b+1
  print(c)
for (k in 1:b){
print(k)
  print (b1)
b9<-append(b0,b5,after=k)
print(b9)  

}
  print(b9)
  }




b3(4)
################
cs<-"kk"
f2<-function(z){
f1<- function(x) {
  for (k in 1:x) {
    y<-cs2<-c(cs,cs)
    
    print(cs2)
    }
return(y)
  }


cs3<-f1(4)
}
print(cs3)
cs4<-f2(3)
print (cs4)
print (f2)
###########
#function base

z<-(function(x, y)
  { z <- x^2 + y^2;x+y+z }
#2,1: 4,2;4+2+2 =8
   )(1:8, 1)


print(z)

z<-function(x,y)
{z<- c(x,y,x,y)}

  

print(z(1,3))


##############



c0<-1:9
lc0<-length(c0)
lc0=2

#append after end
#after darf nicht gröszer sein als länge vector
append(c0, 1:10, after=lc0)
  


#b6<-b3(1:2)
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

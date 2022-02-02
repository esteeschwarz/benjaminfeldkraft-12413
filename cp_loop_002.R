#12055.loop essai
#try to compare two string arrays and put out siblings within
a1<-c("aa","bb","cc","dd","ee","kk","mm")
a2<-c("ff","bb","hh","aa","bb","kk","ll")
#sum
#1 create 2 arrays with strings
#2 compare array 1 array 2
#3 find siblings
#4 put out siblings in 3rd array
##2.1
##

print(a1)
print(a2)
#length of arrays
la<-(length(a1)+length(a2))/2
print(la)

##k<-0
##simv<-c()
##sim1<-a1[k]

#create empty array
aempty<-c(1:la)
aempty<-aempty*0
print (aempty)

##sim2<-c(append(simv,sim1))
##sim3<-function(k) {print("k") 
##print(a1[k])}
##print(sim2)

##2.1
##loop over length of arrays
##
#pos1<-0
out2<- function(in1){
for(k in in1){                             ###########loop 1
  ##if loop position a1==position a2
  ##let positionmarker <-1, else marker <-0
 ifelse(a1[k]==a2[k],pos1<-k,pos1<-0)
  ##create funktion calling loopnumber, position
 # z<-function(x,k){
    ##check if marker!=0, if true > position in empty array == 1
    ##print modified array
  
#    ifelse(x!=0,y<-1,y<-0)
    amod<-aempty[k]<-y
    amod2<-append(aempty,amod)
    print(aempty)
    return(amod)
    ##?how to return modified array to outside loop?
  #}
#y<- function(out1){
  
 #z(pos1,k)
}
  out2(1)
  
  ############ this routine works calling loop defined in function call
  out3<-function(in2)
  {
  
    for (k in in2){
      print(k)
      
    }
    k<-1
  }  
    out3(1:5)
    
    
  #with single run on pos 6 == 1
print(y)
#if function == true
}
print(y)
print(k)
chkposit<-append(aempty,amod,after=0)
ifelse (y==1,chkposit,0)
#print(y[k])
#no access to amod within function
#print(z(amod))
#append(aempty[k],aempty[])
#  pos1<-0
}                                          ##########loop 1


print(aempty)



print(sim2)
print(aempty[k])

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
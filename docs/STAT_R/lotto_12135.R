# lotto




pool<-c(1:49)

a<-sample(pool,6)

b<-sample(pool,6)

c1<-c(2,12,13,27,33,42)
c2<-c(7,15,23,27,33,42)
c3<-c(4,10,16,19,20,40)
c4<-c(5,11,16,17,22,46)

 #7/15/23/24/27/37 # nolint
 #4/10/16/19/20/40
#c)5/11/16/17/22/46


proof2<-function(wh,k,s){

k<-k

  a<-c(1:49)

  a1<-sample(a,6)

  a3<-sample(a,6)

  c<-c(2,12,13,27,33,42)

  a3<-s

  ma<-match(a1,a3)

  #a2<-unique(a1)

  print(a1)

  mac<-c(NA,NA,NA,NA,NA,NA)

#ifelse(match(ma,mac)==T,map<-1,map<-0)

    stopifnot(sum(!is.na(match(a1,a3)))<=4)

  cat("keine wiederholungen in",k, "durchläufen\n")

  #print(k)



}
#for (p in 1:2){
  #cat("run",p,"\n")
for (k in 1:1000){

  proof2(T,k,c4)

}
#}

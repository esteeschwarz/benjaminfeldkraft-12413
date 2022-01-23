#12042.chitest_essai
#20220123(21.08)
#corpus LX

fbj<-c(230,70,70,30)
fej<-c(100,100,100,100)
x<-fbj-fej
x2<-x*x
y<-x2/fej
s<-sum(y)
#chisq.test(fbj,fej)
a<-230
b<-70
c<-70
d<-30
#a2<-100
#b2<-100
#c2<-100
#d2<-100
pa<-0.25
pb<-0.25
pc<-0.25
pd<-0.25
e<-c(a,b,c,d)
#em<-matrix(e)
#f<-c(a2,b2,c2,d2)
f<-c(pa,pb,pc,pd)
#fm<-matrix(f)
#jm<-matrix(e,f)
#chisq.test(em,fm)
#e<-matrix(a,b,c,d)
#jm<-matrix(em,fm)

#m<-matrix(1:4,2)
#n<-matrix(e,4)
#o<-matrix(f,4)
print("12042.corpusLX")

print("-------------------------")
print("(1) meindl uebung")
ch<-cbind(e)
chisq.test(ch,p=f,correct=FALSE)
print("-------------------------")

#übung meindl vierfeldertest, daten rosemeyer
sgen<-c(10,30)
ofgen<-c(40,20)
liv<-c("animate","inanimate")
print("(2) rosemeyer uebung")
chi2<-cbind(sgen,ofgen)
f2<-c(0.4,0.6)
chisq.test(chi2,p=f2,correct=FALSE)

print("-------------------------")
##meindl uebung U test, set C
cst1<-c(22, 22, 29, 30, 32, 30, 21, 29, 28, 29, 25)
cst2<-c(21, 25, 20, 22, 20, 22, 20, 23, 25, 28, 23)
#ccpt<-c(22, 22, 29, 30, 32, 30, 21, 29, 28, 29, 25,21, 25, 20, 22, 20, 22, 20, 23, 25, 28, 23)
c3<-c(cst1,cst2)
c5<-rank(c3)

c6<-sum(c5[1:11])
c7<-sum(c5[12:22])
c8<-cbind(c5,c3)


#rosemeyer uebung set D
d1<-c(7, 10, 12, 12, 18,  6 , 6, 18, 18, 12,  7 ,17,  9,  6 ,16)
d2<-c(17,  5, 13, 20,  6,  6,  9, 12, 18,  7, 14, 17, 17,  6, 18)
d3<-c(d1,d2)
d5<-rank(d3)
d4<-cbind(d5,d3)
d6<-sum(d5[1:15])
d7<-sum(d5[16:30])

dd1<-c(d1[1],d1[2],d1[3],d1[4],d1[5],d1[6],d1[7],d1[8],d1[9],d1[10],d1[11],d1[12],d1[13],d1[14],d1[15])

d11<-cbind(dd1)
dd1<-c(d1[1]:d1[length(d1)])
dd2<-c(d2[1]:d2[length(d2)])
d1l<-length(d1)/length(d1)
d2l<-length(d2)
dd<-cbind(d1,d2)
d3<-c(d1,d2)
d3<-c(d1,d2)
c3<-d3
c3<-c(cst1,cst2)
c3<-d3
#csort<-array(sort(c3))
#a<-csort[1]
#crank<-rank(csort)
#cranksum<-sum(crank)
#tab1<-cbind(c(1:length(csort)),csort,crank)
#a1<-c(tab1[4,3],tab1[8,3],tab1[9,3],tab1[13,3],tab1[15,3],tab1[17,3],tab1[18,3],tab1[19,3],tab1[20,3],tab1[21,3],tab1[22,3])
#a2<-c(tab1[5,3],tab1[6,3],tab1[7,3],tab1[10,3],tab1[11,3],tab1[12,3],tab1[16,3],tab1[1,3],tab1[2,3],tab1[3,3],tab1[14,3])
#tab2<-cbind(a1,a2)
#r1<-sum(a1)
#r2<-sum(a2)

#change values according to set C or D
#a1<-cst1
#a2<-cst2
#r1<-c6
#r2<-c7
a1<-d1
a2<-d2
r1<-d6
r2<-d7
#################
n1<-length(a1)
n2<-n1+1
u1<-r1-((n1*n2)/2)
u2<-r2-((n1*n2)/2)
#should be 0
uproof<-(u1+u2)-(n1*n1)
Ucpt<-c(u1,u2)
usort<-sort(Ucpt)
umin<-usort[1]

print("(3) U-test according to set")
print("stichproben:")
print(a1)
print (a2)
print(z1o<-(umin-((n1*n1))/2))
print(z1u<-sqrt(((n1*n1)*(n1+n1+1))/12))
print(z1<-z1o/z1u)



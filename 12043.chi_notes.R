#12042.chitest_essai
#20220123(21.08)
#corpus LX


print("12042.corpusLX")

print("-------------------------")
print("(1) meindl uebung")
#meindl chisquare example

#datensatz
a<-230
b<-70
c<-70
d<-30
#wahrscheinlichkeiten 100/400 samples > p = 0.25
pa<-0.25
pb<-0.25
pc<-0.25
pd<-0.25

e<-c(a,b,c,d)
f<-c(pa,pb,pc,pd)

chi1<-cbind(e)
chisq.test(chi1,p=f,correct=FALSE)
print("-------------------------")


#datensatz rosemeyer übung
#sgen<-c(10,30)
#ofgen<-c(40,20)
#liv<-c("animate","inanimate")
#chi2<-cbind(sgen,ofgen)
#wahrscheinlichkeiten summe 40/60
#f2<-c(0.4,0.6)
#chisq.test(chi2,p=f2,correct=FALSE)
#print("-------------------------")
#übung meindl vierfeldertest
print("(2) rosemeyer uebung")

#datensatz rosemeyer übung #live
sgen<-c(125,64) #spalte 1
ofgen<-c(69,145) #spalte 2
#rose: rbind
#hoch signifikant, wir können die nullhypothese ablehnen
#sgen 22,18
#ofgen 28,32
psgen<-22+18
pofgen<-28+32
liv<-c("animate","inanimate") # zeilen
chi2<-cbind(sgen,ofgen)
round(prop.table(chi2)*100)
#spaltensumme * zeilensumme/ n
#22-18o 28-32u
#wahrscheinlichkeiten summe 40/60
# p (sgen, fgen)
f2<-c(psgen,pofgen)
chisq.test(chi2,correct=FALSE)
#ohne probability angabe auch möglich
print("-------------------------")

##meindl uebung U test, set C

#stichprobe 1,2
cst1<-c(22, 22, 29, 30, 32, 30, 21, 29, 28, 29, 25)
cst2<-c(21, 25, 20, 22, 20, 22, 20, 23, 25, 28, 23)

c3<-c(cst1,cst2)
c5<-rank(c3)

#sum ranking
c6<-sum(c5[1:11])
c7<-sum(c5[12:22])

c8<-cbind(c5,c3)


##rosemeyer uebung set D
#stichproben 1,2
d1<-c(7, 10, 12, 12, 18,  6 , 6, 18, 18, 12,  7 ,17,  9,  6 ,16)
d2<-c(17,  5, 13, 20,  6,  6,  9, 12, 18,  7, 14, 17, 17,  6, 18)
d3<-c(d1,d2)
#wilcox test
d5<-rank(d3)
d4<-cbind(d5,d3)
d6<-sum(d5[1:15])
d7<-sum(d5[16:30])

#change values according to stichproben set C or D by add/remove comment (#)
#chose set C (meindl daten)
#a1<-cst1
#a2<-cst2
#r1<-c6
#r2<-c7
#choose set D (rosemeyer daten)
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



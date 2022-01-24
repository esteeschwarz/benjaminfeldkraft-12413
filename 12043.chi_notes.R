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

pra<-(prop.table(e)*100)
paa<-sum (pra[1:2])
pbb<-sum (pra[3:4])
p3<-c(paa,pbb)
chi1<-cbind(e)
chisq.test(chi1,p=pra,correct=FALSE)
print("-------------------------")
57.5+17.5+17.5+7.5
#sqrt(11*12)
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
#p sgen 22,18
#p ofgen 28,32
#psgen<-22+18
#pofgen<-28+32
#liv<-c("animate","inanimate") # zeilen
chi2<-cbind(sgen,ofgen)
pr1<-round(prop.table(chi2)*100)
psgen<-sum (pr1[1:2])
pofgen<-sum (pr1[3:4])

#spaltensumme * zeilensumme/ n
#22-18o 28-32u
#wahrscheinlichkeiten summe 40/60
# p (sgen, fgen)
f2<-c(psgen,pofgen)
chisq.test(chi2,p=f2,correct=FALSE)
#ohne probability angabe -f2- auch möglich
print("-------------------------")

##meindl uebung U test, set C
#################
#################
#################
#hier ist der teil, in dem ich krampfhaft versuche, den 
#von meindl angenommenen standardfehler [12]
#der stichporben des U-test beispiels
#zu berechnen. vielleicht können Sie mir helfen...
#ich habe das ganze buch auf varianz, standardfehler usw.
#durchgearbeitet. niemals komme ich auf 12.
#################################
#################################
#stichprobe meindl u-test übung
cst1<-c(22, 22, 29, 30, 32, 30, 21, 29, 28, 29, 25)
cst2<-c(21, 25, 20, 22, 20, 22, 20, 23, 25, 28, 23)
n1<-length(cst1)
n2<-length(cst2)

c3<-c(cst1,cst2)
#sqrt(sd(c3)/(n1+n2))

#cf. p. 182 "die quadrierten abweichungen
#durch n-1 teilen
sdc1sig<-sd(cst1)^2/(n1-1)
#1.46 != 3.82 > not sigma dach
mnc1<-mean(cst1)
#27 == 27 > check
mnc2<-mean(cst2)
#22.636 == 22.636
sdc1<-sd(cst1)
#3.82 == 3.82 > is sigma dach
sdc2<-sd(cst2)
#2.54
#t-test cf. p.183
sqrt((n1-1)*sdc1+(n2-1)*sdc2)/(n1-1+(n2-1))*(sqrt(1/n1+1/n2))
difmn1<-(n1-1)*sdc1^2
#38.209 != 64.54 > mit ^2 geht, p.183 steht sigma ohne ^2
difmn2<-(n2-1)*sdc2^2
#25.4058 != 146  > mit ^2 geht
#nochmal
#standardfehler der differenz der mittelwerte
stdifmn<-sqrt((difmn1+difmn2)/((n1-1)+(n2-1)))*sqrt((1/n1)+(1/n2))
#1.383 == 1.383
#t-wert
tc1<-(mnc1-mnc2)/stdifmn
dfc1<-(n1+n2-2)/2

csd<-sd(c3)
(sum(c3-mn3)^2)/21
sd3<-(c3-mn3)^2
varp3<-sum(sd3[1:22])/21
sqrt(varp3/22)
(csd*csd)/20
vr3<-var(c3)
(vr3*vr3)/21
############################
############################
############################



c5<-rank(c3)

wilcox.test(cst1-cst2,correct=FALSE)
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
wilcox.test(d3)
wilcox.test(d1,d2,alternative="greater")
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



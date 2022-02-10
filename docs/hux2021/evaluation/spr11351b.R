##ST.SCHWARZ/11351b/HUX20210829(00.47)

##von hier der reihe nach:

##begin
##import data
##library(readr)
library(lme4)

sprdataprepared <- read.csv("https://common.rotefadenbuecher.de/uni/public/jespr/modified/sprdataprepared.csv", sep=";")
##View(sprdataprepared)
##top datenframe aus datei, gültige fälle, target 0+1
sprdatasm<-sprdataprepared
##sprdatasm<-sprdata11342dtaxp ##für laden aus dateisystem, importiertes set hier einsetzen
#dtatargetgilt<-subset(sprdatasm, gilt==1)
#for complete dataset next
dtatargetgilt<-sprdatasm
dtatarget<-subset(dtatargetgilt, target==0|target==1)
##dtatarget<-subset(sprdatasm, target==0|target==1)
##dtaexcerpt<-subset(sprdatasm, (item =="2"| item=="5"|item=="6"|item=="10"|item=="11"|item=="12"|item=="18"|item=="26")&(target==0|target==1)&timeinterval>250&timeinterval<6730)
##dtatarget<-dtaexcerpt

##discard outliers
sprmean<-mean(dtatarget$timeinterval)
stdev<-sd(dtatarget$timeinterval)
sdout<-stdev*2.5
outtop<-sprmean+sdout
outbottom<-sprmean-sdout ## negative
#threshold to obs. 499=319ms
outbottommod<-319


##dies hier für die auswertung single metaphor vs. other
liste<-subset(dtatarget,timeinterval<outtop&timeinterval>outbottommod)
dtatarget<-liste
X_SMvsO<-dtatarget$category

##sd(dtatarget$wds)
##sm-em-lc-mm kategorien in spalte $group
sm<-"SM"
em<-"EM"
lc<-"LC"
mm<-"MM"
##hier kategorien x vs y einsetzen comme: (...group==sm|...group==em)
##liste<-subset(dtatarget,dtatarget$group==x|dtatarget$group==y)
SMvsEM<-subset(dtatarget,group==sm|group==em)
SMvsLC<-subset(dtatarget,group==sm|group==lc)
SMvsMM<-subset(dtatarget,group==sm|group==mm)
EMvsLC<-subset(dtatarget,group==em|group==lc)
EMvsMM<-subset(dtatarget,group==em|group==mm)
LCvsMM<-subset(dtatarget,group==lc|group==mm)

A_SMvs<-SMvsEM$group
B_SMvs<-SMvsLC$group
C_SMvs<-SMvsMM$group
D_EMvs<-EMvsLC$group
E_EMvs<-EMvsMM$group
F_LCvs<-LCvsMM$group

##response/RT: addierte lesezeit der targetphrase (0) + target+1 (1)
##proct = timeinterval/wordcount*average wordcount
##addproct = added timeinterval/added wordcount * avg wordcount
SPR_A=SMvsEM$timeinterval
SPR_B=SMvsLC$timeinterval
SPR_C=SMvsMM$timeinterval
SPR_D=EMvsLC$timeinterval
SPR_E=EMvsMM$timeinterval
SPR_F=LCvsMM$timeinterval


##SPR_A=SMvsEM$addproct
##SPR_B=SMvsLC$addproct
##SPR_C=SMvsMM$addproct
##SPR_D=EMvsLC$addproct
##SPR_E=EMvsMM$addproct
##SPR_F=LCvsMM$addproct

##SMvsOthers chars complett für set
#hist(residuals(lmm.model_RT))
##Length-corrected RTs [77] were computed by regressing the remaining raw RTs 
##onto word length using linear mixed effects regression. 
##In addition to a single main effect of word length, this model included a 
##random intercept for subject, and a by-subject random slope for length 
##(these random effects allow the model to discount mean differences in raw RT across 
##subjects as well as variable sensitivity to the effect of word length across subjects). 
##The residuals of this model, length-corrected RTs, served as the dependent variable in 
##all analyses reported below. 
##Qualitatively identical results are obtained if raw RTs are used instead. 
##(Fine, Alex B et al. “Rapid Expectation Adaptation During Syntactic Comprehension.” PloS one 8.10 (2013))
##R/F: "main effect of Category (single vs. other) and random effects of Participant and Item, 
##along with a random slope of Category by Participant > RT ~ group +(1|item)+(1+group|participant)
lme1.formula.1<-(adinterval ~addchar + (1|participant)+(1+char:participant))
lme1.formula.2<-(timeinterval ~ char + (1|participant)+(char:participant))
lme1<-lme1.formula.1
#lme1<-lme1.formula.2
##


RT_1<-lmer((lme1),SMvsEM)
RT_2<-lmer((lme1),SMvsLC)
RT_3<-lmer((lme1),SMvsMM)
RT_4<-lmer((lme1),EMvsLC)
RT_5<-lmer((lme1),EMvsMM)
RT_6<-lmer((lme1),LCvsMM)
RT_7<-lmer((lme1),dtatarget)

#complete:
#resixp7<-residuals(lmer(timeinterval ~ char + (1|participant)+(char:participant),dtatarget))

summary (RT_1)

##1-------descriptive analysis
subdescr<-subset(dtatarget,target==0)

#mean addierte zeichenanzahl target0+1
meanadchar<-mean(subdescr$addchar)


SSM<-subset(subdescr,group=="SM")
SEM<-subset(subdescr,group=="EM")
SLC<-subset(subdescr,group=="LC")
SMM<-subset(subdescr,group=="MM")

#timeinterval=LZ, target 
#speedSM<-mean(SSM$char/SSM$timeinterval*1000)
#speedEM<-mean(SEM$char/SEM$timeinterval*1000)
#speedLC<-mean(SLC$char/SLC$timeinterval*1000)
#speedMM<-mean(SMM$char/SMM$timeinterval*1000)

#adinterval=lesezeiten target(0) + lesezeit target(1)
mnspeedSMd<-mean(SSM$addchar/SSM$adinterval*1000)
mnspeedLCd<-mean(SLC$addchar/SLC$adinterval*1000)
mnspeedEMd<-mean(SEM$addchar/SEM$adinterval*1000)
mnspeedMMd<-mean(SMM$addchar/SMM$adinterval*1000)

# lesezeit = lesegeschwidigkeit * 1000(ms) * durchschnittliche zeichenlänge target 0+1
LZSMbx<-(SSM$addchar/SSM$adinterval*1000*meanadchar)
LZEMbx<-(SEM$addchar/SEM$adinterval*1000*meanadchar)
LZLCbx<-(SLC$addchar/SLC$adinterval*1000*meanadchar)
LZMMbx<-(SMM$addchar/SMM$adinterval*1000*meanadchar)

mnspAd<-mean(mnspeedSMd)
mnspBd<-mean(mnspeedEMd)
mnspCd<-mean(mnspeedLCd)
mnspDd<-mean(mnspeedMMd)

mdspAd<-median(LZSMbx)
mdspBd<-median(LZEMbx)
mdspCd<-median(LZLCbx)
mdspDd<-median(LZMMbx)


LSAd<-1000/mnspAd*meanadchar
LSBd<-1000/mnspBd*meanadchar
LSCd<-1000/mnspCd*meanadchar
LSDd<-1000/mnspDd*meanadchar


#plot mean addierte lesezeiten
boxread<-cbind(SM=LSAd,EM=LSBd,LC=LSCd,ISM=LSDd)
boxplotLZ<-cbind(SM=LZSMbx,EM=LZEMbx,LC=LZLCbx,ISM=LZMMbx)

boxLZmd<-cbind(SM=mdspAd,EM=mdspBd,LC=mdspCd,ISM=mdspDd)
#boxplotLZmd<-cbind(SM=LZSMbx,EM=LZEMbx,LC=LZLCbx,ISM=LZMMbx)
boxplot(boxLZmd,main="median Lesezeiten gesamt in ms/106 zeichen für target und target+1",xlab="106 Zeichen = durchschnittliche Länge target 0+1")


boxplot(boxread,main="mean Lesezeiten gesamt in ms/106 zeichen für target und target+1",xlab="106 Zeichen = durchschnittliche Länge target 0+1")
boxplot(boxplotLZ,main="mean Lesezeiten gesamt in ms/106 zeichen für target und target+1",xlab="106 Zeichen = durchschnittliche Länge target 0+1")

print (boxread)

#mnspA<-mean(mnspeedSM)
#mnspB<-mean(mnspeedEM)
#mnspC<-mean(mnspeedLC)
#mnspD<-mean(mnspeedMM)

#LZ
#LSA<-1000/mnspA*meanadchar
#LSB<-1000/mnspB*meanadchar
#LSC<-1000/mnspC*meanadchar
#LSD<-1000/mnspD*meanadchar



#boxspeed <- cbind(SM = speedSM, EM = speedEM,LC = speedLC, MM = speedMM)
#boxplot(boxspeed,main="verarbeitungsgeschwindigkeit chars/sec bei target 0+1")
#barplot(boxspeed,main="verarbeitungsgeschwindigkeit chars/sec bei target 0+1")

##---END 1---


resixp1<-residuals(RT_1)
resixp2<-residuals(RT_2)
resixp3<-residuals(RT_3)
resixp4<-residuals(RT_4)
resixp5<-residuals(RT_5)
resixp6<-residuals(RT_6)
resixp7<-residuals(RT_7)


## reine lesezeiten, ohne effekt:
plot(resixp1)
plot(resixp2)
plot(resixp3)
plot(resixp4)
plot(resixp5)
plot(resixp6)
plot(resixp7)

lm1m1<-mean(resixp1)
lm1m2<-mean(resixp2)
lm1m3<-mean(resixp3)
lm1m4<-mean(resixp4)
lm1m5<-mean(resixp5)
lm1m6<-mean(resixp6)
lm1m7<-mean(resixp7)


matx <- cbind(smem = resixp1, smlc = resixp2,smmm = resixp3, emlc = resixp4,emmm=resixp5,lcmm=resixp6,SMvsO=resixp7)
lm1mx<-cbind(smem=lm1m1,smlc=lm1m2,smmm=lm1m3,emlc=lm1m4,emmm=lm1m5,lcmm=lm1m6,smvsO=lm1m7)
barplot(lm1mx,main = "means der LZ-response nach length-korrigiertem timeinterval, mit target 0+1")
boxplot(matx,main="residuals der modelle nach length-korrigiertem timeinterval, mit target 0+1")
barplot(matx,main="residuals der modelle nach length-korrigiertem timeinterval, mit target 0+1")


##rubio-fernandez:"In the model, we posited a main effect of Category (single vs. other) 
##and random effects of Participant and Item, along with a random slope of Category by Participant"
##(sumxy<- lmer(SPRxy ~ gruppen +(1|liste$item)+(1+gruppen|liste$participant),liste)) 
#(sumSMEM<- lmer(SPR_A ~ A_SMvs +(1|item)+(1+A_SMvs|participant),SMvsEM)) 
#(sumSMLC<- lmer(SPR_B ~ B_SMvs +(1|item)+(1+B_SMvs|participant),SMvsLC)) 
#(sumSMMM<- lmer(SPR_C ~ C_SMvs +(1|item)+(1+C_SMvs|participant),SMvsMM)) 
#(sumEMLC<- lmer(SPR_D ~ D_EMvs +(1|item)+(1+D_EMvs|participant),EMvsLC)) 
#(sumEMMM<- lmer(SPR_E ~ E_EMvs +(1|item)+(1+E_EMvs|participant),EMvsMM)) 
#(sumLCMM<- lmer(SPR_F ~ F_LCvs +(1|item)+(1+F_LCvs|participant),LCvsMM)) 

lme2.formula.1<-"group +(1|item)+(1+group|participant)"

(sumSMEM<- lmer(resixp1 ~ group +(1|item)+(1|participant)+(1+group|participant),SMvsEM)) 
(sumSMLC<- lmer(resixp2 ~ group +(1|item)+(1|participant)+(1+group|participant),SMvsLC)) 
(sumSMMM<- lmer(resixp3 ~ group +(1|item)+(1|participant)+(1+group|participant),SMvsMM)) 
(sumEMLC<- lmer(resixp4 ~ group +(1|item)+(1|participant)+(1+group|participant),EMvsLC)) 
(sumEMMM<- lmer(resixp5 ~ group +(1|item)+(1|participant)+(1+group|participant),EMvsMM)) 
(sumLCMM<- lmer(resixp6 ~ group +(1|item)+(1|participant)+(1+group|participant),LCvsMM)) 

(sumSMvsO<- lmer(resixp7 ~ category +(1|item)+(1|participant)+(1+category|participant),liste)) 
summary(sumSMvsO)

summary(sumSMEM)
summary(sumSMLC)
summary(sumSMMM)
summary(sumEMLC)
summary(sumEMMM)
summary(sumLCMM)


##bis hier

##
#SPR_X=RT1
#SPR_X=liste$timeinterval
#SPR_X=liste$addproct
#SPR_X=liste$rt_corr

s1<-"LZ SMvsEM target 0+1"
s2<-"SMvsLC"
s3<-"SMvsMM"
s4<-"EMvsLC"
s5<-"EMvsMM"
s6<-"LCvsMM"
s7<-"SMvsOther"

## nach korrektur von timeinterval mit:
## lmer(timeinterval ~ char + (1|participant)+(participant:char),dtatarget)
## rt_corr sind die residuals aus obigem modell
## und finden sich in den formel zum zweiten modell wieder:
## lmer(rt_corr ~ A_SMvs +(1|item)+(1+A_SMvs|participant),SMvsEM)) 
## lmer(liste$rt_corr ~ X_SMvsO +(1|item)+(1+X_SMvsO|participant),liste) 
## liste = set komplett nach filterung auf target 0,1, gilt=1, > 548 obs.
#output<-plot(sumSMvsO,type=c("p","smooth")) ## fitted vs residual
#png(file='sprexpo001.png')
#plot(sumSMvsO,type=c("p","smooth")) ## fitted vs residual
#dev.off()

plottype<-"h" ## p points,l lines,b both,c ,o,h histogram,s steps

plot(sumSMEM,type=c(plottype,"smooth"),main=s1)
plot(sumSMLC,type=c(plottype,"smooth"),main=s2)
plot(sumSMMM,type=c(plottype,"smooth"),main=s3)
plot(sumEMLC,type=c(plottype,"smooth"),main=s4)
plot(sumEMMM,type=c(plottype,"smooth"),main=s5)
plot(sumLCMM,type=c(plottype,"smooth"),main=s6)
plot(sumSMvsO,type=c(plottype,"smooth"),main=s7) ## fitted residuals

x1<-residuals(sumSMEM)
x2<-residuals(sumSMLC)
x3<-residuals(sumSMMM)
x4<-residuals(sumEMLC)
x5<-residuals(sumEMMM)
x6<-residuals(sumLCMM)
x7<-residuals(sumSMvsO)

mat <- cbind(smem = x1, smlc = x2,smmm = x3, emlc = x4,emmm=x5,lcmm=x6,SMvsO=x7)
#boxplot(mat, main = "residuals der modelle nach length-korrigiertem timeinterval, mit target 0+1", col = 1:7)
m1<-mean(x1)
m2<-mean(x2)
m3<-mean(x3)
m4<-mean(x4)
m5<-mean(x5)
m6<-mean(x6)
m7<-mean(x7)
mx<-cbind(smem=m1,smlc=m2,smmm=m3,emlc=m4,emmm=m5,lcmm=m6,smvsO=m7)
##plottype<-"p" ## p points,l lines,b both,c ,o,h histogram,s steps
barplot(mx,main = "means der modellresiduals nach length-korrigiertem timeinterval, mit target 0+1")
boxplot(mat,main="residuals der modelle nach length-korrigiertem timeinterval, mit target 0+1")
barplot(mat,main="residuals der modelle nach length-korrigiertem timeinterval, mit target 0+1")
boxplot(mx,main = "means der modellresiduals nach length-korrigiertem timeinterval, mit target 0+1")


md1<-median(x1)
md2<-median(x2)
md3<-median(x3)
md4<-median(x4)
md5<-median(x5)
md6<-median(x6)
md7<-median(x7)
mdx<-cbind(smem=md1,smlc=md2,smmm=md3,emlc=md4,emmm=md5,lcmm=md6,smvsO=md7)
##plottype<-"p" ## p points,l lines,b both,c ,o,h histogram,s steps
#barplot(mdx,main = "mediane der modellresiduals nach length-korrigiertem timeinterval, mit target 0+1")
#boxplot(mdx,main = "mediane der modellresiduals nach length-korrigiertem timeinterval, mit target 0+1")

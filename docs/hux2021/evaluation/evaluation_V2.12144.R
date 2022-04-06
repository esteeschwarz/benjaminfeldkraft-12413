#12144.
#20220405(08.13)
# HUX2021 evaluation, v.2.0, on base of script 12371b.R
###########################
# file keeping:
# daten original:
src_o<-("https://github.com/esteeschwarz/essais/raw/main/docs/hux2021/evaluation/sprdata.csv")
# daten prepared
src_d<-("https://github.com/esteeschwarz/essais/raw/main/docs/hux2021/evaluation/sprdataprepared.csv")
# evaluation script v.1.x
src_e<-("https://github.com/esteeschwarz/essais/raw/main/docs/hux2021/evaluation/1237b.R")
###########################
###########################

library(lme4)
library(lmerTest)
library(stringi)
library(readr)

#1
dta<-read.csv2(src_d)

###########################

##View(sprdataprepared)
##top datenframe aus datei, gueltige faelle, target 0+1
sprdatasm<-dta
##sprdatasm<-sprdata11342dtaxp ##fuer laden aus dateisystem, importiertes set hier einsetzen
#dtatargetgilt<-subset(sprdatasm, gilt==1)
#for complete dataset next
dtatargetgilt<-dta
#select filter for target:
#wenn mit adinterval gerechnet werden soll, musz target==0 ausgewaehlt werden.
# 
# dtatarget<-sprdatasm
# # 970 obs. target 0/1/-1
# dtacpt<-subset(dtatargetgilt, target==0|target==1|target==-1)
# dta0<-  subset(dtatargetgilt, target==0)
# dta1<-  subset(dtatargetgilt, target==-1)
# dta01<- subset(dtatargetgilt, target==0|target==1) 

###
#add control observation
adcontrol<-function(set){
  con1<-set[1,]
  con1$lfd<-length(dta$lfd)+1
  con1$participant<-"admin"
  con1$tnid<-999
  con1$gilt<-2
  con1$group<-"control"
  con1$group14<-"control"
  con1$category<-"control"
  con1$itemId<-"control"
  con1$item<-"control"
  con1$regionId<-"control"
  con1$elapsedTime<-NA
  con1$timeinterval<-300
  con1$target<-0
  con1$string<-"dies ist ein control string durchschnittlicher länge"
  con1$char<-stri_count_boundaries(con1$string,"character")
  con1$rt_corr<-NA
  con1$speed<-NA
  con1$adinterval<-NA
  con1$wds<-stri_count_boundaries(con1$string)
  con1$addwds<-NA
  con1$proctbywd<-NA
  con1$addproct<-NA
  con1$proctbywdavg<-NA
  con1$addproctbywds<-NA
  con1$proctbychar<-NA
  con1$addchar<-NA
  con1$addproctbychar<-NA
  con1$explique<-"control string zur festlegung der minimal RT"
  return(rbind(set,con1))
}


# to discard negative outliers
#set minimum response to 319ms
#2.
outbottomfix<-319
dtaout<-subset(dta,dta$timeinterval>outbottomfix)
#dtaout<-dta_out1
#dta<-dta_out1

##########################
#3.
#add column with length corrected response times
getchars<-function(set){
charscpt<-stri_count_boundaries(set$string,type="character")
dtares<-residuals(lm(timeinterval~charscpt,set))
head(dtares)
dtap1<-cbind(set,"rtc"=dtares)
}
dtap4<-getchars(dtac)
tail (dtap4$rtc)+dtap4$rtc[length(dtap4$rtc)]*-1
dtap5<-dtap4$rtc+dtap4$rtc[length(dtap4$rtc)]*-1+300
dtap4$rtc[length(dtap4$rtc)]
tail(dtap5)
dtap4$rtc<-dtap5
tail(dtap4)
mean(dtap4$rtc)
min(dtap4$rtc)
min(dtap5)
##########
#4. 
#targetlisten ohne outliers
#set<-dta0

outl.fun<-function(set){
  attach(set)
  outl.form<-set
  sprmean<-mean(timeinterval)
  
  stdev<-sd(timeinterval)
  sdout<-stdev*2.5
  outtop<-sprmean+sdout
 # outbottom<-sprmean-sdout ## negative
#  outbottommod<-319
  #discard outliers according to subset
  liste<-subset(set,timeinterval<outtop)
}
###
#5.
#discard outliers on base of length corrected response time
outl.fun.rtc<-function(set){
  attach(set)
  outl.form<-set
  sprmean<-mean(rtc)
  
  stdev<-sd(rtc)
  sdout<-stdev*2.5
  outtop<-sprmean+sdout
  outbottom<-sprmean-sdout ## negative
  #  outbottommod<-319
  #discard outliers according to subset
  liste<-subset(set,rtc<outtop&rtc>outbottom)
}
#######
#5.1
#set without outliers with resp to target length
dtap2<-outl.fun.rtc(dtap1)
#5.2 manuell ausgeschlossene fälle im scheme
dtap3<-subset(dtap2,gilt==1)
#dtap2<-dtap3
d1ns<-colnames(dtap2)
d1ns[6]<-"grSMvs"
d1ns
colnames(dtap2)<-d1ns
colnames(dtap3)<-d1ns

########################
#berechne outliers zeichenabhängig####
# outl.fun.ch<-function(set){
# chars_cpt<-stri_count_boundaries(set$string,type="character")
# 
# liste_x<-chars_x(dtax_x)
# mean(liste_x)
# mnchar_x<-mean(liste_x)
# listeCH<-dtax_x$timeinterval/chars_x(dtax_x)
# outAmean<-mean(listeCH)
# outAsd<-sd(listeCH)
# outAsd<-outAsd*2.5
# outAtop<-outAmean+outAsd
# ##### discard outliers with respect to target length
# outAliste<-subset(set,set$timeinterval/chars_x(dtax_x)<outAtop&dtax_x$timeinterval)
# 
# }

#6.
#target specific extraction

dtatg<-function(set,t1,t2,t3){
  return(subset(set, target==t1|target==t2|target==t3))
}
#########################
#6.1
#target specific subsets
l01<-dtatg(dtap2,0,1,0)
l0<-dtatg(dtap2,0,0,0)
l101<-dtatg(dtap2,-1,0,1)

l01<-dtatg(dtap4,0,1,0)
l0<-dtatg(dtap4,0,0,0)
l101<-dtatg(dtap4,-1,0,1)

#########################
mean(dtap1$rtc,na.rm = T)
head(dtap1$rtc)
#liste ohne outliers, zeichenabhängig discarded
#flag<-c(0,0,0)
#dta_ch0<- outl.fun.ch(dtatg(dta_out1,0,0,0))
#dta_ch01<-outl.fun.ch(dtatg(dta_out1,0,1,0))
#dta_ch101<-outl.fun.ch(dtatg(dta_out1,-1,0,1))

#listen ohne outliers, zeichenunabhängig discarded
#6.2
dta0<-  outl.fun(dtatg(dta_out1, 0,0,0))
dta01<- outl.fun(dtatg(dta_out1, 0,1,0))
dta101<-outl.fun(dtatg(dta_out1,-1,0,1))
#wks.
dta_ch0<-l0
dta_ch01<-l01
dta_ch101<-l101

mnresp1<-rbind(mean(dta_ch0$timeinterval),mean(dta_ch01$timeinterval),mean(dta_ch101$timeinterval))
mnresp2<-rbind(mean(dta0$timeinterval),mean(dta01$timeinterval),mean(dta101$timeinterval))
mnresp3<-cbind(mnresp1,mnresp2)
colnames(mnresp3)<-c("dep chars","indep chars")
barplot((mnresp3))
#difference sig with/without resp to target length:
chisq.test(mnresp3,correct = F)

#########################
##sm-em-lc-mm kategorien in spalte $group
sm<-"SM"
em<-"EM"
lc<-"LC"
mm<-"MM"
# 
remove(dtax)

dtatg<-function(set,t1,t2,t3){
  return(subset(set, target==t1|target==t2|target==t3))
}
dta_tgx<-function(set,t1,t2,t3){
  return(subset(set, target==t1|target==t2|target==t3))
}


#7.
#subsets according to group
dta_grx<-function(set,g1,g2){
subset(set,group==g1|group==g2)
}

# dtaset<-function(set,t1,t2,t3,sm,g1,g2){
#   ifelse(sm==1,return(dtatg(dta_out1,t1,t2,t3)),
#   return(dta_grx(dtatg(dta_out1,t1,t2,t3),g1,g2)))
# 
# }
remove(dtatg)
############### THIS
#8.
dtaset2<-function(set,t1,t2,t3,sm,g1,g2){
  dtatg<-function(set,t1,t2,t3){
    return(subset(set, target==t1|target==t2|target==t3))
  }

    dta_grx<-function(set,g1,g2){
    subset(set,group==g1|group==g2)
  }
    ifelse(sm==1,return(dtatg(set,t1,t2,t3)),
        return(dta_grx(dtatg(set,t1,t2,t3),g1,g2)))
        # return(dta_grx(dtatg(dta_out1,t1,t2,t3),g1,g2)))
  
#wks. creates subsets for lmer test  
}
##############################################################
dta_setx<-function(set,t1,t2,t3,sm,g1,g2){
  dtatg<-function(set,t1,t2,t3){
    return(subset(set, target==t1|target==t2|target==t3))
  }
  
  dta_grx<-function(set,g1,g2){
    subset(set,group==g1|group==g2)
  }
  ifelse(sm==1,return(dtatg(set,t1,t2,t3)),
         return(dta_grx(dtatg(set,t1,t2,t3),g1,g2)))
  # return(dta_grx(dtatg(dta_out1,t1,t2,t3),g1,g2)))
  
  #wks. creates subsets for lmer test  
}
##############################################################
#rubio-fernandez:
#"We constructed 3 lists of materials, each containing 7 items of each experimental 
#condition (Extended Metaphor, Single Metaphor and Literal)"
#"In the model, we posited a main effect of Category (single vs. other) 
#and random effects of Participant and Item, along with a random slope of Category by Participant"
#"pairwise comparisons of Condition levels"

#choose between "item"(R/F) above interpreted as itemset (8) of four conditions ($item) or 
#item as condition (4x8) of item ($itemId)
#lme2.form2<- paste0("group +(1|itemId)+(1|participant)+(1+group|participant)")
d1ns<-colnames(dta)
d1ns[6]<-"vsGroup"
d1ns
colnames(dtap2)<-d1ns
colnames(dtap4)<-d1ns

#lme2.form1<- paste0("group +(1|itemId)+(1|participant)+(1+group|participant)")
#lme2.form2<- paste0((colnames(dtap4)[6]) +"(1|item)+(1|participant)+(1+grSMvs|participant)")
lme2.form2.rnd<-paste0("(1|item)+(1|participant)")
lme2.form2.cat<-paste0(colnames(dtap4)[6])
#colnames(dtap4)[6]
lme2.form.cpt<- paste(lme2.form2.cat,"+",lme2.form2.rnd,"+(1+",lme2.form2.cat,"|participant)")
(fmla1 <- as.formula(paste("rtc ~ ", lme2.form.cpt)))
(fmla2 <- as.formula(paste("timeinterval ~ ", lme2.form.cpt)))

####################################################
lmerun<-function(form,set,t1,t2,t3,sm,g1,g2){
lmeset<-dta_setx(set,t1,t2,t3,sm,g1,g2)
  (sumSMEM<- lmer(form,lmeset)) 

}
####################################################
summary(lmerun(fmla1,dtap4,0,0,0,F,sm,em))  
summary(lmerun(fmla1,dtap4,0,0,1,F,sm,em))  

sum1<-(lmerun(fmla1,dtap4,0,0,0,F,sm,em))  
sum2<-(lmerun(fmla1,dtap4,0,0,0,F,sm,em))  
sumcpt<-
set1<-(as.list(fmla1,dtap4,c(0,0,0),F,sm,em))
set1<-as.list(1)
set1[["lme"]]<-fmla1
set1[["data"]]<-dtap4
set1[["target"]][1]<-0
set1[["target"]][2]<-0
set1[["target"]][3]<-0
set1[["SMvsO"]]<-F
set1[["group"]][1]<-sm
set1[["group"]][2]<-em

sum2<-summary(lmerun(fmla1,dtap4,0,-1,1,F,sm,mm))
as.data.frame(sum1)
dif<-sum2$coefficients[1]-sum2$coefficients[2]
dif

#head(dtaset2(dtap3,0,0,0,F,sm,em))
#
s1<-paste0("SMvsEM",boxlabtgt)
s2<-paste0("SMvsLC",boxlabtgt)
s3<-paste0("SMvsMM",boxlabtgt)
s4<-paste0("EMvsLC",boxlabtgt)
s5<-paste0("EMvsMM",boxlabtgt)
s6<-paste0("LCvsMM",boxlabtgt)
s7<-paste0("SMvsOther",boxlabtgt)

#png(file='sprexpo001.png')
#plot(sumSMvsO,type=c("p","smooth")) ## fitted vs residual
#dev.off()

plottype<-"h" ## p points,l lines,b both,c ,o,h histogram,s steps

plot(sum2,type=c(plottype,"smooth"),main=s1)
plot(sum1,type=c(plottype,"smooth"),main=s1)

plot(sumSMLC,type=c(plottype,"smooth"),main=s2)
plot(sumSMMM,type=c(plottype,"smooth"),main=s3)
plot(sumEMLC,type=c(plottype,"smooth"),main=s4)
plot(sumEMMM,type=c(plottype,"smooth"),main=s5)
plot(sumLCMM,type=c(plottype,"smooth"),main=s6)
plot(sumSMvsO,type=c(plottype,"smooth"),main=s7) ## fitted residuals
#---------------------------------------------------------------------


#---C---  compare R/F results:-----------------------
#For these raw data, the mean reading time for the critical segments in the 
#Literal condition was       1457 ms (SD 727 ms), in the 
#Extended Metaphor condition 1543 ms (SD 764 ms), and in the 
#Single Metaphor condition   1578 ms (SD 768 ms).
#LC < EM < SM
#C.1
#here results reading time RAW, target 0
#SM: 1623, sd: 834
#EM: 1761, sd: 1648
#LC: 1835, sd: 1304
#MM: 1777, sd: 958
#SM < EM < MM < LC
#(without outliers): the mean response times for the critical segments were 
#1421 ms (SD 650 ms) in the Literal condition, 
#1498 ms (SD 716 ms) in the Extended Metaphor condition, 
#1537 ms (SD 716 ms) in the Single Metaphor condition.
#LC < EM < SM
#C.2
#without outliers (2,5sd),
#      SM       EM       LC      ISM
#1874.245 1689.975 1838.101 1896.411
#rank:  3         1       2        4
#std dev
#      SM       EM       LC      ISM
#790.7908 804.7236 817.7403 858.2711


#C.3
#with respect to target length  > LSAc - LSDc
#SM: 1874, sd: 790
#EM: 1689, sd: 804
#LC: 1838, sd: 817
#MM: 1896, sd: 858
#EM < LC < SM < MM

#mean LZ bei target 0+1, without outliers, mean target0+1=54 zeichen
#      SM       EM       LC      ISM
# 2111.147 2205.45 2254.458 2319.808
#rank:   1       2       3         4
#std dev
#      SM       EM       LC      ISM
#965.6856 1052.556 1025.115 1081.137


#----
#lme4:
#(R/F): with the Single condition being read slower than the others (coefficient = 77.3, SE = 24.9, t = 3.10, p < 0.01)
#here: coef=13.89, SE=105.86, t=0.131, n.s. ($itemID)
#no significant difference between Extended and Single metaphors (coefficient = 38.4, SE = 29.7, t = 1.30, n.s.)
#here: coef=61.8, SE=96.31, t=0.642, n.s.
#highly significant differences between each of these and the Literal condition 
#Literal vs. Extended: coefficient = 75.3, SE = 28.2, t = 2.68, p < 0.01; 
#here: coef=33.96, SE=98.24,t=0.346
#Literal vs. Single:  coefficient = 114.7, SE = 28.6, t = 4.00, p < 0.001).
#here: coef=11.159,SE=96.514,t=0.116
#SPEED:
#SM < Others
#
#hier resultate zeichenunabhängig, target 0+1:
#SM vs Others:
#coef = 77.94, SE = 241.47, t=-0.323, p < 0.001

#------------------------------------------------
##bis hier



##---D----------descriptive analysis


labx<-paste0("Basis durchschnittliche targetlaenge=", round(meanch), " zeichen")
toplab<-paste0("Lesezeiten (ms)",boxlabtgt)

boxLZ<-cbind(SM=LZSMc,EM=LZEMc,LC=LZLCc,ISM=LZMMc)
#boxplot(boxLZ,main="mean Lesezeiten gesamt in ms/106 zeichen fuer target+1",xlab="106 Zeichen = durchschnittliche Laenge target 0+1")
boxplot(boxLZ,main=toplab, xlab=labx,notch=TRUE,col=c(1,2,3,4))

toplab<-paste0("median Lesezeiten (ms)",boxlabtgt)
boxLZmd<-cbind(SM=mdLZA,EM=mdLZB,LC=mdLZC,ISM=mdLZD)
boxplot(boxLZmd,main=toplab,xlab=labx)

toplab<-paste0("mean Lesezeiten (ms)",boxlabtgt)
lab1<-paste0("basis durchschnittliche targetlaenge=", round(meanch), " zeichen")
boxLZmn<-cbind(SM=LSAc,EM=LSBc,LC=LSCc,ISM=LSDc)
boxplot(boxLZmn,main=toplab,xlab=lab1)


#boxLZad<-cbind(SM=LZSMd,EM=LZEMd,LC=LZLCd,MM=LZMMd)
#boxplot(boxLZad,main="Lesezeiten gesamt in ms/106 zeichen fuer target+1",xlab="106 Zeichen = durchschnittliche Laenge target 0+1",notch=TRUE,col=c(1,2,3,4))

#plot median addierte lesezeiten
#boxLZmd<-cbind(SM=mdspAd,EM=mdspBd,LC=mdspCd,ISM=mdspDd)
#boxplot(boxLZmd,main="median Lesezeiten gesamt in ms/106 zeichen fuer target 0+1",xlab="106 Zeichen = durchschnittliche Laenge target 0+1")

#boxspeed <- cbind(SM = speedSM, EM = speedEM,LC = speedLC, MM = speedMM)
#boxplot(boxspeed,main="lesegeschwindigkeit chars/sec bei target 0+1")

##---END 2---

#12373.
#coefficients(sumSMEM)

#12393.

#---D---- added LZ -----------
#R/F der reihe nach > mean response
subdescr<-subset(subdescr,subdescr$target==0)
#subsets kategorienvergleich
SSM<-subset(subdescr,group=="SM")
SEM<-subset(subdescr,group=="EM")
SLC<-subset(subdescr,group=="LC")
SMM<-subset(subdescr,group=="MM")
proofset1a<-length(SSM$lfd)+length(SEM$lfd)+length(SLC$lfd)+length(SMM$lfd)




meanRTrawAd<-mean(SSM$adinterval)
meanRTrawBd<-mean(SEM$adinterval)
meanRTrawCd<-mean(SLC$adinterval)
meanRTrawDd<-mean(SMM$adinterval)
#standardabweichung response
sdRTrawAd<-sd(SSM$adinterval)
sdRTrawBd<-sd(SEM$adinterval)
sdRTrawCd<-sd(SLC$adinterval)
sdRTrawDd<-sd(SMM$adinterval)

meanRTAd<-mean(SSMod$adinterval)
meanRTBd<-mean(SEMod$adinterval)
meanRTCd<-mean(SLCod$adinterval)
meanRTDd<-mean(SMMod$adinterval)
#standardabweichung response
sdRTAd<-sd(SSMod$adinterval)
sdRTBd<-sd(SEMod$adinterval)
sdRTCd<-sd(SLCod$adinterval)
sdRTDd<-sd(SMMod$adinterval)

charsAd<-SSM$addchar
charsBd<-SEM$addchar
charsCd<-SLC$addchar
charsDd<-SMM$addchar

LZSMd<-(SSM$adinterval/charsAd)
LZEMd<-(SEM$adinterval/charsBd)
LZLCd<-(SLC$adinterval/charsCd)
LZMMd<-(SMM$adinterval/charsDd)

#with discard outliers
LZSMd<-(LZSMd[LZSMd<LZouttopd&LZSMd>LZbottommodd])
LZEMd<-(LZEMd[LZEMd<LZouttopd&LZEMd>LZbottommodd])
LZLCd<-(LZLCd[LZLCd<LZouttopd&LZLCd>LZbottommodd])
LZMMd<-(LZMMd[LZMMd<LZouttopd&LZMMd>LZbottommodd])

#check number obs. after discard outliers
proofset2d<-length(LZSMd)+length(LZEMd)+length(LZLCd)+length(LZMMd)

#lesezeit / durchschnittliche targetphrase (zeichenabhängig)
LZSMcd<-LZSMd*meanchd
LZEMcd<-LZEMd*meanchd
LZLCcd<-LZLCd*meanchd
LZMMcd<-LZMMd*meanchd
#mean LZ
LSAcd<-mean(LZSMcd)
LSBcd<-mean(LZEMcd)
LSCcd<-mean(LZLCcd)
LSDcd<-mean(LZMMcd)

#sd LZ
LSAsdd<-sd(LZSMcd)
LSBsdd<-sd(LZEMcd)
LSCsdd<-sd(LZLCcd)
LSDsdd<-sd(LZMMcd)
boxLZsdd<-cbind(SM=LSAsdd,EM=LSBsdd,LC=LSCsdd,ISM=LSDsdd)


#summe LZ abhängig von zeichenanzahl und anzahl observationen

obsAd<-length(SSM$adinterval)
obsBd<-length(SEM$adinterval)
obsCd<-length(SLC$adinterval)
obsDd<-length(SMM$adinterval)



sum1d<-sum(LZSMcd)/obsAd
sum2d<-sum(LZEMcd)/obsBd
sum3d<-sum(LZLCcd)/obsCd
sum4d<-sum(LZMMcd)/obsDd

#lab1<-paste0("basis durchschnittliche targetlaenge=", round(meanch), " zeichen")
#boxLZmn<-cbind(SM=LSAc,EM=LSBc,LC=LSCc,ISM=LSDc)
#boxplot(boxLZmn,main="mean Lesezeit (ms)",xlab=lab1)

#sum1<-sum(LZSM)/obsA
#sum2<-sum(LZEM)/obsB
#sum3<-sum(LZLC)/obsC
#sum4<-sum(LZMM)/obsD


#differenz zwischen kategorien
difsmemd<-sum1d-sum2d
difsmlcd<-sum1d-sum3d
difsmmmd<-sum1d-sum4d
difemlcd<-sum2d-sum3d
difemmmd<-sum2d-sum4d
diflcmmd<-sum3d-sum4d
dfksmemd<-sum1d/sum2d


mdLZAd<-median(LZSMcd)
mdLZBd<-median(LZEMcd)
mdLZCd<-median(LZLCcd)
mdLZDd<-median(LZMMcd)

#differenzen median kategorien
difsmemd<-mdLZAd-mdLZBd
difsmlcd<-mdLZAd-mdLZCd
difsmmmd<-mdLZAd-mdLZDd
difemlcd<-mdLZBd-mdLZCd
difemmmd<-mdLZBd-mdLZDd
diflcmmd<-mdLZCd-mdLZDd
dfksmemd<-sum1d/sum2d

boxlabtgt<-" addiert target 0+1"
labx<-paste0("bewertungsgrundlage durchschnittlich ", round(meanchd), " zeichen")
toplab<-paste0("Lesezeiten (ms)",boxlabtgt)

boxLZd<-cbind(SM=LZSMcd,EM=LZEMcd,LC=LZLCcd,ISM=LZMMcd)
#boxplot(boxLZ,main="mean Lesezeiten gesamt in ms/106 zeichen fuer target+1",xlab="106 Zeichen = durchschnittliche Laenge target 0+1")
boxplot(boxLZd,main=toplab, xlab=labx,notch=TRUE,col=c(1,2,3,4))

toplab<-paste0("median Lesezeiten (ms)",boxlabtgt)
boxLZmdd<-cbind(SM=mdLZAd,EM=mdLZBd,LC=mdLZCd,ISM=mdLZDd)
boxplot(boxLZmdd,main=toplab,xlab=labx)

toplab<-paste0("mean Lesezeiten (ms)",boxlabtgt)
lab1<-paste0("bewertungsgrundlage = durchschnittlich ", round(meanchd), " zeichen")
boxLZmnd<-cbind(SM=LSAcd,EM=LSBcd,LC=LSCcd,ISM=LSDcd)
boxplot(boxLZmnd,main=toplab,xlab=lab1)
################

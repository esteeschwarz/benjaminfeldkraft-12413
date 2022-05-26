#proof notes
#
#
############
#1.
#regex finds
basic<-grep("basic[^s]",books$TITEL,ignore.case = T) #REGEX !!!!!!!!!!!
#finds all basic except basics

#paste
lme2.form.cpt<-    paste(lme2.form2.cat,"+"  ,lme2.form2.rnd,"+(1+",lme2.form2.cat,  "|participant)")
lme2.form.cpt.XvsO<- paste(lme2.form2.XvsO,"+",lme2.form2.rnd,"+(1+",lme2.form2.XvsO,"|participant)")
rtc<-"rtc ~ "
rtcc<-"rtc.1 ~ "
ti<-"timeinterval ~ "
ifelse(resp=="rtc",rt<-rtc,ifelse(resp=="rtcc",rt<-rtcc,rt<-ti))
(fmlRTCgr <- as.formula(paste("rtc ~ ", lme2.form.cpt)))
(fmlTIgr <- as.formula(paste("timeinterval ~ ", lme2.form.cpt)))
(fmlTIvs<-  as.formula(paste("timeinterval ~ ", lme2.form.cpt.XvsO)))
(fmlRTCvs <- as.formula(paste("rtc ~ ", lme2.form.cpt.XvsO)))

(fmlxgr <- as.formula(paste(rt, lme2.form.cpt)))
(fmlxvs <- as.formula(paste(rt, lme2.form.cpt.XvsO)))

stri_flatten(  as.character(setx[1,]),collapse=",")

##### subs, exclude, include
mean(dta$timeinterval[group==sm],na.rm=T)
### this:
length(dta_setx(dta,c(0,0,0,1,sm,sm),1)[with(dta_setx(dta,c(0,0,0,1,sm,sm),1),category==lc),]$timeinterval)
# 0
c1<-(dta_setx(dta,c(0,0,0,1,em,vso),1)[with(dta_setx(dta,c(0,0,0,1,em,vso),1),category=="EMvsAll"),]$category)
s
flag<-em
flag<-1
flagno<-0
flagset<-dta_setx(dta,c(0,0,0,1,em,vso),1)
set<-dta_setx(dta,c(0,0,0,1,em,vso),1)

checkvsall<-function(set){
  stri_detect(set$category,regex="vsAll")
}
c1<-flagset[with(flagset,category==flag|flagno==1&checkvsall(flagset)),]$category
c1<-flagset[with(flagset,category==flag&flagno==0|category!=flag&flagno==1|checkvsall(flagset)),]$category
c1<-flagset[with(flagset,category==flag&flagno==0|category!=flag&flagno==1&checkvsall(flagset)),]$category

c1
##########
c1<-flagset[with(flagset,category==flag),]
length(c1$category)
mean(c1$timeinterval,na.rm=T)
#c2<-flagset[with(flagset,checkvsall(flagset)),]
ifelse(flagall==0,c2<-flagset[with(flagset,group==chose[6]),],
       c2<-flagset[with(flagset,checkvsall(flagset)),])
mn2<-mean(c2$timeinterval)
length(c2$category)
mean(c2$timeinterval,na.rm=T)
mn1<-mean(c1$timeinterval,na.rm=T)
mnx<-cbind(mn1,mn2)
rownames(mnx)<-"mean"
ifelse(flagall==1,col2<-"vsAll",col2<-chose[6])
colnames(mnx)<-c(chose[5],col2)
print(mnx)
############## sequences, steps, schritte, reihe, abfolge, sequenzierte reihe
a1<-seq(1,10,0.5);a1


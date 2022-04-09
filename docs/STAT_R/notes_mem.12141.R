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


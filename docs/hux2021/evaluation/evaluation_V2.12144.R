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
#src_e<-("https://github.com/esteeschwarz/essais/raw/main/docs/hux2021/evaluation/1237b.R")
###########################
###########################

library(lme4)
library(lmerTest)
library(stringi)
library(readr)
library(clipr)
#1
dta<-read.csv2(src_d)
d1ns<-colnames(dta)
d1ns[6]<-"XvsGr"
d1ns
colnames(dta)<-d1ns
#colnames(dta)<-d1ns

###########################


###
#set<-dta
#add control observation
adcontrol<-function(set){
  print(length(set))
  #setns<-colnames(set)
  #print(setns)
  #setns[6]<-"vsGroup"
  #colnames(set)<-setns
  con1<-set[1,]
  con1
  con1$lfd<-length(dta$lfd)+1
  con1$participant<-"admin"
  con1$tnid<-999
  con1$gilt<-2
  con1$group<-"control"
  con1[,6]<-"0Control"
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
  
  print(length(con1))
  print(colnames(con1))
  print(length(set))
  print(colnames(set))
  
  return(rbind(set,con1))
}
dta<-adcontrol(dta)


##########################
#3.
#add column with length corrected response times
get_rtc<-function(set){
charscpt<-stri_count_boundaries(set$string,type="character")
dtares<-residuals(lm(timeinterval~charscpt,set))
#head(dtares)
#dtap1<-cbind(set,"rtc"=dtares)
set<-cbind(set,"rtc"=dtares)
return(set)
}

##########
#4. 
#targetlisten ohne outliers
#set<-dta0

outl.fun<-function(set,outbottom){
  attach(set)
  outl.form<-set
  sprmean<-mean(timeinterval)
  
  stdev<-sd(timeinterval)
  sdout<-stdev*2.5
  outtop<-sprmean+sdout
  # outbottom<-sprmean-sdout ## negative
  #  outbottommod<-319
  #discard outliers according to subset
  liste<-subset(set,timeinterval<outtop&timeinterval>outbottom)
}
###
#5.
#discard outliers on base of length corrected response time
###################################
outl.fun.rtc<-function(set){
 # attach(set)
dtartc<-get_rtc(set)
  #  outl.form<-set
rtc<-dtartc$rtc
  sprmean<-mean(rtc)
  
  stdev<-sd(rtc)
  sdout<-stdev*2.5
  outtop<-sprmean+sdout
  outbottom<-sprmean-sdout ## negative
  #  outbottommod<-319
  #discard outliers according to subset
  liste<-subset(dtartc,rtc<outtop&rtc>outbottom)

  }
#dtax<-outl.fun.rtc(dta)
#outl.fun()
#######
#5.1
#set without outliers with resp to target length
dtap2<-outl.fun.rtc(dtap1)
##########?????????
#5.2 manuell ausgeschlossene fälle im scheme
dtap3<-subset(dtap2,gilt==1)
dtap2<-dtap3
##########????????

#6.

#########################
##sm-em-lc-mm kategorien in spalte $group
sm<-"SM"
em<-"EM"
lc<-"LC"
mm<-"MM"
vso<-"All"
# 
#remove(dtax)


#7.
############### THIS
#8.
# #wks. creates subsets for lmer test  
# }
##############################################################

dta_setx<-function(set,chose,out){
 
  t1<-chose[1]
  t2<-chose[2]
  t3<-chose[3]
  xo<-chose[4]
  g1<-chose[5]
  g2<-chose[6]
   
  setvsx<-  function(set,gr,other){
     dta<-set
   #  attach(dta)
     sublc<-subset(dta,group==gr)
     subnlc<-subset(dta,group!=gr)
     subna<-subset(dta,is.na(group))
     sublc$category<-gr
     subns<-stri_join(gr,"vs",other)
     subnlc$category<-subns
     subna$category<-subns
     lcvsO<-rbind(sublc,subnlc,subna)
     length(lcvsO$category==gr)-length(lcvsO$category==subns)
     set<-get_rtc(lcvsO)
     return(set)
   }

   dtatg<-function(set,t1,t2,t3,g1,g2){
     setxvso<-setvsx(set,g1,g2)
     return(subset(setxvso, target==t1|target==t2|target==t3))

     }

   dta_grx<-function(set,g1,g2){
     subset(set,group==g1|group==g2)
   }
   ifelse(out==1,set<-outl.fun.rtc(set),set<-set)
   
      ifelse(xo==1,return(dtatg(set,t1,t2,t3,g1,g2)),
          return(dta_grx(dtatg(set,t1,t2,t3,g1,g2),g1,g2)))

     #wks. creates subsets for lmer test
}
dtax<-dta_setx(dta,c(0,0,0,0,sm,sm),1)

########12147.


dtax<-dta_setx(dta,-1,0,1,0,sm,em)
dtax<-dta_setx(dta,-1,0,1,0,sm,em)
##############
createsets<-function(){
  ch1<-c(0,0,0,0,sm,em)
  ch2<-c(0,0,0,0,sm,lc)
  ch3<-c(0,0,0,0,sm,mm)
  ch4<-c(0,0,0,0,em,lc)
  ch5<-c(0,0,0,0,em,mm)
  ch6<-c(0,0,0,0,lc,mm)
  
  ch11<-c(0,0,1,0,sm,em)
  ch21<-c(0,0,1,0,sm,lc)
  ch31<-c(0,0,1,0,sm,mm)
  ch41<-c(0,0,1,0,em,lc)
  ch51<-c(0,0,1,0,em,mm)
  ch61<-c(0,0,1,0,lc,mm)
  
  ch111<-c(-1,0,1,0,sm,em)
  ch211<-c(-1,0,1,0,sm,lc)
  ch311<-c(-1,0,1,0,sm,mm)
  ch411<-c(-1,0,1,0,em,lc)
  ch511<-c(-1,0,1,0,em,mm)
  ch611<-c(-1,0,1,0,lc,mm)
  
  ch7a<-c(0,0,0,1,sm,vso)
  ch7b<-c(0,0,0,1,em,vso)
  ch7c<-c(0,0,0,1,lc,vso)
  ch7d<-c(0,0,0,1,mm,vso)
  
  ch71a<-c(0,1,0,1,sm,vso)
  ch71b<-c(0,1,0,1,em,vso)
  ch71c<-c(0,1,0,1,lc,vso)
  ch71d<-c(0,1,0,1,mm,vso)
  
  ch711a<-c(-1,1,0,1,sm,vso)
  ch711b<-c(-1,1,0,1,em,vso)
  ch711c<-c(-1,1,0,1,lc,vso)
  ch711d<-c(-1,1,0,1,mm,vso)
  
  chosx<-rbind(ch1,ch2,ch3,ch4,ch5,ch6,ch7a,ch7b,ch7c,ch7d,
               ch11,ch21,ch31,ch41,ch51,ch61,ch71a,ch71b,ch71c,ch71d,
               ch111,ch211,ch311,ch411,ch511,ch611,ch711a,ch711b,ch711c,ch711d)
  chosx[7:12]
  chosx.ns<-c("target -1","target 0","target +1","SMvsOther","group 1","group 2")
  colnames(chosx)<-chosx.ns
  return(chosx)
}
##########################
setx<-createsets()
setx[11,]
#wks.
#remove(setx)
#ch1
##########################
#####################################################
lmerun(lmef[[1]],dta,setx[1,],1)

getmean<-function(set,chose,out){
 # t1<-chose[1]
 # t2<-chose[2]
 # t3<-chose[3]
 # sxo<-chose[4]
 # g1<-chose[5]
 # g2<-chose[6]
  chose[4]<-0
  mnset<-dta_setx(set,chose[1],chose[2],chose[3],chose[4],chose[5],chose[6])
  
   dta<-mnset
 #attach(dta)
# chose<-c(t1,t2,t3,xo,g1,g2)
 #chose[1]<-t1
 #chose[2]<-
mnx<-mean(mnset$timeinterval,na.rm=T)
 # SM<-mean(dta_setx(dta,c(t1,t2,t3,F,sm,sm))$timeinterval,na.rm=T)
 # EM<-mean(dta_setx(dta,c(t1,t2,t3,F,em,em))$timeinterval)
 # LC<-mean(dta_setx(dta,c(t1,t2,t3,F,lc,lc))$timeinterval)
 # MM<-mean(dta_setx(dta,c(t1,t2,t3,F,mm,mm))$timeinterval)
 # means<-rbind(SM,EM,LC,MM)
 # SM<-sd(dta_setx  (dta,c(t1,t2,t3,F,sm,sm))$timeinterval)
 # EM<-sd(dta_setx  (dta,c(t1,t2,t3,F,em,em))$timeinterval)
 # LC<-sd(dta_setx  (dta,c(t1,t2,t3,F,lc,lc))$timeinterval)
 # MM<-sd(dta_setx  (dta,c(t1,t2,t3,F,mm,mm))$timeinterval)
 # sds<-rbind(SM,EM,LC,MM)
 # tb1<-cbind(means,sds)
 # colnames(tb1)<-c("mean","sd")
 # tb1<-as.data.frame(tb1)
 # tb2<-tb1   [with(tb1,order(mean)),]
 print(mnx)
 #return(tb2)
}
dtax<-dta_setx(dta,0,0,0,0,sm,sm)
setx[1,]
getmean(dta,c(1,0,-1,0,em,em),1)
mean(dta_setx(dta,0,0,0,0,sm,sm)$timeinterval)
lmerun(lmef[[1]],dta,setx[1,],0)
lmerun(lmef[[1]],dta,c(0,0,0,0,sm,em),0)

# SM<-mean(dta_setx(dta,0,0,0,F,sm,sm)$timeinterval)
# remove(SM)
# t1<-0
# t2<-0
# t3<-0

#####################################################
smvso<-setvsx(dta2,sm)

lmerun(fmlRTCvs,dta,setx[7,]) #warum unterschiedliche variablenlängen? rtc
##############################################################
#rubio-fernandez:
#"We constructed 3 lists of materials, each containing 7 items of each experimental 
#condition (Extended Metaphor, Single Metaphor and Literal)"
#"In the model, we posited a main effect of Category (single vs. other) 
#and random effects of Participant and Item, along with a random slope of Category by Participant"
#"pairwise comparisons of Condition levels"

create_lmeforms<-function(set,resp){
lme2.form2.rnd<-paste0("(1|item)+(1|participant)")
########## TD
lme2.form2.cat<-paste0(colnames(set)[6])
########## 
lme2.form2.XvsO<-paste0("category")
#colnames(dtap4)[6]
#colnames(lmedataset)[6]
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
lmeforms2<-list("gr"=fmlxgr,"vs"=fmlxvs)
lmeforms<-list("RTCgr"=fmlRTCgr,"TIgr"=fmlTIgr,"RTCvs"=fmlRTCvs,"TIvs"=fmlTIvs)
#lmeforms[1]
return(lmeforms2)
}
lmef2<-create_lmeforms(dta,"ti")
lmef2
#fmla3
#set1[1]
####################################################
# lmerun<-function(form,set,t1,t2,t3,sm,g1,g2){
# lmeset<-dta_setx(set,t1,t2,t3,sm,g1,g2)
#   (sumSMEM<- lmer(form,lmeset)) 
# 
# }
dtax<-dta_setx(dta,setx[2,],1)
lmerun(lmef[[1]],dta,setx[2,],1)
outl.fun.rtc(dta)

lmerun<-function(set,resp,gr,chose,out){
  form<-create_lmeforms(set,resp)[[gr]]
    set2<-get_rtc(set)
  ifelse(out==1,set2<-outl.fun.rtc(set2),set2<-set2)
  lmeset<-dta_setx(set2,chose,out)
  det_cat<-stri_detect (as.character(form[3]),regex  = "category")
  det_vs<-stri_detect (as.character(form[3]),regex  = "vs")
  sum1<-( lmer(form,lmeset)) 
  sum2<-summary(sum1)
    dif<-abs(coef(sum2)[1]-coef(sum2)[2])
    print(sum2$coefficients)
    #wenn global&category 
    ifelse(chose[4]==1&det_cat==T,out<-c(T,", diff:",dif,"ms"),
           out<-F)
    ifelse(length(sum2$coefficients[,1]<=2),out2<-coef(sum2)[1]>coef(sum2)[2],out2<-"blu")

    cat("global",chose[5],"=",out,"\nIntercept greater =",out2) #nicht beide TRUE > global = F
    # wenn !category | !vsAll >         
    ifelse(out==F,ifelse(det_vs!=T,out<-c("\ndifference category",chose[5]," ~ ",chose[6],dif,"ms\n"),
                         ifelse(chose[4]!=1,out<-c(", diff:",dif,"ms\n"),out<-"\nkeine berechnung\n")),     
                         out<-"\n---------------\n")
    cat(out)
    cat(as.character(form),"\n")
    #(dif)
  return(sum1)
  
}
# as.character(fmlRTCgr[3])
setx[7,4]==T
lmef[[1]][3]
cat(as.character(lmef[[1]]))
(lmerun(dta,"ti",1,c(0,0,0,1,em,sm),1)) #RTC
#             Estimate Std. Error           df   t value  Pr(>|t|)
# (Intercept) -2043.817   1149.383 0.0001254110 -1.778186 0.9992778
# XvsGr1SM     1720.789   1157.860 0.0001291520  1.486180 0.9992813
# XvsGr2EM     1953.149   1166.471 0.0001330369  1.674408 0.9992458
# XvsGr3LC     1807.058   1160.266 0.0001302287  1.557451 0.9992698
# XvsGr4MM     1832.557   1158.211 0.0001293085  1.582230 0.9992724
# SM < LC <= MM < EM
# with outliers discarded 2,5sd
# #             Estimate Std. Error          df   t value  Pr(>|t|)
# (Intercept) -2043.817   953.2100 0.007625365 -2.144141 0.9707632
# XvsGr1SM     1727.195   962.4626 0.007925755  1.794558 0.9711459
# XvsGr2EM     1767.469   964.0069 0.007976722  1.833461 0.9708222
# XvsGr3LC     1812.692   965.0320 0.008010718  1.878376 0.9705283
# XvsGr4MM     1828.215   962.6068 0.007930514  1.899234 0.9706952
# SM < EM < LC < MM
(lmerun(lmef[[2]],dta,c(0,0,0,1,em,vso))) #TI
#            Estimate Std. Error           df   t value  Pr(>|t|)
# (Intercept)  300.000   1140.089 3.058947e-06 0.2631373 0.9999825
# XvsGr1SM    1318.191   1155.392 3.226513e-06 1.1409043 0.9999769
# XvsGr2EM    1460.421   1164.039 3.324189e-06 1.2546154 0.9999760
# XvsGr3LC    1500.030   1158.029 3.256070e-06 1.2953304 0.9999763
# XvsGr4MM    1476.138   1155.909 3.232291e-06 1.2770366 0.9999765
# SM < EM <= MM < LC
(lmerun(lmef[[3]],dta,c(0,0,0,1,em,vso)))
(lmerun(lmef[[4]],dta,c(0,0,0,0,sm,lc)))
(lmerun(lmef[[1]],dta,c(0,0,0,0,sm,lc)))
(lmerun(lmef[[1]],dta,c(0,0,0,0,sm,lc)))

sum1<-(lmerun(fmlTIgr,dta,c(0,0,1,1,sm,em)))
sum2<-summary(sum1)
length(sum2$coefficients[,1])<=2
#
setx[17,]<-c(0,0,1,T,sm,vso)
dtax<-outl.fun(dta,200)
#wks.
#TODO 12146: sets XvsOther dependant on setx[x]
#################################
# lmedataset<-dta_setx(dta2,0,0,0,F,sm,em)
# form<-fmla2
# length(lmedataset$vsGroup)
# lmerun(fmla4,dta,setx[2,])
####################################################
#lmer sets
#formula:
#(target-1,target0,target+1,SMvsO=T/F,group1,group2)
lmerun(fmla1,dta,setx[1,])
tail(dta2)
lmer(fmla2,dta4)
lmerun(set1)
remove(dtatg)
#################### THIS
dta4<-getchars(dta2)
tail(dta4)
lmerun(fmla1,dta4,ch1)


# ch8<-c(0,0,0,F,sm,em)
# ch9<-c(0,0,0,F,sm,em)
# ch10<-c(0,0,0,F,sm,em)
# ch11<-c(0,0,0,F,sm,em)
# 

# summary(lmerun(fmla1,dtap4,0,0,0,F,sm,em))  
# summary(lmerun(fmla1,dtap4,0,0,1,F,sm,em))  
# x<-2
# sum1<-(lmerun(fmla1,dtap4,chosx[x,]))  
# sum2<-(lmerun(fmla1,dtap4,0,0,0,F,sm,em))  
# sum2<-(lmerun(set1))  
# sum1
# sumcpt<-
# set1<-(as.list(fmla1,dtap4,c(0,0,0),F,sm,em))
# set1<-as.list(0)
# set1[[1]]<-as.character(fmla1)
# as.character(fmla1)
# lmeform<-stri_join(set1[[1]][2],set1[[1]][1],set1[[1]][3],sep=" ")
# lmeform
# set1[[1]]<-lmeform 
# set1[["data"]]<-dta4
# #set1[["target"]][1]<-0
# #set1[[3]]
# set1[["chose"]][1]<-0
# set1[["chose"]][2]<-0
# set1[["chose"]][3]<-0
# set1[["chose"]][4]<-F
# set1[["chose"]][5]<-sm
# set1[["chose"]][6]<-em
# ### worked >
# set1[["t1"]]<-0
# set1[["t2"]]<-0
# set1[["t3"]]<-0
# #set1[["target"]][2]<-0
# #set1[["target"]][3]<-0
# set1[["SMvsO"]]<-F
# set1[["g1"]]<-sm
# set1[["g2"]]<-em
# #set1[["group"]][1]<-sm
# #set1[["group"]][2]<-em
# set2<-as.data.frame(set1)
# #as.formula(set1)
# #write_delim(set1,"set1.xls")
# write_csv2(set2,"set2.csv")
# ##############
# set2$data.X.1[9:11]<-mnresp1
# set2$data.X.2[9:11]<-mnresp2
# set2$data.X[7]<-"MEAN"
# set2$data.X.1[8]<-""
# set2$data.X.2[8]<-"MEAN"
# set2$data.X[12]<-"COEF"
# set2$data.X.2[8]<-"MEAN"
# 
# lmerun(set1)
# set1[4]
# sum2<-summary(lmerun(fmla1,dtap4,0,-1,1,F,sm,mm))
# as.data.frame(sum1)
# dif<-sum2$coefficients[1]-sum2$coefficients[2]
# dif

# #---------------------------------------------------------------------

#############################################################################
#############################################################################
#---C---  compare R/F results:-----------------------
#For these raw data, the mean reading time for the critical segments in the 
#Literal condition was       1457 ms (SD 727 ms), in the 
#Extended Metaphor condition 1543 ms (SD 764 ms), and in the 
#Single Metaphor condition   1578 ms (SD 768 ms).
#LC < EM < SM
#C.1
tb1<-getmean(dta,0,0,0,0,0,0) # with RAW dataset
#here results reading time RAW, target 0
#        mean        sd
# SM 1623.482  834.4605
# EM 1761.593 1648.5537
# MM 1777.380  958.3569
# LC 1835.347 1304.3131

##################
#(without outliers): the mean response times for the critical segments were 
#1421 ms (SD 650 ms) in the Literal condition, 
#1498 ms (SD 716 ms) in the Extended Metaphor condition, 
#1537 ms (SD 716 ms) in the Single Metaphor condition.
#LC < EM < SM
#C.2
#without outliers (2,5sd)
dta1<-outl.fun(dta,250) #discard outliers with bottom cutoff at 250ms
tb2<-getmean(dta1,0,0,0,0,0,0)
#      mean        sd
# EM 1655.608 1033.4464
# SM 1734.169  761.2755
# MM 1906.137  879.5549
# LC 1978.348 1261.6891

#second formula, outliers discarded with respect to target length
dta2<-outl.fun.rtc(dta)
tb3<-getmean(dta2,0,0,0,0,0,0)
#       mean        sd
# EM 1563.051 1062.1094
# SM 1623.482  834.4605
# MM 1777.380  958.3569
# LC 1835.347 1304.3131

#mean RT bei target 0+1, without outliers
#       mean       sd
# EM 1846.250 1355.556
# SM 1998.554 1329.243
# MM 2151.842 1299.206
# LC 2206.770 2147.328
# 

#----
#lme4:
#(R/F): with the Single condition being read slower than the others (coefficient = 77.3, SE = 24.9, t = 3.10, p < 0.01)
# SMvsOther
# fmla2: timeinterval ~ vsGroup + (1 | item) + (1 | participant) + (1 + vsGroup | participant)
#             Estimate    Std. Error  df        t value     Pr(>|t|)
# (Intercept) 1627.63038   187.2118  16.62067  8.694058 1.378434e-07
# vsGroup2EM   -53.31639   114.9268  67.23981 -0.463916 6.442063e-01
# vsGroup3LC   171.50058   118.1121 113.13773  1.452016 1.492647e-01
# vsGroup4MM   142.07474   112.3709 206.37242  1.264338 2.075349e-01

# fmla3: timeinterval ~ category + (1 | item) + (1 | participant) + (1 + category | participant)
#                  Estimate Std. Error        df  t value     Pr(>|t|)
# (Intercept)     1609.0465   193.5761  17.06412 8.312216 2.099865e-07
# categorySMvsAll  164.1103   125.8149 158.10744 1.304379 1.940000e-01

# R/F: no significant difference between Extended and Single metaphors (coefficient = 38.4, SE = 29.7, t = 1.30, n.s.)
# fmla1: rtc ~ vsGroup + (1 | item) + (1 | participant) + (1 + vsGroup | participant)
#             Estimate   Std. Error       df    t value     Pr(>|t|)
# (Intercept) 1628.72591   148.5456 21.54982 10.9644857 2.801232e-10
# vsGroup2EM   -55.76876   118.9061 40.99085 -0.4690151 6.415439e-01
#
#highly significant differences between each of these and the Literal condition 
#Literal vs. Extended: coefficient = 75.3, SE = 28.2, t = 2.68, p < 0.01
# fmla3
# #                   Estimate Std.Error        df    t value     Pr(>|t|)
# (Intercept)     1786.69783   208.1213  21.73803  8.5848850 1.976257e-08
# categoryLCvsAll  -72.24273   128.3764 273.48929 -0.5627417 5.740718e-01

#Literal vs. Single: coefficient = 114.7, SE = 28.6, t = 4.00, p < 0.001)
#             Estimate Std. Error        df  t value     Pr(>|t|)
# (Intercept) 1628.8417   201.6062  12.92014 8.079324 2.090719e-06
# vsGroup3LC   151.2718   127.6845 101.21898 1.184731 2.388981e-01
#RT category greater = TRUE , difference category LC  ~ all = 1477.57 ms
#### with length corrected RT:
#              Estimate Std. Error       df   t value  Pr(>|t|)
# (Intercept) -310.20431   124.7393 34.67972 -2.486821 0.0178540
# vsGroup3LC    62.97836   114.9605 92.25119  0.547826 0.5851345
#RT category greater = FALSE , difference category LC  ~ all = 373.1827 ms
#############
formel<-fmla1 #for rtc ~          groups   (length corrected RTs)
formel<-fmla2 #for timeinterval ~ groups   (without length correction)
formel<-fmla3 #for timeinterval ~ category (XvsOther)
formel<-fmla4 #for rtc ~          category (XvsOther)
#create group vs set
# setx[x,] < x has to be 7,71,711 for global compare sets
catsingle<-lc
setsingle<-setvsx(dta2,catsingle)
sum1<-summary(lmerun(fmla3,setsingle,setx[7,]))
sum1<-summary(lmerun(fmla1,dta2,setx[2,]))

dif<-abs(coef(sum1)[1]-coef(sum1)[2])
cat("RT category greater =",coef(sum1)[1]>coef(sum1)[2],", difference category",catsingle," ~ all =",dif,"ms")
(sum1$coefficients[])
diflc<-dif
diflcsm<-dif


#sum1
#with timeinterval fits with means in C.2

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

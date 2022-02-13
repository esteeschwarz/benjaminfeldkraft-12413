#Daten: #r+ &rScript

#library(readr)
#library(stringi)



#srvr<-"https://www.soscisurvey.de/hux2022/?act=fKtaZ3c23D7eVyfBFrzzGT08&rScript"
#srvd<-"https://www.soscisurvey.de/hux2022/?act=4eqU86Oc62EfGZm7PbHOjcmL"
#srvd2<-"https://www.soscisurvey.de/hux2022/?act=4eqU86Oc62EfGZm7PbHOjcmL"
#srvx<-"https://www.soscisurvey.de/hux2022/?act=XFHFbeVybOP6o72MZIy0lUlR"
#pre<-eval(parse("https://www.soscisurvey.de/hux2022/?act=qM85F9YYrhjRhLB3gvub2AyF&rScript"))

#print(ds$F401)
#ifelse(as.logical(attr(ds$F401,"*"))==1,print(5))
#attr()
#as.logical(ds$F404)

#as.logical(c(pi,1:10))
# #length(c(
#   "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","MT02","MT02_01","MT02_02",
#   "MT03","MT03_01","MT03_02","MT03_03","MT03_04","MT03_05","MT04","MT04_01",
#   "MT04_02","MT04_03","MT04_04","F401","F402","F403","F404","F405","F406","F407",
#   "F408","F409","F410","F411","F412","F413","F414","F415","F416","F417","F418",
#   "F419","F420","F421","F422","F423","F424","F425","TIME001","TIME002","TIME003",
#   "TIME004","TIME005","TIME006","TIME007","TIME008","TIME009","TIME010","TIME011",
#   "TIME012","TIME013","TIME014","TIME015","TIME016","TIME017","TIME018","TIME019",
#   "TIME020","TIME021","TIME022","TIME023","TIME024","TIME025","TIME026","TIME027",
#   "TIME028","TIME029","TIME_SUM","MAILSENT","LASTDATA","FINISHED","Q_VIEWER",
#   "LASTPAGE","MAXPAGE","MISSING","MISSREL","TIME_RSI","DEG_TIME"
# ))

#eval(parse(srvd2,encoding="UTF-8"))

#tst<-as.data.frame(ds)
#tst<-attr(ds$FR01,"*",TRUE)
#tst<-read_csv(srvwd,sep=" ",skip=1)
#xcl<-read_table2(srvx)
#library(readxl)
#data_hux2022_2022_02_11_10_02 <- read_excel(srvx)
#View(data_hux2022_2022_02_11_10_02)
#srvwd<-"hux2022b.xlsx"
#download.file(srvx,"hux2022b.csv")
#srvdta<-read_tab
#eval(parse("https://www.soscisurvey.de/hux2022/index.php?act=HI51SUMbGgqxTS7sOaZhvObS&vQuality&useSettings&rScript", encoding="UTF-8"))

#eval(parse("https://www.soscisurvey.de/hux2022/index.php?act=izH5VoemaNi2OS5dlmNdkI4k&vQuality&useSettings&rScript", encoding="UTF-8"))
#eval(parse("https://www.soscisurvey.de/hux2022/index.php?act=HI51SUMbGgqxTS7sOaZhvObS&vQuality&useSettings&rScript", encoding="UTF-8"))


#print(ds$F401["avector"])
###########THISSSSS###########
#insert data + script link
srvd3<-"https://www.soscisurvey.de/hux2022/?act=EpHVi8ffiKSbizRseExLpsec&rScript"

eval(parse(srvd3, encoding="UTF-8"))


##### as.integer(ds$F407) ####
##### print antwortcode # A1 = 1, A6 = 6, -9(NA) = 7 ####
#as.integer(ds$F407)
#########################################################
a1<-1
 a2<-2
 a3<-3
 a4<-4
 a5<-5
 a6<-6
 a7<-7
######
#obs(ds)
obs<- nrow(ds)
 item_names<-names(ds[20:44])
 lq<-length(item_names)
#eval1<-function(set,qx){
# items 1-26  
#  itemx<-"F4xx"
 # items<-c(1:26)
acp<-cbind(1:lq,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
#count A1 in obs
 #ct1<-0
 
 #todo
 #arary obs over options
 #u test arrays / observation
 tn_array_count<-obs*6
 tna<-tn_array_count
# tnid<-c(1,2,3) 
 #tncpt<-matrix(1:18,3)
tncptr<-matrix(1:tn_array_count,6)
tncpt_obs<-matrix(1:tna,obs)*0
tncpt_obs_sk<-matrix(1:tna,obs)*0
tncpt_sk2<-matrix(1:tna,obs)*0

  tncptr<-tncptr*0
  tncpt<-tncptr
  tncpt_sk<-tncpt
# tn<-2
# rm(tn)
  qcount<-lq
  acp2<-cbind(1:obs,1:qcount,1:qcount)
  acp2<-acp2*0
  #start looping
  for (tn in 1:obs){
    # matrix 6 options over 26 questions
   acp<-cbind(1:qcount,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
  
# hier skalierte werte eintrage
   for(k in 1:qcount){
    
  print(op<-  as.integer(ds[[item_names[k]]])) 
 #   ifelse(op[1]==1,ct1<-ct1+1,ifelse(op[2]==1,ct1<-ct1+1,"no"))
   # opv<-tabulate(as.integer(ds[[item_names[k]]]),6)
    op2<-as.integer(ds[[item_names[1][1]]][2])
   # print(op<-  as.integer(ds[[item_names[lq]]])) 
    #  
    #   ifelse(op2)
    #acp[op2,k+26]<-TRUE
    #27=
    #7=1
    #1=7
    f<-k
#    k1<-k+26
    #k2<-
   # tn<-1
    #get array position
  #  o1<-5
    o1<-op[tn] # option per tn
    o3<-o1*qcount+f
    print(o3)
    3*26
   # o3<-f*o2+f*26-f
#put true array position
        acp[o3]<-TRUE
     #########
           #nochmal skaliert
       # otn<-1
        #array position
        #f<-26
        #tn<-3
        #o4<-5
        o4<-op[tn]
        o5<-tn*qcount-qcount+f
        #put scaled value
opox<-ifelse(o4==1,2,ifelse(o4==2,3,ifelse(o4==3,5,ifelse(o4==4,5,ifelse(o4==5,1,ifelse(o4==6,1,0))))))
       acp2[o5]<-opox
          
          
      } # end question loop
   print ("ACP")
	  print(acp)
	 # rows observations
	  tncpt_obs[tn,1]<-sum(  print(acp[1:lq,2]))
	  tncpt_obs[tn,2]<-sum(  print(acp[1:lq,3]))
	  tncpt_obs[tn,3]<-sum(  print(acp[1:lq,4]))
	  tncpt_obs[tn,4]<-sum(  print(acp[1:lq,5]))
	  tncpt_obs[tn,5]<-sum(  print(acp[1:lq,6]))
	  tncpt_obs[tn,6]<-sum(  print(acp[1:lq,7]))
	  # colums observations
	  tncpt[1,tn]<-sum(  print(acp[1:lq,2]))
	  tncpt[2,tn]<-sum(  print(acp[1:lq,3]))
	  tncpt[3,tn]<-sum(  print(acp[1:lq,4]))
	  tncpt[4,tn]<-sum(  print(acp[1:lq,5]))
	  tncpt[5,tn]<-sum(  print(acp[1:lq,6]))
	  tncpt[6,tn]<-sum(  print(acp[1:lq,7]))
	  # skaliertes array
	  tncpt_sk[1,tn]<-sum(  print(acp[1:lq,2]*2))
	  tncpt_sk[2,tn]<-sum(  print(acp[1:lq,3]*3))
	  tncpt_sk[3,tn]<-sum(  print(acp[1:lq,4]*5))
	  tncpt_sk[4,tn]<-sum(  print(acp[1:lq,5]*5))
	  tncpt_sk[5,tn]<-sum(  print(acp[1:lq,6]*1))
	  tncpt_sk[6,tn]<-sum(  print(acp[1:lq,7]*1))
	# with values according to option
	  # tncpt_sk2[1,tn]<-sum(  print(acp2[1:lq,2]))
	  # tncpt_sk2[2,tn]<-sum(  print(acp2[1:lq,3]))
	  # tncpt_sk2[3,tn]<-sum(  print(acp2[1:lq,4]))
	  # tncpt_sk2[4,tn]<-sum(  print(acp2[1:lq,5]))
	  # tncpt_sk2[5,tn]<-sum(  print(acp2[1:lq,6]))
	  # tncpt_sk2[6,tn]<-sum(  print(acp2[1:lq,7]))
	  # 
	    
	  tncpt_obs_sk[tn,1]<-sum(  print(acp[1:lq,2]*2))
	  tncpt_obs_sk[tn,2]<-sum(  print(acp[1:lq,3]*3))
	  tncpt_obs_sk[tn,3]<-sum(  print(acp[1:lq,4]*5))
	  tncpt_obs_sk[tn,4]<-sum(  print(acp[1:lq,5]*5))
	  tncpt_obs_sk[tn,5]<-sum(  print(acp[1:lq,6]*1))
	  tncpt_obs_sk[tn,6]<-sum(  print(acp[1:lq,7]*1))
	#########
	  ##
#	  tncptr[6,2]<-1
	 # a<-c(c(1:6))
	 # b<-c(a,a)
	 # b<-c(c(1:3),c(1:3))
	 # b<-append(b,a,after=length(b))
	 # c<-c(tnop_array(tn,acp))
	 # d_arr<-qcount*obs
	 # d<-d_arr*0
	 # d<-append(d,c,after=length(d))
	 # c<-0
	 # e<-append(c(tnop_array(tn,acp)),c(tnop_array(tn,acp)),after=length(c))
	 # e<-append(c(tnop_array(tn,acp)),c(tnop_array(tn,acp)),after=length(c))
	  
	  
	   } # end tn loop
# print(e)
tnop_array<-function(tn,acp){
tnopsum1<-sum((acp[1:lq,2]))
tnopsum2<-sum((acp[1:lq,3]))
tnopsum3<-sum((acp[1:lq,4]))
tnopsum4<-sum((acp[1:lq,5]))
tnopsum5<-sum((acp[1:lq,6]))
tnopsum6<-sum((acp[1:lq,7]))

  return (c(tnopsum1,tnopsum2,tnopsum3,tnopsum4,tnopsum5,tnopsum6))
}


#### tnarrayr for u test
a<-c(c(1:6))
b<-cbind(a,a)
tn_arrayx<-c(1:6)*0
#rows oure
tn1op<-tncpt[1:6]
tn2op<-tncpt[7:12]
tn3op<-tncpt[13:18]
#rows skaliert
tn1op_sk<-tncpt_sk[1:6]
tn2op_sk<-tncpt_sk[7:12]
tn3op_sk<-tncpt_sk[13:18]

tn1op_sk2<-acp2[1:26]
tn2op_sk2<-acp2[27:54]
tn3op_sk2<-acp2[55:78]
#print(tncpt[1:6])
chisq.test(tn2op,tn3op)
wilcox.test(tn2op,tn3op)
chisq.test(tn2op_sk,tn3op_sk)
wilcox.test(tn2op_sk,tn3op_sk)

chisq.test(tn2op_sk2,tn3op_sk2)
wilcox.test(tn2op_sk2,tn3op_sk2)

levels(ds$F401)
#print(tncpt)
#cbind(tncpt[1:6])
#rm(tnoparray)
########################################################
####################### U TEST #########################
################## VLG 12043.chi_notes #################
# WHAT is the goal? #####
#1. find out if there is significant difference between the
#answers between observations
#2.linear lmer if theres sig. dif. at target
#dependent on (random effect of) other chosen options 
#hux2021 sample formel: 
#lme1<-lme1.formula.1<-(timeinterval ~ charsA + (1|participant)+(1+charsA:participant))
## again array topdown observations
### die werte müssen ordinalskaliert werden, um gewichte pro option vergeben zu können?
# matrix: left: questions, top: tn, vaules: option



linetest<- function(tncpt){
  library(lme4)
  library(lmerTest)
  random2<-tncpt_obs_sk[4:6]
  random3<-tncpt_obs_sk[7:9]
  random4<-tncpt_obs_sk[10:12]
  random5<-tncpt_obs_sk[13:15]
  random6<-tncpt_obs_sk[16:18]
    lme1<-(tncpt_obs[1:3]~random3+(1|random4))
    lme2<-(ds$F401)
    lmer(lme1)
}



d1<-tn1op
d2<-tn2op
d3<-tn3op
d1<-
chisq.test(d1,d2)
wilcox.test(d1,d2,correct = FALSE)
#wilcox.test(d1,d2,alternative="greater")

d3<-c(d1,d2)
#wilcox.test(d3)

##insert first set C ///comment line for further set D
#d3<-c(cst1,cst2)

d5<-rank(d3)
d4<-cbind(d5,d3)
d6<-sum(d5[1:(length(d5)/2)])
#p.175 R1, R2
r1<-d6
d7pos<-length(d5)/2+1
d7<-sum(d5[d7pos:length(d5)])
r2<-d7

#choose SET hux2022
a1<-d1<-tn1op
a2<-d2<-tn2op
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

###########
#12045.
#p.175
z2u<-(umin-(n1*n2/2))/(sqrt(n1*n2*(n1+n2+1)/12))
print(d5)
#verbundene rangplätze finden
########################################
#this counting vorkommen of rank /// WORKS
d5<-d3
d6<-unique(d5)
posd6<-d6
posd7<-d6
x<-1
pos<-1
for (x in pos:length(d6)){
  # print (x)
  #print(d6[x])
  for (l in pos:length(d5)){
    # print(l)
    ifelse (d6[x]==d5[l],posd6[x]<-posd6[x]+1,posd6[x])
    
  }
}
print (d6)
print (d3)
print(posd6-posd7)
d8<-posd6-posd7
#fälle vorkommen ties: 3-1-4-5-6-4-2-1-1-1-1-1 cvd. 1*3,2*4,1*5,1*6,1*2,6*1
#(3^3-3)+6*(1^3-1)+2*(4^3-4)+(5^3-5)+(6^3-6)+(2^3-2)
#480

#####
d9<-unique(d8)
posd8<-d8
posd9<-d8
x<-1
pos<-1
for (x in pos:length(d9)){
  # print (x)
  #print(d6[x])
  for (l in pos:length(d8)){
    # print(l)
    ifelse (d9[x]==d8[l],posd9[x]<-posd9[x]+1,posd9[x])
    
  }
}
print (d8)
print (d9)
print(posd9-posd8)
d10<-posd9-posd8
#(3^3-3)+6*(1^3-1)+2*(4^3-4)+(5^3-5)+(6^3-6)+(2^3-2)
#for(l in 1:length(d10)){
#tie<-(d10[l]*(d9[l]^3-d9[l]))
#}
tie<-(d10*(d9^3-d9))
print (tie)
print(sum(tie)) ####YES!


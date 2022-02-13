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
 item_names<-names(ds[20:45])
#eval1<-function(set,qx){
# items 1-26  
#  itemx<-"F4xx"
 # items<-c(1:26)
acp<-cbind(1:26,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
#count A1 in obs
 #ct1<-0
 
 #todo
 #arary obs over options
 #u test arrays / observation
 tn_array_count<-obs*6
# tnid<-c(1,2,3) 
 #tncpt<-matrix(1:18,3)
tncptr<-matrix(1:tn_array_count,6)
  tncptr<-tncptr*0
  tncpt<-tncptr
# tn<-2
# rm(tn)
  qcount<-26
 for (tn in 1:obs){
   acp<-cbind(1:qcount,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
   
   for(k in 1:qcount){
    
  print(op<-  as.integer(ds[[item_names[k]]])) 
 #   ifelse(op[1]==1,ct1<-ct1+1,ifelse(op[2]==1,ct1<-ct1+1,"no"))
   # opv<-tabulate(as.integer(ds[[item_names[k]]]),6)
    op2<-as.integer(ds[[item_names[1][1]]][2])
 #   ifelse(op2)
    #acp[op2,k+26]<-TRUE
    #27=
    #7=1
    #1=7
    f<-k
#    k1<-k+26
    #k2<-
   # tn<-1
    o1<-op[tn] # option per tn
#    o2<-o1
    o3<-o1*qcount+f
    print(o3)
   # o3<-f*o2+f*26-f
    acp[o3]<-TRUE
  #  o3<-0
   # o2<-0
  #  tnoparray<-tnop_array(tn,acp)
    
      } # end question loop
   print ("ACP")
	  print(acp)
	 # tn_table<-cbind(tnoparray[tn])
	  #tncpt[tn,op]<-
	  # tncpt[tn,1]<-sum(  print(acp[1:26,2]))
	  # tncpt[tn,2]<-sum(  print(acp[1:26,3]))
	  # tncpt[tn,3]<-sum(  print(acp[1:26,4]))
	  # tncpt[tn,4]<-sum(  print(acp[1:26,5]))
	  # tncpt[tn,5]<-sum(  print(acp[1:26,6]))
	  # tncpt[tn,6]<-sum(  print(acp[1:26,7]))
	  # 
	  tncpt[1,tn]<-sum(  print(acp[1:26,2]))
	  tncpt[2,tn]<-sum(  print(acp[1:26,3]))
	  tncpt[3,tn]<-sum(  print(acp[1:26,4]))
	  tncpt[4,tn]<-sum(  print(acp[1:26,5]))
	  tncpt[5,tn]<-sum(  print(acp[1:26,6]))
	  tncpt[6,tn]<-sum(  print(acp[1:26,7]))
	  ###
#	  tncptr[6,2]<-1
	  a<-c(c(1:6))
	  b<-c(a,a)
	  b<-c(c(1:3),c(1:3))
	  b<-append(b,a,after=length(b))
	  c<-c(tnop_array(tn,acp))
	  d_arr<-qcount*obs
	  d<-d_arr*0
	  d<-append(d,c,after=length(d))
	  c<-0
	  e<-append(c(tnop_array(tn,acp)),c(tnop_array(tn,acp)),after=length(c))
	  e<-append(c(tnop_array(tn,acp)),c(tnop_array(tn,acp)),after=length(c))
	  
	  
	   } # end tn loop
 print(e)
tnop_array<-function(tn,acp){
tnopsum1<-sum((acp[1:26,2]))
tnopsum2<-sum((acp[1:26,3]))
tnopsum3<-sum((acp[1:26,4]))
tnopsum4<-sum((acp[1:26,5]))
tnopsum5<-sum((acp[1:26,6]))
tnopsum6<-sum((acp[1:26,7]))

  return (c(tnopsum1,tnopsum2,tnopsum3,tnopsum4,tnopsum5,tnopsum6))
}


#### tnarrayr for u test
a<-c(c(1:6))
b<-cbind(a,a)
tn_arrayx<-c(1:6)*0
tn1op<-tncpt[1:6]
tn2op<-tncpt[7:12]
tn3op<-tncpt[13:18]
#print(tncpt[1:6])
chisq.test(tn2op,tn3op)
wilcox.test(tn2op,tn3op)
#print(tncpt)
#cbind(tncpt[1:6])
#rm(tnoparray)
########################################################
####################### U TEST #########################
################## VLG 12043.chi_notes #################



d1<-tn1op
d2<-tn2op
#wilcox test
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


#12112.SPUND INTERAKTION conversation analysis (MDL)
#20220313(15.47)

#######################################
#file keeping###
#audio
#52: 20201128 podcast "transphilosophisch, folge 52", lockdown, solo recording
##/lanwer/MDL/TP#52.wav
#44: 20200808 pre lockdown, recording ensemble
##/lanwer/MDL/TP#44.wav
#annotation delay table
##/lanwer/MDL/#52#DEL_imp.csv
#partitur editor files
##/lanwer/MDL/MDL_12112_F52.exb
##/lanwer/MDL/MDL_12104_F44.exb
###############################

#import table with delay annotation times
#read.csv2("#52#DEL_imp.txt",sep = " ")
library(readr)
mdl52 <- read_table2("#52#DEL_imp.csv", 
                            col_names = FALSE)

mdl44<-read_table2("#44#DEL_imp.csv", 
                   col_names = FALSE)
mdl1<-mdl44
print(mdl1)
library(stringi)
mdl1[1]
#str2expression(mdl2[1])
mdl2<-matrix(mdl1)
mdl2
#as.numeric(mdl2[1])
#stri_count_boundaries(mdl2[1],"(")
#stri_split(mdl4,"#")
mdl3<-stri_replace_all_fixed(mdl2,"(","")
mdl4<-stri_replace_all_fixed(mdl3,")","#")
mdl5<-unlist(strsplit(mdl4,"#"))
mdl6<-matrix(mdl5)
mdl7<-as.numeric(mdl6)
mdl8<-subset(mdl7,mdl7!=is.na(mdl7))
mean(mdl8)
cat(mdl8)

dis44<-mdl8[1:length(dis52)]
dis52<-mdl8

dis1<-cbind("1x2 record"=dis44,"2x1 record"=dis52)
plot(dis1,main = "delay of speaker secure turns")
#plot(line(dis1))
z<-line(dis1)
abline(coef(z))

boxplot(dis1)
mn44<-mean(dis44)
mn52<-mean(dis52)
mndis<-mn52-mn44
tdis<-t.test(dis1,alternative = "g",var.equal = T)
tdis
#summary(tdis)
tdis$stderr
tdis$p.value
############
# significant difference between delay at:
# p = 2.19e-14
# mean difference:
# 0.232 sec
############


#U-test
##rosemeyer uebung set D
#stichproben 1,2
d1<-dis44
d2<-dis52
d3<-c(d1,d2)

d5<-rank(d3)
d4<-cbind(d5,d3)
d6<-sum(d5[1:25])
d7<-sum(d5[26:50])

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

#according to meindl appendix table A with z = +-2.35 > flächenateil 0.9906
#i.e. restfläche:
100-0.9906
#99.0094
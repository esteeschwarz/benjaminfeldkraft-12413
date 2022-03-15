#12112.SPUND INTERAKTION conversation analysis (MDL)
#20220313(15.47)
#20220315(19.04)

#######################################
#file keeping#
#script source: "https://github.com/esteeschwarz/essais/blob/main/docs/STAT_R/INT_CA.mdl.notes_12112.R"
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
library(readr)
library(stringi)

mdl52 <- read_table2("~/boxHKW/21S/SPUND/lanwer/MDL/#52#DEL_imp.csv", 
                            col_names = FALSE)

mdl44<-read_table2("~/boxHKW/21S/SPUND/lanwer/MDL/#44#DEL_imp.csv", 
                   col_names = FALSE)
#both samples same length
#mdl44<-sample(mdl44,length(mdl52))

#mdl1<-mdl44
#print(mdl1)
#mdl1[1]

#remove(mdl1)
#mdl2
#as.numeric(mdl2[1])
#stri_count_boundaries(mdl2[1],"(")
#stri_split(mdl4,"#")

mdl9<-function(set){
mdl2<-matrix(set)
mdl3<-stri_replace_all_fixed(mdl2,"(","")
mdl4<-stri_replace_all_fixed(mdl3,")","#")
mdl5<-unlist(strsplit(mdl4,"#"))
mdl6<-matrix(mdl5)
mdl7<-as.numeric(mdl6)
mdl8<-subset(mdl7,mdl7!=is.na(mdl7))
#mean(mdl8)
return(mdl8)
}
cat(mdl9(mdl44))
cat(mdl9(mdl52))

#mdl9(mdl44)

#dis44<-mdl8[1:length(dis52)]
dis44<-mdl9(mdl44)
dis52<-mdl9(mdl52)
difmdl<-ifelse(length(dis44)>length(dis52),dif1<-length(dis52),dif1<-length(dis44))
sdis44<-sample(dis44,difmdl)
sdis52<-sample(dis52,difmdl)
mean(dis44)
mean(dis52)

dis1<-cbind("1x2 record"=sdis44,"2x1 record"=sdis52)
plot(dis1,main = "delay of speaker secure turns")
#plot(line(dis1))
z<-line(dis1)
abline(coef(z))

boxplot(dis1)
mn44<-mean(sdis44)
mn52<-mean(sdis52)
mndis<-mn52-mn44
tdis<-t.test(dis1,alternative = "g",var.equal = T)
tdis
#summary(tdis)
tdis$stderr
tdis$p.value
############
# significant difference between delay at:
# p = 2.1e-15
# mean difference:
# 0.242 sec
############


#U-test
##rosemeyer uebung set D
#stichproben 1,2
d1<-sdis44
d2<-sdis52
d3<-c(d1,d2)

d5<-rank(d3)
d51<-length(d5)
d4<-cbind(d5,d3)
d52<-d51/2
d6<-sum(d5[1:d52])
d61<-d52+1
d7<-sum(d5[d61:d51])
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

#according to meindl appendix table A with z = +-2.65 > flächenateil 0.9906
#i.e. restfläche:
100-0.9959
#99.0041% ablehnung der nullhypothese=kein latenzunterschied > hoch signifikanter latenzunterschied

#die signifikanzen sind noch nicht beständig, da die zwei stichproben unterschiedliche längen haben (gemessene delays)
#und ich das bisher nur über sample() anpasse, auf dasz die variablenlängen vergleichbar sind. ich könnte auch einfach
#die obergrenze kappen und von 2x27 observationen ausgehen, dann stellte sich das problem unterschiedlicher ergebnisse nicht,
#das schöne jedoch ist, dasz die hypothese bei jedem neuen sample trotzdem bestätigt wird, mit kleinen abweichungen im
#mean.
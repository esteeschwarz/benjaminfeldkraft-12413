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
plot(dis44,dis52)
ggplot(dis44,dis52)
df<-as.data.frame(dis1)  
ggplot(df)
dis1<-cbind(dis44,dis52)
boxplot(dis1)
ggplot(dis1)
mn44<-mean(dis44)
mn52<-mean(dis52)
mndis<-mn52-mn44
tdis<-t.test(dis1,alternative = "g",var.equal = T)
tdis
tdis$stderr
tdis$p.value

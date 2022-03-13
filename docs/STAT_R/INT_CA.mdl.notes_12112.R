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
mdl1 <- read_table2("#52#DEL_imp.csv", 
                            col_names = FALSE)
print(mdl1)
library(stringi)
mdl1[1]
str2expression(mdl2[1])
mdl2<-matrix(mdl1)
mdl2
as.numeric(mdl2[1])
stri_count_boundaries(mdl2[1],"(")
stri_split(mdl4,"#")
mdl5<-unlist(strsplit(mdl4,"#"))
mdl3<-stri_replace_all_fixed(mdl2,"(","")
mdl4<-stri_replace_all_fixed(mdl3,")","#")
mdl6<-matrix(mdl5)
mdl7<-as.numeric(mdl6)
mdl8<-subset(mdl7,mdl7!=is.na(mdl7))
mean(mdl8)
cat(mdl8)

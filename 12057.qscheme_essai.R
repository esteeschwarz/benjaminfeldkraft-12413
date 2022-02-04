#12054.item XML schemes
#20220304(13.23)
#hux2022 sprichworttest charite
#
#steps:
#1. build musteritems in https://soscisurvey.de
#1.1 2 items using kontext, question, mc test 6 options
#2. export template to .xml
#3.1 task: enable modification of template for reimport new question set into platform by importing new questions from csv-table where the remaining 62 items are stored
#with kontext, question and 6 options
#3.2 
#import data
#xmlschema
library(Hmisc)
#clowess()
download_xml("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/package_hux2022_fragen_templateB_2022-02-04.xml","template.xml")
#getRS()
#getRS(guser="esteeschwarz",grepo="essais",gdir="main")
#getRs(file=NULL, guser='harrelfe', grepo='rscripts', gdir='raw/master',
 #     dir=NULL, browse=c('local', 'browser'), cats=FALSE,
  #    put=c('rstudio', 'source'))
#download.file("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/master/hux2022/proverbs/Kopie%20von%20package_fragen_2022-02-01_mod.xlsx","tempmod.xlsx")
scheme<-read_xml("template.xml")
xml_text(scheme)
#library(tidyverse)
#print(a1<-xml_text(xml_node(scheme,8)))
#html_element()
#xml_name(scheme)
#xml_attr.xml_node(scheme,lead)
#xml_attrs(scheme)
children1<-(xml_children(xml_children(xml_children(xml_contents(scheme)))))
children2<-(xml_children(xml_children(xml_children(xml_children(xml_contents(scheme))))))
children0<-(xml_children(xml_children(xml_contents(scheme))))

#a1_cases<-(xml_text(xml_children(xml_children(xml_children(xml_children(xml_contents(scheme)))))))
#a1<-(xml_text(xml_children(xml_children(xml_children(xml_contents(scheme))))))

a1<-antworten<-(xml_text(children1))
items_complete<-a0<-(xml_text(children0))
print (a0)
print (a1)



a2<-xml_text(scheme,2)
a2<-xml_text(scheme,2)
#modify
a1[7]<-"testing" #works




############################################# from here earlier state #######################
#qscheme <- read.csv("qscheme.csv", sep=";")


#itemtabelle
#templ <- read.csv("https://github.com/esteeschwarz/12431_hux2021-appendix/blob/master/hux2022/proverbs/12054_qscheme/template.csv", sep=";")

#per item:
#2 leerzeilen
#6 *2 optionen
#14 zeilen
#3,4 A1
#5,6 A2

# 
# 
# check1<-2+6*2
# print(qscheme$answer[1:14])
# qscheme$answer[13]<-"test change"
# print(qscheme$answer[1:14])
# #works, restor
# qscheme$answer[13]<-qscheme$answer[14]
# print(qscheme$answer[1:14])
# #works.
# ## assign template
# qscheme$answer[13]<-template$A5[1]
# print(qscheme$answer[1:14])
# #works
# opt1<-1
# opt2<-opt1*2
# #loop
# 
# for (x in opt1:length(qscheme$itemid)) {
#   optv<-opt1*x
#   optv1<-optv/14
#   print(optv1)
#   #wenn x ein vielfaches von 14 ist
# 
#   }
# 
# 14*64
# #896
# 64/4
# 
# questid<-c(1:optv1)
# opv1<-questid
# #now double each
# questid2<-c(0,0,opv1[1],opv1[1],opv1[2],opv1[2],opv1[3],opv1[3],opv1[4],opv1[4],opv1[5],opv1[5],opv1[6],opv1[6])
# print(questid2)
# #now loop
# #16 item, 4 gruppen, 2*(6 optionen+ 1 leerzeile)
# 16*7*2*4 #896
# 896/14 #64 items
# #x2 loop formel:
# 768/6 #128
# 64*2
# 128*6
# 16*6*2
# 
# #x2 in 6er schritten
# 16*6*2*4 #768
# x2f<-c(1:128)
# x2f2<-x2f*6
# x2f3<-
# #for(x3 in 1){
# rr<-(function(x,y){z<-x^2+y^2;x+y+z})(0:7,2)
# norm<-function(x) x*6
# x2f3<-c(norm(1:64))
# print (x2f3)  
#   fill<-  function(id5){
# id0<-c()      
# id1<-c()      
# 
#     #append(id0, 1:5, after =3)
# for (x1 in 1){
# #x2<-x1+6
#   #inner loop 1
# for (x in x2f2) {
# questid3<-c(0,0,x,x,x+1,x+1,x+2,x+2,x+3,x+3,x+4,x+4,x+5,x+5)
# print(questid3)
# id1<-append(id0,questid3)
# }
# #  print(questid3)
#   #return this to vector!
# #id0<-c()  
# #id2<-append(id1,questid3)
# #questid4<-c(questid3[1]:length(questid3))  
# }
# }
# print(id0)
# questid4
# 
# print(questid3)
# print(questid4)
# 
# #let two rows empty, then put numbers 1-6 each twice in the following rows.
# #then let again 2 rwos empty, put numbers 7-12 and so on
# 
# 
# 
# 
# 
# 
# #A1_1<-c(opt0*3,opt0*4)
# #A1_2<-c(opt1*5,opt1*6)
# opv1[2]
# #created row with length of options complete
# check2<-optv/16/4 #16 items in 4 gruppen
# check3<-check2/check1  #should be 1
# 
# 
# # how to fill in questionid row 29-42 with two empty + 2*6
# 
# print(qscheme$questionid[29:42])
# 
# 
# 

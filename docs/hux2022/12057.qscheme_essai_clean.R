#12054.item XML schemes
#hux2022 sprichworttest charite
#20220304(13.23)
#20220307(19.17) working state
###############################
#abstract
#script procedure to substitute itemset in xml-scheme for
#future import of modified scheme into survey platform
#to automatize survey building / building of survey content
#which consitst of 16x (context,question, 6 fold mc test)
#comments:
#ivan: 20220308(12.21) dieses script läuft jetzt zufriedenstellend und ersetzt
#in einem muster xml-schema, das ich von soscisurvey importiert habe die dort 
#gespeicherten fragen (item 1, item 2, + 62 leere items) durch die in der itemvorlage
#als .csv gespeicherten bisher zur verfügung stehenden 26 items aus GR01, GR04.
#die modifizierte scheme datei wird zur überprüfung am ende nochmal importiert und gibt
#bei einer abfrage die aktualisierten fragen aus. es klappt.

#task steps:
#1. build musteritems in https://soscisurvey.de
#1.1 2 items using kontext, question, mc test 6 options
#2. export template to .xml
#3.1 task: enable modification of template for reimport new question set into platform by importing new questions from csv-table where the remaining 62 items are stored
#with kontext, question and 6 options
#3.2 
#import data
#xmlschema
#install.packages("xml2")
library(xml2)
#download_xml("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/package_hux2022_fragen_templateB_2022-02-04.xml","template.xml")
#download.file("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/items/GR1/context2022_items_GR01.csv","items.csv")
#scheme<-read_xml("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/package_hux2022_fragen_templateB_2022-02-04.xml")
#items<-read.csv("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/items/GR1/context2022_items_GR01.csv",skip=1)

#########################################################

#local:
#actualise:
##########################################
#refresh_scheme <- function(ret_file){
#old scheme<-read_xml("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/package_hux2022_fragen_templateB_2022-02-04.xml")
#items<-read.csv("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/items/GR1/context2022_items_GR01.csv",skip=1)

#scheme<-read_xml("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/package_hux2022_fragen_templateB_mod_12061.2022-02-04.xml")
#scheme<-read_xml("https://github.com/esteeschwarz/essais/raw/main/docs/hux2022/package_hux2022_fragen_templateB_mod_12061.2022-02-04.xml")
#scheme<-read_xml(ret_file)

#sroot<-scheme
#}# > not needed
#########################################################
#1.1 fetch item data of still to refresh table
refresh_data <- function(ret_file){
  items<-read.csv(datenset,skip=1,sep=";")
}
#########################################################
#1.2. fetch xml scheme to modify or proof
#1.2.1 fetch elements of questions
refresh_base_q <- function(origin){
  sroot<-origin  
  setq<-(xml_children(xml_children(xml_children(sroot))))
  #seta<-pos4<-xml_children(xml_children(xml_children(xml_children(sroot))))
  #print(pos5<-xml_children(xml_children(xml_children(xml_children(xml_children(sroot))))))
  #print(set_question)
  #print(set_answers)
  return(setq)
}
#########################################################
#1.2.1 fetch elements of answers
refresh_base_a <- function(origin){
  sroot<-origin  
  # setq<-xml_children(xml_children(xml_children(sroot)))
  seta<-(xml_children(xml_children(xml_children(xml_children(sroot)))))
  #print(set_question)
  #print(set_answers)
  return(seta)
}
##########################
#now adress items generale
#lines formula in section (element) <items> (antwortoptionen)
#3,5,7,9,11,13 option A1-A6
#1,2 item id and position
#4,6,8,10,12 meta all same
#formel: ax = antwortoption
#######################################################
#1.2.3. fetch questions data in itemtable
get_question <- function(dataset,itemwitch,option){
  ###4.2.1
  #create new array with modifiying values
  #get new values of group items .csv
  newitem<-c()
  itx<-itemwitch
  items<-dataset
  newitem[1]<-items$item[itx] #description
  #newitem[2]<-"select"
  newitem[2]<-items$kontext[itx] #kontext
  newitem[4]<-"Weisen Sie der Frage die richtige Antwort zu." #explanation
  #newitem[5]<-"right"
  newitem[3]<-items$frage[itx] #fragetext
  #newitem[7]<-"default"
  return (newitem[option])
}

####works
#####################################################
#1.2.4 fetch answers data in item table
get_item<-function (dataset,item,option){
  items<-dataset
  itx<-item
  newopt<-c()
  #newopt[1]<-itx
  #newopt[2]<-itx
  newopt[1]<-items$A1[itx]
  #newopt[4]<-11
  newopt[2]<-items$A2[itx]
  #newopt[6]<-22
  newopt[3]<-items$A3[itx]
  #newopt[8]<-33
  newopt[4]<-items$A4[itx]
  #newopt[10]<-44
  newopt[5]<-items$A5[itx]
  #newopt[12]<-55
  newopt[6]<-items$A6[itx]
  #newopt[14]<-66
  return(newopt[option])
}

#works
#######################################################
#2. adress specific element to replace
#2.1. adress asnwers element
adress_answer<- function (set,itemnr,item_opt) {
  pos4<-set
  ax<-item_opt #antwortoption A1-A6
  lquest<-length(pos4)
  item_cpt<-64*14
  optionx<-c(3,5,7,9,11,13) #A1-A6
  item_adress_last<-(item_cpt/64*itemnr)
  item_adress_0<-item_adress_last-14
  itempos<-(item_adressx<-item_adress_0+optionx[ax])
  out_answer<-(set[itempos])
}
#works as function
###now for parent section (question, kontext) 
#TODO: adapt formula to new amount of items
######################################################
#2.2. adress questions element
adress_question<- function(set,itemnr,questionid){
  pos3<-set
  qx<-questionid
  lquest<-length(pos3)
  #formel adress line: 1+14 each
  quest_cpt<-64*14+1
  questionx<-c(2,4,7) #itemdescription,kontext,question
  question_adress_last<-(quest_cpt/64*itemnr)
  question_adress_0<-question_adress_last-14
  questpos<-(question_adressx<-question_adress_0+questionx[qx])
  #print(pos3[questpos])
  return(pos3[questpos])
}
#works 12062.14.17
##################
#now replace content
#itemdescription
##############################################################
#3. replace function. substitutes old elements content with retrieved content
#3.1. the main function
replace_content<-function(dataset,scheme_n,item,pos_a,pos_q){
  
  # element tags in xml scheme as in original
  val_text<-"text"         #tag, item
  val_lead<-"lead"         #tag, fragetext
  val_descr<-"description" #tag, itemnumber
  val_title<-"title"       #tag, kontext
  val_fail<-"tagfail"
  
  ifelse(pos_q==1,tag<-val_descr,ifelse(pos_q==2,tag<-val_title,ifelse(pos_q==3,tag<-val_lead,ifelse(pos_q==1:3,tag<-val_fail))))
  set_answers<-refresh_base_a(scheme_n)
  set_question<-refresh_base_q(scheme_n)
  
  print(tst_aq<-adress_question(set_question,item,pos_q))
  print(tst_aa<-adress_answer(set_answers,item,pos_a))
  print(tst_rq<-get_question(dataset,item,pos_q))
  print(tst_ra<-get_item(dataset,item,pos_a))
  #works
  #works
  #muster: xml_replace(adress_[answer|question](options),val,get_[item|question](options))
  #testreplace xml_replace(tst2,val ,tst_r )
  
  #### here happening the replacement according to above parameters
  xml_replace(tst_aq,tag ,tst_rq )
  xml_replace(tst_aa,val_text,tst_ra)
  ####
  #works
}
#################
########################################
#3.2 looping replacement over number of items, questions, answers
replace_loop <- function(){
  la<-length(items$item)
  for (k1 in 1:la){
    for (k2 in 1:3){
      replace_content(items,scheme,k1,1:6,k2)
    }
  }
}
##########################################################
#4. init variables
init<- function(set,opt,base_xml){
  items<-refresh_data(datenset)
  ifelse(set=="new",return(read_xml(base_xml)),ifelse(set=="mod",return(scheme<-read_xml(xmlmod)),ifelse(set=="old",return(scheme<-read_xml(xmlorigin)),return(items))))
  #seta<-refresh_base_a(base_xml)
  #setq<-refresh_base_q(base_xml)
  #ifelse(set=="items",return(items))
  #ifelse(opt=="a",return(seta),ifelse(opt=="q",return(setq),return(items)))
}
#################################################################
# proof:
###########################################################
#5.proof substitution
proof<-function(item,ax,qx){
  print("in dataset")
  print(get_question(items,item,qx))
  print(get_item(items,item,ax))
  print("#############################################")
  print("in scheme")
  print(adress_question(refresh_base_q(scheme),item,qx))
  print(adress_answer(refresh_base_a(scheme),item,ax))
  
}
#proof(8,1,3)
#get_item(items,1,2)
###############################################################
# declare scheme & dataset
xmlorigin<-("https://github.com/esteeschwarz/essais/raw/main/docs/hux2022/package_hux2022_fragen_templateB_mod_12061.2022-02-04.xml")
#xmlmod<-("qscheme_output.xml")

#to retrieve modified xml-scheme for integrating in soscisurvey.de set next 2 lines 
xmlmod_git<-("https://github.com/esteeschwarz/essais/raw/main/docs/hux2022/qscheme_output.xml")
xmlmod<-xmlmod_git

datenset<-("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/context2022_items_GR01_GR04.csv")
#datenset<-("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/context2022_items_GR04.csv")

#schemeset<-xmlorigin
schemeset<-xmlmod
base_xml<-xmlorigin
#seta<-refresh_base_a(schemeset)
#setq<-refresh_base_q(schemeset)
######## run routine: ############################## 
#1
items<-init("items",x,x)
#2
scheme<-init("mod",x,base_xml) #values: old=original scheme, mod=modified scheme
#3############################
#replaces all according to itemvorlagen .csv as specified in datenset 
#replace_loop()
#4############################
#write_xml(scheme,"qscheme_output.xml")
##############################
#6.proof: call item/antwort/question in modified scheme
# call: proof([item],[antwortoption 1-6],[questionfield 1-3])
#proof(27,1,3)

#####call replacement #################################
# discomment and rerun for single replacement
# call: replace_content([datenset itemvorlage],[schemeset],[item],[antwortoption],[questionoption])
#replace_content(items,scheme,6,1:6,1)

#5.now loop through the items and replace according to table actualise
# loop replacement
#save substitutions in new scheme
## >>>>>>> obsolete, called in replace_loop above, only run for single replacements
#6. ######## TODO: ############
#1.and to make the whole thing safe das beste zum schlusz: fehlerroutinen einbauen...
#2.adapt script to changing number of items. now: 16x4=64, maybe: 12x3+16 in the end. 

###############################
# proof_scheme<-function(scheme_mod){
# #initiate seta,setq
#   sroot<-scheme_mod
#   setq<-xml_children(xml_children(xml_children(sroot)))
#   seta<-xml_children(xml_children(xml_children(xml_children(sroot))))
#   set_mod<-c(seta,setq)
# }
#read_xml("qscheme_output.xml")
############################################## works 12062(23.54)
############################################## works 12063(19.34)
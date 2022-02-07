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
#install.packages("xml2")
library(xml2)
#download_xml("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/package_hux2022_fragen_templateB_2022-02-04.xml","template.xml")
#download.file("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/items/GR1/context2022_items_GR01.csv","items.csv")
#scheme<-read_xml("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/package_hux2022_fragen_templateB_2022-02-04.xml")
#items<-read.csv("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/items/GR1/context2022_items_GR01.csv",skip=1)

#########################################################

#local:

#scheme<-read_xml("docs/w3school/qtemplate.xml")
#actualise:

refresh_scheme <- function(ret_file){
#old scheme<-read_xml("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/package_hux2022_fragen_templateB_2022-02-04.xml")
#items<-read.csv("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/items/GR1/context2022_items_GR01.csv",skip=1)
  
#scheme<-read_xml("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/package_hux2022_fragen_templateB_mod_12061.2022-02-04.xml")
#scheme<-read_xml("https://github.com/esteeschwarz/essais/raw/main/docs/hux2022/package_hux2022_fragen_templateB_mod_12061.2022-02-04.xml")
scheme<-read_xml(ret_file)

sroot<-scheme
}
refresh_data <- function(ret_file){
  items<-read.csv("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/items/GR1/context2022_items_GR01.csv",skip=1)
}


#refresh_data(xmlmod)
#setq<-refresh_base_q(schemeset)

refresh_base_q <- function(schemeset){
sroot<-schemeset  
setq<-pos3<-xml_children(xml_children(xml_children(sroot)))
#seta<-pos4<-xml_children(xml_children(xml_children(xml_children(sroot))))
#print(pos5<-xml_children(xml_children(xml_children(xml_children(xml_children(sroot))))))
#print(set_question)
#print(set_answers)
return(setq)
}
refresh_base_a <- function(schemeset){
  sroot<-schemeset  
 # setq<-xml_children(xml_children(xml_children(sroot)))
  seta<-xml_children(xml_children(xml_children(xml_children(sroot))))
  #print(set_question)
  #print(set_answers)
return(seta)
  
  }

#refresh_base()
#now adress items generale
#lines formula in section (element) <items> (antwortoptionen)
#3,5,7,9,11,13 option A1-A6
#1,2 item id and position
#4,6,8,10,12 meta all same
#formel: ax = antwortoption
#######################################################
#############################################

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

#print(get_question(1,4))
####works
#########################
get_item<-function (dataset,item,option){
  items<-dataset
  itx<-item
  
  newopt<-c()
  #newopt[1]<-itx
  #newopt[2]<-itx
  newopt[1]<-items$A1.[itx]
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


#get_item(1,4)
#works

#################
adress_answer<- function (set,itemnr,item_opt) {
pos4<-set
ax<-item_opt #antwortoption A1-A6
lquest<-length(pos4)
item_cpt<-64*14
antwort_adress<-
optionx<-c(3,5,7,9,11,13) #A1-A6
item_adress_last<-(item_cpt/64*itemnr)
item_adress_0<-item_adress_last-14
itempos<-(item_adressx<-item_adress_0+optionx[ax])
out_answer<-(set[itempos])
}
#######################################################
#works as function

#print (a1<-(adress_answer (pos4,2,1)))

###now for parent section (question, kontext) 
######################################################
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
############################################################

#print(adress_question(pos3,4,3))

#works 12062.14.17
##################
#now replace content
#itemdescription

replace_content<-function(dataset,seta,setq,item,pos_a,pos_q){
  
val_text<-"text" #tag, item
val_lead<-"lead" #tag, fragetext
val_descr<-"description" #tag, itemnumber
val_context<-"title" #tag, kontext
val_fail<-"tagfail"

#check flag 1:3
ifelse(pos_q==1,tag<-val_descr,ifelse(pos_q==2,tag<-val_lead,ifelse(pos_q==3,tag<-val_context,ifelse(pos_q!=1:3,tag<-val_fail))))
set_answers<-seta
set_question<-setq
dataset<-dataset

print(tst_aq<-adress_question(set_question,item,pos_q))
print(tst_aa<-adress_answer(set_answers,item,pos_a))
print(tst_rq<-get_question(dataset,item,pos_q))
print(tst_ra<-get_item(dataset,item,pos_a))
#works
#works
#muster: xml_replace(adress_[answer|question](options),val,get_[item|question](options))
#testreplace xml_replace(tst2,val ,tst_r )
####
xml_replace(tst_aq,tag ,tst_rq )
xml_replace(tst_aa,val_text,tst_ra)
####
#works
}

####################
#################
#check replacement

#call data
#refresh_base_a(scheme)

#refresh_base()
#works
########################################### THIS GLOBAL

#init variables
init<- function(set,opt){
  items<-refresh_data(datenset)
  ifelse(set=="mod",scheme<-refresh_scheme(xmlmod),ifelse(set=="old",scheme<-refresh_scheme(xmlorigin),return(items)))
seta<-refresh_base_a(scheme)
setq<-refresh_base_q(scheme)
#ifelse(set=="items",return(items))
ifelse(opt=="a",return(seta),ifelse(opt=="q",return(setq),return(items)))
}

xmlorigin<-("https://github.com/esteeschwarz/essais/raw/main/docs/hux2022/package_hux2022_fragen_templateB_mod_12061.2022-02-04.xml")
xmlmod<-("qscheme_output.xml")

datenset<-("https://github.com/esteeschwarz/12431_hux2021-appendix/raw/12057_VS/hux2022/proverbs/items/GR1/context2022_items_GR01.csv")

#schemeset<-xmlorigin
schemeset<-xmlmod
#seta<-refresh_base_a(schemeset)
#setq<-refresh_base_q(schemeset)

#print(seta)
####
 
items<-init("items",1)
seta<-init("old","a")
setq<-init("old","q")

#sets original
#seta<-init(xmlorigin,2)
#setq<-init(xmlorigin,3)
#sets modified proof
#seta<-init(xmlmod,2)
#setq<-init(xmlmod,3)


proof<-function(items,item,qx,ax){
print(get_question(items,item,qx))
print(get_item(items,item,ax))
print(adress_question(setq,item,qx))
print(adress_answer(seta,item,ax))

}
#####call replacement
#replace_content(items,seta,setq,6,6,3)

#####proof

proof_scheme<-function(scheme_mod){
#initiate seta,setq
  sroot<-scheme_mod
  setq<-xml_children(xml_children(xml_children(sroot)))
  seta<-xml_children(xml_children(xml_children(xml_children(sroot))))
  set_mod<-c(seta,setq)
}
#write_xml(schemeset,"qscheme_output.xml")
#read_xml("qscheme_output.xml")



############################################## works 12062(23.54)
###############################

#5.now loop through the items and replace according to table actualise


#Daten: #r+ &rScript

#library(readr)
#library(stringi)



#srvr<-"https://www.soscisurvey.de/hux2022/?act=fKtaZ3c23D7eVyfBFrzzGT08&rScript"
#srvd<-"https://www.soscisurvey.de/hux2022/?act=4eqU86Oc62EfGZm7PbHOjcmL"
#srvd2<-"https://www.soscisurvey.de/hux2022/?act=4eqU86Oc62EfGZm7PbHOjcmL"
srvx<-"https://www.soscisurvey.de/hux2022/?act=XFHFbeVybOP6o72MZIy0lUlR"
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
as.integer(ds$F407)
#########################################################
a1<-1
 a2<-2
 a3<-3
 a4<-4
 a5<-5
 a6<-6
 a7<-7
######
item_names<-names(ds[20:45])
#eval1<-function(set,qx){
# items 1-26  
#  itemx<-"F4xx"
 # items<-c(1:26)
acp<-cbind(1:26,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
#count A1 in obs
 ct1<-0
 
 #todo
 #arary obs over options
 #u test arrays / observation
 
 tnid<-c(1,2,3) 
 tncpt<-matrix(1:18,3)
tncptr<-matrix(1:18,6)
  tncptr<-tncptr*0
  tncpt<-tncptr
# tn<-2
 rm(tn)
 for (tn in 1:3){
   acp<-cbind(1:26,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
   
   for(k in 1:25){
    
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
    o3<-o1*26+f
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
	  
	   } # end tn loop
tnop_array<-function(tn,acp){
tnopsum1<-sum((acp[1:26,2]))
tnopsum2<-sum((acp[1:26,3]))
tnopsum3<-sum((acp[1:26,4]))
tnopsum4<-sum((acp[1:26,5]))
tnopsum5<-sum((acp[1:26,6]))
tnopsum6<-sum((acp[1:26,7]))

  return(c(tnopsum1,tnopsum2,tnopsum3,tnopsum4,tnopsum5,tnopsum6))
}
print(tncpt)
cbind(tncpt[1:6])
#rm(tnoparray)

# as.integer(ds[[item_names[1][1]]][2])
 #now as function for each option
 #a_options<-c(1:7)
 #a_questions<-c(1:26)
 #a_ainq<-c(c(a_questions(c(a_options)))
#acp<-CrossTable(a_questions,1:7)   
#acp<-matrix(a_questions,nrow = 7)
 ###
#rbind(1:4,c=2,d=3)
#acp<-rbind(1:26,A1=1,A2=2,A3=3,A4=4,A5=5,A6=6,A7=7,deparse.level = 1)
#create empty array per participant



#tabulate(c(2,2,2,1,3,3,3,4,2),6)
# print(ds1)
# #F4<-"F401"
# #cn<-colnames(ds)
# as.integer(ds[[item_names[4]]])
# #xnam<-paste0(1:25)
#print(fmla <- paste("ds$",paste(xnam)))

#########
# e1 <- new.env()
# e1$a <- 10
# e1[["a"]]
# e1[["b"]] <- 20
# e1$b
# ls(e1)
# 

#eval(parse("https://www.soscisurvey.de/hux2022/index.php?act=yHcq99bf4xyBI7rKAs5CALnT&vQuality&useSettings&rScript", encoding="UTF-8"))
#source(srvd)
#eval(parse(srvd2))

#hux2022x <- read_table2(srvd)
#read_tab
#hux2022x$`"F401"`
#print(hux2022x$`"F401"`)
#read_table(srvx,se)

# 
# ### this one after updating srvd
# huxdta<-read_table2(srvd)
# print(huxdta$`"F401"`[1])
# #read_csv(("var_hux.csv"))
# adress_answer<-function(dta,x){
# (qx<-huxdta$`"F402"`[x])
#   itemx<-qx
#   print(itemy<-stri_sub_replace((itemx),2,4,replacement=12))
#   print(itemy)
#   }
#   
#   
# adress_answer(huxdat,1)
# ifelse(huxdta$`"F401"`[1]==1,TRUE,FALSE)
# repl<-c("G","H","I","J","k")
# itema<-"ABCDE"
# stri_sub(itema,1:5)<-repl
# print(itema)
# 
# itemb<-print(as.matrix(stri_sub(itema,1,5))[1:5])
# 
# ############# original script import
# function(x,i,...) {
#   r <- NextMethod("[")
#   mostattributes(r) <- attributes(x)
#   r
# }
# 
# 
# function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x))) 
# {
#   force(nm)
#   nrows <- length(x)
#   if (!(is.null(row.names) || (is.character(row.names) && 
#                                length(row.names) == nrows))) {
#     warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
#                      nrows), domain = NA)
#     row.names <- NULL
#   }
#   if (is.null(row.names)) {
#     if (nrows == 0L) 
#       row.names <- character()
#     else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
#       row.names <- .set_row_names(nrows)
#   }
#   if (!is.null(names(x))) 
#     names(x) <- NULL
#   value <- list(x)
#   if (!optional) 
#     names(value) <- nm
#   structure(value, row.names = row.names, class = "data.frame")
# }
# 
# 
#           file:///private/var/mobile/Containers/Shared/AppGroup/C7BD38E1-A387-4CCB-99B8-616A0C5F32FC/File%20Provider%20Storage/Repositories/essais/docs/hux2022/eval_hux2022_12067.R
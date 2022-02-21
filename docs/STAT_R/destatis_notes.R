

cpt<-X12612_0002_flatcpt
sum(cpt$BEV001__Lebendgeborene__Anzahl,na.rm = TRUE)
stri_detect(cpt,regex="...")
dt6<-stri_replace(cpt,0,regex = "...")
type_convert(cpt,na="...")
typeof(cpt$BEV001__Lebendgeborene__Anzahl)

df <- data.frame(
  x = as.character(runif(10)),
  y = as.character(sample(10)),
  stringsAsFactors = FALSE
)
str(df2)
str(type_convert(df))

df <- data.frame(x = c("...", "10","5"), stringsAsFactors = FALSE)
apply(df,1,sum(str(type_convert(df,na="...")),na.rm=TRUE))
apply(str(type_convert(df,na="...")),2,sum)
df
sum(as.double(df2$x))
typeof(df2)
# Type convert can be used to infer types from an entire dataset

# first read the data as character
data <- read_csv(readr_example("mtcars.csv"),
                 col_types = list(.default = col_character())
)
str(data)
# Then convert it with type_convert
type_convert(data)

X12612_0002_flatcpt <- read_delim("PRO/git/essais/docs/STAT_R/data/12612-0002_flatcpt.csv", 
                                  delim = ";", escape_double = FALSE, na = "...", 
                                  trim_ws = TRUE)

sum(dt5$BEV001__Lebendgeborene__Anzahl,na.rm = TRUE)
typeof(dt5$BEV001__Lebendgeborene__Anzahl)
#################
### get userinput how
# #fun <- function() {
#   ANSWER <- readline("Are you a satisfied R user? ")
#   ## a better version would check the answer less cursorily, and
#   ## perhaps re-prompt
#   if (substr(ANSWER, 1, 1) == "n")
#     cat("This is impossible.  YOU LIED!\n")
#   else
#     cat("I knew it.\n")
# }
# if(interactive()) fun()

fun2 <- function() {
  v1 <- as.matrix(readline("computate: V1 > "))
  v2<- (readline("computate: V2 > "))
  ## a better version would check the answer less cursorily, and
  ## perhaps re-prompt
#  if (substr(ANSWER, 1, 1) == "n")
   # cat("This is impossible.  YOU LIED!\n")
 # else
#    c(v1,v2)
   # print(2*v3)
  print(as.data.frame(v1))
  read.csv(v1,skip=1)
# fun3(as.data.frame(v1))   
    }
if(interactive()) fun2()

fun3<-function(a,b,c,d,e){
  print(a+b+c+d+e)
}


inp()
e1<-data.frame(10,10,10)
e1<-new_list(20)
e3<-c(seq(1,20,2))
(e1)
e3<-
e2<-new_list(10)
e1$a<-e3
sum(e2$a)
data.frame(v1)


#4.mixed model on genesis data
library(lme4)
dt5form<-as.formula(dt5$BEV001__Lebendgeborene__Anzahl~dt5$X2_Auspraegung_Code+(1|dt5$X3_Auspraegung_Code))
lmer(data=dt5,dt5form,na.exclude)
lmer
median(dt5$BEV001__Lebendgeborene__Anzahl,na.rm = TRUE)
na.action()
length(dt5$X2_Merkmal_Code)
length(dt5$X2_Auspraegung_Code)
length(dt5$BEV001__Lebendgeborene__Anzahl)
ncol(dt5)


#20220222(14.27)ST: script von Malte Rosemeyer, 16853W, quantitative corpuslinguistik mit R
#upload for ABV to: https://github.com/esteeschwarz/essais/blob/main/docs/STAT_R/rosemeyer_r_intro.R
################### INTRODUCTION TO R ###################
#mathematical functions
#
1+1
1-1
2*2
4/2
2^2
sqrt(9)
round(2.5634,3)
round(2.5634,digits=3) # same result
?round()
abs(5)
abs(-5)

# variables
sqrt(15)
round(3.872983)

a <- 15 # (vector construction)
a
sqrt(a)
round(sqrt(a))

a = 10
b = 10

a+b
a*b

a

c(1,2,3,4,5,6) # longer vector construction
c(1:6) # same

c("I","went","running","today") # characters
running <- c("I","went","running","today")
running

factor(running)
factor(c("I","went","running","I","today","I"))

length(a)
length(running)

c(1,2,3,4,5,6,7,8,9,10)
c(1:10)
seq(1,10,0.5)
rep(10,5)

namedvector <- c(a=1,b=2,c=3)
namedvector
names(namedvector)
names(namedvector) <- c("d","e","f")
namedvector

a <- c(2,4,6)
b <- c(1,2,3)
a+b
a*b
sum(a)
sum(a,b)

# logical operators
3 == 3
a == b
a != b

3 == 3 | 3 == 4
3 == 3 & 3 == 4

# mathematical operators
4 > 3
3 < 4
3 >= 3
3 >= 2
2 >= 3

# NAs
NAvector <- c(1,3,NA,4,10,NA)
NAvector

is.na(NAvector)
which(is.na(NAvector))

# Identify data types

running
is.vector(running)
is.numeric(b)
typeof(running)

grades <- c(4,1,2,3,3,1,4,2)
grades
mean(grades) # descriptive statistics
median(grades)
grades <- as.factor(grades)
grades
mean(grades) # only works with numerical variables
grades <- as.numeric(grades)
sum(grades)

# accessing elements in a vector

capitals <- c("berlin", "madrid", "warsaw", "berlin", "rome", "rome")
capitals

capitals[5]

which(capitals=="berlin")
capitals[which(capitals=="berlin")]

ages <- sample(x=c(1:60),size=60,replace=T)
ages

which(ages >= 40) # was macht dieser aufruf?????
ages[which(ages >= 40)]


# dataframes

condition <- rep(c(1,2,3),20) #repeat
condition
answer <- sample(x=c("correct","wrong"),size=60,replace=T)
reading.time <- sample(x=c(2000:6000),size=60,replace=T)
ages
sample(x=c(1:100),size=20, replace=T) # create random sample
experiment <- data.frame(condition, answer, reading.time, ages)

# explore the new dataframe
head(experiment)

str(experiment)

nrow(experiment) # number of rows
ncol(experiment) # number of columns

tail(experiment)

# accessing elements in dataframes

experiment[2,3]

experiment[3,]
experiment[,2]

experiment$answer
head(experiment)

experiment$answer[50]

experiment[c(1,5,7,10),]

experiment[which(experiment$ages>30),]
experiment_subset1 <- experiment[which(experiment$ages>30),]

nrow(experiment_subset1) # 32 rows

experiment_subset2 <- experiment[experiment$ages>30 & experiment$answer=="correct",]
experiment_subset2

nrow(experiment_subset2)

# add and delete columns

experiment$pay <- sample(x=c(5:10),size=60,replace=T)
head(experiment)

experiment$older_than_thirty <- ifelse(experiment$ages>30,TRUE,FALSE)
is.logical(experiment$older_than_thirty) # TRUE

experiment[,-6]
experiment <- experiment[,-6]
experiment[-1,]

# sorting and ordering

experiment$reading.time <- as.numeric(experiment$reading.time)
typeof(experiment$reading.time)

order(experiment$reading.time,decreasing = T) # gives the positions in the df, not values!
experiment$reading.time[order(experiment$reading.time,decreasing = F)]

experiment[order(experiment$reading.time,decreasing = T),]


##### exploration of corpus data

# install packages

install.packages("readxl")

library(readxl)

# read xl data into R

read_excel("ra_se_test.xlsx")

x <- read_excel("ra_se_test.xlsx")

View(x)

rase <- as.data.frame(x)

head(rase)

## Orientation in the data

str(rase)

summary(rase)


## dealing with categorical variables

table(rase$Type,rase$SpeakerEducation)

type_by_education <- table(rase$Type,rase$SpeakerEducation)
type_by_education

prop.table(type_by_education)*100

prop.table(type_by_education,2)*100

prop.table(type_by_education,1)*100

round(prop.table(type_by_education,2)*100,1)


mytable <- xtabs(~Type + SpeakerEducation + SpeakerRole,data=rase)
mytable

ftable(mytable)
round(ftable(prop.table(mytable,c(1,2)))*100,1)


## dealing with numerical variables

rase2 <- rase[!is.na(rase$SpeakerAge),]
nrow(rase2)

# central tendencies
mean(rase2$SpeakerAge)
median(rase2$SpeakerAge)
sd(rase2$SpeakerAge)

# tapply()
tapply(rase2$SpeakerAge,rase2$Type,mean)
tapply(rase2$RecentDistance,rase2$Type,mean)


### visualization

rase3 <- rase2[rase2$SpeakerRole=="informant",]
nrow(rase3)

# scatterplots

plot(rase3$RecentDistance,rase3$SpeakerAge)

scatter.smooth(rase3$RecentDistance,rase3$SpeakerAge)

scatter.smooth(rase3$RecentDistance,rase3$SpeakerAge,ylab="Age of speaker in years",xlab="Distance to previous subjunctive",las=1,grid())


## boxplots

tapply(rase2$SpeakerAge,rase2$Type,mean)

boxplot(rase2$RecentDistance~rase2$Type)

boxplot(rase2$RecentDistance~rase2$Type,ylab="Age of speaker in years",xlab="Subjunctive form",las=1,col=c("blue","darkgreen"))


## barplot

type_by_personnumber <- table(rase3$Type,rase3$PersonNumber)

barplot(type_by_personnumber,legend=T)

barplot(type_by_personnumber,legend=T,ylab="Subjunctive form",xlab="Person/Number morphology",las=1,col=c("blue","darkgreen"))


## ggplot2

#install.packages("ggplot2")
library(ggplot2)

ggplot(data=rase3,aes(x=PersonNumber,fill=Type))

ggplot(data=rase3,aes(x=PersonNumber,fill=Type)) + geom_bar()

barplot <- ggplot(data=rase3,aes(x=PersonNumber,fill=Type)) + geom_bar()
barplot

barplot + facet_wrap(~Subcorpus,ncol=2)

# exporting

png("Barplot1.png",width=2000,height=1500,res=300)
  barplot
dev.off()


### basic inferential statistical tests

## correlation

scatter.smooth(rase3$RecentDistance,rase3$SpeakerAge)

cor.test(rase3$RecentDistance,rase3$SpeakerAge)

## t-tests and u-tests (Gries 2009: 211ff.)

boxplot(rase3$SpeakerAge ~ rase3$Type)

# are the data normally distributed?
shapiro.test(rase3$SpeakerAge) # not normally distributed

# homogeneity test
ansari.test(rase3$SpeakerAge~rase3$Type) # variances look homogeneous

# call for a u-test
wilcox.test(rase3$SpeakerAge[rase3$Type=="ra"],rase3$SpeakerAge[rase3$Type=="se"],paired=F,correct=T)

## two categorical variables: x2 tests and fisher tests

ggplot(data=rase3, aes(x= PersonNumber, fill=Type)) + geom_bar() # already nicer than barplot()

table(rase3$PersonNumber,rase3$Type)
chisq.test(table(rase3$PersonNumber,rase3$Type))

# Fisher-Yates exact test
fisher.test(table(rase3$PersonNumber,rase3$Type))


### R as a concordancer

corbacho <- scan("corbacho.txt",what="char",sep=" ",encoding="UTF8")
length(corbacho)
head(corbacho)

# grep()
aventura <- grep("aventura",corbacho)
aventura
corbacho[aventura]

aventura2 <- grep("\\baventura\\b",corbacho)
corbacho[aventura2]

# regular expressions
querer <- grep("\\bqu(ie|e|é)r(o|es|e|emos|éis|en)\\b",corbacho) # quiero, queremos, queréis...
corbacho[querer]

table(corbacho[querer])

# gsub

querer2 <- corbacho[querer]
querer2

querer3 <- gsub("\\?","",querer2)
querer3

corbacho[1:10]
gsub("a","o",corbacho[1:10]) #string replace!!!

# create tidy data from our corbacho data

#install.packages("tidyverse")
library(quanteda)

corbacho2 <- paste(corbacho, collapse=" ") # collapse data into a single texts

corbacho2 <- tokens(corpus(corbacho2))
corbacho2

# simple concordance
aventura <- kwic(x=corbacho2,pattern="aventura",window=4)
aventura

# use regular expressions
querer.regex <- c("\\bqu(ie|e|é)r(o|es|e|emos|éis|en)\\b")

querer_corbacho <- kwic(x = corbacho2, pattern = querer.regex, window=4 , valuetype = "regex") 
querer_corbacho

# multi-word expressions
por_consiguiente <- kwic(x = corbacho2, pattern = phrase("por consiguiente"),window=4)
por_consiguiente


## export to excel

install.packages("writexl")
library(writexl)

write_xlsx(querer_corbacho,"querer_corbacho.xlsx")

x<-c(1:5)
y<-c(101:105)
x<-101
y<-100
z<-cbind(x,y)



#chisq.test(x,y,correct = FALSE)
chisq.test(z,correct = FALSE)

text<-unlist(strsplit("eins zwei drei zwei drei eins zwei", " "))
tx<-readline()
txa<-as.double(unlist(strsplit(tx,",")))
txa[1]+txa[2]
cat("helo",txa)


## From Agresti(2007) p.39
M <- as.table(rbind(c(740, 327, 468), c(484, 239, 477)))
M
dimnames(M) <- list(gender = c("F", "M"),
                    
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
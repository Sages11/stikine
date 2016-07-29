
############################################
#H:\sarah\Projects\Stikine\Reports\adult\2012
#stik2012
#Adult escapement work for Stikine Chinook 
#
#Sarah Power
#
#This file tests for size and sex selectivity using the KS test and chisquared test respectively.
#When sex is only recorded for a subsample of individuals the Student t-test is suggested 
#Will also:
# test for consistency for petersen estimator
# calculated abundance and variance
# estimate ASL and varaivce 
# do all calcs necessary for report
#USES data from 1) kakwan point (mark)  
#               2) Canadian commercial fishery (cancom) 
#               3) spawning sites (vertahl)

#install.packages("knitr")
#install.packages("ggplot2")
#install.packages("lattice")
require(graphics)
#library(ggplot2)
library(plyr)
library(sm)
#write.csv(MyData, file = "MyData.csv")

setwd("H:\\sarah\\Projects\\Stikine\\Reports\\adult\\2012")
#getwd()
#sink("H:\\sarah\\Projects\\Stikine\\Reports\\adult\\2012\\StikAdultMR2012.txt", append=FALSE, split=TRUE)

mark    <- read.csv("stikchinmark12.csv")# marks -1rst event
cancom  <- read.csv("stikchincancom12.csv")# 2nd event canadian commercial fishery
vertahl <- read.csv("stikpoolverthal12.csv")# 2nd event spawning grounds verrett and tahltan weir.

#str(mark, list.len = 200)
#str(cancom, list.len = 200)
#str(vertahl, list.len = 200)
#head(mark)
#Some data to enter by hand
year <- 2012 #year that everything is returning

cat(" <660  harvest here from various fisheries.")
(Nless660H <- 1043 + 39 + 49 +5)
cat(" >660  harvest here from spreadsheets from Canadians")
(Nlh <-6 + 4054 + 64 + 513 + 62 + 467 + 5 + 44 )

#Also need to enter numbers for mark recapture for Large fish and medium fish.
#find censor1 and mcensor1 below for those sections. 

###############################################################################
# Clean and organise data
###############################################################################

# clean data function for numeric variables gets rid of non-numeric characters except for "."
clean <- function(ttt){
	as.numeric( gsub('[^a-zA-Z0-9.]', '', ttt))
}

#function cat works with the sink() function
cat("Cleaned MEF values for the marked fish, canadian commericial fisheries fish and spawning groundsfish")
clean(mark$mef)
clean(cancom$mef)
clean(vertahl$mef)

#create some uniformity change all non valid values to NA

mark$sex    <- as.factor(toupper(mark$sex))
cancom$sex  <- as.factor(toupper(cancom$sex))
vertahl$sex <- as.factor(toupper(vertahl$sex))
recode <- c( "F" = "F","M" = "M")
mark$sex    <- factor(mark$sex ,   levels = recode, labels = names(recode))
cancom$sex  <- factor(cancom$sex,  levels = recode, labels = names(recode))
vertahl$sex <- factor(vertahl$sex, levels = recode, labels = names(recode))

#table(mark$sex)
#table(cancom$sex)
#table(vertahl$sex)

mark$ageeu <- mark$agefw.sw
#recode the ages writen as fw.sw in eu notation. Any age not included below will be recoded as NA.
#ages where either the freshwater or saltwater ages are unknown are not included in the age analysis
# Like brood years are organised together below. 
recode <- c("02" = 0.2, "11" = 1.1,
            "03" = 0.3, "12" = 1.2, "21" = 2.1,
            "04" = 0.4, "13" = 1.3, "22" = 2.2,
            "05" = 0.5, "14" = 1.4, "23" = 2.3,
            "15" = 1.5, "24" = 2.4)

#The line below will only work the first time through. Otherwise you get NAs for all the ages. 
mark$ageeu <- factor(mark$ageeu, levels = recode, labels = names(recode))

#The below recode will ensure that any factor not listed will be recoded as NA. 
# Like brood years are organised together below.
recode <- c("02" = "02", "11" = "11", 
            "03" = "03", "12" = "12", "21" = "21",
            "04" = "04", "13" = "13", "22" = "22",
            "05" = "05", "14" = "14", "23" = "23",
            "15" = "15", "24" = "24")

cancom$ageeu  <- factor(cancom$ageeu,  levels = recode, labels = names(recode))
vertahl$ageeu <- factor(vertahl$ageeu, levels = recode, labels = names(recode))

#table(mark$ageeu)
#sum(table(cancom$ageeu))
#sum(table(vertahl$ageeu))

# Brood year labels

by11 <- year - 3
by12 <- year - 4
by13 <- year - 5
by14 <- year - 6
by15 <- year - 7

mark$broodyear[mark$ageeu == "02" | mark$ageeu == "11" ]                      <- by11
mark$broodyear[mark$ageeu == "03" | mark$ageeu == "12" | mark$ageeu == "21" ] <- by12
mark$broodyear[mark$ageeu == "04" | mark$ageeu == "13" | mark$ageeu == "22" ] <- by13
mark$broodyear[mark$ageeu == "05" | mark$ageeu == "14" | mark$ageeu == "23" ] <- by14
mark$broodyear[                     mark$ageeu == "15" | mark$ageeu == "24" ] <- by15
mark$broodyear <- factor(mark$broodyear, levels=c(by11, by12, by13, by14, by15))


cancom$broodyear[cancom$ageeu == "02" | cancom$ageeu == "11" ]                        <- by11
cancom$broodyear[cancom$ageeu == "03" | cancom$ageeu == "12" | cancom$ageeu == "21" ] <- by12
cancom$broodyear[cancom$ageeu == "04" | cancom$ageeu == "13" | cancom$ageeu == "22" ] <- by13
cancom$broodyear[cancom$ageeu == "05" | cancom$ageeu == "14" | cancom$ageeu == "23" ] <- by14
cancom$broodyear[                       cancom$ageeu == "15" | cancom$ageeu == "24" ] <- by15
cancom$broodyear <- factor(cancom$broodyear, levels=c(by11, by12, by13, by14, by15))

vertahl$broodyear[vertahl$ageeu == "02" | vertahl$ageeu == "11" ]                         <- by11
vertahl$broodyear[vertahl$ageeu == "03" | vertahl$ageeu == "12" | vertahl$ageeu == "21" ] <- by12
vertahl$broodyear[vertahl$ageeu == "04" | vertahl$ageeu == "13" | vertahl$ageeu == "22" ] <- by13
vertahl$broodyear[vertahl$ageeu == "05" | vertahl$ageeu == "14" | vertahl$ageeu == "23" ] <- by14
vertahl$broodyear[                        vertahl$ageeu == "15" | vertahl$ageeu == "24" ] <- by15
vertahl$broodyear <- factor(vertahl$broodyear, levels=c(by11, by12, by13, by14, by15))


#table(mark$broodyear)
#sum(table(cancom$broodyear))
#table(vertahl$broodyear)


vertahl$statwk <- NA #In the future we should have statweek for vertahl
mark$statwk    <- as.factor(mark$statwk)
cancom$statwk  <- as.factor(cancom$statwk)
vertahl$statwk <- as.factor(vertahl$statwk)

vertahl$mark   <- as.factor(toupper(vertahl$secondarymark))
cancom$mark    <- as.factor(toupper(cancom$mark))
mark$recap     <- as.factor(toupper(mark$recap))

#create a dataset that has cancom and vertahl together as a capture set
myvars <- c("statwk", "date", "mef","jorl", "sex", "tag", "source", "ageeu", "mark", "broodyear" )

subcancom  <- cancom[myvars]
subvertahl <- vertahl[myvars]
capture    <- rbind(subcancom, subvertahl)

vertahlnorecap <- subvertahl[which(toupper(subvertahl$mark) != "Y"),]
cancomnorecap  <- subcancom [which(toupper(subcancom$mark)  != "Y"),]
capturenorecap <- rbind(cancomnorecap, vertahlnorecap)

marknorecap    <- mark[which(toupper(mark$recap) != "Y"),]

#create a dataset that has only recaptured data
recap          <- mark[which(toupper(mark$recap) == "Y"),]

mark$group           <- as.factor("mark")
capture$group        <- as.factor("capture")
recap$group          <- as.factor("recap")
marknorecap$group    <- as.factor("marknorecap")
capturenorecap$group <- as.factor("capturenorecap")

#join into one df for ease of Chisquare test.

myvars <- c( "statwk", "date", "mef","jorl", "sex", "tag", "source", "group", "ageeu", "broodyear")

marknorecap    <- marknorecap[myvars]
recap          <- recap[myvars]
capturenorecap <- capturenorecap[myvars]

all <- rbind(marknorecap, recap)
all <- rbind(all, capturenorecap)

cat("KS test testing MEF vlaues from all(>660mm and <660mm) marked and captured fish")
#using large (>660) and (small < 660) combined  ks test is significant.
ks.test(mark$mef, capture$mef)

#create datasets with only L fish.
markL           <- mark           [which(mark$mef           >= 660),]
cancomL         <- cancom         [which(cancom$mef         >= 660),]
vertahlL        <- vertahl        [which(vertahl$mef        >= 660),]
captureL        <- capture        [which(capture$mef        >= 660),]
recapL          <- recap          [which(recap$mef          >= 660),]
marknorecapL    <- marknorecap    [which(marknorecap$mef    >= 660),]
capturenorecapL <- capturenorecap [which(capturenorecap$mef >= 660),]
allL            <- all            [which(all$mef            >= 660),]

#create datasets with only m fish.
markm           <- mark           [which(mark$mef           < 660),]
cancomm         <- cancom         [which(cancom$mef         < 660),]
vertahlm        <- vertahl        [which(vertahl$mef        < 660),]
capturem        <- capture        [which(capture$mef        < 660),]
recapm          <- recap          [which(recap$mef          < 660),]
marknorecapm    <- marknorecap    [which(marknorecap$mef    < 660),]
capturenorecapm <- capturenorecap [which(capturenorecap$mef < 660),]
allm            <- all            [which(all$mef            < 660),]


###############################################################################
#Tests for fish in the large Mark recapture
###############################################################################
##############################################################################

##############################################################################
cat("Chi squared to determine if it is appropriate to combine cancom and vertahl 
 capture/recapture data in report based on marked to unmarked ratios.
 Use Fisher test if any expected values are less than 5.")
##############################################################################
tc <-table(cancomL$mark)
tv <-table(vertahlL$mark)
(tv <-cbind( sum(tv)- tv["Y"] , tv["Y"])) 
# above line is since not all non recaptures (marks) are recorded as "N"
tcv <- rbind(tc, tv)
chisq.test(tcv)
cat("chi squared test expected values")
round(chisq.test(tcv)$expected, 2)
#Since expected values on chisquared are less than 5 use the fisher exact test
fisher.test(tcv) # p = .1042 FTR Ho:No difference in marked to unmarked ratios 

##############################################################################
cat("Chisquared of age 1.3 and 1.4s to determine if it is appropriate to 
     combine age compositions from Tahltan and Verrett")
##############################################################################
tbl <- table(vertahl$source, vertahl$ageeu)
(tbl <-cbind(tbl[,"13"], tbl[,"14"]))
chisq.test(tbl) # p value 0.05768 Fail to reject the null hypothesis. HO: Complete mixing occurs. 
cat("chi squared test expected values")
round(chisq.test(tbl)$expected,2)


##############################################################################
cat("test for equality between cancom and vertahl MEF also see graphs")
##############################################################################
ks.test(cancomL$mef, vertahlL$mef, ) # p = 0.001875 
#Graphs show no difference small p result of large sample size that is able to detect small differences.
plot(ecdf(vertahlL$mef), 
     main="Cumulative relative frequency lower river Canadian Fishery and Spawning Grounds", 
     xlab="Length (mm)", ylab="Cumulative relative frequency", pch=19) #
plot(ecdf(cancomL$mef), col = "red",  add = TRUE, lty = "dashed")
#combine cancom and vertahl into capture dataframe to use for testing selectivity

###########################################################################################
#Various (3) tests for equality of length that Reject HO between marking and spawning sites.
# Since we are combining all second event sites (fishery and spawning sites) then perhaps it doesn't matter.
#t.test(markL$mef, vertahlL$mef, )
#wilcox.test(markL$mef, vertahlL$mef, )
#ks.test(markL$mef, vertahlL$mef, ) #p = 0.01318
#Graphs show no difference small p result of large sample size that is able to detect small differences.
#look at plots of cdf
plot(ecdf(markL$mef), main="Cumulative relative frequency Marked and Captured", 
     xlab="Length (mm) ", ylab="Cumulative relative frequency", pch=19)
plot(ecdf(vertahlL$mef), col = "blue", add = TRUE, lty = "dashed")
plot(ecdf(cancomL$mef), col = "red",  add = TRUE, lty = "dashed")
plot(ecdf(captureL$mef), col = "purple", add = TRUE, lty = "dashed")
###########################################################################################

###########################################################################################
cat("tests for size selectivity: Graphs and tests Appendix B in op plan")
###########################################################################################

#look at plots of cdf together:
plot(ecdf(markL$mef), 
     main="Cumulative relative frequency lower river Canadian Fishery and Spawning Grounds", 
     xlab="Length (mm)" , ylab="Cumulative relative frequency", pch=19)
plot(ecdf(vertahlL$mef),  col = "blue",   add = TRUE, lty = "dashed")
plot(ecdf(cancomL$mef),   col = "red",    add = TRUE, lty = "dashed")
plot(ecdf(captureL$mef),  col = "purple", add = TRUE, lty = "dashed")

###########################################################################################
cat("cdf Mark vs Recapture   figure in report")
###########################################################################################
plot(ecdf(markL$mef), main="Cumulative relative frequency Marked and Recaptured", 
     xlab="Length (mm) ", ylab="Cumulative relative frequency", pch=19)
plot(ecdf(recapL$mef), col = "purple", add = TRUE, lty = "dashed")
legend("bottomright", inset=.05, ,
       c("Marked","Recaptured"), fill=c("black","purple"), horiz=FALSE)

###########################################################################################
cat("cdf Capture vs Recapture   figure in report")
###########################################################################################
plot(ecdf(captureL$mef), main="Cumulative relative frequency Captured and Recaptured ", 
     xlab="Length (mm) ", ylab="Cumulative relative frequency", pch=19)
plot(ecdf(recapL$mef), col = "purple", add = TRUE, lty = "dashed")
legend("bottomright", inset=.05, ,
       c("Captured","Recaptured"), fill=c("black","purple"), horiz=FALSE)

###########################################################################################
cat("cdf Mark vs Capture   figure in report")
###########################################################################################
plot(ecdf(markL$mef), main="Cumulative relative frequency Marked and Captured", 
     xlab="Length (mm) ", ylab="Cumulative relative frequency", pch=19)
plot(ecdf(captureL$mef), col = "purple", add = TRUE, lty = "dashed")
legend("bottomright", inset=.05, ,
       c("Marked","Captured"), fill=c("black","purple"), horiz=FALSE)

###########################################################################################
cat("test for size selectivey: appendix B1 in op plan")
###########################################################################################
cat("test for size selectivey: Mark vs Recapture")
ks.test(markL$mef, recapL$mef, )    #uses all recap data regardless of where recaptured #p-value = 0.8804
cat("test for size selectivey: Capture vs Recapture")
ks.test(captureL$mef, recapL$mef, )  #uses all recap data regardless of where recaptured #p-value = 0.976
cat("test for size selectivey: Mark vs Capture")
ks.test(markL$mef, captureL$mef, ) #p-value = 0.3987
# Result: Fail to reject Ho in all 3 cases therefore there is not size selectivity during either sampling event. 

#Other tests for exploration:
#ks.test(markL$mef, cancomL$mef)  # p = 0.8095
#ks.test(markL$mef, vertahlL$mef) # p = 0.01318 Statistically significant but their cdf graphs appear similar enough to me. 


###########################################################################################
cat("test for sex selectivity appendix B1 in op plan")
###########################################################################################
(tbl <-table(allL$group, allL$sex))
#There is one fish in the cancom fishery that was not assigned a sex. remove from chisquared test.
(tbl1 <-cbind(tbl[,"F"], tbl[,"M"]))

chisq.test(tbl)
chisq.test(tbl1)
# Result: Fail to reject Ho. Ho: There is no sex selectivity in either sampling event. 

###########################################################################################
cat("Test for complete mixing for size in Larges (Test III in Appendix B2 of (2015) op plan)
    Recapture vs statweek")
###########################################################################################
# Note time periods are biweekly except for the first three weeks (Biweeks 18-20) which were combined together. 
# They were done on a biweekly basis otherwise sample sizes are too small to run a chi-squared test.
# Result: Fail to reject the null hypothesis that complete mixing occurs. 

(tbl <-table(markL$recap, markL$statwk)) #recap is yes/no
(ctbl = cbind(tbl[,"18"] + tbl[,"19"] + tbl[,"20"], tbl[,"21"] + tbl[,"22"], tbl[,"23"] + tbl[,"24"], tbl[,"25"]+ tbl[,"26"], tbl[,"27"] + tbl[,"28"]) )
chisq.test(ctbl) # p value 0.06654 Fail to reject the null hypothesis that complete mixing occurs. 
cat("chi squared test expected values")
round(chisq.test(ctbl)$expected,2)

#ctbl1 = cbind((tbl[,"18"] + tbl[,"19"], tbl[,"20"], tbl[,"21"], tbl[,"22"], tbl[,"23"], tbl[,"24"], tbl[,"25"], tbl[,"26"], tbl[,"27"] + tbl[,"28"]))
#ctbl1
#chisq.test(ctbl1)
#chisq.test(ctbl1)$expected

###########################################################################################
cat("Appendix B2 test II Equal proportions test")
###########################################################################################
(tblr <- table(recapL$statwk))
(tblc <- table(capturenorecapL$statwk))

(r <- cbind(tblr["18"] + tblr["19"] + tblr["20"], tblr["21"] + tblr["22"], tblr["23"] + tblr["24"], tblr["25"]+ tblr["26"], tblr["27"]+ tblr["28"]) )
(c <- cbind(tblc["19"] + tblc["20"], tblc["21"] + tblc["22"], tblc["23"] + tblc["24"], tblc["25"]+ tblc["26"], tblc["27"]+ tblc["28"] + tblc["29"]+ tblc["30"]+tblc["31"]+ tblc["32"])  ) 

(forchisq <- rbind(r,c))
chisq.test(forchisq)   # p value 0.06097 Fail to Reject the null hypothesis that comelpete mixing occurs. 
cat("chi squared test expected values")
round(chisq.test(forchisq)$expected, 2)

###########################################################################################
cat("Appendix B2 test I : Mixing Test. Can't complete since we don't have dates from Tahltan Weir
 and we sample infrequently on the Verrette River. As long as test II or III pass this isn't an issue.")
###########################################################################################

#Result of Appendix B tests
#Fail to reject Ho That we have equal porportions tags and that we have complete mixing.

###############################################################################
cat("Bootstrap Large data for variance and 95% CI of abundance estimate")
###############################################################################

###########################################################################################
# Mark recapture for large does not need to be statified as per the above hypothesis tests.
#
# Parametric Bootstrap
# Estimate the SE and 95% Confidence intervals via percentiles for the in river run. 

# Table 5.-Capture histories for Chinook salmon in the Stikine River mark-recapture experiment, 2015.
#1.  Marked but censored in U.S. marine recreational fishery.
#2.  Marked but censored in U.S. marine commercial fisheries.
#3.  Marked but censored in Andrew and North Arm creeks.
#4.  Marked and not sampled on spawning grounds or inriver fisheries.
#5.  Marked and recaptured on spawning grounds or inriver fisheries.
#6.  Not marked but captured on spawning grounds and inriver fisheries.
#7.  Not marked and not sampled on spawning grounds and inriver fisheries.

reps <- 10000 # number of bootstraps you want in crease to 10000 when running for a report. 
#create datasets with only L fish.

#Enter the numbers that fit into the table 5 catagories 1-6: (The 7th cagaory is calculated.)
censor1 <- 2
censor2 <- 3
censor3 <- 0
nonrecap4 <- 309
recap5 <- 71
capturenotmarked6 <- 5133

mhat <- recap5 + nonrecap4
C    <- recap5 + capturenotmarked6
Nlr <- (mhat + 1)*(C + 1)/(recap5 + 1)-1 # number of Large (>600mm) in river run. 

unseen7 <- Nlr -sum(nonrecap4, recap5, capturenotmarked6)

#
n = sum(censor1,censor2,censor3, nonrecap4, recap5, capturenotmarked6, unseen7) # Estimated Large in river returns N^_LR in op plan, including those that back out. 
k = 7      # Number of catagories in capture history
p = c(censor1/n, censor2/n, censor3/n, nonrecap4/n, recap5/n, capturenotmarked6/n, unseen7/n) #Estimated probabibility of those capture histories = (est # ofparticular capture histories)/n
abundance_estimates = rep(0,reps) #theta is the parameter of interest in this case in river returns N^_LR,  also number of replicates is reps

for (i in 1:reps) {
  assignments <- sample(1:k, n, prob = p, replace = TRUE)
  catagory <- tabulate(assignments, k)
  
  # use MLE to estimate theta^hat = (M+1)*(C+1)/(R+1) - 1
  abundanceest = (catagory[4] + catagory[5]+1)*(catagory[5]+catagory[6]+1)/(catagory[5]+1)-1
  abundance_estimates[i] = abundanceest
}

###########################################################################################
cat("The estimate of large in-river based on the censored mark recapture")
###########################################################################################
Nlr

###########################################################################################
cat("print the estimated standard error of N^_lr ")
###########################################################################################
(seNlr  <- sd(abundance_estimates))
varNlr <- (seNlr)^2
var(abundance_estimates)

cat("Show sampling distribution of estimated mle abundance estimates") 
###############Should add a title to the graph
hist(abundance_estimates)

# mean(thetas) # you can look at this to see how it compares to your estimate compares to the bootstrapped mean 
#but dont use it as the est in your report.

###########################################################################################
cat("Bias and percent of bias which is reported in report") 
(bias <- Nlr - mean(abundance_estimates))
(percentbias <- bias/mean(abundance_estimates))
###########################################################################################

###########################################################################################
cat("Confidence intervals via percentiles expect this to be asymmetric")
round(quantile(abundance_estimates, c(.05, .95)),0)
round(quantile(abundance_estimates, c(.025, .975)),0)
###########################################################################################

###############################################################################
cat("Tests for fish in the medium Mark recapture")
###############################################################################

#Various (3) tests for equality that fail to reject HO between marking and spawning sites.
t.test(markm$mef, vertahlm$mef, )      # p-value = 0.6967
wilcox.test(markm$mef, vertahlm$mef, ) # p-value = 0.7328
ks.test(markm$mef, vertahlm$mef, )     # p-value = 0.3127

cat("test for size selectivity: Mark vs Capture")
ks.test(markm$mef, cancomm$mef)  # p-value = 1.578e-05 #definite size selectivity in the cancom fishery
ks.test(markm$mef, vertahlm$mef) # p-value = 0.3127 
#combined all captures")
ks.test(markm$mef, capturem$mef) # p-value = 3.558e-05

cat("look at plots of cdf")
plot(ecdf(markm$mef))
plot(ecdf(vertahlm$mef),  col = "blue",   add = TRUE, lty = "dashed")
plot(ecdf(cancomm$mef),   col = "red",    add = TRUE, lty = "dashed")
plot(ecdf(capturem$mef),  col = "purple", add = TRUE, lty = "dashed")
#Size selectivity can also be seen here. 

cat("look at plots of cdf")
plot(ecdf(markm$mef))
plot(ecdf(capturem$mef), col = "purple", add = TRUE, lty = "dashed")

###############################################################################
cat("test for size selectivey: appendix B1 in op plan")
###############################################################################

#first select out the recapture lengths. 
#recapm <- mark[which(mark$daterecovered != NA),]
recapm <- mark[which(toupper(mark$recap) == "Y" & mark$mef < 660),]
recapm$daterecovered
cat("test for size selectivey: Mark vs Recapture")
ks.test(markm$mef, recapm$mef, )    #p-value = 0.6119 uses all recap data regardless of where recaptured 
cat("test for size selectivey: Capture vs Recapture")
ks.test(capturem$mef, recapm$mef, )  #p-value = 0.1102 uses all recap data regardless of where recaptured 
cat("test for size selectivey: Mark vs Capture")
ks.test(markm$mef, capturem$mef, ) #p-value = 3.558e-05
# Result: Fail to reject Ho in all 2 of 3 cases. Reject Ho in MvsC  Therefore case V in op plan

(tbl <-table(allm$group))

#mark   recap capture 
#30       5     356
#Considering sample sizes and powers of tests biometrician (Sarah) beleives there was size selectivity
#in both events so don't use MR instead use proportional method outlined late. 

(tbl <-table(allm$group, allm$sex))
#There is one fish in the cancom fishery that was not assigned a sex. remove from chisquared test.
(tbl1 <-cbind(tbl[,"F"], tbl[,"M"]))

###############################################################################
cat("test for sex selectivity appendix B1")
###############################################################################
chisq.test(tbl1)
# Result: Since expected values are less than 5 in 3 of 6 catagories disreguard test.
cat("chi squared test expected values")
round(chisq.test(tbl1)$expected,2)

sort(unique(mark$daterecovered))

###############################################################################
cat(" Test for complete mixing (Test III in Appendix B2 of (2015) op plan)")
# Note time periods are biweekly but still too small to run a chisquared.
# Result: Not enough recoveries to do a chisquared. 
###############################################################################

markmmixing <- mark[which(mark$mef < 660),]
(tbl <-table(markmmixing$recap, markmmixing$statwk))
(ctbl = cbind(tbl[,"19"] , tbl[,"21"] , tbl[,"23"] + tbl[,"24"], tbl[,"25"]+ tbl[,"26"], tbl[,"27"] + tbl[,"28"]) )
chisq.test(ctbl) #
cat("chi squared test expected values")
round(chisq.test(ctbl)$expected,2)


(tblr <- table(recapm$sex, recapm$statwk))
(tblc <- table(cancomm$sex, cancomm$statwk))

#(newtblr = cbind(tblr[,"18"] + tblr[,"19"] + tblr[,"20"], tblr[,"21"] + tblr[,"22"], tblr[,"23"] + tblr[,"24"], tblr[,"25"]+ tblr[,"26"], tblr[,"27"]+ tblr[,"28"]) )
#names <- c("18-20", "21-22", "23-24","25-26","27-32")
#(rm <- newtblr[1,])
#(rf <- newtblr[2,])
#(r <- rf+rm)
#(newtblc = cbind(tblc[,"19"] + tblc[,"20"], tblc[,"21"] + tblc[,"22"], tblc[,"23"] + tblc[,"24"], tblc[,"25"]+ tblc[,"26"], tblc[,"27"]+ tblc[,"28"] + tblc[,"29"]+ tblc[,"30"]+tblc[,"31"]+ tblc[,"32"])  ) 
#(cm <- newtblc[1,])
#(cf <- newtblc[2,])
#(c  <- cm + cf)

cat("Appedix B2 test II Equal proportions test # not enough recaptures to do this test")
(forchisq <- rbind(r,c))
chisq.test(forchisq)   #
cat("chi squared test expected values")
round(chisq.test(forchisq)$expected, 2)
###############################################################################

###############################################################################
cat("Appendix B2 test I : Mixing Test. Can not complete since we do not have dates from Tahltan Weir
 and we sample infrequently on theVerrette River. As long as test II or III pass this is not an issue.
 since samples sizes were too small we could not do diagnostic tests to determine if a 
 Perterson MR was appropriate. Instead use proportional method.
")
###############################################################################
cat("Bootstrap of medium data for variance and 95% CI of abundance estimate")
###############################################################################
#####################################################################################################
#Since it was likely that size selectivity occured, and samples are too small to 
#further stratify then use the proporitonal method to estimate # of mediums. 
#Don't use mabundance from below or its variance and CI.
#test for <660 fish if estimated relative precision >50% for a 95% CI then don't use this estimation 
mreps <- 100 # number of bootstraps you want

mcensor1 <- 0
mcensor2 <- 0
mcensor3 <- 0
mnonrecap4 <- 24
mrecap5 <- 5
mcapturenotmarked6 <- 24

mmhat <- mrecap5 + mnonrecap4
mC    <- mrecap5 + mcapturenotmarked6
mestN <- (mhat + 1)*(mC + 1)/(mrecap5 + 1)-1

munseen7 <- mestN -sum(mnonrecap4, mrecap5, mcapturenotmarked6)

# Estimated Large in river returns N^_LR in op plan, including those that back out. 
mn = sum(mcensor1,mcensor2,mcensor3, mnonrecap4, mrecap5, mcapturenotmarked6, munseen7) 
mk = 7      # Number of catagories in capture history
#Estimated probabibility of those capture histories = (est # ofparticular capture histories)/n
mp = c(mcensor1/mn, mcensor2/mn, mcensor3/mn, mnonrecap4/mn, mrecap5/mn, mcapturenotmarked6/mn, munseen7/mn) 
#theta is the parameter of interest in this case in river returns N^_LR,  also number of replicates is reps
mabundance_estimates = rep(0,mreps) 

for (i in 1:mreps) {
  massignments <- sample(1:mk, mn, prob = mp, replace = TRUE)
  mcatagory <- tabulate(massignments, mk)
  
  # use MLE to estimate theta^hat = (M+1)*(C+1)/(R+1) - 1
  mabundanceest = (mcatagory[4] + mcatagory[5]+1)*(mcatagory[5]+mcatagory[6]+1)/(mcatagory[5]+1)-1
  mabundance_estimates[i] = mabundanceest
}

cat("The estimate of medium (<660) in river based on the censored mark recapture")
mestN

cat(" print the estimated standard error of N^_mr")# (which is the sd of the sample)
sd(mabundance_estimates)
cat("CV of abundance estimates")
sd(mabundance_estimates)/mestN

cat("show sampling distribution of estimated mle theta")
hist(mabundance_estimates)

# mean(thetas) # you can look at this to see how it compares to your estimate compares to the bootstrapped mean 
#but dont use it as the est in your report.

###########################################################################################
cat("Bias and percent of bias which is reported in report") 
(bias <- Nlr - mean(mabundance_estimates))
(percentbias <- bias/mean(mabundance_estimates))
###########################################################################################

###########################################################################################
cat("Confidence intervals for mediums via percentiles.  Expect this to be asymmetric
 Only use if MR assumptions hold and estimated relative presion is <50% for a 95% CI")
round(quantile(mabundance_estimates, c(.05, .95)),0)
round(quantile(mabundance_estimates, c(.025, .975)),0)
###########################################################################################


###############################################################################
cat("Estimates of abundance when a Mark Recapture cannot be done
 Use this if MR assumptions are violated or estimated relative presion is >50% for a 95% CI")
###############################################################################

# first catagorise large and small based on what was found on spawning grounds
vertahl$cat660[vertahl$mef>=660] <- "L" #1 #
vertahl$cat660[vertahl$mef< 660] <- "M" #2 #
vertahl$cat660 <-as.factor(vertahl$cat660)
t<-as.matrix(table(vertahl$cat660))

cat("Calculate proporitons of less than and greater than 660 on spawning grounds")
(less660p     <- as.double(t["M",]/sum(t)))
(greater660p  <- as.double(t["L",]/sum(t)))# Used in equations 4-6 and others in 2015 op plan

cat("Calculate proportions of greater than and less than 660 in Canadian commercial fishery")
cancom$cat660[cancom$mef>=660] <- "L" #1 #
cancom$cat660[cancom$mef< 660] <- "M" #2 #
cancom$cat660 <-as.factor(cancom$cat660)
t2 <- as.matrix(table(cancom$cat660))
cat("calculate proporitons of greater than and less than 660 in Canadian commercial fishery")
(less660ph     <- as.double((t2["M",]/sum(t2))))
(greater660ph  <- as.double(t2["L",]/sum(t2))) 

###############################################################################

###############################################################################

 cat("Equation 4 in 2015 op plan: estimate of the escapement of fish less than 660 ")
(Nless660E <-Nlr*(1/greater660p -1))
 cat("Number of samples from spawning grounds")
(sampnum <- nrow(vertahl))

 cat("Equation 6 in 2015 op plan used to calculate equation 5")
(var1overple <- (1/greater660p)^4*greater660p*(1-greater660p)/(sampnum -1)) #(equation 6 in 2015 opplan)

 cat("Equation 5 in 2015 opplan: variance of the less than 660 abundance estimate and the SE")
(varNless660E <- var(abundance_estimates)*(1/greater660p-1)^2 + (Nlr)^2*var1overple - var1overple*var(abundance_estimates))
(SENless660E    <- sqrt(varNless660E) )
 cat("Equation 7 in 2015 op plan")
(Nless660R <- Nless660E + Nless660H)
 cat("Equation 8 in 2015 op plan")
(varNless660R <-varNless660E ) # Var of Nless660H is negliable 
(SENless660R  <- sqrt(varNless660R) )


 cat("Equation 9 in 2015 op plan total inriver run at Kakwan Point")
(Nr <-Nlr + Nless660R )
###############################################################################

###############################################################################
 cat("Equation 1 in 2015 op plan")
(Nle <- Nlr-Nlh )

 cat("Equaton 3 in the 2015 op plan")
(varNle <- varNlr )

 cat("Equation 10 in 2015 op plan var of Nr ")
(varNr <- varNlr*(1/greater660p)^2 +(Nle)^2*var1overple - var1overple*varNlr )

 cat("Equation 11 in 2015 op plan total spawning escapement")
(Ne <- Nless660E + Nle )

 cat("Equation 12 in the 2015 op plan variance of spawning escapement")
(varNe <- varNr)
(seNe  <- sqrt(varNe))

#For report ... could be put in op plan
 cat("Escapement estimate of all Chinook medium & large with semmetric CI")
(lower95CINe  <- Ne-qnorm(.975)*seNe)
(upper95CINe  <- Ne+qnorm(.975)*seNe)

###############################################################################
 cat("Used in equation 13 then number of large fish passing thorugh the Tahltan weir")
Wl <-574
 cat("Equaton 13 in the 2015 op plan expansion factor")
(pihat <-Nle /Wl)
(varpihat <- varNle/(Wl^2))

#################################################
###############################################################################
 cat("Age and Sex composition of cancom harvest and mean length at age 
     See final summaries in tables")
###############################################################################

###############################################################################
#total number of fish in the MEDIUM cancom fishery:
###############################################################################
 
#number of fish of each eu age  and gender in the MEDIUM cancom fishery:
tm <- table(cancomm$sex, cancomm$ageeu)
n  <- sum(tm)
tm <-addmargins(tm,1)
tm <- cbind(tm, Total = rowSums(tm))

#equation 15 in 2015 op plan percent of fish of each eu age in the MEDIUM cancom fishery:
pm <- tm/n
# Equation 26 in 2015 op plan variance of fish of each eu age in the MEDIUM cancom fishery:
#could use finite population correction factor:
fpc <-(Nless660H - n)/(Nless660H)
varpmjH <- fpc*pm*(1-pm)/(n-1)#multiply by fpc
SEpmjH <- sqrt(varpmjH)
#part of equation 17 in 2015 op plan perataining to medium sized fish
#number of fish of each age in the MEDIUM cancom fishery:
NmjH <- pm * Nless660H
#part of equation 18 in 2015 op plan
varNmjH <- varpmjH * Nless660H^2 
SENmjH <- sqrt(varpmjH * Nless660H^2)

###############################################################################
#Large cancom Fishery
###############################################################################

#number of fish of each eu age and gender in the LARGE cancom fishery:
tL <-table( cancomL$sex, cancomL$ageeu)
n  <- sum(tL)#total number of fish in the LARGE cancom fishery:
tL <-addmargins(tL,1)
tL <- cbind(tL, Total = rowSums(tL))

#equation 15 in 2015 op plan percent of fish of each eu age in the LARGE cancom fishery:
pL <- tL/n
# Equation 26 in 2015 op plan variance of fish of each eu age in the LARGE cancom fishery:
fpc <- (Nlh - n)/(Nlh)
varpLjH <- fpc*pL*(1-pL)/(n-1)
SEpLjH <- sqrt(varpLjH)

#part of equation 17 in 2015 op plan pertaining to large aged fish 
#could add finite population correction factor. 

#number of fish of each age in the LARGE cancom fishery:
NLjH <- pL * Nlh 
#part of equation 18 in 2015 op plan
varNLjH <- varpLjH * Nlh^2 
SENLjH <- sqrt(varpLjH * Nlh^2) 


###############################################################################
# (small) medium and large combined
###############################################################################

#part of equation 17 in 2015 op plan combining large (L) & medium(m)
(NjH <- NLjH + NmjH) # est number of salmon of various age-sexes with sizes combined
#part of equation 18 in 2015 op plan
varNjH <-varNLjH + varNmjH 
(SENjH <- sqrt(varNjH))

n <-sum(NjH)/4
pNjH <- NjH/n #proportions of all salmon of various age-sexes with sizes combined
Nh <- Nlh + Nless660H
nh <- sum(table(cancom$sex, cancom$ageeu))
fpc <- (Nh-nh)/Nh
varpNjH <- fpc*pNjH * (1-pNjH) / (n-1)
(SEpNjH <- sqrt(varpNjH))

tb <- tm + tL

###############################################################################
# (small) medium and large separate percent of total
###############################################################################

#number of fish of each eu age  and gender in the MEDIUM cancom fishery:
ta <- table(cancom$sex, cancom$ageeu, cancom$cat660)
n  <- sum(ta)
(ta <-addmargins(ta,1))
(ta <-addmargins(ta,2))

#equation 15 in 2015 op plan percent of fish of each eu age in the cancom fishery:
(pa <- ta/n)
# Equation 26 in 2015 op plan variance of fish of each eu age in the LARGE cancom fishery:
(varpijH <- fpc*pa*(1-pa)/(n-1))
(SEpijH <- sqrt(varpijH))

###############################################################################
# Function to display abundances
###############################################################################

displayabundance<- function(t, p, sep, z, sez ){
  #females
  fn         <- round(t[1,], digits = 1)
  fpercent   <- round( p[1,], digits = 3) 
  fSEpercent <- round( sep[1,], digits = 3)
  fHarvest   <- round( z[1,], digits = 0)
  fSEHarvest <- round( sez[1,], digits = 0)
  females    <- rbind(fn, fpercent, fSEpercent, fHarvest, fSEHarvest)
  #males
  mn         <- t[2,]
  mpercent   <- round( p[2,], digits = 3) 
  mSEpercent <- round( sep[2,], digits = 3)
  mHarvest   <- round( z[2,], digits = 0)
  mSEHarvest <- round( sez[2,], digits = 0)
  males <- rbind(mn, mpercent, mSEpercent, mHarvest, mSEHarvest)
  #combined genders
  cn         <- t[3,]
  cpercent   <- round( p[3,], digits = 3) 
  cSEpercent <- round( sep[3,], digits = 3)
  cHarvest   <- round( z[3,], digits = 0)
  cSEHarvest <- round( sez[3,], digits = 0)
  combined   <- rbind(cn, cpercent, cSEpercent, cHarvest, cSEHarvest) 
  report     <- rbind(females, males, combined)
  return(report)
}

#Appendix currently not in report. C2 in excel sheet 
cat("Small and medium Chinook  salmon (< 660mm) Harvest")
Hmasl <- displayabundance(tm, pm, SEpmjH, NmjH, SENmjH )

cat("Large            Chinook salmon  (>=660 mm MEF) Harvest")
HLasl <- displayabundance(tL, pL, SEpLjH, NLjH, SENLjH )

cat("Small, medium and Large Chinook salmon combined Harvest")
Haasl <- displayabundance(tb, pNjH, SEpNjH, NjH, SENjH)

cat("Small and medium Chinook  salmon (< 660mm)     Harvest percent of total")
Hmoasl <- displayabundance(ta[,,2], pa[,,2], SEpijH[,,2], NmjH, SENmjH )

cat("Large             Chinook salmon (>=660 mm MEF) Harvest percent of total")
HLoasl <- displayabundance(ta[,,1], pa[,,1], SEpijH[,,1], NLjH, SENLjH )

#cat("Small, medium and Large Chinook salmon combined Harvest # for in river run.")
#(harvestestall <-displayabundance(tb, pNjH, SEpNjH, NjH, SENjH))

#C2
cat("Estimated age/gender/size composition by size of Chinook  salmon  Harvest")
(harvestseparated <- rbind(Hmasl, HLasl, Haasl))

cat("Estimated age/gender/size composition overall of Chinook  salmon  Harvest")
(harvestoverall <- rbind(Hmoasl, HLoasl, Haasl))

hagecompall    <-Haasl[14,]
varhagecompall <-Haasl[15,]*Haasl[15,]

###############################################################################
cat("Brood year and Sex composition of cancom harvest and mean length at brood year age")
###############################################################################

#total number of fish in the MEDIUM cancom fishery:

#number of fish of each broodyear  and gender in the MEDIUM cancom fishery:
#tmby <- table(cancomm$sex, cancomm$broodyear, factor(broodyear, levels=c(by11, by12, by13, by14, by15)))
tmby <-table(cancomm$sex, cancomm$broodyear)
n  <- sum(tmby)
(tmby <-addmargins(tmby,1))
tmby <- cbind(tmby, Total = rowSums(tmby))

#equation 15 in 2015 op pLbyan percent of fish of each broodyear in the MEDIUM cancom fishery:
(pmby <- tmby/n)
# Equation 26 in 2015 op plan variance of fish of each broodyear in the MEDIUM cancom fishery:
#could use finite population correction factor:
fpc <- (Nless660H - n)/(Nless660H -1)
(varpmbyjH <- fpc*pmby*(1-pmby)/(n-1))
(SEpmbyjH <- sqrt(varpmbyjH))
#part of equation 17 in 2015 op plan perataining to medium sized fish
#number of fish of each age in the MEDIUM cancom fishery:
(NmbyjH <- pmby * Nless660H)
#part of equation 18 in 2015 op plan
(varNmbyjH <- varpmbyjH * Nless660H^2 )
(SENmbyjH <- sqrt(varpmbyjH * Nless660H^2))

###############################################################################
#Large cancom Fishery
###############################################################################

#, col.vars = list(by11, by12, by13, by14, by15)
#number of fish of each broodyear and gender in the LARGE cancom fishery:
(tLby <-table( cancomL$sex, cancomL$broodyear))
n  <- sum(tLby)#total number of fish in the LARGE cancom fishery:
(tLby <-addmargins(tLby,1))
(tLby <- cbind(tLby, Total = rowSums(tLby)))

#equation 15 in 2015 op plan percent of fish of each broodyear in the LARGE cancom fishery:
(pLby <- tLby/n)
# Equation 26 in 2015 op plan variance of fish of each broodyear in the LARGE cancom fishery:
fpc <- (Nlh-n)/Nlh
(varpLbyjH <- fpc*pLby*(1-pLby)/(n-1))
(SEpLbyjH <- sqrt(varpLbyjH))

#part of equation 17 in 2015 op plan pertaining to large aged fish 
#could add finite population correction factor. 

#number of fish of each age in the LARGE cancom fishery:
(NLbyjH <- pLby * Nlh )
#part of equation 18 in 2015 op plan
varNLbyjH <- varpLbyjH * Nlh^2 
(SENLbyjH <- sqrt(varpLbyjH * Nlh^2) )

###############################################################################
# (small) medium and large combined
###############################################################################

#part of equation 17 in 2015 op plan combining large (L) & medium(m)
(NbyjH <- NLbyjH + NmbyjH) # est number of salmon of various age-sexes with sizes combined
#part of equation 18 in 2015 op plan
varNbyjH <-varNLbyjH + varNmbyjH 
(SENbyjH <- sqrt(varNbyjH))

n <-sum(NbyjH)/4
pNbyjH <- NbyjH/n #proportions of all salmon of various age-sexes with sizes combined
fpc <- (Nh-nh)/Nh
varpNbyjH <- fpc * pNbyjH * (1-pNbyjH) / (n-1)
(SEpNbyjH <- sqrt(varpNbyjH))

tbby <- tmby + tLby

###############################################################################
# (small) medium and large separate percent of total
###############################################################################

#number of fish of each broodyear  and gender in the cancom fishery:
taby <- table(cancom$sex, cancom$broodyear, cancom$cat660)
n  <- sum(taby)
(taby <-addmargins(taby,1))
(taby <-addmargins(taby,2))

#equation 15 in 2015 op plan percent of fish of each broodyear in the cancom fishery:
(paby <- taby/n)
# Equation 26 in 2015 op plan variance of fish of each broodyear in the LARGE cancom fishery:
(varpbyijH <- fpc*paby*(1-paby)/(n-1))
(SEpbyijH <- sqrt(varpbyijH))

#C4 in excel 
cat("Appendix currently not in report Harvest data by brood year: ")
cat("Small and medium Chinook  salmon (< 660mm) Harvest")
Hmbysl <- displayabundance(tmby, pmby, SEpmbyjH, NmbyjH, SENmbyjH )

cat("Large            Chinook salmon  (>=660 mm MEF) Harvest")
HLbysl <- displayabundance(tLby, pLby, SEpLbyjH, NLbyjH, SENLbyjH )

cat("Small, medium and Large Chinook salmon combined Harvest")
Habysl <- displayabundance(tbby, pNbyjH, SEpNbyjH, NbyjH, SENbyjH)

cat("Small and medium Chinook  salmon (< 660mm)     Harvest percent of total")
Hmobysl <- displayabundance(taby[,,2], paby[,,2], SEpbyijH[,,2], NmbyjH, SENmbyjH )

cat("Large             Chinook salmon (>=660 mm MEF) Harvest percent of total")
HLobysl <- displayabundance(taby[,,1], paby[,,1], SEpbyijH[,,1], NLbyjH, SENLbyjH )


cat("Estimated broodyear/gender/size composition by size of Chinook  salmon  Harvest")
harvestbyseparated <-rbind(Hmbysl, HLbysl, Habysl)

cat("Estimated broodyear/gender/size composition overall of Chinook  salmon  Harvest")
harvestbyoverall <-rbind(Hmobysl, HLobysl, Habysl)

#hbycompall    <-harvestbyestall[14,]
#varhbycompall <-harvestbyestall[15,]*harvestbyestall[15,]

hbycompall    <-harvestbyoverall [14,]
varhbycompall <-harvestbyoverall [15,]*harvestbyoverall[15,]

###############################################################################
# Function to display age, sex, and mean length at age sex
###############################################################################

# d = dataframe, 
# t, p and sep are tables used in the calculations

displaylength<- function(d, t, p, sep){
  
  #females
  df <- d[which(d$sex=="F"),]
  
  avelength <- tapply(df$mef, df$ageeu, mean)
  sd <-tapply(df$mef, df$ageeu, sd)
  num <-table(df$ageeu)
  se= sd/sqrt(num)
  meantot <-mean(df$mef)
  setot <-sd(df$mef)/length(df$mef)
  
  fn         <- t[1,]
  fpercent   <- round( p[1,], digits = 4) 
  fSEpercent <- round( sep[1,], digits = 4)
  fAvg.length <- round( avelength, digits = 0)
  fAvg.length <- append(fAvg.length, meantot)
  fSElength   <- round(append(se, setot), digits = 1)
  (flen <- rbind(fn, fpercent, fSEpercent, fAvg.length, fSElength))
  
  #males
  dm <- d[which(d$sex=="M"),]
  
  avelength <- tapply(dm$mef, dm$ageeu, mean)
  sd <-tapply(dm$mef, dm$ageeu, sd)
  num <-table(dm$ageeu)
  se= sd/sqrt(num)
  meantot <-mean(dm$mef)
  setot <-sd(dm$mef)/length(dm$mef)
  
  mn         <- t[2,]
  mpercent   <- round( p[2,], digits = 4) # multiply by 100 to get % 
  mSEpercent <- round( sep[2,], digits = 4)
  mAvg.length<- round( avelength, digits = 0)
  mAvg.length<-append(mAvg.length, meantot)
  mSElength  <-round(append(se, setot), digits = 1)
  (mlen <- rbind(mn, mpercent, mSEpercent, mAvg.length, mSElength))
  
  #genders combined
  avelength <- tapply(d$mef, d$ageeu, mean)
  sd <-tapply(d$mef, d$ageeu, sd)
  num <-table(d$ageeu)
  se= sd/sqrt(num)
  meantot <-mean(d$mef)
  setot <-sd(d$mef)/length(d$mef)
  
  n         <- t[3,]
  percent   <- round( p[3,], digits = 4) # multiply by 100 to get % 
  SEpercent <- round( sep[3,], digits = 4)
  Avg.length<- round( avelength, digits = 0)
  Avg.length<-append(Avg.length, meantot)
  SElength  <-round(append(se, setot), digits = 1)
  (clen <- rbind(n, percent, SEpercent, Avg.length, SElength)) 
  
  report     <- rbind(flen, mlen, clen)
  return(report)
}


###############################################################################
cat("Appendix A4 in 2009 report.") 
###############################################################################

cat("Small and medium Chinook  salmon (< 660mm) Harvest")
canmlen <- displaylength(cancomm, tm, pm, SEpmjH)

cat("Large Chinook salmon (>=660 mm MEF) Harvest")
canLlen <-displaylength(cancomL, tL, pL, SEpLjH)

cat("Small, medium and Large Chinook salmon combined Harvest")
canalen <- displaylength(cancom, tb, pNjH, SEpNjH) 

cat("Age Gender composition by size and average length for Chinook salmon Harvest")
rbind(canmlen, canLlen, canalen)

cat("Small and medium Chinook  salmon (< 660mm)     Harvest percent of total")
canmoveralllen <-displaylength(cancomm, ta[,,2], pa[,,2], SEpijH[,,2])

cat("Large             Chinook salmon (>=660 mm MEF) Harvest percent of total")
canLoveralllen <-displaylength(cancomL, ta[,,1], pa[,,1], SEpijH[,,1])

cat("Age Gender composition overall and average length for Chinook salmon Harvest")
(harvest <- rbind(canmoveralllen, canLoveralllen, canalen))


###############################################################################
cat("Age and Sex composition of *Escapement* and mean length at age")
###############################################################################

displayasl<- function(d, NL, varNL, Nm, varNm ){

  dm <- d[which(d$mef < 660),]
  tm <- table(dm$sex, dm$ageeu)
  #total number of fish
  nm <-sum(tm)
  #add totals to columns and rows
  tm <- addmargins(tm,1)
  tm <-cbind (tm, Total = rowSums(tm))
  
  #equation 19 in 2015 op plan percent of fish of each medium eu age in the  spawning grounds (vertahl):
  pm <- tm/nm
  # equation 20 in 2015 op plan variance of fish of each medium eu age in the spawning grounds (vertahl):
  varpm <- pm*(1-pm)/(nm-1)
  sepm  <- sqrt(varpm)
  
  #part of equation 21 in 2015 op plan
  #number of fish of each medium age in the spawning grounds (ver/tahl):
  NmE <- (pm * Nm)
  #part of equation 22 in 2015 op plan
  varNmE <- (varNm * pm^2 + varpm * Nm^2 + varNm * varpm)
  
  dL <- d[which(d$mef >= 660),]
  tL <-table(dL$sex, dL$ageeu)
  #total number of fish
  nL <-sum(tL)
  #add totals to columns and rows
  tL <- addmargins(tL,1)
  tL <-cbind (tL, Total = rowSums(tL))
  
  #equation 19 in 2015 op plan percent of fish of each eu age in the spawning grounds (vertahl):
  pL <- tL/nL
  # equation 20 in 2015 op plan variance of fish of each LARGE eu age in the spawning grounds (vertahl):
  varpL <- pL*(1-pL)/(nL-1)
  sepL  <- sqrt(varpL)
  
  #part of equation 21 in 2015 op plan
  #number of fish of each LARGE age in the spawning grounds (ver/tahl):
  NLE <- (pL *NL)
  #part of equation 22 in 2015 op plan
  varNLE <-(varNL * pL^2 + varpL * NL^2 + varNL * varpL)
  
  #part of equation 21 in 2015 op plan
  NjE <- NLE + NmE
  #part of equation 22 in 2015 op plan
  varNE <-varNLE + varNmE 
  
  #combined
  t <-table(d$sex, d$ageeu)
  #total number of fish
  n <-sum(t)
  #add totals to columns and rows
  t <- addmargins(t,1)
  t <-cbind (t, Total = rowSums(t))
  #equation 19 in 2015 op plan percent of fish of each eu age in the LARGE spawning grounds (vertahl):
  p <- t/n
  # equation 20 in 2015 op plan variance of fish of each eu age in the LARGE spawning grounds (vertahl):
  varp <- p*(1-p)/(n-1)
  sep  <- sqrt(varp)

  medium  <- displaylength(dm, tm, pm, sepm)
  large   <- displaylength(dL, tL, pL, sepL)
  combine <- displaylength(d, t, p, sep)
 
  (report     <- rbind(medium, large, combine))
  return(report)
}

###############################################################################
cat("appendix A3 in 2009 report")
###############################################################################
displayasl(mark, 0, 0, 0, 0)

###############################################################################
#cat("appendix A4 in 2009 report")
###############################################################################
#displayasl(cancom, Nlh, 0, Nless660H, 0) # The cancom display length functions above use the fpc.

###############################################################################
cat("appendix A5 in 2009 report")
###############################################################################
ver <- vertahl [which(vertahl$source == "Verrett"),]
displayasl(ver,0,0,0,0)

###############################################################################
cat("appendix A6 in 2009 report")
###############################################################################
#table(vertahl$source, vertahl$cat660) # 1= large and 2 = mead/small in cat660
tahl <- vertahl [which(vertahl$source == "Tahltan Weir A"),]
displayasl(tahl,0,0,0,0)

###############################################################################
cat("appendix A7 in 2009 report")
###############################################################################
displayasl(vertahl, Nle, varNle, Nless660E, varNless660E)
#########################################################

###############################################################################
cat("Broodyear and Sex composition of *Escapement* and mean length at broodyear")
###############################################################################
displaylengthby<- function(d, t, p, sep){
  
  #females
  df <- d[which(d$sex=="F"),]
  
  avelength <- tapply(df$mef, df$broodyear, mean)
  sd <-tapply(df$mef, df$broodyear, sd)
  num <-table(df$broodyear)
  se= sd/sqrt(num)
  meantot <-mean(df$mef)
  setot <-sd(df$mef)/length(df$mef)
  
  fn         <- t[1,]
  fpercent   <- round( p[1,], digits = 4) 
  fSEpercent <- round( sep[1,], digits = 4)
  fAvg.length <- round( avelength, digits = 0)
  fAvg.length <- append(fAvg.length, meantot)
  fSElength   <- round(append(se, setot), digits = 1)
  (flen <- rbind(fn, fpercent, fSEpercent, fAvg.length, fSElength))
  
  #males
  dm <- d[which(d$sex=="M"),]
  
  avelength <- tapply(dm$mef, dm$broodyear, mean)
  sd <-tapply(dm$mef, dm$broodyear, sd)
  num <-table(dm$broodyear)
  se= sd/sqrt(num)
  meantot <-mean(dm$mef)
  setot <-sd(dm$mef)/length(dm$mef)
  
  mn         <- t[2,]
  mpercent   <- round( p[2,], digits = 4) # multiply by 100 to get % 
  mSEpercent <- round( sep[2,], digits = 4)
  mAvg.length<- round( avelength, digits = 0)
  mAvg.length<-append(mAvg.length, meantot)
  mSElength  <-round(append(se, setot), digits = 1)
  (mlen <- rbind(mn, mpercent, mSEpercent, mAvg.length, mSElength))
  
  #genders combined
  avelength <- tapply(d$mef, d$broodyear, mean)
  sd <-tapply(d$mef, d$broodyear, sd)
  num <-table(d$broodyear)
  se= sd/sqrt(num)
  meantot <-mean(d$mef)
  setot <-sd(d$mef)/length(d$mef)
  
  n         <- t[3,]
  percent   <- round( p[3,], digits = 4) # multiply by 100 to get % 
  SEpercent <- round( sep[3,], digits = 4)
  Avg.length<- round( avelength, digits = 0)
  Avg.length<-append(Avg.length, meantot)
  SElength  <-round(append(se, setot), digits = 1)
  (clen <- rbind(n, percent, SEpercent, Avg.length, SElength)) 
  
  report     <- rbind(flen, mlen, clen)
  return(report)
}

displaybysl<- function(d, NL, varNL, Nm, varNm ){
  dm <- d[which(d$mef < 660),]
  tm <- table(dm$sex, dm$broodyear)
  #total number of fish
  nm <-sum(tm)
  #add totals to columns and rows
  tm <- addmargins(tm,1)
  tm <-cbind (tm, Total = rowSums(tm))
  
  #equation 19 in 2015 op plan percent of fish of each medium eu broodyear in the  spawning grounds (vertahl):
  pm <- tm/nm
  # equation 20 in 2015 op plan variance of fish of each medium eu broodyear in the spawning grounds (vertahl):
  varpm <- pm*(1-pm)/(nm-1)
  sepm  <- sqrt(varpm)
  
  #part of equation 21 in 2015 op plan
  #number of fish of each medium broodyear in the spawning grounds (ver/tahl):
  NmE <- (pm * Nm)
  #part of equation 22 in 2015 op plan
  varNmE <- (varNm * pm^2 + varpm * Nm^2 + varNm * varpm)
  
  dL <- d[which(d$mef >= 660),]
  tL <-table(dL$sex, dL$broodyear)
  #total number of fish
  nL <-sum(tL)
  #add totals to columns and rows
  tL <- addmargins(tL,1)
  tL <-cbind (tL, Total = rowSums(tL))
  
  #equation 19 in 2015 op plan percent of fish of each eu broodyear in the spawning grounds (vertahl):
  pL <- tL/nL
  # equation 20 in 2015 op plan variance of fish of each LARGE eu broodyear in the spawning grounds (vertahl):
  varpL <- pL*(1-pL)/(nL-1)
  sepL  <- sqrt(varpL)
  
  #part of equation 21 in 2015 op plan
  #number of fish of each LARGE broodyear in the spawning grounds (ver/tahl):
  NLE <- (pL *NL)
  #part of equation 22 in 2015 op plan
  varNLE <-(varNL * pL^2 + varpL * NL^2 + varNL * varpL)
  
  #part of equation 21 in 2015 op plan
  NjE <- NLE + NmE
  #part of equation 22 in 2015 op plan
  varNE <-varNLE + varNmE 
  
  #combined
  t <-table(d$sex, d$broodyear)
  #total number of fish
  n <-sum(t)
  #add totals to columns and rows
  t <- addmargins(t,1)
  t <-cbind (t, Total = rowSums(t))
  #equation 19 in 2015 op plan percent of fish of each eu broodyear in the LARGE spawning grounds (vertahl):
  p <- t/n
  # equation 20 in 2015 op plan variance of fish of each eu broodyear in the LARGE spawning grounds (vertahl):
  varp <- p*(1-p)/(n-1)
  sep  <- sqrt(varp)
  
  medium  <- displaylengthby(dm, tm, pm, sepm)
  large   <- displaylengthby(dL, tL, pL, sepL)
  combine <- displaylengthby(d, t, p, sep)
  
  (report     <- rbind(medium, large, combine))
  return(report)
}

###############################################################################
cat("appendix B3 in 2009 report")
###############################################################################
displaybysl(mark, 0, 0, 0, 0)

###############################################################################
#cat("appendix B4 in 2009 report")
###############################################################################
#displaybysl(cancom, Nlh, 0, Nless660H, 0) # The cancom display length functions above use the fpc.

###############################################################################
cat("appendix B5 in 2009 report")
###############################################################################
ver <- vertahl [which(vertahl$source == "Verrett"),]
displaybysl(ver,0,0,0,0)

###############################################################################
cat("appendix B6 in 2009 report")
###############################################################################
#table(vertahl$source, vertahl$cat660) # 1= large and 2 = mead/small in cat660
tahl <- vertahl [which(vertahl$source == "Tahltan Weir A"),]
displaybysl(tahl,0,0,0,0)

###############################################################################
cat("appendix B7 in 2009 report")
###############################################################################
displaybysl(vertahl, Nle, varNle, Nless660E, varNless660E)
#########################################################

#by end
###############################################################################
#Simulation for variance of the proportion of fish of each age in the escapement. 
# "sim" stands for simulated value
# "i" in a name represents sizes (not to confused with just indices of a for loop)
# "j" in a name represents ages
NE   <- Nless660E + Nle  # Number in Escapement
t3   <- table(vertahl$sex, vertahl$ageeu, vertahl$cat660)
#vt3  <- as.vector(t3)
n3   <- sum(t3)
p3   <- t3/n3
NEij <-NE*p3

rep = 10000  #separate from reps
simNlr <- rnorm(rep, mean=Nlr, sd=sqrt(varNlr)) # normally distributed draws of the number Large in the river
simNLE <- simNlr - Nlh                          # Number of Large in escapement

simp <- rmultinom(rep, n3, p3)/n3 #(simulated) proportion all ages/gender/size (Panels A & B females and male sections)

simpmm <- rep(0, rep)
simpmf <- rep(0, rep)
simpmE <- rep(0, rep)
simpLm <- rep(0, rep)
simpLf <- rep(0, rep)
simpLE <- rep(0, rep)
simpf  <- rep(0, rep)
simpm  <- rep(0, rep)
simpj <- matrix(, nrow = 26, ncol = rep)
simpi <- matrix(, nrow = 26, ncol = rep)
simpk <- matrix(, nrow = 13, ncol = rep)
simpby  <- matrix(, nrow = 20, ncol = rep)
simpbyj <- matrix(, nrow = 10, ncol = rep)
simpbyi <- matrix(, nrow = 10, ncol = rep)
simpbyk <- matrix(, nrow = 5 , ncol = rep)
for (i in 1:rep) {
 # The following 8 vectors record for the totals in the Table (right most column)
 simpmf[i] <- sum(simp[c(27,29,31,33,35,37,39,41,43,45,47,49,51),i]) #total mediums female 
 simpmm[i] <- sum(simp[c(28,30,32,34,36,38,40,42,44,46,48,50,52),i]) #total mediums male
 simpmE[i] <- sum(simp[27:52,i])                                     #total mediums combined 
 simpLf[i] <- sum(simp[c(1,3,5,7,9,11,13,15,17,19,21,23,25),i])      #total Larges female 
 simpLm[i] <- sum(simp[c(2,4,6,8,10,12,14,16,18,20,22,24,26),i])     #total Larges male 
 simpLE[i] <- sum(simp[1:26,i])                                      #total Larges combined
 simpf[i]  <- sum(simpmf[i],simpLf[i])                               #total females combined
 simpm[i]  <- sum(simpmm[i],simpLm[i])                               #total males combined
 
 for(j in 1:26){
   simpj[j, i] <- sum(simp[(j-1)*2+1,i], simp[(j-1)*2+2,i]) #proportion all ages/sizes with genders combined 1-13 Large, 14-26 medium
 }                                                          #(Bottom of panels A & B)
 for(h in 1:26){
   simpi[h, i] <- sum(simp[h,i], simp[h + 26,i]) # proportion all ages/genders with sizes combined where odd# (1,3,5,..25) female, even # = male
 }                                               # (Females and males portion of panel C)
 for(k in 1:13){
   simpk[k, i] <- sum(simpi[(k-1)*2+1,i] + simpi[(k-1)*2+2,i]) # proportion all ages with gender/ sizes combined.
 } 
 simpby[1,i]  <- sum(simp[c(1,  3),i])
 simpby[2,i]  <- sum(simp[c(2,  4),i])
 simpby[3,i]  <- sum(simp[c(5,  7,  9) ,i])
 simpby[4,i]  <- sum(simp[c(6,  8,  10),i])
 simpby[5,i]  <- sum(simp[c(11, 13, 15),i])
 simpby[6,i]  <- sum(simp[c(12, 14, 16),i])
 simpby[7,i]  <- sum(simp[c(17, 19, 21),i])
 simpby[8,i]  <- sum(simp[c(18, 20, 22),i])
 simpby[9,i]  <- sum(simp[c(23, 25),i])
 simpby[10,i] <- sum(simp[c(24, 26),i])
 
 simpbyi[1,i]  <- sum(simpi[c(1,  3),i])
 simpbyi[2,i]  <- sum(simpi[c(2,  4),i])
 simpbyi[3,i]  <- sum(simpi[c(5,  7,  9) ,i])
 simpbyi[4,i]  <- sum(simpi[c(6,  8,  10),i])
 simpbyi[5,i]  <- sum(simpi[c(11, 13, 15),i])
 simpbyi[6,i]  <- sum(simpi[c(12, 14, 16),i])
 simpbyi[7,i]  <- sum(simpi[c(17, 19, 21),i])
 simpbyi[8,i]  <- sum(simpi[c(18, 20, 22),i])
 simpbyi[9,i]  <- sum(simpi[c(23, 25),i])
 simpbyi[10,i] <- sum(simpi[c(24, 26),i])
 
 simpbyj[1, i]  <- sum(simpj[c(1,  2),i])
 simpbyj[2, i]  <- sum(simpj[c(3,  4,  5),i])
 simpbyj[3, i]  <- sum(simpj[c(6,  7,  8),i])
 simpbyj[4, i]  <- sum(simpj[c(9,  10, 11),i])
 simpbyj[5, i]  <- sum(simpj[c(12, 13),i])
 simpbyj[6, i]  <- sum(simpj[c(14, 15),i])
 simpbyj[7, i]  <- sum(simpj[c(16, 17, 18),i])
 simpbyj[8, i]  <- sum(simpj[c(19, 20, 21),i])
 simpbyj[9, i]  <- sum(simpj[c(22, 23, 24),i])
 simpbyj[10,i]  <- sum(simpj[c(25, 26),i])
 
 simpbyk[1, i]  <- sum(simpk[c(1,  2),i])
 simpbyk[2, i]  <- sum(simpk[c(3,  4,  5),i])
 simpbyk[3, i]  <- sum(simpk[c(6,  7,  8),i])
 simpbyk[4, i]  <- sum(simpk[c(9,  10, 11),i])
 simpbyk[5, i]  <- sum(simpk[c(12, 13),i])
}

###Variance for p estimates

#varsimp <- var(simp)
varsimp <- rep(0, 52)
for (i in 1:52) {
  varsimp[i] <- var(simp[i,]) 
}
sesimp <- sqrt(varsimp) #(simulated) proportion all ages/gender/size (Panels A & B females and male sections)

varsimpi <- rep(0, 26)
for (i in 1:26) {
  varsimpi[i] <- var(simpi[i,]) 
}
sesimpi <- sqrt(varsimpi)# proportion all ages/genders with sizes combined 1-13 female, 14-26 male
                         # (Females and males portion of panel C)

varsimpj <- rep(0, 26)
for (j in 1:26) {
  varsimpj[j] <- var(simpj[j,])
}
sesimpj <- sqrt(varsimpj)#proportion all ages/sizes with genders combined 1-13 Large, 14-26 medium
                         #(Bottom of panels A & B)
varsimpk <- rep(0, 13)
for (k in 1:13) {
  varsimpk[k] <- var(simpk[k,])
}
sesimpk <- sqrt(varsimpk)#proportion all ageswith genders/sizes  combined 1-13
#(Bottom of panels C)


varsimpby <- rep(0, 20)
for (i in 1:10){
  varsimpby[i]  <- var(simpby[i,])
}
sesimpby  <- sqrt(varsimpby)#proportion of all broodyear/gender/size (panels A&B females and male section)

varsimpbyi<- rep(0, 10)
varsimpbyj <- rep(0, 10)
for (i in 1:10){
  varsimpbyi[i] <- var(simpbyi[i,])
  varsimpbyj[i] <- var(simpbyj[i,])  
}
sesimpbyi <- sqrt(varsimpbyi)
sesimpbyj <- sqrt(varsimpbyj)#proportion of all ages/genders with sizes combined 1-5 female, 6-10 male
#bottom of panels A & B

varsimpbyk  <- rep(0, 5)
for (i in 1:5){
  varsimpbyk[i] <- var(simpbyk[i,])  
}
sesimpbyk <- sqrt(varsimpbyk)#proportion of all broodyear with genders/sizes  combined 1-5 bottom of panel C

varsimpmf <- var(simpmf)     #total mediums female 
varsimpmm <- var(simpmm)     #total mediums male 
varsimpmE <- var(simpmE)     #total mediums combined 

varsimpLf <- var(simpLf)     #total Larges female 
varsimpLm <- var(simpLm)     #total Larges male 
varsimpLE <- var(simpLE)     #total Larges combined

varsimpf  <- var(simpf)      #total females combined
varsimpm  <- var(simpm)      #total males combined

# The following 8 vectors are for SE of % the totals in the table (right most column)
sesimpmf  <- sqrt(varsimpmf) #total mediums female
sesimpmm  <- sqrt(varsimpmm) #total mediums male 
sesimpmE  <- sqrt(varsimpmE) #total mediums combined 

sesimpLf  <- sqrt(varsimpLf) #total Larges female 
sesimpLm  <- sqrt(varsimpLm) #total Larges male 
sesimpLE  <- sqrt(varsimpLE) #total Larges combined. This should be the same as for medium combined.

sesimpf  <- sqrt(varsimpf)   #total female combined
sesimpm  <- sqrt(varsimpm)   #total male   combined

#Now for simulated totals

simNless660E    <- simNLE*(1/simpLE-1) #simulate the total number of medium fish in the escapement
varsimNless660E <- var(simNless660E)   #this should be the same as below
sesimNless660E  <- sqrt(varsimNless660E)

simNE <- simNless660E + simNLE      #simulated total number of all sized fish in the escapement
simN  <- simNE*simp                 #simulated total number of each ages/gender/size (Panels A & B females and male sections)

simNmm  <- rep(0, rep)
simNmf  <- rep(0, rep)
simNmE  <- rep(0, rep)
simNLm  <- rep(0, rep)
simNLf  <- rep(0, rep)
simNLE  <- rep(0, rep)
simNf   <- rep(0, rep)
simNm   <- rep(0, rep)
simNj   <- matrix(, nrow = 26, ncol = rep)
simNi   <- matrix(, nrow = 26, ncol = rep)
simNk   <- matrix(, nrow = 13, ncol = rep)
simNby  <- matrix(, nrow = 20, ncol = rep)
simNbyj <- matrix(, nrow = 10, ncol = rep)
simNbyi <- matrix(, nrow = 10, ncol = rep)
simNbyk <- matrix(, nrow = 5 , ncol = rep)
for (i in 1:rep) {
  # The following 8 vectors record for the totals in the Table (right most column)
  simNmf[i] <- sum(simN[c(27,29,31,33,35,37,39,41,43,45,47,49,51),i]) #total mediums female 
  simNmm[i] <- sum(simN[c(28,30,32,34,36,38,40,42,44,46,48,50,52),i]) #total mediums male
  simNmE[i] <- sum(simN[27:52,i])                                     #total mediums combined 
  simNLf[i] <- sum(simN[c(1,3,5,7,9,11,13,15,17,19,21,23,25),i])      #total Larges female 
  simNLm[i] <- sum(simN[c(2,4,6,8,10,12,14,16,18,20,22,24,26),i])     #total Larges male 
  simNLE[i] <- sum(simN[1:26,i])                                      #total Larges combined
  simNf[i]  <- sum(simNmf[i],simNLf[i])                               #total females combined
  simNm[i]  <- sum(simNmm[i],simNLm[i])                               #total males combined
  for(j in 1:26){
    simNj[j, i] <- sum(simN[(j-1)*2+1,i], simN[(j-1)*2+2,i]) #number of all ages/sizes with genders combined 1-13 Large, 14-26 medium
  }                                                          #(Bottom of panels A & B)
  for(h in 1:26){
    simNi[h, i] <- sum(simN[h,i], simN[h + 26,i]) # number of all ages/genders with sizes combined 1-13 female, 14-26 male
  }                                               # (Females and males portion of panel C)
  for(k in 1:13){
    simNk[k, i] <- sum(simNi[(k-1)*2+1,i] + simNi[(k-1)*2+2,i]) # proportion all ages with gender/ sizes combined.
  } 
  simNby[1,i]  <- sum(simN[c(1,  3),i])
  simNby[2,i]  <- sum(simN[c(2,  4),i])
  simNby[3,i]  <- sum(simN[c(5,  7,  9) ,i])
  simNby[4,i]  <- sum(simN[c(6,  8,  10),i])
  simNby[5,i]  <- sum(simN[c(11, 13, 15),i])
  simNby[6,i]  <- sum(simN[c(12, 14, 16),i])
  simNby[7,i]  <- sum(simN[c(17, 19, 21),i])
  simNby[8,i]  <- sum(simN[c(18, 20, 22),i])
  simNby[9,i]  <- sum(simN[c(23, 25),i])
  simNby[10,i] <- sum(simN[c(24, 26),i])
  
  simNbyi[1,i]  <- sum(simNi[c(1,  3),i])
  simNbyi[2,i]  <- sum(simNi[c(2,  4),i])
  simNbyi[3,i]  <- sum(simNi[c(5,  7,  9) ,i])
  simNbyi[4,i]  <- sum(simNi[c(6,  8,  10),i])
  simNbyi[5,i]  <- sum(simNi[c(11, 13, 15),i])
  simNbyi[6,i]  <- sum(simNi[c(12, 14, 16),i])
  simNbyi[7,i]  <- sum(simNi[c(17, 19, 21),i])
  simNbyi[8,i]  <- sum(simNi[c(18, 20, 22),i])
  simNbyi[9,i]  <- sum(simNi[c(23, 25),i])
  simNbyi[10,i] <- sum(simNi[c(24, 26),i])
  
  simNbyj[1, i]  <- sum(simNj[c(1,  2),i])
  simNbyj[2, i]  <- sum(simNj[c(3,  4,  5),i])
  simNbyj[3, i]  <- sum(simNj[c(6,  7,  8),i])
  simNbyj[4, i]  <- sum(simNj[c(9,  10, 11),i])
  simNbyj[5, i]  <- sum(simNj[c(12, 13),i])
  simNbyj[6, i]  <- sum(simNj[c(14, 15),i])
  simNbyj[7, i]  <- sum(simNj[c(16, 17, 18),i])
  simNbyj[8, i]  <- sum(simNj[c(19, 20, 21),i])
  simNbyj[9, i]  <- sum(simNj[c(22, 23, 24),i])
  simNbyj[10,i]  <- sum(simNj[c(25, 26),i])
  
  simNbyk[1, i]  <- sum(simNk[c(1,  2),i])
  simNbyk[2, i]  <- sum(simNk[c(3,  4,  5),i])
  simNbyk[3, i]  <- sum(simNk[c(6,  7,  8),i])
  simNbyk[4, i]  <- sum(simNk[c(9,  10, 11),i])
  simNbyk[5, i]  <- sum(simNk[c(12, 13),i])
}

###Variance for N estimates

varsimNE <- var(simNE)
sesimNE  <- sqrt(varsimNE)

#varsimN <- var(simN)
varsimN <- rep(0, 52)
for (i in 1:52) {
  varsimN[i] <- var(simN[i,]) 
}
sesimN <- sqrt(varsimN) #(simulated) number of all ages/gender/size (Panels A & B females and male sections)

varsimNi <- rep(0, 26)
for (i in 1:26) {
  varsimNi[i] <- var(simNi[i,])  
}
sesimNi <- sqrt(varsimNi)# number of all ages/genders with sizes combined 1-13 female, 14-26 male
# (Females and males portion of panel C)

varsimNj <- rep(0, 26)
for (j in 1:26) {
  varsimNj[j] <- var(simNj[j,])
}
sesimNj <- sqrt(varsimNj)#number of all ages/sizes with genders combined 1-13 Large, 14-26 medium
#(Bottom of panels A & B)

varsimNk <- rep(0, 13)
for (k in 1:13) {
  varsimNk[k] <- var(simNk[k,])
}
sesimNk <- sqrt(varsimNk)#Number all ages with genders/sizes  combined 1-13 #(Bottom of panels C)

varsimNby <- rep(0, 10)
for (i in 1:10) {
  (varsimNby[i] <- var(simNby[i,])) 
}
sesimNby <- sqrt(varsimNby) #(simulated) number of all ages/gender/size (Panels A & B females and male sections)

varsimNbyi<- rep(0, 10)
varsimNbyj <- rep(0, 10)
for (i in 1:10){
  varsimNbyi[i] <- var(simNbyi[i,])
  varsimNbyj[i] <- var(simNbyj[i,])  
}
sesimNbyi <- sqrt(varsimNbyi)#Number of all broodyear/gender/size (panels A&B females and male section)
sesimNbyj <- sqrt(varsimNbyj)#number of all broodyear/size with genders combined, 1-5 large, 6-10 medium
                             #bottom of panels A & B

varsimNbyk <- rep(0, 5)
for (i in 1:5){
  varsimNbyk[i] <- var(simNbyk[i,])  
}
sesimNbyk <- sqrt(varsimNbyk)#Number all broodyear with genders/sizes  combined 1-5 bottom of panel C

varsimNmf <- var(simNmf)     #total mediums female 
varsimNmm <- var(simNmm)     #total mediums male 
varsimNmE <- var(simNmE)      #total mediums combined 

varsimNLf <- var(simNLf)     #total Larges female 
varsimNLm <- var(simNLm)     #total Larges male 
varsimNLE <- var(simNLE)     #total Larges combined

varsimNf  <- var(simNf)      #total females combined
varsimNm  <- var(simNm)      #total males combined

# The following 8 vectors are for SE of %the totals in the table (right most column)
sesimNmf  <- sqrt(varsimNmf) #total mediums female
sesimNmm  <- sqrt(varsimNmm) #total mediums male 
sesimNmE  <- sqrt(varsimNmE) #total mediums combined 

sesimNLf  <- sqrt(varsimNLf) #total Larges female 
sesimNLm  <- sqrt(varsimNLm) #total Larges male 
sesimNLE  <- sqrt(varsimNLE) #total Larges combined. This should be the same as for medium combined.

sesimNf  <- sqrt(varsimNf)   #total female combined
sesimNm  <- sqrt(varsimNm)   #total male   combined

#### 

#create a shell with appropriate lables.

#Standard Error of percentage of Escapement
#Panel A medium Fish percentage of Escapment SE
dpmed      <- matrix(c(sesimp[27:52],sesimpmf, sesimpmm), 2, 14)
sedpmed    <- rbind(dpmed, c(sesimpj[14:26], sesimpmE)) #se d percent medium
#Panel B Large Fish percentage of Escapment SE
dplarge    <- matrix(c(sesimp[1:26] ,sesimpLf, sesimpLm), 2, 14)
sedplarge  <- rbind(dplarge, c(sesimpj[1:13], sesimpLE)) 
#Panel C Large and medium Fish percentage of Escapment SE
dpall      <- matrix(c(sesimpi[1:26] ,sesimpf, sesimpm), 2, 14)
sedpall   <- rbind(dpall, c(sesimpk[1:13], 0)) 

#NEED TO DO SE FOR PERCENTAGES WTIH BROODYEARS
#Standard Error of Escapement for brood years
#Panel A medium Fish percentage abundance SE
dpmedby      <- matrix(c(sesimpby[11:20],sesimpmf, sesimpmm), 2, 6)
sedpmedby    <- rbind(dpmedby, c(sesimpbyj[6:10], sesimpmE)) 
#Panel B Large Fish Escapment abundance SE
dplargeby    <- matrix(c(sesimpby[1:10] ,sesimpLf, sesimpLm), 2, 6)
sedplargeby  <- rbind(dplargeby, c(sesimpbyj[1:5], sesimpLE)) 
#Panel C Large and medium Fish Escapment abundance SE
dpallby      <- matrix(c(sesimpbyi[1:10] ,sesimpf, sesimpm), 2, 6)
sedpallby     <- rbind(dpallby, c(sesimpbyk[1:5], 0)) 

#Standard Error of Escapement
#Panel A medium Fish Escapment abundance SE
dNmed      <- matrix(c(sesimN[27:52],sesimNmf, sesimNmm), 2, 14)
sedNmed    <- rbind(dNmed, c(sesimNj[14:26], sesimNmE)) 
#Panel B Large Fish Escapment abundance SE
dNlarge    <- matrix(c(sesimN[1:26] ,sesimNLf, sesimNLm), 2, 14)
sedNlarge  <- rbind(dNlarge, c(sesimNj[1:13], sesimNLE)) 
#Panel C Large and medium Fish Escapment abundance SE
dNall      <- matrix(c(sesimNi[1:26] ,sesimNf, sesimNm), 2, 14)
sedNall     <- rbind(dNall, c(sesimNk[1:13], sesimNE)) 

#Standard Error of Escapement for brood years
#Panel A medium Fish Escapment abundance SE
dNmedby      <- matrix(c(sesimNby[1:10],sesimNmf, sesimNmm), 2, 6)
sedNmedby    <- rbind(dNmedby, c(sesimNbyj[6:10], sesimNmE)) 
#Panel B Large Fish Escapment abundance SE
dNlargeby    <- matrix(c(sesimNby[1:10] ,sesimNLf, sesimNLm), 2, 6)
sedNlargeby  <- rbind(dNlargeby, c(sesimNbyj[1:5], sesimNLE)) 
#Panel C Large and medium Fish Escapment abundance SE
dNallby      <- matrix(c(sesimNbyi[1:10] ,sesimNf, sesimNm), 2, 6)
sedNallby     <- rbind(dNallby, c(sesimNbyk[1:5], sesimNE)) 

#THank you Ellie =) give me a call if it has any install issues? 465-6076
#okay

###############################################################################
cat("The estimate of large in-river based on the censored mark recapture")
###############################################################################
Nlr

cat("The estimated standard error of N^_lr:") #(which is the sd of the sample)
(seNlr  <- sd(abundance_estimates))
varNlr <- var(abundance_estimates)

#################################################
###############################################################################
cat("Age and Sex composition of escapement and abundance")
###############################################################################

#total number of fish in the MEDIUM vertahl spawning grounds:
tm <-t3[,,2]
#total number of fish
n <-sum(tm)
#add totals to columns and rows
tm <- addmargins(tm,1)
tm <-cbind (tm, Total = rowSums(tm))
#percentage of fish in the MEDIUM vertahl spawning grounds:
pm <-p3[,,2]
#total number of fish
n <-sum(pm)
#add totals to columns and rows
pm <- addmargins(pm,1)
pm <-cbind (pm, Total = rowSums(pm))
#estimated escapement in the MEDIUM vertahl spawning grounds:
NmjE <- NEij[,,2]
#total number of fish
n <-sum(NmjE)
#add totals to columns and rows
NmjE <- addmargins(NmjE,1)
NmjE <-cbind (NmjE, Total = rowSums(NmjE))


#total number of fish in the LARGE vertahl spawning grounds:
tL <-t3[,,1]
#total number of fish
n <-sum(tL)
#add totals to columns and rows
tL <- addmargins(tL,1)
tL <-cbind (tL, Total = rowSums(tL))
#percentage of fish in the LARGE vertahl spawning grounds:
pL <-p3[,,1]
#total number of fish
n <-sum(pL)
#add totals to columns and rows
pL <- addmargins(pL,1)
pL <-cbind (pL, Total = rowSums(pL))
#estimated escapement in the LARGE vertahl spawning grounds:
NLjE <- NEij[,,1]
#total number of fish
n <-sum(NLjE)
#add totals to columns and rows
NLjE <- addmargins(NLjE,1)
NLjE <- cbind (NLjE, Total = rowSums(NLjE))


#total number of fish in the large and small COMBINED vertahl spawning grounds:
ta <-t3[,,1] + t3[,,2]
#total number of fish
n <-sum(ta)
#add totals to columns and rows
ta <- addmargins(ta,1)
ta <- cbind (ta, Total = rowSums(ta))
#percentage of fish in the large and small COMBINED vertahl spawning grounds:
pa <- p3[,,1]+p3[,,2]
#total number of fish
n <-sum(pa)
#add totals to columns and rows
pa <- addmargins(pa,1)
pa <-cbind (pa, Total = rowSums(pa))
#estimated escapement in the large and small COMBINED vertahl spawning grounds:
NalljE <- NEij[,,1]+NEij[,,2]
#total number of fish
n <-sum(NalljE)
#add totals to columns and rows
NalljE <- addmargins(NalljE,1)
NalljE <- cbind (NalljE, Total = rowSums(NalljE))

####################################################

#total number of fish on the MEDIUM vertahl spawning grounds:

#number of fish of each eu age  and gender on the MEDIUM cancom spawning grounds:
tm1 <- table(vertahlm$sex, vertahlm$ageeu)
n  <- sum(tm1)
(tm1 <-addmargins(tm1,1))
tm1 <- cbind(tm1, Total = rowSums(tm1))

#equation 15 in 2015 op plan percent of fish of each eu age on the MEDIUM vertahl spawning grounds:
(pm1 <- tm1/n)
# Equation 26 in 2015 op plan variance of fish of each eu age on the MEDIUM vertahl spawning grounds:
(varp1mjE <- pm1*(1-pm1)/(n-1))
(SEp1mjE <- sqrt(varp1mjE))


################################################
#Large vertahl Fishery

#number of fish of each eu age and gender on the LARGE vertahl spawning grounds:
(tL1 <-table( vertahlL$sex, vertahlL$ageeu))
n  <- sum(tL1)#total number of fish on the LARGE vertahl spawning grounds:
(tL1 <-addmargins(tL1,1))
(tL1 <- cbind(tL1, Total = rowSums(tL1)))

#equation 15 in 2015 op plan percent of fish of each eu age on the LARGE vertahl spawning grounds:
(pL1 <- tL1/n)
# Equation 26 in 2015 op plan variance of fish of each eu age on the LARGE vertahl spawning grounds:
(varp1LjE <- pL1*(1-pL1)/(n-1))
(SEp1LjE <- sqrt(varp1LjE))

#number of fish of each eu age and gender on the combined vertahl spawning grounds:
(ta1 <-table( vertahl$sex, vertahl$ageeu))
n  <- sum(ta1)#total number of fish on the LARGE vertahl spawning grounds:
(ta1 <-addmargins(ta1,1))
(ta1 <- cbind(ta1, Total = rowSums(ta1)))


#Broodyear calc set up
###############################################################################
#Age and Sex composition of escapement and abundance using Brood years
###############################################################################
#NE   <- Nless660E + Nle  # Number in Escapement
t3by   <- table(vertahl$sex, vertahl$broodyear, vertahl$cat660)
n3by   <- sum(t3by)
p3by   <- t3by/n3by
NEbyij <- NE*p3by

#total number of fish in the MEDIUM vertahl spawning grounds:
tmby <-t3by[,,2]
#total number of fish
n <-sum(tmby)
#add totals to columns and rows
tmby <- addmargins(tmby,1)
tmby <-cbind (tmby, Total = rowSums(tmby))
#percentage of fish in the MEDIUM vertahl spawning grounds:
pmby <-p3by[,,2]
#total number of fish
n <-sum(pmby)
#add totals to columns and rows
pmby <- addmargins(pmby,1)
pmby <-cbind (pmby, Total = rowSums(pmby))
#estimated escapement in the MEDIUM vertahl spawning grounds:
NmbyjE <- NEbyij[,,2]
#total number of fish
n <-sum(NmbyjE)
#add totals to columns and rows
NmbyjE <- addmargins(NmbyjE,1)
NmbyjE <-cbind (NmbyjE, Total = rowSums(NmbyjE))


#total number of fish in the LARGE vertahl spawning grounds:
tLby <-t3by[,,1]
#total number of fish
n <-sum(tLby)
#add totals to columns and rows
tLby <- addmargins(tLby,1)
tLby <-cbind (tLby, Total = rowSums(tLby))
#percentage of fish in the LARGE vertahl spawning grounds:
pLby <-p3by[,,1]
#total number of fish
n <-sum(pLby)
#add totals to columns and rows
pLby <- addmargins(pLby,1)
pLby <-cbind (pLby, Total = rowSums(pLby))
#estimated escapement in the LARGE vertahl spawning grounds:
NLbyjE <- NEbyij[,,1]
#total number of fish
n <-sum(NLbyjE)
#add totals to columns and rows
NLbyjE <- addmargins(NLbyjE,1)
NLbyjE <- cbind (NLbyjE, Total = rowSums(NLbyjE))


#total number of fish in the large and small COMBINED vertahl spawning grounds:
taby <-t3by[,,1] + t3by[,,2]
#total number of fish
n <-sum(taby)
#add totals to columns and rows
taby <- addmargins(taby,1)
taby <- cbind (taby, Total = rowSums(taby))
#percentage of fish in the large and small COMBINED vertahl spawning grounds:
paby <- p3by[,,1]+p3by[,,2]
#total number of fish
n <-sum(paby)
#add totals to columns and rows
paby <- addmargins(paby,1)
paby <-cbind (paby, Total = rowSums(paby))
#estimated escapement in the large and small COMBINED vertahl spawning grounds:
NallbyjE <- NEbyij[,,1]+NEbyij[,,2]
#total number of fish
n <-sum(NallbyjE)
#add totals to columns and rows
NallbyjE <- addmargins(NallbyjE,1)
NallbyjE <- cbind (NallbyjE, Total = rowSums(NallbyjE))

####################################################

#total number of fish on the MEDIUM vertahl spawning grounds:

#number of fish of each broodyear  and gender on the MEDIUM cancom spawning grounds:
tmby1 <- table(vertahlm$sex, vertahlm$broodyear)
n  <- sum(tmby1)
(tmby1 <-addmargins(tmby1,1))
tmby1 <- cbind(tmby1, Total = rowSums(tmby1))

#equation 15 in 2015 op plan percent of fish of each broodyear on the MEDIUM vertahl spawning grounds:
(pmby1 <- tmby1/n)
# Equation 26 in 2015 op plan variance of fish of each broodyear on the MEDIUM vertahl spawning grounds:
(varp1mbyjE <- pmby1*(1-pmby1)/(n-1))
(SEp1mbyjE <- sqrt(varp1mbyjE))


################################################
#Large vertahl Fishery

#number of fish of each broodyear and gender on the LARGE vertahl spawning grounds:
(tLby1 <-table( vertahlL$sex, vertahlL$broodyear))
n  <- sum(tLby1)#total number of fish on the LARGE vertahl spawning grounds:
(tLby1 <-addmargins(tLby1,1))
(tLby1 <- cbind(tLby1, Total = rowSums(tLby1)))

#equation 15 in 2015 op plan percent of fish of each broodyear on the LARGE vertahl spawning grounds:
(pLby1 <- tLby1/n)
# Equation 26 in 2015 op plan variance of fish of each broodyear on the LARGE vertahl spawning grounds:
(varp1LbyjE <- pLby1*(1-pLby1)/(n-1))
(SEp1LbyjE <- sqrt(varp1LbyjE))

#number of fish of each broodyear and gender on the combined vertahl spawning grounds:
(taby1 <-table( vertahl$sex, vertahl$broodyear))
n  <- sum(taby1)#total number of fish on the LARGE vertahl spawning grounds:
(taby1 <-addmargins(taby1,1))
(taby1 <- cbind(taby1, Total = rowSums(taby1)))


displayabundanceE<- function(t, p, sep, z, sez ){
  #females
  fn         <- round(t[1,], digits = 1)
  fpercent   <- round( p[1,], digits = 6) 
  fSEpercent <- round( sep[1,], digits = 6)
  fEscapement   <- round( z[1,], digits = 0)
  fSEEscapement <- round( sez[1,], digits = 0)
  females    <- rbind(fn, fpercent, fSEpercent, fEscapement, fSEEscapement)
  #males
  mn         <- t[2,]
  mpercent   <- round( p[2,], digits = 6) 
  mSEpercent <- round( sep[2,], digits = 6)
  mEscapement   <- round( z[2,], digits = 0)
  mSEEscapement <- round( sez[2,], digits = 0)
  males <- rbind(mn, mpercent, mSEpercent, mEscapement, mSEEscapement)
  #combined genders
  cn         <- t[3,]
  cpercent   <- round( p[3,], digits = 6) 
  cSEpercent <- round( sep[3,], digits = 6)
  cEscapement   <- round( z[3,], digits = 0)
  cSEEscapement <- round( sez[3,], digits = 0)
  combined   <- rbind(cn, cpercent, cSEpercent, cEscapement, cSEEscapement) 
  report     <- rbind(females, males, combined)
  return(report)
}

###############################################################################
cat("number of fish of each eu age  and gender in the vertahl spawning grounds(escapement):")
###############################################################################
#Appendix currently not in report C1 in 2012 excel. 
cat("Small and medium Chinook  salmon (< 660mm) Escapement")
escapmentestm1 <- displayabundanceE(tm1, pm1, SEp1mjE, NmjE, sedNmed )

cat("Large Chinook salmon  (>=660 mm MEF) Escapement")
escapmentestL1 <- displayabundanceE(tL1, pL1, SEp1LjE, NLjE, sedNlarge ) 

cat("Small and medium Chinook  salmon (< 660mm)     Escapement percent of total")
escapmentestm <- displayabundanceE(tm, pm, sedpmed, NmjE, sedNmed )

cat("Large             Chinook salmon (>=660 mm MEF) Escapement percent of total")
escapmentestL <- displayabundanceE(tL, pL, sedplarge, NLjE, sedNlarge )

cat("Small, medium and Large Chinook salmon combined size Escapement")
escapmentestall <- displayabundanceE(ta, pa, sedpall, NalljE, sedNall)

(escapementseparated <-rbind(escapmentestm1, escapmentestL1, escapmentestall))
(escapementoverall <-rbind(escapmentestm, escapmentestL, escapmentestall))

eagecompall    <-escapmentestall[14,]
vareagecompall <-escapmentestall[15,]*escapmentestall[15,]


cat("inriver run agecomp and SE Appendix A8 in report")
inrivercompall   <- hagecompall + eagecompall 
seinrivercompall <- sqrt(varhagecompall + vareagecompall) 
rbind(inrivercompall,seinrivercompall)

cat("Appendix currently not in report. Broodyear C3 in 2012 excel")
cat("Small and medium Chinook  salmon (< 660mm) Escapement")
escapmentestmby1 <- displayabundanceE(tmby1, pmby1, SEp1mbyjE, NmbyjE, sedNmedby )

cat("Large Chinook salmon  (>=660 mm MEF) Escapement")
escapmentestLby1 <- displayabundanceE(tLby1, pLby1, SEp1LbyjE, NLbyjE, sedNlargeby )

cat("Small and medium Chinook  salmon (< 660mm)     Escapement percent of total")
escapmentestmby  <- displayabundanceE(tmby, pmby, sedpmedby, NmbyjE, sedNmedby )

cat("Large             Chinook salmon (>=660 mm MEF) Escapement percent of total")
escapmentestLby  <- displayabundanceE(tLby, pLby, sedplargeby, NLbyjE, sedNlargeby )

cat("Small, medium and Large Chinook salmon combined size Escapement")
escapmentestallby <- displayabundanceE(taby, paby, sedpallby, NallbyjE, sedNallby)

(escapementbyseparated <- rbind(escapmentestmby1, escapmentestLby1, escapmentestallby))  
(escapementbyoverall   <- rbind(escapmentestmby,  escapmentestLby,  escapmentestallby))

(harvestesbyt      <- displayabundance(NbyjH, pNbyjH, SEpNbyjH, NbyjH, SENbyjH))

ebycompall    <- escapmentestallby[14,]
varebycompall <- escapmentestallby[15,]*escapmentestallby[15,]


cat("inriver run agecomp and SE for BY B8") 
inriverbycompall   <- hbycompall + ebycompall 
seinriverbycompall <- sqrt(varhbycompall + varebycompall) 
rbind(inriverbycompall,seinriverbycompall)

#In river calculations

justescapement      <- rbind(escapementoverall[c(4,9,14,19,24,29,34,39,44),])
justescapementse    <- rbind(escapementoverall[c(5,10,15,20,25,30,35,40,45),])
justescapementvar   <- justescapmentse*justescapmentse
justescapementby    <- rbind(escapementbyoverall[c(4,9,14,19,24,29,34,39,44),])
justescapementbyse  <- rbind(escapementbyoverall[c(5,10,15,20,25,30,35,40,45),])
justescapementbyvar <- justescapmentbyse*justescapmentbyse

justharvest      <- rbind(harvestoverall[c(4,9,14,19,24,29,34,39,44),])
justharvestse    <- rbind(harvestoverall[c(5,10,15,20,25,30,35,40,45),])
justharvestvar   <- justharvestse*justharvestse
justharvestby    <- rbind(harvestbyoverall[c(4,9,14,19,24,29,34,39,44),])
justharvestbyse  <- rbind(harvestbyoverall[c(5,10,15,20,25,30,35,40,45),])
justharvestbyvar <- justharvestbyse*justharvestbyse 

justinriverrun      <- justescapement + justharvest
justinriverrunse     <- sqrt(justescapementvar+justharvestvar)
inriverrun          <- rbind(justinriverrun[1,],justinriverrunse[1,],justinriverrun[2,],justinriverrunse[2,],
                             justinriverrun[3,],justinriverrunse[3,],justinriverrun[4,],justinriverrunse[4,],
                             justinriverrun[5,],justinriverrunse[5,],justinriverrun[6,],justinriverrunse[6,],
                             justinriverrun[7,],justinriverrunse[7,],justinriverrun[8,],justinriverrunse[8,],
                             justinriverrun[9,],justinriverrunse[9,])
rownames(inriverrun) <- c("female medium",  "female medium se",  
                          "male medium",    "female medium se",  
                          "combined medium","combined  medium se",
                          "female Large",   "female Large se",   
                          "male Large",     "male Large se",  
                          "combined Large", "combined Large se",
                          "female all",     "female all se",
                          "male all",       "male all se",
                          "combined all",   "combined all se")
inriverrun

justinriverrunby      <- justescapementby + justharvestby
justinriverrunbyse     <- sqrt(justescapementbyvar+justharvestbyvar)

inriverrunby          <- rbind(justinriverrunby[1,],justinriverrunbyse[1,],justinriverrunby[2,],justinriverrunbyse[2,],
                             justinriverrunby[3,],justinriverrunbyse[3,],justinriverrunby[4,],justinriverrunbyse[4,],
                             justinriverrunby[5,],justinriverrunbyse[5,],justinriverrunby[6,],justinriverrunbyse[6,],
                             justinriverrunby[7,],justinriverrunbyse[7,],justinriverrunby[8,],justinriverrunbyse[8,],
                             justinriverrunby[9,],justinriverrunbyse[9,])
rownames(inriverrunby) <- c("female medium",  "female medium se",  
                          "male medium",    "female medium se",  
                          "combined medium","combined  medium se",
                          "female Large",   "female Large se",   
                          "male Large",     "male Large se",  
                          "combined Large", "combined Large se",
                          "female all",     "female all se",
                          "male all",       "male all se",
                          "combined all",   "combined all se")
inriverrunby

 smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
 colnames(smoke) <- c("High","Low","Middle")
 rownames(smoke) <- c("current","former","never")
 smoke <- as.table(smoke)
 smoke

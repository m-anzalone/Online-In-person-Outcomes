#Final Project

#defines the variable "set" as "Most-Recent-Cohorts-Institution.csv" from the source file found at
#https://ed-public-download.app.cloud.gov/downloads/CollegeScorecard_Raw_Data_04192023.zip
#from the Department of Education's College Scorecard website
set <- read.csv(choose.files())

#defines "gradrate" as the numeric expression of the graduation rate from the data frame
gradrate <- as.numeric(set$C150_4_POOLED_SUPP)

#defines "median.10post" as the numeric expression of median income ten years after a student's first semester
median.10post <- as.numeric(set$MD_EARN_WNE_P10)
#cleans up the created income variable by replacing Null entries with "NA"
median.10post[which(median.10post=="")] <- NA

#defines "remote" as a binary variable representing whether a school is remote (remote = 1) or non-remote (remote = 0)
remote <- as.integer(set$DISTANCEONLY)

#defines "cost.pub" as the numeric expression of the yearly cost for public institutions
cost.pub <- as.numeric(set$NPT4_PUB)
#defines "cost.priv" as the numeric expression of the yearly cost for private institutions
cost.priv <- as.numeric(set$NPT4_PRIV)
#installs the "dpylr" package
install.packages("dplyr")
#calls the "dpylr" package from the library
library(dplyr)
#using the package "dpylr", defines "cost.yearly" as one merged column containing "cost.priv" and "cost.pub" 
cost.yearly <- coalesce(cost.priv, cost.pub)

#creates a boxplot comparing median income of graduates for remote and non-remote schools
boxplot(median.10post ~ remote, main = "Graduate Income for Remote and Non-remote Schools", xlab = "Remote (x = 1), Non-remote (x = 0)", ylab = "Median Income for Graduates", na.rm=TRUE)

#creates a boxplot comparing yearly cost for remote and non-remote schools
boxplot(cost.yearly ~ remote, main = "Yearly Cost for Remote and Non-remote Schools", xlab = "Remote (x = 1), Non-remote (x = 0)", ylab = "Yearly Cost" ,null.rm=T)

#creates a boxplot comparing graduation rate for remote and non-remote schools
boxplot(gradrate ~ remote, main = "Graduation Rate for Remote and Non-remote Schools", xlab = "Remote (x = 1), Non-remote (x = 0)", ylab = "Graduation Rate", null.rm=T)

#renames degree type variables to be more intuitive
#the file "Most-Recent-Cohorts-Institution.csv" was used as reference to find the variable names
#https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary.xlsx
agriculture <- as.numeric(set$PCIP01)
conservation <- as.numeric(set$PCIP03)
architecture <- as.numeric(set$PCIP04)
cultural.gend.stdy <- as.numeric(set$PCIP05)
comm.journ <- as.numeric(set$PCIP09)
comm.tech <- as.numeric(set$PCIP10)
comp.info.sci <- as.numeric(set$PCIP11)
culinary <- as.numeric(set$PCIP12)
education <- as.numeric(set$PCIP13)
engineering <- as.numeric(set$PCIP14)
engineering.tech <- as.numeric(set$PCIP15)
foreign.lang <- as.numeric(set$PCIP16)
family.cons.sci <- as.numeric(set$PCIP19)
legal <- as.numeric(set$PCIP22)
english <- as.numeric(set$PCIP23)
lib.arts.sci <- as.numeric(set$PCIP24)
library.sci <- as.numeric(set$PCIP25)
biomed <- as.numeric(set$PCIP26)
math.stats <- as.numeric(set$PCIP27)
millitary.tech <- as.numeric(set$PCIP29)
multidiscipline <- as.numeric(set$PCIP30)
parks.rec <- as.numeric(set$PCIP31)
phil.relig <- as.numeric(set$PCIP38)
theology.relig <- as.numeric(set$PCIP39)
phys.sci <- as.numeric(set$PCIP40)
sci.tech <- as.numeric(set$PCIP41)
psychology <- as.numeric(set$PCIP42)
homeland.sec <- as.numeric(set$PCIP43)
pub.admin <- as.numeric(set$PCIP44)
social.sci <- as.numeric(set$PCIP45)
construc.trade <- as.numeric(set$PCIP46)
mech.repair <- as.numeric(set$PCIP47)
precis.prod <- as.numeric(set$PCIP48)
transport <- as.numeric(set$PCIP49)
vis.perform.art <- as.numeric(set$PCIP50)
health <- as.numeric(set$PCIP51)
business.manag <- as.numeric(set$PCIP52)
history <- as.numeric(set$PCIP54)

#regresses median income for graduates with the explanatory variables for graduation rate, yearly cost, and the remote binary variable
reg1 <- lm(median.10post ~ gradrate+remote+cost.yearly, na.action = na.omit)
#regresses median income for graduates with the explanatory variables for graduation rate, yearly cost, the remote binarry variable, and the 38 degree type variables
reg2 <- lm(median.10post ~ gradrate+remote+cost.yearly+agriculture+conservation+architecture+cultural.gend.stdy+comm.journ+comm.tech+comp.info.sci+culinary+education+engineering+engineering.tech+foreign.lang+family.cons.sci+legal+english+lib.arts.sci+library.sci+biomed+math.stats+millitary.tech+multidiscipline+parks.rec+phil.relig+theology.relig+phys.sci+sci.tech+psychology+homeland.sec+pub.admin+social.sci+construc.trade+mech.repair+precis.prod+transport+vis.perform.art+health+business.manag+history, na.action = na.omit)

#prints a summary of the first regression
summary(reg1)
#prints a summary of the second regression
summary(reg2)

#References
#https://www.r-project.org/nosvn/pandoc/dplyr.html
#https://stackoverflow.com/questions/29634425/merging-two-columns-into-one-in-r
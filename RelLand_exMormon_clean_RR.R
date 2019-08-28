
setwd("C:/Users/crann/Desktop")
library(survey)
#install.packages("foreign")
#install.packages("dummies")
library(dummies)
library(foreign)
#install.packages("psych")
#install.packages("stargazer")
library(stargazer)
library("psych")

pew<-read.spss('Pew Research Center 2014 U.S. Religious Landscape Study.sav', to.data.frame = TRUE)

#Turns background religion into numeric to be able to handle it. 
pew$ch_rel_num<- (as.numeric(pew$QJ1))

#Turns background Mormon branch into numeric to be able to handle it. 
pew$ch_mo_num<- (as.numeric(pew$QJ3Q))

#Creating dummy variable if you were raised Mormon
pew$ch_mormon<-ifelse(pew$ch_rel_num == 3, 1, 0)

# Takes the non-response category and turns it into NA. 
pew$ch_mormon[pew$ch_rel_num== 79]<- NA

#Takes the non-response category and turns it into NA, turn other Mormon branches into 0. 
pew$ch_mormon[pew$ch_mo_num== 7]<- NA
pew$ch_mormon[pew$ch_mo_num== 2]<- 0
pew$ch_mormon[pew$ch_mo_num== 3]<- 0
pew$ch_mormon[pew$ch_mo_num== 4]<- 0
pew$ch_mormon[pew$ch_mo_num== 5]<- 0
pew$ch_mormon[pew$ch_mo_num== 6]<- 0

#Turns current religiosity into numeric
pew$rel_num<- (as.numeric(pew$QE1))

#Turns current religious branch into numeric
pew$mo_num<- (as.numeric(pew$QE3Q))

#Dichotomizes Mormon 
pew$mormon<-ifelse(pew$rel_num == 3, 1, 0)

#Takes the non-response category and turns it into NA, turn other Mormon branches into 0. 

pew$mormon[pew$rel_num== 79]<- NA
pew$mormon[pew$mo_num== 7]<- NA
pew$mormon[pew$mo_num== 2]<- 0
pew$mormon[pew$mo_num== 3]<- 0
pew$mormon[pew$mo_num== 4]<- 0
pew$mormon[pew$mo_num== 5]<- 0
pew$mormon[pew$mo_num== 6]<- 0

#Create ex-Mormon variable from derived ch_mormon and mormon variable
pew$exmormon <- ifelse((pew$ch_mormon ==1) & (pew$mormon==0), 1, 0) 

#Take out missings for ex-Mormon variable
pew$exmormon[is.na(pew$mormon)]<- NA
pew$exmormon[is.na(pew$ch_mormon)]<- NA

#Create Mormon convert variable from derived ch_mormon and mormon variable
pew$mormonconvert <- ifelse((pew$ch_mormon ==0) & (pew$mormon==1), 1, 0) 
pew$mormonconvert[is.na(pew$ch_mormon)]<- NA
pew$mormonconvert[is.na(pew$mormon)]<- NA

#USe this to figure out utah state indicator. 
pew$state_num<- (as.numeric(pew$STATE))

pew$UTAH<-ifelse((pew$state_num == 45) , 1, 0)
pew$UTAH[is.na(pew$state)]<- NA

pew$MALE[pew$SEX== "Male"]<- 1
pew$MALE[pew$SEX== "Female"]<- 0
table(pew$MALE, pew$SEX)

pew$EDUC<- (as.numeric(pew$EDUC))
pew$EDUC[pew$EDUC== 9]<- NA

pew$MARITAL[pew$MARITAL== "(VOL) Don't know/Refused"]<- NA
pew$marital_num<-as.numeric(pew$MARITAL)
table(pew$marital_num, pew$marital)
pew$married <- ifelse((pew$marital_num ==1), 1, 0) 
pew$cohabiting <- ifelse((pew$marital_num ==2), 1, 0) 
pew$divorced <- ifelse((pew$marital_num ==3), 1, 0)
pew$separated <- ifelse((pew$marital_num ==4), 1, 0)
pew$widowed <- ifelse((pew$marital_num ==5), 1, 0)
pew$nevermarried <- ifelse((pew$marital_num ==6), 1, 0)

pew$INCOME<- (as.numeric(pew$INCOME))
pew$INCOME[pew$INCOME== 10]<- NA

pew$FERTREC<-(as.numeric(pew$FERTREC))-1
pew$FERTREC[pew$FERTREC== 6]<- NA

pew$CHILDRENREC<-(as.numeric(pew$CHILDRENREC))-1
pew$CHILDRENREC[pew$CHILDRENREC== 5]<- NA

pew$RACETHN[pew$RACETHN== "Don't know/Refused (VOL)"]<- NA
pew$racethn_num<-as.numeric(pew$RACETHN)

pew$whitenh <- ifelse((pew$racethn_num ==1), 1, 0) 
pew$blacknh <- ifelse((pew$racethn_num ==2), 1, 0) 
pew$HISPANIC <- ifelse((pew$racethn_num ==3), 1, 0)
pew$otherrace <- ifelse((pew$racethn_num ==4), 1, 0)
table(pew$racethn, pew$otherrace)

pew$AGE<-(as.numeric(pew$AGEREC))
pew$AGE[pew$AGE== 16]<- NA

pew$ideo_num<-as.numeric(pew$IDEO)
pew$ideo_num[pew$ideo_num== 6]<- NA
pew$liberal<-pew$ideo_num

pew_mormonbackground<-subset(pew, ch_mormon ==1)
pew_mormongrand<-subset(pew, ch_mormon==1 | mormon==1)
pew_exmormon<-subset(pew, exmormon==1)
pew_lifemormon<-subset(pew, ch_mormon==1 & exmormon==0)
pew_mormon<-subset(pew, mormon==1)

life.mormon <- svydesign(ids = ~1, data = pew_lifemormon, weights = pew_lifemormon$WEIGHT)
small.mormon <- svydesign(ids = ~1, data = pew_mormonbackground, weights = pew_mormonbackground$WEIGHT)
all.design <- svydesign(ids = ~1, data = pew, weights = pew$WEIGHT)
grand.design <- svydesign(ids = ~1, data = pew_mormongrand, weights = pew_mormongrand$WEIGHT)
mormon.design<-svydesign(ids = ~1, data = pew_mormon, weights = pew_mormon$WEIGHT)
ex.mormon<-svydesign(ids = ~1, data = pew_exmormon, weights = pew_exmormon$WEIGHT)

################################### Who leaves the Church? 

###########About as many people convert to the Church as stay in the Church? Hand calculate T-tests. 
#https://www.graphpad.com/quickcalcs/ttest1/?Format=SEM

#estimates approximately 190  people (approximate due to decimal because of the weighting) with a Mormon background left compared to 379 who did not
svytable(~exmormon, design = small.mormon)

#Here about 172 members of the Church (versus the 379 that were born in the Church), are converts
svytable(~mormonconvert, design = mormon.design)

#which is not statistically significantly different from the number of people leaving. 
#https://www.graphpad.com/quickcalcs/ttest1/?Format=SEM

svymean(~  exmormon, design = all.design, na.rm=TRUE)
svymean(~  mormonconvert, design = all.design, na.rm=TRUE)

format(5e-04, scientific=FALSE)

#Where they went
#Also, use these and the t-test website above to test statistical significance below. 
wheretheywent<-as.data.frame(svytable(~RELTRAD, design = ex.mormon))
svymean(~  RELTRAD, design = ex.mormon, na.rm=TRUE)

#Table 1

exmormonmeans<-as.data.frame(svymean(~  AGE + MALE + RACETHN + UTAH + MARITAL + EDUC + INCOME + FERTREC + liberal, design = ex.mormon, na.rm=TRUE))
lifemormons<-as.data.frame(svymean(~  AGE + MALE + RACETHN + UTAH + MARITAL + EDUC + INCOME + FERTREC + liberal, design = life.mormon, na.rm=TRUE))

svyttest(AGE~exmormon, design = small.mormon)
svyttest(UTAH~exmormon, design = small.mormon)
svyttest(MALE~exmormon, design = small.mormon)
svyttest(whitenh~exmormon, design = small.mormon)
svyttest(blacknh~exmormon, design = small.mormon)
svyttest(HISPANIC~exmormon, design = small.mormon)
svyttest(otherrace~exmormon, design = small.mormon)
svyttest(married~exmormon, design = small.mormon)
svyttest(cohabiting~exmormon, design = small.mormon)
svyttest(divorced~exmormon, design = small.mormon)
svyttest(separated~exmormon, design = small.mormon)
svyttest(widowed~exmormon, design = small.mormon)
svyttest(nevermarried~exmormon, design = small.mormon)
svyttest(EDUC~exmormon, design = small.mormon)
svyttest(INCOME~exmormon, design = small.mormon)
svyttest(FERTREC~exmormon, design = small.mormon)
svyttest(liberal~exmormon, design = small.mormon)

#Table 2
model1<-svyglm(exmormon ~ MALE + AGE + UTAH , data= pew_mormonbackground, family=binomial, design= small.mormon)
summary(model1)

model2<-svyglm(exmormon ~ MALE + AGE + UTAH + MARITAL + RACETHN, data= pew_mormonbackground, family=binomial, design= small.mormon)
summary(model2)

model3<-svyglm(exmormon ~ MALE + AGE + UTAH + MARITAL + RACETHN + EDUC + INCOME, data= pew_mormonbackground, family=binomial, design= small.mormon)
summary(model3)

model4<-svyglm(exmormon ~ MALE + AGE + UTAH + MARITAL + RACETHN + EDUC + INCOME + FERTREC, data= pew_mormonbackground, family=binomial, design= small.mormon)

model5<-svyglm(exmormon ~ MALE + AGE + UTAH + MARITAL + RACETHN + EDUC + INCOME + liberal, data= pew_mormonbackground, family=binomial, design= small.mormon)

table2 <- stargazer(model1, model2, model3, model4, model5, 
                    title="Table 2: Predictors of having left the Church (log odds)" ,
                    column.sep.width = "2pt",
                    covariate.labels=c("Male", "Age", "Utah", "Cohabiting", "Divorced", "Separated", "Widowed", "Never been married", "Black non-Hispanic", "Hispanic", "Other race", "Education", "Income", "No. of children", "Politically liberal"),
                    type = "html",
                    dep.var.labels = "Ex-Latter-day Saint status",
                    style = "default", # 
                    digits=2,
                    notes.align = "l",
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    align= TRUE,
                    #   se=TRUE,
                    notes= c("Log odds are reported",
                             "Age: 1= <25, 2=25-29, 3= 30-34, 4=35-39, 5=40-44, 6=45-49, 7=50-54, 8= 55-59 9=60-64, 10=65-69, 11=70-74, 12=75-79, 13=80-84, 14=85-89, 15=90+",
                             "Education: 1= Less than HS, 2=Some HS, 3= HS graduate, 4=Some college, 5=Two-year degree, 6=Four-year degree, 7=Some postgraduate school, 8=postgraduate or professional degree",
                             "Income: 1=less than $10,000, 2= $10-20,000, 3=$20-30,000, 4=$30-40,000, 5=$40-50,000, 6= $50-75,000, 7=$75-100,000, 8=$100-150,000, 9= $150,000+",
                             "Politically liberal: 1= Very conservative, 2= Conservative, 3= Moderate, 4= Liberal, 5= Very liberal"),
                    out="ExMormon.html"
)

#Contrary to stereotypes about liberal ex-Mormons, many (27%) ex-Mormons still identify as politically conservative, with 39% identifying as political moderates, and only a minority (35%) identifying as politically liberal.
svytable(~exmormon+liberal, design=small.mormon)

#given Table 2, Model 2's parameters and using predicted probabilities a white non-Hispanic male who is married and living outside of Utah has a 26% chance of having left the Church

prediction1 <- with(pew_mormonbackground, data.frame(UTAH=0, AGE= mean(AGE, na.rm=TRUE), MALE= 1, MARITAL="Married", RACETHN="White non-Hispanic")) #Ref
prediction2 <- with(pew_mormonbackground, data.frame(UTAH=0, AGE= mean(AGE, na.rm=TRUE), MALE= 1, MARITAL="Married", RACETHN="Black non-Hispanic")) #Black
prediction3 <- with(pew_mormonbackground, data.frame(UTAH=0, AGE= mean(AGE, na.rm=TRUE), MALE= 1, MARITAL="Married", RACETHN="Other")) #Other race
predict(model2, prediction1, type="response")
predict(model2, prediction2, type="response")
predict(model2, prediction3, type="response")

#However, these effects are derived from very small cell sizes, with only .5 (due to weighting) Black members who have stayed versus the 5.5 who left. The “other race” category is perhaps a little more reasonable at 8 who have stayed and 20 who have left
svytable(~exmormon+RACETHN, design=small.mormon)

prediction1 <- with(pew_mormonbackground, data.frame(UTAH=0, AGE= mean(AGE, na.rm=TRUE), MALE= 1, MARITAL="Married", RACETHN="White non-Hispanic")) #Ref
prediction2 <- with(pew_mormonbackground, data.frame(UTAH=0, AGE= mean(AGE, na.rm=TRUE), MALE= 1, MARITAL="Divorced", RACETHN="White non-Hispanic")) #Black
predict(model2, prediction1, type="response")
predict(model2, prediction2, type="response")


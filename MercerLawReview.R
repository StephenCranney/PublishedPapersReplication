setwd("")
library(readxl)
rfra = read_xlsx("Abrams RFRA Data.xlsx")
library(car)
#install.packages("fastDummies")
library("fastDummies")
library(sjstats)

#################Tables presented in MS
Table1<-table(rfra$`Religion of applicant`, rfra$Outcome)

Table1<-table(rfra$`Religion of applicant`, )

write.csv(Table1, file = "Table1.csv") #Change atheist spelling and others. 


#Regression showing insignificance (see Table1 below)

#############Replicate her summary stats

#In total, 56.5% of plaintiffs lost, and 43.5% won
mean(rfra$Outcome)

#The most common responses were a motion to dismiss (34.8% of cases), a motion for summary judgment (30.4%),
#a motion for a preliminary injunction (13.9%), a motion for a permanent injunction (8.7%), or an appeal (frequently from a criminal conviction) (5.2%).
prop.table(table(rfra$Stage))

#Religion of the litigant was also coded. The most frequently occurring religions were Muslim (19.1%), non-Catholic Christian
#(12.2%), Catholic (11.3% of cases), secular religions, including a number of non-theological beliefs (10.5%), and individuals
#for whom the particular religion was not specified by the judge in deciding the case (10.4%).
prop.table(table(rfra$Religion.of.applicant))

####"Non-Catholic Christian" doesn't include Baptists or Mormons. 

#tests of correlation between each independent variable and case outcome were performed. 36
#Because the variables are categorical, a Chi-squared test was used. 37 Two results are statistically significant: there is
#an association between Context and Outcome (p=.001) and another *12 association between Exemption and Outcome
#(p=.024).

chisq.test(rfra$Context, rfra$Outcome)
chisq.test(rfra$requested.exemption, rfra$Outcome)

rfra$MUSLIM=0
rfra$MUSLIM[rfra$Religion.of.applicant=='Muslim'] <- 1

rfra$NRM=0
rfra$NRM[rfra$Religion.of.applicant=='Wiccan'] <- 1
rfra$NRM[rfra$Religion.of.applicant=='Asatru'] <- 1
rfra$NRM[rfra$Religion.of.applicant=='Hebrew Yisraelite'] <- 1
rfra$NRM[rfra$Religion.of.applicant=='Rastafarian'] <- 1
rfra$NRM[rfra$Religion.of.applicant=='Santeria'] <- 1
rfra$NRM[rfra$Religion.of.applicant=='Secular (Satanist)'] <- 1

rfra$HHS=0
rfra$HHS[rfra$Context=='HHS'] <- 1

table(rfra$requested.exemption)
rfra$contraception=0
rfra$contraception[rfra$requested.exemption=='Contraceptive mandate'] <- 1

rfra$CHRIST=0
rfra$CHRIST[rfra$Religion.of.applicant=='Christian'] <- 1
rfra$CHRIST[rfra$Religion.of.applicant=='Baptist'] <- 1
rfra$CHRIST[rfra$Religion.of.applicant=='Catholic'] <- 1
rfra$CHRIST[rfra$Religion.of.applicant=='Pentecostal'] <- 1
rfra$CHRIST[rfra$Religion.of.applicant=='Latter Day Saints'] <- 1

table(rfra$CHRIST, rfra$contraception)
table(rfra$contraception)


rfra$prose_num= ifelse(rfra$Pro.Se.=="Yes", 1, 0)

rfra$NativeA=0
rfra$NativeA[rfra$Religion.of.applicant=='Native American'] <- 1

rfra$TradChrist=0
rfra$TradChrist[rfra$Religion.of.applicant=='Christian'] <- 1
rfra$TradChrist[rfra$Religion.of.applicant=='Baptist'] <- 1
rfra$TradChrist[rfra$Religion.of.applicant=='Catholic'] <- 1
rfra$TradChrist[rfra$Religion.of.applicant=='Pentecostal'] <- 1

rfra$NonCath=0
rfra$NonCath[rfra$Religion.of.applicant=='Christian'] <- 1
rfra$NonCath[rfra$Religion.of.applicant=='Baptist'] <- 1
rfra$NonCath[rfra$Religion.of.applicant=='Pentecostal'] <- 1


rfra$SEC=0
rfra$SEC[rfra$Religion.of.applicant=='Humanist'] <- 1
rfra$SEC[rfra$Religion.of.applicant=='Secular'] <- 1
rfra$SEC[rfra$Religion.of.applicant=='Secular (pro life)'] <- 1
rfra$SEC[rfra$Religion.of.applicant=='Secular (Satanist)'] <- 1
rfra$SEC[rfra$Religion.of.applicant=='Secular (Environmentalism)'] <- 1
rfra$SEC[rfra$Religion.of.applicant=='Athiest'] <- 1
rfra$SEC[rfra$Religion.of.applicant=='Secular (Hawaii Cannabis Ministry)'] <- 1
rfra$SEC[rfra$Religion.of.applicant=='Secular (immigration)'] <- 1

rfra$PRIS=0
rfra$PRIS[rfra$Context=='Prison'] <- 1

rfra$PROSE=0
rfra$PROSE[rfra$Pro.Se.=='Yes'] <- 1

rfra$PRACT=0
rfra$PRACT[rfra$requested.exemption=='Practicing'] <- 1

#Contraception cases and outcomes
table(rfra$Outcome, rfra$contraception)

#Contraception cases and religious affiliation 

#Overfitting, small sample size, sensitivity analysis. 
#The most straightforward would be to simply test whether Christian cases can be statistically shown to be won at higher rates. 

#Sensitivity analysis. 
chisq.test(rfra$TradChrist, rfra$Outcome)
t.test(rfra$Outcome~rfra$TradChrist)

t.test(rfra$Outcome~rfra$NonCath)

t.test(rfra$Outcome~rfra$CHRIST)

t.test(rfra$Outcome~rfra$SEC)

###No statistically significant difference.

#prose_num, 

table(rfra$Pro.Se.)

#To account for each of the *15 categorical independent variables, k-1 dummy
#variables were created for each of Context, Exemption, Religion, and Stage.

table(rfra$Context)
table(rfra$requested.exemption)

#Variable(s) entered on step 1: (ProSe), (Circuit), SumJudge, Dismissal, PrelimInj, PermInj, Appeal, (Criminals HHS, Military,
#Prison, Immigration), Speech, Contraceptive Mandate, DrugUse, Appearance, EX_Immigration. LandUse, Employment, Practicing,)
#Christian, Catholic, Jewish, Muslim, Rastafarian, Unspecified, Secular, Sikh.

results <- fastDummies::dummy_cols(rfra, select_columns = c("Stage", "requested.exemption", "Context", "Religion.of.applicant"))

results$requested.exemption_CM=results$`requested.exemption_Contraceptive mandate`
results$`requested.exemption_Drug use`=0
results$`requested.exemption_Drug use`[results$`requested.exemption_Drug use (Heroin)`==1]<-1
results$`requested.exemption_Drug use`[results$`requested.exemption_Drug use (Marijuana)`==1]<-1

#Unable to replicate. 

model1 <- glm(Outcome ~ Pro.Se. + Circuit + Stage_Appeal + Stage_SumJudge + Stage_Dismissal + Stage_PrelimInj + Stage_PermInj + requested.exemption_Speech
           + `requested.exemption_Contraceptive mandate` + `requested.exemption_Drug use` +
                `requested.exemption_Appearance` + requested.exemption_Immigration + `requested.exemption_Land use` + requested.exemption_Employment +
                requested.exemption_Practicing + Religion.of.applicant_Christian + Religion.of.applicant_Catholic +  Religion.of.applicant_Jewish + Religion.of.applicant_Muslim + 
                Religion.of.applicant_Rastafarian + Religion.of.applicant_Unspecified + SEC + Religion.of.applicant_Sikh
              ,family=binomial(link='logit'),data=results)

model1 <- glm(Outcome ~ Pro.Se. + Stage_Appeal + Stage_SumJudge + Stage_Dismissal + Stage_PrelimInj + Stage_PermInj + requested.exemption_Speech
              + `requested.exemption_Contraceptive mandate` + `requested.exemption_Drug use` +
                `requested.exemption_Appearance` + requested.exemption_Immigration + `requested.exemption_Land use` + requested.exemption_Employment +
                requested.exemption_Practicing + NonCath + Religion.of.applicant_Catholic +  Religion.of.applicant_Jewish + Religion.of.applicant_Muslim + 
                Religion.of.applicant_Rastafarian + Religion.of.applicant_Unspecified + Religion.of.applicant_Secular + Religion.of.applicant_Sikh
              ,family=binomial(link='logit'),data=results)

summary(model1)

model1 <- glm(Outcome ~ Pro.Se. + Stage_Appeal + Stage_SumJudge + Stage_Dismissal + Stage_PrelimInj + Stage_PermInj + requested.exemption_Speech
              + `requested.exemption_Contraceptive mandate` + `requested.exemption_Drug use` +
                `requested.exemption_Appearance` + requested.exemption_Immigration + `requested.exemption_Land use` + requested.exemption_Employment +
                requested.exemption_Practicing + NonCath + Religion.of.applicant_Catholic +  Religion.of.applicant_Jewish + Religion.of.applicant_Muslim + 
                Religion.of.applicant_Rastafarian + Religion.of.applicant_Unspecified + SEC + Religion.of.applicant_Sikh
              ,family=binomial(link='logit'),data=results)

summary(model1)
vif(model1)

#Secular as well. 

#Bewildering number of covariates. 

#To summarize: 

#Despite trying several iterations, I was unable to replicate Abrams' (2019) findings. Probably due to lack of clarity about operationalizations of variables. 
#For example, Christianity. 

################################## 
#Table1
model1 <- glm(Outcome ~ TradChrist +`requested.exemption_Contraceptive mandate`,family=binomial(link='logit'),data=results)
model2 <- glm(Outcome ~ TradChrist +`requested.exemption_Contraceptive mandate` + SEC,family=binomial(link='logit'),data=results)
model3 <- glm(Outcome ~ CHRIST +`requested.exemption_Contraceptive mandate`,family=binomial(link='logit'),data=results)
model4 <- glm(Outcome ~ CHRIST +`requested.exemption_Contraceptive mandate` + SEC,family=binomial(link='logit'),data=results)

stargazer(model1, model2, model3, model4, 
          title="Table 1: Relationship between Christianity and outcomes (logit)", align=TRUE, type="text", 
          covariate.labels=c("Traditional Christians", "All Christians", "Contraception case", "Secular"),
          out= "/Users/stephencranney/Desktop/table1.txt")

#No non-Christian religionists win more? 
#What about Muslims? 

#Non-Christian religionists aren't any higher when secular is controlled for. 
model1 <- glm(Outcome ~ CHRIST + SEC,family=binomial(link='logit'),data=results)
summary(model1)

model1 <- glm(Outcome ~ MUSLIM + SEC,family=binomial(link='logit'),data=results)
summary(model1)

model1 <- glm(Outcome ~ NativeA + SEC,family=binomial(link='logit'),data=results)
summary(model1)

#NRMs both with and without secular Satanists (edge case).
model1 <- glm(Outcome ~ NRM + SEC,family=binomial(link='logit'),data=results)
summary(model1)

model1 <- glm(Outcome ~ Pro.Se. + Circuit + Stage_Appeal + Stage_SumJudge + Stage_Dismissal + Stage_PrelimInj + Stage_PermInj + requested.exemption_Speech
              + `requested.exemption_Contraceptive mandate` + `requested.exemption_Drug use` +
                `requested.exemption_Appearance` + requested.exemption_Immigration + `requested.exemption_Land use` + requested.exemption_Employment +
                requested.exemption_Practicing + Religion.of.applicant_Christian + Religion.of.applicant_Catholic +  Religion.of.applicant_Jewish + Religion.of.applicant_Muslim + 
                Religion.of.applicant_Rastafarian + Religion.of.applicant_Unspecified + SEC + Religion.of.applicant_Sikh
              ,family=binomial(link='logit'),data=results)

#3.             Are any religious groups over- or under-represented as a share of population?

#Christians under-represented
table(rfra$CHRIST)
binom.test(31, 115, p = 0.65,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)
#In U.S., Decline of Christianity Continues at Rapid Pace Pew Research Center, 2019

#Muslims over-represented
table(rfra$MUSLIM)
binom.test(22, 115, p = 0.008,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)
# "New estimates show U.S. Muslim population continues to grow". Pew Research Center. January 3, 2018. Retrieved August 16, 2018.

#NRMs over-represented
table(rfra$NRM)
binom.test(13, 115, p = 0.012,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)
# https://commons.trincoll.edu/aris/files/2011/08/ARIS_Report_2008.pdf ARIS NRMs and "other" religions are 1.2 (not including mormons)

#Seculars under-represented (barely, at .02). 
table(rfra$SEC)
binom.test(16, 115, p = 0.23,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)
# https://gssdataexplorer.norc.org/trends/Religion%20&%20Spirituality?measure=relig_rec   23% in 2018

#https://www.gc.cuny.edu/CUNY_GC/media/CUNY-Graduate-Center/PDF/ARIS/ARIS-PDF-version.pdf

#
table(rfra$requested.exemption)

#NRMs both with and without secular Satanists (edge case).
model1 <- glm(Outcome ~ NRM + SEC,family=binomial(link='logit'),data=results)
summary(model1)

model1 <- glm(Outcome ~ Pro.Se. + Circuit + Stage_Appeal + Stage_SumJudge + Stage_Dismissal + Stage_PrelimInj + Stage_PermInj + requested.exemption_Speech
              + `requested.exemption_Contraceptive mandate` + `requested.exemption_Drug use` +
                `requested.exemption_Appearance` + requested.exemption_Immigration + `requested.exemption_Land use` + requested.exemption_Employment +
                requested.exemption_Practicing + Religion.of.applicant_Christian + Religion.of.applicant_Catholic +  Religion.of.applicant_Jewish + Religion.of.applicant_Muslim + 
                Religion.of.applicant_Rastafarian + Religion.of.applicant_Unspecified + SEC + Religion.of.applicant_Sikh
              ,family=binomial(link='logit'),data=results)

#Control for contraception

model1 <- glm(Outcome ~ contraception + SEC,family=binomial(link='logit'),data=results)
summary(model1)

model4 <- glm(Outcome ~ contraception + SEC + TradChrist,family=binomial(link='logit'),data=results)
summary(model4)

model2 <- glm(Outcome ~ contraception + SEC + CHRIST,family=binomial(link='logit'),data=results)
summary(model2)

model3 <- glm(Outcome ~ contraception + SEC + MUSLIM,family=binomial(link='logit'),data=results)
summary(model3)

#Control for prison cases--doesn't appear to be going anywhere. 

model3 <- glm(Outcome ~ PRIS, family=binomial(link='logit'),data=results)
summary(model3)

model3 <- glm(Outcome ~ contraception + SEC + PRIS,family=binomial(link='logit'),data=results)
summary(model3)

#Control for pro se cases--doesn't appear to be going anywhere either.  

model3 <- glm(Outcome ~ PROSE, family=binomial(link='logit'),data=results)
summary(model3)

model3 <- glm(Outcome ~ PROSE + SEC + contraception,family=binomial(link='logit'),data=results)
summary(model3)

#Control for Practicing  

model3 <- glm(Outcome ~ PRACT, family=binomial(link='logit'),data=results)
summary(model3)

model3 <- glm(Outcome ~ PRACT + SEC + contraception,family=binomial(link='logit'),data=results)
summary(model3)

#Control for HHS-sig but collinear with contraception mandate

model3 <- glm(Outcome ~ HHS, family=binomial(link='logit'),data=results)
summary(model3)

model3 <- glm(Outcome ~ HHS + SEC + contraception,family=binomial(link='logit'),data=results)
summary(model3)

model3 <- glm(Outcome ~ SEC + contraception,family=binomial(link='logit'),data=results)
summary(model3)

#Context for different religions. 

table(rfra$MUSLIM, rfra$PRIS)
table(rfra$NRM, rfra$PRIS)
chisq.test(rfra$NRM, rfra$PRIS)
chisq.test(rfra$MUSLIM, rfra$PRIS)

table(rfra$PRIS, rfra$Pro.Se.)
chisq.test(rfra$PRIS, rfra$Pro.Se.)

chisq.test(rfra$Outcome, rfra$Pro.Se.)
table(rfra$Outcome, rfra$Pro.Se.)

chisq.test(rfra$SEC, rfra$Pro.Se.)

table(rfra$SEC, rfra$PRIS)












##Purpose:	Produce ORs for case-control study using 2x2 table and logistic regression
##Date:		07/25/2023	
##Author: Sophie Zhu
#We'll use data from case-control studies (unmatched and matched) used in Salmonella outbreak 
#investigations to get ORs for various exposures

#Since we have Excel files, we need a special package to read it in called readxl
library(readxl)
salm<-read_xlsx("C:/Users/URI0/Downloads/SalmonellaOutbreak_caco.xlsx")

#Use describe() to get an overview of all the data
describe(salm)

#We will take a look at CANDY and PREDOUGH first using frequency tables
#NOTE: Those with value of '99' (unknown) have been set to missing;
#(May want to do sensitivity analyses, assigning these as 'YES' and 'NO' and evaluating);

table(salm$CASE, salm$PREDOUGH)
table(salm$CASE, salm$CANDY)

#See your tables with NAs - add useNA="always"
table(salm$RESPONDENT, useNA="always")
table(salm$CASE, salm$CANDY, useNA = "always")
table(salm$CASE, salm$CANDY, useNA = "always")

#***Mantel-Haenszel OR;
#In this case-control study, we think that age is a confounder of candy/case status relationship and would like to adjust for age group;

#What is the crude OR?
fisher.test(ftable(salm$CASE, salm$PREDOUGH))
fisher.test(ftable(salm$CASE, salm$CANDY))

#Produce table for each stratum (age group);
table(salm$CASE, salm$CANDY, salm$AGEGRP)
age_salm = xtabs(CASE ~ CANDY + AGEGRP, data=salm)

#Can produce Mantel-Haenszel statistics with epiDisplay package
install.packages("epiDisplay")
#open up new library
library(epiDisplay)

#mhor is the function for the MH OR calculation. Your x variable (CASE) comes first, followed by your other variable and strata
mhor(salm$CASE, salm$CANDY, salm$AGEGRP, graph=F)
#MH age adjusted OR =0.5414

#Result from logistic regression
casecandy<-glm(CASE ~ CANDY, data=salm, family= "binomial")
exp(coef(casecandy))
#OR=0.597

#BUT we want to adjust for AGEGRP here;
#We can do this by also including AGEGRP in the model;
casecandy_adj<-glm(CASE ~ CANDY + AGEGRP, data=salm, family= "binomial")
exp(coef(casecandy_adj))
#OR=0.554

#OR for sex
casesex<-glm(CASE ~ SEX, data=salm, family= "binomial")
exp(coef(casesex))

#Separate ORs for M and F: the separate OR for F is just SEXM minus the intercept
#OR F = 1.502-0.838 = 0.664

#Let's make agegrp a factor so we can see age specific values
salm$AGEGRP<-as.factor(salm$AGEGRP)
glm(CASE ~ CANDY + AGEGRP, data=salm, family= "binomial")

#Or separate ORs for M and F, controlling for AGEGRP
age_salm_sexmodel<-glm(CASE ~ CANDY + SEX + AGEGRP, data=salm, family= "binomial")
exp(coef(age_salm_sexmodel))

#Exact logistic regression OR=0.543

#Firth penalized maximum likelihood
#Base R does not have firth penalized likelihood so we need to get a new package
install.packages("logistf")
library(logistf)
firth_case_model<-logistf(CASE ~ CANDY + SEX + AGEGRP, data=salm, family= "binomial")
exp(coef(firth_case_model))
#Firth penalized OR=0.547

##Matched case-control study;
salm_matched<-read_xlsx("C:/Users/URI0/Downloads/SalmonellaOutbreak_matched_caco.xlsx")
describe(salm_matched)

#Organic has 37 missing, there's missing and 9, let's recode those as NA for analysis purposes
library(dplyr) #need this package in order to use case_when
salm_matched$Organic<-case_when(salm_matched$Organic == 9 ~ NA,
                                salm_matched$Organic == 1 ~ 1,
                                salm_matched$Organic == 0 ~ 0)

#CaCo is character binary 0/1, 1:1 match
ftable(salm_matched$CaCo, salm_matched$Organic)

##########Work in progress, epiR cannot be used because it depends on sf, which I currently cannot download######################
library(epiR)
epi.2by2(mytable, method = "case.control")
*Ratio of discordant pairs (only case exposed/only control exposed) is matched OR;
*Can produce tables for each Matched_pair;
PROC SORT DATA = caco; BY Matched_pairs; RUN;
PROC FREQ DATA =caco;
WHERE Organic not in (., 9) and Matched_pairs in (1, 62);
TABLES Organic*CaCo;
BY Matched_pairs;
RUN;

*We could also reformat the data and look at matched sets (strata);
*This PROC TRANSPOSE gets matched pairs on the same line;
PROC TRANSPOSE DATA = caco OUT = caco_wide PREFIX = caco;
BY Matched_pairs;
ID caco;
VAR Organic;
RUN;

DATA caco_wide2;
SET caco_wide;
RENAME caco0 = Control caco1 = Case;
RUN;

*As we did above, lets format variables to ensure that they display properly in table;
PROC FORMAT;
VALUE exp 1 = 'Exposed' 0 = 'Unexposed';
RUN;

*PROC FREQ on the transposed dataset gives a table for matched pairs;
PROC FREQ DATA = caco_wide2 ORDER = formatted;
WHERE Case not in (., 9) and Control not in (., 9);
TABLES Case*Control;
FORMAT Case exp. Control exp.;
ODS SELECT CrossTabFreqs;
RUN;
*Matched OR = ?;

*Compare to M-H OR;
PROC SORT DATA = caco; 
BY DESCENDING CaCo DESCENDING Organic;
RUN;
PROC FREQ DATA = caco ORDER = data;
WHERE Organic not in (., 9);
TABLES Matched_pairs*Organic*caco/ CMH; 
RUN;


PROC LOGISTIC DATA = caco descending;
WHERE Organic not in (., 9);
MODEL caco = Organic;
STRATA Matched_pairs;
EXACT Organic /CLTYPE = exact ESTIMATE = both; 
RUN;

##########################################################
#Exercises;

#A: Matched case-control;

#Using the SAS macro "mcaco" (which uses "caco" dataset"), produce matched OR and CI for Pizza.
#What are the matched odds ratio and associated confidence interval? Would you report
#the Wald or exact results? Why?;

#####Have not finished running through the macro, will update later on

#For the following questions, go back to the salm dataset (unmatched);
#B: Unadjusted ORs;
#1) Produce table for cereal by case (CEREAL*CASE) using PROC FREQ, with rows and columns 
#in default order. Exclude any missing or unknown values. What is the OR?;

ftable(salm$CASE, salm$CEREAL)
fisher.test(salm$CASE, salm$CEREAL)
#OR=3.879

#2) Produce another table with "Exposed" before "Unexposed" and "Case" before "Control." What is the 
#OR now? Is it the same or different than the one produced above?;
#This would give you a different answer but since R automatically recognizes the pairing of case and 1, we don't really need to run through this
#If you want to specify something else as your reference group. In the code below, the new reference group is 3, not 1
#dataframeofinterest <- within(dataframeofinterest, variable <- relevel(variable, ref = 3))

#3) Instead, use PROC LOGISTIC to find exposure OR for CEREAL. 
#Compare this OR to the one you found in B2 above

cereal_model<-glm(CASE~CEREAL, data=salm, family="binomial")
exp(coef(cereal_model))
#OR=3.885

###########################################
#C: Adjusted ORs;

#1) Request the Mantel-Haenszel OR for CEREAL, adjusting for AGEGRP, using PROC FREQ. How do ORs for individual tables compare with adjusted OR?	

fisher.test(salm$CASE, salm$CEREAL)		
mhor(salm$CASE, salm$CEREAL, salm$AGEGRP, graph=F)
#OR for MH is lower than the Fisher exact test -> 3.395


#2) Use PROC LOGISTIC to find exposure OR for CEREAL, adjusted for AGEGRP. Compare this OR to the Mantel-Haenszel OR found using PROC FREQ in C1 above

cereal_ageadjust<-glm(CASE~CEREAL + AGEGRP, data=salm, family="binomial")
exp(coef(cereal_ageadjust))
#Logistic regression gives us a even lower OR at 3.21
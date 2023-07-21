#Introduction to R
#Author: Sophie Zhu
#Purpose: Read in 2018 natality data and use R procedures to explore

#Date created: 7/19/2023

#R comments use the # sign
#Basic R and RStudio setup
#https://cengel.github.io/R-intro/backgroud.html#knowing-your-way-around-rstudio

#IMPORTANT THINGS TO REMEMBER;
#SAS Statements end in semicolons; R does not require these
#SAS recognizes single (') and double (") quotes, but they must match; R is the same
#SAS does not care if you have spaces between words, but it is best to keep a neat program; It's best in R to use _ or . between words as spaces can be kind of funky
#Generally, SAS is not case-sensitive; R is case sensitive
#Variable and dataset names can be a combination of letters, numbers, and underscores, but they cannot start with numbers; Make your life easier and don't start your variable and datasets with numbers in R
#SAS color-codes words, so pay attention to colors; R doesn't necessarily do this but you can change your global environment so that certain parts of functions are different colors
#After you run code, remember to check the log!; You can scroll up in the console in R to see what you ran, or use the "History" tab in the top right

#Common R mistakes
#Misspelling words, functions, datasets, forgetting to close quotes or parentheses, not commenting
#Commenting will save future you a lot of headache - it is easy to forget why you performed an analysis a certain way 8 months after you last touched the code and need to address reviewer concerns

######################################

#Data was downloaded from https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm
#(note that random sample was taken, some changes were made, and mortality and ID were generated);

#See User Guide here: ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/DVS/natality/UserGuide2018-508.pdf;

#All vars: 
#ID (generated) and residential status:		ID restatus 								
#DOB variables: 								dob_yy dob_tt dob_mm dob_wk 
#Mother characteristics: 					mager m_ht_in bmi wtgain mbstate_rec
#Cigarettes before and during pregnancy: 	cig_0 cig_1 cig_2 cig_3 cig_rec 
#Baby characteristics: 						dbwt sex apgar5 bfed 
#Interval since last live birth: 			illb_r
#Facility/Attended by: 						bfacil3 attend
#Race/Ethnicity of Mother and Father: 		mrace6 frace6 mracehisp fracehisp
#Risks, anitbiotics, prenatal care:			no_risks ab_anti precare5
#Marriage/paternity: 						dmar mar_p 
#Mortality (generated variable): 			mort

#Section A: Read in and save dataset;
#I am saving this dataset in my Downloads folder but you can use whatever location you would like, just make sure that you can identify the folder path, the same as SAS

#Data import is easier in R than SAS in my opinion, see this following code:

nat2018 <- read.csv("C:/Users/URS3/OneDrive - CDC/Summer Course/nat2018samp_clean.csv")

#Make sure to put your 4 digit CDC ID where YOURIDHERE is.

#You do not need to save a R version of the dataset every single time - you should be able to load and run this code each time, or if you do not want to do so there also an option to save a "Workspace" which will re-load all of your data and analyses from your last work session
#https://www.rfaqs.com/tag/save-and-load-r-workspace-object/

#Section B: Explore dataset;
#There isn't a base 1:1 equivalent of PROC CONTENTS in R, but I usually like head(), summary(), or table() for looking at data initially. Here are a bunch of options.

#head prints the first few lines of your dataset
head(nat2018)

#summary spits out summary statistics for your variables, including telling you mean, median, character type, and number of missing datapoints (NA)
summary(nat2018)

#lapply tells you each variable type
lapply(nat2018, class)

#		i.	How many observations and variables do you have in your dataset?
#You don't really need to run anything to see this, the number of observations and variables should be visible under your Environment tab. 
#If you have larger datasets or just want to have everything in one place, use nrow (number of rows) for observations and ncol (number of columns) for variables.
nrow(nat2018)
ncol(nat2018)

#A more advanced alternative is to install a SAS-like package which has a similar function to PROC Content
install.packages("Hmisc")
install.packages('checkmate')
#load in the library you just downloaded
library(Hmisc)

describe(nat2018)

#		ii.	When was the dataset created?
#This is code you need to see dataset creation but I've never had to do this so not sure why they asked for it. 
#Nothing shows up for me even though this was the dataset they emailed us
file.info("C:/Users/YOURIDHERE/Downloads/nat2018samp_clean.csv")$ctime

#		iii.	Is the dataset sorted?
#If you open the dataset you can see it's sorted by ID, but otherwise no
nat2018

#		iv.	Is the variable “ID” character or numeric? 
class(nat2018$ID)
#		v.	Try running the PROC CONTENTS without the “VARNUM” option. How does the output differ?;
#I'm unfamiliar with this so skipping

######################

#Section C: Explore variables;

#a) Categorical variables (can be numeric or character);
#This provides counts for the sex variable
ftable(nat2018$sex)
table(nat2018$sex)

#i.	What is the percent female?
#One way to get this answer is using the Hmisc package I talked about earlier, which is pretty much a 1:1 equivalent for SAS PROC Content
library(Hmisc)
describe(nat2018)

#Another method
prop.table(table(nat2018$sex))
#Table with rounding to 2 digits and percentage versus decimals
round(100*prop.table(table(nat2018$sex)),digits = 2)

#ii.	Try adding the name of another categorical variable next to “sex” and run the code again.
#creating a new datatable so I don't have to type out as much
baby_sextable<-table(nat2018$sex, nat2018$bfed)
prop.table(baby_sextable)

#I don't love the way that data is explored in SAS, there are definitely easier and smoother ways in R.

#b) Numeric/continuous variables;
#Automatically output minimum, 25%tile, median, mean, 75%tile, and max for the "mager" variable
summary(nat2018$mager)
hist(nat2018$mager)

#If you want other quantiles, use the quantile() function
quantile(nat2018$mager, c(0.05)) 

#Run the PROC UNIVARIATE code and answer the following questions about the variable “mager.”
#1.	What are the mean and median? What is the 5th percentile?
#29.02, 29, 20
#2.	How many observations have the highest value of 50?
sum(nat2018$mager==max(nat2018$mager))
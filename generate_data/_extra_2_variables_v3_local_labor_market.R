### Project 1: File 3: create measures + Multiverse ############################

#This file run a multiverse analysis where we create and re-create the data
#over a range of combinations. Lastly we run the models and plot the results. 



################################################################################

#Clean the environment
rm(list=ls())

#Extract data from SQl database
setwd("//micro.intra/projekt/P0515$/P0515_Gem/Karl/NiklasKarl/project1/version3_socio_2oct2023_v1/")
getwd()

#Load packages
library(RODBC)
library(dtplyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(lfe)
library(stargazer)
library(sandwich)
library(modelsummary) 
library(broom)
library(foreach)
library(doParallel)
library(fixest)


DT <- fread("data/project1_combined_v3.csv", sep = ",", header = TRUE)

########## Draw a sample to test code ##############
#DT_largedata - name the imported data: DT_largedata
# set.seed(123)
# subset_size <- 1000000
# DT <- as.data.table(DT_largedata %>% sample_n(subset_size))
####################################################

### Import Local labor Markets: LA to replace mun
la92 <- fread("imported/la92.csv", sep = ",", header = TRUE)
la98 <- fread("imported/la98.csv", sep = ",", header = TRUE)

la92[, c("la92lab", "munlab", "V5", "V6", "V7") := NULL]
la98[, c("la98lab", "munlab") := NULL]
DT <- merge(DT, la92, by=c("mun"), all.x=TRUE)

DT[, mun := NULL]
setnames(DT, "la92", "mun")
DT <- na.omit(DT, cols=c("mun"))

######## Prepare data ##########################################################

### Time period ###
DT <- DT[year >= 1992 & year <= 2017, ]
years <- 1992:2017
# Upper limits: | 1998 | 2012 | 2017

################################################################################

#Rank municipalitites based on their relative size (nr of individuals)
mun_work_pop_perc <- as.data.frame(DT) %>%
    group_by(year, mun) %>% 
    summarise(mun_pop = n()) %>%
    select(year, mun, mun_pop) %>%
    group_by(year) %>%
    mutate(percentile_rank = rank(mun_pop)/length(mun_pop))

DT <- merge(DT, mun_work_pop_perc, by = c("mun", "year"), all.x = TRUE)
rm(mun_work_pop_perc)

################################################################################
#### Theoretically justified drops + Multiverse analysis

## Multiverse set-up

# 1. Employment situation
# 2. Industries
# 3. Salaries
# 4. Public Sector
# 5. Organization size
# 6. City-type

#All combinations: Create all possible combinations in the multiverse analysis
# occ <- c(0, 2) #var1
# ind <- c(0, 9) #var2
# salaries <- c(0, 2500, 5000, 1000) #var3
# pub_sec <- seq(0, 0.5, 0.1) #var4
# org_size <- c(500, 5000, 10000) #var5
# city <- seq(0, 0.5, 0.1) #var6
# private <- c(0, 2) # var7
# choices <- expand.grid(occ,ind, salaries, pub_sec, org_size, city, private)
# colnames(choices) <- c("acc", "ind", "salaries", "pub_sec", "org_size", "city", "private")

#Selected combinations
occ <- c(2) #var1
ind <- c(9) #var2
salaries <- c(0) #var3
pub_sec <- c(0.15) #var4
org_size <- c(5000) #var5 5000
city <- c(0) #var6
private <- c(1) # var7
choices <- expand.grid(occ,ind, salaries, pub_sec, org_size, city, private)
colnames(choices) <- c("acc", "ind", "salaries", "pub_sec", "org_size", "city", "private")

DT_loop <- data.table()







# Choices: 1-4, and 6 ######################################################
# Variable 1: occ (Yrkesposition). Base parameter, keep == 2
if (choices[1] == 2){
    DT_loop <- DT[YrkStalln == 2]
}
unique(DT_loop$YrkStalln)
DT_loop[, YrkStalln := NULL]

# Variable 2: ind (Industry/sni). Base parameter, keep >= 9
DT_loop <- DT_loop[sni >= choices[,2]]
sort(unique(DT_loop$sni))

# Variable 3: salaries (income)
DT_loop <- DT_loop[income >= choices[,3]]
summary(DT_loop$income)

# Variable 4: pub_sec (Base parameter = Drop industries with more than 15% public employees)
dropind15 <- DT_loop[, .(nsni = .N),  by = .(private, sni, year)] #Count N
dropind15 <- reshape(dropind15, idvar=c("sni", "year"), timevar="private", direction = "wide") #Transform to wide
dropind15 <- setnames(dropind15, "nsni.1", "n_priv") #Rename
dropind15 <- setnames(dropind15, "nsni.0", "n_pub") #Rename
dropind15[is.na(dropind15) ] <- 0 #Set NA to 0
dropind15[, share_public := n_pub/(n_priv+n_pub)] #Share public employees in industries (in relation to total employees in industry)
DT_loop <- merge(DT_loop, dropind15, by = c("sni", "year"), all.x = TRUE) #Merge to DT
DT_loop <- DT_loop[!share_public > choices[,4],] #Exclude industries public sector employment more than choices
summary(DT_loop$share_public)
rm(dropind15)

#Variable 6: Ranked list of municipalities (mun/Kommunkod) 
# DT_loop <- DT_loop[ percentile_rank >= choices[,6]]
# unique(DT_loop$percentile_rank)

# Variable 7: private (drop all individuals with a public employment). Base parameter, drop == 0
if (choices[, 7] == 1){
    DT_loop <- DT_loop[private == 1]
}
unique(DT_loop$private)
DT_loop[, private := NULL]

############################################################################
# Variable construction: 1) dependent variables, 2) independent variables
############################################################################


########## Dependent variables: Notes on dependent variables ###################################
# The summary stat varies depending on if log+1 is used (which was used in the first version, but not this),
# or whether income is scaled for all variables or only grosswage (which was the case in the first version)

#Close to the paper:
# no log+1 - there are only a few 0s <1000 of 59 000 000
# scale income: only gross. Better to go with scaling all? consistency. 


##### Dependent variables below #####
DT_loop <- DT_loop[, scaledwage := income/100] ##Scaled income: to reflect salaries counted in 100s of krona


#DV1: Gross Wage, Mean Wage and Wage log
################################################################################
DT_loop <- DT_loop[, grosswage_sd := sd(scaledwage, na.rm=TRUE), by=c("sni", "mun", "year")][is.na(grosswage_sd), grosswage_sd := 0] ##Industry-region-year
#log +1
count_z <- sum(DT_loop$grosswage_sd == 0, na.rm=TRUE)
DT_loop <- DT_loop[grosswage_sd !=0]
DT_loop <- DT_loop[, grosswage_ln := log(grosswage_sd)] ##DV1: Log SD wages

#Note: scaledwage
DT_loop <- DT_loop[, meanwage := mean(scaledwage, na.rm=TRUE), by=c("sni", "mun", "year")] ##Industry-region-year: scaledwage instead??????????????
#log +1
count_z <- sum(DT_loop$meanwage == 0, na.rm=TRUE)
DT_loop <- DT_loop[meanwage !=0]
DT_loop <- DT_loop[, meanwage_ln := log(meanwage)] ##VAR2 S&S = Log mean wages (table 2)

DV1_grosswage <- subset(unique(DT_loop, by = c("sni", "mun", "year"))) #Keep unique values: sni, mun and year
DV1_grosswage <- DV1_grosswage[, c("sni", "mun", "year", "grosswage_ln", "meanwage_ln")] #Keep only relevant variables
#314 736 observations

#Drop NAs across all relevant variables. 
DV1_grosswage <- na.omit(DV1_grosswage, cols=c("grosswage_ln"))
#298 208 observations (old: 1992-2017)
#223 351 observations (1992-2017)


#DV2: Residual Wage
################################################################################
# log +1 and scaled = scaledwage
count_z <- sum(DT_loop$scaledwage == 0, na.rm=TRUE)
DT_loop <- DT_loop[scaledwage !=0]
DT_loop <- DT_loop[, wage_ln := log(scaledwage)] ##DV in individual level regressions used to compute DV2: residual.
DT_loop <- DT_loop[, workexptwo := workexp*workexp] #Create squared variable to be used in the regression




### ols regression
gc()
preliminar = data.table()
for (i in years) {
    df <- DT_loop[year == i] #Keep only one year
    df = data.table(df) #Create data.table
    model <- feols(wage_ln ~ education + age + gender + workexp + workexptwo + children0_3*married | sni + mun, data = df, vcov="twoway")
    df[, resid := residuals(model)] #store residuals: clustered standard errors
    df[, year := i] #add year
    l = list(preliminar, df) #join to DT
    preliminar <- rbindlist(l) ##Save new data file
    # DT2 <- as.data.table(preliminar)
}
gc()

DV2_residwage <- preliminar
DV2_residwage <- DV2_residwage[, residwage := sd(resid, na.rm=TRUE), by=c("sni","mun","year")][is.na(residwage), residwage := 0] ##Industry-region-year
#log+1
#DV2_residwage <- DV2_residwage[residwage !=0]
count_z <- sum(DV2_residwage$residwage == 0, na.rm=TRUE)
DV2_residwage <- DV2_residwage[residwage !=0]
DV2_residwage <- DV2_residwage[, residual_ln := log(residwage)] ##Log residual wage

# Adjusting for sampling error  
DV2_residwage <- DV2_residwage[, n_snimun := .N, by=c("sni","mun","year")] ##Industry-region-year
DV2_residwage <- DV2_residwage[, residwage_s := residwage + 1/(2*n_snimun), by=c("sni","mun","year")] ##Industry-region-year
DV2_residwage[, n_snimun := NULL]
#log+1
count_z <- sum(DV2_residwage$residwage_s == 0, na.rm=TRUE)
DV2_residwage <- DV2_residwage[residwage_s !=0]
DV2_residwage <- DV2_residwage[, residual_ln_s := log(residwage_s)] ##Log residual wage

DV2_residwage <- DV2_residwage[, residwage := NULL]
DV2_residwage <- DV2_residwage[, residwage_s := NULL]
DV2_residwage <- subset(unique(DV2_residwage, by = c("sni","mun","year"))) #Keep unique values: sni, mun and year
DV2_residwage <- DV2_residwage[, c("sni","mun","year", "residual_ln", "residual_ln_s")] #Keep only relevant variables
rm(preliminar, df, model, l)

#Drop NAs across all relevant variables. 
DV2_residwage <- na.omit(DV2_residwage, cols=c("residual_ln"))
#298 208 observations (old)
#223 351 observations (1992-2017)

# summary(DV1_grosswage$grosswage_ln)
# sd(DV1_grosswage$grosswage_ln)
# 
# summary(DV1_grosswage$meanwage_ln)
# sd(DV1_grosswage$meanwage_ln)
# 
# summary(DV2_residwage$residual_ln)
# sd(DV2_residwage$residual_ln, na.rm=TRUE)
# gc()




##### Independent variables below ##############################################

# 1) Individual level
# 2) Firm level
# 3) Industry-region level

################################################################################


# 1) Individual level variables. Measures at the individual level using the sample defined above: for independent variables
################################################################################
DT_ind <- DT_loop[, c("ind", "org" ,"year", "sni", "mun", "foreign", "profit")] ##Added individual
rm(DT_loop)
gc()

#Organizational size: 
#e14_orgsize and e15_largeorg (which will be updated further down)
DT_orgsize <- DT_ind[, .(orgsize_ind = .N),  by = .(org, year)] #Count N individuals


# Choice 5!
DT_orgsize[, largeorg := 0] [orgsize_ind >= choices[,5], largeorg := 1] #Large organizations


#Industry size: e16_industry_size
DT_industrysize <- DT_ind[, .(indsize_ind = .N),  by = .(sni, year)] #Count N individuals

#Region/township/municipality size: e17_region_size
DT_regionsize <- DT_ind[, .(region_size_ind = .N),  by = .(mun, year)] #Count N individuals

#Industry employment in township: how many is employed across industries within a region? 
#v4_indsize
DT_industryemployment <- DT_ind[, .(indreg_size_ind = .N),  by = .(sni, mun, year)] #Count N

DT_ind <- merge(DT_ind, DT_orgsize, by = c("org", "year"), all.x = TRUE)
DT_ind <- merge(DT_ind, DT_industrysize, by = c("sni", "year"), all.x = TRUE)
DT_ind <- merge(DT_ind, DT_regionsize, by = c("mun", "year"), all.x = TRUE)
DT_ind <- merge(DT_ind, DT_industryemployment, by = c("sni", "mun", "year"), all.x = TRUE)

#Fix tomorrow!
#Drop large organizations
#DT_ind <- DT_ind[largeorg == 0]
#summary(DT_ind$orgsize_ind)

rm(DT_orgsize, DT_industrysize, DT_regionsize, DT_industryemployment)


# 2) Firm level variables
################################################################################
# Keep only a list of unique firms per year
DT_firm <- subset(unique(DT_ind, by = c("org", "year")))
DT_firm <- DT_firm[, c("sni", "mun", "year", "org", "orgsize_ind", "largeorg", "foreign", "profit", "region_size_ind")] 
#rm(DT_ind)

#Count number of large firms in industry-regions and year: 
#e15_largeorg
#When using KU1CfarlopNr, effectively workplaces, this number is of course very small
DT_firm <- DT_firm[, nr_largeorg := sum(largeorg), by=.(sni, mun, year)]

# Percentage of employment in large organizations in region
DT_large <- DT_firm[, .(nind = sum(orgsize_ind)),  by = .(largeorg, mun, year)] #Count N
DT_large <- reshape(DT_large, idvar=c("mun", "year"), timevar="largeorg", direction = "wide") #Transform to wide
DT_large <- setnames(DT_large, "nind.1", "n_large") #Rename
DT_large <- setnames(DT_large, "nind.0", "n_small") #Rename
DT_large[is.na(DT_large) ] <- 0 #Set NA to 0

DT_large[, share_ind_largefirms := n_large/(n_small + n_large)] #Share public employees in industries (in relation to total employees in industry)
DT_large <- DT_large[, c("n_large", "n_small") := NULL]

#Measure 5: Total N firms in township (10-3) (across industries): 
#Divide by 1000 to scale the variable
#v5_totfirms
DT_firmsregion <- DT_firm[, .(nr_totfirms_reg = (.N)/1000),  by = .(mun, year)] #Count N

#Measure 8: Log N industry firms in township & Count number of organizations per Industry-Region: 
#v8_logindfirms
DT_firmsregionindustry <- DT_firm[, .(orgindustryregion = .N),  by = .(sni, mun, year)]
count_z <- sum(DT_firmsregionindustry$orgindustryregion == 0, na.rm=TRUE)
DT_firmsregionindustry <- DT_firmsregionindustry[orgindustryregion !=0]
DT_firmsregionindustry[, log_indfirms := log(orgindustryregion)] #Count log N

#Measure 7: Standard deviation of employer sizes: 
#v7_sdemployer
# Recode N/A values (to 0) at the industry region - it indicates that there are no variation of firm sizes within this indreg. 
DT_firmSD <- copy(DT_firm)
DT_firmSD <- DT_firmSD[, sdemployer := sd(orgsize_ind, na.rm = TRUE), by=.(sni, mun, year)][is.na(sdemployer), sdemployer := 0]
DT_firmSD <- DT_firmSD[, c("orgsize_ind", "foreign", "profit", "nr_largeorg", "largeorg", "region_size_ind") := NULL]

DT_firm[, c("largeorg") := NULL]

#Merge measure: 5, 8 and 7 to DT_firm + share_ind_largefirms
DT_firm <- merge(DT_firm, DT_firmsregion, by = c("mun", "year"), all.x = TRUE)
DT_firm <- merge(DT_firm, DT_firmsregionindustry, by = c("sni", "mun", "year"), all.x = TRUE)
DT_firm <- merge(DT_firm, DT_firmSD, by = c("sni", "mun", "year", "org"), all.x = TRUE)

#Obs!Merge: At the region/municipality level
DT_firm <- merge(DT_firm, DT_large, by = c("mun", "year"), all.x = TRUE)
rm(DT_firmsregion, DT_firmsregionindustry, DT_firmSD, DT_large)
gc()

#Measure 11: Ratio of foreign owned firms in ind-reg
#foreign, v18_ratio_foreign
DT_for <- DT_firm[, c("sni", "mun", "year", "org", "foreign", "orgindustryregion")]
DT_for <- DT_for[, .(nfirms = .N),  by = .(foreign, mun, sni, year)] #Count N
DT_for <- reshape(DT_for, idvar=c("mun", "sni", "year"), timevar="foreign", direction = "wide") #Transform to wide
DT_for <- setnames(DT_for, "nfirms.1", "n_foreign") #Rename
DT_for <- setnames(DT_for, "nfirms.0", "n_national") #Rename
DT_for[is.na(DT_for) ] <- 0 #Set NA to 0
DT_for[, share_foreign := n_foreign/(n_foreign + n_national)] #Share public employees in industries (in relation to total employees in industry)
DT_for <- DT_for[, c("n_national", "n_foreign") := NULL]


#Measure 12: Ratio of non-profit firms in ind-reg
#profit, v19_ratio_noprofit
DT_pro <- DT_firm[, c("sni", "mun", "year", "org", "profit", "orgindustryregion")]
DT_pro <- DT_pro[, .(nfirms = .N),  by = .(profit, mun, sni, year)] #Count N
DT_pro <- reshape(DT_pro, idvar=c("mun", "sni", "year"), timevar="profit", direction = "wide") #Transform to wide
DT_pro <- setnames(DT_pro, "nfirms.1", "n_profit") #Rename
DT_pro <- setnames(DT_pro, "nfirms.0", "n_nonprofit") #Rename
DT_pro[is.na(DT_pro) ] <- 0 #Set NA to 0
DT_pro[, share_nonprofit := n_nonprofit/(n_nonprofit + n_profit)] #Share public employees in industries (in relation to total employees in industry)
DT_pro <- DT_pro[, c("n_nonprofit", "n_profit") := NULL]


#Merge
DT_firm <- merge(DT_firm, DT_for, by = c("sni", "mun", "year"), all.x = TRUE)
DT_firm <- merge(DT_firm, DT_pro, by = c("sni", "mun", "year"), all.x = TRUE)

#editing: test the average foreign and profit firms -> residual
#DT_firm <- DT_firm[, !c("orgsize_ind", "region_size_ind")]
DT_firm <- DT_firm[, !c("foreign", "profit", "orgsize_ind", "region_size_ind")]

#6 152 922 observations
#No missing observations


## Merge: create industry-region data that will be used below. Still at the level of firm
##Not needed
DT_ind <- DT_ind[, !c("foreign", "profit", "largeorg")]

#Merge independent variables
DT_indreg <- merge(DT_firm, DT_ind, by =c("sni", "mun", "org", "year"), all.x = TRUE)
rm(DT_firm, DT_for, DT_ind, DT_pro)
################################################################################


# 3) Industry-Region variables
################################################################################
# Keep only a list of unique industry-regions per year
DT_indreg <- subset(unique(DT_indreg, by = c("sni", "mun", "year")))
DT_indreg <- DT_indreg[, !c("ind", "org")]

DT_indreg <- DT_indreg[, c("year", "sni", "mun", "indreg_size_ind",
                           "region_size_ind", "indsize_ind", "nr_largeorg", "nr_totfirms_reg", 
                           "orgindustryregion", "share_foreign", "share_nonprofit", "share_ind_largefirms",
                           "log_indfirms", "sdemployer")]

#Measure 6: Single industry employer (in a region) (Industry dummy yeariable)
#v6_singleind
DT_single <- DT_indreg[, .(year, sni, mun, orgindustryregion)]

DT_single <- DT_single[, singleind := 0][orgindustryregion <= 1, singleind := 1]
DT_single[, orgindustryregion := NULL]
#testing: mun 2584, year 1992 and 1993
###

#Measure 9: N industries in township
#v9_indreg
DT_nind_in_region <- DT_indreg[, .(year, sni, mun)]
DT_nind_in_region <- DT_nind_in_region[, nr_ind_in_reg := .N, by = .(mun, year)]
#testing: mun 2584, year 1992 and 1993
###

#### Entropy S&S ##### pi = industry i share of PRIVATE employment in a region. We run on ALL employment.
#v10_ent
DT_ent <- DT_indreg[, .(year, sni, mun, indreg_size_ind, region_size_ind)]
DT_ent[, share := indreg_size_ind / region_size_ind] ##differ from stata (by group)
DT_ent[, product := share * log(share)]
DT_ent[, ent := sum(-product), by=.(year, mun)]
DT_ent[, c("share", "product") := NULL]
#testing: mun 2584, year 1992 and 1993
DT_ent[, c("indreg_size_ind", "region_size_ind") := NULL]

#Add merge
DT_indreg <- merge(DT_indreg, DT_single, by = c("sni", "mun", "year"), all.x = TRUE)
DT_indreg <- merge(DT_indreg, DT_nind_in_region, by = c("sni", "mun", "year"), all.x = TRUE)
DT_indreg <- merge(DT_indreg, DT_ent, by = c("sni", "mun", "year"), all.x = TRUE)

DT_sample <- DT_indreg
rm(DT_indreg, DT_single, DT_nind_in_region, DT_ent)
#rm(DT)
#314 738 observations
#No missing values


#### Merge: Left join
setkey(DV1_grosswage, sni, mun, year)
setkey(DV2_residwage, sni, mun, year)
setkey(DT_sample, sni, mun, year)

finaldata <- merge(DT_sample, DV1_grosswage, all.x=TRUE)
finaldata <- merge(finaldata, DV2_residwage, all.x=TRUE)

rm(DV1_grosswage, DT_sample, DV2_residwage)
#314 736 observations

#Drop NAs across all relevant variables. 
#Should not be any drops at this stage
finaldata <- na.omit(finaldata, cols=c("grosswage_ln"))
#298 208 observations

#Drop if wage is zero (residual will also follow)
# 2 observations?
finaldata <- finaldata[grosswage_ln != 0,]

#Create interaction variables to be used in the regression
#save scaling
finaldata[, indfirmXindreg := log_indfirms*nr_ind_in_reg]
finaldata[, indfirmXent := log_indfirms*ent]

finaldata$indfirmXindreg <- scale(finaldata$indfirmXindreg, center = TRUE, scale = FALSE) #Center var
finaldata$indfirmXent <- scale(finaldata$indfirmXent, center = TRUE, scale = FALSE) #Center var


###################################################################################

setnames(finaldata, "share_foreign", "ratio_foreign")
setnames(finaldata, "share_nonprofit", "ratio_noprofit")
setnames(finaldata, "share_ind_largefirms", "ratio_ind_largefirms")

## Create the final data
DT_final <- finaldata[, c("sni", "mun", "year", "grosswage_ln", "residual_ln", "residual_ln_s", 
                          "meanwage_ln", "indsize_ind", "nr_totfirms_reg", "singleind", "sdemployer", 
                          "log_indfirms", "indreg_size_ind", "nr_ind_in_reg", "ent", "indfirmXindreg", "indfirmXent",
                          "ratio_foreign", "ratio_noprofit", "ratio_ind_largefirms", "nr_largeorg")]

###################################################################################


#Code above checked with multiverse_v4_runwith_data_v2 2023-06-09, 12:27. All lines seem to be updated. 

## Save data
fwrite(DT_final, "data/project1_finaldata_v3_la.csv")








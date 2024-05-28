### Project 1: Set up ###########################################################

### Setup of extract function and variables #####################################
#We extract data from the SQL database and create a data.table for each year, 
#omit missing values and duplicates before we add them all together through rbind. 
#Time period is set to 1992 until 2017.
#################################################################################


#Clean the environment & Set workdir
rm(list=ls())
gc()

setwd("//micro.intra/projekt/P0515$/P0515_Gem/Karl/NiklasKarl/project1/version3_socio_2oct2023_v1/")
getwd()


# Import Individual data
DT_ind <- fread("data/project1_ind_v3.csv", sep = ",", header = TRUE)


# Import background data
DT_back <- fread("data/project1_background_v3.csv", sep = ",", header = TRUE)


### Merge individual other with individual backgrounds ##########################
DT <- merge(DT_ind, DT_back, by = c("ind"), all.x = TRUE)
rm(DT_back, DT_ind)
gc()
#################################################################################

#### Age ####
#Age variable for each individual, using the birth-year variable. 
#We use average (mean) age
#Calculate age at the individual level
DT[, age:= year-FodelseAr]

#Labor experience
DT[, labexp1 := "0"]
DT[YrkStalln >0, labexp1 := "1"]
DT <- DT[order(ind, year)]
DT <- DT[,labexp := cumsum(labexp1), by=c("ind")]
DT[, labexp1 := NULL]

#### WORK EXPERIENCE ####
#Experience since graduation
#Convert to Integer
DT[, ExamAr:=as.integer(ExamAr)]
#Calculate experience since graduation year or since year 16

#Assign missing values to labexp
DT <- DT[, workexp := 0]
DT <- DT[, workexp := year-ExamAr]
DT <- DT[workexp == year, workexp := 0]
DT <- DT[is.na(workexp), workexp := labexp]
DT[workexp < 0, workexp := 0] #Assign negative values to 0, (or NA)
DT[, c("ExamAr", "labexp")  := NULL] #Drop unused variables

#Drop if some ind is not employed
DT <- na.omit(DT, cols=c("org")) #Drop missing values



# Import and merge institutional variables
DT_inst <- fread("data/project1_inst_v3.csv", sep = ",", header = TRUE)
# Merge
DT <- merge(DT, DT_inst, by = c("org", "year"), all.x = TRUE)
rm(DT_inst)
gc()


# Import and merge industry variables
DT_sni <- fread("data/project1_sni_v3.csv", sep = ",", header = TRUE)
# Merge
DT <- merge(DT, DT_sni, by = c("org", "year"), all.x = TRUE)
rm(DT_sni)
gc()


# Import and merge Township  Municipality variable
DT_mun <- fread("data/project1_town_v3.csv", sep = ",", header = TRUE)
# Merge
DT <- merge(DT, DT_mun, by = c("org", "year"), all.x = TRUE)
rm(DT_mun)
gc()



### Construct individual level sample ###########################################

#Drop missing values in central variables before saving the data
DT <- na.omit(DT, cols=c("sni")) 
DT <- na.omit(DT, cols=c("mun")) 
DT <- na.omit(DT, cols=c("gender")) 
DT <- na.omit(DT, cols=c("private"))

## We exclude municipality coded to 0
DT <- DT[mun !=0]
DT <- DT[sni !=0]

###########################

DT <- setnames(DT, "KU1Ink", "income")
DT <- setnames(DT, "Kon", "kon")
DT <- setnames(DT, "edu_years", "education")
DT <- setnames(DT, "Barn0_3", "children0_3")

DT <- DT[year >= 1992 & year <= 2017,]

################################################################################
##Save individual data
fwrite(DT, "data/project1_combined_v3.csv")
gc()



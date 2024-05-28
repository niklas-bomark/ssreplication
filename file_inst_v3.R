
### Institutional sector ###

#Clean the environment & Set workdir
rm(list=ls())
gc()

setwd("//micro.intra/projekt/P0515$/P0515_Gem/Karl/NiklasKarl/project1/version3_socio_2oct2023_v1/")
getwd()


#Define unit of analysis
unit <- "KU1CfarLopNr"

#Institutional variable
instvar_ <- "KU1InstKod"
#Alternatives: | Org_InstKod | InstKod | KU1InstKod


#################################################################################
#Extract data from SQl database
#################################################################################
#Load packages
library(doParallel)
library(data.table)
library(dplyr)


############################
# Extract function: from SQL
extract_fun <- function(extract_fun) {
    odbcCloseAll()
    myconn <- odbcDriverConnect(connection = paste('driver={ODBC Driver 17 for SQL Server}',
                                                   'server=mq02\\b',
                                                   'database=P0515_IFFS_Segregeringens_dynamik',
                                                   'trusted_connection=yes', sep=';'))
    # Write the name of the table/view
    tb <- (paste("LISA",i,"_Individ", sep=""))
    #data <- data.table()
    df <- sqlQuery(myconn, paste('select ', paste(var.names, collapse = ','), 
                                 ' from ', tb, sep = '')) 
    #Create data.table
    df = data.table(df)
    df <- na.omit(df, cols=c(unit)) #Drop NA
    df <- subset(unique(df, by = c(unit))) #Keep only unique values
    df[, year := i] #Add year variable
}
#############################


### 1) Institutional code: Setup parallel back-end and run extract function. ####

cl <- makeCluster(5) #create clusters/parallel

registerDoParallel(cl)
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(RODBC))

var1 <- c(paste0(instvar_)) #Years: 1990:1998. Specify changes in instkod over time.
var2 <- c(paste0(instvar_,6)) #1999:2000
var3 <- c(paste0(instvar_,7)) #2001:2013
var4 <- c(paste0(instvar_,10)) #2014:2017

list_years1 <- 1990:1998 #list of years to run over 
list_years2 <- 1999:2000
list_years3 <- 2001:2013
list_years4 <- 2014:2017

system.time(
    d1 <- foreach(i = list_years1, .combine=rbind) %dopar% {
        var.names <- c(var1, paste(unit)) # Columns
        temp <- extract_fun()}) #call extract function

system.time(
    d2 <- foreach(i = list_years2, .combine=rbind) %dopar% {
        var.names <- c(var2, paste(unit))
        temp <- extract_fun()})

system.time(
    d3 <- foreach(i = list_years3, .combine=rbind) %dopar% {
        var.names <- c(var3, paste(unit))
        temp <- extract_fun()})

system.time(
    d4 <- foreach(i = list_years4, .combine=rbind) %dopar% {
        var.names <- c(var4, paste(unit))
        temp <- extract_fun()})

stopCluster(cl) #close clusters/parallel

#################################################################################

## Recode Inst
# I have to re-code the INST-code since it changes over the years. 
# See external Excel-file, flik1: Instkod, for details about the recode. 
# For results: compare inst with the new variables: instsek, instaga and instjur

# InstKod 
# 1990-1998: InstKod
# 1-2 Sektorindelning
# 3 Ägarkategori
# 4-5 Juridisk form

# 1999-2000: InstKod6
# 1-3 Sektorindelning
# 4 Ägarkategori
# 5-6 Juridisk form

# 2001-2013: InstKod7
# 1-3 Sektorindelning
# 4-5 Ägarkategori
# 6-7 Juridisk form

# >=2014: InstKod10
# 1-6 Sektorindelning
# 7-8 Ägarkategori
# 9-10 Juridisk form

setnames(d1, get("var1"), "inst")
setnames(d2, get("var2"), "inst")
setnames(d3, get("var3"), "inst")
setnames(d4, get("var4"), "inst")

#Subset
sub1 <- subset(unique(d1, by = c("inst", "year")))
sub2 <- subset(unique(d2, by = c("inst", "year")))
sub3 <- subset(unique(d3, by = c("inst", "year")))
sub4 <- subset(unique(d4, by = c("inst", "year")))

sub1 <- sub1[, instsek := substring(inst, 0, nchar(inst)-3L)] #Sector
sub2 <- sub2[, instsek := substring(inst, 0, nchar(inst)-3L)]
sub3 <- sub3[, instsek := substring(inst, 0, nchar(inst)-4L)]
sub4 <- sub4[, instsek := substring(inst, 0, nchar(inst)-4L)]

sub1 <- sub1[, instaga := substring(inst, 3, nchar(inst)-2L)] #Owner
sub2 <- sub2[, instaga := substring(inst, 4, nchar(inst)-2L)]
sub3 <- sub3[, instaga := substring(inst, 4, nchar(inst)-2L)]
sub4 <- sub4[, instaga := substring(inst, 7, nchar(inst)-2L)]

sub1 <- sub1[, instjur := substring(inst, 4, nchar(inst)-0L)] #Law
sub2 <- sub2[, instjur := substring(inst, 5, nchar(inst)-0L)]
sub3 <- sub3[, instjur := substring(inst, 6, nchar(inst)-0L)]
sub4 <- sub4[, instjur := substring(inst, 9, nchar(inst)-0L)]

inst_subset <- rbindlist(list(sub1, sub2, sub3, sub4)) #append datasets to one
rm(sub1, sub2, sub3, sub4)


inst_subset[, private := recode(instjur, 
                                "81" = "0",
                                "82" = "0",
                                "83" = "0",
                                "84" = "0",
                                "86" = "0",
                                "87" = "0",
                                "89" = "0",
                                .default = "1")]

inst_subset <- inst_subset[!instjur == "00",]
inst_subset <- inst_subset[!instjur == "91",]
inst_subset <- inst_subset[!inst == 0,]


#Classifications: National (0) versus foreign firms (1)
inst_subset[year <= 1998, foreign := recode(instsek, 
                                            "23" = "1",
                                            "60" = "1",
                                            .default = "0")]

inst_subset[year >= 1999 & year <= 2013, foreign := recode(instsek, 
                                                           "130" = "1",
                                                           "213" = "1",
                                                           "820" = "1",
                                                           .default = "0")]

inst_subset[year >= 2014 & year <= 2017, foreign := recode(instsek, 
                                                           "113000" = "1",
                                                           "122200" = "1",
                                                           "128200" = "1",
                                                           "129200" = "1",
                                                           "220000" = "1",
                                                           .default = "0")]

inst_subset[instjur == "96", foreign := "1"]


# Before, 1999 they didnt identified this category - it was lumped together as Other. THus, we shouold get a better 
# categorization after 1998. 
inst_subset[year >= 1999 & instaga == "50", foreign := "1"]


inst_subset[, profit := recode(instjur,
                               "10" = "1",
                               "21" = "1",
                               "22" = "1", #partrederier??
                               "23" = "1", #värdepapper?
                               "31" = "1", 
                               "32" = "1",
                               "41" = "1",
                               "42" = "1",
                               "43" = "1", #europabolag
                               "49" = "1",
                               "96" = "1",
                               .default = "0")]



# Append and merge
DT_inst <- rbindlist(list(d1, d2, d3, d4)) #append datasets to one
rm(d1, d2, d3, d4)

inst_subset[, c(get("unit")) := NULL]

DT_inst <- merge(DT_inst, inst_subset, by = c("inst", "year"), all.x = TRUE) #Merge
rm(inst_subset, cl)

DT_inst <- DT_inst[, 
instsek := as.integer(instsek)][, 
instaga := as.integer(instaga)][, 
instjur := as.integer(instjur)][, 
private := as.integer(private)][, 
foreign := as.integer(foreign)][, 
profit := as.integer(profit)]

#Added v.3 june 9
DT_inst <- na.omit(DT_inst)

DT_inst[, c("inst", "instsek", "instaga", "instjur") := NULL]

setnames(DT_inst, unit, "org")

##Save individual data
fwrite(DT_inst, "data/project1_inst_v3.csv")
gc()


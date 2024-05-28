
### TOwnship / Municipality (mun) ###

#Clean the environment & Set workdir
rm(list=ls())
gc()

setwd("//micro.intra/projekt/P0515$/P0515_Gem/Karl/NiklasKarl/project1/version3_socio_2oct2023_v1/")
getwd()


#Define unit of analysis
unit <- "KU1CfarLopNr"

#Municipality variable
munvar_ <- "KU1AstKommun" 


#################################################################################
#Extract data from SQl database
#################################################################################
#Load packages


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

### 3) Region: Setup parallel back-end and run extract function. ################

#Region is the second important unit of analysis. 
#In S&S they call the unit Township, the smallest administrative unit in Denmark. 
#In their paper they have 275 mutually exclusive and exhaustive Townships. 
#P.772 in S&S

#In this file, we extract region variables: municipality and local labor markets. 
#Since there are some changes over the years, I have to do some re-coding of them. 
#Local labor markets, or LMAS, is created from the list of municipalities. 
#This gives us an opportunity to analyse the data using two different region 
#variables - one based on geography and one based on journey-to-work 
#boundaries e.g. based on commuting patterns.
#See external Excel-file for more details. 

cl <- makeCluster(5) #create clusters/parallel

registerDoParallel(cl)
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(RODBC))

list_years1 <- 1990:2017 #list of years to run over 

system.time(
    d1 <- foreach(i = list_years1, .combine=rbind) %dopar% {
        var.names <- c(munvar_, paste(unit)) # Columns
        temp <- extract_fun()}) #call extract function

stopCluster(cl) #close clusters/parallel

#################################################################################

setnames(d1, get("munvar_"), "mun") #Change name of region variable

## Recode mun (Municipality)
# They change over the years. See external Excel-file for details

sub1 <- d1[, c(get("unit"), "mun", "year")]
#sub1 <- subset(unique(sub1, by = c("mun"))) #Subset

mun_list_recode <- fread("imported/kommun_labels.csv", sep = ",", header = TRUE)
setnames(mun_list_recode, "code", "mun")
mun_list_recode[, c("comments") := NULL]
unique(mun_list_recode$range)

#All values that should be the same
sub1_a <- merge(sub1, mun_list_recode[range == "All year"], by = c("mun")) #Merge

#All conditionals
sub1_b <- merge(sub1, mun_list_recode[range == "<=1996"], by = c("mun")) #Merge
sub1_c <- merge(sub1, mun_list_recode[range == "<=1997"], by = c("mun")) #Merge
sub1_d <- merge(sub1, mun_list_recode[range == ">=1997"], by = c("mun")) #Merge
sub1_e <- merge(sub1, mun_list_recode[range == ">=1998"], by = c("mun")) #Merge
sub1_f <- merge(sub1, mun_list_recode[range == "<=2006"], by = c("mun")) #Merge
sub1_g <- merge(sub1, mun_list_recode[range == ">=2007"], by = c("mun")) #Merge

sub_append <- rbindlist(list(sub1_a, sub1_b, sub1_c, sub1_d, sub1_e, sub1_f, sub1_g)) #append datasets to one
rm(sub1_a, sub1_b, sub1_c, sub1_d, sub1_e, sub1_f, sub1_g)


#Added v.3 june 9
#Drop all municipality coded to 0
DT_mun <- sub_append[!new_code == 0,]


setnames(DT_mun,"mun", "old_muncode")
setnames(DT_mun,"new_code", "mun")
DT_mun[, c("old_muncode", "range", "label") := NULL]

setnames(DT_mun, unit, "org")

##Save individual data
fwrite(DT_mun, "data/project1_town_v3.csv")
gc()

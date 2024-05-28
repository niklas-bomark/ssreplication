
### Industry ###

#Clean the environment & Set workdir
rm(list=ls())
gc()

setwd("//micro.intra/projekt/P0515$/P0515_Gem/Karl/NiklasKarl/project1/version3_socio_2oct2023_v1/")
getwd()


#Define unit of analysis
unit <- "KU1CfarLopNr"

#SNI variable
snivar_ <- "KU1AstSni"


#################################################################################
#Extract data from SQl database
#################################################################################


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



### 2) Industry code. SNI: Setup parallel back-end and run extract function. ####

cl <- makeCluster(5) #create clusters/parallel

registerDoParallel(cl)
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(RODBC))

var1 <- c("AstSNI92") #Years: 1990:1992. Specify changes in instkod over time.
var2 <- c(paste0(snivar_,92)) #1993:2001
var3 <- c(paste0(snivar_,2002)) #2002:2010
var4 <- c(paste0(snivar_,2007)) #2011:2017

list_years1 <- 1990:1992 #list of years to run over 
list_years2 <- 1993:2001
list_years3 <- 2002:2010
list_years4 <- 2011:2017

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

## Recode SNI (Industries)

# We have to re-code the SNI-code since it changes over the years.
# We keep the two first digits in the code. 

setnames(d1, get("var1"), "sni")
setnames(d2, get("var2"), "sni")
setnames(d3, get("var3"), "sni")
setnames(d4, get("var4"), "sni")

d1 <- d1[, sni_short := substring(sni, 0, nchar(sni)-3L)] #Industries
d2 <- d2[, sni_short := substring(sni, 0, nchar(sni)-3L)]
d3 <- d3[, sni_short := substring(sni, 0, nchar(sni)-3L)]
d4 <- d4[, sni_short := substring(sni, 0, nchar(sni)-3L)]

#Recode sni2007 to sni2002 (sni92 and sni2002 are similar)
d4 <- d4[, sni_short := as.integer(sni_short)]
sni_list_recode <- fread("imported/sni_recode.csv", sep = ",", header = TRUE)
setnames(sni_list_recode, "sni07", "sni_short")

d4 <- merge(d4, sni_list_recode, by = c("sni_short"), all.x=TRUE) #Merge
d4[, sni_short := NULL]
setnames(d4, "sni02", "sni_short")

DT_sni <- rbindlist(list(d1, d2, d3, d4)) #append datasets to one
rm(d1, d2, d3, d4)

DT_sni <- DT_sni[, sni_short := as.integer(sni_short)]

#Added v.3 june 9
DT_sni <- na.omit(DT_sni)


DT_sni[, c("sni") := NULL]
setnames(DT_sni, "sni_short", "sni")

setnames(DT_sni, unit, "org")

##Save individual data
fwrite(DT_sni, "data/project1_sni_v3.csv")
gc()


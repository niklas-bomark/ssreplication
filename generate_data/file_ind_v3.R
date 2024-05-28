### Project 1: File 1 ###########################################################

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


################################################################################
## Set parameters

#Organizational level variables

#Define unit of analysis
unit <- "KU1CfarLopNr"
#Alternatives: | ArbstIdLopNr | KU1PeOrgLopNr | PeOrgLopNr | CfarLopNr | KU1CfarLopNr
#Match with sni, inst and municipality

#Specify variable of interest: individuals.
idvar_ <- "PersonLopNr"  

# Individual level data: Variable names
other <- c("Sun2000niva", "ExamAr", "FamStF", "Barn0_3", "KU1Ink", "YrkStalln")
# "MedbGrEg",


### Project 1: File 1 ###########################################################
# Here we extract each working individual for the time period.
# Thus, we focus at the individual level, and individuals characteristics. 

#################################################################################
#Extract data from SQl database
#################################################################################


# Extract function individuals (multi-variables): from SQL.
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
    df <- na.omit(df, cols=c(idvar_, paste(unit))) #Drop NA: idvar_ = ID, unit = CfarLopNr
    df <- subset(unique(df, by = c(var.name))) #Keep only unique values
    setnames(df, paste0(var.name), paste0(new.var.name))
    df[, year := i] #Add year variable
}



### 1) Individual others: Setup parallel back-end and run extract function. ###########
#Individuals are needed to create the wage and residual wage variables. 

cl <- makeCluster(5) #create clusters/parallel

registerDoParallel(cl)
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(RODBC))

# Create variable names and time periods
new.var.name <- "ind" #name of variable of interest

list_years1 <- 1990:2017 #list of years to run over 

system.time(
    d1 <- foreach(i = list_years1, .combine=rbind) %dopar% {
        var.names <- c(idvar_, paste(unit), paste(other)) # Columns
        var.name <- c(idvar_)
        temp <- extract_fun()}) #call extract function

stopCluster(cl) #close clusters/parallel
gc()



#################################################################################

#Recode: education years
sub1 <- d1[, c("Sun2000niva")]
sub1 <- subset(unique(sub1, by = c("Sun2000niva")))

edu_list <- fread("imported/education_recode.csv", sep = ",", header = TRUE)
sub1 <- merge(sub1, edu_list, by = c("Sun2000niva"), all.x=TRUE) #Merge

d1 <- merge(d1, sub1, by = c("Sun2000niva"), all.x = TRUE) #Merge
d1[, c("Sun2000niva") := NULL]
rm(sub1, edu_list, cl)


#Recode: Marital status
# 1 = married
# 0 = unmarried
d1[, married := 0]
d1[FamStF <20 & year <=1998, married := 1]
d1[FamStF <200 & year >=1999, married := 1]
d1[, married:=as.integer(married)]
#DT_other[married == 1]

setnames(d1, unit, "org")

d1 <- d1[year >= 1992 & year <= 2017,]

##Save individual data
fwrite(d1, "data/project1_ind_v3.csv")
gc()






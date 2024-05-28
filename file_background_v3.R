

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


# Individual level data: Background2, Variable names
background_kon_age <- c("PersonLopNr", "Kon" ,"FodelseAr")


### 2) Individuals background: Setup parallel back-end and run extract function.
#Individuals background are needed to create the wage and residual wage variables. 

# Create data frame: Bakgrundsdata
DT_back = data.table()

odbcCloseAll()
myconn <- odbcDriverConnect(connection = paste('driver={SQL Server}',
                                               'server=mq02\\b',
                                               'database=P0515_IFFS_Segregeringens_dynamik',
                                               'trusted_connection=true', sep=';'))

# Write the name of the table/view
tb <- ("Bakgrundsdata")
#data <- data.table()
DT_back <- sqlQuery(myconn, paste('select ', paste(background_kon_age, collapse = ','), 
                                  ' from ', tb, sep = '')) 
#Drop NA
DT_back <- na.omit(DT_back, cols=c("PersonLopNr"))
#Keep only unique values
DT_back <- subset(unique(DT_back, by = c("PersonLopNr")))
setnames(DT_back, "PersonLopNr" , "ind")

#Close all connections
odbcCloseAll()

#Clean the environment & setkeys
rm(background_kon_age, myconn, tb)

#################################################################################


#Recode men and women. Women == 2, Men == 1
#Recode to... Women == 0, Men == 1
setDT(DT_back) 
DT_back[, gender := recode(Kon,        
                           "2" = "0",
                           "1" = "1",
                           .default = "Missing")]
DT_back[, gender:=as.integer(gender)]

##Save individual data
fwrite(DT_back, "data/project1_background_v3.csv")
gc()










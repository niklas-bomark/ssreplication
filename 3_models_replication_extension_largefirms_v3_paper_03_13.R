
## Run models and figures ##

#Clean the environment
rm(list=ls())

#Extract data from SQl database
setwd("//micro.intra/projekt/P0515$/P0515_Gem/Karl/NiklasKarl/project1/version3_socio_2oct2023_v1/")
getwd()


# #### Install packages (if not already installed)
# install.packages("RODBC")
# install.packages("dtplyr")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("tidyverse")
# install.packages("lfe")
# install.packages("stargazer")
# install.packages("sandwich")
# install.packages("modelsummary")
# install.packages("broom")
# install.packages("foreach")
# install.packages("doParallel")
# install.packages("fixest")
# install.packages("vtable")
# install.packages("grid")

#### Load packages
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
library(vtable)
library(grid)

# Load dataset 
DT_final <- fread("data/project1_finaldata_v3.csv", sep = ",", header = TRUE)


#### Labels to variables

# Labels to regression models
dict = c(
    "grosswage_ln" = "Gross wage dispersion",
    "meanwage_ln"  = "Log mean industry wage",
    "residual_ln" = "Residual wage dispersion",
    "residual_ln_s" = "Residual wage dispersion_adj",
    "indreg_size_ind" = "Industry employment in township",
    "nr_totfirms_reg" = "Total N firms in township (10-3)",
    "singleind" = "Single industry employer",
    "sdemployer" = "Standard deviation of employer sizes",
    "log_indfirms" = "Log N industry firms in township",
    "nr_ind_in_reg" = "N industries in township",
    "ent" = "Entropy of industry shares",
    "indfirmXindreg" = "Log N firms X N industries",
    "indfirmXent" = "Log N firms X entropy",
    "ratio_foreign" = "Ratio of foreign owned firms in industry-township",
    "ratio_noprofit" = "Ratio of non-profit making firms in industry-township",
    "mun" = "Township",
    "sni" = "Industry",
    "year" = "Year",
    "year_counter" = "Year counter",
    "ratio_ind_largefirms" = "Ratio of employees in large organizations"
)

# Labels to descriptive tables ()
dict2 <- as.data.frame(t(dict))

# Lookup table
keep_variables <- c("singleind", "log_indfirms", "sdemployer", "nr_ind_in_reg", "ent", "indfirmXindreg", "indfirmXent", "ratio_ind_largefirms")
subset_dict <- dict[keep_variables]

lookup_table <- data.frame(
    varname = names(subset_dict),
    fullname = unname(subset_dict)
)

rm(keep_variables, subset_dict)


#######################################################################
# Change to residual sample error adjustment

DT_final[, residual_ln := NULL]
setnames(DT_final, "residual_ln_s", "residual_ln")




#### To paper: Descriptive ###########################################

### Summary Stats and Correlation Matrix
time_periods <- c(1998, 2012)
directory_path <- "output/tables/"
name <- "table_sumstat_largefirms_"


for (period in time_periods){
    DT <- DT_final[year <= period]
   
    #Name and save
    modified_period <- gsub(":", "_", period)
    filename <- paste0(directory_path, name, modified_period, ".tex")
    filename_cor <- paste0(directory_path, name, modified_period, "_cor_", ".csv")
    
    varlist <- c("grosswage_ln", "meanwage_ln", "residual_ln", "indreg_size_ind", 
                 "nr_totfirms_reg", "singleind", "sdemployer", "log_indfirms", 
                 "nr_ind_in_reg", "ent", "ratio_foreign", "ratio_noprofit", "ratio_ind_largefirms")
    # Summary Stats
    st(DT, vars = varlist, 
       out="latex",
       file=filename,
       summ = c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)', 'max(x)'),
       summ.names = c('N', 'Mean', 'SD', 'Min', 'Max'),
       labels=dict2, digits = 3) 
       
    
    # Corr matrix
    cor <- cor(DT[, ..varlist])
    cor[upper.tri(cor)] <- 0
    fwrite(cor, filename_cor)
}






#### To paper: Models ###########################################################


#Scale coefficients on Y. Don't do this for descriptives 
DT_final <- DT_final[, grosswage_ln := grosswage_ln * 1000]
DT_final <- DT_final[, residual_ln := residual_ln * 1000]






######################## RESIDUAL WAGE ##########################################



# Set of Models: Replications
time_periods <- c(1998, 2012)

for (period in time_periods){
    DT <- DT_final[year <= period]
    ##### Models: Residual wage #####
    m1b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms | sni + year, data = DT, vcov = ~mun)
    m2b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg | sni + year, data = DT, vcov = ~mun)
    m3b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent | sni + year, data = DT, vcov = ~mun)
    m4b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + indfirmXindreg | sni + year, data = DT, vcov = ~mun)
    m5b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent + indfirmXent | sni + year, data = DT, vcov = ~mun)
    etable(m1b, m2b, m3b, m4b, m5b, vcov = ~mun)
    tex8 <- etable(m1b, m2b, m3b, m4b, m5b, dict = dict, tex=TRUE,
                   digits = "r3",
                   depvar = FALSE,
                   digits.stats = "r3",
                   signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
                   fitstat =  ~r2 + wr2 + n)
    
    directory_path <- "output/tables/"
    name <- "table_residwage_replication_largefirms_"
    modified_period <- gsub(":", "_", period)
    filename <- paste0(directory_path, name, modified_period, ".tex")
    write(tex8, file=filename)
    rm(m1b, m2b, m3b, m4b, m5b)
}

#################################################################################


# Set of Models: Extensions 

#Prepare for models/tables
time_periods <- c(1998, 2012)

for (period in time_periods){
    DT <- DT_final[year <= period]
    ## 1992 - 2012: Extension and adding foreign. Models: Residual wage #####
    m1b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + ratio_foreign | sni + year, data = DT, vcov = ~mun)
    m2b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + ratio_noprofit | sni + year, data = DT, vcov = ~mun)
    m3b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit | sni + year, data = DT, vcov = ~mun)
    m4b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms | sni + year, data = DT, vcov = ~mun)
    m5b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + nr_ind_in_reg | sni + year, data = DT, vcov = ~mun)
    m6b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + ent | sni + year, data = DT, vcov = ~mun)
    m7b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + nr_ind_in_reg + indfirmXindreg | sni + year, data = DT, vcov = ~mun)
    m8b <- feols(residual_ln ~ ratio_ind_largefirms + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + ent + indfirmXent | sni + year, data = DT, vcov = ~mun)
    etable(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b, vcov = ~mun)
    tex8 <- etable(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b, dict = dict, tex=TRUE,
                 digits = "r3",
                 digits.stats = "r3",
                 depvar = FALSE,
                 signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
                 fitstat =  ~r2 + wr2 + n)
    
    #Name and save tables
    directory_path <- "output/tables/"
    name <- "table_residwage_extension_largefirms_"
    modified_period <- gsub(":", "_", period)
    filename <- paste0(directory_path, name, modified_period, ".tex")
    write(tex8, file=filename)
  
    #remove models
    rm(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b)
    
  
}    









######################## GROSS WAGE #############################################


# Set of Models: Replications
time_periods <- c(1998, 2012)

for (period in time_periods){
    DT <- DT_final[year <= period]
    ##### Models: Gross wage #####
    m1b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms | sni + year, data = DT, vcov = ~mun)
    m2b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg | sni + year, data = DT, vcov = ~mun)
    m3b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent | sni + year, data = DT, vcov = ~mun)
    m4b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + indfirmXindreg | sni + year, data = DT, vcov = ~mun)
    m5b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent + indfirmXent | sni + year, data = DT, vcov = ~mun)
    etable(m1b, m2b, m3b, m4b, m5b, vcov = ~mun)
    tex8 <- etable(m1b, m2b, m3b, m4b, m5b, dict = dict, tex=TRUE,
                   digits = "r3",
                   depvar = FALSE,
                   digits.stats = "r3",
                   signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
                   fitstat =  ~r2 + wr2 + n)
    
    #Name and save
    directory_path <- "output/tables/"
    name <- "table_grosswage_replication_largefirms_"
    modified_period <- gsub(":", "_", period)
    filename <- paste0(directory_path, name, modified_period, ".tex")
    write(tex8, file=filename)
    
    rm(m1b, m2b, m3b, m4b, m5b)

}
    



#################################################################################


# Set of Models: Extensions

time_periods <- c(1998, 2012)


for (period in time_periods){
  DT <- DT_final[year <= period]
  ## 1992 - 2012: Extension and adding foreign. Models: Gross wage #####
  m1b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign | sni + year, data = DT, vcov = ~mun)
  m2b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_noprofit | sni + year, data = DT, vcov = ~mun)
  m3b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit | sni + year, data = DT, vcov = ~mun)
  m4b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms | sni + year, data = DT, vcov = ~mun)
  m5b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + nr_ind_in_reg | sni + year, data = DT, vcov = ~mun)
  m6b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + ent | sni + year, data = DT, vcov = ~mun)
  m7b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + nr_ind_in_reg + indfirmXindreg | sni + year, data = DT, vcov = ~mun)
  m8b <- feols(grosswage_ln ~ ratio_ind_largefirms + meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + ent + indfirmXent | sni + year, data = DT, vcov = ~mun)
  etable(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b, vcov = ~mun)
  tex8 <- etable(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b, dict = dict, tex=TRUE,
                 digits = "r3",
                 depvar = FALSE,
                 digits.stats = "r3",
                 signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
                 fitstat =  ~r2 + wr2 + n)
  
  #Name and save
  directory_path <- "output/tables/"
  name <- "table_grosswage_extension_largefirms_"
  modified_period <- gsub(":", "_", period)
  filename <- paste0(directory_path, name, modified_period, ".tex")
  write(tex8, file=filename)
  
  rm(m1b, m2b, m3b, m4b, m5b)
  
}










################################################################################


# 
# 
# # Time interaction
# time_periods <- c(2012)
# directory_path <- "output/tables/"
# name <- "table_residwage_time_largefirms_"
# 
# for (period in time_periods){
#     DT <- DT_final[year <= period]
#     # Time interactions
#     DT$year_counter <- DT$year - min(DT$year)
#     m1b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + year_counter + year_counter*singleind | sni, data = DT, vcov = ~mun)
#     m2b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + year_counter + year_counter:log_indfirms | sni, data = DT, vcov = ~mun)
#     m3b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + year_counter + year_counter:sdemployer | sni, data = DT, vcov = ~mun)
#     m4b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + year_counter + year_counter:nr_ind_in_reg | sni, data = DT, vcov = ~mun)
#     m5b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent + year_counter + year_counter:ent | sni, data = DT, vcov = ~mun)
#     
#     etable(m1b, m2b, m3b, m4b, m5b, vcov = ~mun)
#     tex8 <- etable(m1b, m2b, m3b, m4b, m5b, dict = dict, tex=TRUE,
#                    digits = "r3",
#                    digits.stats = "r3",
#                    signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
#                    depvar = FALSE,
#                    fitstat =  ~r2 + wr2 + n)
#     #Name and save
#     modified_period <- gsub(":", "_", period)
#     filename <- paste0(directory_path, name, modified_period, ".tex")
#     write(tex8, file=filename)
#     rm(m1b, m2b, m3b, m4b, m5b)
# }
# 
# 
# 
# 
# # Time interaction
# time_periods <- c(2012)
# directory_path <- "output/tables/"
# name <- "table_grosswage_time_largefirms_"
# 
# for (period in time_periods){
#     DT <- DT_final[year <= period]
#     # Time interactions
#     DT$year_counter <- DT$year - min(DT$year)
#     m1b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + year_counter + year_counter*singleind | sni, data = DT, vcov = ~mun)
#     m2b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + year_counter + year_counter:log_indfirms | sni, data = DT, vcov = ~mun)
#     m3b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + year_counter + year_counter:sdemployer | sni, data = DT, vcov = ~mun)
#     m4b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + year_counter + year_counter:nr_ind_in_reg | sni, data = DT, vcov = ~mun)
#     m5b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent + year_counter + year_counter:ent | sni, data = DT, vcov = ~mun)
#     
#     etable(m1b, m2b, m3b, m4b, m5b, vcov = ~mun)
#     tex8 <- etable(m1b, m2b, m3b, m4b, m5b, dict = dict, tex=TRUE,
#                    digits = "r3",
#                    depvar = FALSE,
#                    digits.stats = "r3",
#                    signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
#                    fitstat =  ~r2 + wr2 + n)
#     #Name and save
#     modified_period <- gsub(":", "_", period)
#     filename <- paste0(directory_path, name, modified_period, ".tex")
#     write(tex8, file=filename)
#     rm(m1b, m2b, m3b, m4b, m5b)
# }



################################################################################
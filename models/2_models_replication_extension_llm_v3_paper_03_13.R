
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
DT_final <- fread("data/project1_finaldata_v3_la.csv", sep = ",", header = TRUE)

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
    "year_counter" = "Year counter"
)

# Labels to descriptive tables ()
dict2 <- as.data.frame(t(dict))

# Lookup table
keep_variables <- c("singleind", "log_indfirms", "sdemployer", "nr_ind_in_reg", "ent", "indfirmXindreg", "indfirmXent")
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
name <- "table_sumstat_la_"


for (period in time_periods){
    DT <- DT_final[year <= period]
   
    #Name and save
    modified_period <- gsub(":", "_", period)
    filename <- paste0(directory_path, name, modified_period, ".tex")
    filename_cor <- paste0(directory_path, name, modified_period, "_cor_", ".csv")
    
    varlist <- c("grosswage_ln", "meanwage_ln", "residual_ln", "indreg_size_ind", 
                 "nr_totfirms_reg", "singleind", "sdemployer", "log_indfirms", 
                 "nr_ind_in_reg", "ent", "ratio_foreign", "ratio_noprofit")
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
    m1b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms | sni + year, data = DT, vcov = ~mun)
    m2b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg | sni + year, data = DT, vcov = ~mun)
    m3b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent | sni + year, data = DT, vcov = ~mun)
    m4b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + indfirmXindreg | sni + year, data = DT, vcov = ~mun)
    m5b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent + indfirmXent | sni + year, data = DT, vcov = ~mun)
    etable(m1b, m2b, m3b, m4b, m5b, vcov = ~mun)
    tex8 <- etable(m1b, m2b, m3b, m4b, m5b, dict = dict, tex=TRUE,
                   digits = "r3",
                   depvar = FALSE,
                   digits.stats = "r3",
                   signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
                   fitstat =  ~r2 + wr2 + n)
    
    directory_path <- "output/tables/"
    name <- "table_residwage_replication_la_"
    modified_period <- gsub(":", "_", period)
    filename <- paste0(directory_path, name, modified_period, ".tex")
    write(tex8, file=filename)
    
    if (period == 1998)  {
    
        #Name and save
        m1b <- tidy(m1b, conf.int = TRUE) %>%
            mutate(model = "Model 1",
                   outcome = "res_wage")
        
        m2b <- tidy(m2b, conf.int = TRUE) %>%
            mutate(model = "Model 2",
                   outcome = "res_wage")
        
        
        m3b <- tidy(m3b, conf.int = TRUE) %>%
            mutate(model = "Model 3",
                   outcome = "res_wage")
        
        
        m4b <- tidy(m4b, conf.int = TRUE) %>%
            mutate(model = "Model 4",
                   outcome = "res_wage")
        
        
        m5b <- tidy(m5b, conf.int = TRUE) %>%
            mutate(model = "Model 5",
                   outcome = "res_wage")
        
        res_out <- rbind(m1b, m2b, m3b, m4b, m5b)
        
        rm(m1b, m2b, m3b, m4b, m5b)
        
        
        
        res_out <- res_out %>% 
            mutate(cat = case_when(term ==  "grosswage_ln" ~ "Vertical",
                                   term == "meanwage_ln"  ~ "Control",
                                   # term == "residual_ln" ~ "Residual wage dispersion",
                                   #  term == "residual_ln_s" ~ "Residual wage dispersion_adj",
                                   term == "indreg_size_ind" ~ "Control",
                                   term == "nr_totfirms_reg" ~ "Control",
                                   term == "singleind" ~ "Vertical",
                                   term == "sdemployer" ~ "Horizontal",
                                   term == "log_indfirms" ~ "Vertical",
                                   term == "nr_ind_in_reg" ~ "Horizontal",
                                   term == "ent" ~ "Horizontal",
                                   term == "indfirmXindreg" ~ "Interactions",
                                   term == "indfirmXent" ~ "Interactions"),
                   term = case_when(term == "meanwage_ln"  ~ "Log mean industry wage",
                                    term == "indreg_size_ind" ~ "Industry employment in township",
                                    term == "nr_totfirms_reg" ~ "Total N firms in township (10-3)",
                                    term == "singleind" ~ "Single industry employer",
                                    term == "sdemployer" ~ "Standard deviation of employer sizes",
                                    term == "log_indfirms" ~ "Log N industry firms in township",
                                    term == "nr_ind_in_reg" ~ "N industries in township",
                                    term == "ent" ~ "Entropy of industry shares",
                                    term == "indfirmXindreg" ~ "Log N firms X N industries",
                                    term == "indfirmXent" ~ "Log N firms X entropy"),
                   cat = factor(cat, levels=c("Control", "Vertical", "Horizontal", "Interactions")),
                   term = factor(term, levels=rev(c("Log mean industry wage", "Industry employment in township", "Total N firms in township (10-3)", "Single industry employer","Log N industry firms in township","Standard deviation of employer sizes", "N industries in township", "Entropy of industry shares","Log N firms X N industries","Log N firms X entropy")) )        )
        
        ## Import Coeffs from SS for comparisons
        directory_path_import <- "imported/ss_res/"
        name <- "res_ss"
        filename_import <- paste0(directory_path_import, name, ".txt")
        res_ss <- read.csv(filename_import, sep="")
        
        
        res_out <- merge(res_out, res_ss, by.x = c("term", "model"), by.y = c("terms", "model"), all.x = TRUE)
        
        res_out %>%
            filter(cat != "Control") %>%
            
            ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high))+
            geom_pointrange(size = 1, lwd = 1.65)+
            geom_pointrange(aes(x=ss_estimate, y= term, xmin = ss_conf.low, xmax = ss_conf.high),color = "red", size = 1, lwd = 1.65,position = position_jitter())+#position = position_jitter(width = 0.25))+
            geom_vline(xintercept = 0, linetype = "dashed", color = "blue", size  = 0.9)+
            #facet_wrap(~model, ncol = 5)+
            facet_grid(cat~model, space = "free", scales = "free_y", switch = "y")+
            labs(y = "Variable", x = "Coefficient")+
            theme_classic()+
            annotation_custom(grid::linesGrob(x = c(0,0), gp = grid::gpar(lwd = 3)))+
            theme(#panel.background.x= element_rect(fill = NA, color = "black"),
                
                strip.placement = "outside",
                strip.background = element_rect(fill = NA, color= NA),
                strip.text = element_text(size=20, 
                                          color = "black"),
                plot.title = element_text(size = 130,
                                          color = "black"),
                axis.text.x=element_text(size=15, 
                                         color = "black"),
                axis.text.y=element_text(size=15, 
                                         color = "black"),
                axis.title.x =element_text(size=25,
                                           color = "black"),
                axis.title.y =element_text(size=25,
                                           color = "black"),
                legend.position = c(0.15, 0.85), 
                strip.text.x = element_text(size = 20),
                legend.text = element_text(size = 20,
                                           color = "black"),
                legend.title = element_text(size = 20,
                                            color = "black"))
        
        directory_path_figure <- "output/figures/"
        name_figure <- "resvar_replication_la_"
        modified_period <- gsub(":", "_", period)
        filename_figure <- paste0(directory_path_figure, name_figure, modified_period, ".png")
        
        ggsave(width =  15,
               height = 7.5,
               dpi = 300,
               file = filename_figure) 
    
    }
    
}

#################################################################################


# Set of Models: Extensions 

#Prepare for models/tables
time_periods <- c(1998, 2012)

for (period in time_periods){
    DT <- DT_final[year <= period]
    ## 1992 - 2012: Extension and adding foreign. Models: Residual wage #####
    m1b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + ratio_foreign | sni + year, data = DT, vcov = ~mun)
    m2b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + ratio_noprofit | sni + year, data = DT, vcov = ~mun)
    m3b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit | sni + year, data = DT, vcov = ~mun)
    m4b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms | sni + year, data = DT, vcov = ~mun)
    m5b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + nr_ind_in_reg | sni + year, data = DT, vcov = ~mun)
    m6b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + ent | sni + year, data = DT, vcov = ~mun)
    m7b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + nr_ind_in_reg + indfirmXindreg | sni + year, data = DT, vcov = ~mun)
    m8b <- feols(residual_ln ~ indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + ent + indfirmXent | sni + year, data = DT, vcov = ~mun)
    etable(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b, vcov = ~mun)
    tex8 <- etable(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b, dict = dict, tex=TRUE,
                 digits = "r3",
                 digits.stats = "r3",
                 depvar = FALSE,
                 signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
                 fitstat =  ~r2 + wr2 + n)
    
    #Name and save tables
    directory_path <- "output/tables/"
    name <- "table_residwage_extension_la_"
    modified_period <- gsub(":", "_", period)
    filename <- paste0(directory_path, name, modified_period, ".tex")
    write(tex8, file=filename)
    
    if (period == 1998)  {
    
        #Prepare for figures
        #Mutate 
        m1b <- tidy(m1b, conf.int = TRUE) %>%
        mutate(model = "Model 1",
               outcome = "res_wage")
        
        m2b <- tidy(m2b, conf.int = TRUE) %>%
        mutate(model = "Model 2",
               outcome = "res_wage")
        
        
        m3b <- tidy(m3b, conf.int = TRUE) %>%
        mutate(model = "Model 3",
               outcome = "res_wage")
        
        
        m4b <- tidy(m4b, conf.int = TRUE) %>%
        mutate(model = "Model 4",
               outcome = "res_wage")
        
        
        m5b <- tidy(m5b, conf.int = TRUE) %>%
        mutate(model = "Model 5",
               outcome = "res_wage")
        
        
        m6b <- tidy(m6b, conf.int = TRUE) %>%
        mutate(model = "Model 6",
               outcome = "res_wage")
        
        
        m7b <- tidy(m7b, conf.int = TRUE) %>%
        mutate(model = "Model 7",
               outcome = "res_wage")
        
        
        m8b <- tidy(m8b, conf.int = TRUE) %>%
        mutate(model = "Model 8",
               outcome = "res_wage")
        
        #Save output to res_out
        res_out <- rbind(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b)
        #remove models
        rm(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b)
    
    
        res_out <- res_out %>% 
          mutate(cat = case_when(term ==  "grosswage_ln" ~ "Vertical",
                                  term == "meanwage_ln"  ~ "Control",
                                  term == "ratio_noprofit" ~ "Extension",
                                  term == "ratio_foreign" ~ "Extension",
                                  term == "indreg_size_ind" ~ "Control",
                                  term == "nr_totfirms_reg" ~ "Control",
                                  term == "singleind" ~ "Vertical",
                                  term == "sdemployer" ~ "Horizontal",
                                  term == "log_indfirms" ~ "Vertical",
                                  term == "nr_ind_in_reg" ~ "Horizontal",
                                  term == "ent" ~ "Horizontal",
                                  term == "indfirmXindreg" ~ "Interactions",
                                  term == "indfirmXent" ~ "Interactions"),
                 term = case_when(term == "meanwage_ln"  ~ "Log mean industry wage",
                                  term == "ratio_noprofit" ~ "Ratio non-profit",
                                  term == "ratio_foreign" ~ "Ratio foreing-owned",
                                  term == "indreg_size_ind" ~ "Industry employment in township",
                                  term == "nr_totfirms_reg" ~ "Total N firms in township (10-3)",
                                  term == "singleind" ~ "Single industry employer",
                                  term == "sdemployer" ~ "Standard deviation of employer sizes",
                                  term == "log_indfirms" ~ "Log N industry firms in township",
                                  term == "nr_ind_in_reg" ~ "N industries in township",
                                  term == "ent" ~ "Entropy of industry shares",
                                  term == "indfirmXindreg" ~ "Log N firms X N industries",
                                  term == "indfirmXent" ~ "Log N firms X entropy"),
                 cat = factor(cat, levels=c("Control", "Extension", "Vertical", "Horizontal", "Interactions")),
                 term = factor(term, levels=rev(c("Ratio non-profit","Ratio foreing-owned","Log mean industry wage", "Industry employment in township", "Total N firms in township (10-3)", "Single industry employer","Log N industry firms in township","Standard deviation of employer sizes", "N industries in township", "Entropy of industry shares","Log N firms X N industries","Log N firms X entropy")) )        )
          
        
        res_out %>%
          filter(cat != "Control") %>%
          
          ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high))+
          geom_pointrange(size = 1, lwd = 1.65)+
         # geom_pointrange(aes(x=ss_estimate, y= term, xmin = ss_conf.low, xmax = ss_conf.high),color = "red", size = 1, lwd = 1.65,position = position_jitter())+#position = position_jitter(width = 0.25))+
          geom_vline(xintercept = 0, linetype = "dashed", color = "blue", size  = 0.9)+
          #facet_wrap(~model, ncol = 5)+
          facet_grid(cat~model, space = "free_y", scales = "free", switch = "y")+
          labs(y = "Variable", x = "Coefficient", color = "Model")+
          theme_classic()+
          annotation_custom(grid::linesGrob(x = c(0,0), gp = grid::gpar(lwd = 3)))+
          theme(#panel.background.x= element_rect(fill = NA, color = "black"),
            
            strip.placement = "outside",
            strip.background = element_rect(fill = NA, color= NA),
            strip.text = element_text(size=20, 
                                      color = "black"),
            plot.title = element_text(size = 130,
                                      color = "black"),
            axis.text.x=element_text(size=15, 
                                     color = "black"),
            axis.text.y=element_text(size=15, 
                                     color = "black"),
            axis.title.x =element_text(size=25,
                                       color = "black"),
            axis.title.y =element_text(size=25,
                                       color = "black"),
            legend.position = ("bottom"), 
            strip.text.x = element_text(size = 20),
            legend.text = element_text(size = 20,
                                       color = "black"),
            legend.title = element_text(size = 20,
                                        color = "black"))
        
        directory_path_figure <- "output/figures/"
        name <- "resvar_extension_la_"
        modified_period <- gsub(":", "_", period)
        filename_figure <- paste0(directory_path_figure, name, modified_period, ".png")
        
        ggsave(width =  15,
               height = 7.5,
               dpi = 300,
               file = filename_figure) 
    }    
}    









######################## GROSS WAGE #############################################


# Set of Models: Replications
time_periods <- c(1998, 2012)

for (period in time_periods){
    DT <- DT_final[year <= period]
    ##### Models: Gross wage #####
    m1b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms | sni + year, data = DT, vcov = ~mun)
    m2b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg | sni + year, data = DT, vcov = ~mun)
    m3b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent | sni + year, data = DT, vcov = ~mun)
    m4b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + nr_ind_in_reg + indfirmXindreg | sni + year, data = DT, vcov = ~mun)
    m5b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + singleind + log_indfirms + sdemployer + ent + indfirmXent | sni + year, data = DT, vcov = ~mun)
    etable(m1b, m2b, m3b, m4b, m5b, vcov = ~mun)
    tex8 <- etable(m1b, m2b, m3b, m4b, m5b, dict = dict, tex=TRUE,
                   digits = "r3",
                   depvar = FALSE,
                   digits.stats = "r3",
                   signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
                   fitstat =  ~r2 + wr2 + n)
    
    #Name and save
    directory_path <- "output/tables/"
    name <- "table_grosswage_replication_la_"
    modified_period <- gsub(":", "_", period)
    filename <- paste0(directory_path, name, modified_period, ".tex")
    write(tex8, file=filename)
    
    if (period == 1998)  {
        
        m1b <- tidy(m1b, conf.int = TRUE) %>%
            mutate(model = "Model 1",
                   outcome = "gross_wage")
        
        m2b <- tidy(m2b, conf.int = TRUE) %>%
            mutate(model = "Model 2",
                   outcome = "gross_wage")
        
        
        m3b <- tidy(m3b, conf.int = TRUE) %>%
            mutate(model = "Model 3",
                   outcome = "gross_wage")
        
        
        m4b <- tidy(m4b, conf.int = TRUE) %>%
            mutate(model = "Model 4",
                   outcome = "gross_wage")
        
        
        m5b <- tidy(m5b, conf.int = TRUE) %>%
            mutate(model = "Model 5",
                   outcome = "gross_wage")
        
        gross_out <- rbind(m1b, m2b, m3b, m4b, m5b)
        
        rm(m1b, m2b, m3b, m4b, m5b)

    gross_out <- gross_out %>% 
        mutate(cat = case_when(term ==  "grosswage_ln" ~ "Vertical",
                               term == "meanwage_ln"  ~ "Control",
                               # term == "residual_ln" ~ "Residual wage dispersion",
                               #  term == "residual_ln_s" ~ "Residual wage dispersion_adj",
                               term == "indreg_size_ind" ~ "Control",
                               term == "nr_totfirms_reg" ~ "Control",
                               term == "singleind" ~ "Vertical",
                               term == "sdemployer" ~ "Horizontal",
                               term == "log_indfirms" ~ "Vertical",
                               term == "nr_ind_in_reg" ~ "Horizontal",
                               term == "ent" ~ "Horizontal",
                               term == "indfirmXindreg" ~ "Interactions",
                               term == "indfirmXent" ~ "Interactions"),
               term = case_when(term == "meanwage_ln"  ~ "Log mean industry wage",
                                term == "indreg_size_ind" ~ "Industry employment in township",
                                term == "nr_totfirms_reg" ~ "Total N firms in township (10-3)",
                                term == "singleind" ~ "Single industry employer",
                                term == "sdemployer" ~ "Standard deviation of employer sizes",
                                term == "log_indfirms" ~ "Log N industry firms in township",
                                term == "nr_ind_in_reg" ~ "N industries in township",
                                term == "ent" ~ "Entropy of industry shares",
                                term == "indfirmXindreg" ~ "Log N firms X N industries",
                                term == "indfirmXent" ~ "Log N firms X entropy"),
               cat = factor(cat, levels=c("Control", "Vertical", "Horizontal", "Interactions")),
               term = factor(term, levels=rev(c("Log mean industry wage", "Industry employment in township", "Total N firms in township (10-3)", "Single industry employer","Log N industry firms in township","Standard deviation of employer sizes", "N industries in township", "Entropy of industry shares","Log N firms X N industries","Log N firms X entropy")) )        )
    
    gross_ss <- read.csv("//micro.intra/projekt/P0515$/P0515_Gem/Karl/NiklasKarl/project1/version3_socio_2oct2023_v1/imported/ss_res/gross_ss.txt", sep="")
    
    
    gross_out <- merge(gross_out, gross_ss, by.x = c("term", "model"), by.y = c("terms", "model"), all.x = TRUE)
    
    
    gross_out %>%
        filter(cat != "Control") %>%
        ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high))+
        
        geom_pointrange(size = 1, lwd = 1.65)+
        geom_pointrange(aes(x=ss_estimate, y= term, xmin = ss_conf.low, xmax = ss_conf.high),color = "red", size = 1, lwd = 1.65,position = position_jitter())+#position = position_jitter(width = 0.25))+
        geom_vline(xintercept = 0, linetype = "dashed", color = "blue", size  = 0.9)+
        #facet_wrap(~model, ncol = 5)+
        facet_grid(cat~model, space = "free", scales = "free_y", switch = "y")+
        labs(y = "Variable", x = "Coefficient")+
        theme_classic()+
        annotation_custom(grid::linesGrob(x = c(0,0), gp = grid::gpar(lwd = 3)))+
        theme(#panel.background.x= element_rect(fill = NA, color = "black"),
            
            strip.placement = "outside",
            strip.background = element_rect(fill = NA, color= NA),
            strip.text = element_text(size=20, 
                                      color = "black"),
            plot.title = element_text(size = 130,
                                      color = "black"),
            axis.text.x=element_text(size=15, 
                                     color = "black"),
            axis.text.y=element_text(size=15, 
                                     color = "black"),
            axis.title.x =element_text(size=25,
                                       color = "black"),
            axis.title.y =element_text(size=25,
                                       color = "black"),
            legend.position = c(0.15, 0.85), 
            strip.text.x = element_text(size = 20),
            legend.text = element_text(size = 20,
                                       color = "black"),
            legend.title = element_text(size = 20,
                                        color = "black"))
    
    
    directory_path_figure <- "output/figures/"
    name <- "grossvar_replication_la_"
    modified_period <- gsub(":", "_", period)
    filename_figure <- paste0(directory_path_figure, name, modified_period, ".png")
    
    ggsave(width =  15,
           height = 7.5,
           dpi = 300,
           file = filename_figure) 
    
    }
}
    



#################################################################################


# Set of Models: Extensions

time_periods <- c(1998, 2012)


for (period in time_periods){
  DT <- DT_final[year <= period]
  ## 1992 - 2012: Extension and adding foreign. Models: Gross wage #####
  m1b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign | sni + year, data = DT, vcov = ~mun)
  m2b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_noprofit | sni + year, data = DT, vcov = ~mun)
  m3b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit | sni + year, data = DT, vcov = ~mun)
  m4b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms | sni + year, data = DT, vcov = ~mun)
  m5b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + nr_ind_in_reg | sni + year, data = DT, vcov = ~mun)
  m6b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + ent | sni + year, data = DT, vcov = ~mun)
  m7b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + nr_ind_in_reg + indfirmXindreg | sni + year, data = DT, vcov = ~mun)
  m8b <- feols(grosswage_ln ~ meanwage_ln + indreg_size_ind + nr_totfirms_reg + ratio_foreign + ratio_noprofit + singleind + log_indfirms + sdemployer + ent + indfirmXent | sni + year, data = DT, vcov = ~mun)
  etable(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b, vcov = ~mun)
  tex8 <- etable(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b, dict = dict, tex=TRUE,
                 digits = "r3",
                 depvar = FALSE,
                 digits.stats = "r3",
                 signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
                 fitstat =  ~r2 + wr2 + n)
  
  #Name and save
  directory_path <- "output/tables/"
  name <- "table_grosswage_extension_la_"
  modified_period <- gsub(":", "_", period)
  filename <- paste0(directory_path, name, modified_period, ".tex")
  write(tex8, file=filename)
  
  
  if (period == 1998)  {
  
      m1b <- tidy(m1b, conf.int = TRUE) %>%
        mutate(model = "Model 1",
               outcome = "gross_wage")
      
      m2b <- tidy(m2b, conf.int = TRUE) %>%
        mutate(model = "Model 2",
               outcome = "gross_wage")
      
      
      m3b <- tidy(m3b, conf.int = TRUE) %>%
        mutate(model = "Model 3",
               outcome = "gross_wage")
      
      
      m4b <- tidy(m4b, conf.int = TRUE) %>%
        mutate(model = "Model 4",
               outcome = "gross_wage")
      
      
      m5b <- tidy(m5b, conf.int = TRUE) %>%
        mutate(model = "Model 5",
               outcome = "gross_wage")
      
      
      m6b <- tidy(m6b, conf.int = TRUE) %>%
        mutate(model = "Model 6",
               outcome = "gross_wage")
      
      
      m7b <- tidy(m7b, conf.int = TRUE) %>%
        mutate(model = "Model 7",
               outcome = "gross_wage")
      
      
      m8b <- tidy(m8b, conf.int = TRUE) %>%
        mutate(model = "Model 8",
               outcome = "gross_wage")
      
      
      gross_out <- rbind(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b)
      rm(m1b, m2b, m3b, m4b, m5b)



    gross_out <- gross_out %>% 
      mutate(cat = case_when(term ==  "grosswage_ln" ~ "Vertical",
                             term == "meanwage_ln"  ~ "Control",
                             term == "ratio_noprofit" ~ "Extension",
                             term == "ratio_foreign" ~ "Extension",
                             term == "indreg_size_ind" ~ "Control",
                             term == "nr_totfirms_reg" ~ "Control",
                             term == "singleind" ~ "Vertical",
                             term == "sdemployer" ~ "Horizontal",
                             term == "log_indfirms" ~ "Vertical",
                             term == "nr_ind_in_reg" ~ "Horizontal",
                             term == "ent" ~ "Horizontal",
                             term == "indfirmXindreg" ~ "Interactions",
                             term == "indfirmXent" ~ "Interactions"),
             term = case_when(term == "meanwage_ln"  ~ "Log mean industry wage",
                              term == "ratio_noprofit" ~ "Ratio non-profit",
                              term == "ratio_foreign" ~ "Ratio foreing-owned",
                              term == "indreg_size_ind" ~ "Industry employment in township",
                              term == "nr_totfirms_reg" ~ "Total N firms in township (10-3)",
                              term == "singleind" ~ "Single industry employer",
                              term == "sdemployer" ~ "Standard deviation of employer sizes",
                              term == "log_indfirms" ~ "Log N industry firms in township",
                              term == "nr_ind_in_reg" ~ "N industries in township",
                              term == "ent" ~ "Entropy of industry shares",
                              term == "indfirmXindreg" ~ "Log N firms X N industries",
                              term == "indfirmXent" ~ "Log N firms X entropy"),
             cat = factor(cat, levels=c("Control", "Extension", "Vertical", "Horizontal", "Interactions")),
             term = factor(term, levels=rev(c("Ratio non-profit","Ratio foreing-owned","Log mean industry wage", "Industry employment in township", "Total N firms in township (10-3)", "Single industry employer","Log N industry firms in township","Standard deviation of employer sizes", "N industries in township", "Entropy of industry shares","Log N firms X N industries","Log N firms X entropy")) )        )
    
    
    gross_out %>%
      filter(cat != "Control") %>%
      
      ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high))+
      geom_pointrange(size = 1, lwd = 1.65)+
      # geom_pointrange(aes(x=ss_estimate, y= term, xmin = ss_conf.low, xmax = ss_conf.high),color = "red", size = 1, lwd = 1.65,position = position_jitter())+#position = position_jitter(width = 0.25))+
      geom_vline(xintercept = 0, linetype = "dashed", color = "blue", size  = 0.9)+
      #facet_wrap(~model, ncol = 5)+
      facet_grid(cat~model, space = "free_y", scales = "free", switch = "y")+
      labs(y = "Variable", x = "Coefficient", color = "Model")+
      theme_classic()+
      annotation_custom(grid::linesGrob(x = c(0,0), gp = grid::gpar(lwd = 3)))+
      theme(#panel.background.x= element_rect(fill = NA, color = "black"),
        
        strip.placement = "outside",
        strip.background = element_rect(fill = NA, color= NA),
        strip.text = element_text(size=20, 
                                  color = "black"),
        plot.title = element_text(size = 130,
                                  color = "black"),
        axis.text.x=element_text(size=15, 
                                 color = "black"),
        axis.text.y=element_text(size=15, 
                                 color = "black"),
        axis.title.x =element_text(size=25,
                                   color = "black"),
        axis.title.y =element_text(size=25,
                                   color = "black"),
        legend.position = ("bottom"), 
        strip.text.x = element_text(size = 20),
        legend.text = element_text(size = 20,
                                   color = "black"),
        legend.title = element_text(size = 20,
                                    color = "black"))
    
    
    
    directory_path_figure <- "output/figures/"
    name <- "grossvar_extension_la_"
    modified_period <- gsub(":", "_", period)
    filename_figure <- paste0(directory_path_figure, name, modified_period, ".png")
    
    ggsave(width =  15,
           height = 7.5,
           dpi = 300,
           file = filename_figure) 
    
    }
}










################################################################################


# 
# 
# # Time interaction
# time_periods <- c(2012)
# directory_path <- "output/tables/"
# name <- "table_residwage_time_la_"
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
# name <- "table_grosswage_time_la_"
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
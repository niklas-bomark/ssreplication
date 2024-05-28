
rm(list = ls()) 

#Extract data from SQl database
setwd("//micro.intra/projekt/P0515$/P0515_Gem/Karl/NiklasKarl/project1/version3_socio_2oct2023_v1/output/figures/")
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
library(RColorBrewer)
library(tidyverse)
library(cowplot)
library(Dict)





# With sample error adjustment
df <- read.csv('//micro.intra/projekt/P0515$/P0515_Gem/Karl/NiklasKarl/project1/version3_socio_2oct2023_v1/data/multiverse_v3_with_erradj2023-10-02.csv')



### Preparation

#Names to regression models
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
keep_variables <- c("Single industry employer", "Log N industry firms in township", "Standard deviation of employer sizes", 
                    "N industries in township", "Entropy of industry shares", "Log N firms X N industries",  "Log N firms X entropy")


lookup_table <- Dict$new(
        "singleind" = "Single industry employer",
        "sdemployer" = "Standard deviation of employer sizes",
        "log_indfirms" = "Log N industry firms in township",
        "nr_ind_in_reg" = "N industries in township",
        "ent" = "Entropy of industry shares",
        "indfirmXindreg" = "Log N firms X N industries",
        "indfirmXent" = "Log N firms X entropy"
)



    

indepvar_list = c("singleind", "sdemployer", "log_indfirms", "nr_ind_in_reg", "ent", "indfirmXindreg", "indfirmXent")





getwd()


gross <- data.frame(var = c("meanwage_ln","meanwage_ln","meanwage_ln","meanwage_ln","meanwage_ln",
                       "indreg_size_ind", "indreg_size_ind","indreg_size_ind","indreg_size_ind","indreg_size_ind",
                       "nr_totfirms_reg","nr_totfirms_reg","nr_totfirms_reg","nr_totfirms_reg","nr_totfirms_reg",
                       "singleind","singleind","singleind","singleind","singleind",
                       "log_indfirms","log_indfirms","log_indfirms","log_indfirms","log_indfirms",
                       NA, "sdemployer", "sdemployer", "sdemployer", "sdemployer", 
                       NA, "nr_ind_in_reg", NA, "nr_ind_in_reg", NA, 
                       NA, NA,"ent",  NA, "ent", 
                       NA,NA, NA, "indfirmXindreg", NA,
                       NA, NA, NA, NA,"indfirmXent"
                       ),
                 s_s = c(1610.821, 1609.610, 1610.350, 1605.546, 1608.287,
                        -.048, -.053, -.058, -.037, -.057,
                        -5.041, -6.149, -4.428, 2.729, -2.328,
                        -131.595, -130.694, -128.853, -97.646, -118.034,
                        145.901, 141.133, 146.809, 160.780, 153.022, 
                        NA, 0.060, 0.071, 0.027, 0.068,
                        NA, .552, NA, -.021, NA, 
                        NA, NA, .465, NA, -6.274, 
                        NA, NA, NA, -1.579, NA, 
                        NA, NA, NA, NA, -25.263),
                        model = c(rep(1:5, 10)),
                        dep_var = c(rep("gross_wage", 50)))
print(gross)

res <- data.frame(var = c("indreg_size_ind","indreg_size_ind","indreg_size_ind","indreg_size_ind","indreg_size_ind",
                       "nr_totfirms_reg","nr_totfirms_reg","nr_totfirms_reg","nr_totfirms_reg","nr_totfirms_reg",
                       "singleind","singleind","singleind","singleind","singleind",
                       "log_indfirms","log_indfirms","log_indfirms","log_indfirms","log_indfirms",
                       NA, "sdemployer", "sdemployer","sdemployer","sdemployer",
                       NA, "nr_ind_in_reg",NA,"nr_ind_in_reg",NA,
                       NA, NA, "ent",NA,"ent",
                       NA,NA,NA,"indfirmXindreg", NA, 
                       NA,NA,NA,NA, "indfirmXent"),
               s_s = c(-.089, -.068, -.065, -.054, -.064,
                        2.417, 3.190, 1.798, 10.210, 3.675, 
                        -97.200, -101.893, -103.527, -75.792, -93.865, 
                        56.332, 61.922, 57.817, 77.315, 63.283,
                        NA, -.172, -.185, -.198, -.188,
                        NA,-.779,NA, -1.232,NA, 
                        NA, NA, -19.032, NA, -25.038, 
                        NA, NA, NA, -1.247, NA, 
                        NA, NA, NA, NA, -22.569),
model = c(rep(1:5, 9)),
dep_var = c(rep("res_wag", 45)))

print(res)
 
s_s <- rbind(gross, res)
s_s <- drop_na(s_s)

df <- merge(df, s_s, by = c("var","model", "dep_var"), all.x = TRUE)


df <- df %>%
    select(-X) %>%
    filter(dep_var == "res_wag") %>%
    filter(var == "log_indfirms" | var == "singleind" | var == "sdemployer" | var == "nr_ind_in_reg" | var == "ent" | var == "indfirmXent" | var == "indfirmXindreg") #%>%
    # mutate(diversity = case_when(var == "log_indfirms" | var == "singleind" ~ "Vertical differentiation")
 
 








## Create a specification curve figure for every independent variable in indepvar_list. Dependent variable -> residual wage dispersion 
counter = 1
for (varname in indepvar_list){
        
    p1 <- df %>%
        filter(dep_var == "res_wag") %>%
        filter(var == varname) %>%
        filter(
            case_when(
                varname == "singleind" ~ model == 2, #Not fine!
                varname == "log_indfirms" ~ model == 2,
                varname == "sdemployer" ~ model == 2,
                varname == "nr_ind_in_reg" ~ model == 2, 
                varname == "ent" ~ model == 3, #ok
                varname == "indfirmXindreg" ~ model == 4,
                varname == "indfirmXent" ~ model == 5,
                TRUE ~ FALSE))
    
    p2 <- p1    
    
        p1 <- p1 %>%
        arrange(coef) %>%
        mutate(id = 1:nrow(.)) %>%
        select(id, var, industries, salaries, public_sector, large_org, city_type) %>%
        gather("parameter_name", "parameter_option", -c(id,var)) %>%
        mutate(parameter_name = factor(str_replace(parameter_name, "_", "\n"))) %>%
        ggplot()+
        geom_point(aes(x = id, y = parameter_option), size = 0.25) +
        facet_wrap(~factor(var,  levels = c("log_indfirms", "singleind", "sdemployer", "nr_ind_in_reg", "ent", "indfirmXent", "indfirmXindreg")), scale = "free", nrow = 1)+
        
        facet_grid(parameter_name ~., space = "fixed", scales = "free_y", switch = "y" )+
        
        theme_minimal()+
        # facet_wrap(~factor(var, levels = c("log_indfirms", "singleind", "sdemployer", "nr_ind_in_reg", "ent", "indfirmXent", "indfirmXindreg")), scale = "free", nrow = 3)+
        theme(strip.placement = "outside", 
              strip.background = element_rect(fill = NA),
              panel.background = element_rect(fill = "lightgrey", colour = NA),
              panel.grid.major = element_line(color = "white"),
              panel.grid.minor = element_line(color = "white"),
              panel.spacing.x=unit(0.15, "cm"),
              #panel.spacing.y=unit(1, "cm"),
              
              strip.text.y = element_text(angle = 189, size = 8),
              panel.spacing = unit(1.25, "lines"),
              axis.text.x = element_blank(),
              text = element_text(face ="plain")
        ) +
        #labs(y = "Analytic decision space")
        labs(x = " ", y = "Analytic decision space")
        #labs(x = "Universe", y = "Analytic decision space") 
    
    #Remove box outline around variable names        
    p1 <- p1 + theme(strip.background = element_blank())  
    
    p2 <- p2 %>%
      arrange(coef, model) %>%
      mutate(id = 1:nrow(.),
             cl = se*1.96,
             sig = ifelse((sign(coef+cl)==sign(coef-cl)), TRUE, FALSE)) %>%
      ggplot() +
      geom_point(aes(x= id, y = coef, color = as.factor(sig)), size = 1) +
      #geom_ribbon(aes(x= id, y = coef, ymin = (coef-cl), ymax = (coef+cl)), fill = "grey70")+
      geom_hline(yintercept = 0, size = 1, color = "black", linetype = "dashed") +
      geom_hline(aes(yintercept = s_s), size = 1, linetype = "dashed", color = "red") +
     # facet_wrap(~factor(var,  levels = c("log_indfirms", "singleind", "sdemployer", "nr_ind_in_reg", "ent", "indfirmXent", "indfirmXindreg")), scale = "free", nrow = 1)+
      labs(y = paste("Coefficient: \n", lookup_table[varname]), x = "") +
      #labs(title = lookup_table$fullname[lookup_table$varname == varname], y = "Coefficient", x = " ")+ 
      theme_minimal()+
      theme(axis.text.x = element_blank(),
            strip.background = element_rect(fill = NA),
            panel.background = element_rect(fill = "lightgrey", colour = NA),
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_line(color = "white"),
            text = element_text(face ="plain"),
            legend.position = "none"
            )
    
    cowplot::plot_grid(p2, p1, axis = "bltr", align = "v", ncol = 1, rel_heights = c(1,1.5))
    
    pdffile <- paste(counter, "_output_errcorr_", varname, ".pdf", sep = "")  
    ggsave(pdffile, width = 5, height = 8, device="pdf")
    counter = counter + 1
}
 
 



# 
# 
# 
#   x <- df %>%
#     filter(dep_var == "res_wag") %>%
#   #mutate(iter = as.numeric(as.factor(with(df, paste(employ_sit, industries, salaries, public_sector, large_org, city_type, sep = "_")))),
#     mutate(cl = se*1.96,
#          sig = ifelse((sign(coef+cl)==sign(coef-cl)), TRUE, FALSE),
#          sign = sign(coef),
#          share_sig_plus = ifelse(sign == 1 & sig == TRUE, 1,0),
#          share_sig_neg = ifelse(sign == -1 & sig == TRUE, 1,0)) %>%
#   group_by(var, model, dep_var) %>%
#   summarise(share_sig_plus = sum(share_sig_plus)/n(),
#             share_sig_neg = sum(share_sig_neg)/n(),
#             share_nonsig = 1-(sum(sig)/n())) 
# 
#   write.csv(x, "corr_table_erradj.csv")
# 

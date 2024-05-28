Create the dataset

1) Run first each of the files: 
	- _load_packages
	- background
 	- ind, 
	- inst,
	- sni, 
	- township, 


When all the datafiles have been created, you can combine
them and then create the variables used in the study. 

2) Run 1_combine_files_v3

3) Run 2_variables_v3

Now, you should be ready to create models and the multiverse
analysis. 




_extra_variables_v3_local_labor_market is the same file as in
2_variables_v3 except we use local labor markets instead of
townships/municipalities. 




> sessionInfo()
R version 4.3.0 (2023-04-21 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows Server 2019 x64 (build 17763)

Matrix products: default


locale:
[1] LC_COLLATE=Swedish_Sweden.1252  LC_CTYPE=Swedish_Sweden.1252    LC_MONETARY=Swedish_Sweden.1252 LC_NUMERIC=C                   
[5] LC_TIME=Swedish_Sweden.1252    

time zone: Europe/Stockholm
tzcode source: internal

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] fixest_0.11.1      broom_1.0.4        modelsummary_1.4.1 sandwich_3.0-2     stargazer_5.2.3    lfe_2.9-0          Matrix_1.5-4      
 [8] lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0      purrr_1.0.1        readr_2.1.4        tidyr_1.3.0        tibble_3.2.1      
[15] ggplot2_3.4.2      tidyverse_2.0.0    dtplyr_1.3.1       RODBC_1.3-20       odbc_1.3.4         dplyr_1.1.2        doParallel_1.0.17 
[22] iterators_1.0.14   foreach_1.5.2      data.table_1.14.8 

loaded via a namespace (and not attached):
 [1] DBI_1.1.3               rlang_1.1.1             magrittr_2.0.3          dreamerr_1.2.3          compiler_4.3.0         
 [6] systemfonts_1.0.4       vctrs_0.6.2             rvest_1.0.3             httpcode_0.3.0          pkgconfig_2.0.3        
[11] crayon_1.5.2            fastmap_1.1.1           backports_1.4.1         ellipsis_0.3.2          utf8_1.2.3             
[16] promises_1.2.0.1        rmarkdown_2.21          tzdb_0.4.0              ragg_1.2.5              bit_4.0.5              
[21] xfun_0.39               jsonlite_1.8.4          blob_1.2.4              later_1.3.1             uuid_1.1-0             
[26] R6_2.5.1                tables_0.9.17           stringi_1.7.12          numDeriv_2016.8-1.1     assertthat_0.2.1       
[31] Rcpp_1.0.10             knitr_1.43              zoo_1.8-12              httpuv_1.6.11           timechange_0.2.0       
[36] tidyselect_1.2.0        rstudioapi_0.14         codetools_0.2-19        curl_5.0.0              lattice_0.21-8         
[41] shiny_1.7.4             withr_2.5.0             flextable_0.9.1         askpass_1.1             evaluate_0.21          
[46] huxtable_5.5.2          zip_2.3.0               xml2_1.3.4              pillar_1.9.0            DT_0.28                
[51] insight_0.19.2          generics_0.1.3          hms_1.1.3               munsell_0.5.0           scales_1.2.1           
[56] xtable_1.8-4            glue_1.6.2              gdtools_0.3.3           tools_4.3.0             gfonts_0.2.0           
[61] webshot_0.5.4           grid_4.3.0              colorspace_2.1-0        nlme_3.1-162            Formula_1.2-5          
[66] cli_3.6.1               kableExtra_1.3.4        textshaping_0.3.6       officer_0.6.2           fontBitstreamVera_0.1.1
[71] fansi_1.0.4             viridisLite_0.4.2       svglite_2.1.1           gt_0.9.0                gtable_0.3.3           
[76] digest_0.6.31           fontquiver_0.2.1        crul_1.4.0              htmlwidgets_1.6.2       htmltools_0.5.5        
[81] lifecycle_1.0.3         httr_1.4.6              mime_0.12               fontLiberation_0.1.0    openssl_2.0.6          
[86] bit64_4.0.5 

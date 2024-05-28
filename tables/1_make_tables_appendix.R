

################################################################################
### Main data: 1992 - 1998
################################################################################

# Load required packages
library(xtable)
library(readxl)


# Set the working directory
setwd("/Users/nick/Dropbox/Research/Projects/Diversity Project/ss_socius_2oct2023/versions_22may/make_tables")




################################################################################
### Table 1a: Summary Statistics

latex_file <- readLines("table_sumstat_1998.tex")

# Define the new caption
new_caption <- "\\\\caption\\{Summary Statistics: 1992 - 1998\\}"

# Find and replace the caption
latex_file <- gsub("\\\\caption\\{Summary Statistics\\}", new_caption, latex_file)

#latex_file

# Save edited LaTeX table as .tex file
writeLines(latex_file, "table1a.tex")




################################################################################
### Table 2a: Correlation matrix

# Import the correlation matrix from CSV file without row names
cor_matrix <- read.csv("table_sumstat_1998_cor_.csv", header = TRUE)

# Rename row names
rownames(cor_matrix) <- c("Gross wage dispersion", "Log mean industry wage", "Residual wage dispersion",
                          "Industry employment in township", "Total N firms in township (10-3)", 
                          "Single industry employer", "Standard deviation of employer sizes",
                          "Log N industry firms in township", "N industries in township",
                          "Entropy of industry shares", "Ratio of foreign owned firms in industry-township",
                          "Ratio of non-profit making firms in industry-township")

# Rename column names
colnames(cor_matrix) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# Round the correlation values to two decimal places
cor_matrix[,] <- round(cor_matrix[,], 2)

# Set upper triangle elements to NA
cor_matrix[upper.tri(cor_matrix)] <- NA

# Set diagonal elements to NA
diag(cor_matrix) <- NA

# Drop the last column
cor_matrix <- cor_matrix[, -ncol(cor_matrix)]


# Write LaTeX code to a file
file_path <- "table2a.tex"

# Assuming latex_table is your data frame
print(xtable(cor_matrix, caption = "Correlation Matrix 1992 - 1998"), 
      file = file_path, caption.placement = "top", include.rownames = TRUE, 
      table.placement = "htb", align = c("left", rep("left", ncol(cor_matrix))))


# For some reason, I was not able to left center. Make an improvised solution... 

# Read the LaTeX file into R
latex_file <- readLines("table2a.tex")

# Print the contents of the file
print(latex_file)

# Identify the row that needs to be edited
# In this case, we want to change "\begin{tabular}{rrrrrrrrrrrr}" to "\begin{tabular}{lrrrrrrrrrrr}"

# Find the index of the row that needs to be edited
row_index <- grep("\\\\begin\\{tabular\\}\\{rrrrrrrrrrrr\\}", latex_file)

# Check if the row is found
if (length(row_index) > 0) {
    # Replace the row with the desired one
    latex_file[row_index] <- "\\begin{tabular}{lrrrrrrrrrrr}"
    
    # Write the updated LaTeX file back to disk
    print(latex_file)
    writeLines(latex_file, "table2a.tex")
    cat("File saved as updated_test.tex\n")
    
} else {
    cat("Row not found\n")
}




################################################################################
### Edit table 3: Gross wage dispersion

latex_file <- readLines("table_grosswage_replication_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Gross Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

# Add new text
#new_lines <- "\\multicolumn{6}{c}{\\textit{DV: Gross Income Dispersion}} \\\\ "

# Add new lines after line
#edited_table <- append(latex_file, new_lines, after = 5)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 13)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 20)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 29)
edited_table


# Delete the row
edited_table <- edited_table[-47]
edited_table <- edited_table[-47]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the yearly income from the largest workplace for 
    employees in a given industry in a township, multiplied by 1,000. Standard 
    errors are adjusted for clustering at the township level. Models include 
    dummy variables for year and industry. Interaction effects are centered. 
    Clustered (Township) standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 46)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 47)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table3.tex")




################################################################################
### Edit table 4: Residual wage dispersion


latex_file <- readLines("table_residwage_replication_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Residual Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

edited_table

# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 11)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 18)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 27)
edited_table

# Delete the row
edited_table <- edited_table[-45]
edited_table <- edited_table[-45]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the residuals from the first-stage human capital equation 
    regressions adjusted for sampling error, multiplied by 1,000. Standard errors 
    are adjusted for clustering at the township level. Models include dummy variables 
    for year and industry. Interaction effects are centered. Clustered (Township) 
    standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 44)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 45)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table4.tex")





################################################################################
### Edit table 5: Gross wage dispersion. Extended models.


latex_file <- readLines("table_grosswage_extension_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{9}{c}{\\\\textit{DV: Gross Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

# Add new text
#new_lines <- "\\multicolumn{6}{c}{\\textit{DV: Gross Income Dispersion}} \\\\ "

# Add new lines after line
#edited_table <- append(latex_file, new_lines, after = 5)
edited_table


# Add new text: H4
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H4} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 13)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 20)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 27)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 36)
edited_table


# Delete the row
edited_table <- edited_table[-54]
edited_table <- edited_table[-54]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{9}{p{24cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the yearly income from the largest workplace for 
    employees in a given industry in a township, multiplied by 1,000. Standard 
    errors are adjusted for clustering at the township level. Models include 
    dummy variables for year and industry. Interaction effects are centered. 
    Clustered (Township) standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 53)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{9}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 54)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table5.tex")





################################################################################
### Edit table 6: Residual wage dispersion. Extended models.


latex_file <- readLines("table_residwage_extension_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{9}{c}{\\\\textit{DV: Residual Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file
edited_table


# Add new text: H4
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H5} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 11)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 18)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 25)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 34)
edited_table


# Delete the row
edited_table <- edited_table[-52]
edited_table <- edited_table[-52]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{9}{p{24cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the residuals from the first-stage human capital equation 
    regressions adjusted for sampling error, multiplied by 1,000. Standard errors 
    are adjusted for clustering at the township level. Models include dummy variables 
    for year and industry. Interaction effects are centered. Clustered (Township) 
    standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 51)
edited_table

# Add new text: sig. symbols
new_line <- "\\multicolumn{9}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "

# Add new lines after line
edited_table <- append(edited_table, new_line, after = 52)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table6.tex")


#########################################################################################













################################################################################
### Main data: 1992 - 2012
################################################################################





################################################################################
### Table 1a: Summary Statistics

latex_file <- readLines("table_sumstat_2012.tex")

# Define the new caption
new_caption <- "\\\\caption\\{Summary Statistics: 1992 - 2012\\}"

# Find and replace the caption
latex_file <- gsub("\\\\caption\\{Summary Statistics\\}", new_caption, latex_file)

#latex_file

# Save edited LaTeX table as .tex file
writeLines(latex_file, "table1b.tex")




################################################################################
### Table 2a: Correlation matrix

# Import the correlation matrix from CSV file without row names
cor_matrix <- read.csv("table_sumstat_2012_cor_.csv", header = TRUE)

# Rename row names
rownames(cor_matrix) <- c("Gross wage dispersion", "Log mean industry wage", "Residual wage dispersion",
                          "Industry employment in township", "Total N firms in township (10-3)", 
                          "Single industry employer", "Standard deviation of employer sizes",
                          "Log N industry firms in township", "N industries in township",
                          "Entropy of industry shares", "Ratio of foreign owned firms in industry-township",
                          "Ratio of non-profit making firms in industry-township")

# Rename column names
colnames(cor_matrix) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# Round the correlation values to two decimal places
cor_matrix[,] <- round(cor_matrix[,], 2)

# Set upper triangle elements to NA
cor_matrix[upper.tri(cor_matrix)] <- NA

# Set diagonal elements to NA
diag(cor_matrix) <- NA

# Drop the last column
cor_matrix <- cor_matrix[, -ncol(cor_matrix)]


# Write LaTeX code to a file
file_path <- "table2b.tex"

# Assuming latex_table is your data frame
print(xtable(cor_matrix, caption = "Correlation Matrix 1992 - 2012"), 
      file = file_path, caption.placement = "top", include.rownames = TRUE, 
      table.placement = "htb", align = c("left", rep("left", ncol(cor_matrix))))


# For some reason, I was not able to left center. Make an improvised solution... 

# Read the LaTeX file into R
latex_file <- readLines("table2b.tex")

# Print the contents of the file
print(latex_file)

# Identify the row that needs to be edited
# In this case, we want to change "\begin{tabular}{rrrrrrrrrrrr}" to "\begin{tabular}{lrrrrrrrrrrr}"

# Find the index of the row that needs to be edited
row_index <- grep("\\\\begin\\{tabular\\}\\{rrrrrrrrrrrr\\}", latex_file)

# Check if the row is found
if (length(row_index) > 0) {
    # Replace the row with the desired one
    latex_file[row_index] <- "\\begin{tabular}{lrrrrrrrrrrr}"
    
    # Write the updated LaTeX file back to disk
    print(latex_file)
    writeLines(latex_file, "table2b.tex")
    cat("File saved as updated_test.tex\n")
    
} else {
    cat("Row not found\n")
}




################################################################################
### Edit table 3: Gross wage dispersion

latex_file <- readLines("table_grosswage_replication_2012.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Gross Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

# Add new text
#new_lines <- "\\multicolumn{6}{c}{\\textit{DV: Gross Income Dispersion}} \\\\ "

# Add new lines after line
#edited_table <- append(latex_file, new_lines, after = 5)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 13)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 20)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 29)
edited_table


# Delete the row
edited_table <- edited_table[-47]
edited_table <- edited_table[-47]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the yearly income from the largest workplace for 
    employees in a given industry in a township, multiplied by 1,000. Standard 
    errors are adjusted for clustering at the township level. Models include 
    dummy variables for year and industry. Interaction effects are centered. 
    Clustered (Township) standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 46)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 47)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table3b.tex")




################################################################################
### Edit table 4: Residual wage dispersion


latex_file <- readLines("table_residwage_replication_2012.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Residual Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

edited_table

# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 11)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 18)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 27)
edited_table

# Delete the row
edited_table <- edited_table[-45]
edited_table <- edited_table[-45]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the residuals from the first-stage human capital equation 
    regressions adjusted for sampling error, multiplied by 1,000. Standard errors 
    are adjusted for clustering at the township level. Models include dummy variables 
    for year and industry. Interaction effects are centered. Clustered (Township) 
    standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 44)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 45)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table4b.tex")





################################################################################
### Edit table 5: Gross wage dispersion. Extended models.


latex_file <- readLines("table_grosswage_extension_2012.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{9}{c}{\\\\textit{DV: Gross Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

# Add new text
#new_lines <- "\\multicolumn{6}{c}{\\textit{DV: Gross Income Dispersion}} \\\\ "

# Add new lines after line
#edited_table <- append(latex_file, new_lines, after = 5)
edited_table


# Add new text: H4
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H4} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 13)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 20)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 27)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 36)
edited_table


# Delete the row
edited_table <- edited_table[-54]
edited_table <- edited_table[-54]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{9}{p{24cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the yearly income from the largest workplace for 
    employees in a given industry in a township, multiplied by 1,000. Standard 
    errors are adjusted for clustering at the township level. Models include 
    dummy variables for year and industry. Interaction effects are centered. 
    Clustered (Township) standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 53)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{9}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 54)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table5b.tex")





################################################################################
### Edit table 6: Residual wage dispersion. Extended models.


latex_file <- readLines("table_residwage_extension_2012.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{9}{c}{\\\\textit{DV: Residual Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file
edited_table


# Add new text: H4
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H5} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 11)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 18)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 25)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 34)
edited_table


# Delete the row
edited_table <- edited_table[-52]
edited_table <- edited_table[-52]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{9}{p{24cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the residuals from the first-stage human capital equation 
    regressions adjusted for sampling error, multiplied by 1,000. Standard errors 
    are adjusted for clustering at the township level. Models include dummy variables 
    for year and industry. Interaction effects are centered. Clustered (Township) 
    standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 51)
edited_table

# Add new text: sig. symbols
new_line <- "\\multicolumn{9}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "

# Add new lines after line
edited_table <- append(edited_table, new_line, after = 52)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table6b.tex")


#########################################################################################




################################################################################
### LLM: 1992 - 1998
################################################################################



################################################################################
### Table 1a: Summary Statistics

latex_file <- readLines("table_sumstat_la_1998.tex")

# Define the new caption
new_caption <- "\\\\caption\\{Summary Statistics: 1992 - 1998. Local Labor Markets.\\}"

# Find and replace the caption
latex_file <- gsub("\\\\caption\\{Summary Statistics\\}", new_caption, latex_file)

#latex_file

# Save edited LaTeX table as .tex file
writeLines(latex_file, "table1c.tex")




################################################################################
### Table 2a: Correlation matrix

# Import the correlation matrix from CSV file without row names
cor_matrix <- read.csv("table_sumstat_la_1998_cor_.csv", header = TRUE)

# Rename row names
rownames(cor_matrix) <- c("Gross wage dispersion", "Log mean industry wage", "Residual wage dispersion",
                          "Industry employment in township", "Total N firms in township (10-3)", 
                          "Single industry employer", "Standard deviation of employer sizes",
                          "Log N industry firms in township", "N industries in township",
                          "Entropy of industry shares", "Ratio of foreign owned firms in industry-township",
                          "Ratio of non-profit making firms in industry-township")

# Rename column names
colnames(cor_matrix) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# Round the correlation values to two decimal places
cor_matrix[,] <- round(cor_matrix[,], 2)

# Set upper triangle elements to NA
cor_matrix[upper.tri(cor_matrix)] <- NA

# Set diagonal elements to NA
diag(cor_matrix) <- NA

# Drop the last column
cor_matrix <- cor_matrix[, -ncol(cor_matrix)]


# Write LaTeX code to a file
file_path <- "table2c.tex"

# Assuming latex_table is your data frame
print(xtable(cor_matrix, caption = "Correlation Matrix 1992 - 1998. Local Labor Markets."), 
      file = file_path, caption.placement = "top", include.rownames = TRUE, 
      table.placement = "htb", align = c("left", rep("left", ncol(cor_matrix))))


# For some reason, I was not able to left center. Make an improvised solution... 

# Read the LaTeX file into R
latex_file <- readLines("table2c.tex")

# Print the contents of the file
print(latex_file)

# Identify the row that needs to be edited
# In this case, we want to change "\begin{tabular}{rrrrrrrrrrrr}" to "\begin{tabular}{lrrrrrrrrrrr}"

# Find the index of the row that needs to be edited
row_index <- grep("\\\\begin\\{tabular\\}\\{rrrrrrrrrrrr\\}", latex_file)

# Check if the row is found
if (length(row_index) > 0) {
    # Replace the row with the desired one
    latex_file[row_index] <- "\\begin{tabular}{lrrrrrrrrrrr}"
    
    # Write the updated LaTeX file back to disk
    print(latex_file)
    writeLines(latex_file, "table2c.tex")
    cat("File saved\n")
    
} else {
    cat("Row not found\n")
}




################################################################################
### Edit table 3: Gross wage dispersion

latex_file <- readLines("table_grosswage_replication_la_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Gross Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

# Add new text
#new_lines <- "\\multicolumn{6}{c}{\\textit{DV: Gross Income Dispersion}} \\\\ "

# Add new lines after line
#edited_table <- append(latex_file, new_lines, after = 5)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 13)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 20)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 29)
edited_table


# Delete the row
edited_table <- edited_table[-47]
edited_table <- edited_table[-47]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the yearly income from the largest workplace for 
    employees in a given industry in a township, multiplied by 1,000. Standard 
    errors are adjusted for clustering at the township level. Models include 
    dummy variables for year and industry. Interaction effects are centered. 
    Clustered (Township) standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 46)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 47)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table3c.tex")




################################################################################
### Edit table 4: Residual wage dispersion


latex_file <- readLines("table_residwage_replication_la_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Residual Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

edited_table

# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 11)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 18)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 27)
edited_table

# Delete the row
edited_table <- edited_table[-45]
edited_table <- edited_table[-45]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the residuals from the first-stage human capital equation 
    regressions adjusted for sampling error, multiplied by 1,000. Standard errors 
    are adjusted for clustering at the township level. Models include dummy variables 
    for year and industry. Interaction effects are centered. Clustered (Township) 
    standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 44)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 45)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table4c.tex")





################################################################################
### Edit table 5: Gross wage dispersion. Extended models.


latex_file <- readLines("table_grosswage_extension_la_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{9}{c}{\\\\textit{DV: Gross Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

# Add new text
#new_lines <- "\\multicolumn{6}{c}{\\textit{DV: Gross Income Dispersion}} \\\\ "

# Add new lines after line
#edited_table <- append(latex_file, new_lines, after = 5)
edited_table


# Add new text: H4
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H4} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 13)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 20)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 27)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 36)
edited_table


# Delete the row
edited_table <- edited_table[-54]
edited_table <- edited_table[-54]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{9}{p{24cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the yearly income from the largest workplace for 
    employees in a given industry in a township, multiplied by 1,000. Standard 
    errors are adjusted for clustering at the township level. Models include 
    dummy variables for year and industry. Interaction effects are centered. 
    Clustered (Township) standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 53)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{9}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 54)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table5c.tex")





################################################################################
### Edit table 6: Residual wage dispersion. Extended models.


latex_file <- readLines("table_residwage_extension_la_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{9}{c}{\\\\textit{DV: Residual Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file
edited_table


# Add new text: H4
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H5} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 11)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 18)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 25)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 34)
edited_table


# Delete the row
edited_table <- edited_table[-52]
edited_table <- edited_table[-52]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{9}{p{24cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the residuals from the first-stage human capital equation 
    regressions adjusted for sampling error, multiplied by 1,000. Standard errors 
    are adjusted for clustering at the township level. Models include dummy variables 
    for year and industry. Interaction effects are centered. Clustered (Township) 
    standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 51)
edited_table

# Add new text: sig. symbols
new_line <- "\\multicolumn{9}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "

# Add new lines after line
edited_table <- append(edited_table, new_line, after = 52)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table6c.tex")


#########################################################################################







################################################################################
### LLM: 1992 - 2012
################################################################################





################################################################################
### Table 1a: Summary Statistics

latex_file <- readLines("table_sumstat_la_2012.tex")

# Define the new caption
new_caption <- "\\\\caption\\{Summary Statistics: 1992 - 2012. Local Labor Markets.\\}"

# Find and replace the caption
latex_file <- gsub("\\\\caption\\{Summary Statistics\\}", new_caption, latex_file)

#latex_file

# Save edited LaTeX table as .tex file
writeLines(latex_file, "table1d.tex")




################################################################################
### Table 2a: Correlation matrix

# Import the correlation matrix from CSV file without row names
cor_matrix <- read.csv("table_sumstat_la_2012_cor_.csv", header = TRUE)

# Rename row names
rownames(cor_matrix) <- c("Gross wage dispersion", "Log mean industry wage", "Residual wage dispersion",
                          "Industry employment in township", "Total N firms in township (10-3)", 
                          "Single industry employer", "Standard deviation of employer sizes",
                          "Log N industry firms in township", "N industries in township",
                          "Entropy of industry shares", "Ratio of foreign owned firms in industry-township",
                          "Ratio of non-profit making firms in industry-township")

# Rename column names
colnames(cor_matrix) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# Round the correlation values to two decimal places
cor_matrix[,] <- round(cor_matrix[,], 2)

# Set upper triangle elements to NA
cor_matrix[upper.tri(cor_matrix)] <- NA

# Set diagonal elements to NA
diag(cor_matrix) <- NA

# Drop the last column
cor_matrix <- cor_matrix[, -ncol(cor_matrix)]


# Write LaTeX code to a file
file_path <- "table2d.tex"

# Assuming latex_table is your data frame
print(xtable(cor_matrix, caption = "Correlation Matrix 1992 - 2012. Local Labor Markets."), 
      file = file_path, caption.placement = "top", include.rownames = TRUE, 
      table.placement = "htb", align = c("left", rep("left", ncol(cor_matrix))))


# For some reason, I was not able to left center. Make an improvised solution... 

# Read the LaTeX file into R
latex_file <- readLines("table2d.tex")

# Print the contents of the file
print(latex_file)

# Identify the row that needs to be edited
# In this case, we want to change "\begin{tabular}{rrrrrrrrrrrr}" to "\begin{tabular}{lrrrrrrrrrrr}"

# Find the index of the row that needs to be edited
row_index <- grep("\\\\begin\\{tabular\\}\\{rrrrrrrrrrrr\\}", latex_file)

# Check if the row is found
if (length(row_index) > 0) {
    # Replace the row with the desired one
    latex_file[row_index] <- "\\begin{tabular}{lrrrrrrrrrrr}"
    
    # Write the updated LaTeX file back to disk
    print(latex_file)
    writeLines(latex_file, "table2d.tex")
    cat("File saved\n")
    
} else {
    cat("Row not found\n")
}




################################################################################
### Edit table 3: Gross wage dispersion

latex_file <- readLines("table_grosswage_replication_la_2012.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Gross Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

# Add new text
#new_lines <- "\\multicolumn{6}{c}{\\textit{DV: Gross Income Dispersion}} \\\\ "

# Add new lines after line
#edited_table <- append(latex_file, new_lines, after = 5)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 13)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 20)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 29)
edited_table


# Delete the row
edited_table <- edited_table[-47]
edited_table <- edited_table[-47]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the yearly income from the largest workplace for 
    employees in a given industry in a township, multiplied by 1,000. Standard 
    errors are adjusted for clustering at the township level. Models include 
    dummy variables for year and industry. Interaction effects are centered. 
    Clustered (Township) standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 46)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 47)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table3d.tex")




################################################################################
### Edit table 4: Residual wage dispersion


latex_file <- readLines("table_residwage_replication_la_2012.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Residual Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

edited_table

# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 11)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 18)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 27)
edited_table

# Delete the row
edited_table <- edited_table[-45]
edited_table <- edited_table[-45]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the residuals from the first-stage human capital equation 
    regressions adjusted for sampling error, multiplied by 1,000. Standard errors 
    are adjusted for clustering at the township level. Models include dummy variables 
    for year and industry. Interaction effects are centered. Clustered (Township) 
    standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 44)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 45)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table4d.tex")





################################################################################
### Edit table 5: Gross wage dispersion. Extended models.


latex_file <- readLines("table_grosswage_extension_la_2012.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{9}{c}{\\\\textit{DV: Gross Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

# Add new text
#new_lines <- "\\multicolumn{6}{c}{\\textit{DV: Gross Income Dispersion}} \\\\ "

# Add new lines after line
#edited_table <- append(latex_file, new_lines, after = 5)
edited_table


# Add new text: H4
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H4} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 13)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 20)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 27)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 36)
edited_table


# Delete the row
edited_table <- edited_table[-54]
edited_table <- edited_table[-54]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{9}{p{24cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the yearly income from the largest workplace for 
    employees in a given industry in a township, multiplied by 1,000. Standard 
    errors are adjusted for clustering at the township level. Models include 
    dummy variables for year and industry. Interaction effects are centered. 
    Clustered (Township) standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 53)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{9}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 54)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table5d.tex")





################################################################################
### Edit table 6: Residual wage dispersion. Extended models.


latex_file <- readLines("table_residwage_extension_la_2012.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{9}{c}{\\\\textit{DV: Residual Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file
edited_table


# Add new text: H4
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H5} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 11)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 18)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 25)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 34)
edited_table


# Delete the row
edited_table <- edited_table[-52]
edited_table <- edited_table[-52]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{9}{p{24cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the residuals from the first-stage human capital equation 
    regressions adjusted for sampling error, multiplied by 1,000. Standard errors 
    are adjusted for clustering at the township level. Models include dummy variables 
    for year and industry. Interaction effects are centered. Clustered (Township) 
    standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 51)
edited_table

# Add new text: sig. symbols
new_line <- "\\multicolumn{9}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "

# Add new lines after line
edited_table <- append(edited_table, new_line, after = 52)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table6d.tex")


#########################################################################################





################################################################################
### Large organizations: 1992 - 1998
################################################################################



################################################################################
### Edit table 3: Gross wage dispersion

latex_file <- readLines("table_grosswage_replication_largefirms_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Gross Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

# Add new text
#new_lines <- "\\multicolumn{6}{c}{\\textit{DV: Gross Income Dispersion}} \\\\ "

# Add new lines after line
#edited_table <- append(latex_file, new_lines, after = 5)
edited_table


# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 15)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 22)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 31)
edited_table


# Delete the row
edited_table <- edited_table[-49]
edited_table <- edited_table[-49]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the yearly income from the largest workplace for 
    employees in a given industry in a township, multiplied by 1,000. Standard 
    errors are adjusted for clustering at the township level. Models include 
    dummy variables for year and industry. Interaction effects are centered. 
    Clustered (Township) standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 48)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 49)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table3e.tex")




################################################################################
### Edit table 4: Residual wage dispersion


latex_file <- readLines("table_residwage_replication_largefirms_1998.tex")
latex_file


# Find the position of \tabularnewline
tabularnewline_index <- grep("\\\\tabularnewline", latex_file)

# Insert text after \tabularnewline
latex_file[tabularnewline_index] <- gsub("\\\\tabularnewline", "\\\\tabularnewline \\\\multicolumn{6}{c}{\\\\textit{DV: Residual Income Dispersion}} \\\\\\\\", latex_file[tabularnewline_index])
edited_table <- latex_file

edited_table

# Add new text: H1
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H1} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 13)
edited_table


# Add new text: H2
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H2} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 20)
edited_table


# Add new text: H3
new_lines <- c("\\hdashline % Dotted line",
               "\\\\[0.1ex] % Add space of 1ex between rows",
               "\\emph{H3} \\\\ ")

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 29)
edited_table

# Delete the row
edited_table <- edited_table[-47]
edited_table <- edited_table[-47]

# Add new text: notes
new_lines <- c(
    "\\multicolumn{6}{p{16cm}}{\\emph{Notes:} The dependent variable is the log 
    standard deviation of the residuals from the first-stage human capital equation 
    regressions adjusted for sampling error, multiplied by 1,000. Standard errors 
    are adjusted for clustering at the township level. Models include dummy variables 
    for year and industry. Interaction effects are centered. Clustered (Township) 
    standard-errors in parentheses.}\\\\"
)

# Add new lines after line
edited_table <- append(edited_table, new_lines, after = 46)
edited_table


# Add new text: sig. symbols
new_line <- "\\multicolumn{6}{l}{\\emph{*** p$<$0.001, ** p$<$0.01, * p$<$0.05}} \\\\ "


# Add new lines after line
edited_table <- append(edited_table, new_line, after = 47)
edited_table


# Save edited LaTeX table as .tex file
writeLines(edited_table, "table4e.tex")




################################################################################

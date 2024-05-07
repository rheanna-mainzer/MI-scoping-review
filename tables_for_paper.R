# Tables for paper
# This script produces Tables 1 - 5 for the paper
#
# Written by R Mainzer
#
# Read data
load("data_clean.R")

# Table 1: Study characteristics -----------------------------------------------

# Summarise data using gtsummary package
table1_data <- data[, c("pub_year", "journal", "design_fct", "outcome_type", 
                        "incl_crit_1", "incl_crit_2", "caus_signal_1", 
                        "caus_signal_2", "caus_signal_3")]
table1 <- table1_data %>%
  gtsummary::tbl_summary(sort = list("design_fct" ~ "frequency")) %>%
  as.data.frame()

# Add header rows
colnames(table1) <- c("Characteristic", "Summary")
table1 <- dplyr::add_row(table1, Characteristic = "Inclusion criteria", Summary = NA, .before = 25)
table1 <- dplyr::add_row(table1, Characteristic = "Typical signals of causal questions", Summary = NA, .before = 28)

# Create flextable
table1_ft <- flextable(table1)
table1_ft <- set_table_properties(table1_ft, layout = "autofit")
table1_ft <- padding(table1_ft, i = c(2:4, 6:10, 12:19, 21:24, 26:27, 29:31), j = 1, padding.left = 20)
table1_ft

# Save output
save_as_docx("Table 1" = table1_ft, path = "tables/table1.docx")

# Table 1 footnote
data[data$design_fct == "Other", c("study_ID", "cov_ID", "design_details")]

# Table 2: Missing data amount -------------------------------------------------

# Select and summarise data
table2_data <- data[, c("inception_avail_fct", "analysis_reduc_fct", 
                        "complete_estab", "complete_p_estab", "complete_p_ub", "exposure_miss",
                        "exposure_miss_p_estab", "exposure_miss_p_lb", "outcome_miss", 
                        "outcome_miss_p_estab", "outcome_miss_p_lb", "cov_miss", "multi_miss_ind")]
table2 <- table2_data %>% 
  gtsummary::tbl_summary(type = list(inception_avail_fct ~ "categorical",
                                     analysis_reduc_fct ~ "categorical",
                                     exposure_miss_p_lb ~ "continuous",
                                     outcome_miss_p_lb ~ "continuous",
                                     multi_miss_ind ~ "categorical")) %>%
  as.data.frame()

# Add header rows
colnames(table2) <- c("Characteristic", "Summary")

# Create flextable
table2_ft <- flextable(table2)
table2_ft <- set_table_properties(table2_ft, layout = "autofit")
table2_ft <- padding(table2_ft, 
                     i = c(2:3, 5:6, 8:10, 12, 14, 16:20, 22, 24, 26:30, 32, 34, 36:39, 41:43), 
                     j = 1, padding.left = 20)
table2_ft

# Save output
save_as_docx("Table 2" = table2_ft, path = "tables/table2.docx")

# Table 2 footnote
# See "clean_dat.R"

# Table 3: Missingness assumptions ---------------------------------------------

# Select and summarise data
table3_data <- data[, c("miss_assumptions", "miss_justified", "mnar", "prim_ana_just")]
table3 <- table3_data %>% 
  gtsummary::tbl_summary(type = list(miss_justified ~ "categorical")) %>%
  as.data.frame()

# Add header rows
colnames(table3) <- c("Characteristic", "Summary")
#table3 <- dplyr::add_row(table3, Characteristic = "Missingness assumptions justified", Summary = NA, .before = 20)

# Create flextable
table3_ft <- flextable(table3)
table3_ft <- set_table_properties(table3_ft, layout = "autofit")
table3_ft <- padding(table3_ft, i = c(2:7, 9:10, 12:17, 19:20), j = 1, padding.left = 20)
table3_ft <- footnote(table3_ft, i = 1, j = 2, value = as_paragraph("Values are n (%)"),
                      part = "header", ref_symbols = "1")
table3_ft

# Save output
save_as_docx("Table 3" = table3_ft, path = "tables/table3.docx")

# Table 3 footnote 1:
# Missing data assumptions that were classified as "Other"
data[grep("Other", data$miss_assumptions), c("study_ID", "cov_ID", "miss_assumptions")]

# Table 3 footnote 2:
# Missing data assumption justifications for those that provided a statement about the missing data
just_ID <- setdiff(1:dim(data)[1], grep("No statement of missingness assumptions was provided", data$miss_assumptions))
flextable(data[intersect(just_ID, grep("Yes", data$miss_justified)), 
               c("cov_ID", "study_ID", "miss_assumptions", "miss_justified", "miss_justified_details")], 
          cwidth = c(1, 1, 2, 1, 9))

# Table 4: Primary and secondary analyses --------------------------------------

# Select and summarise data
table4_data <- data[, c("prim_ana", "second_cond", "second_ana", "second_ana_just", 
                        "mi_and_cca", "mi_cca_diff")]

# Sub-table 1
table4_1_data <- table4_data[, c("prim_ana", "second_cond")]
table4_1 <- table4_1_data %>% 
  gtsummary::tbl_summary(type = list(second_cond ~ "categorical")) %>% 
  as.data.frame()
colnames(table4_1) <- c("Characteristic", "Summary")

# Sub-table 2
table4_2_data <- filter(table4_data[, c("second_cond", "second_ana", "second_ana_just",
                                        "mi_and_cca", "mi_cca_diff")], second_cond == "Yes")
table4_2 <- table4_2_data[, c("second_ana", "second_ana_just", "mi_and_cca", "mi_cca_diff")] %>% 
  gtsummary::tbl_summary(type = list(mi_and_cca ~ "categorical")) %>% 
  as.data.frame()
colnames(table4_2) <- c("Characteristic", "Summary")

# Create table
table4 <- rbind(table4_1, table4_2)
table4_ft <- flextable(table4)
table4_ft <- set_table_properties(table4_ft, layout = "autofit")
table4_ft <- padding(table4_ft, i = c(2:10, 12:13, 15:21, 23:32, 34:35, 37:38), j = 1, padding.left = 20)
table4_ft

# Save output
save_as_docx("Table 4" = table4_ft, path = "tables/table4.docx")

# Details for footnotes

# 2: 
flextable(data[grep("Other", data$prim_ana), c("cov_ID", "prim_ana")], cwidth = c(2, 8))

# 3: See clean_dat.R

# 4: See clean_dat.R

# Table 5: MI implementation----------------------------------------------------

# Select and summarise data
table5_data <- data[, c("mi_m_reported", "mi_m", "mi_meth", "mi_soft", 
                        "mi_ana", "mi_aux", "mi_int", "fcs_models", 
                        "mi_pooled")]
table5 <- table5_data %>% 
  gtsummary::tbl_summary(statistic = 
                           list(mi_m ~ "{median} ({min} - {max})"),
                         type = list(mi_m_reported ~ "categorical")) %>%
  as.data.frame()

# Add header 
colnames(table5) <- c("Characteristic", "Summary")

# Create flextable
table5_ft <- flextable(table5)
table5_ft <- set_table_properties(table5_ft, layout = "autofit")
table5_ft <- padding(table5_ft, i = c(2:3, 5, 7:10, 
                                      12:18, 20:22, 
                                      24:26, 28:30,
                                      32:35), 
                     j = 1, padding.left = 20)
table5_ft

# Save output
save_as_docx("Table 5" = table5_ft, path = "tables/table5.docx")


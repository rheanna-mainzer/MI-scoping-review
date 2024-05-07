# Clean data
# This R script is used to clean the extracted data.
#
# Written by R Mainzer

# ------------------------------------------------------------------------------

# Read data
data <- read.csv("data_28feb24.csv")
data <- as_tibble(data)

# Subset to included studies where double data extraction was performed, 
# otherwise include studies that were reviewed by RM
data <- data[data$Reviewer.Name == "Rheanna Mainzer" | data$Reviewer.Name == "Consensus", ]
unique <- data %>% group_by(Covidence..) %>% filter(n()==1)
repeated <- data %>% group_by(Covidence..) %>% filter(n()>1)
repeated <- data %>% filter(Reviewer.Name == "Consensus")
data <- bind_rows(unique, repeated)
data <- data %>% arrange(desc(Covidence..))
rm("repeated", "unique")

# Rename variables
data <- data %>%
  rename(
    cov_ID = Covidence..,
    study_ID = Study.ID,
    title = Title,
    reviewer = Reviewer.Name,
    first_author = Authors,
    pub_date = Publication.date,
    journal = Journal,
    incl_crit = Inclusion.criteria,
    caus_signal = Did.the.study.use.any.of.the.following.approaches..typical.signals.of.a.causal.question..,
    caus_interpret = Causal.interpretation,
    design = Type.of.study.design,
    inception_avail = Was.the.size.of.the.inception.sample..for.the.research.question.of.interest.available.or.able.to.be.established.,
    inception_n = What.was.the.size.of.the.inception.sample..,
    analysis_reduc = Was.there.a.reduction.in.participants.from.the.inception.sample.to.the.analysis.sample.due.to.non.response.or.missing.data.in.a.variable.used.in.the.analysis..exposure..outcome..covariates..,
    analysis_n = What.was.the.size.of.the.analysis.sample..,
    complete_estab = Was.the.percentage.of.complete.cases..available.or.able.to.be.established.,
    complete_p = Percentage.of.complete.cases...upper.bound.on.the.percentage.of.complete.cases,
    exposure = What.was.the.exposure.,
    exposure_miss = Were.there.missing.values.in.the.exposure.,
    exposure_miss_p = Percentage.of.missing.values.in.the.exposure...lower.bound.on.the.percentage.of.missing.values.in.the.exposure,
    outcome = What.was.the.outcome.,
    outcome_miss = Were.there.missing.values.in.the.outcome.,
    outcome_miss_p = Percentage.of.missing.values.in.the.outcome...lower.bound.on.the.percentage.of.missing.values.in.the.outcome,
    cov_miss = Were.there.missing.values.in.the.covariates.,
    miss_assumptions = Was.a.statement.provided.about.what.missingness.assumptions.were.made.,
    miss_justified = Were.missingness.assumptions.justified.,
    miss_justified_details = Details.of.justification.for.missingness.assumptions,
    mnar = Did.authors.address.the.potential.for.data.to.be.MNAR.,
    prim_ana = Primary.method.of.analysis,
    prim_ana_just = Was.the.primary.analysis.justified.on.the.basis.of.missingness.assumptions.,
    prim_ana_just_detail = Details.of.justification.for.primary.analysis.on.the.basis.of.missingness.assumptions.,
    second_ana = Was.a.secondary.analysis.that.handles.missing.data.differently.used.to.answer.the.same.causal.question.,
    second_ana_just = Was.the.secondary.analysis.justified.,
    delta_adj_ext = If.a.delta.adjusted.analysis.was.used..was.external.information.incorporated.in.the.analysis.,
    delta_adj_detail = If.a.delta.adjusted.analysis.was.used..provide.details.of.the.delta.adjusted.analysis,
    mi_meth = What.method.was.used.for.multiple.imputation.,
    mi_soft = What.software.was.used.for.multiple.imputation.,
    mi_m = Number.of.imputations.used.in.the.multiple.imputation.procedure,
    mi_ana = Were.all.analysis.variables.included.in.the.imputation.model.,
    mi_aux = Were.auxiliary.variables.included.in.the.imputation.model.,
    mi_int = Were.interactions.included.in.the.imputation.model.,
    mi_cca_diff = If.results.were.obtained.using.both.a.CCA.and.MI..did.the.authors.observe.any.substantial.difference.between.these.,
    mi_cca_diff_details = If.results.were.obtained.using.both.a.CCA.and.MI..AND.no.substantial.difference.between.these.two.sets.of.results.was.observed..was.any.interpretation.or.explanation.provided.for.the.similarities.between.the.two.sets.of.results..If.so..what.was.the.interpretation.or.explanation.,
    other = Any.other.comments.
)

# Read in additional data collected for checking
add_data <- read_excel("additional_data_12apr24.xlsx")

# Merge with data
# .x variables - in original data set
# .y variables - in additional data set
data <- as_tibble(merge(data, add_data, by = c("cov_ID", "study_ID", "title")))
rm("add_data")

# Study characteristics ========================================================

## pub_year ----
data$pub_year <- factor(str_sub(data$pub_date, start = -2),
                        levels = c(19, 20, 21),
                        labels = c("2019", "2020", "2021"))
var_label(data$pub_year) <- "Year of publication"
summary(data$pub_year)

## journal -----
data$journal <- factor(data$journal)
var_label(data$journal) <- "Journal"
summary(data$journal)

## incl_crit_1 -----
data$incl_crit_1 <- factor(ifelse(grepl("Study estimated an effect of an exposure on an outcome", data$incl_crit), 1, 0),
                           levels = c(0, 1), labels = c("No", "Yes"))
var_label(data$incl_crit_1) <- "Effect of interest was given a causal interpretation"
summary(data$incl_crit_1)

## incl_crit_2 -----
data$incl_crit_2 <- factor(ifelse(grepl("Study stated they were estimating a causal effect", data$incl_crit), 1, 0),
                           levels = c(0, 1), labels = c("No", "Yes"))
var_label(data$incl_crit_2) <- "Effect of interest was explicity causal"
summary(data$incl_crit_2)

## caus_signal_1 ----
data$caus_signal_1 <- factor(ifelse(grepl("Study used a directed acyclic graph", data$caus_signal), 1, 0),
                             levels = c(0, 1), labels = c("No", "Yes"))
var_label(data$caus_signal_1) <- "DAG or m-DAG used to depict causal assumptions"
summary(data$caus_signal_1)

## caus_signal_2 ----
data$caus_signal_2 <- factor(ifelse(grepl("Study identified a set of variables", data$caus_signal), 1, 0),
                             levels = c(0, 1), labels = c("No", "Yes"))
var_label(data$caus_signal_2) <- "A set of variables were identified to control for confounding"
summary(data$caus_signal_2)

## caus_signal_3 ----
data$caus_signal_3 <- factor(ifelse(grepl("Study estimated an effect of an exposure on an outcome", data$caus_signal), 1, 0),
                             levels = c(0, 1), labels = c("No", "Yes"))
var_label(data$caus_signal_3) <- "Effect was estimated using a linear regression model with adjustment for a set of covariates"
summary(data$caus_signal_3)

## design ----
data$design_details <- ifelse(grepl("Other", data$design), data$design, NA)
data$design_fct <- factor(ifelse(grepl("Other", data$design), "Other", data$design),
                          levels = c("Prospective longitudinal study",
                                     "Individual patient data (IPD) meta-analysis / pooled cohort analysis",
                                     "Retrospective analysis of routinely collected data (e.g., administrative or EMR data)",
                                     "Interrupted time series (ITS)",
                                     "Case-control study",
                                     "Case-cohort study",
                                     "Cross-sectional study",
                                     "Other"), ordered = TRUE)

# Classify "other" types of studies
data[which(!is.na(data$design_details)), c("cov_ID", "study_ID", "design_details")]

data$design_details[data$cov_ID %in% 336]
data$design_fct[data$cov_ID %in% 336] <- "Prospective longitudinal study"

# Secondary analysis of trial
data$design_details[data$cov_ID %in% c(322, 42)]

# Follow-up of trial
data$design_details[data$cov_ID %in% c(261, 253)]

# Pooled data analysis
data$design_details[data$cov_ID %in% c(266, 265)]

# Retrospective analysis of linked data collected on cases
data$design_details[data$cov_ID %in% 254]
data$design_fct[data$cov_ID %in% 254] <- "Retrospective analysis of routinely collected data (e.g., administrative or EMR data)"

# Transportability study
data$design_details[data$cov_ID %in% 202]

var_label(data$design_fct) <- "Study design"
summary(data$design_fct)

## Outcome type ----
data$outcome_type <- factor(data$outcome_type, 
       levels = c("binary", "categorical(not_binary)", "continuous", "time_to_event"),
       labels = c("Binary", "Categorical (excluding binary)", "Continuous", "Time to event"))
var_label(data$outcome_type) <- "Outcome type"
summary(data$outcome_type)

# Missingness summaries ========================================================

## inception_avail ----
# Able to establish the size of the inception sample?
data$inception_avail
data$inception_avail_details <- ifelse(grepl("Other", data$inception_avail), data$inception_avail, NA)

# Classify "Other"
grep("Other", data$inception_avail)
data[grep("Other", data$inception_avail), c("cov_ID", "study_ID", "inception_avail")]

# 327: 
# - Some females excluded due to implausible menstrual cycle information.
# - 2485 females invited male partners to participate (inception sample?)
# - 1967 males enrolled but 11 excluded from analysis due to ineligible. 
# - Other invited males may have been ineligible but this is not known.
# Could not establish the size of inception sample
data$inception_avail[data$cov_ID %in% 327] <- "No"

# 302:
# TABLE 2 FOOTNOTE
# - All analyses appear to be conducted separately by sex 
# - The inception sample size is approximate as n = 32 were missing indicator of 
#   living children, but it is not clear whether these are male or female 
#   (so inception sample could range between 6374 and 6406). This is a small 
#   range, but the inception size could not be established.
data$inception_avail[data$cov_ID %in% 302] <- "No"

# 301:
# TABLE 2 FOOTNOTE
# - All analyses appear to be conducted separately by sex but sample selection 
#   flow chart presented for entire sample.
data$inception_avail[data$cov_ID %in% 301] <- "No"

# 261:
# TABLE 2 FOOTNOTE
# - All analyses conducted separately by site and sex. Inception sample size 
#   for the entire group is provided but not for the sub-groups.
data$inception_avail[data$cov_ID %in% 261] <- "No"

# 256:
# TABLE 2 FOOTNOTE
# - All analyses conducted separately by sex. Analysis of those included vs not 
#   included in analytical sample was not conducted separately by sex.
data$inception_avail[data$cov_ID %in% 256] <- "No"

# 178:
# - Eligibility for inclusion in study were RESPONDENTS 1) identifying as Black or White and 
#   2) with valid death data. It is not stated how many were excluded from the 
#   original cohort (n=3032) due to these criteria.
data$inception_avail[data$cov_ID %in% 178] <- "No"

# 170:
# TABLE 2 FOOTNOTE
# - Total inception sample size stated, but analysis was conducted separately 
#   by sex
data$inception_avail[data$cov_ID %in% 170] <- "No"

# 132:
# - A sub-sample of 79213 participants were selected from the UK Biobank cohort
#   for bicycle ergometer fitness testing , i.e., for this study.
# - It is not described how this sub-sample were selected, but assume this is 
#   the inception cohort (i.e., to be eligible for the study they had to be selected 
#   into the sub-cohort).
data$inception_avail[data$cov_ID %in% 132] <- "Yes"

# 16:
# - The number (%) of cases and controls among eligible participants was given,
#   but the sample was restricted further to those who either had ever worked 
#   at night for at least 1 year with a minimum frequency of 3 nights per month 
#   vs those who had never worked at night, i.e., additional eligibility 
#   criteria applied that were not used for the "inception sample"
data$inception_avail[data$cov_ID %in% 16] <- "No"

# Create factor variable
data$inception_avail_fct <- factor(data$inception_avail,
                                   levels = c("Yes", "No, eligibility criteria required one or more variables to be complete", "No"))
data$inception_avail_fct <- fct_collapse(data$inception_avail_fct, No = c("No, eligibility criteria required one or more variables to be complete", "No"))
var_label(data$inception_avail_fct) <- "Able to establish size of inception sample"
summary(data$inception_avail_fct)

## analysis_reduc ----

# Classify Other and NA
print(data[c(grep("Other", data$analysis_reduc), which(is.na(data$analysis_reduc)), 
             which(data$analysis_reduc == "")), 
           c("cov_ID", "study_ID", "analysis_reduc")], n = 24)

# Classify any study with (Yes + reason) as Yes
data[data$cov_ID %in% c(337, 253, 202, 170, 148, 134, 124, 123, 105, 63, 56, 44, 31, 20, 16, 12), 
     c("cov_ID", "study_ID", "analysis_reduc")]
data$analysis_reduc[data$cov_ID %in% c(337, 253, 202, 170, 148, 134, 124, 123, 105, 63, 56, 44, 31, 20, 16, 12)] <- "Yes"

# Classify any study with (No + reason) as No
data[data$cov_ID %in% c(328, 128), c("cov_ID", "study_ID", "analysis_reduc")]
data$analysis_reduc[data$cov_ID %in% c(328, 128)] <- "No"

# 335: 
# From a larger cohort, 1948 women were invited to do a nutrition component, 
# which was deployed midway through recruitment. Not clear why these women were 
# chosen to do the study, but assume this group was the inception cohort.
# There may be selection bias in results due to the (unknown) way in which the 
# group was selected.
data$analysis_reduc[data$cov_ID %in% 335] <- "No" 

# 332: 
# Included in this study were all participants aged >= 18 years from 6 CNICS 
# sites with comprehensive access to inpatient and outpatient electronic 
# medical records. 2 CNICS sites were excluded (assumed due to EMR NA - assumed
# that these would be included if data were available)
data$analysis_reduc[data$cov_ID %in% 332] <- "Yes"

# 233: 
# Study consisted of participants who were aged 60-64 years old in the 1946 
# National Survey of Health and Development with valid measures of physical 
# functioning. Unclear what "valid" means - (not missing or within a sensible
# range?) Assume that "valid" means not missing for at least some participants.
data$analysis_reduc[data$cov_ID %in% 233] <- "Yes"

# 62:
# Missing data by design and also not by design. Participants in the EPIC cohort
# had to have a blood sample collected to be eligible for the study, i.e., 
# eligibility criteria required a variable to be complete.
data$analysis_reduc[data$cov_ID %in% 62] <- "Yes"

# 22: 
# Participants are hospital patients that were treated with one of two 
# treatment protocols
data$analysis_reduc[data$cov_ID %in% 22] <- "No"

# 19:
# Eligibility criteria included (i) parental consent, (ii) singleton birth and 
# (iii) an available whole dried blood spot circle. 
data$analysis_reduc[data$cov_ID %in% 19] <- "Yes"

# Create factor variable
data$analysis_reduc_fct <- factor(data$analysis_reduc,
                                  levels = c("Yes", "Yes, missing data in an analysis variable",
                                  "Yes, non-response", "Yes, both non-response and missing data in an analysis variable",
                                  "No"))
data$analysis_reduc_fct <- fct_collapse(data$analysis_reduc_fct,
                                       Yes = c("Yes",
                                               "Yes, missing data in an analysis variable",
                                               "Yes, non-response",
                                               "Yes, both non-response and missing data in an analysis variable"))
var_label(data$analysis_reduc_fct) <- "Analysis sample was defined by excluding individuals with missing data"

## analysis_n ----
data$analysis_n <- as.numeric(data$analysis_n)
var_label(data$analysis_n) <- "Number in analysis sample"

## complete_estab ----
data$complete_estab <- factor(data$complete_estab,
                              levels = c("Yes",
                                         "Able to establish an upper bound only",
                                         "No"))
var_label(data$complete_estab) <- "Able to establish % of complete cases"

## complete_p ----
data$complete_p <- gsub("%", "", data$complete_p)
data$complete_p <- gsub("\n", "", data$complete_p)
data$complete_p <- as.numeric(data$complete_p)

## complete_p_estab -----
data$complete_p_estab <- ifelse(data$complete_estab == "Yes", data$complete_p, NA)
var_label(data$complete_p_estab) <- "% of complete cases"

## complete_p_ub -----
data$complete_p_ub <- ifelse(data$complete_estab == "Able to establish an upper bound only", data$complete_p, NA)
var_label(data$complete_p_ub) <- "Upper bound on % of complete cases"

## exposure_miss ----
data$exposure_miss <- factor(data$exposure_miss,
                             levels = c("Yes",
                                        "Yes, but only able to establish a lower bound on the percentage of missing values",
                                        "Yes, but unable to establish the percentage of missing values",
                                        "No",
                                        "Unclear"))
var_label(data$exposure_miss) <- "Missing exposure data"

## exposure_miss_p ----
data$exposure_miss_p <- gsub("%", "", data$exposure_miss_p)
data$exposure_miss_p <- as.numeric(data$exposure_miss_p)

data$exposure_miss_p_estab <- ifelse(data$exposure_miss == "Yes", data$exposure_miss_p, NA)
var_label(data$exposure_miss_p_estab) <- "% of missing values in exposure"

data$exposure_miss_p_lb <- ifelse(data$exposure_miss == "Yes, but only able to establish a lower bound on the percentage of missing values", data$exposure_miss_p, NA)
var_label(data$exposure_miss_p_lb) <- "Lower bound on % of missing values in exposure"

## outcome_miss ----
data$outcome_miss <- factor(data$outcome_miss,
                            levels = c("Yes",
                                       "Yes, but only able to establish a lower bound on the percentage of missing values",
                                       "Yes, but unable to establish the percentage of missing values",
                                       "No",
                                       "Unclear"))
var_label(data$outcome_miss) <- "Missing outcome data"

# After consensus realised there was an error in this - the option should have 
# been "Yes, but only able to establish a lower bound on the percentage of missing values"
# since it is stated that missing data in mediators and outcomes ranged from 
# 6 - 12%
data[data$cov_ID %in% 261, c("cov_ID", "study_ID", "outcome_miss", "outcome_miss_p")]
data$outcome_miss[data$cov_ID %in% 261] <- "Yes, but only able to establish a lower bound on the percentage of missing values"

# Table 2 footnotes:
table(data$outcome_miss, data$outcome_type)
data[which(data$outcome_type == "Time to event" & (data$outcome_miss == "Yes" | data$outcome_miss == "Unclear")),
     c("cov_ID", "study_ID", "outcome_miss", "outcome_type")]

# 202 - do not consider time to event data missing unless explicitly imputed
data[which(data$cov_ID == 202), "outcome_miss"] <- "No"

# 273
# "Finally, given the potential for selction bias by excluding women with missing
# perceived stress data or missing pregnancy outcome data (due to early withdrawal),
# we performed several sensitivity analyses on our primary outcome of time to pregnancy...
# Potential pregnancy outcome of women who withdrew was imputed using the following 
# three strategies..."

# 328
# "We used multiple imputation to impute missing covariate, exposure and outcome data...
# For the 24 (3.1%) women with no follow-up data, we imputed their ar-risk person-time (in years)
# and outcome status (fibroid: yes vs no)."

## outcome_miss_p ----
data$outcome_miss_p
data$outcome_miss_p <- gsub("%", "", data$outcome_miss_p)
data$outcome_miss_p[grepl("No", data$outcome_miss)] <- NA
data$outcome_miss_p[grepl("Unclear", data$outcome_miss)] <- NA

# Change study with "<1%" missing data to "1%" missing data (round-up)
grep("<1", data$outcome_miss_p)
data$outcome_miss_p[grep("<1", data$outcome_miss_p)] <- 1
data$outcome_miss_p <- as.numeric(data$outcome_miss_p )

## outcome_miss_p_estab ----
data$outcome_miss_p_estab <- ifelse(data$outcome_miss == "Yes", data$outcome_miss_p, NA)
var_label(data$outcome_miss_p_estab) <- "% of missing values in outcome"

## outcome_miss_p_lb -----
data$outcome_miss_p_lb <- ifelse(data$outcome_miss == "Yes, but only able to establish a lower bound on the percentage of missing values", data$outcome_miss_p, NA)
var_label(data$outcome_miss_p_lb) <- "Lower bound on % of missing values in outcome"

## cov_miss ----
data$cov_miss <- factor(data$cov_miss,
                        levels = c("Yes, in 2 or more covariates", 
                                   "Yes, in 1 covariate only",
                                   "No",
                                   "Unable to establish"))
var_label(data$cov_miss) <- "Missing values in the covariates"

## multivariable missingness ----
data$out_miss_ind <- ifelse(grepl("Yes", data$outcome_miss), 1, 0)
data$exp_miss_ind <- ifelse(grepl("Yes", data$exposure_miss), 1, 0)
data$cov_miss_fact <- ifelse(grepl("Yes, in 2 or more covariates", data$cov_miss), 2,
                        ifelse(grepl("Yes, in 1 covariate only", data$cov_miss), 1, 0))
data$multi_miss <- data$out_miss_ind + data$exp_miss_ind + data$cov_miss_fact
data$multi_miss_ind <- factor(ifelse(data$multi_miss >= 2, 1, 0),
                              levels = c(1, 0, 9), labels = c("Yes", "No", "Unable to establish"))
var_label(data$multi_miss_ind) <- "Multivariable missingness"

# If unclear for 2 or more variables then there may be multivariable missingness
flextable(data[union(union(grep("Unclear", data$outcome_miss), 
                 grep("Unclear", data$exposure_miss)),
           grep("Unable to establish", data$cov_miss)),
      c("cov_ID", "study_ID", "outcome_miss", "exposure_miss", "cov_miss", "multi_miss_ind")],
      cwidth = c(1, 2, 3, 3, 5, 1))

# Reclassify multi_miss_ind as "Unable to establish"
data$multi_miss_ind[data$cov_ID %in% c(302, 301, 296, 280, 103, 229, 169, 19)] <- "Unable to establish"

# Check those without multivariable missingness
flextable(data[data$multi_miss_ind == "No", c("cov_ID", "study_ID", "outcome_miss", "exposure_miss", "cov_miss", "multi_miss_ind")],
          cwidth = c(1, 2, 3, 3, 3, 3))

# Missingness assumptions ======================================================

## miss_assumptions ----
data$miss_assumptions_details <- ifelse(grepl("Other", data$miss_assumptions), data$miss_assumptions, NA)

# Classify assumptions described as "Other" 
data[grep("Other", data$miss_assumptions), c("cov_ID", "study_ID", "miss_assumptions")]

data$miss_assumptions_details[data$cov_ID %in% 327]
data$miss_assumptions[data$cov_ID %in% 327] <- "Yes, authors assumed data were MCAR"

data$miss_assumptions_details[data$cov_ID %in% 299]
data$miss_assumptions[data$cov_ID %in% 299] <- "Yes, authors assumed (either explicitly or implicitly) that data were not MCAR"

data$miss_assumptions_details[data$cov_ID %in% 244]
data$miss_assumptions[data$cov_ID %in% 244] <- "Yes, authors assumed (either explicitly or implicitly) that data were not MCAR"

data$miss_assumptions_details[data$cov_ID %in% 112]
data$miss_assumptions[data$cov_ID %in% 112] <- "Other"

data$miss_assumptions_details[data$cov_ID %in% 96]
data$miss_assumptions[data$cov_ID %in% 96] <- "Yes, authors assumed (either explicitly or implicitly) that data were not MCAR"

data$miss_assumptions_details[data$cov_ID %in% 44] 
data$miss_assumptions[data$cov_ID %in% 44] <- "Yes, authors invoked (either explicitly or implicitly) the missing at  random assumption"

data$miss_assumptions_details[data$cov_ID %in% 29] 
data$miss_assumptions[data$cov_ID %in% 29] <- "Yes, authors assumed (either explicitly or implicitly) that data were not MCAR"

data$miss_assumptions_details[data$cov_ID %in% 22] 
data$miss_assumptions[data$cov_ID %in% 22] <- "Yes, authors assumed data were MCAR"

data$miss_assumptions_details[data$cov_ID %in% 3] 
data$miss_assumptions[data$cov_ID %in% 3] <- "Yes, authors assumed (either explicitly or implicitly) that data were not MCAR"

data$miss_assumptions <- factor(data$miss_assumptions,
                                levels = c("No",
                                           "Yes, authors invoked (either explicitly or implicitly) the missing at  random assumption",
                                           "Yes, authors assumed (either explicitly or implicitly) that data were not MCAR",
                                           "Yes, authors assumed data were MCAR",
                                           "Yes, authors provided a comprehensive description of assumptions made about the missing data, for example, using a mDAG or a more simplified causal diagram",
                                           "Other"),
                                labels = c("No statement of missingness assumptions was provided",
                                           "Data were assumed to be MAR",
                                           "Data were assumed to be not MCAR",
                                           "Data were assumed to be MCAR",
                                           "Authors provided a comprehensive description of assumptions made about the missing data, for example, using a m-DAG",
                                           "Other"))
var_label(data$miss_assumptions) <- "Missing data assumptions"

# "Missing at random" statements
print(data[grep("MAR", data$miss_assumptions), c("study_ID", "cov_ID")], n = 36)

# study_ID             cov_ID       statement 
# 1 Yu 2021               337       EXPLICIT  "...we used multiple imputation with chained equations to impute missing data and set the number of imputations as 25, with the assumption that the data were missing at random"
# 2 Xuan 2019             334       EXPLICIT "To the best of our knowledge data were missing at random, which is the assumption of the multiple imputation."
# 3 White 2021            330       EXPLICIT "We used multiple imputation to maximize the plausibility of the missing at random assumption and restore sample representativeness."
# 4 Wang 2022             322       EXPLICIT "... multiple imputation with chained equations was applied to impute missing data under the assumption that data were missing at random."
# 5 vanGennip 2022        310       EXPLICIT "We imputed data using multiple imputation by chained equations under the assumption that data were missing at random."       
# 6 Shiba 2019            283       EXPLICIT "For missing information on covariates used in the study, we performed multiple imputation by chained equations, assuming missingness at random."
# 7 Schliep 2019          273       EXPLICIT "We compared complete case results (N = 1,135) to a multiple imputation analysis to address missing exposure and covariate data under the rationale that our data were missing at random (i.e., probability of missing data depends on observed data)."
# 8 Sato 2020             269       EXPLICIT "To address potential bias caused by missing values, we adopted multiple imputation under the missing-at-random assumption."
# 9 Salmon 2021           265       EXPLICIT "Assuming that missing data were missing at random, multiple imputations were performed using three imputed data sets."
# 10 Rudolph 2020         261       *IMPLICIT "We first imputed missing values using multiple imputation by chained equations, which assumes the data are missing at random conditional on the variables in the imputation model."
# 11 Robert 2019          254       EXPLICIT "We assumed that missing data were missing at random."
# 12 Richardson 2019      248       EXPLICIT (Supp) "We assumed the missing at random assumption held and is reasonable."
# 13 Ranzani 2020         242       EXPLICIT (Supp) "We investigated the missingness pattern and assumed a Missing at Random (MAR) mechanism."
# 14 Pongiglione 2020     236       EXPLICIT "Missingness was assumed to be random (MAR) ..."
# 15 Nøst 2021            217       EXPLICIT "Mean imputation was performed for variables with greater than 25% missing under a 'missing at random' assumption..."
# 16 Mollan 2021          202       EXPLICIT "To account for missing baseline covariate data, we employed a missing-at-random assumption, and MI was applied."
# 17 Mitha 2020           201       *IMPLICIT (Limitations) "However, multiple imputation analysis, despite the assumption of missing data at random, provided similar results."
# 18 Mårild 2019          189       *IMPLICIT (Discussion) "To reduce the risk of such bias, we applied multiple imputation; this approach provides valid estimates given missingness in itself is unrelated to why observations are missing (i.e., missing at random)."
# 19 Magnus 2021          185       *IMPLICIT "Missing information on covariates were imputed by chained equations. This multiple imputation approach assumes missing at random."
# 20 Hjorth 2021          133       EXPLICIT "Under the assumption that data were missing at random, missing data were imputed using multiple imputation by chained equations with 50 data sets created."
# 21 Harlow 2021          124       EXPLICIT "We assumed data to be missing at random conditional on measured covariates ...."
# 22 Hamad 2019           121       EXPLICIT (Supp) "We assumed that data were missing at random rather that [sic] missing completely at random."
# 23 Gialamas 2020        103       EXPLICIT "Multiple imputation using chained equations was conducted under the assumption that data were missing at random."
# 24 Garcia-Saenz 2020     99       EXPLICIT "... we performed multiple imputation of missing values of the potential confounders using chained equations assuming the missing at random (MAR) hypothesis."
# 25 Fraser 2020           95       *IMPLICIT "Multiple imputation, with appropriate standard errors, handled missing data (3-9% in particular dietary items), where possible using guided imputation to approximate the missing at random assumption."
# 26 Ferraro 2019          89       EXPLICIT "Assuming that data were missing at random, we dealt with missing values in early-life variables using multiple imputation by chained equations."
# 27 Cohen 2019            51       See below.
# 28 Chigogora 2020        47       EXPLICIT "In total, 1,467 children had missing outcome or covariate data; these data were imputed under a missing at random assumption for 1,464 children ..."
# 29 Chen 2020             46       EXPLICIT "Missing values on covariates were assumed to be missing at random and accounted for using multiple imputation ..."
# 30 Chen 2020             44       *IMPLICIT "Complete case analysis may have induced bias due to missing covariate information, but the multiple imputation analysis, despite the assumption of missing data at random, provided similar and reassuring results."
# 31 Chen 2019             43       EXPLICIT "Missing data mechanism was distinguished as missing at random (MAR) from missing completely at random (MCAR) through Little's test and t-test."
# 32 Chasekwa 2021         42       EXPLICIT "We employed multiple imputation by chained equations to account for any missing data, assuming a missing-at-random pattern (Stata MICE software)."
# 33 Borch 2019            34       EXPLICIT "Under the assumption that data was missing at random, we performed chained multiple imputation to deal with the missing information at baseline and follow-up."
# 34 Blouin 2019           31       EXPLICIT "... missing data were assumed to be random."
# 35 Barul 2019            16       EXPLICIT "Assuming that missing data on night-shift work and early-morning shifts (approximately 8% of jobs) were missing at random, and including occupational codes as predictors, we performed multiple imputation by chained equations using 15 data sets."
# 36 Allison 2020           4       EXPLICIT "Missing data appeared to be missing at random and were handled using multiple imputation."

# Note, the assumption of 27 Cohen 2019 is not clear even though they present additional details on missing data and describe the imputation procedure.
# Update variable.
data$miss_assumptions[data$study_ID %in% "Cohen 2019"] <- "Data were assumed to be not MCAR"

## miss_justified ----
data$miss_justified <- factor(data$miss_justified, levels = c("Yes", "No"))
var_label(data$miss_justified) <- "Missingness assumptions were justified"

# Correct error found in Borch 2019
data$miss_justified[data$study_ID %in% "Borch 2019"] <- "Yes"
data$miss_justified_details[data$study_ID %in% "Borch 2019"] <- "Authors state that the pattern of missing data was confirmed arbitrary.... the missing is arbitrary and equally distributed as the complete data over levels of PA (Table 2), by smoking status and BMI groups (Supplementary Tables 3 and 4)."

# Correct error found in Cohen 2019  
data$miss_justified[data$study_ID %in% "Cohen 2019"] <- "Yes"
data$miss_justified_details[data$study_ID %in% "Cohen 2019"] <- "Although missingness assumptions were not explicit, authors describe characteristics associated with missingness implying not MCAR"

which(data$miss_justified == "Yes")
data$miss_justified_details[which(data$miss_justified == "Yes")]

# Change missingness justification for one study that did not provide a statement 
# of missing data assumptions
flextable(data[data$miss_justified == "Yes" & !is.na(data$miss_justified_details),
               c("cov_ID", "study_ID", "miss_assumptions", "miss_justified", "miss_justified_details")])
data$miss_justified[data$cov_ID %in% 123] <- "No"
data$miss_justified_details[data$cov_ID %in% 123] <- NA

## mnar ----
data$mnar_details <- ifelse(grepl("Other", data$mnar), data$mnar, NA)
data$mnar <- ifelse(grepl("Other", data$mnar), "Other", data$mnar)
data$mnar <- factor(data$mnar,
                    levels = c("Yes, using external evidence such as expert knowledge",
                               "Yes, but only as a study limitation",
                               "No, the possibility that data were MNAR was not addressed",
                               "Commented on potential bias due to attrition",
                               "Described differences between analysis sample and inception sample caused by attrition",
                               "Other"))
var_label(data$mnar) <- "The possibility that data were MNAR was addressed"

# Classify "Other"
data[grep("Other", data$mnar), c("cov_ID", "study_ID", "mnar", "mnar_details")]

data$mnar_details[data$cov_ID %in% c(328, 310, 302, 297, 259, 224, 106)] 
data$mnar[data$cov_ID %in% c(328, 310, 302, 297, 259, 224, 106)] <- "Commented on potential bias due to attrition"

data$mnar_details[data$cov_ID %in% c(265, 256)]
data$mnar[data$cov_ID %in% c(265, 256)] <- "Described differences between analysis sample and inception sample caused by attrition"

data$mnar_details[data$cov_ID %in% c(255, 189)]
data$mnar[data$cov_ID %in% c(255, 189)] <- "Other"

# Check of MNAR statements:
flextable(
  data[which(data$mnar != "No, the possibility that data were MNAR was not addressed"), 
     c("cov_ID", "study_ID", "mnar", "mnar_details")],
  cwidth = c(1, 1, 4, 4))

## prim_ana_just ----
data$prim_ana_just_other <- ifelse(grepl("Other", data$prim_ana_just), data$prim_ana_just, NA)
data$prim_ana_just <- ifelse(grepl("Other", data$prim_ana_just), "Other", data$prim_ana_just)
data$prim_ana_just <- factor(data$prim_ana_just,
                             levels = c("Yes", "No", "Other"))
var_label(data$prim_ana_just) <- "Was the primary analysis method justified based on missingness assumptions?"

# Examine "Other"
# "The exclusion of incompletely reported cases from the analysis would probably 
# not affect the results in terms of bias. Nevertheless, this behavior can produce 
# a substantial loss of efficiency in the estimators (Greenland 1995). Thus, a 
# multiple imputation procedure was adopted to avoid disregarding some data and 
# to use all the available information."
data[grep("Other", data$prim_ana_just), c("cov_ID", "study_ID", "prim_ana_just")]

# Analysis methods ===========================================================

## mi_m_reported ----
data$mi_m_reported <- ifelse(grepl("^[[:digit:]]+", data$mi_m), 1, 0)
data$mi_m_reported <- factor(data$mi_m_reported,
                             levels = c(1, 0), labels = c("Yes", "No"))
var_label(data$mi_m_reported) <- "Reported the number of imputations"

## mi_m ----
data$mi_m <- ifelse(data$mi_m_reported == "Yes", data$mi_m, NA)
data$mi_m <- as.numeric(data$mi_m)
var_label(data$mi_m) <- "Number of imputations"

## mi_meth ----
data$mi_meth <- factor(data$mi_meth,
                       levels = c("MICE", "MVNI", "Unclear", "Other"))
var_label(data$mi_meth) <- "MI method"

# Check "Other"
data[grep("Other", data$mi_meth), c("cov_ID", "study_ID", "mi_meth")]

## mi_soft ----

# Classify "Other"
data[grepl("Other", data$mi_soft), c("cov_ID", "study_ID", "mi_soft")]

# Two software packages used
data$mi_soft[data$cov_ID %in% c(330, 310, 301, 265, 241, 179, 170, 141, 99, 82, 56, 45, 20)] <- "Two software packages used for analysis but not clear which package was used for MI"

# Unclear (not reported)
data$mi_soft[data$cov_ID %in% 52] <- "Unclear"

# SAS
data$mi_soft[data$cov_ID %in% 102] <- "SAS"

# Assume Stata (as used for mediation analysis)
data$mi_soft[data$cov_ID %in% c(255, 233)] <- "Stata"

# mi_soft
data$mi_soft_other <- ifelse(grepl("Other", data$mi_soft), data$mi_soft, NA)
data$mi_soft <- ifelse(grepl("Other", data$mi_soft), "Other", data$mi_soft)
data$mi_soft <- factor(data$mi_soft,
                       levels = c("Stata", "R", "SAS", "SPSS", 
                       "Two software packages used for analysis but not clear which package was used for MI", 
                       "Other", "Unclear"))
var_label(data$mi_soft) <- "MI Software"

## mi_ana ----
data$mi_ana <- factor(data$mi_ana,
                      levels = c("Yes", "No", "Unclear"))
var_label(data$mi_ana) <- "Imputation model included all variables in the analysis model"

## mi_aux ----
data$mi_aux <- factor(data$mi_aux,
                      levels = c("Yes", "No", "Unclear"))
var_label(data$mi_aux) <- "Imputation model included auxiliary variables"

## mi_int ----
data$mi_int <- factor(data$mi_int,
                      levels = c("Yes", "No", "Unclear"))
var_label(data$mi_int) <- "Imputation model included interactions"

## fcs_models ----
# When MICE was used, was the type of imputation model used stated?
print(data[, c("mi_meth", "fcs_models")], n = 130)
data$fcs_models <- ifelse(data$mi_meth != "MICE", NA, data$fcs_models)
data$fcs_models <- factor(data$fcs_models)
data$fcs_models <- forcats::fct_collapse(data$fcs_models,
                      Yes = c("stated",
                             "pmm", 
                             "stated (Appendix 1)",
                             "stated (code in Supplement)",
                             "stated (eAppendix 2)",
                             "stated (main text)",
                             "stated (see output in Supplement)",
                             "stated (see Web Apendix)",
                             "stated (Supplement)",
                             "stated (Supplementary Appendix)",
                             "stated: code provided in supp",
                             "stated: See supplement",
                             "stated: see Web Table 1"),
                      No = c("not stated",
                             "no stated",
                             "not stated (proc MI)"))
summary(data$fcs_models)

# Check studies where "MICE" was extracted, but check was "NA"
data[grepl("NA", data$fcs_models), c("cov_ID", "study_ID")]

# 185 Magnus 2021
data$fcs_models[data$cov_ID == 185] <- "No"

# 250 Riddell 2021
data$fcs_models[data$cov_ID == 250] <- "No"

# 96 Freedman 2022
# State that the "sequential imputation method described in Murad et al. (3)" 
# is used. Looking into the reference, this sequential imputation method can 
# be implemented using MI with FCS.
data$fcs_models[data$cov_ID == 96] <- "Yes"

var_label(data$fcs_models) <- "MICE imputation models stated"

## mi_pooled ----
# Was it stated how MI estimates (point estimates and standard errors) were obtained?
data$mi_pooled <- factor(data$mi_pooled)
data$mi_pooled <- forcats::fct_collapse(data$mi_pooled,
                      Yes = c("Bootstrap then impute, appear to use average bootstrap percentiles",
                              "Bootstrapping",
                              "Rubin's rules",
                              "Rubin's rules (code provided)",
                              "mi estimate",
                              "NORM used - combines results using Rusbin's rules",
                              "not stated (PROC MIANALYZE in SAS)",
                              "not stated (PROC MIANALYZE)",
                              "Rubin's rules (estimates obtained using \"mi estimate\" in Stata)",
                              "Rubin's rules (estimates obtained using \"mim\"",
                              "Rubin's rules (estimates obtained using \"pool\" function in R)",
                              "Rubin's rules (SAS-callable IVE-ware used)",
                              "Rubin's rules for MI estimates within each bootstrap iteration, percentile-based bootstrap CIs were used (not Rubin's variance estimator)",
                              "SASS 9.3 procedure \"PROC MIANALYZE\"",
                              "not stated (MIM)"),
                      No = c("not stated",
                             "not stated - reference provided",
                             "not stated (unclear from code)"))
summary(data$mi_pooled)
var_label(data$mi_pooled) <- "Pooling method stated"

# Sensitivity analyses =========================================================

## prim_ana ----

print(data[, c("primary_ana", "prim_ana")], n = 130)

data$primary_ana <- factor(data$primary_ana)
summary(data$primary_ana)

data$prim_weights <- factor(data$prim_weights)
data$prim_weights <- forcats::fct_collapse(data$prim_weights,
                      yes = c("yes: \"all analyses were weighted to be nationally representative\"",
                              "yes: appears that country-specific weights were used but little detail provided",
                              "yes: inverse probability of censoring weights",
                              "yes: inverse probability of censoring weights to address dependent censoring",
                              "yes: inverse probability of retention weights",
                              "yes: inverse probability of selection weight",
                              "yes: inverse probability weighting to deal with possible selection bias",
                              "yes: inverse probability weighting used to account for case-cohort design",
                              "yes: weights included a component that weighted for attrition",
                              "yes: weights to account for censoring",
                              "yes: weights used to account for attrition",
                              "yes: weights used to account for complex sampling design",
                              "yes: weights used to account for oversampling of cases (by design)"))
summary(data$prim_weights)

data$prim_ana <- factor(paste0(data$primary_ana, ", weight? ", data$prim_weights),
                        levels = c("MI, weight? no", 
                                   "MI, weight? yes", 
                                   "CCA, weight? no",
                                   "Other: combination of MI for one covariate with >25% missing values and single (median/mode) imputation for variables with less than 5% missing values, weight? no",
                                   "Other: include missing indicators for covariates, some single imputation of educational trajectories that were used to derive exposure, weight? yes",
                                   "Other: mean imputation for variables with > 25% missing data, weight? no",
                                   "Other: Median imputation, weight? no",
                                   "Other: missing-value indicators method, weight? no",
                                   "Other: missing data included as a recoded category for unknown, weight? no"),
                        labels = c("MI, unweighted", 
                                   "MI, weighted", 
                                   "CCA, unweighted",
                                   "Other: combination of MI for one covariate with >25% missing values and single (median/mode) imputation for variables with less than 5% missing values, weight? no",
                                   "Other: include missing indicators for covariates, some single imputation of educational trajectories that were used to derive exposure, weight? yes",
                                   "Other: mean imputation for variables with > 25% missing data, weight? no",
                                   "Other: Median imputation, weight? no",
                                   "Other: missing-value indicators method, weight? no",
                                   "Other: missing data included as a recoded category for unknown, weight? no"))
summary(data$prim_ana)
var_label(data$prim_ana) <- "Primary analysis"

## prim_ana ---- (OLD)
# data$prim_ana_details <- ifelse(grepl("Other", data$prim_ana), data$prim_ana, NA)
# 
# data$prim_ana <- factor(data$prim_ana,
#                         levels = c("MI using the full analysis sample",
#                                    "MI using a reduced analysis sample",
#                                    "MI used in primary analysis (previously other)",
#                                    "CCA, weighted (e.g. using IPW)",
#                                    "CCA, unweighted",
#                                    "Other"))
# 
# data$prim_ana <- forcats::fct_collapse(data$prim_ana,
#                                        MI = c("MI using the full analysis sample", 
#                                               "MI using a reduced analysis sample",
#                                               "MI used in primary analysis (previously other)"))
# data$prim_ana <- forcats::fct_collapse(data$prim_ana,
#                                        CCA = c("CCA, weighted (e.g. using IPW)", "CCA, unweighted"))
# var_label(data$prim_ana) <- "Primary method of analysis"
# 
# # Check and classify "Other"
# data[which(!is.na(data$prim_ana_details)), c("cov_ID", "study_ID", "prim_ana_details", "prim_ana")]
# 
# # Check and classify those with missing data
# data[which(is.na(data$prim_ana)), c("cov_ID", "study_ID", "prim_ana_details", "prim_ana")]
# 
# #319
# data$prim_ana_details[data$cov_ID %in% 319]
# data$prim_ana[data$cov_ID %in% 319] <- "Other"
# 
# #306
# data$prim_ana_details[data$cov_ID %in% 306]
# data$prim_ana[data$cov_ID %in% 306] <- "Other"
# 
# #302
# data$prim_ana_details[data$cov_ID %in% 302]
# data$prim_ana[data$cov_ID %in% 302] <- "MI"
# 
# #293
# data$prim_ana_details[data$cov_ID %in% 293]
# data$prim_ana[data$cov_ID %in% 293] <- "MI"
# 
# #283
# data$prim_ana_details[data$cov_ID %in% 283]
# data$prim_ana[data$cov_ID %in% 283] <- "MI"
# 
# #280 
# # MI was used for "calibration". Not a standard application of MI, but MI still used.
# data$prim_ana_details[data$cov_ID %in% 280]
# data$prim_ana[data$cov_ID %in% 280] <- "MI"
# 
# #277
# data$prim_ana_details[data$cov_ID %in% 277]
# data$prim_ana[data$cov_ID %in% 277] <- "MI"
# 
# #275
# data$prim_ana_details[data$cov_ID %in% 275]
# data$prim_ana[data$cov_ID %in% 275] <- "MI"
# 
# #261
# data$prim_ana_details[data$cov_ID %in% 261]
# data$prim_ana[data$cov_ID %in% 261] <- "MI"
# 
# #242
# data$prim_ana_details[data$cov_ID %in% 242]
# data$prim_ana[data$cov_ID %in% 242] <- "MI"
# 
# #217
# data$prim_ana_details[data$cov_ID %in% 217]
# data$prim_ana[data$cov_ID %in% 217] <- "Other"
# 
# #178
# data$prim_ana_details[data$cov_ID %in% 178]
# data$prim_ana[data$cov_ID %in% 178] <- "MI"
# 
# #145
# data$prim_ana_details[data$cov_ID %in% 145]
# data$prim_ana[data$cov_ID %in% 145] <- "MI"
# 
# #142
# data$prim_ana_details[data$cov_ID %in% 142]
# data$prim_ana[data$cov_ID %in% 142] <- "MI"
# 
# #140
# data$prim_ana_details[data$cov_ID %in% 140]
# data$prim_ana[data$cov_ID %in% 140] <- "Other"
# 
# #96
# data$prim_ana_details[data$cov_ID %in% 96]
# data$prim_ana[data$cov_ID %in% 96] <- "Other"
# 
# #89
# data$prim_ana_details[data$cov_ID %in% 89]
# data$prim_ana[data$cov_ID %in% 89] <- "MI"
# 
# #52
# data$prim_ana_details[data$cov_ID %in% 52]
# data$prim_ana[data$cov_ID %in% 52] <- "Other"
# 
# #19 # MI used to obtain individual-level exposure values. 
# # Not a standard application,  but MI used and analysis conducted at the individual level.
# data$prim_ana_details[data$cov_ID %in% 19]
# data$prim_ana[data$cov_ID %in% 19] <- "MI"

## second_cond ----
data$second_cond <- factor(ifelse(grepl("Yes", data$second_ana.x), "Yes",
                                  ifelse(grepl("No", data$second_ana.x), "No", NA)),
                           levels = c("Yes", "No"))
var_label(data$second_cond) <- "Secondary analysis conducted that handled the missing data differently"

# Classify unknown
data[which(is.na(data$second_cond)), c("cov_ID", "study_ID")]

# 342 Zhou 2019
# "Data missing at a moderate rate (<= 10.2%) were replaced via multiple imputation using NORM
# software, originally implemented by Schafer and Olsen. Maximum likelihood estimates of parameters 
# (mean values and covariances) were efficiently computed utilizing the expectation-maximization 
# alogirthm in NORM. A data augmentation procedure was applied to alternately fill in the missing 
# data. 
# It is not really clear what "alternately" means here. Assume that this is referring 
# to the iterative process used within the data augmentation algorithm and not a sensitivity 
# analysis as there were no other comments on missing data sensitivity analysis in the paper.
data$second_cond[data$cov_ID %in% 342]  <- "No"

# Check
print(data[grepl("No", data$second_cond), c("cov_ID", "study_ID", "second_cond", "second_ana.y")], n = 130)
print(data[grepl("NA", data$second_ana.y), c("cov_ID", "study_ID", "second_cond", "second_ana.y")], n = 130)

# 171 Lergenmuller 2022
# Several complete-case sensitivity analyses were conducted, but these could 
# be considered different analysis, as other analytical decisions were changed
# (different subset, direction of pathway reversed, different model)
# Note this in results.
data[grep(171, data$cov_ID), "second_cond"] <- "Yes"
data[grep(171, data$cov_ID), "second_ana.y"] <- "Other: several complete-case sensitivity analyses were conducted but these could be considered different analysis, as other analytical decisions were also changed"

# 320 Walsemann 2020
# Sensitivity analysis conducted. Analyses were weighted and unweighted MI analyses
data[grep(320, data$cov_ID), "second_cond"] <- "Yes"

## second_ana ----

# Secondary analysis method
data$second_ana <- factor(data$second_ana.y)
summary(data$second_ana)

# Additional secondary analyses?
data$add_sens_ana_details <- factor(data$add_sens_ana_details)
summary(data$add_sens_ana_details)

# Simplify weighted analyses into one category
data$second_weights <- factor(data$second_weights)
data$second_weights <- forcats::fct_collapse(data$second_weights,
                                             yes = c("yes: \"all analyses were weighted to be nationally representative\"",
                                                     "yes: appears that country-specific weights were used but little detail provided",
                                                     "yes: inverse probability of censoring weights",
                                                     "yes: inverse probability weighting to account for male participation",
                                                     "yes: weights included a component that weighted for attrition",
                                                     "yes: weights used to account for oversampling of cases (by design)"))
summary(data$second_weights)
print(data[, c("cov_ID", "study_ID", "second_ana", "second_weights", "add_sens_ana_details")], n = 130)

# Table 4 footnotes:
flextable(data[grepl("NA", data$add_sens_ana_details) == FALSE, 
               c("cov_ID", "study_ID", "prim_ana", "second_ana", "second_weights", "add_sens_ana_details")],
          cwidth = c(1, 4, 4, 4, 8))

# Create a new secondary analysis variable that contains information on weights
# and additional analyses
data$second_ana <- paste0(data$second_ana, ", weight? ", data$second_weights)
data$second_ana <- ifelse(grepl("NA", data$add_sens_ana_details), data$second_ana, "multiple_second_ana")

# Listwise deletion with weights is equivalent to CCA, weighted.
data$second_ana[grep("listwise deletion", data$second_ana)] <- "CCA, weight? yes"

# Label factor levels
data$second_ana <- factor(data$second_ana,
                          levels = c("MI, weight? no", 
                                     "MI, weight? yes", 
                                     "CCA, weight? no", 
                                     "CCA, weight? yes",
                                     "multiple_second_ana",
                                     "MI without imputing exposure and outcome, weight? yes",
                                     "MI, with model incorporating outcome variables, weight? no"),
                          labels = c("MI, unweighted", 
                                     "MI, weighted", 
                                     "CCA, unweighted", 
                                     "CCA, weighted",
                                     "Multiple secondary analyses conducted",
                                     "MI without imputing exposure and outcome, weight? yes",
                                     "MI, with model incorporating outcome variables, weight? no"))

summary(data$second_ana)
var_label(data$second_ana) <- "Secondary method of analysis"

## second_ana ---- OLD

# data$second_ana_details <- ifelse(grepl("Other", data$second_ana), data$second_ana, NA)
# data$second_ana_fct <- ifelse(grepl("Other", data$second_ana), "Other", data$second_ana)
# data$second_ana_fct <- factor(data$second_ana_fct,
#                               levels = c("Yes, MI using the full analysis sample",
#                                          "Yes, MI using a reduced analysis sample",
#                                          "Yes, weighted CCA (e.g. using IPW)",
#                                          "Yes, unweighted CCA",
#                                          "Yes, delta-adjusted MI",
#                                          "No",
#                                          "Other"),
#                               labels = c("MI using the full analysis sample",
#                                          "MI using the reduced analysis sample",
#                                          "Weighted CCA (e.g. using IPW)",
#                                          "Unweighted CCA",
#                                          "Delta-adjusted MI",
#                                          "No secondary analysis",
#                                          "Other"))
# 
# data$second_ana_fct <- forcats::fct_collapse(data$second_ana_fct,
#                                              MI = c("MI using the full analysis sample", 
#                                                     "MI using the reduced analysis sample"))
# data$second_ana_fct <- forcats::fct_collapse(data$second_ana_fct,
#                                              CCA = c("Weighted CCA (e.g. using IPW)", 
#                                                      "Unweighted CCA"))
# var_label(data$second_ana_fct) <- "Secondary method of analysis"
# 
# # Classify other if applicable
# flextable(data[grepl("Other", data$second_ana), c("cov_ID", "study_ID", "second_ana")],
#           cwidth = c(1, 2, 10))
# 
# # A version of CCA
# data$second_ana_fct[data$cov_ID %in% c(283, 365, 108, 52)] <- "CCA"
# 
# # Classify NA if applicable
# data[is.na(data$second_ana_fct), c("cov_ID", "study_ID", "prim_ana", "second_ana", "second_ana_fct")]
# data$second_ana_fct[data$cov_ID %in% 132] <- "Other"

## second_ana_just ----
data$second_ana_just_details <- ifelse(grepl("Other", data$second_ana_just), data$second_ana_just, NA)
data$second_ana_just <- ifelse(grepl("Other", data$second_ana_just), "Other", data$second_ana_just)
data$second_ana_just <- factor(data$second_ana_just,
                               levels = c("No",
                                          "Yes, as a sensitivity analysis (without further justification)",
                                          "Yes, as a sensitivity analysis to examine the influence of missing data",
                                          "Yes, as a sensitivity analysis to examine the impact of the missing data method",
                                          "Yes, as a sensitivity analysis to address possible selection bias",
                                          "Yes, as a sensitivity analysis to parametric modelling assumptions",
                                          "Yes, as a sensitivity analysis to causal assumptions made about the missing data mechanism",
                                          "NA",
                                          "Other"))
var_label(data$second_ana_just) <- "Was the secondary analysis method justified based?"

# Classify "Other"
flextable(data[grep("Other", data$second_ana_just_details), 
           c("cov_ID", "study_ID", "second_cond", "second_ana", "second_ana_just", "second_ana_just_details")])

#337 
data$second_ana_just[data$cov_ID %in% 337] <- "Yes, as a sensitivity analysis to examine the impact of the missing data method" 

#328
data$second_ana_just[data$cov_ID %in% 328] <- "Yes, as a sensitivity analysis to examine the impact of the missing data method" 

#322 
data$second_ana_just[data$cov_ID %in% 322] <- "Yes, as a sensitivity analysis to examine the impact of the missing data method" 

#319 - "Yes, to examine the robustness of findings to statistical assumptions"
# Leave as other - not clear which statistical assumptions are being referred to.

#310
data$second_ana_just[data$cov_ID %in% 310] <- "Yes, as a sensitivity analysis (without further justification)"

#304
data$second_ana_just[data$cov_ID %in% 304] <- "Yes, as a sensitivity analysis to examine the impact of the missing data method"

#299
data$second_ana_just[data$cov_ID %in% 299] <- "Yes, as a sensitivity analysis (without further justification)"

#273
data$second_ana_just[data$cov_ID %in% 273] <- "Yes, as a sensitivity analysis to address possible selection bias"

#259
data$second_ana_just[data$cov_ID %in% 259] <- "Yes, as a sensitivity analysis to address possible selection bias"

#244
data$second_ana_just[data$cov_ID %in% 244] <- "Yes, as a sensitivity analysis to address possible selection bias"

#242 
data$second_ana_just[data$cov_ID %in% 242] <- "Yes, as a sensitivity analysis (without further justification)"

#225
data$second_ana_just[data$cov_ID %in% 225] <- "Yes, as a sensitivity analysis (without further justification)"

#217 
data$second_ana_just[data$cov_ID %in% 217] <- "Yes, as a sensitivity analysis to parametric modelling assumptions"

#203
data$second_ana_just[data$cov_ID %in% 203] <- "Yes, as a sensitivity analysis to examine the impact of the missing data method"

#201
data$second_ana_just[data$cov_ID %in% 201] <- "Yes, as a sensitivity analysis to address possible selection bias"

#133
data$second_ana_just[data$cov_ID %in% 133] <- "Yes, as a sensitivity analysis to examine the impact of the missing data method"

#132
data$second_ana_just[data$cov_ID %in% 132] <- "Yes, as a sensitivity analysis to address possible selection bias"

#124
data$second_ana_just[data$cov_ID %in% 124] <- "Yes, as a sensitivity analysis to address possible selection bias"

#105
data$second_ana_just[data$cov_ID %in% 105] <- "Yes, as a sensitivity analysis (without further justification)"

#96 
# "In the context of IPW of MSMS, no clear recommendations for dealing with 
# missing data have yet emerged. In the absence of a theoretical justification
# for a particular method, we investigated 3 approaches ..."
# "All methods for dealing with missing data in the unweighted analysis gave 
# similar estimates ..."
# Assume sensitivity analysis conducted to examine the impact of the missing 
# data method.
data$second_ana_just[data$cov_ID %in% 96] <- "Yes, as a sensitivity analysis to examine the impact of the missing data method"

#65 
data$second_ana_just[data$cov_ID %in% 65] <- "Yes, as a sensitivity analysis (without further justification)"

#44
data$second_ana_just[data$cov_ID %in% 44] <- "Yes, as a sensitivity analysis (without further justification)"

#43 - "Yes, to reduce potential bias caused by including only participants with 
#      complete information and exploit information in incomplete record participants."
# Leave as other - both to address possible selection bias AND to exploit information in incomplete record participants

#41
data$second_ana_just[data$cov_ID %in% 41] <- "Yes, as a sensitivity analysis to examine the influence of missing data"

#31 - Secondary analysis not conducted
data$second_ana_just[data$cov_ID %in% 31] <- NA
data$second_ana_just_details[data$cov_ID %in% 31] <- NA 

#22 - Secondary analysis not conducted 
data$second_ana_just[data$cov_ID %in% 22] <- NA
data$second_ana_just_details[data$cov_ID %in% 22] <- NA 

#20
data$second_ana_just[data$cov_ID %in% 20] <- "Yes, as a sensitivity analysis (without further justification)"

# Second_cond == "Yes" & second_ana_just == NA
data$cov_ID[which(data$second_cond == "Yes" & is.na(data$second_ana_just))]

# 171
data[data$cov_ID %in% 171, c("second_cond", "second_ana_just", "second_ana_just_details")]
data$second_ana_just[data$cov_ID %in% 171] <- "Other"
data$second_ana_just_details[data$cov_ID %in% 171] <- "Other: No justification provide for 
comparing CCA and MI, but fitting different models justified by seeing whether the choice of 
model impacted results and fitting models with and without weights were justifed to assess
how weighting affected the results"

# 320
# "We used person-level weights from the respondents' last core interview to 
# account for the complex sampling design. Unweighted analyses yielded similar
# results.
data[data$cov_ID %in% 320, c("second_cond", "second_ana_just", "second_ana_just_details")]
data$second_ana_just[data$cov_ID %in% 320] <- "No"
data$second_ana_just_details[data$cov_ID %in% 320] <- "No"

## delta_adj_ext ----
data$delta_adj_ext <- factor(data$delta_adj_ext,
                             levels = c("Yes", "No or not stated", "NA"))
var_label(data$delta_adj_ext) <- "Was an analysis that incorporated external information used?"

## mi_cca_diff ----

data$mi_cca_diff <- ifelse(is.na(data$mi_cca_diff), "Study did not conduct both MI and CCA", data$mi_cca_diff)
data$mi_cca_diff <- factor(data$mi_cca_diff,
                           levels = c("Yes", "No", "Study did not conduct both MI and CCA"))
var_label(data$mi_cca_diff) <- "Was a substantial difference between MI and CCA estimates observed?"

## mi_and_cca ----
data$mi_and_cca <- ifelse(data$mi_cca_diff == "Yes" | data$mi_cca_diff == "No", "Yes", "No")
data$mi_and_cca <- factor(data$mi_and_cca, levels = c("No", "Yes"))
var_label(data$mi_and_cca) <- "Conducted both a CCA and an MI analysis"
table(data$mi_cca_diff, data$mi_and_cca)

# Check when second_ana is not NA and mi_and_cca == No
flextable(data[which(!is.na(data$second_ana) & grepl("No", data$mi_and_cca)), 
               c("cov_ID", "prim_ana", "second_ana", "second_cond", "mi_and_cca", "mi_cca_diff")], 
          cwidth = 8)

# 171: Secondary analyses included Standard MI, CCA, CCA combined with weighting
# "None of the sensitivity analyses ... produced meaningful differences"
data[grep(171, data$cov_ID), "mi_and_cca"] <- "Yes"
data[grep(171, data$cov_ID), "mi_cca_diff"] <- "No"

# 189
# A similar pattern of results was found in .... Supplementary Table 5" 
# Supplementary Table 5 reports estimates obtained from complete cases.
data[grep(189, data$cov_ID), "mi_and_cca"] <- "Yes"
data[grep(189, data$cov_ID), "mi_cca_diff"] <- "No"

# 202
# "When transported to our target population, the estimated hazard ratio was 
# somewhat attenuated in borth the MI analysis (HR = 1.8, 95% CI: 0.9, 4.4) and 
# the complete-case analysis (HR = 1.9, 95% CI: 1.0, 4.8), as compared with the 
# trials-only analysis." No mention of a substantial difference between CI and MI.
data[grep(202, data$cov_ID), "mi_and_cca"] <- "Yes"
data[grep(202, data$cov_ID), "mi_cca_diff"] <- "No"

# 51
# CCA reported in Web Table 2 but not mentioned in the main text.
data[grep(51, data$cov_ID), "mi_and_cca"] <- "Yes"
data[grep(51, data$cov_ID), "mi_cca_diff"] <- "No"

# 52 
# "Sensitivity analyses conducted with respondents who had complete data on 
# all metabolic syndrome indicators (n = 599) produced similar findings."
data[grep(52, data$cov_ID), "mi_and_cca"] <- "Yes"
data[grep(52, data$cov_ID), "mi_cca_diff"] <- "No"

# 8 sensitivity analyses were conducted that did not compare MI and CCA estimates:
# Standard MI, weighted and unweighted: 108, 142, 236, 320, 
# Standard MI, weighted, with and without imputation of exposure: 225
# Compared estimates from single imputation (3 versions) and standard MI: 217
# Standard MI, with and without imputation model including the outcome variable: 63
# Compared estimates from treating missing as an additional category, single imputation using last value carried forward and standard MI: 96


# Reorder variables ============================================================

col_order <- c("cov_ID", "study_ID", "title", "reviewer", "first_author",
               "pub_date", "pub_year", "journal", "incl_crit", "incl_crit_1", "incl_crit_2",
               "caus_signal", "caus_signal_1", "caus_signal_2", "caus_signal_3",
               "caus_interpret", "design", "design_details", "design_fct", "outcome_type",
               "inception_avail", "inception_avail_fct", "inception_avail_details",
               "inception_n", "analysis_reduc", "analysis_reduc_fct", "analysis_n",
               "complete_estab", "complete_p", "complete_p_estab",
               "complete_p_ub", "exposure", "exposure_miss",
               "exposure_miss_p", "exposure_miss_p_estab", "exposure_miss_p_lb",
               "outcome.y", "outcome_miss", "outcome_miss_p", 
               "outcome_miss_p_estab", "outcome_miss_p_lb", "cov_miss",
               "multi_miss_ind", "miss_assumptions", "miss_justified",
               "miss_justified_details", "mnar", "mnar_details", "prim_ana", 
               "prim_ana_just", "prim_ana_just_detail", "prim_ana_just_other", "second_cond", 
               "second_ana",
               "second_ana_just", "delta_adj_ext", "delta_adj_detail", "mi_meth", "mi_soft",
               "mi_m", "mi_m_reported", "mi_ana", "mi_aux", "mi_int", "fcs_models", 
               "mi_pooled", "mi_and_cca", "mi_cca_diff", "mi_cca_diff_details", "other", "notes")
data <- data[, col_order]
rm(col_order)

# Save clean data for analysis =================================================
save(data, file = "data_clean.R")
write_xlsx(data, path = "data_clean.xlsx")
rm("data")

# Project:  pcr_discrete_evs
# Author:   Edoardo Costantini
# Created:  2022-04-06
# Modified: 2022-04-20

# Environment ------------------------------------------------------------------

  # Make sure we have a clean environment:
  rm(list = ls())

  # Packages
  library(foreign)  # to import .dta data
  library(labelled) # to extract variable labels
  library(mice)
  library(dplyr)
  library(forcats) # for fct_collapse() function to recode factors

  # Source
  source("./helper/dropVars.R")

# Read Data --------------------------------------------------------------------

  # Define filename
  fileName <- "ZA7500_v4-0-0.dta"

  # Read Integrated data
  EVS2017 <- as.data.frame(haven::read_dta(paste0("../input/", fileName)))

  # Find all haven_labelled
  index_hl <- which(sapply(EVS2017, function (x) class(x)[1]) == "haven_labelled")

  # Transform them to regular factors
  for (v in index_hl){
    EVS2017[, v] <- haven::as_factor(EVS2017[, v])
  }

  # Find all characteer variables
  index_ch <- which(sapply(EVS2017, function (x) class(x)[1]) == "character")

  # Transform them to regular factors
  for (v in index_ch){
    EVS2017[, v] <- haven::as_factor(EVS2017[, v])
  }

# Step 1: Subset cases ---------------------------------------------------------

  # Matrix design: Keep only full cases
  EVS2017 <- EVS2017 %>%
    filter(fmissings == "full/complete case")

# Step 2: Cleaning columns -----------------------------------------------------

  # > Mode variables (mixed mode and matrix desing) (table 9 codebook) ---------
  v_modes <- c("mm_select_sample",
               "mm_mode_fu",
               "mm_matrix_group",
               "mm_mixed_mode",
               "mm_original_matrix_design_IS",
               "mm_original_matrix_group_IS",
               "mm_fw_start_fu",
               "mm_fw_end_fu",
               "mm_year_fu",
               "mr_detailed_mode_DE",
               "mr_contact_mode_DE",
               "mr_incentive_DE",
               "fduplicate",
               "fmissings",
               "mm_v277_fu",
               "mm_v278a_fu",
               "mm_v278b_fu",
               "mm_v279a_fu",
               "mm_v279b_fu")

  # Drop
  EVS2017 <- dropVars(EVS2017, v_modes)

  # > Administrative variables -------------------------------------------------
  v_admin <- c("studyno",
               "studynoc",
               "version",
               "versionc",
               "doi",
               "id_cocas",
               "caseno",
               "year",
               "fw_start",
               "fw_end",
               # "country", # keep
               "cntry_y",
               "c_abrv",
               # "mode", # keep
               "fmissings",
               # "v281a", # keep
               "v281a_r",
               "v282")

  # Drop
  EVS2017 <- dropVars(EVS2017, v_admin)

  # > Variables with country codes ---------------------------------------------

  v_cntco <- c("v52_cs",  # harmonized in v52
               "v174_cs", # harmonized in v174_LR
               "v175_cs"  # harmonized in v175_LR
               )
    # Note: these variables are stored in an harmonized version
    #       we do not need the messy original variables for prediction

  # Drop
  EVS2017 <- dropVars(EVS2017, v_cntco)

  # > Differently coded variables to drop --------------------------------------

  # Comment out the one(s) to keep
  v_codes <- c(
    # educational level respondent
    "v243_cs", # non-harmonized
    "v243_edulvlb",
    "v243_edulvlb_1",
    "v243_edulvlb_2",
    # "v243_ISCED_1", # keep
    "v243_ISCED_2",
    "v243_ISCED_2b",
    "v243_ISCED_3",
    "v243_EISCED",
    "v243_r",

    # educational level spouse / partner
    "v252_cs", # non-harmonized
    "v252_edulvlb",
    "v252_edulvlb_1",
    "v252_edulvlb_2",
    # "v252_ISCED_1", # keep
    "v252_ISCED_2",
    "v252_ISCED_2b",
    "v252_ISCED_3",
    "v252_EISCED",
    "v252_r",

    # education level father
    "v262_cs", # non-harmonized
    "v262_edulvlb",
    "v262_edulvlb_1",
    "v262_edulvlb_2",
    # "v262_ISCED_1", # keep
    "v262_ISCED_2",
    "v262_ISCED_2b",
    "v262_ISCED_3",
    "v262_EISCED",
    "v262_r",

    # educational level mother
    "v263_cs", # non-harmonized
    "v263_edulvlb",
    "v263_edulvlb_1",
    "v263_edulvlb_2",
    # "v263_ISCED_1", # keep
    "v263_ISCED_2",
    "v263_ISCED_2b",
    "v263_ISCED_3",
    "v263_EISCED",
    "v263_r",

    # Job / profession respondent
    "v246_ISCO_2",
    "v246_SIOPS",
    "v246_ISEI",
    # "v246_egp", # keep
    "v246_ESeC",

    # Job / profession spouse / partner
    "v255_ISCO_2",
    "v255_SIOPS",
    "v255_ISEI",
    # "v255_egp", # keep
    "v255_ESeC",

    # Income
    # "v261", # keep
    "v261_ppp",
    "v261_r",

    # Region of interview
    "v275b_N2",
    # "v275b_N1", # keep
    "v275c_N2",
    "v275c_N1",

    # Country of birth
    "v228b",
    # "v228b_r", # keep
    "v231b",
    # "v231b_r", # keep
    "v233b",
    # "v233b_r", # keep
    "v251b",
    # "v251b_r", # keep

    # Language
    # "v281a", # keep
    "v281a_r"
  )

  # Drop all v_codes
  EVS2017 <- dropVars(EVS2017, v_codes)

# > Weights --------------------------------------------------------------------

  v_weights <- c(
    "gweight",
    "gweight_no_edu",
    "dweight",
    "pweight",
    "age_r_weight",
    "v225_weight",
    "v243_r_weight"
  )

  # Drop
  EVS2017 <- dropVars(EVS2017, v_weights)

# > Country specific variables: Germany ----------------------------------------

  # CAWI and Mail matrix-design survey the genuine scale was falsely changed to include
  # an additional middle-category
  v_cs_DE <- c(
    "v72_DE",
    "v73_DE",
    "v74_DE",
    "v75_DE",
    "v76_DE",
    "v77_DE",
    "v78_DE",
    "v79_DE"
  )

  # Drop
  EVS2017 <- dropVars(EVS2017, v_cs_DE) # Drop

# > Country specific variables: Sweden -----------------------------------------

  # Different question wording (can drop)
  v_cs_SE <- c(
    "f112_SE"
  )

  # Drop
  EVS2017 <- dropVars(EVS2017, v_cs_SE) # Drop

# > Country specific variables: Denmark, Finland, Netherlands, Iceland ---------

  # Answer category “0 It is against democracy” not in CAPI is in CAWI
  v_cs_dfni <- c(
    "v133_11c",
    "v134_11c",
    "v135_11c",
    "v136_11c",
    "v137_11c",
    "v138_11c",
    "v139_11c",
    "v140_11c",
    "v141_11c"
  )

  # Cast answers to single question
  for(v in v_cs_dfni){
    # Define baseline variable name
    baseVar <- gsub("\\_(.*)", "", v)

    # Correct level label of "it is against democracy" in baseline
    levels(EVS2017[, baseVar])[9] <- "it is against democracy"

    # Identify the values that should be kept from the source variables
    index <- EVS2017[, v] != "item not included"

    # Replace the values in the base variable with the ones from source variables
    EVS2017[, baseVar][index] <- EVS2017[, v][index]
  }

# > Country specific variables: Denmark ----------------------------------------

  v_cs_DK <- c(
    # Answer category 4 "Not at all often" is missing in field questionnaire
    "v176_DK",
    "v177_DK",
    "v178_DK",
    "v179_DK",
    "v180_DK",
    "v181_DK",
    "v182_DK",
    "v183_DK",

    # Answer category “not at all important” is missing in CAWI
    "v221_DK",
    "v222_DK",
    "v223_DK",
    "v224_DK"
  )

  # Cast answers to single question
  for(v in v_cs_DK){
    # Define baseline variable name
    baseVar <- gsub("\\_(.*)", "", v)

    # Identify the values that should be kept from the source variables
    index <- EVS2017[, v] != "item not included"

    # Replace the values in the base variable with the ones from source variables
    EVS2017[, baseVar][index] <- EVS2017[, v][index]
  }

  # Drop
  EVS2017 <- dropVars(EVS2017, c(v_cs_DK))

# > Country specific variables: Switzerland ------------------------------------

  # Non-standard question wording
  v_cs_CH <- "f252_edulvlb_CH"

  # Drop
  EVS2017 <- dropVars(EVS2017, v_cs_CH)

# > Country specific variables: Italy ------------------------------------------
# Survey Experiment: EVS 2017 Italy

  # Variation of wording flag
  v_cs_IT <- c(
    "f46_IT",
    "f199_IT"
  )

  # Complex wording change
  v_cs_IT_complex <- c(
    "f24_IT",  # flag version
    "v24a_IT", # version 2
    "v24b_IT"  # version 3
  )

  # Cast answers to single question
  for(v in c("v24a_IT", "v24b_IT")){
    # Identify the values that should be kept from the source variables
    index <- EVS2017[, v] != "item not included"

    # Replace the values in the base variable with the ones from source variables
    EVS2017[, "v24"][index] <- EVS2017[, v][index]
  }

  # Drop
  EVS2017 <- dropVars(EVS2017, c(v_cs_IT, v_cs_IT_complex))

# Step 3: Question by question scan --------------------------------------------

# > Q4: mind having any of these as neighbours ---------------------------------

# v20
  # Check that even if v20a == mentioned, answers to 9 to 19 are don't know
  check_Q4_v20 <- EVS2017 %>%
    filter(v20 == "mentioned") %>%
    select(paste0("v", 9:19))

  # Are there rows for which the condition is not met?
  index <- which(rowSums(check_Q4_v20 != "not mentioned") != 0)
  index

  # How do these obs look?
  EVS2017 %>%
    filter(v20 == "mentioned") %>%
    select(paste0("v", 9:19)) %>%
    slice(index)

  # Drop f20 and v20 as all the info is in the other variables already
  EVS2017 <- dropVars(EVS2017, c("v20", "f20"))

# v20a
  # Check that even if v20a == mentioned, answers to 9 to 19 are don't know
  check_Q4_v20a <- EVS2017 %>%
    filter(v20a == "mentioned") %>%
    select(paste0("v", 9:19))

  # Are there rows for which the condition is not met?
  index <- which(rowSums(check_Q4_v20a != "dont know") != 0)
  index

  # I will drop v20a because information is already recorded in v9 to v19
  EVS2017 <- dropVars(EVS2017, "v20a")

# v20b
  # Check that even if v20b == mentioned, answers to 9 to 19 are "no answer"
  check_Q4_v20b <- EVS2017 %>%
    filter(v20b == "mentioned") %>%
    select(paste0("v", 9:19))

  # Are there rows for which the condition is not met?
  index <- which(rowSums(check_Q4_v20b != "no answer") != 0)
  index

  # How does this obs look?
  EVS2017 %>%
    filter(v20b == "mentioned") %>%
    select(paste0("v", 9:19)) %>%
    slice(index)

  # I will drop v20b because information is already recorded in v9 to v19
  EVS2017 <- dropVars(EVS2017, "v20b")

# > Q6: mind having any of these as neighbours ---------------------------------

# v27 to v30
  # These are optional questions that craete a "item not included" type of
  # missing which is not relevant for this study.
  optional_Q6 <- paste0("v", 27:30)
  EVS2017 <- dropVars(EVS2017, optional_Q6)

# v30a
  # Check that even if v30a == mentioned, answers to 22 to 26 are not-mentioned
  check_Q6_v30a <- EVS2017 %>%
    filter(v30a == "mentioned") %>%
    select(v22, v23, v24, v25, v26)

  # Are there rows for which the condition is not met?
  which(rowSums(check_Q6_v30a != "not mentioned") != 0)

  # What row is it?
  t(check_Q6_v30a[rowSums(check_Q6_v30a != "not mentioned") != 0, ])

  # I will drop v30a because information is already recorded in v22 to v26
  # excpet for this specific row
  EVS2017 <- dropVars(EVS2017, "v30a")

# f30a
  # What is f30a?
  unique(EVS2017$f30a)

  # Does it happen in the data?
  t(EVS2017 %>%
      filter(f30a == "inconsistent: respondent mentioned at least one item and none of these") %>%
      select(v22, v23, v24, v25, v26))

  # This was only pointing to the inconsiste case you found for v30a
  # No need to keep this information in
  EVS2017 <- dropVars(EVS2017, "f30a")

# v30b
  # Check that even if v30b == mentioned, answers to 22 to 26 are don't know
  check_Q6_v30b <- EVS2017 %>%
    filter(v30b == "mentioned") %>%
    select(v22, v23, v24, v25, v26)

  # Are there rows for which the condition is not met?
  index <- which(rowSums(check_Q6_v30b != "dont know") != 0)
  index

  # What row is it?
  t(check_Q6_v30b[index, ])

  # I will drop v30b because information is already recorded in v22 to v26
  EVS2017 <- dropVars(EVS2017, "v30b")

# v30c
  # Check that even if v30c == mentioned, answers to 22 to 26 are NAs
  check_Q6_v30c <- EVS2017 %>%
    filter(v30c == "mentioned") %>%
    select(v22, v23, v24, v25, v26)

  # Are there rows for which the condition is not met?
  index <- which(rowSums(check_Q6_v30c != "no answer") != 0)
  index

  # I will drop v30c because information is already recorded in v22 to v26
  EVS2017 <- dropVars(EVS2017, "v30c")

# > Q13: religious belief ------------------------------------------------------

  # First there is gated question
  table(EVS2017$v51)

  # You can check if people delcared a religion after this
  table(religion = EVS2017$v52, gate = EVS2017$v51)

  # Only a few people v51 == no and then v52 != "not applicable"
  # As a result, I will add a no religion category to v52 and drop v51
  EVS2017$v52_r <- as.character(EVS2017$v52)
  EVS2017$v52_r[EVS2017$v51 == "no" & EVS2017$v52 == "not applicable"] <- "no religion"

  # The remaining "not applicable" on v52 are carried over missing values from v51
  EVS2017$v52_r[EVS2017$v52 != "Muslim" &
                  EVS2017$v51 == "no answer" |
                  EVS2017$v51 == "dont know"] <- NA

  # Transform to a factor
  EVS2017$v52_r <- factor(EVS2017$v52_r)

  # Drop
  EVS2017 <- dropVars(EVS2017, c("v51", "v52"))

  # v53 is about having been religious
  table(religion = EVS2017$v52, gate = EVS2017$v53)

  # more than 1000 respondends have answers for v52 and v53, which is inconsistent
  # v53 is not a variable we want to keep in
  EVS2017 <- dropVars(EVS2017, "v53")

# > Q28: qualities children encouraged -----------------------------------------

# f85
  # Check frequencies
  table(EVS2017$f85)

  # Just drop those 2000 obs that have particular nationality status
  check_f85 <- EVS2017 %>%
    filter(f85 == 'inconsistent') %>%
    select(paste0("v", 85:95))

  # Drop country based variables (and keep standardized)
  rowSums(check_f85 == "mentioned")

  # I will keep this variable in thinking that people simply didn't read the
  # up to 5 and we have more information on what they think is important
  # Then, I will drop f85
  EVS2017 <- dropVars(EVS2017, "f85")

# v96
  # Check: v96 == mentioned; v85 to v95 == not mentioned
  check_Q28_v96 <- EVS2017 %>%
    filter(v96 == "mentioned") %>%
    select(v85, v86, v87, v88, v89, v90, v91, v92, v93, v94, v95)

  # Are there rows for which the condition is not met?
  which(rowSums(check_Q28_v96 != "not mentioned") != 0)

  # What row is it?
  t(check_Q28_v96[rowSums(check_Q28_v96 != "not mentioned") != 0, ])

  # I will drop v96 because information is already recorded in v85 to v95
  # v96 is at most inconsistent with these
  EVS2017 <- dropVars(EVS2017, "v96")

# f96
  # What is f96?
  unique(EVS2017$f96)

  # This was only pointing to the inconsistent cases found for v96
  # No need to keep this information in
  EVS2017 <- dropVars(EVS2017, "f96")

# v96a
  # Check: v96a == mentioned; v85 to v95 == dont know
  check_Q28_v96a <- EVS2017 %>%
    filter(v96a == "mentioned") %>%
    select(v85, v86, v87, v88, v89, v90, v91, v92, v93, v94, v95)

  # Are there rows for which the condition is not met?
  which(rowSums(check_Q28_v96a != "dont know") != 0)

  # I will drop v96a because information is already recorded in v85 to v95
  EVS2017 <- dropVars(EVS2017, "v96a")

# v96b
  # Check: v96b == mentioned; v85 to v95 == no answer
  check_Q28_v96b <- EVS2017 %>%
    filter(v96b == "mentioned") %>%
    select(v85, v86, v87, v88, v89, v90, v91, v92, v93, v94, v95)

  # Are there rows for which the condition is not met?
  which(rowSums(check_Q28_v96b != "no answer") != 0)

  # I will drop v96b because information is already recorded in v85 to v95
  EVS2017 <- dropVars(EVS2017, "v96b")

# > Q33: aims of this country --------------------------------------------------

# v108 and v109
  # Possible inconsistency
  table(EVS2017$v108, EVS2017$v109)

  # Inconcisenct flag
  table(EVS2017$f108)

  # Explore what happens for the inconsistent case
  EVS2017 %>%
    select(v108, v109, f108) %>%
    filter(f108 == "inconsistent")

  # Replace inconsistency with missing values
  EVS2017 <- EVS2017 %>%
    mutate(v108 = replace(x      = v108,
                          list   = f108 == "inconsistent",
                          values = NA),
           v109 = replace(x      = v109,
                          list   = f108 == "inconsistent",
                          values = NA))

  # Get rid of inconsistency flag
  EVS2017 <- dropVars(EVS2017, "f108")

# > Q34-35: which of these most important --------------------------------------

# v110 and v111
  # Possible inconsistency
  table(EVS2017$v110, EVS2017$v111)

  # Inconcisenct flag
  table(EVS2017$f110)

  # Explore what happens for the inconsistent case
  EVS2017 %>%
    select(v110, v111, f110) %>%
    filter(f110 == "inconsistent")

  # Replace inconsistency with missing values
  EVS2017 <- EVS2017 %>%
    mutate(v110 = replace(x      = v110,
                          list   = f110 == "inconsistent",
                          values = NA),
           v111 = replace(x      = v111,
                          list   = f110 == "inconsistent",
                          values = NA))

  # Get rid of inconsistency flag
  EVS2017 <- dropVars(EVS2017, "f110")

  # Drop post materialist index
  EVS2017 <- dropVars(EVS2017, "v111_4")

# > Q39: Democracy variables ---------------------------------------------------

  # Variants are tagged by _11c
  table(og = EVS2017$v133,
        variant = EVS2017$v133_11c
  )

  table(og = EVS2017$v134,
        variant = EVS2017$v134_11c
  )

  table(og = EVS2017$v138,
        variant = EVS2017$v138_11c
  )

  # Just get rid of variant (info already in main variable)
  EVS2017 <- dropVars(EVS2017, paste0("v", 133:141, "_11c"))

# > Q45: aspects of a job ------------------------------------------------------

  # Trust the answers on v40 to v45, get rid of rest
  drop_q45 <- c("f45a", "v45a", "v45b", "v45c")

  # Drop f20 and v20 as all the info is in the other variables already
  EVS2017 <- dropVars(EVS2017, drop_q45)

# > Q46 Q47 Nationality --------------------------------------------------------

  # Cross-table
  table(nationa = EVS2017$v169,
        proud = EVS2017$v170
  )

  # Just drop those 2000 obs that have particular nationality status
  EVS2017 <- EVS2017 %>%
    filter(v169 == 'yes')

  # and get rid of v169 which becames a constant
  EVS2017 <- dropVars(EVS2017, "v169")

# > Q49: Political party -------------------------------------------------------

  # Drop country based variables (and keep standardized)
  EVS2017 <- dropVars(EVS2017, c("v174_cs", "v175_cs"))

# > age ------------------------------------------------------------------------

  EVS2017 <- EVS2017 %>%
    mutate(age = replace(x      = age,
                         list   = age %in% levels(EVS2017$age)[1:8],
                         values = NA))


  # Replace "82 and more" with just "82"
  EVS2017$age <- recode_factor(EVS2017$age, "82 and older" = "82")

  # Drop empty levels
  EVS2017$age <- droplevels(EVS2017$age)

  # Transform to numbers
  EVS2017$age <- as.numeric(as.character(EVS2017$age))

  # Drop other age variables
  age_drop <- c("v226", "age_r", "age_r2", "age_r3", "age_r", "age_r3_weight")
  EVS2017 <- dropVars(EVS2017, age_drop)

# > country born in ------------------------------------------------------------

  # Drop base v228b
  EVS2017 <- dropVars(EVS2017, "v228b")

  # Drop year moved in country as very few cases applicable
  EVS2017 <- dropVars(EVS2017, "v229")

# > marital status ------------------------------------------------------------

  # Drop gated questions
  # - 235: living together before marriage
  # - 236: Do you live with a partner?
  # - 237: Do you have a steady relationship?
  # - 250: Was your partner/spouse born in
  EVS2017 <- dropVars(EVS2017, c("v235", "v236", "v237", "v250", "v251b_r"))

# > number of children ---------------------------------------------------------

  # Drop base variables
  EVS2017 <- dropVars(EVS2017, c("v239a", "v239b"))

  # Recode generic number of children and keep it
  EVS2017$v239_r <- recode_factor(EVS2017$v239_r, "5 and more" = "5")
  EVS2017$v239_r <- recode_factor(EVS2017$v239_r, "no children" = "0")

# > leaving in household

  # Recode live alone to 1
  EVS2017$v240 <- recode_factor(EVS2017$v240, "I live alone" = "1")

  # Recode 6 and more to 6
  EVS2017$v240 <- recode_factor(EVS2017$v240, "6 and more" = "6")
  unique(EVS2017$v240)

  # Drop gated questions
  EVS2017 <- dropVars(EVS2017, "v241")

# > completed education --------------------------------------------------------

  # Recode # and younger / older
  EVS2017$v242 <- recode_factor(EVS2017$v242, "7 and younger" = "7")
  EVS2017$v242 <- recode_factor(EVS2017$v242, "70 and older" = "70")

  # Drop the no formal education people
  EVS2017 <- EVS2017 %>%
    filter(v242 != "no formal education")

  # Drop recoded version
  EVS2017 <- dropVars(EVS2017, "v242_r")

  # Drop partner info (gated)
  EVS2017 <- dropVars(EVS2017, c("v255_egp", "v252_ISCED_1"))

  # Drop country specific versins of different education variables
  cs_edu <- c("v243_cs_DE1", "v243_cs_DE2", "v243_cs_DE3", "v243_cs_GB1", "v243_cs_GB2",
              "v252_cs_DE1", "v252_cs_DE2", "v252_cs_DE3", "v252_cs_GB1", "v252_cs_GB2",
              "v262_cs_DE1", "v262_cs_DE2", "v262_cs_DE3", "v262_cs_GB1", "v262_cs_GB2",
              "v263_cs_DE1", "v263_cs_DE2", "v263_cs_DE3", "v263_cs_GB1", "v263_cs_GB2"
  )
  EVS2017 <- dropVars(EVS2017, cs_edu)

# > employment -----------------------------------------------------------------

  # Drop too gated questions
  EVS2017 <- dropVars(EVS2017,
                      c(
                        "v245", # last employment
                        "v247",
                        "v248",
                        "v248a"
                      )
  )

  # Drop other employment related questions not needed
  EVS2017 <- dropVars(EVS2017,
                      paste0("v", c(249, 253:254, 256:258, "255_egp"))
  )

# > date of interview ---------------------------------------------------------------------

  # Keep length of interview in minutes drop everything else
  EVS2017 <- dropVars(EVS2017,
                      paste0("v",
                             c(277,
                               "278a",
                               "278b",
                               "278c_r",
                               "279a",
                               "279b",
                               "279c_r"
                             )
                      )
  )

  # Recode "item not included" to missing value
  EVS2017 <- EVS2017 %>%
    mutate(v279d_r = replace(x      = v279d_r,
                             list   = v279d_r == "item not included",
                             values = NA))

# > interest in interview ------------------------------------------------------

  # Recode not included to missing value
  EVS2017 <- EVS2017 %>%
    mutate(v280 = replace(x      = v280,
                          list   = v280 == "item not included",
                          values = NA))
  # EVS2017$v280 <- recode_factor(EVS2017$v280, "item not included" = "NA")

# > language of interview ------------------------------------------------------

  # Check overlap between country of interview and language of inteview
  table(lan = EVS2017$v281a, EVS2017$v275b_N1)

  # Drop langauge as it is too similar to other country variables in the data
  EVS2017 <- dropVars(EVS2017, "v281a")

# Step 4: Variable types -------------------------------------------------------

var_types <- list(
  # Binary variables
  bin = paste0("v",
               c(
                 # (not) mentioned
                 9:19,
                 22:26,
                 40:45,
                 85:95,
                 # yes / no
                 21,
                 57:61,
                 112,
                 227,
                 259,
                 260,
                 230,
                 232,
                 # man woman
                 225,
                 # trust no trust
                 31,
                 # Agree disagree
                 71
               )),
  # Categorical variables
  cat = c("mode",
          "country",
          paste0("v",
                 c(
                   # Religious denomination
                   "52_r",
                   # Frequency of attendance (7 k)
                   54, 55,
                   # Consider yourself (3k)
                   56,
                   # statements close to beliefs (4 k)
                   62,
                   # how often pray
                   64,
                   # agree disagree (4 k)
                   72:79,
                   # agree disagree (5 k)
                   80:84,
                   # Might have done it (3 k)
                   98:101,
                   # Which of these most important (4 k)
                   108:109,
                   110:111,
                   # good, bad, do not mind
                   113:114,
                   # Preference among two options (with residual category)
                   204,
                   # Country of birth
                   "228b_r",
                   "231b_r",
                   "233b_r",
                   # marital status (6 k)
                   234,
                   # living with parent
                   238,
                   # educational level respondent
                   "243_ISCED_1",
                   # Employment
                   244,
                   # education level father
                   "262_ISCED_1",
                   # educational level mother
                   "263_ISCED_1",
                   # Job / profession respondent
                   "246_egp", # keep
                   # Income
                   261,
                   # When you were 14
                   264:274,
                   # Region of interview,
                   "275b_N1"
                 ))
  ),

  # Ordinal variables
  ord = paste0("v", c(
    # important in life (4 k)
    1:6,
    # how happy (4 k)
    7,
    # state of health (4 k)
    8,
    # Trust level (4 k)
    32:37,
    # control over life (10 k)
    38,
    # satisfied with life (10 k)
    39,
    # agree disagree with statements (5 k)
    46:50,
    # importance of god in life (10 k)
    63,
    # succesful partnership
    65:70,
    # very somewhat (4 k)
    97,
    # place your views on this scale (10 k)
    102:107,
    # Confidence in (4 k)
    115:132,
    # Essential to democracy (10 k)
    133:144,
    # very good, fairly good, fairly bad or very bad (10 k)
    145:148,
    # justified, never be justified, or something in between (10 k)
    149:163,
    # how close you feel to (4 k)
    164:168,
    # proud of nationality (4 k)
    170,
    # how often (3 k)
    171:173,
    # Left/right
    "174_LR", "175_LR",
    # how often in country's elections (4 k)
    176:183,
    # immigration (5 k)
    184,
    # immigration (10 k)
    185:188,
    # important (4 k)
    189:197,
    # important (10 k)
    198,
    # agree or disagree with this statement (5 k)
    199:203,
    # government should or should not (4 k)
    205:207,
    # how often you follow politics (5 k)
    208:211,
    # feel concerned about the living conditions of (5 k)
    212:220,
    # What should a society provide?
    221:224,
    # years completed education
    242,
    # Duration of interview
    "279d_r",
    # Interest in iterview
    280,
    # Size of town interview
    "276_r"
  )
  ),

  # count
  cnts = paste0("v",
                c(
                  # Number of children
                  "239_r",
                  # People in household
                  240
                )
  ),
  num = "age"
)

# Check all variables have measurement level defined
length(colnames(EVS2017)[!colnames(EVS2017) %in% unlist(var_types)]) == 0

# Check variables
unlist(var_types)[!unlist(var_types) %in% colnames(EVS2017)]

# Store variable roles in the input folder
saveRDS(var_types, "../input/var_types.rds")

# Step 3: Missing cases cases ---------------------------------------------------------

  NA_labels <- c("multiple answers Mail",
                 "no follow-up",
                 "follow-up non response",
                 "other missing",
                 "item not included",
                 "not applicable",
                 "no answer",
                 "dont know",
                 "does not apply to me",
                 "never had a paid job",
                 "no formal education",
                 "not allowed to vote",
                 "other answer (code if volunteered only)"
  )

  for (j in 1:ncol(EVS2017)){
    if(is.factor(EVS2017[, j])){
      # Find na labels
      na_index <- which(EVS2017[, j] %in% NA_labels)

      # Replace with NAs
      EVS2017[na_index, j] <- NA
    }
  }

  # Complete cases
  sum(rowSums(is.na(EVS2017)) == 0)

  EVS2017[rowSums(is.na(EVS2017)) == 0, ]

# > Step 6: Final checks on data -----------------------------------------------

  # Drop empty categories
  for (j in 1:ncol(EVS2017)){
    if(is.factor(EVS2017[, j])){
      EVS2017[, j] <- droplevels(EVS2017[, j])
    }
  }

# > Step 4: Reduce sample size -------------------------------------------------

  # Select only obs from founding countries
  EVS2017_fc <- EVS2017 %>%
    filter(country %in% c("Germany", "Italy", "France", "Belgium", "Netherlands"))

  # Drop levels of country not used
  EVS2017_fc$country <- droplevels(EVS2017_fc$country)

# > Step 5: Single imputation --------------------------------------------------

  # Which predictors to use
  qp_mat <- quickpred(EVS2017_fc)

  # Parallel version micemd
  library(micemd)

  # Multiple imputation with pmm of the data
  imp <- mice.par(don.na = EVS2017_fc,
                  method = "pmm",
                  predictorMatrix = qp_mat,
                  m = 5,
                  maxit = 25,
                  seed = 20220421,
                  nnodes = 5)

  # Save the results in output
  saveRDS(imp, "../output/ZA7500_mi.rds")

  # Read if you had saved it already
  imp <- readRDS("../output/ZA7500_mi.rds")

  # Convergence checks
  plot.mids_formula <- as.formula(paste0(
    paste0(colnames(EVS2017_filled)[-c(1:2)][50:60],
           collapse = " + "),
    " ~ .it | .ms"
  ))
  plot(imp, plot.mids_formula, layout = c(2, 5))

  # Extract the first data to use it
  EVS2017_filled <- complete(imp, 1)

# > Step 7: Save new data ------------------------------------------------------

# Small data for experiments (Complete case data)
temp <- EVS2017[rowSums(is.na(EVS2017)) == 0, ]

saveRDS(EVS2017_filled, "../input/ZA7500_processed.rds")
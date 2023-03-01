#### 02 CODE CHECKING AND PROCESSING ###########################################

source("R/001_code_import.R")


# Count transcripts -------------------------------------------------------

stopifnot(length(unique(snippets$transcript)) == 88L)


# Vectors of missing codes ------------------------------------------------

missing_province <- c()
missing_age <- c("DJ_Aug3_MA*weak", "TL_Aug24_BM")
missing_hh_size <- c()
missing_gender <- c()
missing_race <- c("TL_Aug24_BM")
missing_indigenous <- c("TL_Aug24_BM")
missing_children <- c()
missing_lone_parent <- c()
missing_pets <- c()
missing_income <- c("SM_Jan30_CC", "PR_Jul29_CC", "PJ_Oct12_CC", "LSM_Aug2_CC")
missing_eviction_type <- c()
missing_landlord_type <- c()
missing_UD_rent <- c("DM_Aug23_BM", "KA_Sept29_DK", "SG_Aug4_DK", "JB_Jan17_DK", 
                     "MC_Sept16_DK", "MS_Sept8_DK", "RL_Sept16_BM", 
                     "ST_Sept30_BM", "TL_Aug24_BM", "BS_Jul28_MA",
                     "DJ_Aug3_MA*weak", "GB_Oct7_MA*weak", "JJ_Aug17_MA",
                     "KA_Jan26_MA", "MW_Sept30_MA", "RB_Jan20_MA",
                     "SP_Aug29_MA_CC", "CM_Aug10_CC", "JO_Aug1_CC", 
                     "LSM_Aug2_CC", "MW_Oct4_CC")
missing_UD_quality <- c("BS_Jul28_MA", "DJ_Aug3_MA*weak", "GB_Oct7_MA*weak",
                        "JB_Jul29_MA", "JB_Oct14_MA", "JJ_Aug17_MA",
                        "KB_Sept15_MA", "MW_Sept30_MA", "MC_Sept16_DK", 
                        "RL_Sept16_BM", "ST_Sept30_BM", "TL_Aug24_BM",
                        "CM_Aug10_CC", "MW_Oct4_CC", "JO_Aug1_CC", 
                        "ZM_Sept30_BM_CC", "MH_Sept22_CC", "RS_Feb6_CC", 
                        "EB_Jan17_CC")
missing_UD_size <- c("MP_Sept1_DK", "SB_Aug2_BM", "BS_Jul28_MA", 
                     "DJ_Aug3_MA*weak", "GB_Oct7_MA*weak", "JB_Jul29_MA",
                     "JB_Oct14_MA", "JJ_Aug17_MA", "MW_Sept30_MA",
                     "ZM_Sept30_BM_CC", "CM_Aug10_CC", "EB_Jan17_CC", 
                     "JO_Aug1_CC", "LSM_Aug2_CC", "MH_Sept22_CC", "MJ_Oct12_CC",
                     "MW_Oct4_CC", "PR_Jul29_CC")
missing_UD_loc <- c("BS_Jul28_MA", "DJ_Aug3_MA*weak", "GB_Oct7_MA*weak",
                    "JK_Jan31_MA", "MW_Sept30_MA", "NWS_Oct18_MA", 
                    "RB_Jan20_MA", "DM_Aug23_BM", "KA_Sept29_DK", 
                    "ST_Sept30_BM", "CM_Aug10_CC", "MJ_Oct12_CC", 
                    "ZM_Sept30_BM_CC", "RS_Feb6_CC", "MW_Oct4_CC",
                    "PR_Jul29_CC")
missing_UD_tenure <- c("DJ_Aug3", "TO_Jan26_MA", "CM_Aug10_CC", 
                       "SP_Aug29_MA_CC", "DM_Aug23_BM", "DJ_Aug3_MA*weak")
missing_mobility <- c("GB_Oct7_MA*weak")
missing_hcc <- c("CM_Aug10_CC")


# Missing province --------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(province), !transcript %in% missing_province) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing age -------------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(age), !transcript %in% missing_age) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing HH size ---------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(hh_size), !transcript %in% missing_hh_size) |> 
    pull(transcript) |> 
    unique() |> 
    length() 
  } == 0)


# Missing gender ----------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(gender), !transcript %in% missing_gender) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing race ------------------------------------------------------------

stopifnot({
  snippets |> 
    # These transcripts didn't have the question answered
    filter(is.na(race), !transcript %in% missing_race) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing indigenous ------------------------------------------------------

stopifnot({
  snippets |> 
    # These transcripts didn't have the question answered
    filter(is.na(indigenous), !transcript %in% missing_indigenous) |> 
    pull(transcript) |> 
    unique() |> 
    length() 
  } == 0)


# Missing children --------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(children), !transcript %in% missing_children) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing lone-parent -----------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(lone_parent), !transcript %in% missing_lone_parent) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing pets ------------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(pets), !transcript %in% missing_pets) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing income ----------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(income_on_rent), !transcript %in% missing_income) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing eviction type ---------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(ET = sum(code %in% c("ET-OW", "ET-R", "ET-S", "ET-NP", "ET-OT",
                                   "ET-OL", "ET-RT")) > 0, .by = transcript) |> 
    filter(!ET, !transcript %in% missing_eviction_type) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Missing landlord type ---------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(LT = sum(category == "LT") > 0, .by = transcript) |> 
    filter(!LT, !transcript %in% missing_landlord_type) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Missing UD rent ---------------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(rent = sum(code %in% c("UD-C+", "UD-C=", "UD-C-")) >= 1, 
              .by = transcript) |> 
    filter(!rent, !transcript %in% missing_UD_rent) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Missing UD quality ------------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(quality = sum(code %in% c("UD-Q+", "UD-Q=", "UD-Q-")) >= 1, 
              .by = transcript) |> 
    filter(!quality, !transcript %in% missing_UD_quality) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Missing UD size ---------------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(size = sum(code %in% c("UD-S+", "UD-S=", "UD-S-")) >= 1, 
              .by = transcript) |> 
    filter(!size, !transcript %in% missing_UD_size) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Missing UD location -----------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(loc = sum(code %in% c("UD-L+", "UD-L=", "UD-L-")) >= 1, 
              .by = transcript) |> 
    filter(!loc, !transcript %in% missing_UD_loc) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Missing UD tenure -------------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(tenure = sum(code %in% c("UD-TP", "UD-TS", "UD-TNM", 
                                       "UD-TO")) >= 1, 
              .by = transcript) |> 
    filter(!tenure, !transcript %in% missing_UD_tenure) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Missing mobility --------------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(mob = sum(code %in% c("M-S", "M-N", "M-C", "M-R", "M-P", "M-out",
                                    "M-O")) >= 1, 
              .by = transcript) |> 
    filter(!mob, !transcript %in% missing_mobility) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Missing HH change -------------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(hcc = sum(code %in% c("HCC-Y", "HCC-N")) >= 1, 
              .by = transcript) |> 
    filter(!hcc, !transcript %in% missing_hcc) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Process race ------------------------------------------------------------

snippets <-
  snippets |> 
  mutate(race = case_when(
    indigenous == "Yes" | str_detect(race, "Indigenous") ~ "indigenous",
    race %in% c("Asian", "Biracial (Indian and Canadian)",
                "European, Chinese, Japanese") ~ "asian",
    race %in% c("Black", "Caribbean", "Haitian", "Nigerian", 
                "Somalian/ Jamaican") ~ "black",
    race %in% c("Mexican", "Spanish", "Biracial (White and Hispanic)") ~ 
      "hispanic",
    race %in% c("Visible minority", "Biracial (White and Egyptian)") ~ 
      "other_vm",
    race %in% c("White/Caucasian", "Acadian", "Biracial (White and Acadian)",
                "Biracial (White and Italian)", "Biracial (White and Jewish)") ~ 
      "white"))

transcripts <- 
  transcripts |> 
  mutate(race = case_when(
    indigenous == "Yes" | str_detect(race, "Indigenous") ~ "indigenous",
    race %in% c("Asian", "Biracial (Indian and Canadian)",
                "European, Chinese, Japanese") ~ "asian",
    race %in% c("Black", "Caribbean", "Haitian", "Nigerian", 
                "Somalian/ Jamaican") ~ "black",
    race %in% c("Mexican", "Spanish", "Biracial (White and Hispanic)") ~ 
      "hispanic",
    race %in% c("Visible minority", "Biracial (White and Egyptian)") ~ 
      "other_vm",
    race %in% c("White/Caucasian", "Acadian", "Biracial (White and Acadian)",
                "Biracial (White and Italian)", "Biracial (White and Jewish)") ~ 
      "white"))
    

# Process children --------------------------------------------------------

snippets <- 
  snippets |> 
  mutate(children = children == "Yes")

transcripts <- 
  transcripts |> 
  mutate(children = children == "Yes")


# Process pets ------------------------------------------------------------

snippets <- 
  snippets |> 
  mutate(pets = pets %in% c("Cat", "Dog", "Dog and cat"))

transcripts <- 
  transcripts |> 
  mutate(pets = pets %in% c("Cat", "Dog", "Dog and cat"))


# Process income ----------------------------------------------------------

snippets <- 
  snippets |> 
  mutate(income = case_when(
    parse_number(income_on_rent) < 30 ~ "0 - 29",
    parse_number(income_on_rent) < 49 ~ "30 - 49",
    parse_number(income_on_rent) <= 100 ~ "50 - 100"), 
    .after = income_on_rent) |> 
  select(-income_on_rent)

transcripts <- 
  transcripts |> 
  mutate(income = case_when(
    parse_number(income_on_rent) < 30 ~ "0 - 29",
    parse_number(income_on_rent) < 49 ~ "30 - 49",
    parse_number(income_on_rent) <= 100 ~ "50 - 100"), 
    .after = income_on_rent) |> 
  select(-income_on_rent)


# Process disability ------------------------------------------------------

snippets <-
  snippets |> 
  mutate(
    intellectual = case_when(
      intellectual == "Yes" ~ TRUE,
      intellectual == "No" ~ FALSE,
      .default = NA),
    physical = case_when(
      str_detect(physical, "Yes") ~ TRUE,
      physical == "No" ~ FALSE,
      .default = NA),
    disability = intellectual + physical > 0) |> 
  relocate(disability, .after = physical) |> 
  select(-intellectual, -physical)
    
transcripts <-
  transcripts |> 
  mutate(
    intellectual = case_when(
      intellectual == "Yes" ~ TRUE,
      intellectual == "No" ~ FALSE,
      .default = NA),
    physical = case_when(
      str_detect(physical, "Yes") ~ TRUE,
      physical == "No" ~ FALSE,
      .default = NA),
    disability = intellectual + physical > 0) |> 
  relocate(disability, .after = physical) |> 
  select(-intellectual, -physical)



# Clean up ----------------------------------------------------------------

rm(missing_province, missing_age, missing_hh_size, missing_gender, missing_race,
   missing_indigenous, missing_children, missing_lone_parent, missing_pets,
   missing_income, missing_eviction_type, missing_landlord_type, 
   missing_UD_rent, missing_UD_quality, missing_UD_size, missing_UD_loc,
   missing_UD_tenure, missing_mobility)

#### 02 CODE CHECKING AND PROCESSING ###########################################

source("R/001_code_import.R")


# Count transcripts -------------------------------------------------------

stopifnot(length(unique(snippets$transcript)) == 88L)


# Missing province --------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(province)) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing age -------------------------------------------------------------

stopifnot({
  snippets |> 
    # These transcripts didn't have the question answered
    filter(is.na(age), !transcript %in% c("DJ_Aug3_MA*weak", "TL_Aug24_BM")) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing HH size ---------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(hh_size)) |> 
    pull(transcript) |> 
    unique() |> 
    length() 
  } == 0)


# Missing gender ----------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(gender)) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing race ------------------------------------------------------------

stopifnot({
  snippets |> 
    # These transcripts didn't have the question answered
    filter(is.na(race), !transcript %in% c("TL_Aug24_BM")) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing indigenous ------------------------------------------------------

stopifnot({
  snippets |> 
    # These transcripts didn't have the question answered
    filter(is.na(indigenous), !transcript %in% c("TL_Aug24_BM")) |> 
    pull(transcript) |> 
    unique() |> 
    length() 
  } == 0)


# Missing children --------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(children)) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing lone-parent -----------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(lone_parent)) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing pets ------------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(pets)) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing income ----------------------------------------------------------

stopifnot({
  snippets |> 
    filter(is.na(income_on_rent), !transcript %in% c(
      "SM_Jan30_CC", "PR_Jul29_CC", "PJ_Oct12_CC", "LSM_Aug2_CC")) |> 
    pull(transcript) |> 
    unique() |> 
    length()
  } == 0)


# Missing eviction type ---------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(ET = sum(code %in% c("ET-OW", "ET-R", "ET-S", "ET-NP", "ET-OT",
                                   "ET-OL", "ET-RT")) > 0, .by = transcript) |> 
    filter(!ET) |> 
    pull(transcript) |> 
    length()
  } == 0)


# Missing landlord type ---------------------------------------------------

stopifnot({
  transcripts |> 
    summarize(LT = sum(category == "LT") > 0, .by = transcript) |> 
    filter(!LT) |> 
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

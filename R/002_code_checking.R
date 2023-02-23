#### 02 CODE CHECKING AND PROCESSING ###########################################

source("R/001_code_import.R")


# Missing province --------------------------------------------------------

snippets |> 
  filter(is.na(province)) |> 
  pull(transcript) |> 
  unique()


# Missing age -------------------------------------------------------------

snippets |> 
  filter(is.na(age), !transcript %in% c("DJ_Aug3_MA*weak")) |> 
  pull(transcript) |> 
  unique()


# Missing HH size ---------------------------------------------------------

snippets |> 
  filter(is.na(hh_size)) |> 
  pull(transcript) |> 
  unique()


# Missing gender ----------------------------------------------------------

snippets |> 
  filter(is.na(gender)) |> 
  pull(transcript) |> 
  unique()


# Missing race ------------------------------------------------------------

snippets |> 
  filter(is.na(race)) |> 
  pull(transcript) |> 
  unique()


# Missing indigenous ------------------------------------------------------

snippets |> 
  filter(is.na(indigenous)) |> 
  pull(transcript) |> 
  unique()


# Process race ------------------------------------------------------------

snippets <- 
  snippets |> 
  mutate(race = case_when(
    indigenous == "Yes" ~ "indigenous",
    race == "Asian" ~ "asian",
    str_detect(race, "Biracial|Mixed") & !str_detect(race, "Indigenous") ~ 
      "biracial",
    race %in% c("Black", "Caribbean", "Haitian", "Nigerian") ~ "black",
    race %in% c("Mexican", "Spanish") ~ "hispanic",
    race %in% c("White/Caucasian", "Acadian") ~ "white"))

transcripts <- 
  transcripts |> 
  mutate(race = case_when(
    indigenous == "Yes" ~ "indigenous",
    race == "Asian" ~ "asian",
    str_detect(race, "Biracial|Mixed") & !str_detect(race, "Indigenous") ~ 
      "biracial",
    race %in% c("Black", "Caribbean", "Haitian", "Nigerian") ~ "black",
    race %in% c("Mexican", "Spanish") ~ "hispanic",
    race %in% c("White/Caucasian", "Acadian") ~ "white"))


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

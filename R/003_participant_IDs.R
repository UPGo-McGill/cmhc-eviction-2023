#### 03 PARTICIPANT IDS ########################################################


# Make correspondence table -----------------------------------------------

ids <- c(
  "1" =	"BS_Jul28_MA",
  "2" =	"JB_Jul29_MA",
  "3" =	"PR_Jul29_CC",
  "4" =	"JO_Aug1_CC",
  "5" =	"SB_Aug2_BM",
  "6" =	"PS_Aug2_BM",
  "7" =	"LSM_Aug2_CC",
  "8" =	"KC_Aug3_CC",
  "9" =	"DJ_Aug3_MA*weak",
  "10" = "SG_Aug4_DK",
  "11" = "LW_Aug4_DK",
  "12" = "MC_Aug10_CC",
  "13" = "CM_Aug10_CC",
  "14" = "JJ_Aug17_MA",
  "15" = "DM_Aug23_BM",
  "16" = "AL_Aug24_DK",
  "17" = "TL_Aug24_BM",
  "18" = "SP_Aug29_MA_CC",
  "19" = "MP_Sept1_DK",
  "20" = "MS_Sept8_DK",
  "21" = "LB_Sept12_CS",
  "22" = "KB_Sept15_MA",
  "23" = "MC_Sept16_DK",
  "24" = "RL_Sept16_BM",
  "25" = "MH_Sept22_CC",
  "26" = "KA_Sept29_DK",
  "27" = "CM_Sept29_BM",
  "28" = "AD_Sept29_DK",
  "29" = "ZM_Sept30_BM_CC",
  "30" = "ST_Sept30_BM",
  "31" = "MW_Sept30_MA",
  "32" = "MF_Oct3_calibration",
  "33" = "SNB_Oct3_BM",
  "34" = "MW_Oct4_CC",
  "35" = "GB_Oct7_MA*weak",
  "36" = "MJ_Oct12_CC",
  "37" = "PJ_Oct12_CC",
  "38" = "JB_Oct14_MA",
  "39" = "NWS_Oct18_MA",
  "40" = "BD_Nov8_MA",
  "41" = "NR_Nov09_CS",
  "42" = "SD_Nov11_DK",
  "43" = "PSC_Nov11_CS",
  "44" = "AJ_Nov17_CS",
  "45" = "MEL_Nov17_CS",
  "46" = "MC_Dec6_CS",
  "47" = "SS_Dec13_DK",
  "48" = "RD_Dec20_CS",
  "49" = "PA_Dec20_calibration",
  "50" = "EM_Dec22_CS",
  "51" = "SF_Jan12_CS",
  "52" = "RM_Jan13_CS",
  "53" = "KS_Jan13_CS",
  "54" = "POT_Jan16_CS",
  "55" = "VL_Jan16_CS",
  "56" = "JC_Jan17_CS",
  "57" = "MB_Jan17_CS",
  "58" = "EB_Jan17_CC",
  "59" = "MA_Jan18_23_DK",
  "60" = "CD_Jan18_CS",
  "61" = "HGB_Jan18_CS",
  "62" = "RB_Jan20_MA",
  "63" = "KL_Jan20_CS",
  "64" = "ED_Jan20_CS",
  "65" = "MB_Jan23_CS",
  "66" = "GB_Jan24_MA",
  "67" = "CBP_Jan24_CS",
  "68" = "DE_Jan25_CC",
  "69" = "MP_Jan25_CS",
  "70" = "RLD_Jan25_CS",
  "71" = "TO_Jan26_MA",
  "72" = "KA_Jan26_MA",
  "73" = "HS_Jan26_DW",
  "74" = "ISP_Jan26_CS",
  "75" = "OL_Jan26_CS",
  "76" = "EMF_Jan26_CS",
  "77" = "DKT_Jan27_CS",
  "78" = "AW_Jan27_CS",
  "79" = "JB _Jan27_DK",
  "80" = "JW_Jan30_DK",
  "81" = "SM_Jan30_CC",
  "82" = "SD_Jan30_CS",
  "83" = "KD_Jan31_CC",
  "84" = "MB_Jan31_CS",
  "85" = "JK_Jan31_MA",
  "86" = "ZF_Feb1_CC",
  "87" = "DB_Feb1_DK",
  "88" = "RS_Feb6_CC"
  ) |> 
  enframe() |> 
  set_names(c("id", "transcript"))


# Update tables -----------------------------------------------------------

transcripts <- 
  transcripts |> 
  inner_join(ids, by = "transcript") |> 
  relocate(id, .before = transcript) |> 
  select(-transcript) |> 
  arrange(id, category, code)

snippets <- 
  snippets |> 
  inner_join(ids, by = "transcript") |> 
  relocate(id, .before = transcript) |> 
  select(-transcript) |> 
  arrange(id)

rm(ids)


# Helper functions for querying IDs and quotes ----------------------------

# Get info on an ID
info <- function(x) {
  
  transcripts |> 
    filter(id == as.character(x)) |> 
    summarize(
      across(c(id, province, city, gender, age, indigenous, race, children, 
               lone_parent, pets, income, intellectual, physical, hh_size), 
             first),
      corporate = sum(code == "LT-C") >= 1,
      own_use = sum(code == "ET-OW") > 0,
      reno = sum(code == "ET-R") > 0,
      sale = sum(code == "ET-S") > 0,
      retal = sum(code == "ET-RT") > 0) |> 
    glimpse()
  
}

# Get snippets by text contents
snip_text <- function(x) {
  
  snippets |> 
    filter(str_detect(snippet, x)) |> 
    select(id, snippet)
  
}

# Get snippets by code and other filters
snip_code <- function(code, ...) {
  
  snippets |> 
    filter(map_lgl(codes, \(x) sum(x == code) > 0)) |> 
    filter(...)
  
}

library(tidyverse)


#..........................
# read in data
#..........................
knywr <- readxl::read_excel(path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/2012 Final Measures NFB_ZPM_6_27_19.xlsx", sheet = 6) %>%
  dplyr::rename(id = ChimpID_Final,
                length = AvgOfAvg_Length_Series,
                age = AvgOfAge
                ) %>%
  dplyr::mutate(site = "Kanyawara")

cpt <- readxl::read_excel(path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/Ngamba and UWEC weights with area and length.xlsx", sheet = 1) %>%
  dplyr::rename(length = avg.length) %>%
  dplyr::mutate(site = "Uganda-Captive")

snw <- dplyr::bind_rows(
  readr::read_csv("~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/sanwa_extracted_males_agelengths.csv"),
  readr::read_csv("~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/sanwa_extracted_females_agelengths.csv")
  ) %>%
  dplyr::mutate(sex = ifelse(sex == "Female", "F", ifelse(sex == "Male", "M", NA)))

dat <- dplyr::bind_rows(knywr, cpt, snw)

#..........................
# Get Summaries
#..........................

dat %>%
  dplyr::group_by(site, sex) %>%
  dplyr::summarise(
    meanlength = mean(length)
  ) # can do all sorts of combinations here...




saveRDS(dat, file = "data/chimpanzee_lengthage_averages.rds")



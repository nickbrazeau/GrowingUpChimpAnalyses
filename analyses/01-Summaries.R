library(tidyverse)


#..........................
# read in data
#..........................
knywr <- readxl::read_excel(path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/Final_combined 2012_2013_body size data.xlsx",
                            sheet = 2) %>%
  dplyr::rename(id = ChimpID_Final,
                length = AvgOfAvgOfAVG_LENGTH,
                age = AvgOfage
                ) %>%
  dplyr::mutate(site = "Kanyawara") %>%
  select(-c("series count"))

cpt <- readxl::read_excel(path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/Ngamba and UWEC weights with area and length.xlsx", sheet = 1) %>%
  dplyr::rename(length = avg.length) %>%
  dplyr::mutate(site = "Uganda-Captive")

snw <- dplyr::bind_rows(
  readr::read_csv("~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/sanwa_extracted_males_agelengths.csv"),
  readr::read_csv("~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/sanwa_extracted_females_agelengths.csv")
  ) %>%
  dplyr::mutate(sex = ifelse(sex == "Female", "F", ifelse(sex == "Male", "M", NA)),
                length = length/10)

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


















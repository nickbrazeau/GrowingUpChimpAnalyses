

nfb <- readxl::read_excel(path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/JHE_Quality Compare_July_2015.xlsx") %>%
  magrittr::set_colnames(tolower(colnames(.))) %>%
  dplyr::mutate(photoid = gsub("MC2_", "", photoid),
                photoid = as.numeric(photoid)) %>%
  dplyr::select(c("date", "photoid", "chimpid", "sex", "age", "mleng_new", "sum_picqual"))

jr <- readxl::read_excel(path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/2012 Final Measures NFB_ZPM_6_27_19.xlsx",
                         sheet = 2) %>%
  magrittr::set_colnames(tolower(colnames(.))) %>%
  dplyr::rename(chimpid = chimpid_final) %>%
  dplyr::mutate(photoid = gsub("IMG_", "", photoid),
                photoid = gsub("INF", "", photoid),
                photoid = gsub("a", "", photoid),
                photoid = gsub("b", "", photoid),
                photoid = gsub("c", "", photoid),
                photoid = gsub("D", "", photoid),
                photoid = as.numeric(photoid)) %>%
  dplyr::select(c("date", "photoid", "chimpid", "jr_truelengthcm"))


comb <- dplyr::left_join(nfb, jr, by = c("date", "photoid", "chimpid"))

write_csv(x = comb, path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/nfb_jr_lenght_measures_combined.csv")





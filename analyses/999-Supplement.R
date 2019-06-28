library(tidyverse)
source("R/01-plotting.R")
knywr <- readxl::read_excel(path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/Final_combined 2012_2013_body size data.xlsx",
                            sheet = 1) %>%
  dplyr::rename(id = ChimpID_Final,
                length = AvgOfAVG_LENGTH
  ) %>%
  magrittr::set_colnames(tolower(colnames(.))) %>%
  dplyr::mutate(site = "Kanyawara",
                year = lubridate::year(date),
                year = factor(year))


# look at individuals over time
plotall <- ggplot(data = knywr) +
  geom_point(aes(x = date, y = length, group = id, color = year)) +
  facet_wrap(~id) +
  mytheme +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.text = element_text(angle = 0, size = 9)
  )


figpathout <- "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/"
svglite::svglite(file = paste0(figpathout, "supp-all-Kanyawara-series.svg"), width = 11, height = 8)
plotall
graphics.off()



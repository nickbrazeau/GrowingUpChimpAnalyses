mytheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", hjust = 0.5, size = 10),
        axis.title = element_text(family = "Helvetica", face = "bold", hjust = 0.5, size = 8),
        axis.text = element_text(family = "Helvetica", hjust = 0.5, size = 7),
        legend.position = "right",
        legend.title = element_text(family = "Helvetica", face = "bold", vjust = 0.85, size = 8),
        legend.text = element_text(family = "Helvetica", hjust = 0.5, vjust = 0.5, angle = 90, size = 7),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        panel.border = element_blank())





make_FO_plot <- function(lowerCIpredsFO, medianCIpredsFO, upperCIpredsFO){

  FO.CIbind <- dplyr::left_join(lowerCIpredsFO, upperCIpredsFO, by = "age")

  FO.plot <- ggplot() +
    geom_smooth(data = medianCIpredsFO, aes(x = age, y = outcome), color = "#08306b") +
#    geom_ribbon(data = FO.CIbind, aes(x = age, ymin = outcome.y, ymax = outcome.x), color = NA, fill = "#c6dbef", alpha = 0.4) +
    xlab("Age (years)") + ylab("Growth (cm)") +
    mytheme

  return(FO.plot)
}


make_SO_plot <-  function(lowerCIpredsSO,
                          medianCIpredsSO,
                          upperCIpredsSO){
  SO.CIbind <- dplyr::left_join(lowerCIpredsSO, upperCIpredsSO, by = "age")
  SO.plot <- ggplot() +
    geom_line(data  = medianCIpredsSO, aes(x = age, y = outcome), color = "#08306b") +
#    geom_ribbon(data = SO.CIbind, aes(x = age, ymin = outcome.x, ymax = outcome.y), color = NA, fill = "#c6dbef", alpha = 0.4) +
    xlab("Age (years)") + ylab("Growth (cm/year)") +
    mytheme

  return(SO.plot)
}


make_fullplot <- function(FOplots, SOplots){
  fullplot <- ggdraw() +
    draw_plot(SOplots) +
    draw_plot(FOplots, x = 0.6, y = .7, width = .4, height = .2)

  return(fullplot)

}



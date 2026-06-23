library(ggplot2)

save_plot <- function(plot, filename,
                      width = 8, height = 5) {
  ggsave(
    paste0("graphics/", filename, ".png"),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}
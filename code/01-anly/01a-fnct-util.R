# Utility Functions -------------------------------------------------------

#' Export Graphical Object as PDF
#'
#' @param .x Graphical object
#' @param .path Filesystem destination to save object.
#' @param .width Figure width in cm.
#' @param .height Figure height in cm.
#'
#' @return PDF output to disk.
ExportPDF <- function (.x, .path, .width, .height) {

  # initialize empty pdf file with given dimensions
  # 0.4 is the rough conversion factor cm to inch
  pdf(.path, width = 0.4*.width, height = 0.4*.height,
      useDingbats = FALSE) # avoid problems with missing fonts

  # draw an empty canvas
  grid.newpage()
  # specify an area to draw on the canvas
  vp <- viewport(
    x = 0.5, y = 0.5,
    width  = unit(.width, "cm"),
    height = unit(.height, "cm")
  )
  pushViewport(vp)

  # print graphical object to pdf
  print(.x, vp = vp)
  # close the pdf device
  dev.off()
}

# Generate Useful y-Scale Breaks and Labels Containing the Extremes
GenerateYScale <- function (y_data, y_break_mid, sparse = TRUE) {

  y_max <- max(y_data, na.rm = TRUE)
  y_min <- min(y_data, na.rm = TRUE)

  y_break <- c(y_min, y_break_mid, y_max)

  y_lab_mid <- format(y_break_mid,
                      scientific = FALSE,
                      drop0trailing = TRUE)

  if (sparse == TRUE) {
    y_lab_mid  <- ifelse(grepl(y_lab_mid, pattern = "[15]$"),
                         y_lab_mid,
                         "")
  }

  y_lab <- c(
    format(y_min, scientific = TRUE, digits = 3),
    y_lab_mid,
    format(y_max, scientific = TRUE, digits = 3)
  )
  y_lab <- ifelse(grepl("^0", y_lab),
                  substring(y_lab, 2),
                  y_lab)

  return(
    data.frame(breaks = y_break, labels = y_lab)
  )

}

# Generate Useful Scale Breaks and Labels
GenerateScale <- function (breaks, sparse = TRUE) {

  breaks_format <- format(breaks,
                          scientific = FALSE,
                          drop0trailing = TRUE)

  if (sparse == TRUE) {
    breaks_format  <- ifelse(grepl(breaks_format, pattern = "[15]$"),
                             breaks_format,
                             "")
  }

  breaks_format <- ifelse(grepl("^0.", breaks_format),
                          substring(breaks_format, 2),
                          breaks_format)

  return(
    data.frame(breaks = breaks, labels = breaks_format)
  )

}

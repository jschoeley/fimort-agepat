# Utility Functions -------------------------------------------------------

#' Export Graphical Object as PDF
#'
#' @param x      graphical object
#' @param path   filesystem destination to save object
#' @param width  figure width in cm
#' @param height figure height in cm
#'
#' @return PDF output to disk.
ExportPDF <- function (x, path, width, height) {

  # initialize empty pdf file with given dimensions
  # 0.4 is the rough conversion factor cm to inch
  pdf(path, width = 0.4*width, height = 0.4*height,
      useDingbats = FALSE) # avoid problems with missing fonts

  # draw an empty canvas
  grid.newpage()
  # specify an area to draw on the canvas
  vp <- viewport(
    x = 0.5, y = 0.5,
    width  = unit(width, "cm"),
    height = unit(height, "cm")
  )
  pushViewport(vp)

  # print graphical object to pdf
  print(x, vp = vp)
  # close the pdf device
  dev.off()
}

#' Strip Leading Zero Integer
#'
#' @param breaks   a numeric vector of scale breaks
#' @param integer0 should 0.0 become 0?
#' @param ...      arguments to \code{\link{format}}
#'
#' @return A character vector of formatted labels.
StripLeadingZeroInteger <- function (breaks, integer0 = TRUE, ...) {

  # ensure non scientific format
  breaks_format <- format(breaks, scientific = FALSE, ...)

  # replace decimal break 0 with integer 0
  if (integer0 == TRUE) {
    breaks_format <-
      ifelse(grepl("0\\.0+$", breaks_format),
             "0",
             breaks_format)
  }

  # remove leading 0 integer part
  breaks_format <- ifelse(grepl("^0\\.", breaks_format),
                          substring(breaks_format, 2),
                          breaks_format)

  # return formatted breaks
  return(breaks_format)

}

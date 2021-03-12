# Simple function to export mapview viz to share with collaborators more easily
# To build better, not time now
exportMapview <- function(object, output) {
  mapviewOptions(fgb = FALSE)
  dat <- object %>%
         mapview()

  mapshot(dat, url = output)

  dat
}

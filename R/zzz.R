# Internals
#' @importFrom exactextractr exact_extract
#' @importFrom fs path path_package
#' @importFrom glue glue glue_sql
#' @importFrom raster getData
#' @importFrom whisker whisker.render
#' @importFrom yaml yaml.load_file write_yaml read_yaml
NULL

# ------------------------------------------------------------------------------
# Gracieuseté de Kevin Cazelles: https://github.com/KevCaz
# my simple(r) version of use template
use_template <- function(template, save_as = stdout(), pkg = "ceanav", ...) {
  template <- readLines(
    path_package(package = pkg, template)
  )
  # NB by default whisker forward the parent envi and I used this
  writeLines(whisker::whisker.render(template, ...), save_as)
}

# ------------------------------------------------------------------------------
# Création d'un hyperlien en format markdown à partir de deux vecteurs
rep_hyperlien <- function(texte, url) {
  nl <- length(texte)
  hyperlien <- character(nl)

  for(i in 1:nl) {
    if(!is.na(url[i])) {
      hyperlien[i] <- paste0("[",texte[i],"](",url[i],")")
    } else {
      hyperlien[i] <- texte[i]
    }
  }

  # Return
  hyperlien
}



# ------------------------------------------------------------------------------
# Clean global environment
clean <- function() {
  objs <- ls(envir = globalenv())
  rm(list = objs, pos = ".GlobalEnv")
}

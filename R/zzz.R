# Internals
#' @importFrom exactextractr exact_extract
#' @importFrom fs path path_package
#' @importFrom glue glue glue_sql
#' @importFrom hexSticker sticker
# @importFrom kableExtra kable_styling row_spec column_spec
#' @importFrom latex2exp TeX
#' @importFrom magick image_read image_append image_write
#' @importFrom raster getData
#' @importFrom units set_units
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
    if(!is.null(url[i])) {
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


# ------------------------------------------------------------------------------
# Scale between 0 and 1 using the 99th quantile
quantNorm <- function(x) {
  id <- x != 0
  x <- x / quantile(x[id], probs = .99, na.rm = T)
  x[x > 1] <- 1
  x[x < 0] <- 0
  x
}


# ------------------------------------------------------------------------------
# Remove cells that do not intersect water
removeCoast <- function(dat) {
  # -----
  data(aoi)
  data(grid1p)

  # -----
  uid <- st_intersects(aoi, dat) %>% unlist()
  nid <- !1:nrow(dat) %in% uid

  # -----
  dat <- st_drop_geometry(dat)
  for(i in 1:ncol(dat)) dat[nid, i] <- NA

  # -----
  dat <- cbind(grid1p, dat)

  # -----
  dat
}


# ------------------------------------------------------------------------------
# For proper referencing in markdown syntax
modif_md <- function(dat) {
  dat <- gsub("_", "", dat)
  dat <- gsub("\\.", "", dat)
}


# ------------------------------------------------------------------------------
# To acknowledge data sources for each sections 
show_source <- function(dat, lang = "fr") {
  nid <- length(dat)
  nm <- paste(dat, collapse = ", ")
  if (nid == 1) {
    if (lang == "fr") {
      glue("> *L'identifiant unique attribué aux données présentées au sein de cette section est :* ***{nm}***. *Consultez la section suivante et l'[annexe 1](#annexe1) pour plus de détails sur ces données.*")
    } else if (lang == "en") {
      glue("> *The unique identifier given to the data presented in this section is:* ***{nm}***. *Refer to the following section and to [Appendix 1](#annexe1) for more details on these data.*")
    }
  } else {
    if (lang == "fr") {
      glue("> *Les identifiants uniques attribués aux données présentées au sein de cette section sont :* ***{nm}***. *Consultez la section suivante et l'[annexe 1](#annexe1) pour plus de détails sur ces données.*")
    } else if (lang == "en") {
      glue("> *The unique identifiers given to the data presented in this section are:* ***{nm}***. *Refer to the following section and to [Appendix 1](#annexe1) for more details on these data.*")
    }
  }
}
#' Function to evaluate the distribution of observations for marine mammals
#'
#' The function evaluates the frequency of marine mammal observations as in data0027
#'
#' @param dat observation points for marine mammals as sf object
#' @param studygrid regular grid as sf object
#'
#' @keywords mammifères marins
#'
#' @export
#'

observation_distribution <- function(dat, studygrid) {

  # -----
  nm <- unique(dat$Taxon)
  nm2 <- tolower(nm) %>% gsub(" ", "_", .)

  # -----
  for(i in 1:length(nm)) {
    uid <- data0054$Taxon == nm[i]
    dat <- data0054[uid, ]

    # 1. Nombre d'individus observés par cellule de la grille
    dat <- st_intersects(studygrid, dat) %>%
           sapply(function(x) sum(dat$Individual.Count[x])) %>%
           mutate(studygrid, frequence = .)

    # 2. Normalisation par le nombre total d'observation
    dat$normalisation <- dat$frequence / max(dat$frequence)

    # 3. Lissage Gaussien de 0.2o
    dat$lissage <- lissage(dat[,"normalisation"])

    # 4. Transformation logarithmique
    dat$log <- log(dat$lissage + 1)

    # 5. Select and rename
    dat <- select(dat, !!nm2[i] := log) %>%
           st_drop_geometry()

    # -----
    studygrid <- cbind(studygrid, dat)
  }

  # ----
  studygrid
}

# #' Cumulative effects
# #'
# #' Function to evaluate cumulative effects of stressors on valued components
# #'
# #' @param stress matrix with stressors intensity
# #' @param valued matrix with presence or amount of valued components
# #' @param vulnerability matrix with vulnerability of valued components to stressors. Names used in stress and valued matrices must be the same as that found in vulnerability matrix
# #'
# #' @keywords cumulative effects
# #'
# #' @export
# #'
# #' @details
# #'
#
# cumulativeEffects <- function(stress, valued, vulnerability) {
#
#   nS <- 10
#   nV <- 12
#   nR <- 50
#   stress <- matrix(nrow = nR, ncol = nS, dimnames = list(c(), paste0("stress", 1:nS)), data = runif(nR*nS, 0, 1))
#   valued <- matrix(nrow = nR, ncol = nV, dimnames = list(c(), paste0("valued", 1:nV)), data = runif(nR*nV, 0, 1))
#   vulnerability <- matrix(nrow = nV, ncol = nS, data = runif(nV*nS, 0, 1), dimnames = list(colnames(valued), colnames(stress)))
#
#   # -----
#   st <- colnames(stress)
#   cv <- colnames(valued)
#
#   # -----
#   nst <- ncol(stress)
#   ncv <- ncol(valued)
#
#
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   # Risque associé aux effets des stresseurs en considérant uniquement la
#   # vulnérabilité et l'intensité des stresseurs.
#   # Donc : quel est le risque pour une composante valorisée si elle se retrouve
#   #        au sein d'un milieu X
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   # Evaluate risk
#   risk <- list()
#   system.time({
#     for(i in 1:nrow(stress)) {
#       risk[[i]] <- sweep(vulnerability,
#                          MARGIN = 2,
#                          stress[i,],
#                          `*`)
#     }
#   })
#
#   intensityCumulative <- rowSums(intensity)
#
#   #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
#   # Risk as matrix
#   #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
#   # As matrix
#   dRisk <- matrix(0, nrow = length(risk), ncol = nrow(risk[[1]][]),
#                   dimnames = list(c(), rownames(risk[[1]])))
#   for(i in 1:length(risk)) dRisk[i, ] <- risk[[i]][,1]
#
#
#   #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
#   # Risk rasters
#   #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
#   risk <- list()
#   for(i in 1:ncol(dRisk)) {
#     dat <- r
#     values(dat)[idBiotic] <- dRisk[,i]
#     risk[[i]] <- dat
#   }
#   names(risk) <- colnames(dRisk)
#
#
#
#
#       IndividualRisk(drivers = dr[i, ],
#                                             vulnerability = vulnerability,
#                                      sensitivity = sensitivity_dix)
#     }
#   })
#
#
#   # -----
#   intensity <-
#
#
#
# }

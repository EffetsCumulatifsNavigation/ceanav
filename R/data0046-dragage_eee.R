#' Data 0046 : Dragage
#'
#' Dragage d'entretien annuel de la voie navigable du Saint-Laurent
#'
#' @keywords dragage
#' @keywords stresseurs
#'
#' @source
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0046 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  #
  # _________________________________________________________________________ #

  # Output folder
  output <- "data0046-dragage_eee/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Identifier les tronçons de la voie navigable et numériser base de données
  # ----------------------------------------
  # Importer données de la GCC
  load_format("data0018")
  data0018 <- data0018[!is.na(data0018$basename), ]

  # -----
  annee <- 1985:2018

  # -----
  # Montréal à Contrecoeur
  nm <- c("C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11",
          "C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22",
          "C23","C24","C26","C28","C29","C30")
  uid <- unlist(lapply(nm, FUN = function(x) which(stringr::str_detect(data0018$name, x))))
  mtcc <- data0018[uid, ] %>%
          st_buffer(0.5) %>%
          st_union() %>%
          st_sf(name = "Montréal à Contrecoeur",
                annee = annee,
                volume = 0)
  mtcc$volume[mtcc$annee == 1999] <- 51496


  # -----
  # Sorel
  nm <- c("C25","C27","C31","C32","C33","D01 \\(AM\\)")
  uid <- unlist(lapply(nm, FUN = function(x) which(stringr::str_detect(data0018$name, x))))
  sorel <- data0018[uid, ] %>%
           st_buffer(0.5) %>%
           st_union() %>%
           st_sf(name = "Sorel",
                 annee = annee,
                 volume = c(
                   2600,1910,0,30029,0,0,1156,4881,4288,2839,451,
                   1372,1141,3690,0,0,0,0,0,880,0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0
                 ))


  # -----
  # Lac Saint-Pierre
  nm <- c("D01 \\(CEN\\)","D01 \\(AV\\)","D02","D03","D04","D05","D06",
          "D07","D08","D09","D10","D11","D12","D13","D14","D15","D16")#,
          # "D17","D18","D19","D20","D21","D22","D23","D24","D25","D26")
  uid <- unlist(lapply(nm, FUN = function(x) which(stringr::str_detect(data0018$name, x))))
  lsp <- data0018[uid, ] %>%
         st_buffer(0.5) %>%
         st_union() %>%
         st_sf(name = "Lac Saint-Pierre",
               annee = annee,
               volume = c(
                 113111,1800,12333,54270,17951,41807,9018,53276,18990,
                 34356,33012,4036,5000,63123,20253,12319,23997,12397,
                 18150,15219,13536,10210,21563,16751,11401,0,5315,0,
                 5832,0,0,13098,0,3970
               ))

  # -----
  # Bécancour à Batiscan
  nm <- c("E05","E06","E07","E08","E09","E10","E11","E12","E20")
  uid <- unlist(lapply(nm, FUN = function(x) which(stringr::str_detect(data0018$name, x))))
  beba <- data0018[uid, ] %>%
          st_buffer(0.5) %>%
          st_union() %>%
          st_sf(name = "Bécancour à Batiscan", annee = 2012:2018,
                volume = c(
                  21026,20926,18424,34767,22354,18447,1784
                ))

  # -----
  # Trois-Rivière à Bécancour
  nm <- c("E01","E02","E03","E04","E05","E06")
  uid <- unlist(lapply(nm, FUN = function(x) which(stringr::str_detect(data0018$name, x))))
  trbe <- data0018[uid, ] %>%
          st_buffer(0.5) %>%
          st_union() %>%
          st_sf(name = "Trois-Rivière à Bécancour",
                annee = 1985:2011,
                volume = c(
                  500,2200,25709,14334,12736,19723,16610,31456,
                  20612,15293,19430,21159,27779,23162,61846,40427,
                  30298,40442,28857,33509,23893,23060,19670,37963,
                  19633,13179,14316
                ))

  # -----
  # Champlain à Deschaillons
  nm <- c("E08","E09","E10","E11","E12","E13","E14","E15","E19")
  uid <- unlist(lapply(nm, FUN = function(x) which(stringr::str_detect(data0018$name, x))))
  cade <- data0018[uid, ] %>%
          st_buffer(0.5) %>%
          st_union() %>%
          st_sf(name = "Champlain à Deschaillons",
                annee = 1985:2011,
                volume = c(
                  0,640,240,0,12329,0,0,2442,2421,979,1251,0,213,
                  0,11410,2057,2758,9478,6988,9585,2698,4255,3155,
                  1386,2286,5511,5184
                ))

  # -----
  # Traverse Cap Santé
  nm <- c("F25","F26","F27")
  uid <- unlist(lapply(nm, FUN = function(x) which(stringr::str_detect(data0018$name, x))))
  sante <- data0018[uid, ] %>%
          st_buffer(0.5) %>%
          st_union() %>%
          st_sf(name = "Traverse Cap Santé",
                annee = annee,
                volume = c(
                  13159,14098,6927,5518,9304,7035,6742,8013,8580,6919,
                  6155,0,3560,0,0,0,0,0,0,0,1000,7013,3104,4648,2500,
                  3000,2200,2312,2293,2845,2435,3409,1954,2409
                ))

  # -----
  # Traverse Nord I.O.
  uid <- stringr::str_detect(data0018$name, "G")
  tnio <- data0018[uid, ] %>%
          st_buffer(0.5) %>%
          st_union() %>%
          st_sf(name = "Traverse Nord I.O.",
                annee = annee,
                volume = c(
                  96634,118989,83919,73591,75624,57342,74877,71746,
                  66147,65348,81637,65907,78331,75179,87709,84662,
                  86969,67784,76470,62758,64433,59703,57652,59495,
                  49616,53032,55040,52694,53627,55945,51484,51162,
                  50461,32894
                ))
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Sites de dépôt
  # ----------------------------------------
  # -----
  # X-02
  #   Traverse Nord I.O.
  #   Maximum 10000 m^3
  X02 <- tnio$volume
  X02 <- ifelse(X02 > 10000, 10000, X02)
  X02 <- data0018[data0018$name == "X02",] %>%
         st_geometry() %>%
         st_sf(name = "X-02", annee = tnio$annee, volume = X02)

  # -----
  # X-03
  #   Traverse Nord I.O.
  #   Restant du volume non déposé à X-02
  X03 <- tnio$volume - 10000
  X03 <- ifelse(X03 < 0, 0, X03)
  X03 <- data0018[data0018$name == "X03",] %>%
         st_geometry() %>%
         st_sf(name = "X-03", annee = tnio$annee, volume = X03)

  # -----
  # X-04
  #   Traverse Cap Santé
  #   Traverse Cap-Santé à Saint-Antoine
  X04 <- data0018[data0018$name == "X04",] %>%
         st_geometry() %>%
         st_sf(name = "X-04", annee = sante$annee, volume = sante$volume)


  # -----
  # S-17
  #   Lac Saint-Pierre
  S17 <- data0018[data0018$name == "S17",] %>%
         st_geometry() %>%
         st_sf(name = "S-17", annee = lsp$annee, volume = lsp$volume)

  # -----
  # M02
  #   Montréal à Contrecoeur
  M02 <- data0018[data0018$name == "M02",] %>%
         st_geometry() %>%
         st_sf(name = "M-02", annee = mtcc$annee, volume = (mtcc$volume)/2)

  # -----
  # M27
  #   Montréal à Contrecoeur
  #   Sorel
  M27 <- (mtcc$volume)/2 + sorel$volume
  M27 <- data0018[data0018$name == "M27",] %>%
         st_geometry() %>%
         st_sf(name = "M-27", annee = mtcc$annee, volume = M27)

  # -----
  # T02
  #   Trois-Rivière à Bécancour
  T02 <- data0018[data0018$name == "T02",] %>%
         st_geometry() %>%
         st_sf(name = "T-02", annee = trbe$annee, volume = (trbe$volume)/2)

  # -----
  # T06
  #   Trois-Rivière à Bécancour
  T06 <- data0018[data0018$name == "T06",] %>%
         st_geometry() %>%
         st_sf(name = "T-06", annee = trbe$annee, volume = (trbe$volume)/2)

  # -----
  # T16
  #   Champlain à Deschaillons
  T16 <- data0018[data0018$name == "T16",] %>%
         st_geometry() %>%
         st_sf(name = "T-16", annee = cade$annee, volume = (cade$volume)/2)

  # -----
  # T11
  #   Champlain à Deschaillons
  #   Bécancour à Batiscan
  v1 <- data.frame(annee = cade$annee, volume = (cade$volume)/2)
  v2 <- data.frame(annee = beba$annee, volume = beba$volume)
  T11 <-  rbind(v1,v2) # NOTE: no need for left join ,they're consecutive years
  T11 <- data0018[data0018$name == "T11",] %>%
         st_geometry() %>%
         st_sf(name = "T-11", annee = T11$annee, volume = T11$volume)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Bind objects
  # ----------------------------------------
  # -----
  dragage <- bind_rows(mtcc,sorel,lsp,beba,trbe,cade,sante,tnio) %>%
             mutate(type = "dragage")

  # -----
  depot <- bind_rows(X02,X03,X04,S17,M02,M27,T02,T06,T16,T11) %>%
           mutate(type = "depot")

  # -----
  data0046 <- bind_rows(dragage, depot) %>%
              st_transform(crs = global_parameters()$crs)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0046,
           dsn = "./data/data-format/data0046-dragage_eee.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}

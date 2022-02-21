#' Data 0070 : Sites d'ancrage commerciaux 
#'
#' Localisation des sites d'ancrage commerciaux provenant des fiches techniques de mouillage entre Les Escoumins et Montréal
#'
#' @keywords ancrage
#'
#' @source Garde côtière canadienne (2022) Fiches techniques de mouillage - Les Escoumins à Montréal.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0070 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0070-ancrage_gcc/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # _________________________________________________________________________ #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  
  # Points -----
  dd <- function(d,m,s) d + (m/60) + (s/3600)
  dd2 <- function(d,m) d + (m/60)
  pts <- list()
  
  # Latitude 45  33’ 25.4’’ N Longitude 073  30’ 46.5’’ W Longueuil (Vickers) (APM)
  pts[[1]] <- c(lat = dd(45,33,25.4), long = -dd(73,30,46.5), nom = "Longueuil (Vickers) (APM)")
  
  # Latitude 45  36’ 47.4’’ N Longitude 073  29’ 50.0’’ W Montr al-Est (APM)
  pts[[2]] <- c(lat = dd(45,36,47.4), long = -dd(73,29,50.0), nom = "Montr al-Est (APM)")
  
  # Latitude 45  37’ 56.7’’ N Longitude 073  29’ 02.9’’W Pointe-aux-Trembles (APM) 1
  pts[[3]] <- c(lat = dd(45,37,56.7), long = -dd(73,29,02.9), nom = "Pointe-aux-Trembles (APM) 1")
  
  # Latitude 45  38’ 10.3’’ N Longitude 073  28’ 53.8’’ W Pointe-aux-Trembles (APM) 2
  pts[[4]] <- c(lat = dd(45,38,10.3), long = -dd(073,28,53.8), nom = "Pointe-aux-Trembles (APM) 2")
  
  # Latitude 45  38’ 22.2’’ N Longitude 073  28’ 45.8’’ W Pointe-aux-Trembles (APM) 3
  pts[[5]] <- c(lat = dd(45,38,22.2), long = -dd(73,28,45.8), nom = "Pointe-aux-Trembles (APM) 3")
  
  # Latitude 45  38’ 35.6’’ N Longitude 073  28’ 37.2’’ W Pointe-aux-Trembles (APM) 4
  pts[[6]] <- c(lat = dd(45,38,35.6), long = -dd(73,28,37.2), nom = "Pointe-aux-Trembles (APM) 4")
  
  # Latitude 46  00’ 10’’ N Longitude 073  10’ 25’’ W Lanoraie L1 (APM) (FG)
  pts[[7]] <- c(lat = dd(46,00,10), long = -dd(73,10,25), nom = "Lanoraie L1 (APM) (FG)")
  
  # Latitude 46  00’ 24.0’’ N Longitude 073  10’ 20.0’’ W Lanoraie L2 (APM) (FG)
  pts[[8]] <- c(lat = dd(46,00,24.0), long = -dd(73,10,20.0), nom = "Lanoraie L2 (APM) (FG)")
  
  # Latitude 46  00’ 39.0’’ N Longitude 073  10’ 14.0’’ W Lanoraie L3 (APM) (FG)
  pts[[9]] <- c(lat = dd(46,00,39.0), long = -dd(73,10,14.0), nom = "Lanoraie L3 (APM) (FG)")
  
  # Latitude 46  00’ 53.0’’ N Longitude 073  10’ 08.0’’ W Lanoraie L4 (APM) (FG)
  pts[[10]] <- c(lat = dd(46,00,53.0), long = -dd(73,10,08.0), nom = "Lanoraie L4 (APM) (FG)")
  
  # Latitude 46  01’ 08,0’’ N Longitude 073  10’ 03.0’’ W Lanoraie L5
  pts[[11]] <- c(lat = dd(46,01,08.0), long = -dd(73,10,03.0), nom = "Lanoraie L5")
  
  # Latitude 46  01’ 23,0’’ N Longitude 073  09’ 57.0’’ W Lanoraie L6
  pts[[12]] <- c(lat = dd(46,01,23.0), long = -dd(73,09,57.0), nom = "Lanoraie L6")
  
  # Latitude 46  01’ 37,0’’ N Longitude 073  09’ 52.0’’ W Lanoraie L7
  pts[[13]] <- c(lat = dd(46,01,37.0), long = -dd(73,09,52.0), nom = "Lanoraie L7")
  
  # Latitude 46  01’ 52,0’’ N Longitude 073  09’ 46,0’’ W Lanoraie L8
  pts[[14]] <- c(lat = dd(46,01,52.0), long = -dd(73,09,46.0), nom = "Lanoraie L8")
  
  # Latitude 46  02’ 06,0’’ N Longitude 073  09’ 41,0’’ W Lanoraie L9
  pts[[15]] <- c(lat = dd(46,02,06.0), long = -dd(73,09,41.0), nom = "Lanoraie L9")
  
  # Latitude 46  03' 05"N Longitude : 73  08' 31"W Sorel T1
  pts[[16]] <- c(lat = dd(46,03,05), long = -dd(73,08,31), nom = "Sorel T1")
  
  # Latitude 46  03' 09"N Longitude 73  08' 08"W Sorel T2
  pts[[17]] <- c(lat = dd(46,03,09), long = -dd(73,08,08), nom = "Sorel T2")
  
  # Latitude 46  03' 15"N Longitude 73  07' 48"W Sorel T3
  pts[[18]] <- c(lat = dd(46,03,15), long = -dd(73,07,48), nom = "Sorel T3")
  
  # Latitude 46  03' 18"N Longitude 73  07' 28"W Sorel T4
  pts[[19]] <- c(lat = dd(46,03,18), long = -dd(73,07,28), nom = "Sorel T4")
  
  # Latitude 46  03' 004" N Longitude : 73  07' 30" W Sorel P1
  pts[[20]] <- c(lat = dd(46,03,004), long = -dd(73,07,30), nom = "Sorel P1")
  
  # Latitude 46  03' 27" N Longitude 73  06' 51" W Sorel S1
  pts[[21]] <- c(lat = dd(46,03,27), long = -dd(73,06,51), nom = "Sorel S1")
  
  # Latitude 46,03' 32" N Longitude 73  06' 30" W Sorel S2
  pts[[22]] <- c(lat = dd(46,03,32), long = -dd(73,06,30), nom = "Sorel S2")
  
  # Latitude 46  03' 35" N Longitude 73  06' 09" W Sorel S3
  pts[[23]] <- c(lat = dd(46,03,35), long = -dd(73,06,09), nom = "Sorel S3")
  
  # Latitude 46  03' 38" N Longitude 73  05' 48" W Sorel S4
  pts[[24]] <- c(lat = dd(46,03,38), long = -dd(73,05,48), nom = "Sorel S4")
  
  # Latitude 46  03' 15" N Longitude 73  06' 29" W Sorel S5
  pts[[25]] <- c(lat = dd(46,03,15), long = -dd(73,06,29), nom = "Sorel S5")
  
  # Latitude 46  03' 20" N Longitude 73  06' 06" W Sorel S6
  pts[[26]] <- c(lat = dd(46,03,20), long = -dd(73,06,06), nom = "Sorel S6")
  
  # Latitude 46  16' 57" N Longitude 72  36' 57" W Ile aux Sternes
  pts[[27]] <- c(lat = dd(46,16,57), long = -dd(72,36,57), nom = "Ile aux Sternes")
  
  # Latitude 46  17' 20" N Longitude 72  35' 14" W Pointe-aux-Ormes 1 (Sud) (APTR) (FG)
  pts[[28]] <- c(lat = dd(46,17,20), long = -dd(72,35,14), nom = "Pointe-aux-Ormes 1 (Sud) (APTR) (FG)")
  
  # Latitude 46  17' 30" N Longitude 72  34' 54" W Pointe-aux-Ormes 2 (Sud) (APTR) (FG)
  pts[[29]] <- c(lat = dd(46,17,30), long = -dd(72,34,54), nom = "Pointe-aux-Ormes 2 (Sud) (APTR) (FG)")
  
  # Latitude 46  17' 40" N Longitude 72  34' 34" W Pointe-aux-Ormes 3 (Sud) (APTR) (FG)
  pts[[30]] <- c(lat = dd(46,17,40), long = -dd(72,34,34), nom = "Pointe-aux-Ormes 3 (Sud) (APTR) (FG)")
  
  # Latitude 46  17' 50" N Longitude 72  34' 13" W Pointe-aux-Ormes 4 (Sud) (APTR)
  pts[[31]] <- c(lat = dd(46,17,50), long = -dd(72,34,13), nom = "Pointe-aux-Ormes 4 (Sud) (APTR)")
  
  # Latitude 46  17' 36" N Longitude 72  35' 21" W Pointe-aux-Ormes 5 (Nord) (APTR)
  pts[[32]] <- c(lat = dd(46,17,36), long = -dd(72,35,21), nom = "Pointe-aux-Ormes 5 (Nord) (APTR)")
  
  # Latitude 46  17' 47" N Longitude 72  35' 05" W Pointe-aux-Ormes 6 (Nord) (APTR)
  pts[[33]] <- c(lat = dd(46,17,47), long = -dd(72,35,05), nom = "Pointe-aux-Ormes 6 (Nord) (APTR)")
  
  # TR1 Latitude 46  19' 59"N Longitude 72  32' 21"W Trois-Rivières ville (APTR) (FG) TR1
  pts[[34]] <- c(lat = dd(46,19,59), long = -dd(72,32,21), nom = "Trois-Rivières ville (APTR) (FG) TR1")
  
  # TR2 Latitude 46  20' 14"N Longitude 72  32' 15"W Trois-Rivières ville (APTR) (FG) TR2
  pts[[35]] <- c(lat = dd(46,20,14), long = -dd(72,32,15), nom = "Trois-Rivières ville (APTR) (FG) TR2")
  
  # TR3 Latitude 46  20' 15"N Longitude 72  31' 54"W Trois-Rivières ville (APTR) (FG) TR3
  pts[[36]] <- c(lat = dd(46,20,15), long = -dd(72,31,54), nom = "Trois-Rivières ville (APTR) (FG) TR3")
  
  # TR4 Latitude 46  20' 30"N Longitude 72  31' 52"W Trois-Rivières ville (APTR) (FG) TR4
  pts[[37]] <- c(lat = dd(46,20,30), long = -dd(72,31,52), nom = "Trois-Rivières ville (APTR) (FG) TR4")
  
  # TR5 Latitude 46  20' 35"N Longitude 72  31' 30"W Trois-Rivières ville (APTR) (FG) TR5
  pts[[38]] <- c(lat = dd(46,20,35), long = -dd(72,31,30), nom = "Trois-Rivières ville (APTR) (FG) TR5")
  
  # Latitude 46  30’ 05’’ N Longitude 72  14’01’’W Batiscan Centre de la zone
  pts[[39]] <- c(lat = dd(46,30,05), long = -dd(72,14,01), nom = "Batiscan Centre de la zone")
  
  # Latitude 46  35’04’’ N Longitude 72  00’54’’ W Grondines Sud
  pts[[40]] <- c(lat = dd(46,35,04), long = -dd(72,00,54), nom = "Grondines Sud")
  
  # Latitude 46  34’52’’ N Longitude 72  01’58’’ W Grondines Nord
  pts[[41]] <- c(lat = dd(46,34,52), long = -dd(72,01,58), nom = "Grondines Nord")
  
  # Latitude 46  40' 35"N Longitude 71  50' 28"W Pointe-Platon 1
  pts[[42]] <- c(lat = dd(46,40,35), long = -dd(71,50,28), nom = "Pointe-Platon 1")
  
  # Latitude 46  40' 22"N Longitude 71  49' 55"W Pointe-Platon 2
  pts[[43]] <- c(lat = dd(46,40,22), long = -dd(71,49,55), nom = "Pointe-Platon 2")
  
  # Latitude 46  41' 59"N Longitude 71  31' 42"W Les Ilets Dombourg 1
  pts[[44]] <- c(lat = dd(46,41,59), long = -dd(71,31,42), nom = "Les Ilets Dombourg 1")
  
  # Latitude 46  42' 08"N Longitude 71  31' 13"W Les Ilets Dombourg 2
  pts[[45]] <- c(lat = dd(46,42,08), long = -dd(71,31,13), nom = "Les Ilets Dombourg 2")
  
  # Latitude 46  43' 39" N Longitude : 71  22' 03" W Pointe Deschambault (Pointe Conf d ration)
  pts[[46]] <- c(lat = dd(46,43,39), long = -dd(71,22,03), nom = "Pointe Deschambault (Pointe Conf d ration)")
  
  # Latitude 46  43' 00"N Longitude 71  22' 39"W Saint-Nicolas (FG)
  pts[[47]] <- c(lat = dd(46,43,00), long = -dd(71,22,39), nom = "Saint-Nicolas (FG)")
  
  # Lat. 46  48' 16"N Long. 71  11' 40"W Québec Alpha (A) (APQ)
  pts[[48]] <- c(lat = dd(46,48,16), long = -dd(71,11,40), nom = "Québec Alpha (A) (APQ)")
  
  # Lat. 46  47' 58"N Long. 71  11' 48"W Québec Bravo (B) (APQ)
  pts[[49]] <- c(lat = dd(46,47,58), long = -dd(71,11,48), nom = "Québec Bravo (B) (APQ)")
  
  # Lat. 46  47' 42"N Long. 71  12' 00"W Québec Charlie (C) (APQ)
  pts[[50]] <- c(lat = dd(46,47,42), long = -dd(71,12,00), nom = "Québec Charlie (C) (APQ)")
  
  # Latitude 46  50' 52" N Longitude 71  09' 26" W Québec Delta (D) (APQ)
  pts[[51]] <- c(lat = dd(46,50,52), long = -dd(71,09,26), nom = "Québec Delta (D) (APQ)")
  
  # Latitude 46  52.407N Longitude 070  57.852W Rivière Maheu RM1 (APQ)
  pts[[52]] <- c(lat = dd2(46,52.407), long = -dd2(70,57.852), nom = "Rivière Maheu RM1 (APQ)")
  
  # Latitude 46  53.090N Longitude 070  56.734W Rivière Maheu RM2 (APQ)
  pts[[53]] <- c(lat = dd2(46,53.090), long = -dd2(70,56.734), nom = "Rivière Maheu RM2 (APQ)")
  
  # Latitude 46  53.687N Longitude 070 55.537 W Rivière Maheu RM3 (APQ)
  pts[[54]] <- c(lat = dd2(46,53.687), long = -dd2(70,55.537), nom = "Rivière Maheu RM3 (APQ)")
  
  # Latitude : 46  54' 50,17"N Longitude : 70  52' 12,72"W Pointe St-Jean
  pts[[55]] <- c(lat = dd(46,54,50), long = -dd(70,52,12.72), nom = "Pointe St-Jean")
  
  # Latitude 47  35,4’ N Longitude 70  08,9’ W Pointe au Pic 1
  pts[[56]] <- c(lat = dd2(47,35.4), long = -dd2(70,08.9), nom = "Pointe au Pic 1")
  
  # Latitude 47  36,3’ N Longitude 70  08,0’ W Pointe au Pic 2
  pts[[57]] <- c(lat = dd2(47,36.3), long = -dd2(70,08.0), nom = "Pointe au Pic 2")
  
  # Latitude 47  55’ 36’’ N Longitude 69  33’ 36’’ W Cacouna 1
  pts[[58]] <- c(lat = dd(47,55,36), long = -dd(69,33,36), nom = "Cacouna 1")
  
  # Latitude 47  55’ 00’’ N Longitude 69  37’ 18’’ W Cacouna 2
  pts[[59]] <- c(lat = dd(47,55,00), long = -dd(69,37,18), nom = "Cacouna 2")
  
  # Lat. 48 16.0 N Long. 069 11.1 W Les Rasades R1
  pts[[60]] <- c(lat = dd2(48,16.0), long = -dd2(69,11.1), nom = "Les Rasades R1")
  
  # Lat. 48 16.5 N Long. 069 10.0 W Les Rasades R2
  pts[[61]] <- c(lat = dd2(48,16.5), long = -dd2(69,10.0), nom = "Les Rasades R2")
  
  # Lat. 48 17.1 N Long. 069 08.7 W Les Rasades R3
  pts[[62]] <- c(lat = dd2(48,17.1), long = -dd2(69,08.7), nom = "Les Rasades R3")
  
  # Lat. 48 18.5 N Long. 069 06.2 W Les Rasades R4
  pts[[63]] <- c(lat = dd2(48,18.5), long = -dd2(69,06.2), nom = "Les Rasades R4")
  
  # Lat. 48 15.5 N Long. 069 10.0 W Les Rasades R5
  pts[[64]] <- c(lat = dd2(48,15.5), long = -dd2(69,10.0), nom = "Les Rasades R5")
  
  # Lat. 48 15.5 N Long. 069 08.7 W Les Rasades R6
  pts[[65]] <- c(lat = dd2(48,15.5), long = -dd2(69,08.7), nom = "Les Rasades R6")
  
  # Lat. 48 16.5 N Long. 069 07.6 W Les Rasades R7
  pts[[66]] <- c(lat = dd2(48,16.5), long = -dd2(69,07.6), nom = "Les Rasades R7")
  
  # Lat. 48 17.5 N Long. 069 06.8 W Les Rasades R8
  pts[[67]] <- c(lat = dd2(48,17.5), long = -dd2(69,06.8), nom = "Les Rasades R8")
  
  # Lat. 48 18.5 N Long. 069 04.8 W Les Rasades R9
  pts[[68]] <- c(lat = dd2(48,18.5), long = -dd2(69,04.8), nom = "Les Rasades R9")
  
  # Lat. 48 19.5 N Long. 069 03.5 W Les Rasades R10
  pts[[69]] <- c(lat = dd2(48,19.5), long = -dd2(69,03.5), nom = "Les Rasades R10")
  
  # Lat. 48 14.6 N Long. 069 07.9 W Les Rasades R11
  pts[[70]] <- c(lat = dd2(48,14.6), long = -dd2(69,07.9), nom = "Les Rasades R11")
  
  # Lat. 48 15.5 N Long. 069 07.0 W Les Rasades R12
  pts[[71]] <- c(lat = dd2(48,15.5), long = -dd2(69,07.0), nom = "Les Rasades R12")
  
  # Lat. 48 16.5 N Long. 069 05.9 W Les Rasades R13
  pts[[72]] <- c(lat = dd2(48,16.5), long = -dd2(69,05.9), nom = "Les Rasades R13")
  
  # Lat. 48 17.5 N Long. 069 05.1 W Les Rasades R14
  pts[[73]] <- c(lat = dd2(48,17.5), long = -dd2(69,05.1), nom = "Les Rasades R14")
  
  # Lat. 48 18.5 N Long. 069 03.1 W Les Rasades R15
  pts[[74]] <- c(lat = dd2(48,18.5), long = -dd2(69,03.1), nom = "Les Rasades R15")
  
  # Lat. 48 15.5 N Long. 069 05.4 W Les Rasades R16
  pts[[75]] <- c(lat = dd2(48,15.5), long = -dd2(69,05.4), nom = "Les Rasades R16")
  
  # Latitude 48  20,75’ N Longitude 070  52,05’ W Port-Alfred PA1 
  pts[[76]] <- c(lat = dd2(48,20.75), long = -dd2(70,52.05), nom = "Port-Alfred PA1 ")
  
  # Lat. 48  20,3’ N 070  50,0’ W Port-Alfred PA2
  pts[[77]] <- c(lat = dd2(48,20.3), long = -dd2(70,50.0), nom = "Port-Alfred PA2")
  
  # Latitude 48  25,47’ N Longitude 070  52,24’ W Grande-Anse 1 (APS)
  pts[[78]] <- c(lat = dd2(48,25.47), long = -dd2(70,52.24), nom = "Grande-Anse 1 (APS)")
  
  # Latitude 48  25,6’ N Longitude 070  52,1’ W Grande-Anse 2 (APS)
  pts[[79]] <- c(lat = dd2(48,25.6), long = -dd2(70,52.1), nom = "Grande-Anse 2 (APS)")
  
  # -----
  # Single object 
  pts <- bind_rows(pts) %>%
         st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
         st_transform(crs = global_parameters()$crs)
        
  
  # ------------------------------------------------
  # ------------------------------------------------
  # Polygons -----
  plg <- list()

  # Lanoraie A (APM)
  # 45  58’ 33.0’’ N 073  11’ 22.7’’ W (Coin N-W)
  # 45  58’ 30.7’’ N 073  11’ 16.5’’ W (Coin N-E)
  # 45  57’ 40.8’’ N 073  12’ 03.0’’ W (Coin S-W)
  # 45  57’ 38.6’’ N 073  11’ 57.1’’ W (Coin S-E)
  plg[[1]] <- rbind(c(-dd(73,11,22.7), dd(45,58,33.0)),
                    c(-dd(73,11,16.5), dd(45,58,30.7)),
                    c(-dd(73,11,57.1), dd(45,57,38.6)),
                    c(-dd(73,12,03.0), dd(45,57,40.8)),
                    c(-dd(73,11,22.7), dd(45,58,33.0)))

  # Lanoraie B (APM)
  # 45  59’ 05.9’’ N 073  11’ 21.8’’ W (Coin N-W)
  # 45  59’ 03.7’’ N 073  11’ 14.7’’ W (Coin N-E)
  # 45  58’ 35.3’’ N 073  11’ 42.1’’ W (Coin S-W)
  # 45  58’ 33.2’’ N 073  11’ 36.2’’ W (Coin S-E)
  plg[[2]] <- rbind(c(-dd(73,11,21.8), dd(45,59,05.9)),
                    c(-dd(73,11,14.7), dd(45,59,03.7)),
                    c(-dd(73,11,36.2), dd(45,58,33.2)),
                    c(-dd(73,11,42.1), dd(45,58,35.3)),
                    c(-dd(73,11,21.8), dd(45,59,05.9)))


  # Lanoraie C (APM)
  # 45  59’ 43.6’’ N 073  11’ 02.6’’ W (Coin N-W)
  # 45  59’ 42.3’’ N 073  10’ 57.1’’ W (Coin N-E)
  # 45  59’ 17.6’’ N 073  11’ 09.6’’ W (Coin S-E)
  # 45  59’ 19.0’’ N 073  11’ 15.3’’ W (Coin S-W)
  plg[[3]] <- rbind(c(-dd(73,11,02.6), dd(45,59,43.6)),
                    c(-dd(73,10,57.1), dd(45,59,42.3)),
                    c(-dd(73,11,09.6), dd(45,59,17.6)),
                    c(-dd(73,11,15.3), dd(45,59,19.0)),
                    c(-dd(73,11,02.6), dd(45,59,43.6)))


  # Lanoraie D (APM)
  # 45  59’ 41.1’’ N 073  10’ 44.1’’ W (Coin N-W)
  # 45  59’ 39.7’’ N 073  10’ 37.5’’ W (Coin N-E)
  # 45  59’ 20.8’’ N 073  10’ 53.7’’ W (Coin S-W)
  # 45  59’ 19.7’’ N 073  10’ 48.9’’ W (Coin S-E)
  plg[[4]] <- rbind(c(-dd(73,10,44.1), dd(45,59,41.1)),
                    c(-dd(73,10,37.5), dd(45,59,39.7)),
                    c(-dd(73,10,48.9), dd(45,59,19.7)),
                    c(-dd(73,10,53.7), dd(45,59,20.8)),
                    c(-dd(73,10,44.1), dd(45,59,41.1)))


  # Lanoraie E (APM)
  # 46  00’ 20.0’’ N 073  10’ 44.4’’ W (Coin N-W)
  # 46  00’ 19.1’’ N 073  10’ 40.6’’ W (Coin N-E)
  # 45  59’ 59.4’’ N 073  10’ 54.8’’ W (Coin S-W)
  # 45  59’ 58.4’’ N 073  10’ 50.2’’ W (Coin S-E)
  plg[[5]] <- rbind(c(-dd(73,10,44.4), dd(46,00,20.0)),
                    c(-dd(73,10,40.6), dd(46,00,19.1)),
                    c(-dd(73,10,50.2), dd(45,59,58.4)),
                    c(-dd(73,10,54.8), dd(45,59,59.4)),
                    c(-dd(73,10,44.4), dd(46,00,20.0)))

  # Spatial objects 
  nm <- c("Lanoraie A (APM)","Lanoraie B (APM)","Lanoraie C (APM)",
          "Lanoraie D (APM)","Lanoraie E (APM)")
  for(i in 1:length(plg)) {
    plg[[i]] <- list(plg[[i]]) %>%
                st_polygon() %>%
                st_sfc(crs = 4326) %>%
                st_sf(nom = nm[i]) %>%
                rename("geometry" = ".")
  }

  # Single object 
  plg <- bind_rows(plg) %>%
         st_transform(crs = global_parameters()$crs)

  # ------------------------------------------------
  # Combine together as polygons for the analysis
  data0070 <- bind_rows(st_buffer(pts, 100), plg)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output raw 
  st_write(obj = pts,
           dsn = "./data/data-raw/data0070-ancrage_gcc/ancrage_gcc_points.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)

  st_write(obj = plg,
           dsn = "./data/data-raw/data0070-ancrage_gcc/ancrage_gcc_polygons.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)

  # Output format
  st_write(obj = data0070,
           dsn = "./data/data-format/data0070-ancrage_gcc.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}

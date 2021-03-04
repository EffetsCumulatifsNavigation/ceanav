
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ceanav

Compendium de recherche pour le projet d'évaluation des effets cumulatifs des activités maritimes dans le Système du Saint-Laurent entre Montréal et Pointe-des-Monts.


### Comment citer

Citer ce compendium de recherche comme suit :

> ------------------------------------------------------------------------

## Contenu

Ce compendium de recherche est structuré de la façon suivante :

-   :file\_folder:
     [**R/**](https://github.com/EffetsCumulatifsNavigation/ceanav/tree/master/R) :
     contient les scripts et fonctions R utilisées pour ce projet


-   :file\_folder:
     [**man/**](https://github.com/ahasverus/ceanavR/tree/master/man):
    contains help files of R functions

-   :page\_facing\_up:
     [**DESCRIPTION**](https://github.com/ahasverus/ceanavR/tree/master/DESCRIPTION):
    contains project metadata (author, date, dependencies, etc.)

-   :page\_facing\_up:
     [**make.R**](https://github.com/ahasverus/ceanavR/tree/master/make.R):
    master R script to run the entire project by calling each R script
    stored in the **analyses/rscripts/** folder

-   :file\_folder:
     [**analyses/data/**](https://github.com/ahasverus/ceanavR/tree/master/analyses/data):
    contains all raw data required to perform analyses

-   :file\_folder:
     [**analyses/rscripts/**](https://github.com/ahasverus/ceanavR/tree/master/analyses/rscripts/):
    contains R scripts to run each step of the workflow

-   :file\_folder:
     [**analyses/outputs/**](https://github.com/ahasverus/ceanavR/tree/master/analyses/outputs):
    contains all the results created during the workflow

-   :file\_folder:
     [**analyses/figures/**](https://github.com/ahasverus/ceanavR/tree/master/analyses/figures):
    contains all the figures created during the workflow

## Usage

Clone the repository and run this command in R/RStudio:

``` r
source("make.R")
```

## Notes

-   All required packages, listed in the `DESCRIPTION` file, will be
    installed (if necessary)
-   All required packages and R functions will be loaded
-   Figures will be stored in `figures/`

:warning:  Some computations may take time.

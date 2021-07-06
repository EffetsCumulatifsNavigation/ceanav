
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ceanav

Ce dépôt contient le compendium de recherche pour le projet pilote
d’évaluation des effets cumulatifs des activités maritimes sur le
Saint-Laurent et la rivière Saguenay.

### Comment citer

Veillez citer ce compendium de recherche comme suit:

> Beauchesne D., Grant C., Archambault P. (2021) Compendium de recherche
> pour le projet pilote d’évaluation des effets cumulatifs des activités
> maritimes sur le Saint-Laurent et la rivière Saguenay. Consulté le
> \[AAAA-MM-JJ\].
> <https://github.com/EffetsCumulatifsNavigation/ceanav>.

## Contenu

Ce dépôt est structuré comme suit:

-   [**data/**](https://github.com/EffetsCumulatifsNavigation/ceanav/tree/main/data):
    contient les données du projet

    -   [*data-contact/*](): contient les données sur les différentes
        personnes ressources
    -   [*data-format/*](): contient les données formatées
    -   [*data-metadata/*](): contient les métadonnées pour les données
        brutes
    -   [*data-output/*](): contient les données intégrées et les
        résultats d’analyse du projet
    -   [*data-raw/*](): contient les données brutes. :warning:
        l’ensemble des données du projet ne sont pas disponibles puisque
        certaines données sont protégées par des ententes de
        confidentialité. Consultez les métadonnées pour de plus amples
        informations.

-   [**man/**](https://github.com/EffetsCumulatifsNavigation/ceanav/tree/main/man):
    contient la documentation des fonctions R

-   [**R/**](https://github.com/EffetsCumulatifsNavigation/ceanav/tree/main/R):
    contains R functions developed especially for this project

    -   [*00-pipeline.R*](): contient une fonction qui permet d’accéder
        aux données brutes, les formater, l’intégration des données, les
        analyses, les figures et tableaux, et le rapport du projet.
        :warning: cette fonction prend un temps considérable è executer.
    -   [*dataXXXX-nom.R*](): contient les routines pour accéder aux
        données brutes et les formater
    -   [*int-cv\_nom*](): contient les routines pour faire
        l’intégration des composantes valorisées
    -   [*int-st\_nom*](): contient les routines pour faire
        l’intégration des stresseurs environnementaux
    -   [*fnc\_nom*](): contient des fonctions R utilisées pour le
        projet d’évaluation des effets cumulatifs
    -   [*rep\_nom*](): contien des fonctions R utilisées pour générer
        des portions du rapport d’étude

-   [**report/**](https://github.com/EffetsCumulatifsNavigation/ceanav/tree/main/report):
    contient le rapport du projet

-   [**DESCRIPTION**](https://github.com/EffetsCumulatifsNavigation/ceanav/tree/main/DESCRIPTION):
    contient les métadonnées du projet (auteurs, date, dépendences,
    etc.)

## Utilisation

Clonez ce dépôt et executez ces commandes dans votre terminal:

``` r
R -e 'library(devtools);document()'
R CMD INSTALL .
R
library(ceanav)
pipeline()
```

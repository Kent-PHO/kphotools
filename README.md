# kphotools R package
This package contains useful functions for the [Kent Public Health Observatory](https://www.kpho.org.uk/) team in Kent County Council.

## Installation
###  With devtools
```R
devtools::install_github("harrywhitlow/kphotools")
```

## Current functions
### kpho_map()  
Standardised KPHO style map using ggplot2, sf and patchwork code.
### kpho_trend() ***WIP***  
Standardised KPHO style trend graph using ggplot2 code.
### kpho_colours() ***WIP***  
Standardised KPHO colour palettes (for district, utla, hcp and nhs trust) printed in the console or within ggplot2 code.  
### icd10_name()  
Converts ICD10 codes to the corresponding name of causes of death. Can provide high-level categories of causes of death or more detailed categories.  
### age_banding()  
Creates a new column of age bands based on a column of single year ages. The age bands come out in the format needed to use the PHE indicators life expectancy function. 
### add_esp()
Creates a new column of european standard population (ESP) weights based on a column of age bands in your dataframe. The ESP weights come out in the format needed to use some of the PHEindicators functions. 

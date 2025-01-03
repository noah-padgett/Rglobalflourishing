
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rglobalflourishing

<!-- badges: start -->
<!-- badges: end -->

The goal of Rglobalflourishing is to provides researchers interested in
analyzing data from the Global Flourishing Study with a coordinated set
of analyses.

WARNING: The package was set up to be user-friendly for researchers part
of the GFS core team who mainly have experience with other statistical
analysis software such as STATA, SAS, and SPSS. This package and
implementation of the analyses for Wave 2 of the Global Flourishing
Study does NOT conform to “tidy” principles in general. While some
elements of tidy evaluation and syntax structure are used throughout, we
did not implement everything with “tidyness” in mind. As such, we make
no guarantees that the package will integrate or “play nice” with other
packages.

## Installation

You can install the development version of Rglobalflourishing like so:

``` r
# install.packages("devtools")
devtools::install_github("noah-padgett/Rglobalflourishing")
```

The package is currently not available on CRAN or bioconductor.

## Example

``` r
library(Rglobalflourishing)
#> Warning: replacing previous import 'flextable::continuous_summary' by
#> 'gtsummary::continuous_summary' when loading 'Rglobalflourishing'
#> Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading
#> 'Rglobalflourishing'
#> Warning: replacing previous import 'mice::filter' by 'stats::filter' when
#> loading 'Rglobalflourishing'
#> Warning: replacing previous import 'EValue::HR' by 'survey::HR' when loading
#> 'Rglobalflourishing'

# load required packages
load_packages()
#> [[1]]
#>  [1] "survey"             "survival"           "Matrix"            
#>  [4] "grid"               "Rglobalflourishing" "stats"             
#>  [7] "graphics"           "grDevices"          "utils"             
#> [10] "datasets"           "methods"            "base"              
#> 
#> [[2]]
#>  [1] "gtsummary"          "survey"             "survival"          
#>  [4] "Matrix"             "grid"               "Rglobalflourishing"
#>  [7] "stats"              "graphics"           "grDevices"         
#> [10] "utils"              "datasets"           "methods"           
#> [13] "base"              
#> 
#> [[3]]
#>  [1] "mice"               "gtsummary"          "survey"            
#>  [4] "survival"           "Matrix"             "grid"              
#>  [7] "Rglobalflourishing" "stats"              "graphics"          
#> [10] "grDevices"          "utils"              "datasets"          
#> [13] "methods"            "base"              
#> 
#> [[4]]
#>  [1] "EValue"             "mice"               "gtsummary"         
#>  [4] "survey"             "survival"           "Matrix"            
#>  [7] "grid"               "Rglobalflourishing" "stats"             
#> [10] "graphics"           "grDevices"          "utils"             
#> [13] "datasets"           "methods"            "base"              
#> 
#> [[5]]
#>  [1] "metafor"            "numDeriv"           "metadat"           
#>  [4] "EValue"             "mice"               "gtsummary"         
#>  [7] "survey"             "survival"           "Matrix"            
#> [10] "grid"               "Rglobalflourishing" "stats"             
#> [13] "graphics"           "grDevices"          "utils"             
#> [16] "datasets"           "methods"            "base"              
#> 
#> [[6]]
#>  [1] "harmonicmeanp"      "FMStable"           "metafor"           
#>  [4] "numDeriv"           "metadat"            "EValue"            
#>  [7] "mice"               "gtsummary"          "survey"            
#> [10] "survival"           "Matrix"             "grid"              
#> [13] "Rglobalflourishing" "stats"              "graphics"          
#> [16] "grDevices"          "utils"              "datasets"          
#> [19] "methods"            "base"              
#> 
#> [[7]]
#>  [1] "robsurvey"          "harmonicmeanp"      "FMStable"          
#>  [4] "metafor"            "numDeriv"           "metadat"           
#>  [7] "EValue"             "mice"               "gtsummary"         
#> [10] "survey"             "survival"           "Matrix"            
#> [13] "grid"               "Rglobalflourishing" "stats"             
#> [16] "graphics"           "grDevices"          "utils"             
#> [19] "datasets"           "methods"            "base"              
#> 
#> [[8]]
#>  [1] "robustbase"         "robsurvey"          "harmonicmeanp"     
#>  [4] "FMStable"           "metafor"            "numDeriv"          
#>  [7] "metadat"            "EValue"             "mice"              
#> [10] "gtsummary"          "survey"             "survival"          
#> [13] "Matrix"             "grid"               "Rglobalflourishing"
#> [16] "stats"              "graphics"           "grDevices"         
#> [19] "utils"              "datasets"           "methods"           
#> [22] "base"              
#> 
#> [[9]]
#>  [1] "knitr"              "robustbase"         "robsurvey"         
#>  [4] "harmonicmeanp"      "FMStable"           "metafor"           
#>  [7] "numDeriv"           "metadat"            "EValue"            
#> [10] "mice"               "gtsummary"          "survey"            
#> [13] "survival"           "Matrix"             "grid"              
#> [16] "Rglobalflourishing" "stats"              "graphics"          
#> [19] "grDevices"          "utils"              "datasets"          
#> [22] "methods"            "base"              
#> 
#> [[10]]
#>  [1] "flextable"          "knitr"              "robustbase"        
#>  [4] "robsurvey"          "harmonicmeanp"      "FMStable"          
#>  [7] "metafor"            "numDeriv"           "metadat"           
#> [10] "EValue"             "mice"               "gtsummary"         
#> [13] "survey"             "survival"           "Matrix"            
#> [16] "grid"               "Rglobalflourishing" "stats"             
#> [19] "graphics"           "grDevices"          "utils"             
#> [22] "datasets"           "methods"            "base"              
#> 
#> [[11]]
#>  [1] "officer"            "flextable"          "knitr"             
#>  [4] "robustbase"         "robsurvey"          "harmonicmeanp"     
#>  [7] "FMStable"           "metafor"            "numDeriv"          
#> [10] "metadat"            "EValue"             "mice"              
#> [13] "gtsummary"          "survey"             "survival"          
#> [16] "Matrix"             "grid"               "Rglobalflourishing"
#> [19] "stats"              "graphics"           "grDevices"         
#> [22] "utils"              "datasets"           "methods"           
#> [25] "base"              
#> 
#> [[12]]
#>  [1] "patchwork"          "officer"            "flextable"         
#>  [4] "knitr"              "robustbase"         "robsurvey"         
#>  [7] "harmonicmeanp"      "FMStable"           "metafor"           
#> [10] "numDeriv"           "metadat"            "EValue"            
#> [13] "mice"               "gtsummary"          "survey"            
#> [16] "survival"           "Matrix"             "grid"              
#> [19] "Rglobalflourishing" "stats"              "graphics"          
#> [22] "grDevices"          "utils"              "datasets"          
#> [25] "methods"            "base"              
#> 
#> [[13]]
#>  [1] "lubridate"          "forcats"            "stringr"           
#>  [4] "dplyr"              "purrr"              "readr"             
#>  [7] "tidyr"              "tibble"             "ggplot2"           
#> [10] "tidyverse"          "patchwork"          "officer"           
#> [13] "flextable"          "knitr"              "robustbase"        
#> [16] "robsurvey"          "harmonicmeanp"      "FMStable"          
#> [19] "metafor"            "numDeriv"           "metadat"           
#> [22] "EValue"             "mice"               "gtsummary"         
#> [25] "survey"             "survival"           "Matrix"            
#> [28] "grid"               "Rglobalflourishing" "stats"             
#> [31] "graphics"           "grDevices"          "utils"             
#> [34] "datasets"           "methods"            "base"              
#> 
#> [[14]]
#>  [1] "lubridate"          "forcats"            "stringr"           
#>  [4] "dplyr"              "purrr"              "readr"             
#>  [7] "tidyr"              "tibble"             "ggplot2"           
#> [10] "tidyverse"          "patchwork"          "officer"           
#> [13] "flextable"          "knitr"              "robustbase"        
#> [16] "robsurvey"          "harmonicmeanp"      "FMStable"          
#> [19] "metafor"            "numDeriv"           "metadat"           
#> [22] "EValue"             "mice"               "gtsummary"         
#> [25] "survey"             "survival"           "Matrix"            
#> [28] "grid"               "Rglobalflourishing" "stats"             
#> [31] "graphics"           "grDevices"          "utils"             
#> [34] "datasets"           "methods"            "base"              
#> 
#> [[15]]
#>  [1] "tidyselect"         "lubridate"          "forcats"           
#>  [4] "stringr"            "dplyr"              "purrr"             
#>  [7] "readr"              "tidyr"              "tibble"            
#> [10] "ggplot2"            "tidyverse"          "patchwork"         
#> [13] "officer"            "flextable"          "knitr"             
#> [16] "robustbase"         "robsurvey"          "harmonicmeanp"     
#> [19] "FMStable"           "metafor"            "numDeriv"          
#> [22] "metadat"            "EValue"             "mice"              
#> [25] "gtsummary"          "survey"             "survival"          
#> [28] "Matrix"             "grid"               "Rglobalflourishing"
#> [31] "stats"              "graphics"           "grDevices"         
#> [34] "utils"              "datasets"           "methods"           
#> [37] "base"              
#> 
#> [[16]]
#>  [1] "future"             "tidyselect"         "lubridate"         
#>  [4] "forcats"            "stringr"            "dplyr"             
#>  [7] "purrr"              "readr"              "tidyr"             
#> [10] "tibble"             "ggplot2"            "tidyverse"         
#> [13] "patchwork"          "officer"            "flextable"         
#> [16] "knitr"              "robustbase"         "robsurvey"         
#> [19] "harmonicmeanp"      "FMStable"           "metafor"           
#> [22] "numDeriv"           "metadat"            "EValue"            
#> [25] "mice"               "gtsummary"          "survey"            
#> [28] "survival"           "Matrix"             "grid"              
#> [31] "Rglobalflourishing" "stats"              "graphics"          
#> [34] "grDevices"          "utils"              "datasets"          
#> [37] "methods"            "base"              
#> 
#> [[17]]
#>  [1] "future"             "tidyselect"         "lubridate"         
#>  [4] "forcats"            "stringr"            "dplyr"             
#>  [7] "purrr"              "readr"              "tidyr"             
#> [10] "tibble"             "ggplot2"            "tidyverse"         
#> [13] "patchwork"          "officer"            "flextable"         
#> [16] "knitr"              "robustbase"         "robsurvey"         
#> [19] "harmonicmeanp"      "FMStable"           "metafor"           
#> [22] "numDeriv"           "metadat"            "EValue"            
#> [25] "mice"               "gtsummary"          "survey"            
#> [28] "survival"           "Matrix"             "grid"              
#> [31] "Rglobalflourishing" "stats"              "graphics"          
#> [34] "grDevices"          "utils"              "datasets"          
#> [37] "methods"            "base"              
#> 
#> [[18]]
#>  [1] "fastDummies"        "future"             "tidyselect"        
#>  [4] "lubridate"          "forcats"            "stringr"           
#>  [7] "dplyr"              "purrr"              "readr"             
#> [10] "tidyr"              "tibble"             "ggplot2"           
#> [13] "tidyverse"          "patchwork"          "officer"           
#> [16] "flextable"          "knitr"              "robustbase"        
#> [19] "robsurvey"          "harmonicmeanp"      "FMStable"          
#> [22] "metafor"            "numDeriv"           "metadat"           
#> [25] "EValue"             "mice"               "gtsummary"         
#> [28] "survey"             "survival"           "Matrix"            
#> [31] "grid"               "Rglobalflourishing" "stats"             
#> [34] "graphics"           "grDevices"          "utils"             
#> [37] "datasets"           "methods"            "base"              
#> 
#> [[19]]
#>  [1] "haven"              "fastDummies"        "future"            
#>  [4] "tidyselect"         "lubridate"          "forcats"           
#>  [7] "stringr"            "dplyr"              "purrr"             
#> [10] "readr"              "tidyr"              "tibble"            
#> [13] "ggplot2"            "tidyverse"          "patchwork"         
#> [16] "officer"            "flextable"          "knitr"             
#> [19] "robustbase"         "robsurvey"          "harmonicmeanp"     
#> [22] "FMStable"           "metafor"            "numDeriv"          
#> [25] "metadat"            "EValue"             "mice"              
#> [28] "gtsummary"          "survey"             "survival"          
#> [31] "Matrix"             "grid"               "Rglobalflourishing"
#> [34] "stats"              "graphics"           "grDevices"         
#> [37] "utils"              "datasets"           "methods"           
#> [40] "base"              
#> 
#> [[20]]
#>  [1] "ranger"             "haven"              "fastDummies"       
#>  [4] "future"             "tidyselect"         "lubridate"         
#>  [7] "forcats"            "stringr"            "dplyr"             
#> [10] "purrr"              "readr"              "tidyr"             
#> [13] "tibble"             "ggplot2"            "tidyverse"         
#> [16] "patchwork"          "officer"            "flextable"         
#> [19] "knitr"              "robustbase"         "robsurvey"         
#> [22] "harmonicmeanp"      "FMStable"           "metafor"           
#> [25] "numDeriv"           "metadat"            "EValue"            
#> [28] "mice"               "gtsummary"          "survey"            
#> [31] "survival"           "Matrix"             "grid"              
#> [34] "Rglobalflourishing" "stats"              "graphics"          
#> [37] "grDevices"          "utils"              "datasets"          
#> [40] "methods"            "base"
```

# RShiny_Microclim

[![DOI](https://zenodo.org/badge/311754360.svg)](https://zenodo.org/badge/latestdoi/311754360) Ecosphere manuscript version

RShiny_Microclim is an interactive shiny app that facilitates the selection of microclimate datasets.
The app compares 8 different datasets (SCAN, ERA5-Land, GLDAS, gridMET, NOAA NCDC, microclim, microclimUS, USCRN) both temporally and spatially.
Once users selects a dataset, we offer [this](https://bookdown.org/huckley/microclimate_users_guide/) user guide, which explains how to access and download each dataset.

## Prerequisites for opening in Rstudio
Git and Rstudio ([Instructions](https://resources.github.com/whitepapers/github-and-rstudio/))  
Installation of the following R packages: shiny, raster, ggplot2, leaflet, shinyWidgets, shinythemes, shinycssloaders, magrittr, shinyBS, shinyjs, climateR, AOI, plotly, data.table, viridis, cicerone, utils, MALDIquant, ncdf4, rnoaa 

```
pkgs <- c("shiny", "raster", "ggplot2", "leaflet", "shinyWidgets", "shinythemes", "shinycssloaders", "magrittr", "shinyBS", "shinyjs", "climateR", "AOI", "plotly", "data.table", "viridis", "cicerone", "utils", "MALDIquant", "ncdf4", "rnoaa")
lapply(pkgs, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

devtools::install_github(c("mikejohnson51/AOI", "mikejohnson51/climateR"))
```

## Using RShiny_Microclim
* Opening in Rstudio:  
Click on "Code" on the top right to copy the link to this repository.  
Click ```File```, ```New Project```, ```Version Control```, ```Git```  
Paste the repository URL and click ```Create Project```.

* Alternatively, go to [this link](https://map.trenchproject.com/RShiny_Microclim/).

## Contributing to RShiny_Microclim
<!--- If your README is long or you have some specific process or steps you want contributors to follow, consider creating a separate CONTRIBUTING.md file--->
To contribute to RShiny_Microclim, follow these steps:

1. Fork this repository.
2. Create a branch: `git checkout -b <branch_name>`.
3. Make your changes and commit them: `git commit -m '<commit_message>'`
4. Push to the original branch: `git push origin <project_name>/<location>`
5. Create the pull request.

Alternatively see the GitHub documentation on [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).


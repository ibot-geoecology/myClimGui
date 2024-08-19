# myClimGui

# Install
```R
requiered_packages <- c("purrr", "stringr", "lubridate", "ggplot2", "plotly", "myClim", "shiny", "shinyjs", "dplyr",
                        "DT", "tibble", "shinythemes", "callr", "RColorBrewer")
missing_packages <- requiered_packages[!(requiered_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)

if(!("shinyTree" %in% installed.packages()[,"Package"])) {
    remotes::install_github("shinyTree/shinyTree")
}

# installation of myClimGui package
install.packages("http://labgis.ibot.cas.cz/myclim/myClimGui_latest.tar.gz", repos=NULL, build_vignettes=TRUE)
```

# Run simple example
```R
myClimGui::mcg_run(myClim::mc_data_example_agg)
```

# Documentation & user manual
* [manual - functions documentation](http://labgis.ibot.cas.cz/myclim/gui/reference/index.html)
* [myClimGui tutorial](https://labgis.ibot.cas.cz/myclim/gui/articles/myClimGui-tutorial.html)

# Source code of package
* [GitHub repository](https://github.com/ibot-geoecology/myClimGui)   

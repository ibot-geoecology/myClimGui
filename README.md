# myClimGui

```R
requiered_packages <- c('shiny', 'shinyjs', 'shinyTree','myClim')
missing_packages <- requiered_packages[!(requiered_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
```


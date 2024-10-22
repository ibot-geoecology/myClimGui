library(shinytest2)

devtools::load_all()
app <- .app_get_shiny_object(myClim::mc_data_example_agg)
record_test(app, test_file = "tests/testthat/test_shiny.R")

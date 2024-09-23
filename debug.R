setwd("tests/testthat")
devtools::load_all()

mcg_run(myClim::mc_data_example_agg, port=1151, launch.browser=FALSE)

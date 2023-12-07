setwd("tests/testthat")
devtools::load_all()

mcg_run(myClim::mc_data_example_agg, port=8989, launch.browser=FALSE)

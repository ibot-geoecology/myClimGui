test_that("test app", {
    data <- myClim::mc_read_data("../data/TOMST/files_table.csv", silent=TRUE, clean=TRUE)
    app
})
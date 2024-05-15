test_that(".data_get_sensors", {
    sensors <- sort(.data_get_sensors(myClim::mc_data_example_clean))
    expect_equal(sensors, sort(c("Dendro_raw", "Dendro_T", "HOBO_RH", "HOBO_T", "Thermo_T", "TMS_moist",
                                 "TMS_T1", "TMS_T2", "TMS_T3")))
})

test_that(".data_get_filtered_data_table", {
    table <- .data_get_filtered_data_table(myClim::mc_data_example_clean)
    expect_equal(colnames(table), c("locality_id", "index", "serial_number", "logger_type"))
    table <- .data_get_filtered_data_table(myClim::mc_data_example_agg)
    expect_equal(colnames(table), "locality_id")
})

test_that(".data_get_date_range", {
    date_range <- .data_get_date_range(myClim::mc_data_example_raw)
    expect_equal(date_range, c(lubridate::ymd_h("2020-10-06 9"), lubridate::ymd_h("2021-02-01 0")))
    date_range <- .data_get_date_range(myClim::mc_data_example_raw, "day")
    expect_equal(date_range, c(lubridate::ymd_h("2020-10-06 0"), lubridate::ymd_h("2021-02-01 0")))
    date_range <- .data_get_date_range(myClim::mc_data_example_agg)
    expect_equal(date_range, c(lubridate::ymd_h("2020-10-06 9"), lubridate::ymd_h("2021-02-01 0")))
    date_range <- .data_get_date_range(myClim::mc_data_example_agg, "day")
    expect_equal(date_range, c(lubridate::ymd_h("2020-10-06 0"), lubridate::ymd_h("2021-02-01 0")))
})

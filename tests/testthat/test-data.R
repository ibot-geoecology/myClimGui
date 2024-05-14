test_that(".data_get_sensors", {
    sensors <- sort(.data_get_sensors(myClim::mc_data_example_clean))
    expect_equal(sensors, sort(c("Dendro_raw", "Dendro_T", "HOBO_RH", "HOBO_T", "Thermo_T", "TMS_moist",
                                 "TMS_T1", "TMS_T2", "TMS_T3")))
})

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

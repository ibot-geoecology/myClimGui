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

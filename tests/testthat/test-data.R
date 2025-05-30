test_that(".data_get_sensors", {
    sensors <- sort(.data_get_sensors(myClim::mc_data_example_clean))
    expect_equal(sensors, sort(c("Dendro_raw", "Dendro_T", "HOBO_RH", "HOBO_T", "Thermo_T", "TMS_moist",
                                 "TMS_T1", "TMS_T2", "TMS_T3")))
})

test_that(".data_get_filtered_data_table", {
    selection_table <- tibble::tibble(locality_id = c("A1E05", "A2E32", "A2E32"),
                                      logger_name = c("Dendro_1", "HOBO_U23-001A_1", "HOBO_U23-001A_1"),
                                      sensor_name = c("Dendro_T", "HOBO_T", "HOBO_RH"))
    table <- .data_get_filtered_data_table(selection_table)
    expect_equal(nrow(table), 2)
    expect_equal(colnames(table), c("locality_id", "logger_name"))
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

test_that(".data_filter_by_selection_table", {
    selected_raw <- readRDS("../data/tree/selected_raw.rds")
    selection_table <- .tree_get_selection_table(myClim::mc_data_example_raw, selected_raw)
    expect_equal(colnames(selection_table), c("locality_id", "logger_name", "sensor_name"))
    filtered_raw_data <- .data_filter_by_selection_table(myClim::mc_data_example_raw, selection_table)
    expect_equal(length(filtered_raw_data$localities), 2)
    expect_equal(length(filtered_raw_data$localities$A1E05$loggers), 2)
    expect_equal(length(filtered_raw_data$localities$A1E05$loggers[[1]]$sensors), 1)
    expect_equal(length(filtered_raw_data$localities$A1E05$loggers[[2]]$sensors), 2)
    expect_equal(length(filtered_raw_data$localities$A2E32$loggers), 1)
    expect_equal(length(filtered_raw_data$localities$A2E32$loggers[[1]]$sensors), 4)
    selected_agg <- readRDS("../data/tree/selected_agg.rds")
    selection_table_agg <- .tree_get_selection_table(myClim::mc_data_example_agg, selected_agg)
    filtered_agg_data <- .data_filter_by_selection_table(myClim::mc_data_example_agg, selection_table_agg)
    expect_equal(length(filtered_agg_data$localities), 2)
    expect_equal(length(filtered_agg_data$localities$A1E05$sensors), 3)
    expect_equal(length(filtered_agg_data$localities$A2E32$sensors), 4)
})

test_that(".data_get_selection_table", {
    selection_table <- .data_get_selection_table(myClim::mc_data_example_clean, "A1E05", "Dendro")
    expect_equal(colnames(selection_table), c("locality_id", "logger_name", "sensor_name"))
    expect_equal(nrow(selection_table), 2)
    expect_equal(selection_table$logger_name, c("Dendro_1", "Dendro_1"))
})

test_that(".data_get_dataview_table", {
    data <- myClim::mc_data_example_clean
    selection_table <- tibble::tibble(locality_id = c("A1E05", "A2E32", "A2E32"),
                                      logger_name = c("Dendro_1", "HOBO_U23-001A_1", "HOBO_U23-001A_1"),
                                      sensor_name = c("Dendro_T", "HOBO_T", "HOBO_RH"))
    crop_intervals <- lubridate::interval(lubridate::ymd_h("2020-11-01 0"), lubridate::ymd_h("2020-12-01 0"))
    table <- .data_get_dataview_table(data, selection_table, crop_intervals)
    expect_equal(colnames(table), c("datetime", "A1E05_Dendro_1_Dendro_T", "A2E32_HOBO_U23-001A_1_HOBO_T", "A2E32_HOBO_U23-001A_1_HOBO_RH"))
    crop_intervals <- c(lubridate::interval(lubridate::ymd_h("2020-11-01 0"), lubridate::ymd_hm("2020-11-01 0:30")),
                        lubridate::interval(lubridate::ymd_h("2020-11-01 12"), lubridate::ymd_hm("2020-11-01 12:15")))
    table <- .data_get_dataview_table(data, selection_table, crop_intervals)
    expect_equal(table$datetime,
                 c("2020-11-01 00:00:00",
                   "2020-11-01 00:15:00",
                   "2020-11-01 00:30:00",
                   "2020-11-01 12:00:00",
                   "2020-11-01 12:15:00"))
})

test_that(".data_get_dataview_table name colision", {
    data <- myClim::mc_read_files("../data/dataview_table", "TOMST", silent = TRUE)
    selection_table <- tibble::tibble(locality_id = c("91184101", "91184101"),
                                      logger_name = c("Thermo_2", "Thermo_3"),
                                      sensor_name = c("Thermo_T", "Thermo_T"))
    crop_range <- lubridate::interval(lubridate::ymd_hm("2020-10-28 8:45"), lubridate::ymd_hm("2020-10-28 11:15"))
    table <- .data_get_dataview_table(data, selection_table, crop_range)
    expect_equal(colnames(table), c("datetime", "91184101_Thermo_2_Thermo_T", "91184101_Thermo_3_Thermo_T"))
})

test_that(".data_delete_states", {
    data <- myClim::mc_data_example_clean
    old_states_table <- myClim::mc_info_states(data)
    delete_table <- dplyr::filter(old_states_table, .data$locality_id == "A1E05")
    data_delete <- .data_delete_states(data, delete_table = delete_table)
    new_states_table <- myClim::mc_info_states(data_delete)
    expect_equal(nrow(old_states_table), nrow(new_states_table) + nrow(delete_table))
    expect_false("A1E05" %in% new_states_table$locality_id)
})

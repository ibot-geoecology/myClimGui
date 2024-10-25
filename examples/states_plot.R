library(myClim)
library(tibble)
library(Cairo)

#devtools::load_all()
data <- mc_data_example_clean

states <- tribble(
        ~locality_id, ~logger_index,      ~tag,                                ~start,                                  ~end,
        "A1E05"    ,              1,   "error",  lubridate::ymd_hm("2020-11-01 0:00"), lubridate::ymd_hm("2020-11-01 23:30"),
        "A1E05"    ,              1,   "error", lubridate::ymd_hm("2020-11-03 10:00"), lubridate::ymd_hm("2020-11-03 12:00"),
        "A1E05"    ,              1,    "test", lubridate::ymd_hm("2020-11-01 12:00"),  lubridate::ymd_hm("2020-11-02 5:00"),
)

data <- mc_states_insert(data, states)
#states <- dplyr::filter(mc_info_states(data), .data$tag != "source")
#states <- mc_info_states(data)[1, ]
#print(.plot_states(data, states))

#new_data <- mcg_run(data, port=1151, launch.browser=FALSE)
new_data <- mcg_run(data)

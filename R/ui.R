.ui_const_PLOT_TITLE <- "Plot"
.ui_const_STATES_TITLE <- "States"
.ui_const_DATA_TITLE <- "Data"

.ui_get_main <- function(data, data_loggers) {
    shiny::navbarPage("myClimGui",
                      .ui_plot_tab(data, data_loggers),
                      .ui_states_tab(data),
                      .ui_data_tab(data),
                      id="navbar_page")
}


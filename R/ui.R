.ui_get_main <- function(data, data_loggers) {
    shiny::navbarPage("myClimGui",
                      .ui_plot_tab(data, data_loggers),
                      .ui_states_tab(data))
}


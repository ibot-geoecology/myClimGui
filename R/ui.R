.ui_const_PLOT_TITLE <- "Plot"
.ui_const_STATES_TITLE <- "States"
.ui_const_DATA_TITLE <- "Data"

.ui_get_main <- function(data, data_loggers) {
    shiny::fluidPage(theme = shinythemes::shinytheme("flatly"),
        style = "padding: 0px;",
        shiny::actionButton("return_button", "Return", icon = shiny::icon("door-open"),
                            style = "position: absolute; top: 8px; right: 8px; z-index:10000;"),
        shiny::tagAppendAttributes(
            shiny::textOutput("datetime_range"), style="font-weight: bold; color: white; position: absolute; top: 20px; right: 120px; z-index:10000;"),
        shiny::navbarPage("myClimGui",
                          .ui_plot_tab(data, data_loggers),
                          .ui_states_tab(data),
                          .ui_data_tab(data),
                          id="navbar_page"))
}


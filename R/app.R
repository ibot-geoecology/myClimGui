.app_const_SETTINGS_PLOTLY_KEY <- "plotly"
.app_const_SETTINGS_MULTI_SELECT_KEY <- "multi_select"
.app_const_SETTINGS_COLOR_BY_LOGGER_KEY <- "color_by_logger"

#' Start shiny app
#'
#' Main function, which start shiny application.
#'
#' @param data myClim data
#' @export
mcg_run <- function (data, ...) {
    data_loggers <- .data_get_locality_logger_type(data)
    ui <- .ui_get_main(data, data_loggers)

    server <- function (input, output, session) {
        shared <- .app_get_initialized_shared(data, data_loggers)
        states_table_value <- shiny::reactiveVal()
        data_table_value <- shiny::reactiveVal()
        
        shiny::observeEvent(input$navbar_page, {
            if(is.null(shared$filter_data)) {
                return()
            }
            tab_value <- shiny::req(input$navbar_page)
            if(tab_value == .ui_const_STATES_TITLE) {
                states_table_value(.server_states_get_table(shared))
            } else if(tab_value == .ui_const_DATA_TITLE) {
                data_table_value(.server_data_get_table(shared))
            }
        })
    
        shiny::observeEvent(input$return_button, {
            shiny::stopApp(shared$data)
        })
    
        .server_plot_get_main(input, output, session, shared)
        .server_states_get_main(input, output, session, shared, states_table_value)
        .server_data_get_main(input, output, session, shared, data_table_value)
    }

    app <- shiny::shinyApp(ui, server)
    shiny::runApp(app, ...)
}

.app_get_initialized_shared <- function(data, data_loggers) {
    result <- new.env()
    result$data <- data
    result$data_loggers <- data_loggers
    result$data_range <- .data_get_date_range(data)
    result$selection_table <- NULL
    result$filter_data <- NULL
    result$crop_range <- NULL
    return(result)
}
.app_const_SETTINGS_PLOTLY_KEY <- "plotly"
.app_const_SETTINGS_MULTI_SELECT_KEY <- "multi_select"
.app_const_SETTINGS_COLOR_BY_LOGGER_KEY <- "color_by_logger"
.app_const_JSON_MESSAGE <- "Input to asJSON(keep_vec_names=TRUE) is a named vector. In a future version of jsonlite, this option will not be supported, and named vectors will be translated into arrays instead of objects. If you want JSON object output, please use a named list instead. See ?toJSON.\n"

#' Start shiny app
#'
#' The main function that starts the Shiny application.
#' 
#' @details
#' The Shiny application opens in the RStudio viewer and can be opened in a web browser from there. 
#' The Shiny app provides an interactive tool for browsing *myClim* objects, 
#' inspecting, and comparing time-series data. 
#' You can view your time-series in interactive line plots or simple tables. 
#' It is also possible to inspect, add, and edit states (data quality tags).
#' For now, it is not possible to edit  time-series, but only states. To 
#' return *myClim* with edited states back to R environment hit "Return". 
#' See examples. 
#' For more details, see the [myClimGui] package description and vignette.
#' @param data myClim object
#' @param ... parameters for [shiny::runApp] function
#' @return myClim object edited in app
#' @export
#' @examples
#' \dontrun{
#' # start app only for browsing
#' myClimGui::mcg_run(myClim::mc_data_example_agg) 
#' 
#' # edit states, hit "return", save modified object into R environment. 
#' states.edit <- myClimGui::mcg_run(myClim::mc_data_example_agg)  
#' }
mcg_run <- function (data, ...) {
    app <- .app_get_shiny_object(data)
    .app_suppress_message(shiny::runApp(app, ...), .app_const_JSON_MESSAGE)
}

.app_suppress_message <- function(.expr, text_message) {
  eval.parent(
    substitute(
      withCallingHandlers( .expr, message = function (m) {
        cm   <- conditionMessage(m)
        cond <- text_message == cm
        if (cond) invokeRestart("muffleMessage")   
      })
    )
  )
}

#' Start shiny app in background
#'
#' This function starts the Shiny application in new process. The R console doesn't freeze.
#'
#' @details
#' The function mcg_run_bg doesn't open automatically app. You must open it manually in browser
#' with link http://localhost:1151 (default). The result of function is r_process object see [callr::r_bg()].
#' If you edit interactively, you can get myClim object returned from app by 
#' call function `r_process$get_result()`.
#'
#' @param data myClim object
#' @param port of app (default 1151)
#' @return An r_process object see [callr::r_bg()]
#' @export
#' @examples
#' \dontrun{
#'      proc <- myClimGui::mcg_run_bg(myClim::mc_data_example_agg)
#'      # after click Return button
#'      data <- proc$get_result()
#' }
mcg_run_bg <- function (data, port=1151) {
    print(stringr::str_glue("Open http://localhost:{port} in browser."))
    fun <- function(data, port) {
        myClimGui::mcg_run(data, port=port, launch.browser=FALSE)
    }
    result <- callr::r_bg(fun, args = list(data=data, port=port))
    return(result)
}

.app_get_shiny_object <- function(data) {
    data_loggers <- .data_get_locality_logger_type(data)
    ui <- .ui_get_main(data, data_loggers)

    server <- function (input, output, session) {
        shared <- .app_get_initialized_shared(data, data_loggers)
        states_table_value <- shiny::reactiveVal()
        data_table_value <- shiny::reactiveVal()
        
        shiny::observeEvent(input$navbar_page, {
            if(is.null(shared$selection_table)) {
                return()
            }
            tab_value <- shiny::req(input$navbar_page)
            if(tab_value == .ui_const_STATES_TITLE) {
                .server_states_reload_table(input, session, shared, states_table_value)
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

    return(shiny::shinyApp(ui, server))
}

.app_get_initialized_shared <- function(data, data_loggers) {
    result <- new.env()
    result$data <- data
    result$data_loggers <- data_loggers
    result$data_range <- .data_get_date_range(data)
    result$selection_table <- NULL
    result$crop_range <- NULL
    result$last_crop_range_params <- list(crop_range = NULL,
                                          selection_table = NULL)
    return(result)
}
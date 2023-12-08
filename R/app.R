#' Start shiny app
#'
#' Main function, which start shiny application.
#'
#' @param data myClim data
#' @export
mcg_run <- function (data, ...) {
    sidebar_width <- 300
    widget_space <- 30
    date_range <- .data_get_date_range(data)
    ui <- shiny::fillPage(
        shiny::tags$style(type="text/css",
            stringr::str_glue(".sidebar {{width: {sidebar_width}px; float: left; padding-left: 10px; height: 100%; overflow: scroll; }}"),
            stringr::str_glue(".main {{width: calc(100% - {sidebar_width}px); float: right; }}")),
        shinyjs::useShinyjs(),
        shiny::div(
            class="sidebar",
            shiny::actionButton("refresh_button", "Show", width=stringr::str_glue("{sidebar_width - widget_space}px"),
                                style="background-color: green; color: white; font-weight: bold"),
            shiny::checkboxInput("plotly_checkbox", "Use plotly", value=FALSE),
            shiny::selectInput("facet_select", "Facet", c("NULL", "locality", "physical"), selected="physical",
                               width=stringr::str_glue("{sidebar_width-widget_space}px")),
            shiny::div(style="display: inline-block; vertical-align: top; ",
                       shiny::dateRangeInput("date_range", NULL, start=date_range[[1]], end=date_range[[2]],
                                             width=stringr::str_glue("{sidebar_width-widget_space-50}px"))),
            shiny::div(style="display: inline-block; vertical-align: top; ",
                       shiny::actionButton("reset_button", NULL, icon=shiny::icon("refresh"), width="50px")),
            shiny::selectInput("sensor_select", "Sensors", sort(.data_get_sensors(data)), width=stringr::str_glue("{sidebar_width-30}px"),
                               multiple=TRUE),
            shinyTree::shinyTree("data_tree", checkbox=TRUE, search=TRUE, theme="proton", themeIcons=FALSE),
        ),
        shiny::div(
            class="main",
            plotly::plotlyOutput("plot_plotly", width="100%", height="100vh"),
            shiny::plotOutput("plot_ggplot", width="100%", height="100vh"),
        )
    )

    server <- function (input, output, session) {
        previous_sensors <- shiny::reactiveVal()
        
        shiny::observeEvent(input$plotly_checkbox, {
            use_plotly <- input$plotly_checkbox
            if(use_plotly) {
                shinyjs::show(id="plot_plotly")
                shinyjs::hide(id="plot_ggplot")
            } else {
                shinyjs::show(id="plot_ggplot")
                shinyjs::hide(id="plot_plotly")
            }
        })

        shiny::observeEvent(input$reset_button, {
            date_range <- .data_get_date_range(data)
            shiny::updateDateRangeInput(session, "date_range",
                                        start=date_range[[1]], end=date_range[[2]])
        })

        shiny::observeEvent(input$sensor_select, {
            tree <- shiny::isolate(input$data_tree)
            if(is.null(tree)) {
                tree <- .tree_get_list(data)
            }
            add_sensor <- length(input$sensor_select) > length(previous_sensors())
            if(add_sensor) {
                sensor <- lubridate::setdiff(input$sensor_select, previous_sensors())
            }
            else {
                sensor <- lubridate::setdiff(previous_sensors(), input$sensor_select)
            }
            shinyTree::updateTree(session, "data_tree", .tree_change_selection(tree, sensor, add_sensor))
            previous_sensors(input$sensor_select)
        })

        output$data_tree <- shinyTree::renderTree({.tree_get_list(data)})
        
        output$plot_plotly <- plotly::renderPlotly({
            input$refresh_button
            shiny::isolate(plot <- .app_get_plot(data, input))
            if(is.null(plot)) {
                return(NULL)
            }
            return(plotly::ggplotly(plot))
        })

        output$plot_ggplot <- shiny::renderPlot({
            input$refresh_button
            shiny::isolate(plot <- .app_get_plot(data, input))
            if(is.null(plot)) {
                return(NULL)
            }
            return(plot)
        })
    }

    app <- shiny::shinyApp(ui, server)
    shiny::runApp(app, ...)
}

.app_get_plot <- function(data, input)
{
    data_tree <- input$data_tree
    date_range <- input$date_range
    selected_facet_text <- input$facet_select
    if(is.null(data_tree)) {
        return(NULL)
    } else {
        slices <- shinyTree::get_selected(data_tree, format="slices")
        if(length(slices) == 0) {
            return(NULL)
        }
        filtered_data <- .tree_filter_data(data, slices)
        facet <- if(selected_facet_text == "NULL") NULL else selected_facet_text
        plot <- myClim::mc_plot_line(filtered_data, facet=facet, start_crop=date_range[[1]], end_crop=date_range[[2]])
        return(plot)
    }
}
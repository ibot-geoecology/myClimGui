#' Start shiny app
#'
#' Main function, which start shiny application.
#'
#' @param data myClim data
#' @export
mcg_run <- function (data, ...) {
    sidebar_width <- 250
    
    ui <- shiny::fillPage(
        shiny::tags$style(type="text/css",
            stringr::str_glue(".sidebar {{width: {sidebar_width}px; float: left; padding-left: 10px; }}"),
            stringr::str_glue(".main {{width: calc(100% - {sidebar_width}px); float: right; }}")),
        shinyjs::useShinyjs(),
        shiny::div(
            class="sidebar",
            shiny::selectInput("facet_select", "Facet", c("NULL", "locality", "physical"), selected="physical", width=stringr::str_glue("{sidebar_width-30}px")),
            shiny::checkboxInput("plotly_checkbox", "Use plotly", value=FALSE),
            shiny::actionButton("refresh_button", "Refresh", width=stringr::str_glue("{sidebar_width-30}px")),
            shiny::br(),
            shiny::br(),
            shinyTree::shinyTree("data_tree", checkbox=TRUE, search=TRUE, theme="proton", themeIcons=FALSE)
        ),
        shiny::div(
            class="main",
            plotly::plotlyOutput("plot_plotly", width="100%", height="100vh"),
            shiny::plotOutput("plot_ggplot", width="100%", height="100vh"),
        )
    )

    server <- function (input, output, session) {
        shiny::observeEvent(input$plotly_checkbox, {
            use_plotly <- input$plotly_checkbox
            if(use_plotly) {
                shinyjs::show(id="plot_plotly")
                shinyjs::hide(id="plot_ggplot")
            } else {
                shinyjs::show(id="plot_ggplot")
                shinyjs::hide(id="plot_plotly")
            }
            #shinyjs::click("refresh_button")
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
        plot <- myClim::mc_plot_line(filtered_data, facet=facet)
        return(plot)
    }
}
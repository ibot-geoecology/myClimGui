#' Start shine app
#'
#' Main function, which start shiny application.
#'
#' @param data myClim data
#' @export
mcg_run <- function (data) {
    sidebar_width <- 250
    
    ui <- shiny::fillPage(
        shiny::tags$style(type="text/css",
            stringr::str_glue(".sidebar {{width: {sidebar_width}px; float: left; padding-left: 10px; }}"),
            stringr::str_glue(".main {{width: calc(100% - {sidebar_width}px); float: right; }}")),
        shiny::div(
            class="sidebar",
            shiny::selectInput("facet_selectbox", "Facet", c("NULL", "locality", "physical"), selected="physical", width=stringr::str_glue("{sidebar_width-30}px")),
            shinyTree::shinyTree("data_tree", checkbox=TRUE, search=TRUE, theme="proton", themeIcons=FALSE),
        ),
        shiny::div(
            class="main",
            plotly::plotlyOutput("plot", width="100%", height="100vh"),
        )
    )

    server <- function (input, output, session) {
        output$data_tree <- shinyTree::renderTree({.tree_get_list(data)})

        output$plot <- plotly::renderPlotly({
            data_tree <- input$data_tree
            if(is.null(data_tree)) {
                return(NULL)
            } else {
                slices <- shinyTree::get_selected(data_tree, format="slices")
                filtered_data <- .tree_filter_data(data, slices)
                facet <- if(input$facet_selectbox == "NULL") NULL else input$facet_selectbox
                plotly::ggplotly(myClim::mc_plot_line(filtered_data, facet=facet))
            }
        })
    }

    shiny::shinyApp(ui, server)
}
mcg_run <- function (data) {
    ui <- shiny::fluidPage(
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shinyTree::shinyTree("data_tree", checkbox=TRUE, search=TRUE, theme="proton", themeIcons=FALSE)
            ),
            shiny::mainPanel(
                plotly::plotlyOutput("plot")
            )
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
                plotly::ggplotly(myClim::mc_plot_line(filtered_data))
            }
        })
    }

    shiny::shinyApp(ui, server)
}
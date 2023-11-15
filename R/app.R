app_state <- new.env()
app_state$prev_slices <- ""
app_state$facet <- ""
app_state$plot <- NULL

#' Start shiny app
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
                slices_character <- .tree_selected_to_vector(slices)
                selected_facet_text <- input$facet_selectbox
                if(length(slices) == 0 ||
                   (all(app_state$prev_slices == slices_character) && selected_facet_text == app_state$facet)) {
                    return(app_state$plot)
                }
                app_state$prev_slices <- slices_character
                app_state$facet <- selected_facet_text
                filtered_data <- .tree_filter_data(data, slices)
                facet <- if(selected_facet_text == "NULL") NULL else selected_facet_text
                app_state$plot <- plotly::ggplotly(myClim::mc_plot_line(filtered_data, facet=facet))
                return(app_state$plot)
            }
        })
    }

    shiny::shinyApp(ui, server)
}
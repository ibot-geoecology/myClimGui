mcg_run <- function (data) {
    ui <- shiny::fluidPage(
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shinyTree::shinyTree("data_tree", checkbox=TRUE, search=TRUE)
            ),
            shiny::mainPanel(

            )
        )
    )

    server <- function (input, output, session) {
        output$data_tree <- shinyTree::renderTree({.tree_get_list(data)})
    }

    shiny::shinyApp(ui, server)
}
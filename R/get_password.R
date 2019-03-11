#' Shiny gadget to obtain a password
get_password <- function(){
  library(shiny)
  library(miniUI)
  ui <- miniPage(
    gadgetTitleBar("Enter Password"),
    miniContentPanel(
      passwordInput('pwd','Password',width = '100%')
    )
  )

  server <- function(input, output, session) {

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(input$pwd)
    })
  }
  runGadget(ui, server, viewer = dialogViewer("Password",height = 10))
}

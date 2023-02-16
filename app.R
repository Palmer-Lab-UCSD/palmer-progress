# non-reactive constants/functions, run once to set up the environment
source("helper.R", local = TRUE)
# define UI of app
source("ui.R", local = TRUE)

# define backend logic of app, split up by page
server <- function(input, output, session) { 
  source("setup.R", local = TRUE)
  source("first_page.R", local = TRUE)
  source("second_page.R", local = TRUE)
  source("third_page.R", local = TRUE)
  source("fourth_page.R", local = TRUE)
}

# actually run app
shinyApp(ui = ui, server = server)

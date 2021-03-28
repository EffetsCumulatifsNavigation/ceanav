ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Sidebar"),
      selectInput(inputId = "layers", label = "Select layer", choices = dat)#,
      # actionButton('save', 'Save from Map', icon = icon("download"))
    ),
    mainPanel(
      h1("Main Panel"),
      mapedit::editModUI("map", width = "100%", height = "90vh")
    )
  )
)


shinyUI(fluidPage(
  
  includeCSS('progress.css'),

  ###  Application title
  headerPanel("Graph Explorer"),
  
  ### Sidebar 
  makeSidebar(),

  ### Main Panel
  mainPanel(
    tabsetPanel(
       ## Graph, vertex and edge attributes
       attrTab(),
       ## Plot the graph in various ways.
       graphplotTab(),
       ## Graph invariants and statistics
       invTab(),
       communitiesTab()#,
       #fusionTab()
   )
  )
))


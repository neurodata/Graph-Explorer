graphplotTab <- function()
{
   tabPanel("Graph Plotting",
      h2("Plot the graph and some of its attributes"),
      p("Select the tab to plot in the plane or to plot the adjacency matrix as an image."),
      tabsetPanel("Plot Vertices/Edges",
         tabPanel("2D",
            h3("Two dimensional plots"),
               p("Click on a vertex to view values."),
               verbatimTextOutput('info'),
               plotOutput("plotgraph", height="800px",
                  click="plot_click")),
         tabPanel("Adjacency Matrix",
         h3("The adjacency matrix of the graph"),
            checkboxInput(inputId='contour',"Contour Plot",FALSE),
            bsTooltip(id='contour',
                title="A contour plot of a subsampled version of the adjacency matrix. Unlikely to be of much interest for small graphs.",
                placement='top'),
            conditionalPanel(
                 condition = "input.contour == true",
                     sliderInput(inputId='subsample',
                          label="Subsampling",min=2,max=3,
                          value=2,step=1)
            ),
            plotOutput("adjacencyPlot", height="600px",width="600px"),
            br())
      )
   )
}

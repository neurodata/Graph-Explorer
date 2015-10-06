graphplotTab <- function()
{
   tabPanel("Graph Plotting",
      h2("Plot the graph and some of its attributes"),
      p("Select the tab to plot in the plane, in 3D, or to plot the adjacency matrix as an image."),
      tabsetPanel("Plot Vertices/Edges",
         tabPanel("2D",
            h3("Two dimensional plots"),
               p("Click on a vertex to view values."),
               verbatimTextOutput('info'),
               plotOutput("plotgraph", height="800px",
                  click="plot_click")),
         tabPanel("3D",
         h3("Three dimensional plots"),
                p("If the plot does not display, try selecting/deselecting alpha blending."),
                checkboxInput(inputId='UseAlpha3D',"Use Alpha Blending",TRUE),
               conditionalPanel(
                    condition = "input.UseAlpha3D == true",
                    sliderInput(inputId="alphaLevel3D",label="Alpha Level",
                            min=0.0005,max=.25,value=0.1,step=0.0005),
                    p("Hold the left button down on the plot to rotate. Use the wheel to zoom in/out.")
               ),
                checkboxInput(inputId='randomZ',
                   label="Random Z (if layout doesn't provide z)",TRUE),
                  webGLOutput("plotgraph3d", height="800px",width="800px"),
                  br()),
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

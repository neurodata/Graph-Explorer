fusionTab <- function()
{
   tabPanel("Fusion UNDER CONSTRUCTION",
      h2("Fusion of graph and vertex attributes and/or covariates."),
      p("Under construction!"),
      p("Currently this does something that isn't quite right and does it incorrectly, no doubt. At some point it will be all rainbows and bunnies. For now, ignore it."),
      selectInput(inputId="fusion",label="Embedding Algorithm",
                   choices=c("RDPG","Laplacian"),selected="RDPG"),
      sliderInput(inputId='Fd',
                       label="Embedding Dimension",min=2,max=15,
                       value=5,step=1),
      sliderInput(inputId='lambda',
                       label="lambda",min=0.0,max=1.0,
                       value=0.5,step=0.1),
      sliderInput(inputId='fusek',
                       label="k",min=1,max=100,
                       value=10,step=1),
      checkboxInput(inputId='append',label="Append Covariates",value=FALSE),
      sliderInput(inputId='FG',
                       label="#Mclust models",min=2,max=50,
                       value=c(2,10),step=1),
      plotOutput("fusionPlot", height="800px",width="800px"),
      br(),br()
   )
}

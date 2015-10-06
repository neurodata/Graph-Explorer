communitiesTab <- function()
{
   tabPanel("Communities",
      h2("Community structure in the graph."),
      tabsetPanel(
      tabPanel("Single Algorithm",
      selectInput(inputId="communities",label="Communities to Compute",
                   choices=communities.list,selected="Fast Greedy"),
      conditionalPanel(
         condition = "input.communities == 'Walktrap' || input.communities == 'Fast Greedy' || input.communities == 'Edge Betweenness'",
      checkboxInput(inputId='dendPlot',label='Plot Dendrogram',FALSE)),
      conditionalPanel(
           condition = "input.communities == 'Laplacian' || input.communities == 'RDPG'",
      sliderInput(inputId='Cd',
                       label="Embedding Dimension",min=2,max=15,
                       value=5,step=1),
      sliderInput(inputId='CG',
                       label="Communities Range",min=2,max=50,
                       value=c(2,10),step=1)
      ),
      conditionalPanel(
         condition = "input.communities == 't-SNE' || input.CplotMethod == 't-SNE'",
            sliderInput(inputId='Ctheta',
                label="Theta",value=0.5,min=0,max=1,step=0.1)
            
      ),
      conditionalPanel(
         condition = "input.fast == false && input.dendPlot == false",
             checkboxInput(inputId="markGroups",label="Group the Communities",
                           value=FALSE)
      ),
      p("Click on a vertex to view values."),
      verbatimTextOutput('comm_info'),
      plotOutput("communityPlot", height="800px",width="800px",
                 click="comm_click"),
      checkboxInput(inputId='usecomm',label='Use community for x-axis',FALSE),
      br(),br()
   ),
   tabPanel("Compare All Algorithms",
      h2("Compare All Communities"),
      p(paste(
           "This computes all the community structures currently implemented.",
           "This will take a while to compute (see upper right corner of", 
           "the page), and will show a heatmap",
           "of a comparison between the communities.")),
      p(paste(
           "This uses the variation of information index of Meila,",
           "Journal of Multivariate Analysis, 98, 2007, 873-895.")),
      p(paste(
         "To the right of a designator of the community algorithm",
         "is a number indicating the number of communities",
         "found by the given algorithm.")),
      p(paste(
          "Depending on your browser, the gray levels may appear a bit off.",
          "The diagonal should be pure white, and the matrix symmetric.",
          "Variations from this are an artefact of the display.")),
      wellPanel(
         fluidRow(
            column(6,
               h3("Heatmap comparison"),
               plotOutput("communityCompM", height="800px",width="500px",
                          click="communityImage_click")
            ),
            column(6,
               h3("Community Names"),
               p("Laplac: Laplacian embedding + Mclust."),
               p("RDPG: RDPG embedding + Mclust."),
               p("t-SNE: t-SNE embedding + Mclust."),
               p("Fast: fast greedy."),
               p("Edge: edge betweenness."),
               p("Walk: walktrap."),
               p("LEigen: leading eigenvector."),
               p("LabelP: label progagation."),
               p("SpinG: spinglass."),
               p("MulitL: multilevel."),
               p("InfoM: infomap."),
               verbatimTextOutput("communityM_info")
            )
         )
      )
      )
   )
)
}

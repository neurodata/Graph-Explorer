attrTab <- function(){
   tabPanel("Attributes of the Graph",
      h2("The graph, edge and vertex attributes."),
      h3("Graph Attributes and Statistics"),
      DT::dataTableOutput("graphAtt"),
      wellPanel(
         fluidRow(
            column(6,
               h3("Vertex Attributes"),
               h4("Statistics of the attributes."),
               DT::dataTableOutput("vertexAtt"),
               h4("Plots of the attributes."),
					checkboxInput(inputId="VACor",label="Plot Correlation",
					              value=FALSE),
               selectInput(inputId="vertexAtts",
                           label="Attribute to plot",
               choices='None',selected='None',selectize=FALSE),
               plotOutput("plotVA", height="350px")
               ),
            column(6,
               h3("Edge Attributes"),
               h4("Statistics of the attributes."),
               DT::dataTableOutput("edgeAtt"),
               h4("Plots of the attributes."),
               selectInput(inputId="edgeAtts",label="Attribute to plot",
               choices='None',selected='None',selectize=FALSE),
               plotOutput("plotEA", height="350px"))))
   )
}

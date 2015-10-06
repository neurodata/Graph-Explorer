invTab <- function()
{
   tabPanel("Graph Invariants and Statistics",
       h2("Compute various graph invariants."),
       tabsetPanel(
         tabPanel("Tabular",
       p("Clicking on 'Select Invariants' opens/closes a selector to choose the invariants to compute."),
            bsCollapse(
               bsCollapsePanel(title="Select Invariants",
       p("Note that some invariants (clique number for example) may take a very long time on even moderate sized graphs"),
                  checkboxGroupInput(inputId='invariantsList',
                      label="Invariants to Compute",
                      choices=invariants$Invariant,
                      selected=c("Order","Size","#Components",
                                 "Max Component Size")))
               ),
               DT::dataTableOutput("mytable",width='50%'),
               br()
         ),
         tabPanel("Graphical",
            selectInput(inputId="invariants",label="Invariant to Plot",
                   choices=plot.invariants,selected="Degree Distribution"),
            plotOutput("plotinvariants", height="500px"),
      conditionalPanel(
           condition = "input.invariants == 'Alpha Centrality'",
              numericInput(inputId="alpha",label="Alpha",
                   min=0,max=1,value=0.5,step=0.1)
      ),
      conditionalPanel(
           condition = "input.invariants == 'Page Rank'",
              sliderInput(inputId="damping",label="Damping",
                   min=0,max=1,value=0.85,step=0.05)
      )
      )
      )
   )
}

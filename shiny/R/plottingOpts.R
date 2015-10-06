plottingOpts <- function(){
            bsCollapse(
                  bsCollapsePanel(title="Plotting Parameters",
      p("Plotting the graph may take a long time unless the fast plotting option is checked."),
               selectInput(inputId="plotMethod",label="Plot Method",
                  choices=plot.methods,selected=plot.methods[1],selectize=FALSE),
               bsTooltip(id='plotMethod',
                         title="The type of layout for the graph.",
                         placement='top'),
               conditionalPanel(
                    condition = "input.plotMethod == 'Fruchterman Reingold' || input.plotMethod == 'Fruchterman Reingold Grid'",
                       selectInput(inputId="FRniter",label="Number of Iterations",
                            choices=c(10,100,500,1000),selected=500),
                       selectInput(inputId="FRcoolexp",label="Cooling exponent",
                            choices=c(0.1,0.5,0.99,1,3,5,10),selected=3)
               ),
               conditionalPanel(
                    condition = "input.plotMethod == 'Reingold Tilford'",
                         checkboxInput('circular',"Circular",FALSE)
               ),
               conditionalPanel(
                  condition = "input.plotMethod == 't-SNE'",
                     sliderInput(inputId='theta',
                         label="Theta",value=0.5,min=0,max=1,step=0.1)
                     
               ),
               conditionalPanel(
                    condition = "input.plotMethod == 'RDPG'",
                         radioButtons(inputId='u',label="Vectors",choices=c("U","V","UV"),
                                      selected="U")
               ),
               conditionalPanel(
                    condition = "input.plotMethod == 'star'",
                         numericInput(inputId='star.center',
                                      label="Center",min=1,max=1000,
                                      value=1,step=1)
               ),
               conditionalPanel(
                    condition = "input.plotMethod == 'Laplacian'",
                    checkboxInput(inputId="scaleLaplacian","Scaled Laplacian",
                                  value=TRUE)
               ),
               conditionalPanel(
                    condition = "input.plotMethod == 'Kamada Kawai'",
                       selectInput(inputId="KKniter",label="Number of Iterations",
                            choices=c(10,100,500,1000),selected=500),
                       selectInput(inputId="KKinittemp",label="Initial Temperature",
                            choices=c(0.1,0.5,0.99,1,3,5,10,20,50,100),selected=10),
                       selectInput(inputId="KKcoolexp",label="Cooling exponent",
                            choices=c(0.1,0.5,0.99,1,3,5,10),selected=0.99)
               ),
               conditionalPanel(
                    condition = "input.plotMethod == 'Coordinates'",
                    wellPanel(
                     fluidRow(
                     column(4,
                    radioButtons(inputId="xcoordinates",
                    label="X Coordinate",
                    choices="None",selected=NULL)),
                     column(4,
                    radioButtons(inputId="ycoordinates",
                    label="Y Coordinate",
                    choices="None",selected=NULL)),
                     column(4,
                    radioButtons(inputId="zcoordinates",
                    label="Z Coordinate",
                    choices="None",selected=NULL))))
               ),
                  radioButtons(inputId='layoutD',label="Layout dimension",
                      choices=2:3,selected=2),
                  bsTooltip(id='layoutD',
                     title="Toggling the dimension may change the layout even for 2D plotting.",
                     placement='top'),
               checkboxInput('fast',"Fast Plotting",TRUE),
                checkboxInput('UseAlpha',"Use Alpha Blending",TRUE),
               conditionalPanel(
                    condition = "input.UseAlpha == true",
                    sliderInput(inputId="alphaLevel",label="Alpha Level",
                            min=0.0005,max=0.25,value=0.1,step=0.0005)
               ),
               bsTooltip(id='fast',
                   title="Fast plotting only plots vertices and straight, single edges. No loops or multiple edges. It is typically faster than the igraph plot, but less flexible.",
                         placement='top'),
         checkboxInput(inputId='sizeByVar',
                       label='Size vertices by variable',
                       value=FALSE),
         conditionalPanel(
            condition = "input.sizeByVar == false",
               sliderInput(inputId='vertexSize',
                       label="Vertex Size",min=1,max=15,
                       value=1,step=1),
      bsTooltip(id='vertexSize',
                title="The size of the dot representing the vertex.",
                placement='top')
           ),
         conditionalPanel(
            condition = "input.sizeByVar == true",
                  selectInput(inputId="vertexAttsSize",
                              label="Attribute associated with vertex size",
                  choices='None',selected='None',selectize=FALSE)
             ),
      checkboxInput(inputId='colorByVar',
         label='Color vertices by variable', value=FALSE),
         conditionalPanel(
            condition = "input.colorByVar == true",
      selectInput(inputId="vertexAttsColor",
                              label="Attribute associated with vertex color",
                  choices='None',selected='None',selectize=FALSE),
       checkboxInput('showLegend',"Show vertex color legend",FALSE)),
      conditionalPanel(
           condition = "input.fast == false",
      selectInput(inputId="vertexLabel",label="Vertex Label",
         choices="None",selected="None",selectize=FALSE),
      selectInput(inputId="edgeLabel",label="Edge Label",
         choices="None",selected="None",selectize=FALSE),
      selectInput(inputId="edgeColor",label="Color Edges by",
         choices="None",selected="None",selectize=FALSE),
       checkboxInput('useWeights',"Weight Edges",FALSE))
            ))
}

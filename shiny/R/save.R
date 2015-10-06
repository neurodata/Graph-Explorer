initializeVariables <- function()
{
   selectVariables <- data.frame(inputId="vertexAtts",
               label="Attribute to plot",
               choices='None',
               selected='None',
               stringsAsFactors=FALSE)
   selectVariables <- rbind(selectVariables,
      data.frame(inputId="edgeAtts",
        label="Attribute to plot",
        choices='None',
        selected='None',
        stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
      data.frame(inputId="communities",
        label="Communities to Compute",
       choices=paste(communities.list,collapse="|"),
       selected="Fast Greedy",
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="invariants",
       label="Invariant to Plot",
       choices=paste(plot.invariants,collapse="|"),
       selected="Degree Distribution",
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="plotMethod",
       label="Plot Method",
       choices=paste(plot.methods,collapse="|"),
       selected=plot.methods[1],
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="FRniter",
       label="Number of Iterations",
       choices=paste(c(10,100,500,1000),collapse="|"),
       selected=500,
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="FRcoolexp",
       label="Cooling exponent",
       choices=paste(c(0.1,0.5,0.99,1,3,5,10),collapse="|"),
       selected=3,
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="KKniter",
       label="Number of Iterations",
       choices=paste(c(10,100,500,1000),collapse="|"),
       selected=500,
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="KKinittemp",
       label="Initial Temperature",
       choices=paste(c(0.1,0.5,0.99,1,3,5,10,20,50,100),collapse="|"),
       selected=10,
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="KKcoolexp",
       label="Cooling exponent",
       choices=paste(c(0.1,0.5,0.99,1,3,5,10),collapse="|"),
       selected=0.99,
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="vertexAttsSize",
       label="Attribute associated with vertex size",
       choices="None",
       selected="None",
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="vertexAttsColor",
       label="Attribute associated with vertex color",
       choices="None",
       selected="None",
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="vertexLabel",
       label="Vertex Label",
       choices="None",
       selected="None",
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="edgeLabel",
       label="Edge Label",
       choices="None",
       selected="None",
       stringsAsFactors=FALSE))
   selectVariables <- rbind(selectVariables,
     data.frame(inputId="edgeColor",
       label="Color Edges by",
       choices="None",
       selected="None",
       stringsAsFactors=FALSE))

   checkboxVariables <- data.frame(inputId='usecomm',
        label='Use community for x-axis',value=FALSE,
        stringsAsFactors=FALSE)
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='dendPlot',label='Plot Dendrogram',value=FALSE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId="markGroups",label="Group the Communities", value=FALSE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='UseAlpha3D',label="Use Alpha Blending",value=TRUE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='randomZ', 
       label="Random Z (if layout doesn't provide z)",
       value=TRUE,stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='contour',label="Contour Plot",value=FALSE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='circular',label="Circular",value=FALSE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='scaleLaplacian',label="Scaled Laplacian",value=TRUE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='fast',label="Fast Plotting",value=TRUE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='UseAlpha',label="Use Alpha Blending",value=TRUE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='sizeByVar',label="Size vertices by variable",
           value=FALSE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='colorByVar',label="Color vertices by variable",
           value=FALSE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='showLegend',label="Show vertex color legend",
           value=FALSE,
     stringsAsFactors=FALSE))
   checkboxVariables <- rbind(checkboxVariables,
     data.frame(inputId='useWeights',label="Weight Edges",
           value=FALSE,
     stringsAsFactors=FALSE))

   sliderVariables <- data.frame(inputId='Cd',
           label="Embedding Dimension",min=2,max=15,
           value=5,upper.value=NA,step=1,stringsAsFactors=FALSE)
   sliderVariables <- rbind(sliderVariables,
     data.frame(inputId='CG',
           label="Communities Range",min=2,max=50,
           value=2,upper.value=10,step=1,stringsAsFactors=FALSE))
   sliderVariables <- rbind(sliderVariables,
     data.frame(inputId='Ctheta',
       label="Theta",value=0.5,upper.value=NA,min=0,max=1,step=0.1,
    stringsAsFactors=FALSE))
   sliderVariables <- rbind(sliderVariables, 
     data.frame(inputId="alphaLevel3D",label="Alpha Level",
                min=0.0005,max=.25,value=0.1,upper.value=NA,step=0.0005,
          stringsAsFactors=FALSE))
   sliderVariables <- rbind(sliderVariables, 
     data.frame(inputId='subsample',
              label="Subsampling",min=2,max=3,
              value=2,upper.value=NA,step=1,stringsAsFactors=FALSE))
   sliderVariables <- rbind(sliderVariables, 
     data.frame(inputId="damping",label="Damping",
       min=0,max=1,value=0.85,upper.value=NA,step=0.05,
       stringsAsFactors=FALSE))
   sliderVariables <- rbind(sliderVariables, 
     data.frame(inputId="theta",label="Theta",
       min=0,max=1,value=0.5,upper.value=NA,step=0.1,
       stringsAsFactors=FALSE))
   sliderVariables <- rbind(sliderVariables, 
     data.frame(inputId="alphaLevel",label="Alpha Level",
       min=0.0005,max=0.25,value=0.1,upper.value=NA,step=0.0005,
       stringsAsFactors=FALSE))
   sliderVariables <- rbind(sliderVariables, 
     data.frame(inputId="vertexSize",label="Vertex Size",
       min=1,max=15,value=1,upper.value=NA,step=1,
       stringsAsFactors=FALSE))

   checkboxGroupVariables <- data.frame(inputId='invariantsList',
          label="Invariants to Compute",
          choices=paste(invariants$Invariant,collapse="|"),
          selected=paste("Order","Size","#Components",
                     "Max Component Size",sep="|"),
            stringsAsFactors=FALSE)

  numericVariables <- data.frame(inputId="alpha",label="Alpha",
       min=0,max=1,value=0.5,step=0.1,stringsAsFactors=FALSE)
  numericVariables <- rbind(numericVariables,
    data.frame(inputId='seed',label="Random Number Seed",
      min=1,max=100000,value=seed,
      step=1,stringsAsFactors=FALSE))
  numericVariables <- rbind(numericVariables,
    data.frame(inputId='star.center',label="Center",
      min=1,max=1000,value=1,
      step=1,stringsAsFactors=FALSE))

  radioVariables <- data.frame(inputId='u',
      label="Vectors",choices=paste("U","V","UV",sep="|"),
      selected="U",stringsAsFactors=FALSE)
  radioVariables <- rbind(radioVariables,
     data.frame(inputId="xcoordinates",
        label="X Coordinate",
        choices="None",selected="None",stringsAsFactors=FALSE))
  radioVariables <- rbind(radioVariables,
    data.frame(inputId="ycoordinates",
        label="Y Coordinate",
        choices="None",selected="None",stringsAsFactors=FALSE))
  radioVariables <- rbind(radioVariables,
     data.frame(inputId="zcoordinates",
        label="Z Coordinate",
        choices="None",selected="None",stringsAsFactors=FALSE))
  radioVariables <- rbind(radioVariables,
     data.frame(inputId='layoutD',label="Layout dimension",
             choices=paste(2:3,collapse="|"),
             selected=2,stringsAsFactors=FALSE))

   list(numeric=numericVariables,
        radio=radioVariables,
        select=selectVariables,
        slider=sliderVariables,
        checkbox=checkboxVariables,
        checkboxGroup=checkboxGroupVariables)
}

numericSave <- function(data,inputId,min,max,value,step)
{
   a <- which(data$inputId==inputId)
   if(length(a)==1){
      data[a,'inputId'] <- inputId
      data[a,'min'] <- min
      data[a,'max'] <- max
      data[a,'step'] <- step
      data[a,'value'] <- value
   }
   data
}

sliderSave <- function(data,inputId,min,max,value,upper.value,step)
{
   a <- which(data$inputId==inputId)
   if(length(a)==1){
      data[a,'inputId'] <- inputId
      data[a,'min'] <- min
      data[a,'max'] <- max
      data[a,'step'] <- step
      data[a,'value'] <- value
      data[a,'upper.value'] <- upper.value
   }
   data
}

## Use also for checkboxGroup
## Use also for radioButtons
selectSave <- function(data,inputId,choices,selected)
{
   a <- which(data$inputId==inputId)
   if(length(a)==1){
      data[a,'inputId'] <- inputId
      data[a,'choices'] <- paste(choices,collapse="|")
      data[a,'selected'] <- selected
   }
   data
}

checkboxSave <- function(data,inputId,value)
{
   a <- which(data$inputId==inputId)
   if(length(a)==1){
      data[a,'inputId'] <- inputId
      data[a,'value'] <- value
   }
   data
}

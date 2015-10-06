
## Load the state data from the disk. 
loadState <- function(session,datafile,v)
{
   if(!is.null(datafile)){
     cat("Loading state data\n")
     file <- datafile$datapath
     fnames <- datafile$name
     if(length(file)>0){
        names <- load(file)
        if(names != "variables"){
           cat("File does not contain a saved state\n")
           return(v)
        }
        cat("Updating the numeric inputs\n")
        dat <- variables$numeric
        for(i in 1:nrow(dat)){
           updateNumericInput(session,
              inputId=dat[i,'inputId'],
              label=dat[i,'label'],
              min=dat[i,'min'],
              max=dat[i,'max'],
              value=dat[i,'value'],
              step=dat[i,'step'])
        }
        cat("Updating the radio buttons\n")
        dat <- variables$radio
        for(i in 1:nrow(dat)){
           choices <- unlist(strsplit(dat[i,"choices"],split="\\|"))
           updateRadioButtons(session,
              inputId=dat[i,'inputId'],
              label=dat[i,'label'],
              choices=choices,
              selected=dat[i,'selected'])
        }
        cat("Updating the select inputs\n")
        dat <- variables$select
        for(i in 1:nrow(dat)){
           choices <- unlist(strsplit(dat[i,"choices"],split="\\|"))
           updateSelectInput(session,
              inputId=dat[i,'inputId'],
              label=dat[i,'label'],
              choices=choices,
              selected=dat[i,'selected'])
        }
        cat("Updating the slider inputs\n")
        dat <- variables$slider
        for(i in 1:nrow(dat)){
           if(is.na(dat[i,'upper.value'])){
              updateSliderInput(session,
                 inputId=dat[i,'inputId'],
                 label=dat[i,'label'],
                 min=dat[i,'min'],
                 max=dat[i,'max'],
                 value=dat[i,'value'],
                 step=dat[i,'step'])
           } else {
              updateSliderInput(session,
                 inputId=dat[i,'inputId'],
                 label=dat[i,'label'],
                 min=dat[i,'min'],
                 max=dat[i,'max'],
                 value=c(dat[i,'value'],dat[i,'upper.value']),
                 step=dat[i,'step'])
           }
        }
        cat("Updating the checkbox inputs\n")
        dat <- variables$checkbox
        for(i in 1:nrow(dat)){
           updateCheckboxInput(session,
              inputId=dat[i,'inputId'],
              label=dat[i,'label'],
              value=dat[i,'value'])
        }
        cat("Updating the checkbox group inputs\n")
        dat <- variables$checkboxGroup
        for(i in 1:nrow(dat)){
           choices <- unlist(strsplit(dat[i,"choices"],split="\\|"))
           updateCheckboxGroupInput(session,
              inputId=dat[i,'inputId'],
              label=dat[i,'label'],
              choices=choices,
              selected=dat[i,'selected'])
        }
        v <- variables
        cat("Done\n")
     }
   }
   v
}


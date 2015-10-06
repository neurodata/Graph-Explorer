
## Save the state to variables
saveState <- function(input)
{
  cat("Saving state data\n")
  cat("Updating the numeric inputs\n")
  dat <- variables$numeric
  for(i in 1:nrow(dat)){
     variables$numeric[i,'value'] <- input[[dat[i,'inputId']]]
  }
  cat("Updating the radio buttons\n")
  dat <- variables$radio
  for(i in 1:nrow(dat)){
     variables$radio[i,'value'] <- input[[dat[i,'inputId']]]
  }
  cat("Updating the select inputs\n")
  dat <- variables$select
  for(i in 1:nrow(dat)){
     variables$select[i,'selected'] <- input[[dat[i,'inputId']]]
  }
  cat("Updating the slider inputs\n")
  dat <- variables$slider
  for(i in 1:nrow(dat)){
     val <- input[[dat[i,'inputId']]]
     if(length(val)==1){
        variables$slider[i,'value'] <- val
        variables$slider[i,'upper.value'] <- NA
     } else {
        variables$slider[i,'value'] <- val[1]
        variables$slider[i,'upper.value'] <- val[2]
     }
  }
  cat("Updating the checkbox inputs\n")
  dat <- variables$checkbox
  for(i in 1:nrow(dat)){
     variables$checkbox[i,'value'] <- input[[dat[i,'inputId']]]
  }
  cat("Updating the checkbox group inputs\n")
  dat <- variables$checkboxGroup
  for(i in 1:nrow(dat)){
     variables$checkboxGroup[i,'selected'] <- paste(input[[dat[i,'inputId']]],
          collapse="|")
  }
  cat("Done\n")
  variables
}

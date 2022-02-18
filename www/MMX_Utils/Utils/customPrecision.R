#########################################################################################
## THIS FILE CONTAINS THE FUNCTION TO HANDLE PRECISION FOR NUMERIC DATA FRAME DISPLAYS ##
#########################################################################################

# -----------------------------------------------------
# The UI code to be inserted in the Quick Peek section:
# -----------------------------------------------------
#
# inputPanel(
#   checkboxInput("checkbox_displayPrecision", "Suppress large decimals on display", value = TRUE)
# )
# renderUI({
#   if(!is.null(input$checkbox_displayPrecision) && input$checkbox_displayPrecision)
#     inputPanel(sliderInput("slider_precision", "Select number of decimal places to display", min = 0, max = 10, value = 4))
# })
# 
# ------------------------------------------------
# Guidelines to call and use the function defined:
# ------------------------------------------------
# 
# If currently any data frame is being rendered as: 
# shiny::renderDataTable(data, options = list(searching = F,dom='t',scrollX = T))
#
# The call needs to change to:
# shiny::renderDataTable(roundOffDataFrame(data, input$checkbox_displayPrecision, input$slider_precision), 
#                        options = list(searching = F,dom='t',scrollX = T))
#

CheckColumnTypeNew <- function(dataVector) {
  #Check if the column type is "numeric" or "character" & decide type accordDingly
  if (base::class(dataVector) == "integer" || class(dataVector) == "numeric") {
    columnType <- "numeric"
  } else { columnType <- "character" }
  #Return the result
  return(columnType)
}

roundOffDataFrame <- function(data, roundOff = FALSE, precision = 4)
{
  #
  # data: a data frame; roundOff: TRUE/FALSE/NULL; precision: numeric/integer/NULL
  #
  if(roundOff) {
    indices <- base::which(base::unname(base::unlist(base::sapply(data, CheckColumnTypeNew))) == 'numeric')
    invisible(
      lapply(indices, function(x){
        data[, x] <<- base::round(data[, x, drop=F], precision)
      })
    )
  }
  return(data)
}

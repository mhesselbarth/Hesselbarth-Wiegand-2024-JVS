#' Detection of habitat association
#'
#' Correct and false detections of the results of habitat associations
#' @param input List created with results_habitat_associations
#' @param species_type Species name
#' 
#' @return Tibble with correct and false detections

#' @export
detect_habitat_associations <- function(input, species_type){
  
  # get class id of associated habitat
  habitat <- stringr::str_extract(string = species_type, pattern = "(\\d)+") |> 
    as.numeric()
  
  # determine if positive or negative association
  association <- stringr::str_extract(string = species_type, pattern = "(?<=_).+?(?=_)")
  
  # opposite association
  opposite <- ifelse(test = association == "positive", yes = "negative", no = "positive")
  
  # count correct association
  correct <- ifelse(test = input$significance[input$habitat == habitat] == association,
                    yes = 1, no = 0)
  
  # count false associations
  false <- ifelse(test = any(input$significance[input$habitat != habitat] == association,
                             input$significance[input$habitat == habitat] == opposite, 
                             input$significance[input$habitat == habitat] == "n.s."),
                  yes = 1, no = 0)
  
  # count correct/wrong detections of habitat associations
  result_summarised <- c(correct = correct, false = false)
  
  return(result_summarised)
}

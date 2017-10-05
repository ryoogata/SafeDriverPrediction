require(dplyr)

splitData <- function(DATA, TARGET = "response", SPLIT = .1){
  # Index <- caret::createDataPartition(DATA$response, p = SPLIT, 
  #                                          list = FALSE, 
  #                                          times = 1)
  # Index <- caret::createDataPartition(DATA$response, p = SPLIT, 
  #                                          list = FALSE, 
  #                                          times = 1)
  Index <- dplyr::select(DATA, matches(TARGET)) %>%
    .[,1] %>%
    caret::createDataPartition(. 
                               ,p = SPLIT
                               ,list = FALSE
                               ,times = 1
                               )
  
  return(DATA[Index,])
}
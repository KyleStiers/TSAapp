parse_raw_output <- function(xlsx){
    temp <- c()
    
    names <- c()
    for(i in 1:8){
      names <- append(names, paste(LETTERS[i], 1:12, sep=""))
    }
    df <- data.frame(nrow=)
                     
                     
    for(i in length(xlsx[,2])){
      well_id <- xlsx[i,2]
      while(xlsx)
    }
}

parse_raw_output()
file <- read_excel("C:\\Users\\kmskvf\\Desktop\\QS3_TSA\\beamer\\500_loop_screen\\test_export\\test_500loop_export_MeltCurve Data.xls", skip=42)





wellid_to_df <- function(well_vector){
  column_ids <- c()
  if(length(well_vector) >= 1){
    for(i in 1:length(well_vector)){
      current_id <- well_vector[i]
      
      r <- switch(
        toupper(substr(current_id,1,1)),
        "A" = 1,
        "B" = 2,
        "C" = 3,
        "D" = 4,
        "E" = 5,
        "F" = 6,
        "G" = 7,
        "H" = 8
      )
      
      c <- strtoi(substr(current_id,2,3))
      current_col <- calc_well(r,c)
      #print(current_col)
      column_ids <- append(column_ids,values = current_col)
      #print(column_ids)
    }
    return(column_ids)
  }
  else{
    warning("Need more input")
  }
}

calc_well <- function(r,c){
  return((r*12)+(c-11))
}

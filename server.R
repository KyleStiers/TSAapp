require(shiny)
require(tools)
require(stats)
require(shinyjs)
require(graphics)
require(png)
require(readxl)
#require(readxl)

shinyServer(function(input, output) {
  options(shiny.maxRequestSize = 30*1024^2)
  
  observeEvent(input$help_btn, {
    alert("To use this webserver simply type in the ID of the wells you'd like to analyze into the \"Well Select\" field. You can also enter names in the \"Well Names\" field as long as the number of wells and names match. You can use output directly from the QuantStudio 3 instrument (MeltCurve Data.xlsx) with the Qs3 checkbox checked. Make sure you only exported that single sheet otherwise you'll get an error. Note: You can still use the legacy format (column 1 Temp, Columns 2->97 data) with the QS3 button unchecked.")
  })
  datainput <- reactive({
    inFile <- input$file1
    if(!is.null(inFile)){
      if(input$qs3_check == FALSE){
        data <- read.csv(inFile$datapath, header=T)
      }
      else{
        ext <- file_ext(inFile$name)
        file.rename(inFile$datapath,
                    paste(inFile$datapath, ext, sep="."))
        data<- parse_raw_output(read_excel(paste(inFile$datapath, ext, sep="."), 1, skip=42))
      }
    }
  })
  
  output$table1 <- renderTable({
    if(input$checkbox){head(datainput())}
  })
  
  output$plot1 <- renderPlot({
      userdata <- datainput()
      if(!is.null(userdata)){
        #intital values
        m <- as.matrix(userdata)
        m[m<0] <- 0
        style <- ""
        userdata <- as.data.frame(m)
        vis_min <-input$TSA_visrange[1]
        vis_max <-input$TSA_visrange[2]
        minTemp <- input$TSA_datarange[1]
        maxTemp <- input$TSA_datarange[2]
        wells <- wellid_to_df(parse_text_input(input$well_select))
        well_names <- parse_text_input(input$Well_names)
        print(well_names)
        cat("Wells: ",wells)
        if(length(wells)==0){
          filtered<- subset(userdata, userdata[,1] >= minTemp & userdata[,1] <= maxTemp)
        }
        else{
          filtered<- subset(userdata, userdata[,1] >= minTemp & userdata[,1] <= maxTemp)
          filtered<- filtered[,c(1,wells)]
        }
        
        #extract temperature data, perform normalization, setup interpolation calls to find Tms and determine style of plot
        temps <- filtered[,1]
        df <- sapply(seq(2,ncol(filtered),1),function(i) ((filtered[,i] - min(filtered[,i]))/(max(filtered[,i])-min(filtered[,i]))))
        interp_tms <- sapply(seq(1,ncol(df),1), function(i) interp(temps, df[,i],0.50)) 
        if(input$tsa_style == "line"){style <- "l"}
        else if(input$tsa_style == "both"){style <- "o"}
        else{style <- "p"}
        
        #plot results with user guided styling and legend including Tms
        matplot(temps, cbind(df[,1:ncol(df)]), col=1:ncol(df), xlim=c(vis_min,vis_max), lty=1, type=style, lwd=input$linewidth[1],pch=19, ylab="Fraction Unfolded", xlab="Temperature (°C)")
        abline(h=0.5,lty=2)
        if(length(well_names) == ncol(df-1)){
          legend("topright", title="Sample - Tm (°C)",cex=0.75,legend=paste(well_names,"-", round(interp_tms[1:length(interp_tms)],2)), pch=19, col=1:ncol(df))
        }
        else{
          legend("topright", title="Sample - Tm (°C)",cex=0.75,legend=paste(names(filtered[2:ncol(filtered)]),"-", round(interp_tms[1:length(interp_tms)],2)), pch=19, col=1:ncol(df))        
        }
        
        #repeat all above, albeit with 1st derivative being the Tm and plot
        if(input$tsa_radio == "dt"){
          diffd <- rbind(diff(df), c=(0))
          matplot(temps, cbind(diffd[,1:ncol(diffd)]),col=1:ncol(diffd), xlim=c(vis_min,vis_max), lty=1, type=style,pch=19, lwd=input$linewidth[1],ylab="dFu/dT", xlab="Temperature (°C)")
          abline(h=0.0,lty=2)
          interp_tms <- sapply(seq(1,ncol(diffd),1), function(i) interp(temps,diffd[,i],max(diffd[,i])))
          if(length(well_names) == ncol(df-1)){
            legend("topright", title="Sample - Tm (°C)",cex=0.75,legend=paste(well_names,"-", round(interp_tms[1:length(interp_tms)],2)), pch=19, col=1:ncol(df))
          }
          else{
            legend("topright", title="Sample - Tm (°C)",cex=0.75,legend=paste(names(filtered[2:ncol(filtered)]),"-", round(interp_tms[1:length(interp_tms)],2)), pch=19, col=1:ncol(df))        
          }
        }
      }
  })
  
  draw_legend <- function(df, well_names){
    
  }
    
  #interpolate T0.5s
  interp <- function(x,y,target){
    f<-approxfun(x=y,y=x)
    return (as.double((f(target))))
  }
  
  #parse the raw text input from the fields in ui
  parse_text_input <- function(sel_string){
    unlist(strsplit(sel_string,split=","))
  }
  
  #translating the current set of wells selected to column positions
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
        column_ids <- append(column_ids,values = current_col)
      }
      return(column_ids)
    }
    else{
      warning("need moar input")
    }
  }
  
  #Math to return correct position in dataframe given row and column 
  calc_well <- function(r,c){
    return((r*12)+(c-11))
  }
  
  #parse raw output from 1s3 instrument into useable format with rest of functions
  parse_raw_output <- function(xlsx){
    #Generate the well names for indexing and naming later
    well_names <- c()
    for(i in 1:8){
      well_names <- append(well_names, paste(LETTERS[i], 1:12, sep=""))
    }
    #Grab the temperature column for the whole spreadsheet
    df <- cbind(subset(xlsx, subset = xlsx$`Well Position`=="A1", select = Temperature))
    
    #Loop through the vector of well IDs and place them in the dataframe (+1 to not overwrite temps)
    for(i in 1:96){
      df[,i+1] <- cbind(subset(xlsx, subset = xlsx$`Well Position`==well_names[i], select = Fluorescence))
    }
    
    #change column names from subset back to well names and return DF
    colnames(df) <- c("Temperature",well_names)
    return(df)
  }
})
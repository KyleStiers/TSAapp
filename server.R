library(shiny)
library(stats)
library(shinyjs)
library(graphics)
library(png)

shinyServer(function(input, output) {
  datainput <- reactive({
    inFile <- input$file1
    if(!is.null(inFile)){
      data <- read.csv(inFile$datapath, header=T)
    }
  })
  
  output$table1 <- renderTable({
    if(input$checkbox){head(datainput())}
  })
  
  output$plot1 <- renderPlot({
      show("TSA")
      userdata <- datainput()
      #if dataset isn't present, do nothing
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
        wells <- wellid_to_df(parse_wells(input$well_select))
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
        legend("topright", title="Sample - Tm (°C)",cex=0.75,legend=paste(names(filtered[2:ncol(filtered)]),"-", round(interp_tms[1:length(interp_tms)],2)), pch=19, col=1:ncol(df))        
        
        #repeat all above, albeit with 1st derivative being the Tm and plot
        if(input$tsa_radio == "dt"){
          diffd <- rbind(diff(df), c=(0))
          matplot(temps, cbind(diffd[,1:ncol(diffd)]),col=1:ncol(diffd), xlim=c(vis_min,vis_max), lty=1, type=style,pch=1, lwd=input$linewidth[1],ylab="dFu/dT", xlab="Temperature (°C)")
          abline(h=0.0,lty=2)
          interp_tms <- sapply(seq(1,ncol(diffd),1), function(i) interp(temps,diffd[,i],max(diffd[,i])))
          legend("topright",title="Sample - Tm (°C)",cex=0.75,legend=paste(names(filtered[2:ncol(filtered)]),"-",round(interp_tms[1:length(interp_tms)],2)), pch=1, col=1:ncol(diffd))
        }
      }
  })
    
  
  interp <- function(x,y,target){
    f<-approxfun(x=y,y=x)
    return (as.double((f(target))))
  }
  
  parse_wells <- function(sel_string){
    unlist(strsplit(sel_string,split=","))
  }
  
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
        print(current_col)
        column_ids <- append(column_ids,values = current_col)
        print(column_ids)
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
})
options(shiny.maxRequestSize = 9*1024^2)
library(XLConnect)
library(ggplot2)

shinyServer(function(input, output) {
  blah=reactive({inFile <- input$file1
  readWorksheetFromFile(inFile$datapath, sheet=1)}) 
  
  output$contents <- renderTable({
    blah()
  })
  
  x=reactive({ 
    
    as.integer(input$region)
  #   if (input$region ==1){
  #   blah()$kerja
  # }else if (input$region==2){
  #   blah()$pertanian	
  # }else if (input$region==3){
  #   blah()$hotel
  # }else if (input$region==4){
  #   blah()$ipm
  # }else if (input$region==5){
  #   blah()$pmdn
  # }else if (input$region==6){
  #   blah()$pma
  # }else if (input$region==7){
  #   blah()$wisatawan
  # }else{
  #   blah()$penduduk	
  # }
    
    
    
  })
 
  
  
  
  
  
  
  output$plot <-renderPlot({
    if (input$select==3){

      df <- blah()
      nam.all <- names(df)[-c(1,10:12)]
      inp <- x()
      nam.checkbox <- nam.all[inp[1]]
      #nam.add <- c(nam.checkbox,"y")
      df.sub <- df[,c(nam.checkbox,"y")]

       #
       # plot(x(),blah()$y,data=blah())
       # abline(lm(blah()$y ~ x(),data=blah()))

     df <- blah()
     p <- ggplot(data = df.sub, aes(x = df.sub[,1], y = y)) +
       geom_smooth(method = "lm", se=FALSE, color="blue", size=0.5) +
       # stat_poly_eq(formula = my.formula,
       #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
       #              parse = TRUE) +
       geom_point(color="black",size=3) +
       ggtitle("Regression")

     print(p)
    }
    

    if (input$select==2){
      
      df <- blah()
      nam.all <- names(df)[-c(1,10:12)]
      inp <- x()
      nam.checkbox <- nam.all[inp]
      #nam.add <- c(nam.checkbox,"y")
      df.sub <- df[,c(nam.checkbox,"y")]
    
      gwr.bw <- gwr.sel(y~ .,
                        data=df.sub, coords=cbind(df$latitude,df$longitude))
      
      model <-  gwr(y ~ .,data = df.sub,
                    coords=cbind(df$latitude, df$longitude),
                    bandwidth = gwr.bw, hatmatrix=TRUE)
  
      # df <- blah()
      # gwr.bw <- gwr.sel(y~ kerja+pertanian+hotel+ipm+pmdn+pma+wisatawan+penduduk,
      #                   data=df, coords=cbind(df$latitude,df$longitude))
      # 
      # model <-  gwr(y ~ kerja+pertanian+hotel+ipm+pmdn+pma+wisatawan+penduduk,data = df,
      #               coords=cbind(df$latitude, df$longitude),
      #               bandwidth = gwr.bw, hatmatrix=TRUE)

      results<-as.data.frame(model$SDF)
      df.sub$coefkerja <- results$kerja
      df.sub$coefpertanian <- results$pertanian
      df.sub$coefhotel <- results$hotel
      df.sub$coefipm <- results$ipm
      df.sub$coefpmdn  <- results$pmdn
      df.sub$coefpma <- results$pma
      df.sub$coefwisatawan <- results$wisatawan
      df.sub$coefpenduduk <- results$penduduk

      # if (1 %in% input$region){
      #  reg = "kerja"
      # }else if (2 %in% input$region){
      #   reg = "pertanian"
      # }else if (3 %in% input$region){
      #   reg = "hotel"
      # }else if (4 %in% input$region){
      #   reg = "ipm"
      # }else if (5 %in% input$region){
      #   reg = "pmdn"
      # }else if (6 %in% input$region){
      #   reg = "pma"
      # }else if (7 %in% input$region){
      #   reg = "wisatawan"
      # }else{
      #   reg = "penduduk"}
      
      
      m <- length(nam.checkbox)
      
      for(i in 1:m){
        
      reg <- nam.checkbox[i]
      
      df.sub$coefvar <- results[[reg]]
      
      df.sub$latitude <- df$latitude
      df.sub$longitude <- df$longitude
      
      
      gwr.plot <- ggplot(df.sub, aes(x=latitude,y=longitude))+
      geom_point(aes(colour=coefvar))
      scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0
                                # , space = "rgb",
                               ,na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
      print(gwr.plot)
      print(reg)
      }
      
      # nie moge naysowac plotow pod soba, gdyz sie nadpisujac.....
    }
  })
  
  # output$plotGWR <-renderPlot({
  #   if (input$select==3){
  #     plot(x(),blah()$y,data=blah())
  #     abline(lm(blah()$y ~ x(),data=blah()))
  #   }
  # })
  
  # output$text0 <- renderPrint({ 
  #   print(names(df))
  #   print(summary(lm( blah()$y ~ x(), data=blah()  ) ))
  # })
  # 
  output$text1 <- renderPrint({ 
    
    if (input$select==3){

      df <- blah()
      nam.all <- names(df)[-c(1,10:12)]
      inp <- x()
      nam.checkbox <- nam.all[inp]
      nam.checkbox <- c(nam.checkbox,"y")
      df.sub <- df[,nam.checkbox] 
      
     mod <-  summary(lm( y ~ . , data=df.sub  ) )
     print(mod)
    
    print(" SIGNIFICANT VARIABLES FOR ALPHA = 0.05")
    n <- ncol(df.sub)
    
   
      
     (wh <- which( mod$coeff[-1,4] < 0.05))
     (var.nam <- row.names(mod$coeff)[-1] )
     print(var.nam[wh])

    #print(names(df)[mod$coeff[-1,4] < 0.05])
    
    #print(summary(lm( y ~ kerja+pertanian+hotel+ipm+pmdn+pma+wisatawan+penduduk, data=blah()  ) ))
    
    
    }
    
    if (input$select==1){
    #   
    # print(names(df))
    # print(summary(lm( blah()$y ~ x(), data=blah()  ) ))
    }
    
    if (input$select==2){
      
      
      df <- blah()
      nam.all <- names(df)[-c(1,10:12)]
      inp <- x()
      nam.checkbox <- nam.all[inp]
      nam.checkbox <- c(nam.checkbox,"y")
      df.sub <- df[,nam.checkbox] 
    
      gwr.bw <- gwr.sel(y~.,
                        data=df.sub, coords=cbind(blah()$latitude,blah()$longitude))
      
      model <- gwr(y ~ . ,data = df.sub,
                   coords=cbind(blah()$latitude, blah()$longitude),
                   bandwidth = gwr.bw, hatmatrix=TRUE)
    #   
    # gwr.bw <- gwr.sel(y~ kerja+pertanian+hotel+ipm+pmdn+pma+wisatawan+penduduk,
    #                   data=blah(), coords=cbind(blah()$latitude,blah()$longitude))
    # model <- gwr(y ~ kerja+pertanian+hotel+ipm+pmdn+pma+wisatawan+penduduk,data =blah(),
    #              coords=cbind(blah()$latitude, blah()$longitude),
    #              bandwidth = gwr.bw, hatmatrix=TRUE)
    print(model)
    
    print("")
    print("BFC02.gwr.test")
    
    
    print(BFC02.gwr.test(model))
    
    print("")
    print("b1/tb1")
    
    print("kerja")
    
    
    b1 <- model$SDF$kerja
    seb1 <- model$SDF$kerja_se
    tb1 <- b1/seb1
    print(tb1)
    
    print("pertanian")
    
    b1 <- model$SDF$pertanian
    seb1 <- model$SDF$pertanian_se
    tb1 <- b1/seb1
    print(tb1)
    
    print("wisatawan")
    
    b1 <- model$SDF$wisatawan
    seb1 <- model$SDF$wisatawan_se
    tb1 <- b1/seb1
    print(tb1)
    
    print("penduduk")
    
    b1 <- model$SDF$penduduk
    seb1 <- model$SDF$penduduk_se
    tb1 <- b1/seb1
    print(tb1)
    
    }
    
    })
  
  # output$text2 <- renderPrint({ 
  # 
  #   
  # })
})
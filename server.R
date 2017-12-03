#options(error = recover)
function(input, output, session) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Airports" = location2,
           "Flights" = airline)
  })
    
  ## Output Table
  output$table <- DT::renderDataTable({
    datasetInput()
  })
  
  observe({
    a <- input$level
    updateSelectInput(session, "select",
                      label = "Primary Selection:",
                      choices = switch(a,"1"=region_list,"2"=country_list,"3"=airport_list),
                      selected = switch(a,"1"="China","2"=country_list[1],"3"=airport_list[1]))
    updateSelectInput(session, "select2",
                      label = "Choose For Comparison:",
                      choices = switch(a,"1"=region_list,"2"=country_list,"3"=airport_list),
                      selected = switch(a,"1"=region_list,"2"=country_list,"3"=airport_list)[1])
    })
  
  
  observe({
    nn=switch(input$filter,"degree_centrality"=round(location$degree_centrality,0),
              "betweenness_centrality"=round(location$betweenness_centrality,1),
              "closeness_centrality"=round(location$closeness_centrality,4))
    updateSliderInput(session, "range",label = "",
                      min=min(nn),max=max(nn),value=range(nn),step=switch(input$filter,
                      "degree_centrality"= 1,
                      "closeness_centrality"=0.001)
                      )
  })
  
  df_input2<-reactive({
     if ((input$level=="1") & (input$select!="China") & (input$select2!="China"))
     subset(df, df$Tregion==input$select|df$Tregion==input$select2) %>%
     group_by("Region"=Tregion) %>%
     summarise("No.of Flights/w"=sum(Weight),"No.of Chinese Airports"=n_distinct(from),
               "No.of Oversea Airports"=n_distinct(to)) 
     else if ((input$level=="1") & (input$select=="China"|input$select2=="China"))
     df %>%
     group_by("Region"=Sregion) %>%
     summarise("No.of Flights/w"=sum(Weight),"No.of Chinese Airports"=n_distinct(from),
                "No.of Oversea Airports"=n_distinct(to)) 

     else if (input$level=="2")
     subset(df, df$Tcountry==input$select|df$Tcountry==input$select2) %>%
     group_by("Country"=Tcountry) %>%
     summarise("No.of Flights/w"=sum(Weight),"No.of Chinese Airports"=n_distinct(from),
               "No.of Airports"=n_distinct(to))
    
     else if ((input$level=="3") & ((input$select %in% df$Sairport) & (input$select2 %in% df$Sairport)))
     subset(df, df$Sairport==input$select|df$Sairport==input$select2) %>%
     group_by("Airport"=Sairport) %>%
     summarise("No.of Flights/w"=sum(Weight),"No.of Connected Airports"=n_distinct(Tairport))
     else if ((input$level=="3") & ((input$select %in% df$Tairport) & (input$select2 %in% df$Tairport)))
     subset(df, df$Tairport==input$select|df$Tairport==input$select2) %>%
     group_by("Airport"=Tairport) %>%
     summarise("No.of Flights/w"=sum(Weight),"No.of Connected Airports"=n_distinct(Sairport))
     else
     subset(df, df$Sairport==input$select|df$Tairport==input$select) %>%
     rowwise() %>%
     mutate("Airport"=add(Tairport,Sairport,input$select)) %>%
     mutate("Connection"=add2(Tairport,Sairport,input$select)) %>%
     group_by(Airport) %>%
     summarise("No.of Flights/w"=sum(Weight),"No.of Connected Airports"=n_distinct(Connection))
  })
  
  df_input3<-reactive({
      if ((input$level=="1") & !("China" %in% input$select) & !("China" %in% input$select2))
      subset(flight3, flight3$Tregion==input$select|flight3$Tregion==input$select2) %>%
      group_by("by"=Tregion,Airline) %>%
      summarise("Flights"=sum(value)) %>%
      group_by(by) %>%
      mutate(percent=round(100*Flights/sum(Flights))) %>%
      arrange(desc(percent),.by_group=TRUE)

      else if ((input$level=="1") & (input$select=="China"|input$select2=="China"))
      flight3 %>%
      group_by("by"=Sregion,Airline) %>%
      summarise("Flights"=sum(value)) %>%
      group_by(by) %>%
      mutate(percent=round(100*Flights/sum(Flights))) %>%
      arrange(desc(percent),.by_group=TRUE)
    
      else if (input$level=="2")
      subset(flight3, flight3$Tcountry==input$select|flight3$Tcountry==input$select2) %>%
      group_by("by"=Tcountry,Airline) %>%
      summarise("Flights"=sum(value)) %>%
      group_by(by) %>%
      mutate(percent=round(100*Flights/sum(Flights))) %>%
      arrange(desc(percent),.by_group=TRUE) 
    
      else if ((input$level=="3") & ((input$select %in% df$Sairport) & (input$select2 %in% df$Sairport)))
      subset(flight3, flight3$Sairport %in% list(input$select)|flight3$Sairport %in% list(input$select2)) %>%
      group_by("by"=Sairport,Airline) %>%
      summarise("Flights"=sum(value)) %>%
      group_by(by) %>%
      mutate(percent=round(100*Flights/sum(Flights))) %>%
      arrange(desc(percent),.by_group=TRUE)

      else if ((input$level=="3") & ((input$select %in% df$Tairport) & (input$select2 %in% df$Tairport)))
      subset(flight3, flight3$Tairport %in% list(input$select)|flight3$Tairport %in% list(input$select2)) %>%
      group_by("by"=Tairport,Airline) %>%
      summarise("Flights"=sum(value)) %>%
      group_by(by) %>%
      mutate(percent=round(100*Flights/sum(Flights))) %>%
      arrange(desc(percent),.by_group=TRUE)

      else if (input$level=="3" & input$select %in% df$Tairport & input$select2 %in% df$Sairport)
      subset(flight3, flight3$Tairport %in% list(input$select)|flight3$Sairport %in% list(input$select)) %>%
      mutate("by"=add(Tairport,Sairport,input$select)) %>%
      mutate("Connection"=add2(Tairport,Sairport,input$select)) %>%
      group_by(by,Airline) %>%
      summarise("Flights"=sum(value)) %>%
      group_by(by) %>%
      mutate(percent=round(100*Flights/sum(Flights))) %>%
      arrange(desc(percent),.by_group=TRUE)

      else if (input$level=="3" & input$select %in% df$Sairport & input$select2 %in% df$Tairport)
      subset(flight3, flight3$Tairport %in% list(input$select)|flight3$Sairport %in% list(input$select)) %>%
      mutate("by"=add(Tairport,Sairport,input$select)) %>%
      mutate("Connection"=add2(Tairport,Sairport,input$select)) %>%
      group_by(by,Airline) %>%
      summarise("Flights"=sum(value)) %>%
      group_by(by) %>%
      mutate(percent=round(100*Flights/sum(Flights))) %>%
      arrange(desc(percent),.by_group=TRUE)
  })

  # output$tb <- DT::renderDataTable({
  #   datatable(
  #     df_input2(),
  #     options = list(colnames= NULL,searching = FALSE,lengthChange = FALSE,info=FALSE,ordering=FALSE,paginate=FALSE)
  #     )
  # })
  
  output$tb <- renderTable({
      df_input2()
  })
  
  
  output$bar1 <- renderPlot({
    if ((!("China" %in% input$select) & !("China" %in% input$select2))|(("China" %in% input$select) & !("China" %in% input$select2))|(("China" %in% input$select) & ("China" %in% input$select2))){
    k=df_input3()[df_input3()$by==input$select,]
    n=min(5,nrow(k))
    ggplot(k[c(1:n),], aes(y=percent, x=reorder(Airline,percent))) +
      labs(title=input$select)+
      geom_bar(stat="identity",fill="steelblue3") + coord_flip() +
      geom_text(aes(label=percent), hjust=1.5, colour="white",size=4)+
      
      theme(axis.title.y=element_blank(),panel.background = element_blank(),
            plot.title = element_text(hjust = 1),
            axis.ticks = element_blank(),
            axis.text.x =element_blank(),
            axis.text.y =element_text(size=12)
            )
    }
  
  })
  
  output$bar2 <- renderPlot({
    if (input$level=="3" & input$select %in% df$Tairport & input$select2 %in% df$Sairport) {plot.new()}
    else if (input$level=="3" & input$select %in% df$Sairport & input$select2 %in% df$Tairport) {plot.new()}
    else if ((!("China" %in% input$select) & !("China" %in% input$select2))|(!("China" %in% input$select) & ("China" %in% input$select2))|(("China" %in% input$select) & ("China" %in% input$select2))){
    kk=df_input3()[df_input3()$by==input$select2,]
    nn=min(5,nrow(kk))
    ggplot(kk[c(1:nn),], aes(y=percent, x=reorder(Airline,percent))) +
      labs(title=input$select2)+
      geom_bar(stat="identity",fill="steelblue3") + coord_flip() +
      geom_text(aes(label=percent), hjust=1.5, colour="white",size=4)+
      theme(axis.title.y=element_blank(),panel.background = element_blank(),
            plot.title = element_text(hjust = 1),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12)
            )
    }
    
  })
  
  
  
  output$tree <- renderD3tree2({
    
    a <- airline[,1:3] %>%
      group_by(Country,Airline)%>%
      mutate(count = n())
    
    company <- unique(a)
    
    tm <- treemap(company,
                  index=c("Country","Airline"),
                  vSize="count",
                  vColor="count",
                  type="index",
                  palette="Blues",
                  overlap.labels = 0,
                  lowerbound.cex.labels = .4,
                  bg.labels = 0,
                  range=c(-50000,50000),
                  fun.aggregate="mean")
    
    d3tree2(data = tm, rootname = "All")
  })
  
  v <- reactiveValues(msg = "")
  
  observeEvent(input$tree_click, {
    v$msg <- paste("(Selected", input$tree_click$name,")")
  })
  
  output$clickedinfo <- renderText(v$msg)
  
  
  output$dependent1 <- renderUI({
    selectInput("select_airline","Select Airlines", choices = unique(airline[,input$select_country]), selected = "Air China")
  })
  
  
  
  output$bar3 <- renderPlot({
    
    df<-data.frame(airline2)
    df <- df[df$Airline == input$select_airline,]
    
    dep<-as.data.frame(table(df[,4]))%>%
            arrange(desc(Freq))
    
    
    nn=min(10,nrow(dep))
    
    ggplot(dep[c(1:nn),],aes(x=reorder(Var1,Freq),y=Freq))+
      geom_bar(stat="identity",fill="steelblue3",width=0.8) + 
      geom_text(aes(label=Freq), hjust=1.5, colour="white",size=4)+
      coord_flip()+
      theme(axis.title.y=element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.6),
            axis.ticks = element_blank()
      )
    
  })
  
  
  output$bar4 <- renderPlot({
    
    df<-data.frame(airline2)
    df <- df[df$Airline == input$select_airline,]
    
    arr<-as.data.frame(table(df[,5]))%>%
      arrange(desc(Freq))
    
    nn=min(10,nrow(arr))
    
    ggplot(arr[c(1:nn),],aes(x=reorder(Var1,Freq),y=Freq))+
      geom_bar(stat="identity",fill="steelblue3",width=0.8) + 
      geom_text(aes(label=Freq), hjust=1.5, colour="white",size=4)+
      coord_flip()+
      theme(axis.title.y=element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.6),
            axis.ticks = element_blank()
      )
    
    
  })
  
  
  output$plt2<-renderPlotly({
    
    dd <-data.frame(airline2)
    
    if(input$tree_click$name == "All"){
      dd<-dd
    }
    
    else if(input$tree_click$name %in% unique(dd$Country)){
      dd <- dd[dd$Country == input$tree_click$name,]
    }
    
    
    else{
      dd <- dd[dd$Airline == input$tree_click$name,]
    }
    
    
    df_airline<-as.data.frame(table(dd[,1]))%>%
      arrange(desc(Freq))
    
    nnn=min(10,nrow(df_airline))
    
    plot_ly(data=df_airline[c(1:nnn),],x = ~Freq, y = ~reorder(Var1,Freq),
            type = 'bar', orientation = 'h', marker = list(color = 'steelblue3',width=0.5)) %>%
      layout(yaxis = list(title="",margin=list(r=10),showgrid = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
             xaxis = list(title="No.of Air Routes in 2017",zeroline = FALSE, showticklabels = TRUE, showgrid = TRUE),
             title = 'Airlines Performances - No.of Flights (TOP10)',
             margin = list(l=160,r=80,t=80, b = 40))
    
  })
  
  
  
  
  output$map2 <- renderLeaflet({
    
    ### Define data frame
    df<-data.frame(airline2)
    df <- df[df$Airline == input$select_airline,]
    
    df_edge <- df[,c(4,5)] 
    df_node <- location2
    
    china_flight <- tbl_graph(nodes = df_node, edges = df_edge, directed = FALSE)
    
    m<-china_flight %>%  
          mutate(degree_centrality = centrality_degree())
    df_node<-as.data.frame(m)
    
    dff_edge<-df_edge %>%
                 group_by(Dep,Arr)%>%
                 summarise(n())
    
    colnames(dff_edge)=c("from","to","Freq")
    meta <- data.frame("name"=df_node$IATA, 
                       "lon"=df_node$LONG,  
                       "lat"=df_node$LAT)
    
    g <- graph.data.frame(dff_edge, directed=FALSE, vertices=meta)
    gg <- get.data.frame(g, "both")
    vert <- gg$vertices
    coordinates(vert) <- ~lon+lat
    select_edges <- gg$edges
    select_edges <- lapply(1:nrow(select_edges), function(i) {
      as(rbind(vert[vert$name == select_edges[i, "from"], ], 
               vert[vert$name == select_edges[i, "to"], ]), "SpatialLines")
    })
    
    
    for (i in seq_along(select_edges)) {
      select_edges[[i]] <- spChFIDs(select_edges[[i]], as.character(i))
    }
    select_edges <- do.call(rbind, select_edges)
    
    
    pop_edge_2 <- paste0("<span style='color: #7f0000'><strong>Basic Information</strong></span>",
                         "<br><strong>Origin: </strong>",             
                         dff_edge$from,         
                         "<br><strong>Destination: </strong>", 
                         dff_edge$to,
                         "<br><strong>Number of Flights: </strong>", 
                         dff_edge$Freq
    )
    
    pop_node_2 <- paste0("<span style='color: #7f0000'><strong>Basic Information</strong></span>",
                         "<br><strong>Name: </strong>",
                         df_node$Airport,
                         "<br><strong>Country: </strong>",
                         df_node$Country,"|",df_node$Region,
                         "<br><strong>Degree: </strong>",
                         df_node$degree_centrality
    )
    
    colorpal <- colorFactor("Paired", df_node$Region)
    
    
    leaflet(options = leafletOptions(minZoom = 3, maxZoom = 18)) %>% 
      addTiles(group = "Default")%>% 
      setView(lng = 120, lat = 37, zoom = 3) %>% 
      addPolylines(data = select_edges,color="red", 
                   weight=sqrt(dff_edge$Freq), opacity=0.2, popup= pop_edge_2,
                   highlightOptions = highlightOptions(color = "red", 
                                                       weight = 5,
                                                       opacity=0.4,
                                                       bringToFront = TRUE)) %>%
      addCircleMarkers(
        lng = df_node$LONG, lat = df_node$LAT,
        radius = sqrt(df_node$degree_centrality),

        color = colorpal(df_node$Region),
        stroke = FALSE, 
        fillOpacity = 0.7,
        popup= pop_node_2
      )
    
    #addLegend("bottomleft",pal=~colorpal(),title = "Legend",opacity = 0.8)
    
  })
  
  output$map <-renderLeaflet({
    a<-input$level
    if (a==1) {
      file<-selected2
      if (input$select=="China") {df_input=df}
      else {
        if (nrow(df[df$Tregion==input$select,])>0) {df_input=df[df$Tregion==input$select,]}
        else {df_input=df}
      }
    }
    else if (a==2) {
      file<-selected
      if (nrow(df[df$Tcountry==input$select,])>0) {df_input=df[df$Tcountry==input$select,]}
      else {df_input=df}
      }
    
    else if (a==3) {
      file<-selected
      temp=df[df$Tairport==input$select|df$Sairport==input$select,]
      if (nrow(temp)>0) {df_input=temp}
      else {df_input=df}
      }

    loc<-reactive({
      if (input$filter=="degree_centrality")
        subset (location,degree_centrality>=input$range[1]&degree_centrality<=input$range[2])
      else if (input$filter=="betweenness_centrality")
        subset (location,betweenness_centrality>=input$range[1]&betweenness_centrality<=input$range[2])
      else
        subset (location,closeness_centrality>=input$range[1]&closeness_centrality<=input$range[2])
    })
    
    dff<-reactive({
      subset(df_input,(df_input$to %in% loc()$IATA)&(df_input$from %in% loc()$IATA))
    })
    
    meta <- reactive({
      data.frame("name"=loc()$IATA, 
                 "lon"=loc()$LONG,  
                 "lat"=loc()$LAT)
    })
    
    # Make into graph to spatial object 
    g <- graph_from_data_frame(dff(), directed=FALSE, vertices=meta())
    gg <- as_data_frame(g, "both")
    vert <- gg$vertices
    coordinates(vert) <- ~lon+lat
    #dots<-gcIntermediate(edges)
    edges <- gg$edges
    if (nrow(dff())>0) {
    edges <- lapply(1:nrow(edges), function(i) {
      as(rbind(vert[vert$name == edges[i, "from"], ], 
               vert[vert$name == edges[i, "to"], ]), "SpatialLines")
    })
    
    for (i in seq_along(edges)) {
      edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
    }
    
    edges <- do.call(rbind, edges)
    }
    
    colorpal <- reactive({
      colorFactor(input$colors, loc()$Region)
    })
    
    pal <- colorpal()
    
    pop_node <- paste0("<span style='color: #7f0000'><strong>Basic Information</strong></span>",
                       "<br><strong>Name: </strong>",
                       loc()$Airport,
                       "<br><strong>Country: </strong>",
                       loc()$Country,"|",loc()$Region,
                       "<br><span style='color: #7f0000'><strong>Centrality</strong></span>",
                       "<br><strong>Betweenness: </strong>",
                       round(loc()$betweenness_centrality,2),
                       "<br><strong>Closeness: </strong>",
                       round(loc()$closeness_centrality,2),
                       "<br><strong>Degree: </strong>",
                       loc()$degree_centrality
    )
    
    pop_edge <- paste0("<span style='color: #7f0000'><strong>Basic Information</strong></span>",
                       "<br><strong>Origin: </strong>",             
                       dff()$Sairport,         
                       "<br><strong>Destination: </strong>", 
                       dff()$Tairport,     
                       "<br><strong>Number of Flights: </strong>", 
                       dff()$Weight  
    )
    
    
    xx <- leaflet(file,options = leafletOptions(minZoom = 3, maxZoom = 18)) %>%
      setView(lng = 120, lat = 37, zoom = 3) %>% 
      addTiles(group = "Default") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "DarkMatter") %>%
      addProviderTiles(providers$OpenStreetMap.DE, group = "OpenStreet") %>%
      # addMarkers(data = vert,icon=Icon,label = ~as.character(location$IATA),clusterOptions = markerClusterOptions()) %>% 
      addCircleMarkers(lng = loc()$LONG, lat = loc()$LAT, group = "Airport", 
                 radius = ~switch(input$size,
                                  "degree_centrality"=sqrt(loc()$degree_centrality)*3,
                                  "betweenness_centrality"=sqrt(loc()$betweenness_centrality)*0.6,
                                  "closeness_centrality"=sqrt(loc()$closeness_centrality*100000*0.8)),
                 fillOpacity=0.6,label=~loc()$Airport,
                 color = ~pal(loc()$Region), 
                 layerId = loc()$IATA,
                 popup= pop_node,
                 stroke = FALSE
                 #highlightOptions = highlightOptions(color = "red",bringToFront = TRUE)
                 )   %>%
      addLegend("bottomleft",pal=colorpal(),values = ~as.character(loc()$Region),title = "Legend",opacity = 0.8) %>%
      addLayersControl(
        baseGroups = c("OpenStreet","Default", "DarkMatter"),
        overlayGroups = c("Airport", "Country","Routes"),
        options = layersControlOptions(collapsed = TRUE)
      )
    if (input$level==3&nrow(dff())>0) {
      xx %>%
      addPolylines(data = edges,color="red", group = "Routes",weight=flight$Weight/20 , opacity = 0.2, popup=pop_edge, 
                   highlightOptions = highlightOptions(color = "red", weight = 4,bringToFront = TRUE))
    }else if (nrow(dff())==0) {
      xx
    }
    else{
      xx %>%
        addPolylines(data = edges,color="red", group = "Routes",weight=flight$Weight/20 , opacity = 0.2, popup=pop_edge, 
                     highlightOptions = highlightOptions(color = "red", weight = 4,bringToFront = TRUE)) %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.9,group = "Country",
                    opacity = 1.0, fillOpacity = 0.05,
                    fillColor = NA, layerId=switch(input$level,"1"=~Region,"2"=~NAME),
                    label=switch(input$level,"1"=~Region,"2"=~NAME),
                    highlightOptions = highlightOptions(color = "red", weight = 4,bringToFront = TRUE)) 
    }
  })
    
    output$plt<-renderPlotly({
      if (input$level2==1 & input$VAR==1){
      p <- ggplot(region_analysis, aes(y=`No.of Air Routes`, x=reorder(Region,`No.of Air Routes`))) + 
           geom_bar(aes(fill=Year),position="dodge",width = 0.4,stat="identity") +
           theme(legend.position="bottom")+
           theme(axis.text.x = element_text(angle = 25, hjust = 1))+
           theme(plot.margin=unit(c(1,1,2,1.8),"cm"))+
           #geom_text(aes(label=`No.of Air Routes`),position=position_dodge(width=1),hjust=0,vjust=-0.5,angle=0)
           labs(title="No. of Air Routes in each B&R Region",x="Region")
      ggplotly(p,tooltip="y",width=700)
      }
      
      else if (input$level2==1 & input$VAR==2){
      p <- ggplot(region_analysis, aes(y=`No.of Flights`, x=reorder(Region,`No.of Flights`))) + 
           geom_bar(aes(fill=Year),position="dodge",width = 0.4,stat="identity") +
           theme(legend.position="bottom")+
           #geom_text(aes(label=`No.of Air Routes`),position=position_dodge(width=1),hjust=0,vjust=-0.5,angle=0)
           theme(axis.text.x = element_text(angle = 25, hjust = 1))+
           theme(plot.margin=unit(c(1,1,2,1.8),"cm"))+
           labs(title="Total Flights in each B&R Region",x="Region")
      ggplotly(p,tooltip="y",width=700)
      }
      
      else if (input$level2==2 & input$VAR==1)
      plot_ly(data = country_analysis2, type="scatter",x = ~`2017`, y = ~Growth,
                marker = list(size = ~sqrt(Growth)*5, color = ~Growth),
              text=~Country,hoverinfo="x+y+text") %>%
      layout(title = 'No.of Air Routes in each B&R Country',width=600,
               margin = list(t=30, b = 40),
               yaxis = list(zeroline = FALSE,title="Air Route Increase"),
               xaxis = list(zeroline = FALSE,title="No.of Air Routes in 2017",type="log",autorange=TRUE))
      
      else if (input$level2==2 & input$VAR==2)
      plot_ly(data = country_analysis1, type="scatter",x = ~`2017`, y = ~Growth,
                marker = list(size = ~sqrt(Growth)*2, color = ~Growth),
                text=~Country,hoverinfo="x+y+text") %>%
      layout(title = 'Total Flights in each B&R Country',width=600,
               margin = list(t=30, b = 40),
               yaxis = list(zeroline = FALSE,title="Flights Increase"),
               xaxis = list(zeroline = FALSE,title="Total Flights in 2017",type="log",autorange=TRUE))
      
      else if (input$level2==3 & input$VAR==1)
      plot_ly(data=airport_analysis1,x = ~`No.of Air Routes`, y = ~reorder(Airport,`No.of Air Routes`),
                type = 'bar', orientation = 'h', marker = list(color = 'steelblue3',width=0.5)) %>%
        layout(yaxis = list(title="Airport",margin=list(r=10),showgrid = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
               xaxis = list(title="No.of Air Routes in 2017",zeroline = FALSE, showticklabels = TRUE, showgrid = TRUE),
               title = 'Chinese Airport Performances--No.of Air Routes (TOP10)',
               margin = list(l=320,t=30, b = 40),width=800)
      
      else if (input$level2==3 & input$VAR==2)
        plot_ly(data=airport_analysis2,x = ~`No.of Flights`, y = ~reorder(Airport,`No.of Flights`),
                type = 'bar', orientation = 'h', marker = list(color = 'steelblue2',width=0.5)) %>%
        layout(yaxis = list(title="Airport",margin=list(r=10),showgrid = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
               xaxis = list(title="No.of Flights in 2017",zeroline = FALSE, showticklabels = TRUE, showgrid = TRUE),
               title = 'Chinese Airport Performances--No.of Flights (TOP10)',
               margin = list(l=320,t=30, b = 40),width=800)
      })
}
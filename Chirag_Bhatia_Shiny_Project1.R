# Clear workspace 
rm(list = ls())

library(shiny)
library(shinythemes)
library(networkD3)
library(sna)
library(plyr)
library(markdown)
library(igraph)
library(sqldf)
library(visNetwork)

# Define UI ----
  ui = fluidPage(theme = shinytheme("cyborg"),
  
  # App title ----
    titlePanel("Chirag_Bhatia_Social_Network_Analysis"),
  
    # Sidebar layout with input and output definitions ----
      sidebarLayout(
    
      # Sidebar panel for inputs ----
        sidebarPanel(
      
        # Input1: Select file with connections ----
          fileInput("file1", label = h4("Select File with Connection(s)"), 
          multiple = FALSE, accept = c("text/csv",
          "text/comma-separated-values,text/plain",".csv")),
      
        # Input2: Select file with Node Departmental Information ----
          fileInput("file2", label = h4("Select File with Department Labels"), 
          multiple = FALSE, accept = c("text/csv",
          "text/comma-separated-values,text/plain",".csv")),
        
        # Horizontal line ----
          hr(),
        
        # Input3: Specify the number of connections to view for Task 3
          numericInput("obs", h4("Enter Number of Connections to View for Task T3:"), 10)
          #actionButton("update", "Update View"),
      
        ),
    
        mainPanel(
        navbarPage("ALL TASKS ARE DISPLAYED IN SEPARATE TABS || ABBREVIATED ACCORDING TO THEIR TASK NUMBER",
      
        #tabsetPanel(
          tabPanel("T3: Display 'n' Connections",
            simpleNetworkOutput("Graph1", width = "100%", height = "500px")
            #htmlOutput('simple')
          ),
        
          tabPanel("T4: Number of Emails Sent",
            tableOutput("Table1")
          ),
  
          tabPanel("T5:Number of Emails Received",
            tableOutput("Table2")
          ),
          
          tabPanel("T6a: 2-hop Neighbor Plot", h4("2-hop Neighbor Visualization for Top 10 Members who sent most number of Emails"), forceNetworkOutput("twohop1")
          ),
          
          tabPanel("T6b: 2-hop Neighbor Plot", h4("2-hop Neighbor Visualization for Top 10 Members who received most number of Emails"),forceNetworkOutput("twohop2")
          ),
          
          tabPanel("T7: Degree Centrality", h4("2-hop Neighbor Visualization for Top 10 Members with highest Degree Centrality"),forceNetworkOutput("twohop3")
          ),
          
          tabPanel("T8: Betweenness", h4("2-hop Neighbor Visualization for Top 10 Members with highest Degree Betweenness"),forceNetworkOutput("twohop4")
          ),
          
          tabPanel("T9: Indegree Centrality", h4("2-hop Neighbor Visualization for Top 10 Members with highest Indegree Centrality"),forceNetworkOutput("twohop5")
          ),
          
          tabPanel("T10a: Department-level Aggregation", h4("Table: Emails sent by Members aggregated to Department Level"),tableOutput("Table3")
          ),
          
          tabPanel("T10b: Department-level Aggregation", h4("Network Visualization: Email activity amongst Departments"),forceNetworkOutput("Graph2")
          ),
        
          tabPanel("T11: Description", h5("The assigned connection dataset for this project had 25,571 observations. This kind of a dataset offered extreme complexity not in terms of visualization, 
                                          but in terms of delivering understandable visualization. Even though the visualizations of task 7,8,9 were kept limited to just 2-hop neighbors of Top 10 members,
                                          the output network graph was seen similar to a hairball."), h5("If the number of observations in input file is reduced few hundreds, effective visualization is obtained."),
                                       h5("When compared with significantly less number of connection rows, it seems that there is more connectivity among nodes in outputs from Task 7 & Task 8 than Task 9.")
          )
        #) 
      )
    )
  )
)

# Define server logic ----
  server <- function(input, output) {
  

#--------------Task 3: Display First 'N' connections------------------------------------------

  output$Graph1=renderSimpleNetwork({
    input1 = input$file1
    if (is.null(input1))
      return(NULL)
    dataframe1 = read.table(input1$datapath, nrows=input$obs)
    source1=subset.data.frame(dataframe1, select=V1)
    target1=subset.data.frame(dataframe1, select=V2)
    networkData=data.frame(source1, target1)
    
    #Plotting 2-hop neighbors with NetworkD3
    simpleNetwork(networkData, zoom=T,
    nodeColour = "blue", 
    height=300,
    width=300,
    charge=-5,
    fontSize = 12,
    fontFamily = "fantasy",
    opacity=5)
  })
  
#--------------Task 4: Programmatically compute the number of emails sent by each person------------------------------------------
    
  output$Table1=renderTable({
    
    #File Input
    input2 = input$file1
    if (is.null(input2))
      return(NULL)
    
    dataframe2 = read.table(input2$datapath)
    Member=subset.data.frame(dataframe2, select=V1)
    Table1=as.data.frame(table(Member), responseName="Number_of_Emails_Sent")
    
  })
    
    #y = count(df, V1)
    
#--------------Task 5: Programmatically compute the number of emails received by each person------------------------------------------
    
  output$Table2=renderTable({

    #File Input
    input3 = input$file1
    if (is.null(input3))
      return(NULL)
    
    dataframe3 = read.table(input3$datapath)
    Member=subset.data.frame(dataframe3, select=V2)
    Table2=as.data.frame(table(Member), responseName="Number_of_Emails_Received")
  
  })
    
    #y = count(df, V1)
    
#--------------Task 6: Display up to 2-hop neighbors of the top 10 from (4) and (5)------------------------------------------
# Part 1
  
  output$twohop1=renderForceNetwork({  
  
    #File Input
    input4 = input$file1
    if (is.null(input4))
      return(NULL)
    
    dataframe4 = read.table(input4$datapath)
    colnames(dataframe4)=c("person_from","person_to")
    Person=subset.data.frame(dataframe4, select=person_from)
    Table_temp=as.data.frame(table(Person))
   
    #Sort by descending order and identifying top 10
    Table3=head(Table_temp[order(-Table_temp$Freq),],n=10) 
    vector_of_Table3=as.vector(Table3$Person)
    graph_Table3=graph_from_data_frame(dataframe4, directed = F)
   
    #Identifying upto 2-hop Neighbors
    neighbor_list=make_ego_graph(graph_Table3, order = 2, nodes = vector_of_Table3, mode = "all") 
    union_of_graph=graph.union(neighbor_list[[1]],neighbor_list[[2]], neighbor_list[[3]], neighbor_list[[4]], neighbor_list[[5]], neighbor_list[[6]], neighbor_list[[7]], neighbor_list[[8]], neighbor_list[[9]], neighbor_list[[10]], byname = T)
    
    #Simplifying to remove multiple edges
    simplified_graph=simplify(union_of_graph, remove.multiple = T) 
    c=cluster_walktrap(simplified_graph)
    m=membership(c)
    graph_object=igraph_to_networkD3(simplified_graph, group = m, what = "both")
    
    #Plotting 2-hop neighbors with NetworkD3
    forceNetwork(Links = graph_object$links, Nodes = graph_object$nodes,
                 Source = 'source', Target = 'target', NodeID = 'name',
                 Group = 'group', zoom = T, linkWidth = -1.5, opacity = 0.6, fontSize = 20, fontFamily = "serif")  
  })
  
#--------------Task 6: Display up to 2-hop neighbors of the top 10 from (4) and (5)------------------------------------------
# Part 2
  
  output$twohop2=renderForceNetwork({  
    
    #File Input
    input5 = input$file1
    if (is.null(input5))
      return(NULL)
    
    dataframe5 = read.table(input5$datapath)
    colnames(dataframe5)=c("person_from","person_to")
    Person=subset.data.frame(dataframe5, select=person_to)
    Table_temp=as.data.frame(table(Person))
    
    #Sorting by descending order and identifying top 10
    Table3=head(Table_temp[order(-Table_temp$Freq),],n=10) 
    vector_of_Table3=as.vector(Table3$Person)
    graph_Table3=graph_from_data_frame(dataframe5, directed = F)
    
    #Identifying upto 2-hop Neighbors
    neighbor_list=make_ego_graph(graph_Table3, order = 2, nodes = vector_of_Table3, mode = "all") 
    union_of_graph=graph.union(neighbor_list[[1]],neighbor_list[[2]], neighbor_list[[3]], neighbor_list[[4]], neighbor_list[[5]], neighbor_list[[6]], neighbor_list[[7]], neighbor_list[[8]], neighbor_list[[9]], neighbor_list[[10]], byname = T)
    
    #Simplifying to remove multiple edges
    simplified_graph=simplify(union_of_graph, remove.multiple = T) 
    c=cluster_walktrap(simplified_graph)
    m=membership(c)
    graph_object=igraph_to_networkD3(simplified_graph, group = m, what = "both")
    
    #Plotting 2-hop neighbors with NetworkD3
    forceNetwork(Links = graph_object$links, Nodes = graph_object$nodes,
                 Source = 'source', Target = 'target', NodeID = 'name',
                 Group = 'group', zoom = T, linkWidth = -1.5, opacity = 0.6, fontSize = 20, fontFamily = "serif")  
  })  
  
  
#--------Task 7: Display/visualize up to 2-hop neighbors of 10 people with the highest centrality--------------------------------
  
  output$twohop3=renderForceNetwork({  
    
    #File Input
    input6 = input$file1
    if (is.null(input6))
      return(NULL)
    
    dataframe6 = read.table(input6$datapath)
    colnames(dataframe6)=c("person_from","person_to")
    graph_Table3=graph_from_data_frame(dataframe6, directed = F)
    
    #Calculating Degree Centrality of each node
    deg=degree(graph_Table3, v=V(graph_Table3), mode="all") 
    deg_df=as.data.frame(deg)
    temp=row.names(deg_df)
    new=cbind(temp, deg_df)
    
    #Sorting by descending order and identifying top 10
    Table3=head(new[order(-new$deg),],n=10) 
    vector_of_Table3=as.vector(Table3$temp)
    
    #Identifying upto 2-hop Neighbors with highest centrality
    neighbor_list=make_ego_graph(graph_Table3, order = 2, nodes = vector_of_Table3, mode = "all") 
    union_of_graph=graph.union(neighbor_list[[1]],neighbor_list[[2]], neighbor_list[[3]], neighbor_list[[4]], neighbor_list[[5]], neighbor_list[[6]], neighbor_list[[7]], neighbor_list[[8]], neighbor_list[[9]], neighbor_list[[10]], byname = T)
    
    #Simplifying to remove multiple edges
    simplified_graph=simplify(union_of_graph, remove.multiple = T) 
    c=cluster_walktrap(simplified_graph)
    m=membership(c)
    graph_object=igraph_to_networkD3(simplified_graph, group = m, what = "both")
    
    #Plotting 2-hop neighbors with NetworkD3
    forceNetwork(Links = graph_object$links, Nodes = graph_object$nodes,
                 Source = 'source', Target = 'target', NodeID = 'name',
                 Group = 'group', zoom = T, linkWidth = -1.5, opacity = 0.6, fontSize = 20, fontFamily = "serif")  
  })    
  
#--------Task 8: Display/visualize up to 2-hop neighbors of 10 people with the highest betweenness--------------------------------
  
  output$twohop4=renderForceNetwork({  
    
    #File Input
    input7 = input$file1
    if (is.null(input7))
      return(NULL)
    
    dataframe7 = read.table(input7$datapath)
    colnames(dataframe7)=c("person_from","person_to")
    graph_Table3=graph_from_data_frame(dataframe7, directed = F)
    
    #Calculating Betweenness of each node
    bet=betweenness(graph_Table3, v=V(graph_Table3), directed = F) 
    bet_df=as.data.frame(bet)
    temp=row.names(bet_df)
    new=cbind(temp, bet_df)
    
    #Sorting by descending order and identifying top 10 nodes with highest betweenness
    Table3=head(new[order(-new$bet),],n=10) 
    vector_of_Table3=as.vector(Table3$temp)
    
    #Identifying upto 2 Neighbors
    neighbor_list=make_ego_graph(graph_Table3, order = 2, nodes = vector_of_Table3, mode = "all") 
    union_of_graph=graph.union(neighbor_list[[1]],neighbor_list[[2]], neighbor_list[[3]], neighbor_list[[4]], neighbor_list[[5]], neighbor_list[[6]], neighbor_list[[7]], neighbor_list[[8]], neighbor_list[[9]], neighbor_list[[10]], byname = T)
    simplified_graph=simplify(union_of_graph, remove.multiple = T) #Simplifying to remove multiple edges between same nodes
    c=cluster_walktrap(simplified_graph)
    m=membership(c)
    graph_object=igraph_to_networkD3(simplified_graph, group = m, what = "both")
    
    #Plotting 2-hop neighbors with NetworkD3
    forceNetwork(Links = graph_object$links, Nodes = graph_object$nodes,
               Source = 'source', Target = 'target', NodeID = 'name',
               Group = 'group', zoom = T, linkWidth = -1.5, opacity = 0.6, fontSize = 20, fontFamily = "serif")  
  })    
  
#--------Task 9: Display/visualize up to 2-hop neighbors of 10 people with the highest indegree centrality--------------------------------
  
  output$twohop5=renderForceNetwork({  
    
    #File Input
    input8 = input$file1
    if (is.null(input8))
      return(NULL)
    
    dataframe8 = read.table(input8$datapath)
    
    colnames(dataframe8)=c("person_from","person_to")
    graph_Table3=graph_from_data_frame(dataframe8, directed = T)
    
    #Calculating Indegree Centrality of each node
    indeg=degree(graph_Table3, v=V(graph_Table3), mode="in")
    indeg_df=as.data.frame(indeg)
    temp=row.names(indeg_df)
    new=cbind(temp, indeg_df)
    
    #Sorting by descending order and identifying top 10 nodes with highest indegree centrality
    Table3=head(new[order(-new$indeg),],n=10) 
    vector_of_Table3=as.vector(Table3$temp)
    
    #Identifying upto 2 Neighbors
    neighbor_list=make_ego_graph(graph_Table3, order = 2, nodes = vector_of_Table3, mode = "all") 
    union_of_graph=graph.union(neighbor_list[[1]],neighbor_list[[2]], neighbor_list[[3]], neighbor_list[[4]], neighbor_list[[5]], neighbor_list[[6]], neighbor_list[[7]], neighbor_list[[8]], neighbor_list[[9]], neighbor_list[[10]], byname = T)
    simplified_graph=simplify(union_of_graph, remove.multiple = T) #Simplifying to remove multiple edges between same nodes
    c=cluster_walktrap(simplified_graph)
    m=membership(c)
    graph_object=igraph_to_networkD3(simplified_graph, group = m, what = "both")
    
    #Plotting 2-hop neighbors with NetworkD3
    forceNetwork(Links = graph_object$links, Nodes = graph_object$nodes,
                 Source = 'source', Target = 'target', NodeID = 'name',
                 Group = 'group', zoom = T, linkWidth = -1.5, opacity = 0.6, fontSize = 20, fontFamily = "serif")  
  })
  
  

#----------------Task 10: Table of Email activity amongst Departments----------------------------------------  
#----------------------------------------- PART a -------------------------------------------------------------------
  
  
  output$Table3=renderTable({
  
    #File Input
    input9 = input$file1
    if (is.null(input9))
      return(NULL)
    
    #File Input
    input10 = input$file2
    if (is.null(input10))
      return(NULL)
    
    dataframe9 = read.table(input9$datapath)
    dataframe10 = read.table(input10$datapath)
    
    #Using SQL to obtain desired Table of Department Email Activity
    Table3=sqldf("SELECT temp.dept1 AS Originating_Department, temp.dept2 AS Receiving_Department, count(*) AS Total_Emails_Sent FROM
           (SELECT dataframe9.V1, dataframe10.V2 AS dept1, dataframe9.V2,
           (SELECT dataframe10.V2 FROM dataframe10 WHERE dataframe10.V1=dataframe9.V2) AS dept2 FROM
           dataframe9 INNER JOIN dataframe10 ON dataframe9.V1=dataframe10.V1) temp
           GROUP BY  temp.dept1, temp.dept2")
  })
    
  
#----------------Task 10: Visualization of Email activity amongst Departments----------------------------------------  
#----------------------------------------- PART b -------------------------------------------------------------------
  
  output$Graph2=renderForceNetwork({  
  
    #File Input
    input9 = input$file1
    if (is.null(input9))
      return(NULL)
    
    #File Input
    input10 = input$file2
    if (is.null(input10))
      return(NULL)
    
    dataframe9 = read.table(input9$datapath)
    dataframe10 = read.table(input10$datapath)
    
    #Using SQL to obtain desired Table of Department Email Activity
    Table3=sqldf("SELECT temp.dept1, temp.dept2,count(*) FROM
           (SELECT dataframe9.V1 ,dataframe10.V2 AS dept1, dataframe9.V2,
           (SELECT dataframe10.V2 FROM dataframe10 WHERE dataframe10.V1=dataframe9.V2) AS dept2 FROM
           dataframe9 INNER JOIN dataframe10 ON dataframe9.V1=dataframe10.V1) temp
           GROUP BY  temp.dept1, temp.dept2")
    
    graph_dept=graph_from_data_frame(Table3)
    c=cluster_walktrap(graph_dept)
    m=membership(c)
    graph_object=igraph_to_networkD3(graph_dept, group = m, what = "both")
    forceNetwork(Links = graph_object$links, Nodes = graph_object$nodes,
                 Source = 'source', Target = 'target', NodeID = 'name',
                 Group = 'group', zoom = T, charge = -100,
                 linkWidth = -1.5, opacity = 0.6, fontSize = 20, fontFamily = "serif")
  
  })
  
  
  
        
}
# Run the app ----
  shinyApp(ui = ui, server = server)

#devtools::install_github("hrbrmstr/speedtest")
library(speedtest)

# Quick test
#speedtest::spd_test()

NoServersInEachGroup <- 3

library(speedtest)
library(stringi)
library(hrbrthemes)
library(ggbeeswarm)
library(tidyverse)
library(patchwork)
library(RSQLite)

theme_AI <- function()
{
  
  palette <- c("#FFFFFF", "#F0F0F0", "#A7A8AA", "#BDBDBD", "#969696", "#16C5A8",
               "#525252", "#252525", "#000000")
  
  color.background = palette[1]
  color.grid.major = palette[2]
  color.axis.text = palette[7]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  theme_bw(base_size=10) +
    
    theme(
      
      panel.background=element_rect(fill=color.background, color=color.background),
      plot.background=element_rect(fill=color.background, color=color.background),
      panel.border=element_rect(color=color.background),
      
      legend.position = "top",
      
      panel.grid.major=element_line(color=color.grid.major,size=.25),
      panel.grid.minor=element_blank(),
      
      axis.ticks=element_blank(),
      
      legend.background = element_rect(fill=color.background),
      legend.key = element_rect(fill=color.background, color=NA),
      legend.text = element_text(size=10,color=color.axis.title),
      
      plot.title=element_text(color=color.title, size=12, vjust=1.25),
      plot.subtitle=element_text(color=color.title, size=11, vjust=1.25),
      axis.text.x=element_text(size=10,color=color.axis.text),
      axis.text.y=element_text(size=10,color=color.axis.text),
      axis.title.x=element_text(size=11,color=color.axis.title, vjust=0),
      axis.title.y=element_text(size=11,color=color.axis.title, vjust=1.25),
      
      plot.margin  = unit(c(0.1, 0.1, 0.9, 0.1), "cm")
      
    )
  
}

for (i in seq(1:60))
{

  # Display in non scientific notation
  options(scipen=999)
  
  # Load the servers
  config <- spd_config()
  servers <- spd_servers(config=config)
  closest_servers <- spd_closest_servers(servers, config=config)
  only_the_best_severs <- spd_best_servers(closest_servers, config)
  
  # Run tests
  #DownloadSpeeds <- spd_download_test(closest_servers[1,], config=config)
  #
  #UploadSpeeds <- spd_upload_test(only_the_best_severs[1,], config=config)
  
  
  # More complex test
  set.seed(8675309)
  
  bind_rows(
    closest_servers[1:NoServersInEachGroup,] %>%
      mutate(type="closest"),
    only_the_best_severs[1:NoServersInEachGroup,] %>%
      mutate(type="best") 
  ) %>%
    group_by(type) %>%
    ungroup() -> to_compare
  
  ####################################################################################
  #
  # Download speeds first
  #
  ####################################################################################
  map_df(1:nrow(to_compare), ~{
    spd_download_test(to_compare[.x,], config=config, summarise=FALSE, timeout=30)
  }) -> dl_results_full
  
  dl_results_full$size <- dl_results_full$size / 1000000
  
  #### Stats
  Download_NoTests <- nrow(dl_results_full)
  Download_Median_BW <- round(median(dl_results_full$bw,na.rm = TRUE),1)
  Download_Lowest5Pct_BW <- round(quantile(dl_results_full$bw,0.05,na.rm = TRUE),1)
  Download_Highest95Pct_BW <- round(quantile(dl_results_full$bw,0.95,na.rm = TRUE),1)
  Download_Subtitle <- paste("No tests:",Download_NoTests,
                   " Highest:",Download_Highest95Pct_BW,
                   " Median:",Download_Median_BW,
                   " Lowest:",Download_Lowest5Pct_BW,sep="")
  
  DownloadPlot <- mutate(dl_results_full, type=stri_trans_totitle(type)) %>%
    ggplot(aes(size, bw, fill=name)) +
    geom_point(aes(size=size, color=name), shape=21) +
    scale_y_continuous(limits=c(0,40)) +
    scale_size(range=c(1,5)) +
    facet_wrap(type ~ .) +
    geom_smooth(method = "lm",se=TRUE,alpha=0.2) +
  #  scale_color_manual(values=c(Random="#b2b2b2", Best="#2b2b2b", Closest="#2b2b2b")) +
    scale_fill_ipsum() +
    labs(x="Size in MB", y="MB/s", title="Download bandwidth",
         subtitle=Download_Subtitle) +
    theme_AI() +
    theme(legend.position="none")
  print(DownloadPlot)
  
  ####################################################################################
  #
  # Upload speeds 
  #
  ####################################################################################
  
  map_df(1:nrow(to_compare), ~{
    spd_upload_test(to_compare[.x,], config=config, summarise=FALSE, timeout=30)
  }) -> ul_results_full
  
  ul_results_full$size <- ul_results_full$size / 1000000
  
  #### Stats
  Upload_NoTests <- nrow(ul_results_full)
  Upload_Median_BW <- round(median(ul_results_full$bw,na.rm = TRUE),1)
  Upload_Lowest5Pct_BW <- round(quantile(ul_results_full$bw,0.05,na.rm = TRUE),1)
  Upload_Highest95Pct_BW <- round(quantile(ul_results_full$bw,0.95,na.rm = TRUE),1)
  Upload_Subtitle <- paste("No tests:",Upload_NoTests,
                             " Highest:",Upload_Highest95Pct_BW,
                             " Median:",Upload_Median_BW,
                             " Lowest:",Upload_Lowest5Pct_BW,sep="")
  UploadPlot <- mutate(ul_results_full, type=stri_trans_totitle(type)) %>%
    ggplot(aes(size, bw, fill=name)) +
    geom_point(aes(size=size, color=name), shape=21) +
    scale_y_continuous(limits=c(0,5)) +
    scale_size(range=c(1,5)) +
    facet_wrap(type ~ .) +
    geom_smooth(method = "lm",se=TRUE,alpha=0.2) +
    scale_fill_ipsum() +
    labs(x="Size in MB", y="MB/s", title="Upload bandwidth",
         subtitle=Upload_Subtitle) +
    theme_AI() +
    theme(legend.position="none")
  print(UploadPlot)
  
  ###########################
  
  # Combine the plots
  
  patchwork_grid <- DownloadPlot + UploadPlot
  patchwork_grid <- patchwork_grid + plot_annotation(
    title = "Bandwidth monitor",
    subtitle = paste("Run at: ",Sys.Date()))
  print(patchwork_grid)
  
  TodaysDate <- as.character(Sys.Date())
  TodaysTime <- as.character(format(Sys.time(), "%X"))
  
  # Now create data frame to hold a history of results
  NewData <- data.frame(Date=TodaysDate,
                        Time=TodaysTime,
                        Download_Tests = Download_NoTests,
                        Download_Highest = Download_Highest95Pct_BW,
                        Download_Median = Download_Median_BW,
                        Download_Lowest = Download_Lowest5Pct_BW,
                        Upload_Tests = Upload_NoTests,
                        Upload_Highest = Upload_Highest95Pct_BW,
                        Upload_Median = Upload_Median_BW,
                        Upload_Lowest = Upload_Lowest5Pct_BW
                        )
  
  ######
  print("Creating new DB with all tables being overwritten")
  
  # Create a connection to our new database, CarsDB.db
  # you can check that the .db file has been created on your working directory
  conn <- dbConnect(RSQLite::SQLite(), "Bandwidth.db")
  
  dbWriteTable(conn,"Bandwidth", NewData, append = TRUE,overwrite = FALSE)
  
  # List all the Tables
  print("The following tables have been loaded")
  print(dbListTables(conn))
  
  dbDisconnect(conn)
  
  # Now print the history of results
  
  print("Getting all data history")
  conn <- dbConnect(RSQLite::SQLite(), "Bandwidth.db")
  
  Report <- dbGetQuery(conn, "SELECT * FROM Bandwidth")
  
  # Disconnect the DB
  dbDisconnect(conn)
  
  print(Report)
  
  print("Finished loading data from SQL")
  
  # Wait ten mins
  print("Waiting for ten mins")
  Sys.sleep(60*10)

}

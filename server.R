library(shiny)
library(data.table)
library(DT)
library(tidyverse)
library(ggplot2)
library(bit64)

shinyServer(
  function(input, output) {
    playerTable <- as.data.frame(fread('https://raw.githubusercontent.com/shelchang/tf2cc-data/main/persons.csv')) %>% select(steamID, steamName) %>% arrange(steamName)
    matchesTable <- as.data.frame(fread('https://raw.githubusercontent.com/shelchang/tf2cc-data/main/matches.csv'))
    matchesTable$date <- as.Date(matchesTable$date)
    resultsTable <- as.data.frame(fread('https://raw.githubusercontent.com/shelchang/tf2cc-data/main/results.csv'))
    
    lastUpdate <- max(matchesTable$date)
    lastUpdateText <- paste("Logs last updated: ", format(lastUpdate, "%A, %d %B %Y"))
    
    output$lastUpdate <- renderText(lastUpdateText)
    
    observeEvent(input$sessionsPerMonth, {
      sessionsByMonthTable <- matchesTable
      sessionsByMonthTable$month <- as.Date(cut(sessionsByMonthTable$date, breaks = "month"))
      sessionsByMonthTable <- sessionsByMonthTable %>% group_by(month,region) %>% summarize(sessions = n_distinct(date))
      sessionsByMonthTable$month <- format(as.Date(sessionsByMonthTable$month), '%Y %m')
      
      mainPlot <- ggplot(data = sessionsByMonthTable, aes(x = month, y = sessions, fill = region)) + geom_bar(stat = "identity", position = 'stack') + 
        ggtitle("Number of PUG sessions per month") + theme_minimal()
      
      output$mainPlot <- renderPlot(mainPlot)
      output$mainTable <- renderDataTable(sessionsByMonthTable)
    })
    
    observeEvent(input$matchTypes, {
      matchtypeByMonthTable <- matchesTable
      matchtypeByMonthTable$month <- as.Date(cut(matchtypeByMonthTable$date, breaks = "month"))
      matchtypeByMonthTable <- matchtypeByMonthTable %>% mutate(map2 = str_replace(map, 'koth_ultiduo', 'ultiduo_ultiduo')) %>% mutate(matchType = str_replace(map2, '[_].+', '')) %>% 
        group_by(month,region) %>% select(month,matchType,region)
      matchtypeByMonthTable$month <- format(as.Date(matchtypeByMonthTable$month), '%Y %m')
      
      matchtypeByMonthTable2 <- matchesTable %>% select(logsID,region,date,map) %>% arrange(desc(logsID))
      
      mainPlot <- ggplot(data = matchtypeByMonthTable, aes(x = month, fill = matchType)) + geom_bar(stat = 'count', position = 'stack') +
        ggtitle("Matches by type per month") + theme_minimal()
      
      output$mainPlot <- renderPlot(mainPlot)
      output$mainTable <- renderDataTable(matchtypeByMonthTable2, options = list(columnDefs = list(list(className = 'dt-center',
        targets = 1,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' ?",
          "'<a href=\"https://logs.tf/' + data + '\" target = \"_blank\">' + data + '</a>' : data",
          "}")
      ))))
    })
    
    observeEvent(input$sessionPlayers,{
      playersPerSessionTable <- resultsTable %>% select(steamID, date) %>% group_by(date) %>% summarize(numPlayers = n_distinct(steamID))
      
      mainPlot <- ggplot(data = playersPerSessionTable, aes(date,numPlayers)) + geom_bar(stat = 'identity') +
        ggtitle("Players per session over time") + theme_minimal()
      
      output$mainPlot <- renderPlot(mainPlot)
      output$mainTable <- renderDataTable(playersPerSessionTable)
    })
    
    observeEvent(input$playerSessions, {
      sessionsPerPlayerTable <- resultsTable %>% select(steamID, date) %>% group_by(steamID) %>% summarize(numSessions = n_distinct(date)) %>%
        inner_join(playerTable, by='steamID') %>% select(steamName, numSessions) %>% arrange(desc(numSessions))
      
      mainPlot <- ggplot(data = sessionsPerPlayerTable, aes(numSessions)) + geom_histogram(binwidth = 1) + 
        ggtitle("Number of sessions played histogram") + theme_minimal()
      
      output$mainPlot <- renderPlot(mainPlot)
      output$mainTable <- renderDataTable(sessionsPerPlayerTable)
    })
    
    observeEvent(input$mapTime, {
      mapTimeTable <- matchesTable %>% select(map, length) %>% transmute(map = str_replace(map, 'pro_viaduct', 'product'), length = length) %>%
        transmute(map = str_replace(map, '([a-z]+_[a-z]+)_.+', '\\1'), lengthMin = round(length/60, digits=1)) %>%
        group_by(map) %>% summarize(totalTime = sum(lengthMin)) %>% arrange(desc(totalTime))
      
      mainPlot <- ggplot(data = mapTimeTable, aes(map,totalTime)) + geom_bar(stat = 'identity') + coord_flip() + ggtitle("Total time spent per map (min)") + theme_minimal()
      
      output$mainPlot <- renderPlot(mainPlot)
      output$mainTable <- renderDataTable(mapTimeTable)
    })
    
    observeEvent(input$uniquePlayers, {
      uniquePlayersTable <- resultsTable %>% select(steamID, date)
      uniquePlayersTable$month <- as.Date(cut(uniquePlayersTable$date, breaks = "month"))
      uniquePlayersTable <- uniquePlayersTable %>% group_by(month) %>% summarize(uniquePlayers = n_distinct(steamID))
      uniquePlayersTable$month <- format(as.Date(uniquePlayersTable$month), '%Y %m')
      
      mainPlot <- ggplot(data = uniquePlayersTable, aes(month,uniquePlayers)) + geom_bar(stat = "identity") + 
        ggtitle("Number of unique players per month") + theme_minimal()
      
      output$mainPlot <- renderPlot(mainPlot)
      output$mainTable <- renderDataTable(uniquePlayersTable)
    })
    
    observeEvent(input$applyRankings, {
      req(input$dateFilter)
      validate(need(!is.na(input$dateFilter[1]) & !is.na(input$dateFilter[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$dateFilter[1] < input$dateFilter[2], "Error: Start date should be earlier than end date."))
      matchFilter <- matchesTable %>% mutate(map2 = str_replace(map, 'koth_ultiduo', 'ultiduo_ultiduo')) %>%
        mutate(mapType = str_replace(map2, '[_].+', '')) %>%
        filter(date >= input$dateFilter[1] & date <= input$dateFilter[2]) %>%
        filter(numplayers >= input$matchFormat[1] & numplayers <= input$matchFormat[2]) %>%
        filter(mapType %in% input$mapFilter) %>%
        select(logsID)
      playerRankings <- resultsTable %>% filter(logsID %in% matchFilter$logsID)
      
      if(input$stats == "KA/D") {
        output$medicExp <- NULL
        playerRankings <- playerRankings %>% filter(class %in% input$classFilter) %>% select(steamID,total_time,kills,assists,deaths) %>% group_by(steamID) %>%
          summarize(time_played = sum(total_time), overall_kad = round((sum(kills/total_time) + sum(assists/total_time))/sum(deaths/total_time), digits = 2)) %>%
          inner_join(playerTable, by='steamID') %>%
          select(steamName,time_played,overall_kad) %>% arrange(desc(overall_kad))
      }
      
      if(input$stats == "DPM") {
        output$medicExp <- NULL
        playerRankings <- playerRankings %>% filter(class %in% input$classFilter) %>% select(steamID,total_time,damage) %>% group_by(steamID) %>%
          summarize(time_played = sum(total_time), overall_dpm = round(sum(damage)/sum(total_time)*60)) %>%
          inner_join(playerTable, by='steamID') %>%
          select(steamName,time_played,overall_dpm) %>% arrange(desc(overall_dpm))
      }
      
      if(input$stats == "Win/loss record") {
        output$medicExp <- NULL
        matchResults <- transmute(matchesTable, logsID = logsID, winner = case_when(blu_score - red_score > 0 ~ "Blue", blu_score - red_score < 0 ~ "Red", blu_score - red_score == 0 ~ "TIE"))
        playerRankings <- playerRankings %>% filter(class %in% input$classFilter) %>% select(logsID,steamID,team) %>% unique() %>%
          inner_join(matchResults, by = 'logsID') %>%
          transmute(logsID = logsID, steamID = steamID, win = case_when(team == winner ~ 1, team != winner ~ 0), loss = case_when(winner == "TIE" ~ 0, team != winner ~ 1, team == winner ~ 0), tie = case_when(winner == "TIE" ~ 1)) %>%
          inner_join(playerTable, by = 'steamID') %>% select(steamName,win,loss,tie) %>% group_by(steamName) %>% summarize(wins = sum(win), losses = sum(loss), ties = sum(tie,na.rm=TRUE)) %>% arrange(desc(wins))
      }
      
      if(input$stats == "Healing stats") {
        output$medicExp <- renderText("Medic only, overrides class filters chosen below")
        playerRankings <- playerRankings %>% filter(class == "medic") %>% select(steamID,total_time,med_healing,med_charges,med_drops) %>% group_by(steamID) %>%
          summarize(time_played = sum(total_time), total_healing = sum(med_healing), hpm = round(sum(med_healing)/sum(total_time)*60)) %>%
          inner_join(playerTable, by='steamID') %>%
          select(steamName,time_played,total_healing,hpm) %>% arrange(desc(total_healing))
      }
      
      output$rankingTable <- renderDataTable(playerRankings, options = list(pageLength = 50))
    })
    
    observeEvent(input$applyCompare, {
      req(input$dateFilter2)
      validate(need(!is.na(input$dateFilter2[1]) & !is.na(input$dateFilter2[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$dateFilter2[1] < input$dateFilter2[2], "Error: Start date should be earlier than end date."))
      matchFilter <- matchesTable %>% mutate(map2 = str_replace(map, 'koth_ultiduo', 'ultiduo_ultiduo')) %>%
        mutate(mapType = str_replace(map2, '[_].+', '')) %>%
        filter(date >= input$dateFilter2[1] & date <= input$dateFilter2[2]) %>%
        filter(numplayers >= input$matchFormat2[1] & numplayers <= input$matchFormat2[2]) %>%
        filter(mapType %in% input$mapFilter2) %>%
        select(logsID)
      playerRankings <- resultsTable %>% filter(logsID %in% matchFilter$logsID)
      
      playerSelection <- data.frame(steamName = input$players) %>% inner_join(playerTable, by='steamName')
      
      if(input$compare == "KA/D") {
        playerRankings <- playerRankings %>% filter(class %in% input$classFilter2) %>% filter(steamID %in% playerSelection$steamID) %>%
          select(steamID,kills,assists,deaths) %>% mutate(kad = (kills + assists) / deaths) %>% group_by(steamID) %>%
          inner_join(playerTable, by='steamID') 
        comparisonPlot <- ggplot(playerRankings, aes(x = steamName, y = kad)) + geom_boxplot(notch = FALSE) + coord_flip() + theme_minimal()
      }
      
      if(input$compare == "DPM") {
        playerRankings <- playerRankings %>% filter(class %in% input$classFilter2) %>% filter(steamID %in% playerSelection$steamID) %>%
          select(steamID,total_time,damage) %>% mutate(dpm = damage / total_time * 60) %>% group_by(steamID) %>%
          inner_join(playerTable, by='steamID') 
        comparisonPlot <- ggplot(playerRankings, aes(x = steamName, y = dpm)) + geom_boxplot(notch = FALSE) + coord_flip() + theme_minimal()
      }
      
      output$comparisonPlot <- renderPlot(comparisonPlot)
    })
    
  }
)
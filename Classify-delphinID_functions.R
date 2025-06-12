library(rvest)
library(janitor)
library(dbplyr)
library(ggplot2)
library(RSQLite)
library(DBI)
library(lubridate)
library(stringr)
suppressWarnings(library(data.table))
suppressWarnings(library(randomForest))
suppressWarnings(library(ggplot2))
suppressWarnings(library(scales))
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
library(jsonlite)

makeDirSysDT <- function(create=TRUE) {
  # Get the current date and time
  current_date <- format(Sys.Date(), "%d%m%y")
  current_time <- format(Sys.time(), "%H%M")
  
  # Construct the directory name
  dir_name <- paste0("classificationResults_", current_date, "-", current_time)
  
  # Create the directory
  if (create==TRUE) {
    dir.create(dir_name)
  }
  
  # Return the name of the created directory (optional, but good practice)
  return(dir_name)
}

processdataRocca <- function(db_con, dateRange, verbose=TRUE) {
  data <- dbGetQuery(db_con, paste0("SELECT * FROM Rocca_Whistle_Stats"))
  SAQ <- dbGetQuery(db_con, paste0("SELECT * FROM Sound_Acquisition"))
  data$UTC <- as.POSIXct(data$UTC, '%Y-%m-%d %H:%M:%OS', tz='UTC')
  SAQ$UTC <- as.POSIXct(SAQ$UTC, '%Y-%m-%d %H:%M:%OS', tz='UTC')
  specieslist <- c('Dde', 'Ggr', 'Gme', 'Lal', 'Oor', 'Ttr')
  
  # Filter data based on date range
  if (!is.null(dateRange)) {
    data <- data[data$UTC >= dateRange[1] & data$UTC <= dateRange[2] + 1, ]
    SAQ <- SAQ[SAQ$UTC >= dateRange[1] & SAQ$UTC <= dateRange[2] + 1, ]
  }
  
  event_count <- 1
  test_events <- data.frame()
  pb <- txtProgressBar(min = 0, max = length(unique(SAQ$SystemName)), style = 3)
  cat(sprintf('Loading %s acoustic events from database.\n', length(unique(SAQ$SystemName))))
  for (evID in unique(SAQ$SystemName)) {
    setTxtProgressBar(pb, value = event_count) 
    subSAQ <- subset(SAQ, SystemName == evID)
    if (dim(subSAQ)[1] > 0) {
      t0 <- NaN
      t1 <- NaN
      for (i in 1:dim(subSAQ)[1]) {
        if (grepl('Start', subSAQ$Status[i])) {
          t0 <- as.POSIXct(subSAQ$UTC[i], '%Y-%m-%d %H:%M:%OS', tz='UTC')
        } else if (grepl('Stop', subSAQ$Status[i])) {
          t1 <- as.POSIXct(subSAQ$UTC[i], '%Y-%m-%d %H:%M:%OS', tz='UTC')
        }
      }
      
      if (!is.na(t0) & !is.na(t1)) {
        sub <- subset(data, UTC > t0 & UTC <= t1)
        if (length(dim(sub)) > 0) {
          if (dim(sub)[1] > 0) {
            count <- 1
            namesc <- c()
            namesw <- c()
            for (sp in specieslist) {
              namesc <- append(namesc, sprintf('%s_c', sp))
              namesw <- append(namesw, sprintf('%s_w', sp))
              votes <- c()
              for (i in 1:dim(sub)[1]){
                vl <- sub$voteList[i]
                endind <- gregexpr(')', vl)[[1]][1]
                vl <- substr(vl, 2, endind-2)
                dash_inds <- as.numeric(gregexpr('-', vl)[[1]])
                if (count == 1) {
                  ind0 <- 1
                  ind1 <- dash_inds[count]-1
                  p <- as.numeric(substr(vl, ind0, ind1))
                } else if (count == 6) {
                  ind0 <- dash_inds[count-1] + 1
                  ind1 <- nchar(vl)
                  p <- as.numeric(substr(vl, ind0, ind1))
                } else {
                  ind0 <- dash_inds[count-1] + 1
                  ind1 <- dash_inds[count] - 1
                  p <- as.numeric(substr(vl, ind0, ind1))
                }
                votes <- append(votes, p)
              }
              sub[[sp]] <- votes
              count <- count + 1
            }
            
            dfc <- subset(sub, freqPeak >= 10000 & freqPeak <= 40000)
            dfw <- subset(sub, freqMin >= 2000 & freqMax <= 20000 & duration >= 0.2)
            
            dfc_e <- colMeans(dfc[,specieslist])/sum(colMeans(dfc[,specieslist]))
            if (any(is.na(dfc_e))) {
              for (name in names(dfc_e)) {
                dfc_e[[name]] <- runif(1,10,15)
              }
              dfc_e <- dfc_e/sum(dfc_e)
            }
            
            
            dfw_e <- colMeans(dfw[,specieslist])/sum(colMeans(dfw[,specieslist]))
            if (any(is.na(dfw_e))) {
              for (name in names(dfw_e)) {
                dfw_e[[name]] <- runif(1,10,15)
              }
              dfw_e <- dfw_e/sum(dfw_e)
            }
            
            names(dfc_e) <- namesc
            names(dfw_e) <- namesw
            xtest <- data.frame(cbind(t(dfc_e), t(dfw_e)))
            xround <- as.integer(xtest*100)
            
            barcode <- ''
            for (item in xround) {
              item <- as.character(item)
              if (nchar(item) < 2) {
                item <- gsub(' ', '', paste('0', item))
              }
              barcode <- gsub(' ', '', paste(barcode, item))
            }
            
            evDur <- as.numeric(seconds(difftime(sub$UTC[dim(sub)[1]], sub$UTC[1])))*60/60
            xtest$startUTC <- sub$UTC[1]
            xtest$endUTC <- sub$UTC[dim(sub)[1]]
            xtest$minutes <- evDur/60
            xtest$eventID <- str_trim(as.character(evID))
            xtest$clicks <- dim(dfc)[1]
            xtest$whistles <- dim(dfw)[1]
            xtest$barcode <- barcode
            if (verbose == TRUE) {
              print(sprintf('Event %s start %s', event_count, xtest$startUTC))
              print(sprintf('Event %s end %s', event_count, xtest$endUTC))
            }
       
            test_events <- rbind(test_events, xtest)
            sub <- data.frame()
            event_count <- event_count + 1
          }
        }
      }
    }
  }
  list(df=test_events)
}

processdataDelphinID <- function(db_con, dateRange, ctable=NULL, wtable=NULL, randseed=42, verbose=TRUE) {
  ctable <- gsub(' ', '', paste(ctable, '_Predictions'))
  wtable <- gsub(' ', '', paste(wtable, '_Predictions'))

  cdata <- dbGetQuery(db_con, paste0(sprintf("SELECT * FROM %s", ctable)))
  wdata <- dbGetQuery(db_con, paste0(sprintf("SELECT * FROM %s", wtable)))

  SAQ <- dbGetQuery(db_con, paste0("SELECT * FROM Sound_Acquisition"))
  if (nrow(wdata) > 0) {
    wdata$UTC <- as.POSIXct(wdata$UTC, '%Y-%m-%d %H:%M:%OS', tz='UTC')
  }
  if (nrow(cdata) > 0) {
    cdata$UTC <- as.POSIXct(cdata$UTC, '%Y-%m-%d %H:%M:%OS', tz='UTC')
  }
  
  SAQ$UTC <- as.POSIXct(SAQ$UTC, '%Y-%m-%d %H:%M:%OS', tz='UTC')
  cspecies <- c('Dde', 'Ggr', 'Gme', 'Lal', 'Ttr')
  wspecies <- c('Dde', 'Ggr', 'Gme', 'Lac', 'Lal', 'Oor', 'Ttr')
  
  # Filter data based on date range
  if (!is.null(dateRange)) {
    cdata <- cdata[cdata$UTC >= dateRange[1] & cdata$UTC <= dateRange[2] + 1, ]
    wdata <- wdata[wdata$UTC >= dateRange[1] & wdata$UTC <= dateRange[2] + 1, ]
    SAQ <- SAQ[SAQ$UTC >= dateRange[1] & SAQ$UTC <= dateRange[2] + 1, ]
  }
  
  event_count <- 1
  test_events <- data.frame()
  df_whistles <- data.frame()
  df_clicks <- data.frame()
  cat(sprintf('Loading %s acoustic events from database.\n', length(unique(SAQ$SystemName))))
  pb <- txtProgressBar(min = 0, max = length(unique(SAQ$SystemName)), style = 3)
  for (evID in unique(SAQ$SystemName)) {
    setTxtProgressBar(pb, value = event_count) 
    subSAQ <- subset(SAQ, SystemName == evID)
    if (dim(subSAQ)[1] > 0) {
      t0 <- NaN
      t1 <- NaN
      for (i in 1:dim(subSAQ)[1]) {
        if (grepl('Start', subSAQ$Status[i])) {
          t0 <- as.POSIXct(subSAQ$UTC[i], '%Y-%m-%d %H:%M:%OS', tz='UTC')
        } else if (grepl('Stop', subSAQ$Status[i])) {
          t1 <- as.POSIXct(subSAQ$UTC[i], '%Y-%m-%d %H:%M:%OS', tz='UTC')
        }
      }
      
      if (!is.na(t0) & !is.na(t1)) {
        csub <- subset(cdata, UTC > t0 & UTC <= t1)
        wsub <- subset(wdata, UTC > t0 & UTC <= t1)
        
        #discard bad rows/missing values in click classifier output
        keep <- c()
        for (j in 1:nrow(csub)) {
          preds <- csub$Predicition[j]
          if (length(preds) > 0) {
            if (is.character(preds) == TRUE & length(preds) > 0 & !is.na(preds)) {
              nums <- as.numeric(fromJSON(preds)$predictions)
              if (all(nums >= 0) == TRUE) {
                keep <- append(keep, 1)
              } else {
                keep <- append(keep, 0)
              }
            } else {
              keep <- append(keep, 0)
            }
          } else {
            keep <- append(keep, 0)
          }
        }
        
        if (nrow(csub) == length(keep)) {
          csub$keep <- keep
          csub <- subset(csub, keep > 0)
        } else {
          csub <- csub[0:0,]
        }
        
        
        #discard bad rows/missing values in whistle classifier output
        keep <- c()
        for (j in 1:nrow(wsub)) {
          preds <- wsub$Predicition[j]
          if (length(preds) > 0) {
            if (is.character(preds) == TRUE & !is.na(preds)) {
              nums <- as.numeric(fromJSON(preds)$predictions)
              if (all(nums >= 0) == TRUE) {
                keep <- append(keep, 1)
              } else {
                keep <- append(keep, 0)
              }
            } else {
              keep <- append(keep, 0)
            }
          } else {
            keep <- append(keep, 0)
          }
        }
        
        if (nrow(wsub) == length(keep)) {
          wsub$keep <- keep
          wsub <- subset(wsub, keep > 0)
        } else {
          wsub <- wsub[0:0,]
        }
        
        #get all predictions from click classifier
        if (length(dim(csub)) > 0) {
          if (dim(csub)[1] > 0) {
            count <- 1
            namesc <- c()
            for (sp in cspecies) {
              namesc <- append(namesc, sprintf('%s_c', sp))
              votes <- c()
              for (i in 1:dim(csub)[1]){
                preds <- csub$Predicition[i]
                p <- as.numeric(fromJSON(preds)$predictions[[count]])
                votes <- append(votes, p)
              }
              csub[[sp]] <- votes
              count <- count + 1
            }
          } else {
            csub <- data.frame(t(data.frame(rep(NaN, length(cspecies)))))
            rownames(csub) <- c(1)
            names(csub) <- cspecies
          }
        } else {
          csub <- data.frame(t(data.frame(rep(NaN, length(cspecies)))))
          rownames(csub) <- c(1)
          names(csub) <- cspecies
        }
        
        if (length(dim(wsub)) > 0) {
          if (dim(wsub)[1] > 0) {
            count <- 1
            namesw <- c()
            for (sp in wspecies) {
              namesw <- append(namesw, sprintf('%s_w', sp))
              votes <- c()
              for (i in 1:dim(wsub)[1]){
                preds <- wsub$Predicition[i]
                nums <- as.numeric(fromJSON(preds)$predictions)
                p <- as.numeric(fromJSON(preds)$predictions[[count]])
                votes <- append(votes, p)
              }
              wsub[[sp]] <- votes
              count <- count + 1
            }
          } else {
            wsub <- data.frame(t(data.frame(rep(NaN, length(wspecies)))))
            rownames(wsub) <- c(1)
            names(wsub) <- wspecies
          }
        } else {
          wsub <- data.frame(t(data.frame(rep(NaN, length(wspecies)))))
          rownames(wsub) <- c(1)
          names(wsub) <- wspecies
        }
        
        dfc <- csub
        dfw <- wsub
        
        if (nrow(dfw) > 0) {
          if (nrow(df_whistles) == 0) {
            df_whistles <- dfw
            rownames(df_whistles) <- 1:nrow(df_whistles)
          } else if (all(colnames(dfw) == colnames(df_whistles))) {
            df_whistles <- rbind(df_whistles, dfw)
            rownames(df_whistles) <- 1:nrow(df_whistles)
          }
        }

        if (nrow(dfc) > 0) {
          if (nrow(df_clicks) == 0) {
            df_clicks <- dfc
            rownames(df_clicks) <- 1:nrow(df_clicks)
          } else if (all(colnames(dfc) == colnames(df_clicks))) {
            df_clicks <- rbind(df_clicks, dfc)
            rownames(df_clicks) <- 1:nrow(df_clicks)
          }
        }
        
        
        dfc_e <- colMeans(dfc[,cspecies])/sum(colMeans(dfc[,cspecies]), na.rm=TRUE)
        seed <- as.integer(nrow(dfc)*nrow(dfw))
        cc <- 0
        if (any(is.na(dfc_e))) {
          for (name in names(dfc_e)) {
            dfc_e[[name]] <- runif(1,10,15)
            set.seed(seed + cc)
            cc <- cc + 1
          }
          dfc_e <- dfc_e/sum(dfc_e)
          nc <- 0
        } else {
          nc <- nrow(dfc)
        }
        
        set.seed(randseed)
        
        namesc <- c('Dde_c', 'Ggr_c', 'Gme_c', 'Lal_c', 'Ttr_c')
        
        dfw_e <- colMeans(dfw[,wspecies])/sum(colMeans(dfw[,wspecies]), na.rm=TRUE)
        seed <- as.integer(nrow(dfc)*nrow(dfw))
        cc <- 0
        if (any(is.na(dfw_e))) {
          for (name in names(dfw_e)) {
            dfw_e[[name]] <- runif(1,10,15)
            set.seed(seed + cc)
            cc <- cc + 1
          }
          dfw_e <- dfw_e/sum(dfw_e)
          nw <- 0
        } else {
          nw <- nrow(dfw)
        }
        
        namesw <- c('Dde_w', 'Ggr_w', 'Gme_w', 'Lac_w', 'Lal_w', 'Oor_w', 'Ttr_w')
        
        set.seed(randseed)
        
        names(dfc_e) <- namesc
        names(dfw_e) <- namesw
        xtest <- data.frame(cbind(t(dfc_e), t(dfw_e)))
        xround <- as.integer(xtest*100)
        barcode <- ''
        for (item in xround) {
          item <- as.character(item)
          if (nchar(item) < 2) {
            item <- gsub(' ', '', paste('0', item))
          }
          barcode <- gsub(' ', '', paste(barcode, item))
        }
        
        startT <- min(c(csub$UTC[1], wsub$UTC[1]), na.rm=TRUE)
        endT <- max(c(csub$UTC[dim(csub)[1]], wsub$UTC[dim(wsub)[1]]), na.rm=TRUE)
        evDur <- as.numeric(seconds(difftime(endT, startT)))*60/60
        
        xtest$startUTC <- startT
        xtest$endUTC <- endT
        xtest$minutes <- evDur/60
        xtest$eventID <- str_trim(as.character(evID))
        xtest$clicks <- nc
        xtest$whistles <- nw
        xtest$barcode <- barcode
        if (verbose == TRUE) {
          print(sprintf('Event %s start %s', event_count, xtest$startUTC))
          print(sprintf('Event %s end %s', event_count, xtest$endUTC))
        }
 
        test_events <- rbind(test_events, xtest)
        sub <- data.frame()
        event_count <- event_count + 1
      }
    }
  }
  list(df=test_events, df_whistles=df_whistles, df_clicks=df_clicks)
}

getEvents <- function(selectDB, classifierType='delphinID', dateRange=NULL, verbose = FALSE,
                      wtable='Deep_Learning_Classifier___Whistles', ctable='Deep_Learning_Classifier___Clicks', rseed=42) {
  set.seed(rseed)
  if (is.null(dateRange)) {
    dateRange <- c(Sys.Date() - 20000, Sys.Date())
  }
  
  db_con <- dbConnect(RSQLite::SQLite(), sprintf('%s', selectDB))
  on.exit(dbDisconnect(db_con))
  model <- readRDS(sprintf('EventClassifier_%s.rds', classifierType))
  if (classifierType == 'ROCCA') {
    infoEvents <- processdataRocca(db_con, dateRange,
                                   verbose = verbose)
    
  } else if (classifierType == 'delphinID') {
    infoEvents <- processdataDelphinID(db_con, dateRange, 
                                       ctable = cTable, 
                                       wtable = wTable, 
                                       randseed = rseed,
                                       verbose = verbose)
  }
  
  test_events <- infoEvents$df
  df_whistles <- infoEvents$df_whistles
  df_clicks <- infoEvents$df_clicks
  return(list(test_events=test_events, df_whistles=df_whistles, df_clicks=df_clicks))
}

getClassifications <- function(test_events, model, classifierType='delphinID', evScore=0, minClicks=0, minWhistles=0, AndOr='or', 
                               export = FALSE, savefolder = NULL, append_to_file=NULL) {
  rownames(test_events) <- 1:nrow(test_events)
  test_events$uid <- 1:nrow(test_events)
  bc <- test_events$barcode
  probs <- data.frame(predict(model, test_events, type = 'prob'))
  
  if (classifierType %in% c('delphinID', 'ROCCA')) {
    predspecieslist <- c('De. delphis', 'Gr. griseus', 'Gl. melas', 'La. acutus',
                         'La. albirostris', 'Or. orca', 'Tu. truncatus')
  } else { 
    predspecieslist <- model$classes
  }
  
  
  ind1 <- which(names(test_events) == 'Dde_c')
  ind2 <- which(names(test_events) == 'Ttr_w')
  pcadf <- test_events[, ind1:ind2]
  PCAvars <- names(pcadf)
  
  pred_df <- data.frame()
  for (i in 1:dim(probs)[1]) {
    row <- as.numeric(probs[i, ])
    pred <- predspecieslist[which.max(row)]
    conf <- max(row)
    prom <- row[rev(order(row))][1] - row[rev(order(row))][2]
    score <- prom * conf
    predrow <- data.frame(uid=test_events$uid[i], 
                          eventID = test_events$eventID[i], 
                          clicks = as.integer(test_events$clicks[i]), 
                          whistles = as.integer(test_events$whistles[i]),
                          minutes = test_events$minutes[i], 
                          predictedSpecies = pred, 
                          score = score, 
                          prom = prom, 
                          conf = conf, 
                          barcode = bc[i])
    
    spcount <- 1
    for (sp in predspecieslist) {
      predrow[[sp]] <- row[spcount]
      spcount <- spcount + 1
    }
    
    pred_df <- rbind(pred_df, predrow)
  }
  
  pred_df$eventGroup <- NA
  
  if (nrow(pcadf) > 1) {
    pca_result <- prcomp(pcadf, scale. = TRUE)
  } else {
    pca_result <- prcomp(pcadf, scale. = FALSE)
  }
  
  pca_result <- data.frame(pca_result$x)
  pcadf$predictedSpecies <- pred_df$predictedSpecies
  pcadf$PC1 <- pca_result$PC1
  pcadf$PC2 <- pca_result$PC2
  pcadf$clicks <- pred_df$clicks
  pcadf$whistles <- pred_df$whistles
  pcadf$score <- pred_df$score
  
  
  if (AndOr %in% c('and', 'AND')) {
    pcadf <- subset(pcadf, 
                    score>=evScore & clicks>=minClicks & whistles>=minWhistles)
  } else {
    pcadf <- subset(pcadf, 
                    score>=evScore & (clicks>=minClicks | whistles>=minWhistles))
  }
  
  allpreds <- pred_df
  if (AndOr %in% c('and', 'AND')) {
    pred_df <- subset(pred_df, 
                      score>=evScore & clicks>=minClicks & whistles>=minWhistles)
  } else {
    pred_df <- subset(pred_df, 
                      score>=evScore & (clicks>=minClicks | whistles>=minWhistles))
  }
  
  allpreds <- subset(allpreds, clicks > 0 | whistles > 0)
  pred_df <- subset(pred_df, clicks > 0 | whistles > 0)
  
  cat(sprintf('\n\nDone. Loaded %s event classifications. \n\n', nrow(pred_df)))

  preds <- pred_df
  PCAdf <- pcadf
  info <- list(preds=preds, allpreds=allpreds, PCAdf=PCAdf)
  preds$barcode <- as.character(preds$barcode)
  
  newdata <- data.frame()
  for (i in 1:nrow(preds)) {
    cols <- names(preds)
    row <- list()
    for (col in cols) {
      row[[col]] <- preds[[col]][i]
    }
    row <- data.frame(row)
    bc <- preds$barcode[i]
    vals <- c()
    for (j in seq(1, 24, 2)) {
      vals <- append(vals, as.integer(substr(bc, j, (j+1)))/100)
    }
    vals <- t(as.data.frame(vals))
    rownames(vals) <- c(1)
    colnames(vals) <- paste0('VAR', 1:length(vals))
    row <- cbind(row, vals)
    newdata <- rbind(newdata, row)
    rownames(newdata) <- 1:nrow(newdata)
  }
  
  preds <- newdata
  
  if (export==TRUE) {
    if (!is.null(append_to_file)) {
      existing_data <- read.csv(sprintf('classifications/%s.csv', append_to_file), fill=TRUE)
      max_uid <- max(existing_data$uid)
      if (ncol(existing_data) == ncol(preds)) {
        rownames(preds) <- (max_uid + 1):(max_uid + nrow(preds))
        new_data <- rbind(existing_data, preds)
        write.csv(new_data, sprintf('classifications/%s.csv', append_to_file), row.names = FALSE)
      }
    } else {
      current_date <- format(Sys.Date(), "%d%m%y")
      current_time <- format(Sys.time(), "%H%M")
      
      write.csv(preds, sprintf('classifications/groupClassifications-%s%s.csv', current_date, current_time), row.names = FALSE)
    }
  }
  
  return(info)
}

plotClassifications <- function(preds, PCAdf, evScore=0, plotType='Map', classifierType='delphinID') {
  if (grepl('Count', plotType)) {
    df <- preds
    x <- seq(0,1,0.01)
    y <- as.numeric(evScore)/x
    thr <- data.frame(x=x, y=y)
    
    if (classifierType %in% c('delphinID', 'ROCCA')) {
      all_levels <- c('De. delphis', 'Gr. griseus', 'Gl. melas', 'La. acutus',
                      'La. albirostris', 'Or. orca', 'Tu. truncatus')
    } else { 
      all_levels <- model$classes
    }
    
    df$predictedSpecies <- factor(df$predictedSpecies, levels = all_levels)
    df_full <- data.frame(predictedSpecies = all_levels) %>%
      left_join(df %>% count(predictedSpecies), by = "predictedSpecies") %>%
      mutate(n = ifelse(is.na(n), 0, n))
    
    ymax <- max(df_full$n)
    block <- as.integer(max(df_full$n)/10)
    if (block < 1) {
      block <- 1
    }
    
    p<-ggplot(data = df_full, 
              aes(x = predictedSpecies, y=n, fill = predictedSpecies)) +
      geom_bar(stat='identity', fill='#849d7c', color = "#849d7c") +
      theme_minimal() +
      scale_y_continuous(breaks = seq(1, ymax, by = block)) +
      theme(
        axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
    
  } else {
    df <- PCAdf
    df <- subset(df, clicks > 0 | whistles > 0)
    
    if (classifierType %in% c('delphinID', 'ROCCA')) {
      all_levels <- c('De. delphis', 'Gr. griseus', 'Gl. melas', 'La. acutus',
                      'La. albirostris', 'Or. orca', 'Tu. truncatus')
    } else { 
      all_levels <- model$classes
    }
    
    label_counts <- df %>%
      group_by(predictedSpecies) %>%
      summarise(n = n()) %>%
      mutate(label_n = paste0(predictedSpecies, " (n = ", n, ")"))
    
    df <- df %>% left_join(label_counts, by='predictedSpecies')
    
    p <-ggplot(df, aes(x = PC1, y = PC2, color = label_n, label=1:nrow(df))) +
      geom_text(size = 3, alpha = 0.6, show.legend = FALSE) +
      stat_ellipse(level = 0.90) +
      theme_minimal(base_size = 8) +
      theme(legend.text = element_text(size = 8),
            axis.title.x = element_text(size=8),
            axis.title.y = element_text(size=8),
      ) +
      xlab('Component 1') +
      ylab('Component 2') +
      guides(color = guide_legend(title = "Classified species"))
  }
  
  print(p)
  return(p)
}

pruneData <- function(data, targetVar, vars, prune=0.10) {
  predictor_data <- data %>% select(all_of(vars))
  pca_result <- prcomp(predictor_data, scale. = FALSE) 
  pca_scores <- as.data.frame(pca_result$x)
  
  pca_scores_with_target <- pca_scores %>%
    mutate(targetVar = data[[targetVar]], id=1:nrow(pca_scores))
  
  centroids <- pca_scores_with_target %>%
    group_by(targetVar) %>%
    summarise(across(starts_with("PC"), mean))
  
  euclidean_distance <- function(row, centroid) {
    sqrt(sum((row - centroid)^2))
  }
  
  distances <- pca_scores_with_target %>%
    rowwise() %>% # Operate row by row
    mutate(
      centroid_PC1 = centroids$PC1[centroids$targetVar == targetVar],
      centroid_PC2 = centroids$PC2[centroids$targetVar == targetVar],
      distance_to_centroid = euclidean_distance(
        c(PC1, PC2),
        c(centroid_PC1, centroid_PC2)
      )
    ) %>%
    ungroup() 
  
  removeRows <- character(0) # Initialize an empty character vector to store rownames
  
  for (level in unique(distances$targetVar)) {
    subset_distances <- distances %>%
      filter(targetVar == level)
    
    thr <- quantile(subset_distances$distance_to_centroid, 1-prune)
    rows_to_remove <- subset(subset_distances, distance_to_centroid >= thr)$id
    removeRows <- c(removeRows, rows_to_remove)
  }
  
  data <- data[-as.integer(removeRows), ]
  return(data)
}

imputeData <- function(data, targetVar, groupVar, impLim=2, verbose=FALSE) {
  fill_value <- 'random_sample' # Or 'mean' if you want to switch back
  count <- 1 # Initialize count if you intend to use it elsewhere
  
  # Get unique species levels
  unique_species <- unique(data[[targetVar]])
  
  # Specify the VAR columns for clicks and whistles
  vars_clicks <- paste0("VAR", 1:5)[paste0("VAR", 1:5) %in% names(data)]
  vars_whistles <- paste0("VAR", 6:12)[paste0("VAR", 6:12) %in% names(data)]
  
  # --- Iterate through the rows of data and impute ---
  for (i in 1:nrow(data)) {
    current_species <- data[[targetVar]][i]
    current_enc <- data[[groupVar]][i]
    
    # --- Impute for zero clicks ---
    if (data$clicks[i] < impLim) {
      species_non_zero_clicks <- data[data[[targetVar]] == current_species & data[[groupVar]] == current_enc & data$clicks != 0, vars_clicks]
      num_non_zero <- nrow(species_non_zero_clicks)
      if (num_non_zero >= 1) {
        # Randomly select one row of non-zero values
        random_index <- sample(1:num_non_zero, 1)
        imputed_values <- species_non_zero_clicks[random_index, ]
        data[i, vars_clicks] <- imputed_values
      } else {
        if (verbose == TRUE) {
          cat(paste("Warning: No non-zero clicks found for species:", current_species, "at row", i, ". Skipping imputation for VAR1-VAR5.\n"))
        }
      }
    }
    
    # --- Impute for zero whistles ---
    if (data$whistles[i] == impLim) {
      species_non_zero_whistles <- data[data[[targetVar]] == current_species & data[[groupVar]] == current_enc & data$whistles != 0, vars_whistles]
      num_non_zero <- nrow(species_non_zero_whistles)
      if (num_non_zero >= 1) {
        # Randomly select one row of non-zero values
        random_index <- sample(1:num_non_zero, 1)
        imputed_values <- species_non_zero_whistles[random_index, ]
        data[i, vars_whistles] <- imputed_values
      } else {
        if (verbose == TRUE) {
          cat(paste("Warning: No non-zero whistles found for species:", current_species, "at row", i, ". Skipping imputation for VAR6-VAR12.\n"))
        }
      }
    }
  }
  return(data)
}

loadResults <- function(filename, targetVar='species', groupVar='encID', samples_omit=NULL, startVar='VAR1', endVar='VAR12',
                        minClicks=0, minWhistles=0, AndOr='or', plot_data=TRUE, verbose=FALSE) {
  
  data_cmb <- read.csv(sprintf('classifications/%s.csv', filename), fill=TRUE)
  if (!targetVar %in% colnames(data_cmb)) {
    stop(sprintf('%s not found in table.\n', targetVar))
  }
  if (!groupVar %in% colnames(data_cmb)) {
    stop(sprintf('%s not found in table.\n', groupVar))
  }
  
  if (!'eventID' %in% colnames(data_cmb)) {
    stop('eventID not found in table.\n')
  }

  targets <- unique(data_cmb[[targetVar]])[order(unique(data_cmb[[targetVar]]))]
  for (target in targets) {
    sub <- subset(data_cmb, data_cmb[[targetVar]] == target)
    numGroups <- length(unique(sub[[groupVar]]))
    numSamps <- nrow(sub)
    if (verbose == TRUE) {
      cat(sprintf('%s: %s samples from %s groups\n', target, numSamps, numGroups))
    }
  }
  
  for (omit_cat in names(samples_omit)) {
    omit <- unlist(samples_omit[[omit_cat]])
    data_cmb <- subset(data_cmb, !data_cmb[[omit_cat]] %in% omit)
  }
  
  if (AndOr %in% c('And', 'and')) {
    data_cmb <- subset(data_cmb, clicks >= minClicks & whistles >= minWhistles)
  } else {
    data_cmb <- subset(data_cmb, clicks >= minClicks | whistles >= minWhistles)
  }
  
  newdata <- data.frame()
  for (i in 1:nrow(data_cmb)) {
    sp <- data_cmb[[targetVar]][i]
    enc <- data_cmb[[groupVar]][i]
    row <- data.frame(uid=i, target=sp, group=enc, eventID=data_cmb$eventID[i], 
                      clicks=data_cmb$clicks[i], whistles=data_cmb$whistles[i], minutes=data_cmb$minutes[i], barcode=data_cmb$barcode[i])
    
    row1 <- list()
    for (var in paste0('VAR', 1:12)) {
      row1[[var]] <- data_cmb[[var]][i]
    }
    row1 <- data.frame(row1)
    row <- cbind(row, row1)
    newdata <- rbind(newdata, row)
    rownames(newdata) <- 1:nrow(newdata)
  }
  
  data_cmb <- newdata
  data_cmb <- data_cmb %>% rename_with(~ targetVar, .cols = "target")
  if (groupVar != 'eventID') {
    data_cmb <- data_cmb %>% rename_with(~ groupVar, .cols = "group")
  }
  
  targets <- unique(data_cmb[[targetVar]])
  groups <- unique(data_cmb[[groupVar]])
  ind1 <- which(names(data_cmb)==startVar)
  ind2 <- which(names(data_cmb)==endVar)
  vars <- names(data_cmb)[ind1:ind2]
  
  if (plot_data == TRUE) {
    pca_result <- prcomp(data_cmb[, vars], scale. = FALSE)
    pc_data <- as.data.frame(pca_result$x[, 1:2])
    colnames(pc_data) <- c("PC1", "PC2")
    data_pca <- cbind(data_cmb, pc_data)
    
    p <- ggplot(data_pca, aes(x = PC1, y = PC2, color = .data[[targetVar]])) +
      geom_point(size=2) +
      labs(x = "(PC1)",
           y = "(PC2)") +
      theme_minimal() +
      scale_color_brewer(palette = "Set2")
    
    print(p)
  }
  
  info <- list(data=data_cmb, variables=vars, targets=unique(data_cmb[[targetVar]]), groups=unique(data_cmb[[groupVar]]))
  return(info)
}

trainClassifier <- function(data_cmb, vars, targetVar='species', groupVar='eventID', 
                            nTrees=500, mtry=NULL, ns=1, prune=0, impute=NULL, verbose=FALSE, export=FALSE, savefolder=NULL) {
  if (verbose == TRUE) {
    cat(sprintf('Training classifiers over %s-fold cross-validation...\n', nrow(data_cmb)))
  }
  
  groupCount <- 0
  minEx <- 0
  results_cmb <- data.frame()
  for (k in 1:nrow(data_cmb)) {
    groupCount <- groupCount + 1
    groupTest <- data_cmb[[groupVar]][k]
    xtest <- data_cmb[k,]
    if (xtest$clicks >= minEx | xtest$whistles >= minEx) {
      xtrain <- subset(data_cmb, data_cmb[[groupVar]]!=groupTest)
      
      if (!is.null(impute)) {
        if (impute > 0) {
          xtrain <- imputeData(xtrain, 
                               targetVar=targetVar, 
                               groupVar=groupVar, 
                               impLim=impute)
        }
      }
      
      
      xtrain <- pruneData(xtrain, 
                          targetVar=targetVar, 
                          vars=vars, 
                          prune=prune)
      
      if (length(unique(xtrain[[targetVar]])) != length(targets)) {
        xtrain <- subset(data_cmb, data_cmb[[groupVar]]!=groupTest)
      }
      
      sampsizes <- rep(min(table(xtrain[[targetVar]])), 
                       length(unique(xtrain[[targetVar]])))
      
      formula_str <- paste(sprintf("as.factor(%s) ~", targetVar), 
                           paste(vars, collapse = " + "))
      
      if (is.null(mtry)) {
        mtry <- floor(sqrt(length(vars)))
      }
      
      m <- randomForest(as.formula(formula_str),
                        data = xtrain,
                        ntree = nTrees,
                        mtry = mtry, 
                        nodesize=ns,
                        strata = as.factor(xtrain[[targetVar]]),
                        sampsize = sampsizes,
                        na.action = na.roughfix,
                        keep.inbag = TRUE)
      
      label <- xtest[[targetVar]][1]
      predFrame <- as.numeric(predict(m, xtest, type='prob'))
      targets <- colnames(predict(m, xtest, type='prob'))
      pred <- targets[which.max(predFrame)]
      predFrame <- predFrame[rev(order(predFrame))]
      conf <- predFrame[1]
      prom <- predFrame[1] - predFrame[2]
      score <- conf*prom
      row <- list()
      row[['id']] <- groupCount
      row[[targetVar]] <- label
      row[[groupVar]] <- groupTest
      row[['eventID']] <- xtest$eventID[1]
      row[['pred']] <- pred
      row[['score']] <- score
      
      predFrame <- as.numeric(predict(m, xtest, type='prob'))
      for (i in 1:length(predFrame)) {
        row[[targets[i]]] <- predFrame[i]
      }
      
      if (verbose == TRUE) {
        cat(sprintf('(%s/%s) %s (%s): prediction %s (%s)\n', k, nrow(data_cmb), groupTest, label, pred, round(score, 3)))
      }
      row <- as.data.frame(row)
      
      results_cmb <- rbind(results_cmb, row)
      rownames(results_cmb) <- 1:nrow(results_cmb)
    }
  }
  
  if (export==TRUE) {
    if (is.null(savefolder)) {
      dirName <- makeDirSysDT()
    } else {
      dirName <- savefolder
    }
    
    write.csv(data_cmb, sprintf('%s/trainingData.csv', savefolder), row.names = FALSE)
    write.csv(results_cmb, sprintf('%s/groupClassifications.csv', savefolder), row.names = FALSE)
    saveRDS(m, sprintf('%s/classifier.rds', savefolder))
  }
  
  return(results_cmb)
}

summResults <- function(df, targetVar, minScore=0, digits = 2) {
  nTot <- nrow(df)
  # 1. Check for required columns and classes
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data.frame.")
  }
  if (!all(c(targetVar, "pred") %in% names(df))) {
    stop(sprintf("Input 'df' must contain columns named '%s' and 'pred'.", targetVar))
  } else {
    df[[targetVar]] <- as.factor(df[[targetVar]])
    df$pred <- as.factor(df$pred)
  }
  if (!is.factor(df[[targetVar]]) || !is.factor(df$pred)) {
    stop(sprintf("Columns '%s' and 'pred' must be factors.", targetVar))
  }
  
  df <- subset(df, score >= minScore)
  acc_all <- sum(df$pred == df[[targetVar]])/nrow(df)
  acc_mean <- c()
  for (group in unique(df[[targetVar]])) {
    sub <- subset(df, df[[targetVar]] == group)
    acc_mean <- append(acc_mean, sum(sub$pred == sub[[targetVar]])/nrow(sub))
  }
  
  lowAcc <- min(acc_mean)
  highAcc <- max(acc_mean)
  lowGroup <- unique(df[[targetVar]])[which.min(acc_mean)]
  highGroup <- unique(df[[targetVar]])[which.max(acc_mean)]
  pctClassified <- nrow(df)/nTot
  acc_mean <- mean(acc_mean)
  
  # 2. Create the confusion matrix as a data.table
  conf_matrix_dt <- as.data.table(table(df[[targetVar]], df$pred))
  setnames(conf_matrix_dt, c("Actual", "Predicted", "Count"))
  
  # 3. Calculate percentages
  conf_matrix_dt[, `Total Actual` := sum(Count), by = Actual]
  conf_matrix_dt[, Percentage := round((Count / `Total Actual`), digits), by = Actual]
  conf_matrix_dt[, Display := paste0(Count, " (", Percentage, ")")]
  
  # 4. Pivot the data.table for better presentation
  conf_matrix_wide <- dcast(conf_matrix_dt, Actual ~ Predicted, value.var = "Display")
  
  # 5.  Return the data.table
  print(conf_matrix_wide)
  
  # 6.  Print summary of results
  cat(sprintf('\n\033[1mClassification results, %s-fold cross-validation:\033[0m\n', nTot))
  cat(sprintf('Classifications discarded: %s (%s/%s)\n', round((1-pctClassified), 3), nrow(df), nTot))
  cat(sprintf('Overall accuracy: %s\n', round(acc_all, 3)))
  cat(sprintf('Mean %s accuracy: %s\n', targetVar, round(acc_mean, 3)))
  cat(sprintf('Lowest: %s (%s)\n', round(lowAcc, 3), lowGroup))
  cat(sprintf('Highest: %s (%s)', round(highAcc, 3), highGroup))
}

cat('\u2713 Done loading functions.\n')

suppressWarnings(library(data.table))
suppressWarnings(library(randomForest))
suppressWarnings(library(ggplot2))
suppressWarnings(library(scales))
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(igraph))
suppressWarnings(library(ggraph))


summData <- function(data, hierarchy) {
  if (!is.data.table(data)) {
    setDT(data)
  }
  
  first_level <- hierarchy[1]
  later_levels <- tail(hierarchy, -1)
  
  summary_dt <- data[, .N, by = first_level]
  setnames(summary_dt, "N", "n")
  
  for (level in later_levels) {
    level_counts <- data[, uniqueN(get(level)), by = first_level]
    setnames(level_counts, "V1", level)
    summary_dt <- merge(summary_dt, level_counts, by = first_level, all.x = TRUE)
  }
  
  print(summary_dt)
}

summVars <- function(data, hierarchy, vars) {
  if (!is.data.table(data)) {
    setDT(data)
  }
  
  if (length(hierarchy) < 1) {
    stop("Hierarchy must have at least one level.")
  }
  
  first_level <- hierarchy[1]
  
  if (!all(vars %in% names(data))) {
    stop("One or more variables in 'vars' not found in the data.")
  }
  
  # Calculate the average of each variable for each category of the first level
  average_dt <- data[, lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 3)), .SDcols = vars, by = first_level]
  
  # Transpose the table to have variables as rows and first-level categories as columns
  melted_dt <- melt(average_dt, id.vars = first_level, variable.name = "variable", value.name = "average")
  output_dt <- dcast(melted_dt, variable ~ get(first_level), value.var = "average")
  setnames(output_dt, "variable", "Variable") # Capitalize 'Variable' for consistency
  
  print(output_dt)
}

loadDataFromHier <- function(root_path, startVar, endVar, vocType, from_folders=FALSE, levels=NULL, omitVars=c(), filterVarsMin=c(), filterVarsMax=c(), file_pattern='RoccaContourStats', file_type='csv', show_output=TRUE) {
  all_data <- list()
  if (vocType %in% c('whistle', 'whistles', 'Whistles', 'Whistle')) {
    startVar <- 'FREQMAX'
    endVar <- 'STEPDUR'
  } else if (vocType %in% c('click', 'clicks', 'Click', 'Clicks')) {
    startVar <- 'DURATION'
    endVar <- 'VARIANCETIMEZC'
  }
  
  if (from_folders == FALSE) {
    meta_df <- data.frame()
    dirfiles <- dir(root_path)
    hierarchy_levels <- c('KnownSpecies', 'EncounterID')
    for (file in dirfiles) {
      if (grepl('RoccaContourStats', file) & grepl('.csv', file)) {
        x <- read.csv(sprintf('%s/%s', root_path, file))
        if (vocType %in% c('whistle', 'whistles', 'Whistles', 'Whistle')) {
          x <- subset(x, FREQMAX > 0 & FREQPEAK == 0)
        } else if (vocType %in% c('click', 'clicks', 'Click', 'Clicks')) {
          x <- subset(x, FREQMAX == 0 & FREQPEAK != 0)
        }
        meta_df <- rbind(meta_df, x)
        rownames(meta_df) <- 1:nrow(meta_df)
      }
    }
    
    ind1 <- which(names(meta_df)==startVar)
    ind2 <- which(names(meta_df)==endVar)
    if (vocType %in% c('click', 'clicks', 'Click', 'Clicks')) {
      omitVars <- append(omitVars, names(meta_df[ind1:ind2])[!names(meta_df[ind1:ind2]) %in% c('DURATION', 'FREQCENTER')])
      omitVars <- omitVars[!omitVars %in% c('FREQPEAK', 'BW3DB', 'BW3DBLOW', 'BW3DBHIGH','BW10DB', 'BW10DBLOW', 'BW10DBHIGH', 
                                            'NCROSSINGS', 'SWEEPRATE', 'MEANTIMEZC', 'MEDIANTIMEZC', 'VARIANCETIMEZC')]
    }
    
    vars <- names(meta_df)[ind1:ind2]
    vars <- vars[!vars %in% omitVars]
    
    for (var in names(filterVarsMin)) {
      val <- filterVarsMin[[var]]
      meta_df <- subset(meta_df, meta_df[[var]] >= val)
      rownames(meta_df) <- 1:nrow(meta_df)
    }
    for (var in names(filterVarsMax)) {
      val <- filterVarsMax[[var]]
      meta_df <- subset(meta_df, meta_df[[var]] <= val)
      rownames(meta_df) <- 1:nrow(meta_df)
    }
    
    if (show_output == TRUE) {
      summVars(meta_df, hierarchy_levels, vars)
      summData(meta_df, hierarchy_levels)
    }
    
    meta_df$species <- meta_df$KnownSpecies
    meta_df$encounter <- meta_df$EncounterID
    information <- list(meta_df, vars)
    return(information)
    
  } else {
    hierarchy_levels <- levels

    process_level <- function(current_path, level_index, current_level_names = list(), accumulated_data = list()) {
      if (level_index > length(hierarchy_levels)) {
        files_to_load <- list.files(
          current_path,
          pattern = sprintf("%s.*\\.%s$", file_pattern, file_type),
          full.names = TRUE
        )
        
        
        for (file_path in files_to_load) {
          tryCatch({
            current_data <- read.csv(file_path)
            if (vocType %in% c('whistle', 'whistles', 'Whistles', 'Whistle')) {
              current_data <- subset(current_data, FREQMAX != 0 & FREQPEAK == 0)
            } else if (vocType %in% c('click', 'clicks', 'Click', 'Clicks')) {
              current_data <- subset(current_data, FREQMAX == 0 & FREQPEAK != 0)
            }
            
            ind1 <- which(names(current_data) == startVar)
            ind2 <- which(names(current_data) == endVar)
            if (vocType %in% c('click', 'clicks', 'Click', 'Clicks')) {
              omitVars <- append(omitVars, names(current_data[ind1:ind2])[!names(current_data[ind1:ind2]) %in% c('DURATION', 'FREQCENTER')])
              a <- which(names(meta_df) == 'FREQPEAK')
              b <- which(names(meta_df) == 'VARIANCETIMEZC')
              omitVars <- omitVars[!omitVars %in% names(meta_df)[a:b]]
            }
            
            current_data <- current_data[,ind1:ind2]
            if (nrow(current_data) > 0) {
              for (i in seq_along(hierarchy_levels)) {
                if (i <= length(current_level_names)) {
                  current_data[[hierarchy_levels[i]]] <- current_level_names[[i]]
                }
              }
              accumulated_data[[length(accumulated_data) + 1]] <- current_data # Append to the passed list
            }
          }, error = function(e) {
            cat(paste("Error reading file:", file_path, "\n"))
            print(e)
          })
        }
        return(accumulated_data) # Return the modified list
      }
      
      sub_dirs <- list.dirs(current_path, full.names = TRUE, recursive = FALSE)
      
      for (dir_path in sub_dirs) {
        current_name <- basename(dir_path)
        accumulated_data <- process_level(dir_path, level_index + 1, c(current_level_names, current_name), accumulated_data) # Update the list
      }
      return(accumulated_data) # Return the accumulated data from this level
    }
    
    all_data <- process_level(root_path, 1, accumulated_data = list()) # Initialize and capture the returned list
    
    if (length(all_data) > 0) {
      meta_df <- do.call(rbind, all_data)
      ind1 <- which(names(meta_df)==startVar)
      ind2 <- which(names(meta_df)==endVar)
      vars <- names(meta_df)[ind1:ind2]
      vars <- vars[!vars %in% omitVars]
      for (var in names(filterVarsMin)) {
        val <- filterVarsMin[[var]]
        meta_df <- subset(meta_df, meta_df[[var]] >= val)
        rownames(meta_df) <- 1:nrow(meta_df)
      }
      for (var in names(filterVarsMax)) {
        val <- filterVarsMax[[var]]
        meta_df <- subset(meta_df, meta_df[[var]] <= val)
        rownames(meta_df) <- 1:nrow(meta_df)
      }
      
      if (show_output == TRUE) {
        summVars(meta_df, hierarchy_levels, vars)
        summData(meta_df, hierarchy_levels)
      }
      
      original <- nrow(meta_df)
      meta_df <- meta_df %>% drop_na()
      dropped_rows <- original - nrow(meta_df)
      information <- list(meta_df, vars)
      return(information)
    } else {
      cat("No matching files found.\n")
      return(NULL)
    }
  }

}

limitByGroup <- function(df, groupVar, nMax) {
  if (!is.data.table(df)) {
    setDT(df)
  }
  
  if (!groupVar %in% names(df)) {
    stop(paste("Error: Grouping variable '", groupVar, "' not found in the data frame."))
  }
  
  # Create an index within each group
  df[, .group_index := 1:.N, by = groupVar]
  
  # Filter rows where the index is less than or equal to nMax
  filtered_df <- df[.group_index <= nMax]
  
  # Remove the temporary index column
  filtered_df[, .group_index := NULL]
  
  return(filtered_df)
}

getScore <- function(x) {
  x <- unlist(x)
  x <- x[rev(order(x))]
  return(as.numeric(x[1]*(x[1]-x[2])))
}

getPred <- function(x) {
  labels <- names(x)
  x <- unlist(x)
  return(labels[which.max(x)])
}

classifyData <- function(data, targetVar, groupVar, vars, select_groups=c(), omit=c(), prune=0, minScore=0, nMax=1000, nTrees=500, mtry=NULL, node_size=1, verbose=TRUE) {
  results <- data.frame()
  allpreds <- data.frame()
  data$uid <- 1:nrow(data)
  for (var in names(omit)) {
    items <- unlist(omit[[var]])
    data <- subset(data, !data[[var]] %in% items)
  }
  
  if (is.null(mtry)) {
    mtry <- sqrt(length(vars))
  }
  
  all_labels <- unique(data[[targetVar]])[order(unique(data[[targetVar]]))]
  all_groups <- unique(data[[groupVar]])[order(unique(data[[groupVar]]))]

  if (length(select_groups) > 0) {
    all_groups <- select_groups
  }
  
  cat(sprintf('Classifying %s groups...\n', length(all_groups)))
  for (group in all_groups) {
    xtest <- subset(data, data[[groupVar]]==group)
    xtrain <- subset(data, data[[groupVar]]!=group)
    if (prune > 0) {
      xtrain <- pruneData(xtrain, targetVar=targetVar, vars=vars, prune=prune)
    }
    xtrain <- limitByGroup(xtrain, groupVar=groupVar, nMax=nMax)
    if (nrow(xtrain) > 0 & nrow(xtest) > 0) {
      sampsizes <- rep(min(table(xtrain[[targetVar]])), length(unique(xtrain[[targetVar]])))
      formula_str <- paste(sprintf("as.factor(%s) ~", targetVar), paste(vars, collapse = " + "))
      m <- randomForest(as.formula(formula_str),
                        data = xtrain,
                        ntree = nTrees,
                        mtry = mtry, 
                        nodesize = node_size,
                        strata = as.factor(xtrain[[targetVar]]),
                        sampsize = sampsizes,
                        na.action = na.roughfix)
      
      predFrame <- predict(m, xtest, type='prob')
      testUID <- xtest$uid
      testUID <- testUID[apply(predFrame, 1, getScore) >= minScore]
      predFrame <- predFrame[apply(predFrame, 1, getScore) >= minScore,]
      xtest <- xtest[xtest$uid %in% testUID, ]

      if (!is.null(dim(predFrame)) & nrow(xtest) > 0) {
        probs <- colMeans(predFrame)
        predFrame <- data.frame(predFrame)
        predList <- unlist(apply(predFrame, 1, getPred))
        scoreList <- unlist(apply(predFrame, 1, getScore))
        predFrame$pred <- predList
        predFrame$score <- scoreList
        predFrame[[targetVar]] <- unique(xtest[[targetVar]])[1]
        predFrame[[groupVar]] <- group
        predFrame <- cbind(predFrame, xtest)
        allpreds <- rbind(allpreds, predFrame)
        
        pred <- all_labels[which.max(probs)]
        conf <- max(probs)
        prom <- probs[rev(order(probs))][1] - probs[rev(order(probs))][2]
        probs <- data.frame(probs)
        score <- prom*conf
        
        predrow <- data.frame(dummy_col = NA)
        predrow[[targetVar]] <- unique(xtest[[targetVar]])[1]
        predrow[[groupVar]] <- group
        predrow$n <- nrow(xtest)
        predrow$pred <- pred
        predrow$score <- score
        count_lab <- 1
        for (lab in all_labels) {
          predrow[[lab]] <- probs[count_lab,1]
          count_lab <- count_lab + 1
        }
        results <- rbind(results, predrow)
        if (verbose == TRUE) {
          cat(sprintf('%s (%s) - prediction: %s (%s)\n', group,  unique(xtest[[targetVar]])[1], pred, round(score, 3)))
        }
      } else {
        cat(sprintf('%s (%s) - no predictions\n', group, unique(xtest[[targetVar]])[1]))
      }
    }
  }
  
  xtrain <- data
  if (prune > 0) {
    xtrain <- pruneData(xtrain, targetVar=targetVar, vars=vars, prune=prune)
  }
  xtrain <- limitByGroup(xtrain, groupVar=groupVar, nMax=nMax)
  sampsizes <- rep(min(table(xtrain[[targetVar]])), length(unique(xtrain[[targetVar]])))
  formula_str <- paste(sprintf("as.factor(%s) ~", targetVar), paste(vars, collapse = " + "))
  m <- randomForest(as.formula(formula_str),
                    data = xtrain,
                    ntree = nTrees,
                    mtry = mtry, 
                    strata = as.factor(xtrain[[targetVar]]),
                    sampsize = sampsizes,
                    na.action = na.roughfix,
                    importance=TRUE)
  
  
  rownames(allpreds) <- 1:nrow(allpreds)
  rownames(results) <- 1:nrow(results)
  
  return(list(groupPreds=results[,2:ncol(results)], allPreds=allpreds, model=m))
}

summResults <- function(df, targetVar, minScore=0, digits = 2) {
  nTot <- nrow(df)
  # 1. Check for required columns and classes
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data.frame.")
  }
  if (!all(c("species", "pred") %in% names(df))) {
    stop("Input 'df' must contain columns named 'species' and 'pred'.")
  } else {
    df$species <- as.factor(df$species)
    df$pred <- as.factor(df$pred)
  }
  if (!is.factor(df$species) || !is.factor(df$pred)) {
    stop("Columns 'species' and 'pred' must be of class factor.")
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
  conf_matrix_dt <- as.data.table(table(df$species, df$pred))
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
  cat(sprintf('\033[1mClassification results, %s-fold cross-validation:\033[0m\n', nTot))
  cat(sprintf('Classifications discarded: %s (%s/%s)\n', round((1-pctClassified), 3), nrow(df), nTot))
  cat(sprintf('Overall accuracy: %s\n', round(acc_all, 3)))
  cat(sprintf('Mean %s accuracy: %s\n', targetVar, round(acc_mean, 3)))
  cat(sprintf('Lowest: %s (%s)\n', round(lowAcc, 3), lowGroup))
  cat(sprintf('Highest: %s (%s)', round(highAcc, 3), highGroup))
}

plotResults <- function(results, results_all, model, targetVar, show_vars=15, thrMax=0.10, point_size=1, line_width=1, 
                        axis_title_fontsize=14, axis_title_margin=10, legend_fontsize=12, border_col=NULL,
                        show_plots=TRUE, export=FALSE, savefolder=NULL, plot_dims=c(8,6), plot_DPI=600,
                        point_transparency=1, axis_tick_fontsize=10, background='white') {
  
  thrList <- seq(0, thrMax, 0.01)
  RetAcc <- data.frame()
  for (thr in thrList) {
    sub <- subset(results, score >= thr)
    group_accs <- c()
    for (group in unique(sub[[targetVar]])) {
      subsub <- subset(sub, sub[[targetVar]] == group)
      group_accs <- append(group_accs, sum(subsub$pred == subsub[[targetVar]])/nrow(subsub))
    }
    row <- data.frame(label='ALL', minScore=thr, retention=(100*((nrow(sub)/nrow(results)))), ovr_acc=(100*(sum(sub$pred==sub[[targetVar]])/nrow(sub))), mean_acc=(100*mean(group_accs)))
    RetAcc <- rbind(RetAcc, row)
    
    for (lab in unique(sub[[targetVar]])) {
      subsub <- subset(sub, sub[[targetVar]]==lab)
      labAll <- subset(results, results[[targetVar]]==lab)
      row <- data.frame(label=lab, minScore=thr, retention=100*((nrow(subsub)/nrow(labAll))), ovr_acc=100*(sum(subsub$pred==subsub[[targetVar]])/nrow(subsub)), mean_acc=100*(sum(subsub$pred==subsub[[targetVar]])/nrow(subsub)))
      RetAcc <- rbind(RetAcc, row)
    }
  }
  
  label_colors <- c("ALL" = "black")
  other_labels <- setdiff(unique(RetAcc$label), "ALL")
  choose_colors <- c('#E69F00', '#56B4E9', '#009E73', '#D55E00', '#CC79A7', '#0072B2', '#E2D630')
  num_other_labels <- length(other_labels)
  if (num_other_labels > 0) {
    other_colors <- c()
    if (num_other_labels > length(choose_colors)) {
      for (i in 1:num_other_labels) {
        other_colors <- append(other_colors, rgb(runif(1,0,1), runif(1,0,1), runif(1,0,1), 1))
      }
    } else {
      for (i in 1:num_other_labels) {
        other_colors <- append(other_colors, choose_colors[i])
      }
    }
    
    label_colors[other_labels] <- other_colors
  }
  
  label_sizes <- c("ALL" = 2.5*point_size)
  default_size <- 1.5*point_size
  label_sizes[setdiff(unique(RetAcc$label), "ALL")] <- default_size
  label_alphas <- c("ALL" = 1)
  default_alpha = 0.5
  label_alphas[setdiff(unique(RetAcc$label), "ALL")] <- default_alpha
  
  subALL <- subset(RetAcc, label == 'ALL')
  
  p1 <- varImpPlot(model, 
             type = 2, 
             main = "Variable Importance \n(Mean Decrease Gini)", 
             sort = TRUE,
             n.var = show_vars, # Show top 10 variables
             col = "steelblue") 
  
  p2 <- ggplot(data = subALL) +
    theme_bw() +
    geom_line(aes(x = minScore, y = ovr_acc, colour = 'Overall accuracy'), lw=line_width) +
    geom_point(aes(x = minScore, y = ovr_acc, colour = 'Overall accuracy'), size=point_size, alpha=point_transparency) +
    geom_line(aes(x = minScore, y = mean_acc, colour = 'Mean accuracy'), lw=line_width) +
    geom_point(aes(x = minScore, y = mean_acc, colour = 'Mean accuracy'), size=point_size, alpha=point_transparency) +
    geom_line(aes(x = minScore, y = retention, colour = '% groups classified'), lw=line_width) +
    geom_point(aes(x = minScore, y = retention, colour = '% groups classified'), size=point_size, alpha=point_transparency) +
    scale_colour_manual(
      name = NULL,
      values = c('Overall accuracy' = '#000000',
                 'Mean accuracy' = '#E69F00',
                 '% groups classified' = '#56B4E9')
    ) +
    xlab('Minimum prediction score') +
    ylab(NULL) + 
    ylim(c(0,100)) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = axis_title_margin), size=axis_title_fontsize),
      axis.title.y = element_text(face = "bold", margin = margin(r = axis_title_margin), size=axis_title_fontsize),
      axis.text.x = element_text(size = axis_tick_fontsize),
      axis.text.y = element_text(size = axis_tick_fontsize),
      legend.position = "top",
      legend.box = "horizontal",
      legend.justification = "left",
      legend.text = element_text(size = legend_fontsize)
    )
  
  p3 <- ggplot(data = RetAcc, aes(x = retention, y = ovr_acc, group = label, colour = label, size = label, alpha = label)) +
    theme_bw() +
    geom_point(size=point_size, alpha=point_transparency, pch=16) +
    scale_color_manual(values = label_colors) +
    scale_size_manual(values = label_sizes) +
    scale_alpha_manual(values = label_alphas) +
    xlim(0,100) +
    ylim(0,100) +
    xlab('% groups classified') +
    ylab('% of groups classified correctly') +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_text(face = "bold", margin = margin(t = axis_title_margin), size=axis_title_fontsize),
      axis.title.y = element_text(face = "bold", margin = margin(r = axis_title_margin), size=axis_title_fontsize),
      axis.text.x = element_text(size = axis_tick_fontsize),
      axis.text.y = element_text(size = axis_tick_fontsize),
      legend.text = element_text(size = legend_fontsize)
    ) +
    guides(colour = guide_legend(nrow = 1)) # Forces labels to be in a single row
  
  if (!is.null(border_col)) {
    p2 <- p2 + theme(panel.border = element_rect(colour = border_col, fill=NA, linewidth=1))
    p3 <- p3 + theme(panel.border = element_rect(colour = border_col, fill=NA, linewidth=1))
  } else {
    p2 <- p2 + theme(panel.border = element_blank())
    p3 <- p3 + theme(panel.border = element_blank())
  }
  
  if (!is.null(background)) {
    p2 <- p2 + theme(
      panel.background = element_rect(fill = background),   # White background for the plotting panel
      plot.background = element_rect(fill = background)    # White background for the entire plot area (including margins, titles, legend)
    )
    p3 <- p3 + theme(
      panel.background = element_rect(fill = background),   # White background for the plotting panel
      plot.background = element_rect(fill = background)    # White background for the entire plot area (including margins, titles, legend)
    )
    
  } else {
    p2 <- p2 + theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
    p3 <- p3 + theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
  }
  
  info <- list(varImportance=p1, plotOverall=p2, plotGroups=p3, df=RetAcc)
  if (show_plots == TRUE) {
    print(p3)
    print(p2)
  }
  
  if (export==TRUE) {
    if (is.null(savefolder)) {
      dirName <- makeDirSysDT(create=FALSE)
      if (!dirName %in% dir()) {
        dirName <- makeDirSysDT(create=TRUE)
      }
    } else {
      dirName <- savefolder
    }
    
    if (!dirName %in% dir()) {
      dir.create(dirName)
    }
    
    if (!'figures' %in% dir(dirName)) {
      dir.create(sprintf('%s/figures', dirName))
    }
    num <- 1
    for (item in dir(sprintf('%s/figures', dirName))) {
      if (grepl('dataPlot', item) & grepl('.png', item)) {
        num <- num + 1
      }
    }
    
    ggsave(sprintf('%s/figures/classifierPerformanceA.png', dirName, num), 
           plot=p2, width=plot_dims[1], height=plot_dims[2], units="cm", dpi=plot_DPI)
    
    ggsave(sprintf('%s/figures/classifierPerformanceB.png', dirName, num), 
           plot=p3, width=plot_dims[1], height=plot_dims[2], units="cm", dpi=plot_DPI)
    
    
    write.csv(results, sprintf('%s/groupPredictions.csv', dirName), row.names = FALSE)
    write.csv(results_all, sprintf('%s/individualPredictions.csv', dirName), row.names = FALSE)
    
    if (!is.null(model)) {
      saveRDS(model, sprintf('%s/classifier.rds', dirName))
    }
    
  }
  
  
  return(info)
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

combineResults <- function(dataSelect, group, targetVar, groupVar, fillValue='noise', noiseMag=20) {
  init <- 0
  nList <- list()
  for (k in 1:length(dataSelect)) {
    label <- names(dataSelect)[k]
    data <- dataSelect[k][[1]]
    sub <- subset(data, data[[groupVar]] == group)
    ind <- which(names(sub)=='score')+1
    names <- names(sub)[ind:ncol(sub)]
    if (nrow(sub) == 0) {
      sub <- list()
      sub[[targetVar]] <- 'Unk'
      sub[[groupVar]] <- group
      sub$n <- 0
      sub$pred <- 0
      sub$score <- 0
      vals <- c()
      for (name in names) {
        if (fillValue == 'noise') {
          vals <- append(vals, runif(1, 100, 100+noiseMag))
        } else if (is.numeric(fillValue)) {
          vals <- append(vals, fillValue)
        }
      }
      if (fillValue == 'noise') {
        vals <- vals/sum(vals)
      }
      vals <- as.list(vals)
      names(vals) <- names
      vals <- as.data.frame(vals)
      sub <- cbind(sub, vals)
    }
    
    if (nrow(sub) > 0 & init == 0) {
      row <- list()
      row[[targetVar]] <- sub[[targetVar]][1]
      row[[groupVar]] <- sub[[groupVar]][1]
      row <- as.data.frame(row)
      rownames(row) <- c(1)
      init <- 1
    }
    
    if (sub[[targetVar]][1] != 'Unk') {
      targetLabel <- sub[[targetVar]][1]
    } else {
      targetLabel <- 'Unk'
    }
    
    if ('n' %in% names(sub)) {
      nList[[sprintf('%s_n', label)]] <- sub$n[1]
    }
    
    ind <- which(names(sub) == 'score')+1
    subPreds <- sub[,ind:ncol(sub)]
    colnames(subPreds) <- paste0(sprintf("%s_", label), colnames(subPreds))
    row <- cbind(row, subPreds)
  }
  
  nList <- as.data.frame(nList)
  ind <- which(names(row)==groupVar)
  if ('n' %in% names(data)) {
    row <- cbind(row[,1:ind], nList, row[,(ind+1):ncol(row)])
  } else {
    row <- cbind(row[,1:ind], row[,(ind+1):ncol(row)])
  }
  
  row[[targetVar]] <- targetLabel
  rownames(row) <- c(1)
  
  return(row)
}

dataPlot <- function(d, variables=list('x'=NULL, 'y'=NULL, 'group'=NULL), point_transparency=1, point_size=1, 
                     line_width=1, axis_title_fontsize=12, axis_title_margin=10, axis_tick_fontsize=10, 
                     legend_fontsize=12, legend_names=NULL, border_col=NULL, axis_title_names=NULL,
                     export=FALSE, background='white', savefolder=NULL, plot_DPI=400, plot_dims=c(8,6)) {
  
  xVar <- unlist(variables['x'])[1]
  yVar <- unlist(variables['y'])[1]
  gVar <- unlist(variables['group'])[1]
  
  if (is.null(xVar)) {
    if (is.null(yVar)) {
      print('Error: x or y variable must be specified')
      stop()
    } else {
      xVar <- yVar
      yVar <- NULL
    }
  }
  
  num_var <- 0
  for (item in c(xVar, yVar)) {
    if (!is.null(item)) {
      num_var <- num_var + 1
    }
  }
  
  p <- ggplot(data=d) + theme_bw() # Initialize ggplot and the base theme
  
  if (!is.null(border_col)) {
    p <- p + theme(panel.border = element_rect(colour = border_col, fill=NA, linewidth=1))
  } else {
    p <- p + theme(panel.border = element_blank())
  }
  
  if (num_var == 1) {
    p <- p + geom_density(aes(x=.data[[xVar]], linewidth=line_width, colour=.data[[gVar]])) # Corrected 'lw' to 'linewidth' for geom_density
  } else if (num_var == 2) {
    p <- p + geom_point(aes(x=.data[[xVar]], y=.data[[yVar]], colour=.data[[gVar]]), alpha=point_transparency, size=point_size)
  }
  
  # Add labels and title
  # The axis title and text themes are applied to both x and y axes if either xVar or yVar is present.
  # This ensures consistent styling regardless of which variable is specified first.
  p <- p + theme(axis.title.x = element_text(face = "bold", margin = margin(t = axis_title_margin), size=axis_title_fontsize),
                 axis.title.y = element_text(face = "bold", margin = margin(r = axis_title_margin), size=axis_title_fontsize),
                 axis.text.x = element_text(size = axis_tick_fontsize),
                 axis.text.y = element_text(size = axis_tick_fontsize))
  
  if (!is.null(xVar)) p <- p + xlab(xVar)
  if (!is.null(yVar)) p <- p + ylab(yVar)
  
  if (!is.null(axis_title_names)) {
    # If custom axis names are provided, they override the variable names
    p <- p + xlab(axis_title_names[1]) + ylab(axis_title_names[2])
  }
  
  # Define the base color palette
  base_palette <- c('#E69F00', '#56B4E9', '#CC79A7', '#009E73', '#D55E00','#0072B2', '#E2D630')
  
  # Conditionally handle the legend and apply custom colors
  if (!is.null(gVar)) {
    # Get the number of unique groups
    num_groups <- length(unique(d[[gVar]]))
    
    # Initialize the final palette with the base colors
    final_palette <- base_palette
    
    # If more colors are needed, generate random hex colors
    if (num_groups > length(base_palette)) {
      colors_to_add <- num_groups - length(base_palette)
      
      # Function to generate a random hex color
      generate_random_hex_color <- function() {
        paste0("#", paste(sample(c(0:9, LETTERS[1:6]), 6, replace = TRUE), collapse = ""))
      }
      
      # Generate and add random colors
      for (i in 1:colors_to_add) {
        final_palette <- c(final_palette, generate_random_hex_color())
      }
    }
    
    p <- p + labs(colour = gVar) +
      theme(legend.position = "top",
            legend.text = element_text(size = legend_fontsize),
            legend.title = element_blank()) # Remove legend title and position at the top
    
    # Apply the custom color scale using scale_color_manual
    # If legend_names are provided, apply them as labels
    if (!is.null(legend_names)) {
      if (length(legend_names) != num_groups) {
        warning(paste("Number of manual legend labels (", length(legend_names), ") does not match number of groups (", num_groups, "). Labels may be misapplied or incomplete.", sep=""))
      }
      p <- p + scale_color_manual(values = final_palette, labels = legend_names)
    } else {
      p <- p + scale_color_manual(values = final_palette)
    }
    
  } else {
    p <- p + theme(legend.position = "none") # Remove legend if gVar is NULL
  }
  
  if (!is.null(background)) {
    p <- p + theme(
      panel.background = element_rect(fill = background),   # White background for the plotting panel
      plot.background = element_rect(fill = background)    # White background for the entire plot area (including margins, titles, legend)
    )
  } else {
    p <- p + theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
  }
  
  if (export==TRUE) {
    if (is.null(savefolder)) {
      dirName <- makeDirSysDT(create=FALSE)
      if (!dirName %in% dir()) {
        dirName <- makeDirSysDT(create=TRUE)
      }
    } else {
      dirName <- savefolder
    }
    
    if (!dirName %in% dir()) {
      dir.create(dirName)
    }
    
    figures_path <- sprintf('%s/figures', dirName)
    if (!dir.exists(figures_path)) { 
      dir.create(figures_path)
    }
    
    num <- 1
    existing_plots <- list.files(figures_path, pattern = "^dataPlot.*\\.png$")
    if (length(existing_plots) > 0) {
      plot_numbers <- as.numeric(gsub("dataPlot(\\d+)\\.png", "\\1", existing_plots))
      num <- max(plot_numbers, na.rm = TRUE) + 1
    }
    
    ggsave(sprintf('%s/dataPlot%s.png', figures_path, num), 
           plot=p, width=plot_dims[1], height=plot_dims[2], units="cm", dpi=plot_DPI)
    
    write.csv(d, sprintf('%s/allData.csv', dirName), row.names = FALSE)
  }
  
  print(p)
}

exportClassificationResults <- function(groupPreds=NULL, allPreds=NULL, plot_info=NULL, plotDPI=400, plotDims=c(8,6), model=NULL, resultsFolder=NULL) {
  if (is.null(resultsFolder)) {
    dirName <- makeDirSysDT(create=FALSE)
    if (!dirName %in% dir()) {
      dirName <- makeDirSysDT(create=TRUE)
    }
  } else {
    dirName <- resultsFolder
  }

  if (!'figures' %in% dir(dirName)) {
    dir.create(sprintf('%s/figures', dirName))
  }
  
  if (!is.null(groupPreds)) {
    write.csv(groupPreds, sprintf('%s/groupPredictions.csv', dirName), row.names=FALSE)
  }
  
  if (!is.null(allPreds)) {
    write.csv(allPreds, sprintf('%s/allPredictions.csv', dirName), row.names=FALSE)
  }
  
  if (!is.null(plot_info)) {
    ggsave(sprintf('%s/figures/performanceThresholds.png', dirName), 
           plot=plot_info$plotOverall, width=plotDims[1], height=plotDims[2], units="in", dpi=DPI)
    
    ggsave(sprintf('%s/figures/performanceGroups.png', dirName), 
           plot=plot_info$plotGroups, width=plotDims[1], height=plotDims[2], units="in", dpi=DPI)
  }

  if (!is.null(model)) {
    saveRDS(model, sprintf('%s/classifier.rds', dirName))
  }
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


plotDecisionTree <- function(m, tree_num=1, nodeSize=4, nodeText=2, labelText=2, show_plot=FALSE, background='white',
                             export=FALSE, savefolder=NULL, plot_dims=c(8,6), plot_DPI=600) {
  
  # Extract a single tree (e.g., the first tree)
  tree_info <- getTree(m, k = tree_num, labelVar = TRUE)
  
  # Initialize data frames for nodes and edges
  nodes_df <- data.frame(
    id = 1:nrow(tree_info),
    label = character(nrow(tree_info)),
    is_terminal = tree_info$status == -1,
    prediction = tree_info$prediction
  )
  
  edges_df <- data.frame(
    from = integer(),
    to = integer(),
    label = character()
  )
  
  # Populate nodes and edges
  for (i in 1:nrow(tree_info)) {
    node <- tree_info[i, ]
    
    # Node label based on whether it's a terminal node or a split node
    if (node$status == -1) { # Terminal node
      nodes_df$label[i] <- paste0("Pred: ", node$prediction)
    } else { # Split node
      nodes_df$label[i] <- paste0(node$`split var`, " <= ", round(node$`split point`, 2))
      
      # Add edges
      if (node$`left daughter` > 0) {
        edges_df <- rbind(edges_df, data.frame(
          from = i,
          to = node$`left daughter`,
          label = "True" # Or 'Yes', '<= SplitPoint'
        ))
      }
      if (node$`right daughter` > 0) {
        edges_df <- rbind(edges_df, data.frame(
          from = i,
          to = node$`right daughter`,
          label = "False" # Or 'No', '> SplitPoint'
        ))
      }
    }
  }
  
  # Remove prediction for non-terminal nodes (they don't have a final prediction)
  nodes_df$prediction[!nodes_df$is_terminal] <- NA
  
  
  # Create an igraph object
  tree_graph <- graph_from_data_frame(d = edges_df, vertices = nodes_df, directed = TRUE)
  
  
  plot_tree <- ggraph(tree_graph, layout = 'tree') +
    geom_edge_link(aes(label = label),
                   arrow = arrow(length = unit(3, 'mm'), type = "closed"),
                   end_cap = circle(2, 'mm'),
                   start_cap = circle(2, 'mm'),
                   color = "gray30",
                   label_colour = "darkblue",
                   label_size = labelText) +
    geom_node_point(aes(color = is_terminal), size = nodeSize) + 
    geom_node_text(aes(label = label), repel = TRUE, size = nodeText, bg.colour = "white", bg.r = 0.1) +
    scale_color_manual(values = c("FALSE" = "skyblue", "TRUE" = "darkgreen")) +
    theme_void() +
    labs(title = paste("Decision Tree", tree_num, "from Random Forest Model")) +
    theme(legend.position = "none") # Hide legend for simplicity
  
  if (!is.null(background)) {
    plot_tree <- plot_tree + theme(
      panel.background = element_rect(fill = background),   # White background for the plotting panel
      plot.background = element_rect(fill = background)    # White background for the entire plot area (including margins, titles, legend)
    )
  } else {
    plot_tree <- plot_tree + theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
  }
  
  
  if (show_plot == TRUE) {
    print(plot_tree)
  }
  
  
  if (export==TRUE) {
    if (is.null(savefolder)) {
      dirName <- makeDirSysDT(create=FALSE)
      if (!dir.exists(dirName)) { # Changed from !dirName %in% dir() to dir.exists() for robustness
        dirName <- makeDirSysDT(create=TRUE)
      }
    } else {
      dirName <- savefolder
      if (!dir.exists(dirName)) { # Ensure the specified folder exists
        dir.create(dirName, recursive = TRUE)
      }
    }
    
    figures_path <- sprintf('%s/figures', dirName)
    if (!dir.exists(figures_path)) { # Changed from !'figures' %in% dir(dirName) to dir.exists()
      dir.create(figures_path)
    }
    
    ggsave(sprintf('%s/treePlot%s.png', figures_path, tree_num), 
           plot=plot_tree, width=plot_dims[1], height=plot_dims[2], units="cm", dpi=plot_DPI)
    
    
    cat(sprintf('Saved plot: %s', sprintf('%s/treePlot%s.png\n', figures_path, tree_num)))
    return(list(p = plot_tree, savepath=figures_path))
    
    
  } else {
    return(list(p = plot_tree, savepath=NULL))
  }
  
  
}

cat('\u2713 Done loading functions')

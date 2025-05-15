library(data.table)
library(randomForest)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

library(data.table)
library(randomForest)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)


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

loadDataFromHier <- function(root_path, hierarchy_levels, startVar, endVar, omitVars=c(), filterVarsMin=c(), filterVarsMax=c(), file_pattern='RoccaContourStats', file_type='csv', show_output=TRUE) {
  all_data <- list()
  
  # Recursive function to traverse levels
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
          ind1 <- which(names(current_data) == startVar)
          ind2 <- which(names(current_data) == endVar)
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

classifyData <- function(data, targetVar, groupVar, vars, select_groups=c(), omit=c(), prune=0, minScore=0, nMax=1000, nTrees=500, mtry=NULL, verbose=TRUE) {
  results <- data.frame()
  allpreds <- data.frame()
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
                        strata = as.factor(xtrain[[targetVar]]),
                        sampsize = sampsizes,
                        na.action = na.roughfix)
      
      predFrame <- predict(m, xtest, type='prob')
      predFrame <- predFrame[apply(predFrame, 1, getScore) >= minScore,]

      if (!is.null(dim(predFrame))) {
        probs <- colMeans(predFrame)
        predFrame <- data.frame(predFrame)
        predList <- unlist(apply(predFrame, 1, getPred))
        scoreList <- unlist(apply(predFrame, 1, getScore))
        predFrame$pred <- predList
        predFrame$score <- scoreList
        predFrame[[targetVar]] <- unique(xtest[[targetVar]])[1]
        predFrame[[groupVar]] <- group
        print(names(predFrame))
        print(names(allpreds))
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
                    na.action = na.roughfix)
  
  
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

plotResults <- function(results, targetVar, thrMax=0.10, point_size=1, show_plots=TRUE, 
                        axistitle_fontsize=14, legend_fontsize=12, ticklabel_fontsize=10) {
  thrList <- seq(0, thrMax, 0.01)
  RetAcc <- data.frame()
  for (thr in thrList) {
    sub <- subset(results, score >= thr)
    group_accs <- c()
    for (group in unique(sub[[targetVar]])) {
      subsub <- subset(sub, sub[[targetVar]] == group)
      group_accs <- append(group_accs, sum(subsub$pred == subsub[[targetVar]])/nrow(subsub))
    }
    row <- data.frame(label='ALL', minScore=thr, retention=(1-(nrow(sub)/nrow(results))), ovr_acc=sum(sub$pred==sub[[targetVar]])/nrow(sub), mean_acc=mean(group_accs))
    RetAcc <- rbind(RetAcc, row)
    
    for (lab in unique(sub[[targetVar]])) {
      subsub <- subset(sub, sub[[targetVar]]==lab)
      labAll <- subset(results, results[[targetVar]]==lab)
      row <- data.frame(label=lab, minScore=thr, retention=(1-(nrow(subsub)/nrow(labAll))), ovr_acc=sum(subsub$pred==subsub[[targetVar]])/nrow(subsub), mean_acc=sum(subsub$pred==subsub[[targetVar]])/nrow(subsub))
      RetAcc <- rbind(RetAcc, row)
    }
  }
  
  label_colors <- c("ALL" = "black")
  other_labels <- setdiff(unique(RetAcc$label), "ALL")
  num_other_labels <- length(other_labels)
  if (num_other_labels > 0) {
    other_colors <- c()
    for (i in 1:num_other_labels) {
      other_colors <- append(other_colors, rgb(runif(1,0,1), runif(1,0,1), runif(1,0,1), 1))
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
  
  p1 <- ggplot(data = subALL) +
    theme_bw() +
    geom_line(aes(x = minScore, y = ovr_acc, colour = 'Overall accuracy')) +
    geom_point(aes(x = minScore, y = ovr_acc, colour = 'Overall accuracy')) +
    geom_line(aes(x = minScore, y = mean_acc, colour = 'Mean accuracy')) +
    geom_point(aes(x = minScore, y = mean_acc, colour = 'Mean accuracy')) +
    geom_line(aes(x = minScore, y = (1-retention), colour = '% groups classified')) +
    geom_point(aes(x = minScore, y = (1-retention), colour = '% groups classified')) +
    scale_colour_manual(
      name = NULL,
      values = c('Overall accuracy' = 'darkgreen',
                 'Mean accuracy' = 'darkcyan',
                 '% groups classified' = 'chocolate')
    ) +
    xlab('Minimum prediction score') +
    ylab(NULL) + 
    ylim(c(0,1)) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 10), size=axistitle_fontsize),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10), size=axistitle_fontsize),
      axis.text.x = element_text(size = ticklabel_fontsize),
      axis.text.y = element_text(size = ticklabel_fontsize),
      legend.position = "top",
      legend.box = "horizontal",
      legend.justification = "left",
      legend.text = element_text(size = legend_fontsize)
    )
  
  p2 <- ggplot(data = RetAcc, aes(x = retention, y = ovr_acc, group = label, colour = label, size = label, alpha = label)) +
    theme_bw() +
    geom_point(pch=16) +
    scale_color_manual(values = label_colors) +
    scale_size_manual(values = label_sizes) +
    scale_alpha_manual(values = label_alphas) +
    xlim(0,1) +
    ylim(0,1) +
    xlab('% classifications discarded') +
    ylab('% of groups classified correctly') +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_text(face = "bold", margin = margin(t = 10), size=axistitle_fontsize),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10), size=axistitle_fontsize),
      axis.text.x = element_text(size = ticklabel_fontsize),
      axis.text.y = element_text(size = ticklabel_fontsize),
      legend.text = element_text(size = legend_fontsize)
    ) +
    guides(colour = guide_legend(nrow = 1)) # Forces labels to be in a single row
  
  info <- list(plotOverall=p1, plotGroups=p2, df=RetAcc)
  if (show_plots == TRUE) {
    print(p2)
    print(p1)
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
    
    nList[[sprintf('%s_n', label)]] <- sub$n[1]
    ind <- which(names(sub) == 'score')+1
    subPreds <- sub[,ind:ncol(sub)]
    colnames(subPreds) <- paste0(sprintf("%s_", label), colnames(subPreds))
    row <- cbind(row, subPreds)
  }
  
  nList <- as.data.frame(nList)
  ind <- which(names(row)==groupVar)
  row <- cbind(row[,1:ind], nList, row[,(ind+1):ncol(row)])
  row[[targetVar]] <- targetLabel
  rownames(row) <- c(1)
  
  return(row)
}

dataPlot <- function(d, variables=list('x'=NULL, 'y'=NULL, 'group'=NULL), alpha=0.7, size=1, 
                     export=FALSE, plotDPI=400, plotDims=c(8,6), resultsFolder=NULL) {
  
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
  
  if (num_var == 1) {
    p <- p + geom_density(aes(x=.data[[xVar]], colour=.data[[gVar]]))
  } else if (num_var == 2) {
    p <- p + geom_point(aes(x=.data[[xVar]], y=.data[[yVar]], colour=.data[[gVar]]), alpha=alpha, size=size,)
  }
  
  # Add labels and title
  if (!is.null(xVar)) p <- p + xlab(xVar)
  if (!is.null(yVar)) p <- p + ylab(yVar)
  
  # Conditionally handle the legend
  if (!is.null(gVar)) {
    p <- p + labs(colour = gVar) +
      theme(legend.position = "top",
            legend.title = element_blank()) # Remove legend title and position at the top
  } else {
    p <- p + theme(legend.position = "none") # Remove legend if gVar is NULL
  }
  
  if (export==TRUE) {
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
    num <- 1
    for (item in dir(sprintf('%s/figures', dirName))) {
      if (grepl('dataPlot', item) & grepl('.png', item)) {
        num <- num + 1
      }
    }
  
    ggsave(sprintf('%s/figures/dataPlot%s.png', dirName, num), 
           plot=p, width=plotDims[1], height=plotDims[2], units="in", dpi=plotDPI)
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


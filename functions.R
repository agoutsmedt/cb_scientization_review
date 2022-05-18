tf_idf <- function(graph = NULL, nodes = NULL, title_column = "Titre", com_column = "Com_ID", color_column = "color",
                   com_name_column = "Community_name", com_size_column = "Size_com", threshold_com = 0.01, number_of_words = 12,
                   palette = NULL, size_title_wrap = 8, lemmatize_bigrams = TRUE, min_count = 10) {
  #' Creating a TF-IDF analysis of the titles of WoS corpus
  #'
  #' This function takes as input a tidygraph object or a data frame with nodes, both with a community attribute, and analyzes
  #' the words use in the title of the articles to calculate the words with the highest TF-IDF
  #' value for each community.
  #'
  #' @param graph
  #' A tidygraph object. By default `NULL` in case you prefer to enter a data frame with nodes.
  #'
  #' @param nodes
  #' A data frame with the nodes of the network, and community and title attributes.
  #'
  #' @param title_column
  #' The name of the column with the titles of the articles. The function renames the column
  #' "Titre", as in the OST WoS database ("Titre" is the default value).
  #'
  #' @param com_column
  #' The name of the column with the id of the communities. The function renames the column
  #' "Com_ID" (default value).
  #'
  #' @param color_column
  #' The name of the column with the color attribute of the communities. The function renames the column
  #' "color" (default value).
  #'
  #' @param com_name_column
  #' The name of the column with the name of the communities.
  #'
  #' @param com_size_column
  #' The name of the column with the share of total nodes in each community.
  #'
  #' @param threshold_com
  #' The minimun percentage of nodes in the community for the community to be displayed on the plot.
  #'
  #' @param number_of_words
  #' How many words you want to display on the final graph.
  #'
  #' @param palette
  #' If you don't already have a color attribute for your communities in your tidygraph object,
  #' the function will generate one from a palette that you can add in the paramaters (NULL by default).
  #'
  #' @param size_title_wrap
  #' The size of the community title in the plot.
  #' 
  #' @param lemmatize_bigrams
  #' Chose whether you want to lemmatize each word in a bigram (which could lead to a bigram that has no 
  #' clear meaning) or not.
  #' 
  #' @param min_count
  #' Number of time an ngram should appeared in the corpus to be integrated in the
  #' tf-idf calculation.
  
  # extracting the nodes
  if (!is.null(graph)) {
    tf_idf_save <- graph %>%
      activate(nodes) %>%
      as.data.table()
  }
  else {
    tf_idf_save <- nodes %>% as.data.table()
  }
  
  # changing the names of the column for titles and communities
  colnames(tf_idf_save)[colnames(tf_idf_save) == com_column] <- "Com_ID"
  colnames(tf_idf_save)[colnames(tf_idf_save) == title_column] <- "Titre"
  colnames(tf_idf_save)[colnames(tf_idf_save) == com_name_column] <- "Community_name"
  colnames(tf_idf_save)[colnames(tf_idf_save) == com_size_column] <- "Size_com"
  
  
  # adding a color column attribute in case it doesn't exist
  if (!any(names(tf_idf_save) == color_column)) {
    color <- data.table(
      Com_ID = 1:500,
      color = palette
    )
    color <- color %>%
      mutate(Com_ID = sprintf("%02d", Com_ID)) %>%
      mutate(Com_ID = as.character(Com_ID))
    
    tf_idf <- merge(tf_idf_save, color, by = "Com_ID", all.x = TRUE)
  } else {
    colnames(tf_idf_save)[colnames(tf_idf_save) == color_column] <- "color"
  }
  
  tf_idf <- tf_idf_save # we will need tf_idf_save later
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Unigram ####
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Cleaning the titles
  
  tf_idf <- tf_idf[Titre != "NULL"]
  tf_idf[, Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[, Titre := gsub("-", " ", Titre)] # to avoid having non-existing words like UnitedStates
  tf_idf[, Titre := stripWhitespace(Titre)]
  tf_idf[, Titre := removePunctuation(Titre)]
  tf_idf[, Titre := removeNumbers(Titre)]
  tf_idf[, Titre := tolower(Titre)]
  tf_idf[, Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[, Titre := as.character(Titre)]
  tf_idf$Titre <- quanteda::tokens(tf_idf$Titre)
  tf_idf$Titre <- quanteda::tokens_ngrams(tf_idf$Titre, n = 1)
  tible_tf_idf <- tf_idf[, paste(Titre, collapse = " "), by = "Com_ID"]
  tible_tf_idf[, V1 := stripWhitespace(V1)]
  # Dictionnary to find the root of stem word before stemming
  tf_idf_table <- tible_tf_idf
  tf_idf_table <- tf_idf_table %>% unnest_tokens(word, V1) %>% as.data.table()
  tf_idf_table <- tf_idf_table[, word := textstem::lemmatize_words(word)] 
  tf_idf_table <- tf_idf_table[, count := .N, by = c("Com_ID","word")] %>% 
    filter(count >= min_count) %>% 
    unique()
  # applying tf-idf
  tf_idf_table <- tidytext::bind_tf_idf(tf_idf_table, word, Com_ID, count)
  tf_idf_table_uni <- tf_idf_table
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Bigram ####
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # cleaning
  
  tf_idf <- tf_idf_save[Titre != "NULL"]
  tf_idf[, Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[, Titre := gsub("-", " ", Titre)] # to avoid having non-existing words like UnitedStates
  tf_idf[, Titre := stripWhitespace(Titre)]
  tf_idf[, Titre := removePunctuation(Titre)]
  tf_idf[, Titre := removeNumbers(Titre)]
  tf_idf[, Titre := tolower(Titre)]
  tf_idf[, Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[, Titre := as.character(Titre)]
  # ngraming
  # tf_idf[, Titre := stemDocument(Titre)]
  tf_idf$Titre <- tokens(tf_idf$Titre)
  tf_idf$Titre <- tokens_ngrams(tf_idf$Titre, n = 2)
  tible_tf_idf <- tf_idf[, paste(Titre, collapse = " "), by = "Com_ID"]
  tible_tf_idf[, V1 := stripWhitespace(V1)]
  
  tf_idf_table <- tible_tf_idf
  tf_idf_table <- tf_idf_table %>% unnest_tokens(word, V1) %>% as.data.table()
  tf_idf_table$word <- gsub("_", " ", tf_idf_table$word)
  
  if(lemmatize_bigrams == TRUE){
    tf_idf_table[, word1 := str_extract(tf_idf_table$word, "\\S+")]
    tf_idf_table[, word2 := str_extract(tf_idf_table$word, "\\S+$")]
    tf_idf_table <- tf_idf_table[, word1 := textstem::lemmatize_words(word1)]
    tf_idf_table <- tf_idf_table[, word2 := textstem::lemmatize_words(word2)] 
    tf_idf_table <- tf_idf_table %>%
      tidyr::unite(word, word1, word2, sep = " ") %>% 
      as.data.table()
  }
  
  tf_idf_table <- tf_idf_table[, count := .N, by = c("Com_ID","word")] %>% 
    filter(count >= min_count) %>% 
    unique()
  
  # applying tf-idf
  tf_idf_table <- tidytext::bind_tf_idf(tf_idf_table, word, Com_ID, count)
  tf_idf_table_bi <- tf_idf_table
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Plot ####
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  tf_idf_table <- rbind(tf_idf_table_uni, tf_idf_table_bi, fill = TRUE)
  tf_idf_table <- tf_idf_table[order(-tf_idf)][, head(.SD, number_of_words), Com_ID]
  
  # Get info about size of communities
  Size_com <- unique(tf_idf_save[, .(Com_ID, Community_name, Size_com, color)])
  tf_idf_table <- merge(tf_idf_table, Size_com, by = "Com_ID", all.x = TRUE) # merge
  
  # Wrap Name and Reorder according to share_leiden
  tf_idf_table$Com_wrap <- str_wrap(tf_idf_table$Community_name, width = 10)
  tf_idf_table <- tf_idf_table[order(-Size_com)] # order by variable
  tf_idf_table <- tf_idf_table[order(-Size_com)] # order by variable
  tf_idf_table$Com_wrap <- factor(tf_idf_table$Com_wrap) # make a factor
  tf_idf_table$Com_wrap <- fct_inorder(tf_idf_table$Com_wrap) # by order of appearance
  
  # fixing the number of columns, depending of the number of communities
  n_columns <- 3
  if (length(unique(tf_idf_table[Size_com >= threshold_com]$Com_wrap)) > 9) {
    n_columns <- 4
  }
  
  if (length(unique(tf_idf_table[Size_com >= threshold_com]$Com_wrap)) > 12) {
    n_columns <- 5
  }
  
  if (length(unique(tf_idf_table[Size_com >= threshold_com]$Com_wrap)) > 15) {
    n_columns <- 6
  }
  
  # plotting the graph
  tf_idf_plot <- ggplot(tf_idf_table[Size_com >= threshold_com], aes(reorder_within(word, tf_idf, color), tf_idf, fill = color)) +
    geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
    labs(
      title = "Highest tf-idf",
      x = "Words", y = "tf-idf"
    ) +
    facet_wrap(~Com_wrap, ncol = n_columns, scales = "free") +
    scale_x_reordered() +
    scale_fill_identity() +
    theme(strip.text = element_text(size = size_title_wrap)) +
    coord_flip()
  
  list_return <- list("plot" = tf_idf_plot, "list_words" = tf_idf_table)
  return(list_return)
}


library(shiny)
library(RSQLite)
library(data.table)
library(stringi)
library(dplyr)
library(bslib)

database <- 'ngrams.db'
if(file.exists(database)){
  con <- dbConnect(SQLite(), dbname = database)
} else {
  stop('Database not found')
}

tetragrams <- data.table(dbGetQuery(con, "SELECT * FROM tetragrams;"))
trigrams <- data.table(dbGetQuery(con, "SELECT * FROM trigrams;"))
bigrams <- data.table(dbGetQuery(con, "SELECT * FROM bigrams;"))
unigrams <- data.table(dbGetQuery(con, "SELECT * FROM unigrams;"))
words <- data.table(dbGetQuery(con, "SELECT * FROM words;"))

setkey(tetragrams, w3, w2, w1)
setkey(trigrams, w2, w1)
setkey(bigrams, w1)

process_line <- function(string) {
  string <- tolower(string)
  string <- gsub('[^a-zA-Z0-9. \\-\'!\\?]+', '', string, perl=TRUE)
  string <- paste('##START## ', string)
  string <- gsub('[0-9]+', ' ##NUMBER##', string)
  string <- gsub('[\\.|!|\\?]+ ', ' ##END## ##START## ', string)
  string <- gsub('\\.+', ' ', string)
  split_string <- unlist(strsplit(string, '[[:space:]]+'))
  if (grepl(' $', string)) {
    split_string[[length(split_string)]] <- paste(tail(split_string, 1), ' ', sep='')
  }
  tail(split_string, 4)
}

get_next_words <- function(processed_line, n, valid_ints = NULL) {
  last_word <- tail(processed_line, 1)
  if(length(last_word) == 0){
    return(unigrams[,][order(-prob)][1:3,])
  }
  if(stri_endswith_fixed(last_word, ' ')) {
    if(nrow(words[word == stri_trim_right(last_word), ]) == 0) {
      return(unigrams[,][order(-prob)][1:3,])
    }
    ints <- lapply(c(head(processed_line, -1), stri_trim(last_word)), function(x) words[word == x,int])
    ints <- tail(ints, n-1)
    while(any(sapply(ints, function(x) length(x) == 0))){
      ints <- tail(ints, -1)
    }
    n <- length(ints) + 1
    next_words <- switch(n,
                         unigrams[,][order(-prob)][1:3,],
                         bigrams[ints, w0, prob][order(-prob)][1:3,],
                         trigrams[ints, w0, prob][order(-prob)][1:3,],
                         tetragrams[ints, w0, prob][order(-prob)][1:3,])
  } else {
    if(is.null(valid_ints)) {
      valid_words <- words[stri_startswith_fixed(word, last_word), ]
      valid_ints <- valid_words[valid_words$int %in% unigrams$w0]$int
      if(length(valid_ints) < 3) {
        next_words <- unigrams[w0 %in% valid_ints]
        return(next_words)
      }
    }
    ints <- sapply(c(head(processed_line, -1)), function(x) words[word == x,int], USE.NAMES = FALSE)
    next_words <- switch(n, 
                         unigrams[w0 %in% valid_ints, ][order(-prob)][1:3,],
                         bigrams[w1 == ints[[1]] & w0 %in% valid_ints, w0, prob][order(-prob)][1:3,],
                         trigrams[w2 == ints[[1]] & w1 == ints[[2]] & w0 %in% valid_ints, w0, prob][order(-prob)][1:3,],
                         tetragrams[w3 == ints[[1]] & w2 == ints[[2]] & w1 == ints[[3]] & w0 %in% valid_ints, w0, prob][order(-prob)][1:3,])    
  }
  if((!all(complete.cases(next_words)) || (nrow(next_words) < 3)) && (n != 1)) {
    backoff <- get_next_words(tail(processed_line, -1), n-1, valid_ints)
    next_words <- join(next_words, anti_join(backoff, next_words, by="w0"), type="full", by="w0")
  }
  next_words
}

shinyServer(function(input, output, session) {
  next_words <- reactiveVal(list('I', 'the', 'it'))
  prediction_history <- reactiveVal(data.frame(Prediction = character(), Time = character()))
  text_history <- reactiveVal(character())
  highlighted_word <- reactiveVal(NULL)
  show_correction_btn <- reactiveVal(FALSE)
  correction_word <- reactiveVal("")
  
  theme_mapping <- list(
    "Cosmo" = bs_theme(version = 4, bootswatch = "cosmo"),
    "Flatly" = bs_theme(version = 4, bootswatch = "flatly"),
    "Darkly" = bs_theme(version = 4, bootswatch = "darkly"),
    "Sandstone" = bs_theme(version = 4, bootswatch = "sandstone")
  )
  
  observeEvent(input$theme, {
    selected_theme <- theme_mapping[[input$theme]]
    session$setCurrentTheme(selected_theme)
  })
  
  observeEvent(input$clearBtn, {
    updateTextInput(session, "sentence", value = "")
    text_history(character())
    highlighted_word(NULL)
    show_correction_btn(FALSE)
  })
  
  observeEvent(input$undoBtn, {
    if(length(text_history()) > 0) {
      updateTextInput(session, "sentence", value = text_history()[length(text_history())])
      text_history(text_history()[-length(text_history())])
      highlighted_word(NULL)
      show_correction_btn(FALSE)
    }
  })
  
  observeEvent(input$sentence, {
    current_text <- isolate(input$sentence)
    if(length(text_history()) == 0 || tail(text_history(), 1) != current_text) {
      text_history(c(text_history(), current_text))
    }
    
    sentence_words <- unlist(strsplit(current_text, "\\s+"))
    last_word <- tail(sentence_words, 1)
    
    if(!is.null(last_word) && length(last_word) == 1 && !is.na(last_word) && nchar(last_word) > 0 && !(last_word %in% words$word)) {
      highlighted_word(last_word)
      
      possible_corrections <- words[startsWith(word, substr(last_word, 1, 1))]
      
      if(nrow(possible_corrections) > 0){
        correction_word(possible_corrections$word[1])
        show_correction_btn(TRUE)
      } else {
        show_correction_btn(FALSE)
      }
    } else {
      show_correction_btn(FALSE)
      highlighted_word(NULL)
    }
  }, ignoreInit = TRUE)
  
  observe({
    invalidateLater(input$predictionSpeed, session)
    isolate({
      sentence <- process_line(input$sentence)
      n <- ifelse(stri_endswith_fixed(tail(sentence, 1), ' '), min(4, length(sentence) + 1), min(4, length(sentence)))
      next_list <- arrange(get_next_words(sentence, n), -prob)[1:3, ]
      next_list <- unlist(sapply(next_list$w0, function(x) words[int == x, word], USE.NAMES = FALSE))
      while(length(next_list) < 3){
        next_list <- c(next_list, " ")
      }
      next_list <- gsub("^i(\'m|\'ve|\'ll|\'ma)?$", 'I\\1', next_list)
      next_words(next_list)
      
      if(length(next_list) > 0 && next_list[1] != " ") {
        new_entry <- data.frame(
          Prediction = next_list[1],
          Time = format(Sys.time(), "%H:%M:%S")
        )
        prediction_history(rbind(new_entry, prediction_history()))
      }
    })
  })
  
  output$wordButtons <- renderUI({
    if(input$threeOutputs) {
      fluidRow(
        column(4, actionButton("word2", label = next_words()[[2]], class = "btn-primary word-btn", width = "100%")),
        column(4, actionButton("word1", label = next_words()[[1]], class = "btn-success word-btn", width = "100%")),
        column(4, actionButton("word3", label = next_words()[[3]], class = "btn-primary word-btn", width = "100%"))
      )
    } else {
      fluidRow(
        column(12, actionButton("word1", label = next_words()[[1]], class = "btn-success word-btn", width = "100%"))
      )
    }
  })
  
  observeEvent(input$word1, {
    updateTextInput(session, "sentence", value = paste0(input$sentence, " ", next_words()[[1]], " "))
  })
  observeEvent(input$word2, {
    updateTextInput(session, "sentence", value = paste0(input$sentence, " ", next_words()[[2]], " "))
  })
  observeEvent(input$word3, {
    updateTextInput(session, "sentence", value = paste0(input$sentence, " ", next_words()[[3]], " "))
  })
  
  output$predictionHistory <- renderPrint({
    if(nrow(prediction_history()) > 0) {
      head(prediction_history(), 10)
    } else {
      "No predictions yet"
    }
  })
  
  output$correctionBtnUI <- renderUI({
    if(show_correction_btn()) {
      actionButton("correctWord", paste("Correct to:", correction_word()))
    }
  })
  
  observeEvent(input$correctWord, {
    req(highlighted_word())
    current_text <- input$sentence
    pattern <- paste0("\\b", highlighted_word(), "\\b")
    corrected_text <- gsub(pattern, correction_word(), current_text)
    updateTextInput(session, "sentence", value = corrected_text)
    highlighted_word(NULL)
    show_correction_btn(FALSE)
  })
})

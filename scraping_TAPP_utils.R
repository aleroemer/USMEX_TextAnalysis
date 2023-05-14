library(tidyverse)
library(rvest)
library(stringr)
library(purrr)
library(lubridate)
library(readr)
library(xml2)

retrieve_number_of_pages <- function(html_object, num_elements_per_page){
  
  # Returns the number of pages that a given URL from The American Presidency 
  #   project has.
  
  #     Arguments:
  #       html_object (html): object produced by using the <read_html> function
  #                               with a given URL from The American Presidency
  #     
  #     Returns: numeric object with the number of pages in the given URL
  
  html_elements = html_elements(html_object, css = "h3")
  
  # If there's less than 3 elements, the page is empty, so we return NA
  if (length(html_elements)<3){
    return(NA)
  }
  
  raw_string = html_elements[3][1] %>% 
    html_text2()
  raw_records_found = strsplit(raw_string, str_c(as.character(num_elements_per_page), ' of '))[[1]][2]
  total_records = as.integer(strsplit(raw_records_found, ' records found')[[1]])
  
  if (is.na(total_records)){
    total_records = 1
  }
  
  return(ceiling(total_records/num_elements_per_page))
}


retrieve_urls <- function(html_object){
  
  # Returns the URLs of the documents displayed in the page of the HTML
  
  #     Arguments:
  #       html_object (html): object produced by using the <read_html> function
  #                               with a given URL from The American Presidency
  #     
  #     Returns: numeric object with the number of pages in the given URL
  
  page_urls = html_elements(html_object, css = ".views-field-title a") %>% 
    html_attr('href')
  
  return(page_urls)
}


scrape_doc <- function(html_object, categories_matrix){
  
  # Takes an html object from an article in The American Presidency Project
  #   and stores its elements in a list
  
  #     Arguments:
  #       html_object (html): object produced by using the <read_html> function
  #                               with a given URL from The American Presidency
  #     
  #     Returns: a list with the name of the speaker, date, title and text
  
  speaker <- html_nodes(html_object, ".diet-title a") %>% 
    html_text() # Speaker
  date <- html_nodes(html_object, ".date-display-single") %>%
    html_text() %>%
    mdy() # Date
  title <- html_nodes(html_object, "h1") %>%
    html_text() # Title
  text <- html_nodes(html_object, "div.field-docs-content") %>%
    html_text() # Article text
  
  #Grab relevant categories and store them in a vector of dummies
  relevant_categories = xml_find_all(html_object, ".//*[@typeof='skos:Concept']") %>%
    html_text2()
  
  #Create a vector of dummies representing each category represented by this article
  cat_vector = c()
  for (row in 1:nrow(categories_matrix)){
    if (categories_matrix[row,2][[1]] %in% relevant_categories){
      cat_vector = c(cat_vector, 1)
    } else {
      cat_vector = c(cat_vector, 0)
    }
  }
  
  return(list(article_data = c(speaker, date, title, text),
              categories = cat_vector))
}  


scrape_all_docs_from_URL <- function(url){
  
  # Takes a search URL from The American Presidency Project and retrieves information
  # from each of the articles found. It stores in a DataFrame the following:
  #      - Speaker/Writer/Sender 
  #      - Date
  #      - Title
  #      - Content
  #      - Category
  
  #     Arguments:
  #       html_object (html): object produced by using the <read_html> function
  #                               with a given URL from The American Presidency
  #     
  #     Returns: a DF with all the information suggested above

  
  BASE_ARTICLE_URL_HEAD = "https://www.presidency.ucsb.edu"
  
  # Relación entre códigos y categorías
  html_element_wcategories = html_elements(read_html(url), css = "select")[2][[1]]
  categories_xml_nodeset <- xml_find_all(html_element_wcategories, ".//option")
  category_code <- lapply(categories_xml_nodeset, xml_attr, "value")
  category_label <- lapply(categories_xml_nodeset, xml_text)
  categories_matrix = cbind(category_code, category_label)[-1,]
  
  # Where we'll store our results
  df_all_articles = data.frame() # Here we'll store all articles
  df_articles_categories = data.frame() # Here we'll store the category dummies
  
  # Get the current URL based on the category code
  current_html = read_html(url)
  current_num_pages = retrieve_number_of_pages(current_html, 
                                               num_elements_per_page = 25)
  
  print(str_c('   Starting to loop over ', current_num_pages, ' pages'))
  
  ### Loop over every page
  for (page in 1:current_num_pages){ 
    current_page_urls = retrieve_urls(current_html)
    
    if (page %% 20 == 0) {
      print(str_c('               Page: ', page))
      print(str_c('                   No. articles scraped: ', nrow(df_all_articles)))
    }
    ### Loop over every article within a page
    for (article_num in 1:length(current_page_urls) ){
      current_article_url = str_c(BASE_ARTICLE_URL_HEAD, current_page_urls[article_num])
      current_article_html = read_html(current_article_url)
      
      list_article = scrape_doc(current_article_html,
                                categories_matrix = categories_matrix)
      
      # We store the article data and the category vector into a df
      df_all_articles = rbind(df_all_articles, list_article$article_data)
      df_articles_categories =rbind(df_articles_categories, list_article$categories)
    }
    
    # update url so that it goes forward one page
    current_url = str_c(url, "&page=", as.character(page))
    current_html = read_html(current_url)
    
  }
  # Give the dfs the right column names
  names(df_articles_categories) = unlist(categories_matrix[,'category_label'])
  names(df_all_articles) = c('speaker', 'date', 'title', 'text')
  
  #Combine together both dfs
  return( as_tibble(cbind(df_all_articles, df_articles_categories)) )
}
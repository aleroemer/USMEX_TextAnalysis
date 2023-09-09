#' Add President's Last Name and Document Year Columns to Data Frame
#'
#' This function adds a last name and year column to the input data frame.
#'
#' @param df The input data frame.
#'
#' @return The data frame with added last name and year columns.
#'
#' @examples
#' data <- read.csv("your_data.csv")
#' df_with_columns <- add_last_name_n_year_to_df(data)
#' # Continue working with the modified data frame
#'
#' @export
add_last_name_n_year_to_df <- function(df){
  
  "Adds a last name and year column to DF"
  
  df <- df %>% 
    mutate(year = year(date),  
           last_name = str_extract(speaker, pattern = "[[:alpha:]]*$"))
  
  df <- df %>% 
    mutate(last_name = case_when(
      last_name == "Bush" & year > 1994 ~ "W. Bush",
      last_name == "Roosevelt" & year > 1930 ~ "FDR",
      TRUE ~ last_name
    )) 
  
  return(df)
}


#' Clean Party Data Frame
#'
#' This function cleans and modifies a party data frame by extracting the last 
#'    name from the President column, selecting specific columns, renaming 
#'    columns, and making additional adjustments.
#'
#' @param party_df The input party data frame.
#'
#' @return The cleaned and modified party data frame.
#'
#' @examples
#' party_data <- read.csv("party_data.csv")
#' cleaned_data <- clean_party_df(party_data)
#' # Continue working with the cleaned and modified data frame
#'
#' @export
clean_party_df <- function(party_df){

  party_df <- party_df %>% 
    mutate(last_name = str_extract(President, pattern = "[[:alpha:]]*$")) %>% 
    select(President, last_name, Party, `Took office`, `Left office`) %>% 
    rename(took_office = `Took office`, left_office = `Left office`, )
  
  #two roosevelts, two bushs, two adams, two Harrisons, two Johnsons
  #... Cleveland president in two nonconsecutive terms
  
  party_df[6, 2] <- "JQ Adams" #John Quincy Adams != John Adams
  party_df[9, 2] <- "W.H Harrison"
  party_df[23, 2] <- "B. Harrison"
  party_df[24, 2] <- "Cleveland (second term)" 
  party_df[36, 2] <- "LB Johnson" 
  party_df[43, 2] <- "W. Bush"
  party_df[32,2] <- "FDR"
  
  
  missing_pres <- data.frame("President" = c("Donald Trump", "Joe Biden"),
                             "last_name" = c("Trump", "Biden"),
                             "Party" = c("Republican", "Democratic"), 
                             "took_office" = c("20/01/2017", "20/01/2021"),
                             "left_office" = c("20/01/2021", "28/04/2023"))
  
  party_df[44,5] <- "20/01/2017" #add obama's exit
  
  party_df <- party_df %>% 
    rbind(missing_pres) %>% 
    mutate(took_office = dmy(took_office),
           left_office = dmy(left_office),
           duration =  difftime(left_office,took_office, units="days"))
  
  party_df <- party_df %>% 
    mutate(duration = as.numeric(duration))
  
  return(party_df)
}


#' Recategorize Parties in Data Frame
#'
#' This function recategorizes parties in the input data frame by grouping them 
#'  into "Democratic" and "Republican" categories. Any other party names will 
#'  remain unchanged.
#'
#' @param df The input data frame.
#'
#' @return The data frame with recategorized parties.
#'
#' @examples
#' data <- read.csv("your_data.csv")
#' df_with_recategorized_parties <- recategorize_parties(data)
#' # Continue working with the modified data frame
#'
#' @export
recategorize_parties <- function(df){

  df<- df %>% 
    mutate(Party =  if_else(str_detect(Party,"Democratic"), "Democratic",
                            if_else(str_detect(Party, "Republican"), "Republican", Party)))

  return(df)
}


#' Creates a unique ID for country texts
#'
#' This function creates an additional column in a country tibble to create
#'  and ID for each text.
#'    
#' @param country A string with the name of a country in the list trade_partners_lst.
#'
#' @return The resulting tibble with a new column
#'
#' @examples
#' create_unique_id('MEXICO')
create_unique_id <- function(country){
  
  return(trade_partners_lst[[country]] %>% 
           mutate(text_ID = row_number()))
}


#' Separates texts into paragraphs
#'
#' This function separates texts in a tibble into paragraphs in another tibble.
#'    It also removes paragraphs where the desired country string (e.g. 'MEXICO')
#'    is not included within the paragraph, transforms the text into upper case,
#'    removes empty paragraphs and adds a unique paragraph ID. 
#'    
#' @param country A string with the name of a country in the list trade_partners_lst.
#'
#' @return The resulting paragraph tibble.
#'
#' @examples
#' separate_text_into_paragraphs('MEXICO')
#' 
#' @export
separate_text_into_paragraphs <- function(country){
  
  # Generate a new df with each row being a paragraph that contains the selected country name
  paragraphs_df <- trade_partners_lst[[country]] %>% 
    #divide based on the newline character + any 
    #potential white space
    mutate(text_paragraph = str_split(toupper(text), "\n+\\s*")) %>% 
    unnest(text_paragraph) %>% 
    filter(text_paragraph != "") %>%
    filter(str_detect(text_paragraph, country)) %>% 
    select(text_ID, text_paragraph, last_name, President, Party, year) # AR: agregué last_name, president, party, year
  
  # Assign each paragraph to a unique ID
  paragraphs_df <- paragraphs_df %>% 
    mutate(paragraph_ID = row_number())

  return(paragraphs_df)
}


#' Add Annotations to a Time Series Plot Related to Mexico
#'
#' This function adds vertical lines and text annotations to a time series plot 
#'  for specific years in Mexican history.
#'
#' @param plot The original plot to which the annotations will be added.
#' @param text_color The color of the annotation text. Default is 'blue'.
#' @param text_y_val The y-axis value at which the annotations will be placed. Default is 25.
#' @param text_size The size of the annotation text. Default is 3.
#' @param plot_subtitle The subtitle of the plot. Default is an empty string.
#' @param xlab The label for the x-axis. Default is an empty string.
#' @param ylab The label for the y-axis. Default is an empty string.
#' @param bottom_note_text The caption or additional note displayed at the bottom of the plot. Default is an empty string.
#' @param plot_title The title of the plot. Default is an empty string.
#'
#' @return The plot with added vertical lines and text annotations.
#'
#' @examples
#' plot <- ggplot(data = your_data) +
#'          # Add your other plot layers
#'
#' plot_with_annotations <- add_annotations_to_mx_time_series_plot(plot)
#' # Continue customizing and displaying the plot
#'
#' @export
add_annotations_to_mx_time_series_plot <- function(plot, 
                                                   text_color = 'blue', 
                                                   text_y_val = 25,
                                                   text_size = 3,
                                                   plot_subtitle = "",
                                                   xlab = "",
                                                   ylab = "",
                                                   bottom_note_text = "",
                                                   plot_title = ""){
  
  plot +
  geom_line() +
  theme_test() +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       y = ylab, 
       x = xlab, 
       caption = bottom_note_text) +
  geom_vline(aes(xintercept = 1847.5), linetype = "dotted", color = text_color)+   
  annotate("text", x=1827, y=text_y_val, label="1848 - Fin de Guerra \nMéxico-Americana ", color = text_color, size = text_size)+
  geom_vline(aes(xintercept = 1864), linetype = "dotted", color = text_color)+   
  annotate("text", x=1872, y=text_y_val, label="1862 - \n                         Maximiliano en México", color = text_color, size = text_size)+
  geom_vline(aes(xintercept = 1923), linetype = "dotted", color = text_color) +   
  annotate("text", x=1937, y=text_y_val, label="1923 - \nSuspensión \nde relaciones \ndiplomáticas", color = text_color, size = text_size) +
  geom_vline(aes(xintercept = 1993), linetype = "dotted", color = text_color) +   
  annotate("text", x=1985, y=text_y_val, label="1993 - \nTLCAN", color = text_color, size = text_size) +
  geom_vline(aes(xintercept = 2017), linetype = "dotted", color = text_color) +   
  annotate("text", x=2010, y=text_y_val, label="2017 - \nTrump", color = text_color, size = text_size)
}


#' Create wordcloud data frame for US presidents
#' 
#' This function creates a dataframe ready for wordclouds for the president provided 
#' note: I dont want to create the wordcloud itself within a function because 
#' every president has its tweaks (i.e. specific words to eliminate)
#' 
#' @param president The last name of the president for whom we want a wordcloud
#' @param exclude_vector Name of the vector of words to exclude

wordcloud_df <- function(president, exclude_vector){
  tokens_df_nosw %>%
    filter(last_name == president) %>%
    count(word_tokens) %>%
    arrange(desc(n)) %>%
    filter(n > 1) %>%
    rename(freq = n) %>% 
    filter(!(word_tokens %in% exclude_vector)) %>% 
    filter(!str_detect(word_tokens, "\\d"))
}


#' Create graphs for comparing threat words across parties
#' 
#' This function creates a dataframe that aggregates the number of threat words
#' per party and weighs them as a porcentage of total tokens. 
#' It also creates the graph. 
#' 
 ##create summary table
threat_party_graphs <- function(year_threshold){
  total_toks_dem <- 205798
  total_toks_rep <- 196421	 
  threat_party <- tokens_df %>% 
    filter(threat_feature == 1, 
           year >= year_threshold) %>%
    mutate(word_tokens = if_else(word_tokens == "problem" | word_tokens == "problems", "problem", word_tokens),
           word_tokens = if_else(word_tokens == "concern" | word_tokens == "concerns", "concern", word_tokens)) %>% 
    group_by(Party, word_tokens) %>% 
    summarise(n = n())%>% 
    ungroup() %>% 
    mutate(n_weighted = if_else(Party == "Democratic", round(n/total_toks_dem*100,3),
                                round(n/total_toks_rep*100,3))) %>% 
    arrange(desc(n_weighted)) %>%
    head(25) 
  
  #create plot
  plot <-   threat_party %>% 
    ggplot(aes(x = fct_reorder(word_tokens,n_weighted), y = n_weighted, fill = Party))+ #should normalize by total party tokens
    geom_col(position = "dodge2") +
    theme_bw()+
    scale_fill_manual(values = wesanderson::wes_palette("Moonrise3", n = 2))+
    theme(axis.text.x = element_text(angle = 45, vjust = .7))+
    labs(title = 'Palabras más comunes del "threat dictionary"',
         subtitle = paste0("A partir del año ", year_threshold),
         x = "", y = "Frecuencia", 
         caption = '*Consular https://www.pnas.org/doi/10.1073/pnas.2113891119')
 return(plot)
}


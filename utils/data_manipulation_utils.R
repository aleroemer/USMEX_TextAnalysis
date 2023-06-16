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


recategorize_parties <- function(df){

  df<- df %>% 
    mutate(Party =  if_else(str_detect(Party,"Democratic"), "Democratic",
                            if_else(str_detect(Party, "Republican"), "Republican", Party)))

  return(df)

}

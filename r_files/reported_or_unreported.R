source("./r_files/scraping_prep.R")


reported_status_of_cases <- data.table("case_id" = character(), 
                                 "reported_status" = character())

case_links <- as.data.table(read_feather("./data/case_links.feather"))
focus_links <- case_links[, case_links]

focus_links[]


for (i in 1:length(focus_links)) {
  
  #### Test getting relevant information
  
  example_decision <- xml2::read_html(paste0("https://tribunalsdecisions.service.gov.uk", focus_links[i]))
  
  reported_status <- example_decision %>%
    html_node("li:nth-child(3) .label+ span") %>%
    html_text()
  
  case_id <- example_decision %>%
    html_node("h1") %>%
    html_text()

  
  id_dt <- data.table("case_id" = case_id, 
                      "reported_status" = reported_status)
  
  reported_status_of_cases <- rbind(reported_status_of_cases, id_dt[, .(case_id, reported_status)], fill = TRUE)

}


write_feather(reported_status_of_cases, "./data/reported_status_of_cases.feather")



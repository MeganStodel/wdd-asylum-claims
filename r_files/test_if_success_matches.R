library(rvest)             # For webscraping
library(data.table)        # For data structure
library(googlesheets)      # To read from google sheets
library(stringr)           # Regex value extraction
library(feather)           # Storage of files
library(textreadr)         # Reading .doc files


#### Data for identifying stuff

key_phrases_doc <- gs_title("Phrases used in asylum cases")

key_phrases <- as.data.table(gs_read(key_phrases_doc, ws = "simplified_phrases"))


#### Cases - successful / not

success_doc <- gs_title("sample_asylum_cases")

success_dt <- as.data.table(gs_read(success_doc, ws = "Sheet4"))

##### Existing text data

first_5000_text <- as.data.table(read_feather("./data/case_text_to_5000.feather"))
last_6000_text <- as.data.table(read_feather("./data/case_text_last_6000.feather"))

some_text <- rbind(first_5000_text, last_6000_text)

success_dt[case_id %in% some_text[, case_id], text_exists := TRUE]

cases_to_test <- success_dt[text_exists == TRUE]

cases_to_loop <- cases_to_test[, case_id]

to_loop <- some_text[case_id %in% cases_to_loop]

text_to_loop <- to_loop[, full_text]
case_id_to_loop <- to_loop[, case_id]

for (i in 1:length(cases_to_loop)) {
  
  cases_to_test[case_id %in% case_id_to_loop[i],
                `:=` (
                  regex_says_unsuccessful = grepl(regex_unsuccessful, 
                                       text_to_loop[i], ignore.case = TRUE),
                  regex_says_successful = grepl(regex_successful, 
                                     text_to_loop[i], ignore.case = TRUE),
                  regex_says_ambiguous = grepl(regex_ambiguous, 
                                    text_to_loop[i], ignore.case = TRUE)
                )
                ]
}

cases_to_test[unsuccessful == 1, unsuccessful := TRUE]
cases_to_test[unsuccessful == 0, unsuccessful := FALSE]
cases_to_test[successful == 1, successful := TRUE]
cases_to_test[successful == 0, successful := FALSE]
cases_to_test[ambiguous_outcome == 1, ambiguous_outcome := TRUE]
cases_to_test[ambiguous_outcome == 0, ambiguous_outcome := FALSE]

cases_to_test <- cases_to_test[, .(case_id, unsuccessful, successful, ambiguous_outcome, withdrawn, 
                                   regex_says_unsuccessful, regex_says_successful, regex_says_ambiguous)]

cases_to_test[, `:=` (
  unsuccessful_match = unsuccessful == regex_says_unsuccessful, 
  successful_match = successful == regex_says_successful, 
  ambiguous_match = ambiguous_outcome == regex_says_ambiguous
)]

# Number uncategorised

cases_to_test[regex_says_unsuccessful == FALSE & regex_says_successful == FALSE & regex_says_ambiguous == FALSE, .N]

success_not_matched <- cases_to_test[unsuccessful_match == FALSE | successful_match == FALSE | ambiguous_match == FALSE]

write_feather(success_not_matched, "./data/success_not_matched.feather")

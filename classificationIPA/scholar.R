library(rvest)
library(xml2)

page <- read_html ("https://scholar.google.com/scholar?as_vis=0&q=virtual+assistants&hl=en&as_sdt=1,33")

all_results <- page %>% 
  html_nodes ("h3 a")
all_results

titles <- page %>% 
  html_nodes ("h3 a") %>% 
  html_text()

article_links <- page %>% 
  html_nodes ("h3 a") %>% 
  html_attr('href')

article_links

article_abstracts_short <- page %>%
  html_nodes('div.gs_rs') %>% 
  html_text()

article_abstracts_short

article_page <- read_html (article_links[1])

df_results = data.frame(titles, article_links, article_abstracts_short)

df_results
# save into a new dataset
write.csv(df_results, '/Users/tracykubert/Google Drive/05 Pratt/07_2019_Fall/INFO-640-01 Data Analysis/rProjects_old/classificationIPA/scholar_test_results.csv')

# Get all Results & Full Abstracts
# https://www.r-bloggers.com/yet-another-post-on-google-scholar-data-analysis/
get_all_results = function(searchTerm) {
  all_results = NULL
  cstart = 0
  notstop = TRUE
  
  while (notstop)
    # new_results = 
}




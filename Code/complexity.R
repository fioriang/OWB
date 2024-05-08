#devtools::install_github("yrvelez/claudeR")
#install.packages(c("httr", "pdftools", "jsonlite"))
library(claudeR)
library(tidyverse)

#needs API KEY to be able to run
api_key <- "xxxxxxxx"
# Load packages
library(httr)
library(pdftools)
library(jsonlite)
library(tm)


pdf_text_ca <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/CA_4.pdf") %>% paste(collapse = " ")
pdf_text_ma <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/MA_3.pdf") %>% paste(collapse = " ")
pdf_text_ct <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/CT.pdf") %>% paste(collapse = " ")
pdf_text_vt <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/VT2.pdf") %>% paste(collapse = " ")
pdf_text_ri <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/RI.pdf") %>% paste(collapse = " ")



#pdf_text_ca <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/From bans and beyond/ca.pdf") %>% paste(collapse = " ")
#pdf_text_ma <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/From bans and beyond/ma.pdf") %>% paste(collapse = " ")
#pdf_text_ct <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/From bans and beyond/ct.pdf") %>% paste(collapse = " ")
#pdf_text_vt <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/From bans and beyond/vt.pdf") %>% paste(collapse = " ")
#pdf_text_ri <- pdf_text("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/From bans and beyond/ri.pdf") %>% paste(collapse = " ")


claude_res_function <- function(i)
{
  Sys.sleep(30)
  summary_prompt <- 
    list(
      list(
        role = "user", 
        content = paste("Read the following texts (texts 1-5) and assign a grade on each of these laws based on the complexity of the regulation, where 10 is the most complex. Focus only on organic waste (Not other materials). Focus on the part about organic waste generators (Not other facilities). The degree of complexity has to do with how clear it is for businesses to implement. Just give me a grade from 1-10 for each of the regulations."
                        ,"Text 1: ", pdf_text_ca, "Text 2: ", pdf_text_ct, "Text 3: ", pdf_text_ma, "Text 4: ", pdf_text_vt, "Text 5: ", pdf_text_ri)
      ))
  
  # Using the claudeR function defined earlier
  result <- claudeR(
    prompt = summary_prompt,
    model = "claude-3-opus-20240229",  # Use the appropriate model version
    max_tokens = 500,
    api_key = api_key, 
    temperature = 1
  )
  
  return(result)
}


claude_res_bans <- lapply(1:20, claude_res_function)
#claude_res_reg <- claude_res_bans

#claude_res_bans <- claude_res_bans %>% unlist %>% c(claude_res_reg)

write.csv(claude_res_bans %>% unlist, "C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/Claude/claude_res.csv")


#### this extracts the words
corpus <- Corpus(VectorSource(pdf_text_ca))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

processed_text <- sapply(corpus, as.character)
words <- unlist(strsplit(processed_text, " ")) %>% str_to_lower()
word_freq_df <- as.data.frame(table(words), stringsAsFactors = FALSE)
colnames(word_freq_df) <- c("Word", "Frequency")

write.csv(word_freq_df, "C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/ca_words_check.csv", row.names=FALSE)


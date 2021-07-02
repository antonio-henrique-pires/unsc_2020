### unsc extract
library(tidyverse)
library(magick)
library(tesseract)
library(lubridate)
library(quanteda)
library(stringi)

eng <- tesseract(language = "eng", options = list(tessedit_pageseg_mode = 1))
readpdf <- image_read_pdf("speeches_pdf/S_2020_1324_E.pdf")
readpdf <- ocr(readpdf, engine = eng)
readpdf <- str_replace_all(paste(readpdf, collapse= " "), "[:blank:]{2,}","") 
readpdf <- str_replace_all(readpdf, "\\\n"," ")

readpdf <- stri_trans_general(readpdf, "Latin-ASCII")

# president speech ----

# vector speech

separate_unsc <- str_split(readpdf,
                           "(?=\\b(Annex[ ]([A-Z]*|[0-9]*)[ ][ ]|Enclosure[ ][ ]))|(?=(([ ][ ]|[A-Z]{1}[.][ ])(Belgium|China|Dominican Republic|Estonia|France|Germany|Indonesia|Niger|Russian[ ]Federation|Saint[ ]Vincent[ ]and[ ]the[ ]Grenadines|South[ ]Africa|Tunisia|Viet[ ]Nam|United[ ]States[ ]of[ ]America|United[ ]Kingdom[ ]of[ ]Great[ ]Britain[ ]and[ ]Northern[ ]Ireland)[ ][ ]))") %>%
  unlist()

df <- tibble(content = separate_unsc)

# date

# df$date <- str_extract(df$content[1], "(?<=\\b(adopted on ))(.*)(?=( in accordance))")
# 
# df$date <- str_remove(df$date, ",")

df$date <- str_extract(df$content[1], "[0-9]{1,2}[ ][A-Z]*[a-z]*[ ][0-9]{4}")

df$date <- str_extract_all(df$content[1], "[0-9]{1,2}[ ][A-Z]*[a-z]*[ ][0-9]{4}") %>% `[[`(1) %>% `[[`(3)

# spv

df$spv <- str_extract(df$content[1], "2020[/][0-9]{1,4}")

df$spv <- sub("[/]", "_", df$spv)

# topic

# df$topic <- str_extract(df$content[1], "(?<=item entitled [:punct:])(.*)(?=[:punct:][.][ ](and|The))")
# 
# df$topic <- str_extract(df$content[1], "(?<=item [:punct:])(.*)(?=[:punct:][.][ ](The|Resolution))")
# 
# df$topic <- str_remove(df$topic[1], "[:punct:][.].*")
# 
# df$topic <- str_trim(df$topic)

#df$topic <- str_extract(df$content[1], '(?<=\").+?(?=\")')

#df$topic <- str_trim(df$topic)

df$topic <- "Non-proliferation"

# speaker

df$speaker <- str_trim(str_extract(df$content[1], "(?<=([(]Signed[)][ ]))(.*)(?=( President))"))

# participant type

df$participanttype <- "The President"

# speech number

df$speech <- 1:(nrow(df))

# country

df$country <- str_extract(df$content, "(?<=[(])[^)]+")

df$country[df$participanttype == "The President"] <- 'South Africa'

# day, month, year

df$date2 <- dmy(df$date)

df$year <- year(df$date2)

df$month <- month(df$date2)

df$day <- day(df$date2)

df <- df %>% select(-date2)

# filename

df$filename <- paste("UNSC_",df$year,"_SPV.",df$spv,"_spch",formatC(1:(length(df$content)), digits = 2, flag = "0"),".txt", sep = "")

# basename

df$basename <- sub("_spch[0-9]{3}.txt", "", df$filename)

# role in un

df$role_in_un <- ""

# clean

df$content[1] <- sub("Council", "", df$content[1])

df$content[1] <- sub("Council", "", df$content[1])

df$content[1] <- str_remove(df$content[1], "(?=United).+?(?<=Council)")

df$content[1] <- str_remove(df$content[1], "[(]Signed.*")

#df$content[1] <- str_remove(df$content[1], "\\[Original\\: English and French\\]") 

df$content <- str_trim(df$content)

######################################### speaker ----

#df$speaker[2:nrow(df)] <- str_trim(str_extract(df$content[2:nrow(df)], "(?=(Mr|Mrs|Ms)).+?(?=[,])"))

df$speaker[2:nrow(df)] <- str_trim(str_extract(df$content[2:nrow(df)], "(?<=([,][ ])).+?(?=([ ][ ]))"))

#df$speaker[2:nrow(df)] <- str_trim(str_extract(df$content[2:nrow(df)], "(?<=([(]Signed[)])).+?(?=(Ambassador))"))

# participant type

df$country[2:nrow(df)] <- str_trim(str_extract(df$content[2:nrow(df)], "(?<=(of)).+?(?=(to the|of the))"))

#df$country[2:14] <- str_trim(str_extract(df$content[2:14], "(?<=([ ])).+?(?=([ ][ ]))"))

df$country[2:nrow(df)] <- str_trim(str_remove(df$country[2:nrow(df)], "the "))

#df$country <- str_trim(str_remove(df$country, "(?=(the)).+?(?<=of)"))

df$participanttype[!df$country %in% c("South Africa")] <- 'Mentioned'

df$participanttype[!df$country %in% c("Viet Nam",
                                      "Belgium",
                                      "China",
                                      "Dominican Republic",
                                      "Estonia",
                                      "France",
                                      "Germany",
                                      "Indonesia",
                                      "Niger",
                                      "Russian Federation",
                                      "Saint Vincent and the Grenadines",
                                      "Saint Vincent and Grenadines",
                                      "South Africa",
                                      "Tunisia",
                                      "United Kingdom",
                                      "United Kingdom of Great Britain and Northern Ireland",
                                      "United States of America")] <- 'Guest'

# headers and footers 

df$content <- gsub("[ ][ ][0-9]{1,3}[/][0-9]{1,3}[ ][ ]", "", df$content)

df$content <- str_remove(df$content, "[0-9]{1,3}[/][0-9]{1,3}$")

df$content <- str_remove(df$content, "[0-9]{1,3}[/][0-9]{1,3}[ ][0-9]{1,3}[/][0-9]{1,3}")

df$content <- gsub("21-00031", "", df$content)

df$content <- gsub("(S|[$])[/]2020[/]1324", "", df$content)

# ----

#df$content[1:2] <- str_trim(str_remove(df$content[1:2], "(?=(Briefing)).+?(?<=[ ][ ])"))

#df$content[1] <- str_trim(str_remove(df$content[1], "(?=(Briefing)).+?(?<=[ ][ ])"))

#df$content[2:nrow(df)] <- str_trim(str_remove(df$content[2:nrow(df)], "(?=(Annex)).+?(?<=to the United Nations)"))

df$content[2:nrow(df)] <- str_trim(str_remove(df$content[2:nrow(df)], "(?=(Annex)).+?(?<=[ ][ ])"))

df$content[2:nrow(df)] <- str_trim(str_remove(df$content[2:nrow(df)], "(?=(Statement)).+?(?<=[ ][ ])"))

#df$content[19] <- str_trim(str_remove(df$content[19], "(?=(Annex)).+?(?<=Konjufca)"))

df$content <- str_trim(str_remove(df$content, "\\[Original\\: English and French\\]|\\[Original\\: Spanish\\]|\\[Original\\: Chinese\\]|\\[Original\\: Arabic\\]|\\[Original\\: French\\]|\\[Original\\: English and Chinese\\]|\\[Original\\: English and Arabic\\]|\\[Original\\: Russian\\]"))

df$content <- str_trim(df$content)

df$content <- gsub("[ ]{1,}", " ", df$content)

# Check

#x <- str_extract_all(df$content, "[ ][ ][A-Z]*[a-z]*[0-9]*[ ][ ]|[0-9]{1,3}[/][0-9]{1,3}")

# types, tokens, sentences

df$types <- ntype(df$content)

df$tokens <- ntoken(df$content)

df$sentences <- nsentence(df$content)

# export

for(i in 1:length(df$content)){write(df$content[i], file = df$filename[i])}

# update base

#load("speeches_meta/unsc_2020.RData")

unsc_2020 <- rbind(unsc_2020, df)

save(unsc_2020, file = "speeches_meta/unsc_2020.RData")

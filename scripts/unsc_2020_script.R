### unsc extract
library(textreadr)
library(tidyverse)
library(pdftools)
library(magick)
library(tesseract)
library(lubridate)
library(quanteda)
library(stringi)

eng <- tesseract(language = "eng", options = list(tessedit_pageseg_mode = 1))
readpdf <- image_read_pdf("pdfs/S_PV.8699_E.pdf")
txt<-ocr(readpdf, engine = eng)
readpdf2 <- str_replace_all(paste(txt, collapse= " "), "[:blank:]{2,}","") 
readpdf3 <- str_replace_all(readpdf2, "\\\n"," ")

readpdf3 <- stri_trans_general(readpdf3, "Latin-ASCII")

# vector speech

separate_unsc <- str_split(readpdf3,
                           "(?=\\b(The President|The Secretary-General)\\:)|(?=\\b(Mr|Ms|Mrs)[.][ ](Lowcock|Griffiths|Chambas|Robinson|Skoog)[:])|(?=\\b(Mr|Mrs|Ms)[.][ ]([A-Z]*[a-z]*|[A-Z]*[a-z]*[ ][A-Z]*[a-z]*|[A-Z]*[a-z]*[ ][A-Z]*[a-z]*[ ][A-Z]*[a-z]*|[A-Z]*[a-z]*[-][A-Z]*[a-z]*|[A-Z]*[a-z]*[A-Z]*[a-z]*|[A-Z]*[a-z]*.[A-Z]*[a-z]*)[ ][(])") %>% unlist()

# tibble speech

df <- tibble(content = separate_unsc)

# date

df$date <- str_extract(df$content[1], "[0-9]{1,2}[ ][A-Z]*[a-z]*[ ][0-9]{4}")

# spv

df$spv <- sub("S[/]PV[.]", "", str_extract(df$content[1], "S[/]PV[.][0-9]{4}"))

# topic

df$topic <- str_extract(df$content[1], "(?=S[/]PV[.][0-9]{4}[ ]*)(.*)(?=[ ]*[0-9]{2}[/][0-9]{2}/[0-9]{4})")

df$topic <- sub("S[/]PV[.][0-9]{4}[ ]*", "", df$topic)

df$topic <- str_trim(df$topic)

# speaker part 1

df$speaker <- str_extract(df$content, "[^([(]|[:])]+")

df$speaker <- sub("[ ][(]", "", df$speaker)

# participant type part 1

df$participanttype <- str_extract(df$content, "(The President|The Secretary-General)|((Mr|Ms|Mrs)[.][ ](Lowcock|Griffiths|Chambas|Robinson|Skoog))|((Mr|Mrs|Ms)[.][ ]([A-Z]*[a-z]*|[A-Z]*[a-z]*[ ][A-Z]*[a-z]*|[A-Z]*[a-z]*[ ][A-Z]*[a-z]*[ ][A-Z]*[a-z]*|[A-Z]*[a-z]*[-][A-Z]*[a-z]*|[A-Z]*[a-z]*[A-Z]*[a-z]*|[A-Z]*[a-z]*.[A-Z]*[a-z]*)[ ][()])")

df$participanttype <- sub("[ ][(]", "", df$participanttype)

# speaker part 2

df$speaker[df$speaker == "The President"] <- 'Mr. Pham Binh Minh/Mr. Dang'

# speech number

df$speech <- 0:(nrow(df)-1)

# country

df$country <- str_extract(df$content, "[^)]+")

df$country <- str_extract(df$country, "(?<=[(]).*")

df$country[df$participanttype == "The President"] <- 'Viet Nam'

df$country[df$speaker %in% c("Mr. Chambas", "Mrs. Robinson", "The Secretary-General")] <- 'UN'

# participant type part 2

df$participanttype[df$country %in% c("Belgium",
                                     "China",
                                     "Dominican Republic",
                                     "Estonia",
                                     "France",
                                     "Germany",
                                     "Indonesia",
                                     "Niger",
                                     "Russian Federation",
                                     "Saint Vincent and the Grenadines",
                                     "South Africa",
                                     "Tunisia",
                                     "United Kingdom",
                                     "United States of America")] <- 'Mentioned'

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
                                     "South Africa",
                                     "Tunisia",
                                     "United Kingdom",
                                     "United States of America")] <- 'Guest'

# day, month, year

df$date2 <- dmy(df$date)

df$year <- year(df$date2)

df$month <- month(df$date2)

df$day <- day(df$date2)

df <- df %>% select(-date2)

# filename

df$filename <- paste("UNSC_",df$year,"_SPV.",df$spv,"_spch",formatC(0:(length(df$content)-1), digits = 2, flag = "0"),".txt", sep = "")

# basename

df$basename <- sub("_spch[0-9]{3}.txt", "", df$filename)

# role in un

df$role_in_un <- ""

# types, tokens, sentences

df$types <- ntype(df$content)

df$tokens <- ntoken(df$content)

df$sentences <- nsentence(df$content)

# Remove headers and footers

df$content <- gsub("[ ][ ][0-9]{1,2}[/][0-9]{1,2}", "", df$content)

df$content <- gsub("[ ][ ]20-00676[/]2020", "", df$content)

df$content <- gsub("[ ][ ]20-00676", "", df$content)

df$content <- gsub("[ ][ ]S[/]PV[.]8699[ ][ ]Maintenance[ ]of[ ]international[ ]peace[ ]and[ ]security[/]2020", "", df$content)

df$content <- gsub("[ ][ ]Maintenance[ ]of[ ]international[ ]peace[ ]and[ ]security[ ][ ]S[/]PV[.]8699", "", df$content)

df$content <- str_trim(df$content)

df$content <- gsub("[ ]{1,}", " ", df$content)

# remove speech 0

df <- df %>% slice(-1)

# export

for(i in 1:length(df$content)){write(df$content[i], file = df$filename[i])}

unsc_2020 <- rbind(unsc_2020, df)

save(unsc_2020, file = "unsc_2020.RData")

# possível regex para países (?<=[(])[^)]+

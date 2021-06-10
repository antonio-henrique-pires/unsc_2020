### unsc extract
library(tidyverse)
library(magick)
library(tesseract)
library(lubridate)
library(quanteda)
library(stringi)

eng <- tesseract(language = "eng", options = list(tessedit_pageseg_mode = 1))
readpdf <- image_read_pdf("speeches_pdf/S_PV.8732_E.pdf")
txt <- ocr(readpdf, engine = eng)
readpdf2 <- str_replace_all(paste(txt, collapse= " "), "[:blank:]{2,}","") 
readpdf3 <- str_replace_all(readpdf2, "\\\n"," ")

readpdf3 <- stri_trans_general(readpdf3, "Latin-ASCII")

# vector speech

separate_unsc <- str_split(readpdf3,
                           "(?=\\b(Dato[ ]Lim[ ]Jock[ ]Hoi[:]))|(?=\\b(Monsignor[ ]Hansen|The[ ]President|President[ ]Abbas|Aboul[ ]Gheit|King[ ]Philippe|The Secretary-General)[ ][()])|(?=\\b(The President|The Secretary-General)\\:)|(?=\\b(Mr|Ms|Mrs)[.][ ](Lowcock|Griffiths|Chambas|Robinson|Skoog|Mardini|Ruiz[ ]Massieu|Lacroix|Rajasingham|DiCarlo|Mueller|Raz|Matar|Salame|Licharz|Nakamitsu|Spleeters|Rama|Pedersen|Voronkov|Coninsx|Freij|Mladenov|Becker|Bachelet|De[ ]Roux|Sooka|Sori[-]Coulibaly|Costa[ ]Filho|Grau|Cevik|La[ ]Lime|Swan|Madeira|Smith)[:])|(?=\\b(Mr|Mrs|Ms)[.][ ]([A-Z]*[a-z]*|[A-Z]*[a-z]*[ ][A-Z]*[a-z]*|[A-Z]*[a-z]*[ ][A-Z]*[a-z]*[ ][A-Z]*[a-z]*|[A-Z]*[a-z]*[-][A-Z]*[a-z]*|[A-Z]*[a-z]*[A-Z]*[a-z]*|[A-Z]*[a-z]*.[A-Z]*[a-z]*|Pinto[ ]Lopes[ ]D.Alva)[ ][(])") %>% unlist()

# tibble speech

df <- tibble(content = separate_unsc)

# collapse false positives

#df$content[26] <- paste(df$content[26], df$content[27], sep = " ")

#df <- df %>% slice(-27)

# date

df$date <- str_extract(df$content[1], "[0-9]{1,2}[ ][A-Z]*[a-z]*[ ][0-9]{4}")

# spv

df$spv <- sub("S[/]PV[.]", "", str_extract(df$content[1], "S[/]PV[.][0-9]{4}"))

# spv if Resumption

#df$spv <- paste(df$spv, "Resumption1", sep = "")

# topic part 1

df$topic <- str_extract(df$content[1], "(?=S[/]PV[.][0-9]{4}[ ]*)(.*)(?=[ ]*[0-9]{2}[/][0-9]{2}/[0-9]{4})")

df$topic <- sub("S[/]PV[.][0-9]{4}[ ]*", "", df$topic)

# topic if resumption

#df$topic <- sub("S[/]PV[.][0-9]{4}[ ]*[ ][(]Resumption 1[)]", "", df$topic)

#df$topic <- sub("[(]Resumption 1[)]", "", df$topic)

# topic part 2

df$topic <- str_trim(df$topic)

# speaker part 1

df$speaker <- str_extract(df$content, "[^([(]|[:])]+")

df$speaker <- sub("[ ][(]", "", df$speaker)

# participant type part 1

df$participanttype <- str_extract(df$content, "(Monsignor[ ]Hansen|Dato[ ]Lim[ ]Jock[ ]Hoi|President[ ]Abbas|Aboul[ ]Gheit|King[ ]Philippe)|(The President|The Secretary-General)|((Mr|Ms|Mrs)[.][ ](Lowcock|Griffiths|Chambas|Robinson|Skoog|Mardini|Ruiz[ ]Massieu|Lacroix|Rajasingham|DiCarlo|Mueller|Raz|Matar|Salame|Licharz|Nakamitsu|Spleeters|Rama|Pedersen|Voronkov|Coninsx|Freij|Mladenov|Becker|Bachelet|De[ ]Roux|Sooka|Sori[-]Coulibaly|Costa[ ]Filho|Grau|Cevik|La[ ]Lime|Swan|Madeira|Smith))|((Mr|Mrs|Ms)[.][ ]([A-Z]*[a-z]*|[A-Z]*[a-z]*[ ][A-Z]*[a-z]*|[A-Z]*[a-z]*[ ][A-Z]*[a-z]*[ ][A-Z]*[a-z]*|[A-Z]*[a-z]*[-][A-Z]*[a-z]*|[A-Z]*[a-z]*[A-Z]*[a-z]*|[A-Z]*[a-z]*.[A-Z]*[a-z]*|Pinto[ ]Lopes[ ]D.Alva)[ ][()])")

df$participanttype <- sub("[ ][(]", "", df$participanttype)

# speaker part 2

df$speaker <- str_trim(df$speaker)

df$speaker[df$speaker %in% c("The President", "Mr. President")] <- 'Mr. Pecsteen de Buytswerve'

# speech number

df$speech <- 0:(nrow(df)-1)

# country

df$country <- str_extract(df$content, "(?<=[(])[^)]+")

df$country[df$participanttype == "The President"] <- 'Belgium'

df$country[df$speaker %in% c("Mr. Chambas", "Mrs. Robinson", "The Secretary-General", "Ruiz Massieu", "Lacroix", "Mr. Griffiths",
                             "Mr. Rajasingham", "Ms. DiCarlo", "Ms. Mueller", "Mr. Lowcock", "Ms. Matar", "Mr. Salame", "Mr. Licharz",
                             "Mrs. Nakamitsu", "Mr. Spleeters", "Mr. Pedersen", "Mr. Voronkov", "Ms. Coninsx",
                             "Mr. Mladenov", "Ms. Bachelet", "Ms. Sori-Coulibaly", "Mr. Ndiaye", "Mr. Hilale", "Ms. La Lime",
                             "Mr. Swan")] <- 'UN'

# Maybe also representative
df$country[df$speaker %in% c("Mr. Costa Filho")] <- 'UN'
#
df$country[df$speaker %in% c("Mr. Mardini")] <- 'International Committee of the Red Cross'

df$country[df$speaker %in% c("Mrs. Raz")] <- 'Committee on the Exercise of the Inalienable Rights of the Palestinian People'

df$country[df$speaker %in% c("Monsignor Hansen")] <- 'Holy See'

df$country[df$speaker %in% c("Mr. Abdelaziz", "Mr. Aboul Gheit")] <- 'League of the Arab States'

df$country[df$speaker %in% c("Dato Lim Jock Hoi")] <- 'ASEAN'

df$country[df$speaker %in% c("Mr. Rama", "Ms. Grau", "Mr. Cevik")] <- 'OSCE'

df$country[df$speaker %in% c("Ms. Freij")] <- 'Individual'

df$country[df$speaker %in% c("President Abbas")] <- 'Palestine'

df$country[df$speaker %in% c("Ms. Becker")] <- 'Watchlist on Children and Armed Conflict'

df$country[df$speaker %in% c("Mr. Chergui", "Mr. Matondo", "Mr. Madeira")] <- 'African Union'

df$country[df$speaker %in% c("King Philippe")] <- 'Belgium'

df$country[df$speaker %in% c("Mr. De Roux")] <- 'Commission for the Clarification of Truth, Coexistence and Non-Repetition of Colombia'

df$country[df$speaker %in% c("Ms. Sooka")] <- 'Foundation for Human Rights in South Africa'

df$country[df$speaker %in% c("Ms. Gilles")] <- 'Fondasyon Je Klere (FJKL)'

df$country[df$speaker %in% c("Mr. Smith")] <- 'Stockholm International Peace Research Institute'

# participant type part 2

df$participanttype[df$speaker %in% c("Mr. President")] <- 'The President'

df$participanttype[df$country %in% c("Viet Nam",
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

df$content <- gsub("[ ][ ][0-9]{1,2}[/][0-9]{1,2}[ ][ ]", "", df$content)

df$content <- gsub("[ ][ ]20-05035[ ][ ]", "", df$content)

#df$content <- gsub("[(]Resumption[ ]1[)][ ]", "", df$content)

df$content <- gsub("S[/]PV[.]8732  The situation in the Middle East  25[/]02[/]2020", "", df$content)

df$content <- gsub("25[/]02[/]2020  The situation in the Middle East  S[/]PV[.]8732", "", df$content)

# teste para footer

x <- str_extract_all(df$content, "TA")

#df$content <- gsub("[ ]TA9[ ]", "", df$content)

# 

df$content <- str_trim(df$content)

df$content <- gsub("[ ]{1,}", " ", df$content)

# remove speech 0

df <- df %>% slice(-1)

# export

for(i in 1:length(df$content)){write(df$content[i], file = df$filename[i])}

# update base

load("speeches_meta/unsc_2020.RData")

unsc_2020 <- rbind(unsc_2020, df)

save(unsc_2020, file = "speeches_meta/unsc_2020.RData")

# possível regex para países (?<=[(])[^)]+

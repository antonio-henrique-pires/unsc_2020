# Cleaning
library(tidyverse)
library(quanteda)
library(quanteda.textstats)

load("speeches_meta/unsc_2020.RData")

# headers and footers (not previously detected)

x <- kwic(unsc_2020$content, phrase("$ /"))

unsc_2020$content <- str_remove(unsc_2020$content, "[$][/]2$")

unsc_2020$content <- str_remove_all(unsc_2020$content, "[$][/]2020[/]254")

unsc_2020$content <- paste(unsc_2020$content, " ", sep = "")

unsc_2020$content <- str_remove(unsc_2020$content, "S[/]2[ ]")

unsc_2020$content <- str_remove(unsc_2020$content, "[$][/]2[ ]")

unsc_2020$content <- str_trim(unsc_2020$content)

x <- kwic(unsc_2020$content, phrase("S / 2"))

x <- kwic(unsc_2020$content, phrase("S / 2*"), window = 10)

unsc_2020$content <- str_remove_all(unsc_2020$content, "S[/]2020[/]1274")

unsc_2020$content <- str_remove_all(unsc_2020$content, "[ ]S[/]2020[/]263[ ]")

unsc_2020$content <- str_remove_all(unsc_2020$content, "[ ]S[/]2020[/]497[ ]")

x <- kwic(unsc_2020$content, phrase("20[-][0-9]{5}"), window = 10, valuetype = "regex")

unsc_2020$content <- str_remove(unsc_2020$content, "20[-][0-9]{5}")

x <- kwic(unsc_2020$content, phrase("Maintenance of international peace and security"))

z <- unsc_2020 %>% filter(str_detect(content, "Maintenance of international peace and security S[/]PV[.]8699 [(]Resumption 1[)]"))

unsc_2020$content <- str_remove(unsc_2020$content, "Maintenance of international peace and security S[/]PV[.]8699 [(]Resumption 1[)]")

x <- kwic(unsc_2020$content, phrase("The situation in the Middle East"), case_insensitive = FALSE, window = 10)

unsc_2020$content <- str_remove(unsc_2020$content, "16[/]01[/]2020 The situation in the Middle East S[/]PV[.]8704")

unsc_2020$content <- str_remove(unsc_2020$content, "05[/]10[/]2020 The situation in the Middle East S[/]PV[.]8764")

unsc_2020$content <- str_remove(unsc_2020$content, "S[/]PV[.]8770 The situation in the Middle East 15[/]10[/]2020")

unsc_2020$content <- str_remove(unsc_2020$content, "21[/]01[/]2020 The situation in the Middle East, including the Palestinian question S[/]PV[.]8706")

unsc_2020$content <- gsub("[ ]{1,}", " ", unsc_2020$content)

x <- kwic(unsc_2020$content, phrase("S / 2020"), case_insensitive = FALSE, window = 10)

z <- unsc_2020 %>% filter(str_detect(content, "[0-9]{1,2}[/][0-9]{1,2}[/][0-9]{4}"))

unsc_2020$content <- str_remove(unsc_2020$content, "The situation in the Middle East 06[/]02[/]2020 S[/]PV[.]8715")

unsc_2020$content <- str_remove(unsc_2020$content, "S[/]PV[.]8773 Election of five members of the International Court of Justice 11[/]11[/]2020")

x <- kwic(unsc_2020$content, phrase("$ / 2020"), case_insensitive = FALSE, window = 10)

unsc_2020$content <- str_remove(unsc_2020$content, "[$][/]2020[/]509")

unsc_2020$content <- str_replace_all(unsc_2020$content, "[$][/]", "S/")

z <- unsc_2020 %>% filter(str_detect(content, "[0-9]{1,3}[/][0-9]{1,3}[ ]$"))

unsc_2020$content <- str_remove(unsc_2020$content, "[0-9]{1,3}[/][0-9]{1,3}[ ]$")

# Some typos

z <- unsc_2020 %>% filter(str_detect(content, "Iam"))

x <- kwic(unsc_2020$content, "Iam*", case_insensitive = FALSE, window = 10)

unsc_2020$content <- str_replace_all(unsc_2020$content, "Iam", "I am")

x <- kwic(unsc_2020$content, "Ialso*", case_insensitive = FALSE, window = 10)

unsc_2020$content <- str_replace_all(unsc_2020$content, "Ialso", "I also")

x <- kwic(unsc_2020$content, "amtaking*", case_insensitive = FALSE, window = 10)

unsc_2020$content <- str_replace_all(unsc_2020$content, "amtaking", "am taking")

unsc_2020$types <- ntype(unsc_2020$content)

unsc_2020$tokens <- ntoken(unsc_2020$content)

unsc_2020$sentences <- nsentence(unsc_2020$content)

#save(unsc_2020, file = "speeches_meta/unsc_2020.RData")

unsc_dfm <- corpus(unsc_2020, docid_field = 'filename', text_field='content') %>% 
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  dfm() %>% 
  textstat_frequency()

# Check Countries' names

countries <- unsc_2020 %>% count(country)

unsc_2020$country[unsc_2020$country == "El] Salvador"] <- "El Salvador"

unsc_2020$country[unsc_2020$country == "European Union"] <- "EU"

unsc_2020$country[unsc_2020$country == "High Representative for Bosnia and Herzegovina"] <- "Bosnia And Herzegovina"

unsc_2020$country[unsc_2020$country == "International Criminal Court"] <- "ICC"

unsc_2020$country[unsc_2020$country == "International Court of Justice"] <- "ICJ"

unsc_2020$country[unsc_2020$country == "Iran"] <- "Islamic Republic Of Iran"

unsc_2020$country[unsc_2020$country == "Kyrgyztan"] <- "Kyrgyzstan"

unsc_2020$country[unsc_2020$country == "Cabo Verde"] <- "Cape Verde"

unsc_2020$country[unsc_2020$country == "Kyrgyz Republic"] <- "Kyrgyzstan"

unsc_2020$country[unsc_2020$country == "League of Arab States"] <- "League Of Arab States"

unsc_2020$country[unsc_2020$country == "League of the Arab States"] <- "League Of Arab States"

unsc_2020$country[unsc_2020$country == "Republic of Azerbaijan"] <- "Azerbaijan"

unsc_2020$country[unsc_2020$country == "Republic of Croatia"] <- "Croatia"

unsc_2020$country[unsc_2020$country == "Republic of Kazakhstan"] <- "Kazakhstan"

unsc_2020$country[unsc_2020$country == "Republic of San Marino"] <- "San Marino"

unsc_2020$country[unsc_2020$country == "Republic of Serbia"] <- "Serbia"

unsc_2020$country[unsc_2020$country == "Republic of Tunisia"] <- "Tunisia"

unsc_2020$country[unsc_2020$country == "Russia"] <- "Russian Federation"

unsc_2020$country[unsc_2020$country == "Russian Federations"] <- "Russian Federation"

unsc_2020$country[unsc_2020$country == "Saint Vincent and Grenadines"] <- "Saint Vincent and the Grenadines"

unsc_2020$country[unsc_2020$country == "Slovak Republic"] <- "Slovakia"

unsc_2020$country[unsc_2020$country == "Socialist Republic of Viet Nam"] <- "Viet Nam"

unsc_2020$country[unsc_2020$country == "Syria"] <- "Syrian Arab Republic"

unsc_2020$country[unsc_2020$country == "the Russian Federation"] <- "Russian Federation"

unsc_2020$country[unsc_2020$country == "the United States of America"] <- "United States Of America"

unsc_2020$country[unsc_2020$country == "United States of America"] <- "United States Of America"

unsc_2020$country[unsc_2020$country == "United Kingdom and Northern Ireland"] <- "United Kingdom Of Great Britain And Northern Ireland"

unsc_2020$country[unsc_2020$country == "Bosnia and Herzegovina"] <- "Bosnia And Herzegovina"

unsc_2020$country[unsc_2020$country == "Committee on the Exercise of the Inalienable Rights of the Palestinian People"] <- "Committee On The Exercise Of The Inalienable Rights Of The Palestinian People"

unsc_2020$country[unsc_2020$country == "Community Empowerment for Progress Organization"] <- "Community Empowerment For Progress Organization"

unsc_2020$country[unsc_2020$country == "Cote d'Ivoire"] <- "Cote D'Ivoire"

unsc_2020$country[unsc_2020$country == "Democratic Republic of the Congo"] <- "Democratic Republic Of The Congo"

unsc_2020$country[unsc_2020$country == "Families for Freedom"] <- "Families For Freedom"

unsc_2020$country[unsc_2020$country == "International Committee of the Red Cross"] <- "International Committee Of The Red Cross"

unsc_2020$country[unsc_2020$country == "International Organization of la Francophonie"] <- "International Organization Of La Francophonie"

unsc_2020$country[unsc_2020$country == "Islamic Republic of Iran"] <- "Islamic Republic Of Iran"

unsc_2020$country[unsc_2020$country == "NGO Working Group on Women, Peace and Security"] <- "Ngo Working Group On Women Peace And Security"

unsc_2020$country[unsc_2020$country == "Organization of Islamic Cooperation"] <- "Organization Of Islamic Cooperation"

unsc_2020$country[unsc_2020$country == "Physicians for Human Rights"] <- "Physicians For Human Rights"

unsc_2020$country[unsc_2020$country == "Republic of Korea"] <- "Republic Of Korea"

unsc_2020$country[unsc_2020$country == "Saint Vincent and the Grenadines"] <- "Saint Vincent And The Grenadines"

unsc_2020$country[unsc_2020$country == "Slovakia"] <- "Slovak Republic"

unsc_2020$country[unsc_2020$country == "South African"] <- "South Africa"

unsc_2020$country[unsc_2020$country == "Trinidad and Tobago"] <- "Trinidad And Tobago"

unsc_2020$country[unsc_2020$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom Of Great Britain And Northern Ireland"

unsc_2020$country[unsc_2020$country == "United Kingdom"] <- "United Kingdom Of Great Britain And Northern Ireland"

unsc_2020$country[unsc_2020$country == "Bolivarian Republic of Venezuela"] <- "Venezuela (Bolivarian Republic Of)"

unsc_2020$country[unsc_2020$country == "Plurinational State of Bolivia"] <- "Bolivia (Plurinational State Of)"

unsc_2020$country[unsc_2020$country == "State of Palestine"] <- "Palestine"

z <- unsc_2020 %>% filter(str_detect(country, "Estonia[,]|Belgium[,]|Indonesia[,]|Permanent Mission|South Africa[,]|spoke in French|State of Germany|State of Palestine|State with Responsibility|today|Tunisia[,]|Law and Security|Political and|Ministry"))

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_791_spch005.txt"] <- "Tunisia"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.8724_spch010.txt"] <- "France"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1092_spch012.txt"] <- "Germany"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1286_spch015.txt"] <- "United Kingdom Of Great Britain And Northern Ireland"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch002.txt"] <- "UN"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch003.txt"] <- "UN"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch004.txt"] <- "African Union"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch005.txt"] <- "South Africa"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch006.txt"] <- "Belgium"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch007.txt"] <- "Estonia"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch008.txt"] <- "Tunisia"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch009.txt"] <- "Saint Vincent And The Grenadines"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch010.txt"] <- "Germany"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1176_spch011.txt"] <- "Indonesia"

save(unsc_2020, file = "speeches_meta/unsc_2020.RData")

# on behalf

z <- unsc_2020 %>% filter(str_detect(content, "speak on behalf")) %>% select(content, country, filename)

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.8699Resumption1_spch036.txt"] <- "Movement of Non-Aligned Countries"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1055_spch020.txt"] <- "Movement of Non-Aligned Countries"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_791_spch024.txt"] <- "Nordic Countries"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.2020_1183_spch012.txt"] <- "A3+1"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.8699_spch098.txt"] <- "Nordic Countries"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.8723_spch029.txt"] <- "Nordic Countries"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.8699_spch086.txt"] <- "ASEAN"

unsc_2020$country[unsc_2020$filename == "UNSC_2020_SPV.8706Resumption1_spch022.txt"] <- "EU"

#Only speeches UNSC_2020_SPV.2020_1090_spch012.txt and UNSC_2020_SPV.2020_1179_spch007.txt were not delivered
#on behalf of the A3+1 (at least not explicitly)

z <- unsc_2020 %>% filter(str_detect(content, "A3")) %>% select(content, country, filename)

z <- z %>% filter(!filename %in% c("UNSC_2020_SPV.2020_1090_spch012.txt", "UNSC_2020_SPV.2020_1179_spch007.txt"))

unsc_2020$country[unsc_2020$filename %in% z$filename] <- "A3+1"

z <- unsc_2020 %>% filter(str_detect(content, "I have the honour")) %>% select(content, country, filename)

unsc_2020$country[unsc_2020$filename %in% c("UNSC_2020_SPV.8706Resumption1_spch024.txt",
                                            "UNSC_2020_SPV.8723_spch033.txt",
                                            "UNSC_2020_SPV.2020_346_spch022.txt")] <- "Movement of Non-Aligned Countries"

unsc_2020$country[unsc_2020$filename %in% c("UNSC_2020_SPV.8706Resumption1_spch030.txt",
                                            "UNSC_2020_SPV.2020_341_spch048.txt",
                                            "UNSC_2020_SPV.2020_596_spch031.txt")] <- "Organization Of Islamic Cooperation"

unsc_2020$country[unsc_2020$filename %in% "UNSC_2020_SPV.2020_1237_spch004.txt"] <- "UN"

unsc_2020$country[unsc_2020$filename %in% c("UNSC_2020_SPV.8728_spch010.txt",
                                            "UNSC_2020_SPV.2020_489_spch007.txt",
                                            "UNSC_2020_SPV.2020_598_spch011.txt",
                                            "UNSC_2020_SPV.2020_436_spch013.txt",
                                            "UNSC_2020_SPV.8731_spch016.txt")] <- "A3+1"

z <- unsc_2020 %>% filter(str_detect(content, "in my capacity")) %>% select(content, country, filename, participanttype)

unsc_2020$country[unsc_2020$filename %in% c("UNSC_2020_SPV.8777_spch002.txt",
                                            "UNSC_2020_SPV.2020_987_spch003.txt")] <- "UN"

z <- unsc_2020 %>% filter(str_detect(content, "statement on behalf")) %>% select(content, country, filename, participanttype)

unsc_2020$country[unsc_2020$filename %in% c("UNSC_2020_SPV.8735_spch005.txt",
                                            "UNSC_2020_SPV.8743_spch010.txt",
                                            "UNSC_2020_SPV.2020_325_spch012.txt",
                                            "UNSC_2020_SPV.2020_514_spch014.txt")] <- "A3+1"

unsc_2020$country[unsc_2020$filename %in% c("UNSC_2020_SPV.2020_663_spch028.txt",
                                            "UNSC_2020_SPV.2020_727_spch024.txt",
                                            "UNSC_2020_SPV.2020_751_spch020.txt")] <- "Nordic Countries"

unsc_2020$country[unsc_2020$filename %in% "UNSC_2020_SPV.2020_751_spch050.txt"] <- "Pacific Islands Forum"

unsc_2020$country[unsc_2020$filename %in% "UNSC_2020_SPV.2020_751_spch022.txt"] <- "Group of Friends on Climate and Security"

save(unsc_2020, file = "speeches_meta/unsc_2020.RData")



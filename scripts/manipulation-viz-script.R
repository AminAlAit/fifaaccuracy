# ==============================================================================
# How Accurate Is FIFA At Predicting Future Stars?
# Analysis & Visualization Pipeline
#
# Author: Amin Al-Ait
# Course: Seminar - Sports Data Visualization, TU Dortmund (M.Sc. Data Science)
# Grade: 1.0 (1/1)
# Published Article: https://kurt.digital/2022/05/11/footballs-future-stars-how-accurately-is-ea-fifa-predicting-them/
# Full Report PDF: https://github.com/AminAlAit/fifaaccuracy/blob/main/How%20Accurate%20Is%20FIFA%20At%20Predicting%20Future%20Stars.pdf
# ==============================================================================

### 1. Packages & Data Import
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(grid)
  library(gridExtra)
  library(cowplot)
  library(ggthemes)
  library(hrbrthemes)
  library(viridis)
  library(RColorBrewer)
  library(ggrepel)
  library(gghighlight)
  library(ggridges)
  library(directlabels)
  library(ggtext)
  library(countrycode)
  library(lubridate)
  library(janitor)
  library(reshape2)
  library(igraph)
  library(ggraph)
  library(tm)
  library(wordcloud)
  library(wordcloud2)
  library(SnowballC)
  library(rworldmap)
  library(emojifont)
  library(quantmod)
  library(lattice)
  library(packcircles)
  library(gifski)
  library(gganimate)
})

# Optional packages for flag icons and player images if installed
if (requireNamespace("ggflags", quietly = TRUE)) library(ggflags)
if (requireNamespace("ggimage", quietly = TRUE)) library(ggimage)
if (requireNamespace("ggpubr", quietly = TRUE))  library(ggpubr)

# Helper function to locate data files across environments (local or container)
find_data_file <- function(filename) {
  candidates <- c(
    file.path("data", filename),
    file.path("..", "data", filename),
    file.path(".", filename),
    file.path("~", "workspace", filename)
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  stop(paste("Data file not found:", filename))
}

### 2. Our Data
fifa_path <- find_data_file("fifatable.csv")
table <- read_csv(fifa_path, col_types = cols(...1 = col_skip()), show_col_types = FALSE)
names(table)
glimpse(table)
head(table)

### 3.1 Data Prep
## Data Prep
{
  table <- as.data.frame(sapply(table, gsub, pattern = "<fc>",     replacement = "ü"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<c5>",     replacement = "Å"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<c7>",     replacement = "Ç"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<c9>",     replacement = "É"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<d6>",     replacement = "Ö"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<ea>",     replacement = "ê"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<ee>",     replacement = "î"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<ed>",     replacement = "í"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<e1>",     replacement = "á"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<e2>",     replacement = "â"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<e3>",     replacement = "ã"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<e4>",     replacement = "ä"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<e5>",     replacement = "å"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<e6>",     replacement = "æ"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<e7>",     replacement = "ç"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<e8>",     replacement = "è"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<e9>",     replacement = "é"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<fa>",     replacement = "ú"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<fc>",     replacement = "ü"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<f1>",     replacement = "ñ"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<f3>",     replacement = "ó"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<f6>",     replacement = "ö"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<f8>",     replacement = "ø"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<U+009E>", replacement = "ž"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<U+008E>", replacement = "ž"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<f0>",     replacement = "ð"))
  table <- as.data.frame(sapply(table, gsub, pattern = "<ef>",     replacement = "ï"))
  table[, "id"]              <- as.numeric(table[, "id"])
  table[, "FifaIndex"]       <- as.numeric(table[, "FifaIndex"])
  table[, "Fifa_year"]       <- as.numeric(table[, "Fifa_year"])
  table[, "player_id"]       <- as.numeric(table[, "player_id"])
  table[, "sofifa_page"]     <- as.numeric(table[, "sofifa_page"])
  table[, "pos_in_list"]     <- as.numeric(table[, "pos_in_list"])
  table[, "SoFifaID"]        <- as.numeric(table[, "SoFifaID"])
  table[, "age"]             <- as.numeric(table[, "age"])
  table[, "overall"]         <- as.numeric(table[, "overall"])
  table[, "max_potential"]   <- as.numeric(table[, "max_potential"])
  table[, "games_played"]    <- as.numeric(table[, "games_played"])
  table[, "points_per_game"] <- as.numeric(table[, "points_per_game"])
  table[, "goals"]           <- as.numeric(table[, "goals"])
  table[, "assists"]         <- as.numeric(table[, "assists"])
  table[, "minutes"]         <- as.numeric(table[, "minutes"])
  
  table <- table %>% 
    mutate(difference = max_potential - overall)
  #, short_name = paste(str_to_title(unlist(strsplit(SoFifaName, "-"))), collapse = " "))
  
  scatter_df <- table %>%
    group_by(player_id) %>%
    summarise(prime  = max(overall, na.rm = TRUE), 
              bottom =  min(overall, na.rm = TRUE),
              pot19  =  max(max_potential, na.rm = TRUE)) %>%
    mutate(passed    = case_when(
      prime  >= pot19 ~ TRUE, 
      prime  <  pot19 ~ FALSE)
    ) 
  
  ## To add more Columns and limit duplicated players
  extra_cols <- table %>%
    # filter by age
    group_by(player_id) %>%
    summarise_all(first) %>%
    select(SoFifaID, player_id, sofifa_page, pos_in_list, short_name, overall, difference, FifaIndex, nationality, club)
  
  scatter_df <- inner_join(scatter_df, extra_cols, by = c("player_id" = "player_id")) %>%
    distinct()
  
  table1 <- scatter_df %>%
    select(SoFifaID, player_id, sofifa_page, pos_in_list, short_name, prime, difference, bottom) %>%
    rename(diff19 = difference) %>%
    inner_join(table, by = c("SoFifaID" = "SoFifaID")) %>%
    select(SoFifaID, player_id.x, sofifa_page.x, pos_in_list.x, FifaIndex, short_name.x, long_name, age, Fifa_year, 
           overall, prime, bottom, max_potential, diff19, nationality, club) %>%
    rename(SoFifaID = SoFifaID, sofifa_page = sofifa_page.x, pos_in_list = pos_in_list.x, 
           short_name = short_name.x, player_id = player_id.x) %>%
    mutate(pass_pot           = ifelse(prime >= max_potential  , TRUE         , FALSE),  
           pass_pot_names     = ifelse(pass_pot                , short_name   , NA),
           breakthrough_pot   = ifelse(overall >= max_potential, max_potential, NA),
           # breathrough_pot_1 should hold only the first value of the breakthrough, 
           # while breakthrough_pot has all others
           breakthrough_pot_1 = ifelse(overall >= max_potential, max_potential, NA),
           # for plot: Age Distribution of Pot Achievers
           pass_pot_num       = ifelse(is.na(overall), 0, (ifelse(overall < max_potential, 0, 1))),
           # this will be filled later in a for loop in section: 
           pass_pot_num_1     = 0,
           prime_num = ifelse(is.na(overall), 0, (ifelse(overall == prime, 1, 0))),
           prime_num_1 = 0
    )
  
  # Creating pass_pot_num_1
  for (row in 1:nrow(table1)) {
    if(table1[row, "age"] == 19){
      one_done = FALSE
    }
    if(table1[row, "pass_pot_num"] == 0) # [0] => 0
      table1[row, "pass_pot_num_1"] <- 0
    
    else if(table1[row, "pass_pot_num"] == 1 & table1[row - 1, "pass_pot_num"] == 0 & !one_done){ # [0][1] => 1
      table1[row, "pass_pot_num_1"] <- 1
      one_done = TRUE
    }
    else if(table1[row, "pass_pot_num"] == 1 & table1[row - 1, "pass_pot_num"] == 1) # [1][1] => 0
      table1[row, "pass_pot_num_1"] <- 0
  }
  # Creating prime_num_1
  for (row in 1:nrow(table1)) {
    if(table1[row, "age"] == 19){
      one_done = FALSE
    }
    if(table1[row, "prime_num"] == 0) # [0] => 0
    {table1[row, "prime_num_1"] <- 0}
    
    else if(table1[row, "prime_num"] == 1 & table1[row - 1, "prime_num"] == 0 & !one_done){ # [0][1] => 1
      table1[row, "prime_num_1"] <- 1
      one_done = TRUE
    }
    else if(table1[row, "prime_num"] == 1 & table1[row - 1, "prime_num"] == 1) # [1][1] => 0
    {table1[row, "prime_num_1"] <- 0}
  }
  ## Housekeeping code
  record("renv@0.14.0")
  restore(packages <- "renv")
  year_labs        <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                        "2015", "2016", "2017", "2018", "2019", "2020", "2021")
  options(repr.plot.width = 5)
  range_max_pot    <- range(table$max_potential, na.rm = TRUE)
  range_1          <- as.integer(((as.integer(range_max_pot[2]) - as.integer(range_max_pot[1]))/3) + 
                                   as.integer(range_max_pot[1]))
  range_2          <- as.integer(((as.integer(range_max_pot[2]) - as.integer(range_max_pot[1]))/3) + range_1)
  pot_brackets     <- c(
    paste("[", range_max_pot[1], ", ", range_1,          "[", sep = ""),
    paste("[", range_1,          ", ", range_2,          "[", sep = ""),
    paste("[", range_2,          ", ", range_max_pot[2], "]", sep = ""))
}

### 4.1 How Many?
## Data Preds
{
  # Measuring accuracy in predictions
  acc_preds <- scatter_df %>%
    mutate(passed = case_when(
      prime >= pot19 ~ 1, 
      prime <  pot19 ~ 0)
    )
  
  ## Data prep for first pie chart
  #pie_1 <- data.frame(
  #  group = c("Passed", "Did Not Pass"),
  #  value = c(players_pass, players_not_pass))
  # Compute the position of pie category labels
  #pie_1 <- pie_1 %>% 
  #  arrange(desc(group)) %>%
  #  mutate(prop = value / sum(pie_1$value) * 100,
  #         ypos = cumsum(prop) - 0.5 * prop)
  
  # Data prep for 1st 
  pass_pot_pass_plot_1 <- table1 %>%
    filter(FifaIndex <= 11) %>%
    mutate(passed = case_when(
      prime >= max_potential ~ "Passed", 
      prime <  max_potential ~ "Did not pass"),
      max_pot_category = case_when(
        max_potential >= range_max_pot[1] & max_potential < range_1 
        ~ paste("[", range_max_pot[1], ", ", range_1, "[", sep = ""), 
        max_potential >= range_1 & max_potential < range_2          
        ~ paste("[", range_1, ", ", range_2, "[", sep = ""), 
        max_potential >= range_2 & max_potential <= range_max_pot[2] 
        ~ paste("[", range_2, ", ", range_max_pot[2], "]", sep = ""))) %>%
    group_by(player_id) %>%
    summarize_all(first) %>%
    ungroup() %>%
    group_by(passed, max_pot_category) %>%
    select(passed, max_pot_category) %>%
    filter(!is.na(max_pot_category)) %>%
    count()
  # Add these extra lines to turn the potential brackets into columns
  #spread(passed, n, fill = 0)
  
  # 2nd bar charts
  pass_pot_pass_plot_2 <- table1 %>%
    filter(FifaIndex > 11) %>%
    mutate(passed = case_when(
      prime >= max_potential ~ "Passed", 
      prime <  max_potential ~ "Did not pass"),
      max_pot_category = case_when(
        max_potential >= range_max_pot[1] & max_potential < range_1 
        ~ paste("[", range_max_pot[1], ", ", range_1, "[", sep = ""), 
        max_potential >= range_1 & max_potential < range_2          
        ~ paste("[", range_1, ", ", range_2, "[", sep = ""), 
        max_potential >= range_2 & max_potential < range_max_pot[2] 
        ~ paste("[", range_2, ", ", range_max_pot[2], "]", sep = ""))) %>%
    group_by(player_id) %>%
    summarize_all(first) %>%
    ungroup() %>%
    group_by(passed, max_pot_category) %>%
    select(passed, max_pot_category) %>%
    filter(!is.na(max_pot_category)) %>%
    count()
  # Add these extra lines to turn the potential brackets into columns
  #spread(passed, n, fill = 0)
  
  ## Viz Part
  # Plotting first pie chart
  #plot_1 <- ggplot(pie_1, aes(x = "", y = prop, fill = group)) +
  #  geom_bar(stat = "identity", width = 1, color = "white") +
  #  coord_polar("y", start = 0) +
  #  theme_void() + 
  #  theme(legend.position = "none") +
  #  geom_text(aes(
  #    y = ypos, 
  #    label = c(
  #      paste(round(100 * (players_pass/nrow(acc_preds)),     2), "%\nPassed",       sep = ""), 
  #      paste(round(100 * (players_not_pass/nrow(acc_preds)), 2), "%\nDid not pass", sep = ""))), 
  #size = 5,
  #    color = "white") +
  #  scale_fill_brewer(palette = "Set1") +
  #  labs(
  #    title    = paste(nrow(acc_preds), " players.", sep = ""),
  #subtitle = paste("360 players of age 19, from each FIFA 07 to FIFA 21. 5,040 players in total." , sep = "")
  #    )
  
  
  ## Housekeeping code
  total_overachivers_1   <- acc_preds %>% drop_na(passed)   %>% filter(passed    == 1, FifaIndex <= 11) %>% nrow()
  total_fails_1          <- acc_preds %>% drop_na(passed)   %>% filter(passed    == 0, FifaIndex <= 11) %>% nrow()
  total_1                <- acc_preds %>% drop_na(player_id) %>% filter(FifaIndex <= 11)                 %>% nrow()
  total_overachivers_2   <- acc_preds %>% drop_na(passed)   %>% filter(passed    == 1, FifaIndex >  11) %>% nrow()
  total_fails_2          <- acc_preds %>% drop_na(passed)   %>% filter(passed    == 0, FifaIndex >  11) %>% nrow()
  total_2                <- acc_preds %>% drop_na(player_id) %>% filter(FifaIndex >  11)                 %>% nrow()
  
  total_players          <- acc_preds %>% drop_na(player_id) %>% nrow()
  
  total_players_pass     <- sum(acc_preds$passed)
  total_players_not_pass <- nrow(acc_preds) - sum(acc_preds$passed)
  
  # Plotting second bar chart
  p1 <- ggplot(pass_pot_pass_plot_1, aes(fill = max_pot_category, y = n, x = passed)) + 
    geom_bar(position = "dodge", stat = "identity") +
    theme_ipsum() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          #axis.line        = element_line(colour = "black"),
          panel.background = element_blank()) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title     = paste("There are ", total_players, " players in this study, partitioned into two groups:", sep = ""),
      subtitle  = paste(
        "Group 1: ",
        total_1, 
        " Players that have 10+ years of recorded stats (played from 2007 to 2011):\n" , 
        total_overachivers_1, " players passed (", 
        round(100 * (total_overachivers_1 / total_1), 2), "%)", 
        " vs ",
        total_fails_1, " that did not (", 
        round(100 * (total_fails_1 / total_1), 2), "%).", 
        sep = ""),
      #subtitle = paste("360 players of age 19, from each FIFA 07 to FIFA 21. 5,040 players in total." , sep = ""),
      x        = "",
      y        = "",
      fill     = "Potential Brackets")
  
  p2 <- ggplot(pass_pot_pass_plot_2, aes(fill = max_pot_category, y = n, x = passed)) + 
    geom_bar(position = "dodge", stat = "identity") +
    theme_ipsum() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          #axis.line        = element_line(colour = "black"),
          panel.background = element_blank()) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      subtitle  = paste(
        "Group 2: ",
        total_2, " Players that have less that 10 years of recorded stats (played from 2012 to 2021):\n" , 
        total_overachivers_2, " players passed (", 
        round(100 * (total_overachivers_2 / total_2), 2), "%)", 
        " vs ",
        total_fails_2, " that did not (", 
        round(100 * (total_fails_2 / total_2), 2), "%).", 
        sep = ""),
      #subtitle = paste("360 players of age 19, from each FIFA 07 to FIFA 21. 5,040 players in total." , sep = ""),
      x        = "",
      y        = "",
      fill     = "Potential Brackets")
  
  # Plotting third bar chart
  #plot_3 <- ggplot(pass_pot_pass_plot, aes(fill = passed, y = n, x = max_pot_category)) + 
  #  geom_bar(position = "dodge", stat = "identity") +
  #  scale_fill_brewer(palette = "Set1") +
  #  labs(
  #    title    = paste(players_not_pass, " players did not pass vs ", players_pass, " that did.", sep = ""),
  #subtitle = paste("360 players of age 19, from each FIFA 07 to FIFA 21. 5,040 players in total." , sep = ""),
  #    x        = "",
  #    y        = "",
  #    fill     = "") +
  #  theme_ipsum()
  
  ## Putting all plots in one figure
  #ggarrange(plot_1, plot_2, ncol = 1, nrow = 2, labels = c("", ""))
  #ggarrange(plot_1, plot_2, ncol = 2, nrow = 1, labels = c("", ""))
  #ggarrange(plot_1, plot_3, ncol = 1, nrow = 1, labels = c("", ""))
  #ggarrange(plot_1, plot_3, ncol = 2, nrow = 1, labels = c("", ""))
}  

ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")

bar_plot <- acc_preds %>%
  mutate(year = as.numeric(case_when(
    FifaIndex <  10 ~ paste("200", FifaIndex, sep = ""),
    FifaIndex >= 10 ~ paste("20", FifaIndex, sep = "")
  ))) %>%
  group_by(year) %>%
  count() 

ggplot(bar_plot, aes(y = n, x = year, fill = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(x = year, y = n, label = n), vjust = -0.5) +
  scale_fill_viridis() +
  theme_ipsum() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        #axis.line        = element_line(colour = "black"),
        panel.background = element_blank()) +
  scale_x_continuous(breaks = round(seq(min(bar_plot$year), max(bar_plot$year), by = 1), 1)) +
  guides(fill = FALSE) +
  labs(
    title  = paste(total_players, " players, 360 players from each year." , sep = ""),
    x      = "FIFA game editions",
    y      = "")

### 4.2 How Accurate Was FIFA Game Every Year?
## Data Prep
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

temp <- scatter_df %>%
  mutate(max_pot_category = case_when(
    pot19 >= range_max_pot[1] & pot19 < range_1 
    ~ paste("[", range_max_pot[1], ", ", range_1, "[", sep = ""), 
    pot19 >= range_1 & pot19 < range_2          
    ~ paste("[", range_1, ", ", range_2, "[", sep = ""), 
    pot19 >= range_2 & pot19 < range_max_pot[2] 
    ~ paste("[", range_2, ", ", range_max_pot[2], "]", sep = ""))) %>%
  select(passed, max_pot_category, FifaIndex, short_name) %>%
  filter(!is.na(max_pot_category)) %>%
  group_by(FifaIndex, passed) %>%
  count(max_pot_category) 

brck_acc <- as.data.frame(cbind.fill(
  unique(table$FifaIndex),
  temp %>% filter(passed == FALSE, max_pot_category == "[64, 74[") 
  %>% ungroup() %>% select(n, -FifaIndex,  -max_pot_category, -passed) %>% rename(np_bot = n), 
  temp %>% filter(passed == FALSE, max_pot_category == "[74, 84[") 
  %>% ungroup() %>% select(n, -FifaIndex,  -max_pot_category, -passed) %>% rename(np_mid = n), 
  temp %>% filter(passed == FALSE, max_pot_category == "[84, 94]") 
  %>% ungroup() %>% select(n, -FifaIndex,  -max_pot_category, -passed) %>% rename(np_top = n), 
  temp %>% filter(passed == TRUE, max_pot_category == "[64, 74[") 
  %>% ungroup() %>% select(n, -FifaIndex,  -max_pot_category, -passed) %>% rename(p_bot = n), 
  temp %>% filter(passed == TRUE, max_pot_category == "[74, 84[") 
  %>% ungroup() %>% select(n, -FifaIndex,  -max_pot_category, -passed) %>% rename(p_mid = n), 
  temp %>% filter(passed == TRUE, max_pot_category == "[84, 94]") 
  %>% ungroup() %>% select(n, -FifaIndex,  -max_pot_category, -passed) %>% rename(p_top = n),
  fill = NA)) %>% select(-V8) %>%
  mutate(accuracy_bot = round((as.integer(p_bot) / (as.integer(p_bot) + as.integer(np_bot))) * 100),
         accuracy_mid = round((as.integer(p_mid) / (as.integer(p_mid) + as.integer(np_mid))) * 100),
         accuracy_top = round((as.integer(p_top) / (as.integer(p_top) + as.integer(np_top))) * 100),
         total = as.integer(p_top) + as.integer(p_mid) + as.integer(p_bot) + 
           as.integer(np_top) + as.integer(np_mid) + as.integer(np_bot)) %>%
  rename(fifa_id = V1) %>%
  select(fifa_id, accuracy_bot, accuracy_mid, accuracy_top) %>%
  gather(key = "accuracy_bracket", value = "count", -fifa_id) %>%
  mutate(year = as.integer(case_when(
    as.integer(fifa_id) <  10 ~ paste("200", fifa_id, sep = ""),
    as.integer(fifa_id) >= 10 ~ paste("20",  fifa_id, sep = ""))),
    count = ifelse(is.na(count), 0, count))

## Vizualization Part
ggplot(brck_acc, aes(x = year, y = count)) + 
  geom_line(aes(color = accuracy_bracket)) +
  geom_point(aes(x  = year, y = count, color = accuracy_bracket)) +
  theme_bw() +
  geom_rect(data = brck_acc[1,], aes(xmin = 2007, xmax = 2011, ymin = 0, ymax = 70), fill = "green", alpha = 0.1) +
  scale_color_manual(values = c("darkred", "steelblue", "darkgreen"), labels = c("[68, 74[", "[74, 84[", "[84, 94]")) +
  coord_cartesian(xlim = c(2007, 2021)) + #ylim = c(, )
  scale_x_continuous(breaks = round(seq(min(brck_acc$year), max(brck_acc$year), by = 1), 1)) +
  #scale_fill_manual(labels = c("")) +
  labs(
    x        = "Years",
    y        = "Percent %",
    title    = "Is FIFA's Accuracy Consistent?",
    subtitle = "Percent of correct predictions depending on the potential brackets of the players*.", 
    color    = "Potential Brackets",
    caption  = "Players from the years 2007-2011 (green background) have had 10 years, or more, of recorded overall.",
    alpha    = "",
    fill     = "") +
  theme(legend.position   = c(0.9, 0.9),
        legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"))
#guides(linetype = FALSE)

top_brack <- brck_acc %>% filter(accuracy_bracket == "accuracy_top")
ggplot(top_brack, aes(x = year, y = count)) +
  geom_point() +
  geom_text(aes(label = paste(count, "%", sep = ""), vjust = -0.5)) +
  theme_bw() +
  geom_line() +
  scale_x_continuous(breaks = round(seq(min(brck_acc$year), max(brck_acc$year), by = 1), 1)) +
  labs(
    x        = "Years",
    y        = "Percent %",
    title    = "EA FIFA's Accuracy For Future Stars",
    subtitle = "Percent of correct predictions of the [84, 94] potential bracket.")
#color    = "Potential Brackets",
#caption  = "Players from the years 2007-2011 (green background) have had 10 years, or more, of recorded overall."
#alpha    = "",
#fill     = "")

### 5. Frequency Distributions
freq_dist <- acc_preds %>%
  mutate(max_pot_category = case_when(
    pot19 >= range_max_pot[1] & pot19 < range_1 
    ~ paste("[", range_max_pot[1], ", ", range_1, "[", sep = ""), 
    pot19 >= range_1 & pot19 < range_2 
    ~ paste("[", range_1, ", ", range_2, "[", sep = ""), 
    pot19 >= range_2 & pot19 < range_max_pot[2] 
    ~ paste("[", range_2, ", ", range_max_pot[2], "]", sep = ""))) %>%
  drop_na(max_pot_category) 

## Oveerall Distribution
p1 <- ggplot(freq_dist, aes(overall, fill = as.factor(FifaIndex))) + 
  geom_histogram(binwidth = 1, bins = length(unique(freq_dist$overall, na.rm = TRUE))) +
  labs(
    title    = "Frequency Distribution of Players' Overall Scores at Age 19",
    x        = "Overall",
    y        = "") +
  scale_fill_viridis(discrete = TRUE, name = "Potential Brackets", labels = pot_brackets) +
  scale_x_continuous(breaks = round(seq(min(freq_dist$overall), max(freq_dist$overall), by = 1), 1)) +
  guides(fill = FALSE) +
  theme_classic()

## Potential Distribution
p2 <- ggplot(freq_dist, aes(pot19, fill = as.factor(FifaIndex))) + 
  geom_histogram(binwidth = 1, bins = length(unique(freq_dist$pot19, na.rm = TRUE))) +
  labs(
    title    = "Frequency Distribution of Players' Potential Scores at Age 19",
    x        = "Potential",
    y        = "") +
  scale_fill_viridis(discrete = TRUE, name = "Year", labels = year_labs) +
  scale_x_continuous(breaks = round(seq(min(freq_dist$pot19), max(freq_dist$pot19), by = 1), 1)) +
  theme_classic()

ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")


### 5.1 Countries producing the most overachivers - Barchart
acc_preds %>%
  filter(passed == 1) %>%
  drop_na(nationality) %>%
  rename(country = nationality) %>%
  group_by(country) %>%
  count() %>%
  mutate(code = if(country == "Northern Ireland" | country == "England" | country == "Scotland" | country == "Wales") {"gb"} 
         else {tolower(countrycode(country, origin = 'country.name', destination = 'iso2c'))},
         country = country) %>%
  filter(n >= 4) %>%
  ggplot(aes(x = reorder(country, n), y = n, country = as.factor(tolower(country)))) +
  geom_bar(stat = "identity", aes(fill = n)) +
  geom_flag(y = -1, aes(country = as.factor(code))) +
  #scale_y_continuous(expand = c(0.1, 1)) +
  labs(
    x        = "",
    y        = "",
    title    = "Nationalities of Our Overachievers",
    fill     = "",
    subtitle = paste(total_overachivers_1 + total_overachivers_2, " out of ", total_players, 
                     " players reached their potential. This plot takes a look at the countries with the highest number of potential achievers.",
                     sep = "")) +
  geom_text(aes(label = n, alpha = 0.9), size = 3.5, position = position_dodge(width = 0), hjust = -0.1, vjust = 0.5) +
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_gradient2(low = 'white', high = '#005EFF') +
  scale_country() +
  #scale_size(range = c(0, 15)) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none") +
  coord_flip()

### 5.2 Clubs producing the most overachievers - Barchart
p1 <- table1 %>%
  #filter(passed == 1) %>%
  filter(prime_num_1 == 1) %>%
  drop_na(prime_num_1) %>%
  select(club) %>%
  group_by(club) %>%
  count(sort = TRUE) %>%
  #arrange(n, decreasing = TRUE) %>% 
  ungroup() %>%
  top_n(50) %>%
  filter(n >= 13) %>%
  ggplot(aes(x = reorder(club, n), y = n, club = as.factor(tolower(club)))) +
  geom_bar(stat = "identity", aes(fill = n)) +
  #geom_flag(y = -1, aes(club = as.factor(code))) +
  #scale_y_continuous(expand = c(0.1, 1)) +
  labs(
    x        = "", 
    y        = "Number of Players",
    title    = "Clubs Distribution of Players' Prime",
    fill     = "",
    subtitle = "The top clubs where players have reached their prime in.") +
  geom_text(aes(label = n, alpha = 0.9), size = 3.5, position = position_dodge(width = 0), hjust = 0.5, vjust = -0.5) +
  #scale_fill_viridis(option = "magma") +
  scale_fill_gradient2(low = '#AEAEAE', high = '#00A300') +
  scale_country() +
  ylim(0, 39) +
  #scale_size(range = c(0, 15)) +
  theme_bw() +
  #coord_flip() +
  theme(
    legend.title = element_blank(), legend.position = "none",
    axis.text.x  = element_text(angle = 60, hjust = 1),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

p2 <- table1 %>%
  filter(pass_pot == TRUE) %>%
  filter(prime_num_1 == 1) %>%
  drop_na(prime_num_1) %>%
  drop_na(pass_pot_num_1) %>%
  select(club) %>%
  group_by(club) %>%
  count(sort = TRUE) %>%
  #arrange(n, decreasing = TRUE) %>% 
  ungroup() %>%
  top_n(50) %>%
  filter(n >= 5) %>%
  ggplot(aes(x = reorder(club, n), y = n, club = as.factor(tolower(club)))) +
  geom_bar(stat = "identity", aes(fill = n)) +
  #geom_flag(y = -1, aes(club = as.factor(code))) +
  #scale_y_continuous(expand = c(0.1, 1)) +
  labs(
    x        = "", 
    y        = "Number of Players",
    title    = "",
    fill     = "",
    subtitle = "Top clubs where players, who passed their potential, have achieved their prime in.") +
  geom_text(aes(label = n, alpha = 0.9), size = 3.5, position = position_dodge(width = 0), hjust = 0.5, vjust = -0.5) +
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_gradient2(low = '#AEAEAE', high = '#00A300') +
  ylim(0, 21) +
  scale_country() +
  #scale_size(range = c(0, 15)) +
  theme_bw() +
  #coord_flip() +
  theme(
    legend.title     = element_blank(), legend.position = "none",
    axis.text.x      = element_text(angle = 60, hjust = 1),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = FALSE, legend = "none")

### 6 Age Distribution of Potential Achievers
# Editing breakthrough_pot_1
{
  table1 <- table1 %>%
    select(-breakthrough_pot_1) %>%
    mutate(breakthrough_pot_1 = case_when(
      pass_pot_num_1 == 1 ~ as.character(max_potential),
      pass_pot_num_1 == 0 ~ as.character(0)
    ),
    prime_1 = case_when(
      prime_num_1 == 1 ~ as.character(prime),
      prime_num_1 == 0 ~ as.character(0)
    ))
  
  age_plots <- table1 %>%
    mutate(max_pot_category = case_when(
      max_potential >= range_max_pot[1] & max_potential < range_1 ~ 
        paste("[", range_max_pot[1], ", ", range_1,          "[", sep = ""), 
      max_potential >= range_1 & max_potential < range_2          ~ 
        paste("[", range_1,          ", ", range_2,          "[", sep = ""), 
      max_potential >= range_2 & max_potential < range_max_pot[2] ~ 
        paste("[", range_2,          ", ", range_max_pot[2], "]", sep = ""))) %>%
    group_by(age, max_pot_category) %>%
    summarise(pass_pot_num_sum_1 = sum(pass_pot_num_1),
              prime_sum_1        = sum(prime_num_1)) %>%
    filter(!is.na(max_pot_category))  
  # Add these extra lines to turn the potential brackets into columns
  #count(age, pass_pot_num_sum_1, max_pot_category) %>%
  #spread(max_pot_category, pass_pot_num_sum_1, fill = 0) %>%
  #select(-n)
  
  # Reordering stacked groups for potential19
  age_pot_pass_plot <- age_plots
  age_pot_pass_plot$max_pot_category <- reorder(
    age_pot_pass_plot$max_pot_category, 
    age_pot_pass_plot$pass_pot_num_sum_1)
  age_pot_pass_plot$max_pot_category <- factor(
    age_pot_pass_plot$max_pot_category, 
    levels = levels(age_pot_pass_plot$max_pot_category))
  # Reordering stacked groups for prime
  age_prime_plot <- age_plots
  age_prime_plot$max_pot_category <- reorder(
    age_prime_plot$max_pot_category, 
    age_prime_plot$prime_sum_1)
  age_prime_plot$max_pot_category <- factor(
    age_prime_plot$max_pot_category, 
    levels = levels(age_prime_plot$max_pot_category))
  
  # Creating dfs for the lines on top of the stacked bar chart
  line_pot_df <- age_pot_pass_plot %>%
    group_by(age) %>%
    summarize(pot_sum_line = sum(pass_pot_num_sum_1))
  line_prime_df <- age_prime_plot %>%
    group_by(age) %>%
    summarize(prime_sum_line = sum(prime_sum_1))
  
  # Small multiple
  p1 <- ggplot(age_pot_pass_plot) +
    geom_bar(aes(x = age, y = pass_pot_num_sum_1, fill = max_pot_category), position = "stack", stat = "identity") +
    #scale_fill_viridis(discrete = TRUE) +
    labs(
      title    = "At which age are players reaching their potential?",
      subtitle = paste((total_overachivers_1 + total_overachivers_2), 
                       " players reached their FIFA potential (of age 19) in the following age distribution.", 
                       sep = ""),
      x        = "Age",
      y        = "",
      fill     = "Potential Brackets") +
    theme_bw() +
    ylim(0, 160) +
    geom_line(data  = line_pot_df, aes(x = age, y = pot_sum_line), size  = 1, alpha = 0.3, linetype = "dashed") +
    geom_point(data = line_pot_df, aes(x = age, y = pot_sum_line), alpha = 0.5) +
    geom_text(data  = line_pot_df, aes(x = age, y = pot_sum_line,  label = pot_sum_line), 
              alpha = 0.5, size = 4, hjust = 0.5, vjust = -0.5)
  
  # Small multiple
  p2 <- ggplot(age_prime_plot) +
    geom_bar(aes(x = age, y = prime_sum_1, fill = max_pot_category), position = "stack", stat = "identity") +
    #scale_fill_viridis(discrete = TRUE) +
    labs(
      title    = "At which age are players reaching their prime?",
      subtitle = paste(sum(age_prime_plot$prime_sum_1), 
                       " players reached their prime in the following age distribution.", 
                       sep = ""), 
      x        = "Age",
      y        = "",
      fill     = "Potential Brackets") +
    theme_bw() +
    ylim(0, 924) +
    geom_line(data  = line_prime_df, aes(x = age, y = prime_sum_line), size  = 1, alpha = 0.3, linetype = "dashed") +
    geom_point(data = line_prime_df, aes(x = age, y = prime_sum_line), alpha = 0.5) +
    geom_text(data  = line_prime_df, aes(x = age, y = prime_sum_line,  label = prime_sum_line), 
              alpha = 0.5, size = 4, hjust = 0.5, vjust = -0.5)
  
}
ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")

age_pot_pass_high_brck_df <- age_pot_pass_plot %>%
  filter(max_pot_category == "[84, 94]")

age_prime_high_brck_df <- age_prime_plot %>%
  filter(max_pot_category == "[84, 94]")

line_pot_high_brck_df <- age_pot_pass_high_brck_df %>%
  group_by(age) %>%
  summarize(pot_sum_line = sum(pass_pot_num_sum_1))

line_prime_high_brck_df <- age_prime_high_brck_df %>%
  group_by(age) %>%
  summarize(prime_sum_line = sum(prime_sum_1))

p1 <- ggplot(age_pot_pass_high_brck_df) +
  geom_bar(aes(x = age, y = pass_pot_num_sum_1, fill = max_pot_category), position = "stack", stat = "identity") +
  #scale_fill_viridis(discrete = TRUE) +
  labs(
    title    = "At which age are players reaching their potential?",
    subtitle = paste(sum(age_pot_pass_high_brck_df$pass_pot_num_sum_1), 
                     " players of the [84, 94] potential bracket reached their FIFA potential (of age 19) in the following age distribution.", sep = ""),
    x        = "Age",
    y        = "",
    fill     = "Potential Brackets") +
  theme_bw() +
  ylim(0, 10) +
  geom_line(data  = line_pot_high_brck_df, aes(x = age, y = pot_sum_line), size  = 1, alpha = 0.3, linetype = "dashed") +
  geom_point(data = line_pot_high_brck_df, aes(x = age, y = pot_sum_line), alpha = 0.5) +
  geom_text(data  = line_pot_high_brck_df, aes(x = age, y = pot_sum_line,  label = pot_sum_line), 
            alpha = 0.5, size = 4, hjust = 0.5, vjust = -0.5)


p2 <- ggplot(age_prime_high_brck_df) +
  geom_bar(aes(x = age, y = prime_sum_1, fill = max_pot_category), position = "stack", stat = "identity") +
  #scale_fill_viridis(discrete = TRUE) +
  labs(
    title    = "At which age are players reaching their prime?",
    subtitle = paste(sum(age_prime_high_brck_df$prime_sum_1), 
                     " players of the [84, 94] potential bracket reached their prime in the following age distribution.", sep = ""), 
    x        = "Age",
    y        = "",
    fill     = "Potential Brackets") +
  theme_bw() +
  ylim(0, 100) +
  geom_line(data  = line_prime_high_brck_df, aes(x = age, y = prime_sum_line), size  = 1, alpha = 0.3, linetype = "dashed") +
  geom_point(data = line_prime_high_brck_df, aes(x = age, y = prime_sum_line), alpha = 0.5) +
  geom_text(data  = line_prime_high_brck_df, aes(x = age, y = prime_sum_line,  label = prime_sum_line), 
            alpha = 0.5, size = 4, hjust = 0.5, vjust = -0.5)

ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")

### 7.1 Insights: The Top 1: Do they always make it? 
{
  top1 <- table1 %>%
    filter(sofifa_page == 1, pos_in_list == 1) %>%
    rename(pot19 = max_potential) %>%
    mutate(alpha = case_when(overall >= pot19 ~ overall))
  
  top1 <- top1 %>% 
    mutate(beta  = case_when(overall <= pot19 ~ overall))
  
  top1 <- top1 %>%
    mutate(alpha_1 <- case_when(pass_pot_num_1 == 1 ~ overall))
  
  top1 <- top1 %>%
    rename(alpha_1 = "alpha_1 <- case_when(pass_pot_num_1 == 1 ~ overall)") %>%
    mutate(name_age  = paste(short_name, ", Age: ", age, "\nPotential: ", pot19, "\nOverall: ", overall, sep = "")) %>%
    group_by(short_name) 
  
  data_ends <- top1 %>% 
    group_by(short_name) %>%
    drop_na(overall) %>% 
    summarise_all(last) %>%
    select(FifaIndex, age, overall, name_age)
  
  # anim <-
  top1_above_ten <- top1 %>%
    filter(FifaIndex <= 11) 
  
  data_ends_below_ten <- data_ends %>%
    filter(FifaIndex <= 11)
  
  top1_below_ten <- top1 %>%
    filter(FifaIndex > 11) 
  
  data_ends_above_ten <- data_ends %>%
    filter(FifaIndex > 11)
}

ggplot(top1_above_ten) +
  theme_bw() +
  ## Point of Potential breakthough
  #geom_point(aes(x = age, y = alpha_1, color = "passeds"), 
  #           alpha = 0.01, size = 0.1, data = (top1_above_ten %>% filter(pass_pot == TRUE))) +
  geom_point(aes(x = age, y = alpha_1, color = "gold", shape = "★"), size = 2.5) +
  geom_label(aes(x = age, y = alpha_1, color = short_name, label = pot19), 
             hjust = 0, nudge_x = -0.4, size = 5, check_overlap = TRUE) +
  ## Lines of general overall
  geom_line(aes(x  = age, y = overall, color = short_name, linetype = "dotted"), size = 0.7) +
  geom_point(aes(x = age, y = overall, color = short_name)) +
  ## Lines of overall after breaking their potnetial point, if they ever do
  geom_line(aes(x  = age, y = alpha,   color = short_name, linetype = "solid"), size = 1) +
  geom_label_repel(data_ends_below_ten, colour = "white", fontface = "bold", max.overlaps = 4,
                   mapping = aes(x = age, y = overall, label = name_age, fill = name_age)) +
  geom_abline(intercept = 90, slope = 0, size = 0.5, alpha = 0.3, color = "red", linetype = "dashed") +
  geom_text(aes(x = age, y = alpha_1, label = "★"), size = 7, color = "gold", family = "HiraKakuPro-W3") +
  labs(
    title = "**The Top 1: Do they always make it?**",
    x = "Age", y = "Overall", color = "", 
    subtitle = paste("The players with the highest potential of every year: 200", 
                     min(top1_above_ten$FifaIndex), " to 20", max(top1_above_ten$FifaIndex), 
                     ".\n<span style='font-size:11pt'>
                    Crossing the <span style='color:#F2ABAB;'>dashed line</span> means the player has reached world class performance. Morever, players are passing their potential at the <span style='color:#FFD700;'>★</span> symbol.</span>", 
                     sep = "")
  ) + 
  guides(fill = FALSE, color = FALSE, shape = FALSE) + # 
  #geom_text(aes(colour = "white")) +
  #scale_colour_discrete(l = 40) +
  scale_x_continuous(breaks = c(19:35), expand = c(0, 0), limits = c(19, as.integer(max(as.integer(top1_above_ten$age), 
                                                                                        na.rm = TRUE) + 1))) +
  scale_y_continuous(expand = c(0, 0), limits = c(as.integer(min(as.integer(top1_above_ten$overall), 
                                                                 na.rm = TRUE) + 1), 
                                                  as.integer(max(as.integer(top1_above_ten$overall), 
                                                                 na.rm = TRUE) + 1))) +
  scale_linetype_identity(name = "", guide = 'legend', labels = c('Under potential\n\nAt and above potential')) +
  #scale_color_viridis(discrete = TRUE) +
  theme(
    plot.title           = element_markdown(lineheight = 1.1),
    plot.subtitle        = element_markdown(lineheight = 1.1),
    legend.text          = element_markdown(size = 12),
    legend.position      = c(0.84, 0.63),
    legend.justification = c(0.1, 0.1),
    legend.background    = element_rect(fill = "white", colour = "grey"))

ggplot(top1_below_ten) +
  theme_bw() +
  ## Point of Potential breakthough
  #geom_point(aes(x = age, y = alpha_1, color = "passeds"), 
  #           alpha = 0.01, size = 0.1, data = (top1_below_ten %>% filter(pass_pot == TRUE))) +
  geom_point(aes(x = age, y = alpha_1, color = "gold", shape = "★"), size = 2.5) +
  geom_label(aes(x = age, y = alpha_1, color = short_name, label = pot19), 
             hjust = 0, nudge_x = -0.4, size = 5, check_overlap = TRUE) +
  ## Lines of general overall
  geom_line(aes(x  = age, y = overall, color = short_name, linetype = "dotted"), size = 0.7) +
  geom_point(aes(x = age, y = overall, color = short_name)) +
  ## Lines of overall after breaking their potnetial point, if they ever do
  geom_line(aes(x  = age, y = alpha,   color = short_name, linetype = "solid"), size = 1) +
  geom_label_repel(data_ends_above_ten, colour = "white", fontface = "bold", max.overlaps = 4,
                   mapping = aes(x = age, y = overall, label = name_age, fill = name_age)) +
  geom_abline(intercept = 90, slope = 0, size = 0.5, alpha = 0.3, color = "red", linetype = "dashed") +
  geom_text(aes(x = age, y = alpha_1, label = "★"), size = 7, color = "gold", family = "HiraKakuPro-W3") +
  labs(
    title = "**The Top 1: Do they always make it?**",
    x = "Age", y = "Overall", color = "", 
    subtitle = paste("The players with the highest potential of every year: 20", 
                     min(top1_below_ten$FifaIndex), " to 20", max(top1_below_ten$FifaIndex), 
                     ".\n<span style='font-size:11pt'>
                    Crossing the <span style='color:#F2ABAB;'>dashed line</span> means the player has reached world class performance. Morever, players are passing their potential at the <span style='color:#FFD700;'>★</span> symbol.</span>", 
                     sep = "")
  ) + 
  guides(fill = FALSE, color = FALSE, shape = FALSE) + # 
  #geom_text(aes(colour = "white")) +
  #scale_colour_discrete(l = 40) +
  scale_x_continuous(breaks = c(19:35), expand = c(0, 0), limits = c(19, as.integer(max(as.integer(top1_below_ten$age), 
                                                                                        na.rm = TRUE) + 1))) +
  scale_y_continuous(expand = c(0, 0), limits = c(as.integer(min(as.integer(top1_below_ten$overall), 
                                                                 na.rm = TRUE) + 1), 
                                                  as.integer(max(as.integer(top1_below_ten$overall), 
                                                                 na.rm = TRUE) + 1))) +
  scale_linetype_identity(name = "", guide = 'legend', labels = c('Under potential\n\nAt and above potential')) +
  #scale_color_viridis(discrete = TRUE) +
  theme(
    plot.title           = element_markdown(lineheight = 1.1),
    plot.subtitle        = element_markdown(lineheight = 1.1),
    legend.text          = element_markdown(size = 12),
    legend.position      = c(0.84, 0.63),
    legend.justification = c(0.1, 0.1),
    legend.background    = element_rect(fill = "white", colour = "grey"))

#ggarrange(p1, p2, ncol = 1, nrow = 2)

#transition_reveal(age)

# Save at gif:
#animate(anim, duration = 6, fps = 20, width = 500, height = 700, renderer = gifski_renderer())

### 7.2 Insights: Top 25 highest jumps in Overall - Segment Plot
top25_seg <- acc_preds %>%
  mutate(real_change = prime - overall) %>%
  filter(real_change > 0) %>%
  top_n(25) %>%
  select(SoFifaID, short_name, overall, prime, pot19, real_change) %>%
  # Reorder data using average
  rowwise() %>%
  mutate(mean = mean(c(overall, prime) )) %>% 
  arrange(desc(real_change)) %>%
  mutate(short_name = factor(short_name, short_name))

ggplot(top25_seg) +
  geom_segment(aes(x = reorder(short_name, real_change), y = overall, xend = short_name, yend = prime), color = "grey") +
  geom_point(aes(x   = short_name, y = overall, color = "Overall at age 19"), size  = 3) +
  geom_point(aes(x   = short_name, y = prime,   color = "Prime"),             size  = 3) +
  geom_point(aes(x   = short_name, y = pot19,   color = "Potential"),         alpha = 0.75) +
  coord_flip()+
  theme_bw() +
  theme(legend.position = "top") +
  #scale_x_discrete(limits = c(as.integer(max(as.integer(top25_seg$overall), na.rm = TRUE) + 1), 
  #                              as.integer(max(as.integer(top25_seg$overall), na.rm = TRUE) + 1))) +
  scale_color_viridis(discrete = TRUE) +
  xlab("") +
  ylab("Overall") +
  ggtitle("Top 25: Players' Overall Progress") +
  labs(color = "", fill = "f", y = "", subtitle = "These players had the highest positive change in their overall") +
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

### 7.3 Skip: Insights: Top 25 higest jumps in Overall - Ridgeline
top25_ridge <- top25_seg %>%
  left_join(table1, by = c("SoFifaID" = "SoFifaID", "prime" = "prime", "overall" = "overall", 
                           "short_name" = "short_name")) %>%
  select(short_name, overall, prime, pot19, real_change)  %>%
  # Reorder data using average
  rowwise() %>%
  arrange(desc(real_change))
ggplot() +
  geom_segment(aes(y = reorder(short_name, real_change), x = overall, xend = short_name, yend = prime), color = "grey") +
  geom_point(aes(x   = short_name, y = overall, color = "Overall at age 19"), size = 3) +
  geom_point(aes(x   = short_name, y = prime,   color = "Prime"),  size = 3) +
  geom_point(aes(x   = short_name, y = pot19,   color = "Potential"),  alpha = 0.75) +
  coord_flip()+
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_viridis(discrete = TRUE) +
  xlab("") +
  ylab("Overall") +
  ggtitle("Top 25: Players' Overall Progress") +
  labs(color = "", fill = "f", y = "", subtitle = "These players had the highest positive change in their overall.") +
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggplot(top25_ridge, aes(x = overall, y = reorder(short_name, real_change))) + #, fill = ..x..
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  #geom_density_ridges(alpha = 0.6, stat = "binline", bins = 20) +
  geom_area(fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  theme_ridges()
scale_fill_viridis(name = "Temp. [F]", option = "C") 
#scale_color_viridis(discrete = TRUE, option = "C") +
ggtitle("Top 25: Players' Overall Progress") +
  labs(color = "", fill = "", y = "", subtitle = "These players had the highest positive change in their overall.") +
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.spacing   = unit(0.1, "lines"),
    strip.text.x    = element_text(size = 8),
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

### 7.4 Insights: Top 25 steepest declines in Overall - Segment Plot
steep25 <- table1 %>%
  group_by(SoFifaID) %>%
  drop_na(overall) %>% 
  summarise_all(last) %>%
  select(SoFifaID, overall) %>%
  rename(last_overall = overall) %>%
  inner_join(table1, by = c("SoFifaID" = "SoFifaID")) %>%
  mutate(drop = prime - last_overall) %>%
  drop_na(overall) %>%
  filter(age == 19) %>%
  select(SoFifaID, short_name, overall, prime, max_potential, drop, bottom, nationality) %>%
  # Reorder data using average
  mutate(mean = as.integer(mean(c(prime, bottom)))) %>%
  arrange(desc(drop)) %>%
  rowwise() %>%
  head(25)

ggplot(steep25) +
  geom_segment(aes(x = reorder(short_name, drop), y = prime, xend = short_name, yend = bottom), color = "grey") +
  geom_point(aes(x   = short_name, y = prime,           color = "Prime"),  size = 3) +
  geom_point(aes(x   = short_name, y = bottom,          color = "Last recorded overall*"),              size = 3) +
  geom_point(aes(x   = short_name, y = max_potential,   color = "Potential"),         alpha = 0.75) +
  coord_flip()+
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_viridis(discrete = TRUE) +
  xlab("") +
  ylab("Overall") +
  ggtitle("Bottom 25: Players' Overall Progress") +
  labs(color = "", fill = "f", y = "", subtitle = "These players had the highest negative change in their overall.",
       caption = paste("Last recorded overall as of ", Sys.Date(), sep = "")) +
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

### 8. Players' Profiles
var = "K. Benzema"
var = "L. Deaux"
var = "L. Messi"
var = "L. Suárez"
var = "De Marcos"
var = "V. van Dijk"
var = "Neymar"
var = "J. Mikel"
var = "F. Fabregas"

{
  ### Execute code chunk
  players_list <- as.data.frame(rbind(
    acc_preds %>% filter(difference < 0) %>%                 select(SoFifaID, short_name),     #Potential < Overall?
    top25_seg %>%                                            select(SoFifaID, short_name),     #Top 25 highest jumps in Overall
    steep25   %>%                                            select(SoFifaID, short_name),     #Top 25 steepest falls in overall
    top1      %>% filter(age == 19) %>%                      select(SoFifaID, short_name),     #The Top 1
    table1    %>% filter(prime_num_1 == 1,    age >= 32) %>% select(SoFifaID, short_name),     #Achieved prime in 30s
    table1    %>% filter(pass_pot_num_1 == 1, age >= 32) %>% select(SoFifaID, short_name)) %>% #Achieved potential in 30s
      distinct())
  
  stats_df <- read_csv(find_data_file("tm_stats.csv"), 
                       col_types  = cols(
                         FifaIndex    = col_integer(), 
                         games_played = col_integer(), 
                         goals        = col_integer(), 
                         assists      = col_integer(), 
                         minutes      = col_integer()),
                       show_col_types = FALSE)
  #trophies_df <- read_csv(find_data_file("tm_trophies.csv"), 
  #    col_types = cols(
  #      team_trophy       = col_integer(), 
  #      individual_trophy = col_integer(), 
  #      year              = col_integer()),
  #    show_col_types = FALSE)
  
  #columns            = c("player_id", "short_name", "text_plot", "trophies_bar", "stats_line", "overall_plot") 
  #plots_df           = data.frame(matrix(nrow = nrow(players_list), ncol = length(columns))) 
  #colnames(plots_df) = columns
  
  #for (row in 1:nrow(players_list)) {
  #if(identical(player_stats$FifaIndex, integer(0))) next
  
  # Cleaning the trophies dataframe
  #player_trophies <- trophies_df %>%
  #  filter(short_name == var) %>%
  #filter(short_name == players_list[row, "short_name"]) %>%
  #  group_by(short_name, year) %>%
  #  summarise(team_trophies = sum(team_trophy,             na.rm = TRUE),
  #            individual_trophies = sum(individual_trophy, na.rm = TRUE)) %>%
  #  rename(FifaIndex =  year) %>%
  #  filter(FifaIndex >= 7)
  
  #player_individual_trophies = sum(player_trophies$individual_trophies, na.rm = TRUE)
  #player_team_trophies       = sum(player_trophies$team_trophies,       na.rm = TRUE)
  
  # Cleaning the stats dataframe
  player_stats <- stats_df %>%
    filter(short_name == var) %>%
    #filter(short_name == players_list[row, "short_name"]) %>%
    select(-club, competition) %>%
    group_by(FifaIndex) %>%
    summarise(games   = sum(games_played, na.rm = TRUE),
              goals   = sum(goals,        na.rm = TRUE),
              assists = sum(assists,      na.rm = TRUE),
              minutes = sum(minutes,      na.rm = TRUE)) %>%
    filter(FifaIndex >= 7)
  
  player_games   = sum(player_stats$games,   na.rm = TRUE)
  player_goals   = sum(player_stats$goals,   na.rm = TRUE)
  player_assists = sum(player_stats$assists, na.rm = TRUE)
  player_minutes = sum(player_stats$minutes, na.rm = TRUE)
  
  player <- player_stats 
  #%>%  inner_join(player_trophies, by = c("FifaIndex" = "FifaIndex"))
  
  player <- table1 %>%
    filter(short_name == var) %>%
    #filter(short_name == players_list[row, "short_name"]) %>%
    select(overall, short_name, long_name, Fifa_year, age, max_potential, nationality, club, 
           pass_pot_num_1, prime_num_1, breakthrough_pot_1, prime_1) %>%
    inner_join(player, by = c("Fifa_year" = "FifaIndex")) 
  #select(-overall.y, -age.y, -max_potential.y, -nationality.y, -club.y, -pass_pot_num_1.y, 
  #       -prime_num_1.y, -breakthrough_pot_1.y, -prime_1.y, -short_name.y) %>%
  #rename("age" = "age.x", "max_potential" = "max_potential.x", "nationality" = "nationality.x", 
  #       "club" = "club.x", "pass_pot_num_1" = "pass_pot_num_1.x", "prime_num_1" = "prime_num_1.x",
  #       "breakthrough_pot_1" = "breakthrough_pot_1.x", "prime_1" = "prime_1.x")
  
  #player <- player[, c("short_name", "long_name", "Fifa_year", "age", "overall", "nationality", "club", "max_potential", 
  #                     "pass_pot_num_1", "breakthrough_pot_1", "prime_num_1", "prime_1", "individual_trophies", 
  #                     "team_trophies", "games", "goals", "assists", "minutes")]
  
  player <- player[, c("short_name", "long_name", "Fifa_year", "age", "overall", "nationality", "club", "max_potential", 
                       "pass_pot_num_1", "breakthrough_pot_1", "prime_num_1", "prime_1", "games", "goals", "assists", "minutes")]
  
  player <- player %>%
    #rename(pot19   <- max_potential) %>%
    mutate(alpha   <- case_when(overall >= max_potential ~ overall),
           beta    <- case_when(overall <= max_potential ~ overall),
           alpha_1 <- case_when(pass_pot_num_1 == 1 ~ overall)) 
  
  names(player)[names(player) == "alpha_1 <- case_when(pass_pot_num_1 == 1 ~ overall)"]    <- "alpha_1"
  names(player)[names(player) == "alpha <- case_when(overall >= max_potential ~ overall)"] <- "alpha"
  names(player)[names(player) == "beta <- case_when(overall <= max_potential ~ overall)"]  <- "beta"
  
  player <- player %>%
    mutate(name_age = paste(short_name, ", Age: ", age, "\nPotential: ", max_potential, "\nOverall: ", overall, sep = ""))
  
  data_ends <- player %>% 
    drop_na(overall) %>% 
    summarise_all(last) %>%
    select(age, Fifa_year, overall, name_age)
  
  ## Data Viz
  # Potential, Prime, Overall
  overall_plot <- ggplot(player) +
    theme_bw() +
    ## Point of Potential breakthough
    geom_point(aes(x = Fifa_year, y = alpha_1, color = "gold", shape = "★"), size = 2.5) +
    geom_label(aes(x = Fifa_year, y = alpha_1, color = short_name, label = max_potential), 
               hjust = 0, nudge_x = -0.4, size = 5, check_overlap = TRUE) +
    ## Lines of general overall
    geom_line(aes(x = Fifa_year, y = overall, color = short_name, linetype = "dotted"), size = 0.7) +
    ## Lines of overall after breaking their potnetial point, if they ever do
    geom_line(aes(x  = Fifa_year, y = alpha, color = short_name, linetype = "solid"), size = 1) +
    geom_label_repel(data_ends, colour = "white", fontface = "bold", max.overlaps = 4,
                     mapping = aes(x = Fifa_year, y = overall, label = name_age, fill = name_age)) +
    geom_abline(intercept = ifelse(identical(player$max_potential, numeric(0)), 0, player$max_potential), 
                slope = 0, size = 0.5, alpha = 0.3, color = "red", linetype = "dashed") +
    geom_text(aes(x = Fifa_year, y = alpha_1, label = "★"), size = 7, color = "gold", family = "HiraKakuPro-W3") +
    labs(
      x = "Years", y = "Overall", color = "", 
      subtitle = paste("<span style='font-size:11pt'>
                      \nCrossing the <span style='color:#F2ABAB;'>dashed line</span> means the player has reached his potential. \nMorever, players are passing their potential at the <span style='color:#FFD700;'>★</span> symbol.</span>", 
                       sep = "")
    ) + 
    guides(fill = FALSE, color = FALSE, shape = FALSE) + # 
    #geom_text(aes(colour = "white")) +
    #scale_colour_discrete(l = 40) +
    #scale_x_continuous(breaks = c(7:21), expand = c(0, 0), 
    #                   limits = c(19, as.integer(max(as.integer(top1$age), na.rm = TRUE) + 1))) +
    #xlim(-100, 100) +
    #scale_y_continuous(expand = c(0, 0), 
    #                   limits = c(70, as.integer(max(as.integer(top1$overall), na.rm = TRUE) + 1))) +
    scale_linetype_identity(name = "", guide = 'legend', labels = c('Under potential\n\nAt and above potential')) +
    #scale_color_viridis(discrete = TRUE) +
    theme(
      plot.title           = element_markdown(lineheight = 1.1),
      plot.subtitle        = element_markdown(lineheight = 1.1),
      legend.text          = element_markdown(size = 12),
      legend.position      = c(0.04, 0.63),
      legend.justification = c(0.1, 0.1),
      legend.background    = element_rect(fill = "white", colour = "grey")) 
  
  #trophies_bar <- player %>%
  #  select(Fifa_year, individual_trophies, team_trophies) %>%
  #  gather(key = "trophies", value = "n", -Fifa_year) %>%
  #  group_by(Fifa_year) %>%
  #  mutate(Fifa_year = as.integer(Fifa_year) + 2000) %>%
  #  ggplot(aes(x = factor(Fifa_year), y = n, fill = trophies)) + 
  #  theme_bw() +
  #  labs(
  #    title = "Trophies",
  #    x = ""
  #  ) +
  #  geom_bar(stat = "identity", position = "dodge") + 
  #scale_fill_discrete(name = "Dose", labels = c("A", "B", "C"))
  #  scale_fill_brewer(palette = "Set1", name = "", labels = c("Individual Trophies", "Team Trophies")) +
  #  theme(
  #    legend.text          = element_markdown(size = 12),
  #    legend.position      = c(0.72, 0.7),
  #    legend.justification = c(0.1, 0.1),
  #    legend.background    = element_rect(fill = "white", colour = "grey")) 
  
  stats_line <- player %>%
    select(Fifa_year, games, assists, goals) %>%
    mutate(Fifa_year = as.integer(Fifa_year) + 2000) %>%
    gather(key = "stats", value = "n", -Fifa_year) %>%
    ggplot(aes(x = Fifa_year, y = n)) +
    geom_line(aes(color = stats, linetype = stats)) +
    scale_color_manual(values = c("darkred", "steelblue", "darkgreen"), labels = c("Assists", "Games", "Goals")) +
    theme_bw() +
    guides(linetype = FALSE) +
    labs(y = "", x = "", color = "") +
    theme(
      legend.text          = element_markdown(size = 12),
      legend.position      = c(0.82, 0.71),
      legend.justification = c(0.1, 0.1),
      legend.background    = element_rect(fill = "white", colour = "grey")) 
  
  text_plot <- ggplot(player) +
    geom_blank() +
    labs(
      title = paste(
        "Name: ",                      unique(player$long_name),                    "\n",
        "Team(s): ",                   paste(unique(player$club), collapse = ", "), "\n",
        "Nationality: ",               unique(player$nationality),                  "\n",
        "Potential at age 19: ",       max(player$max_potential),                   "\n",
        #"Achieved potential at age: ", player %>% filter(prime_num_1 == 1) %>% select(age),
        "Prime Overall: ",             ifelse(identical(max(player$prime_1), NA), "NA", max(player$prime_1)), "\n",
        #"Team Trophies: ",             player_team_trophies,           "\n",
        #"Individual Trophies: ",       player_individual_trophies,     "\n",
        "Games: ", player_games, " | Minutes: ", player_minutes, "\n",
        "Goals: ", player_goals, " | Assists: ", player_assists,
        sep = "") 
    ) +
    theme(text = element_text(size = 20))
  #identical(player_trophies$short_name, character(0))
  
  #plots_df[row, "player_id"]    <- row
  #plots_df[row, "short_name"]   <- players_list[row, "short_name"]
  #plots_df[row, "text_plot"]    <- text_plot
  #plots_df[row, "trophies_bar"] <- trophies_bar
  #plots_df[row, "trophies_bar"] <- NA
  #plots_df[row, "stats_line"]   <- stats_line
  #plots_df[row, "overall_plot"] <- overall_plot
  #}
}
#ggarrange(
#  ggarrange(text_plot, trophies_bar, stats_line, ncol = 3, nrow = 1),
#  overall_plot, ncol = 1, nrow = 2)
ggarrange(
  ggarrange(text_plot, stats_line, ncol = 2, nrow = 1),
  overall_plot, ncol = 1, nrow = 2)


### 9 What Is The Probability Of Making It? Heatmap X-Y-Z
scatter_df <- scatter_df %>%
  mutate(pass_pot_num = case_when(
    passed == FALSE ~ 0,
    passed == TRUE  ~ 1
  ))

heat_df_axis <- cbind(
  (acc_preds %>% 
     select(overall, pot19, passed) %>% 
     group_by(overall, pot19, passed) %>% 
     count() %>% 
     group_by(overall, pot19) %>% 
     summarize(prob = (round((passed / n), 2)) * 100))$prob, 
  (acc_preds %>% 
     select(overall, pot19, passed) %>% 
     group_by(overall, pot19, passed) %>% 
     count() %>% 
     group_by(overall, pot19))
) %>%
  rename(prob = ...1)
heat_df_axis <- heat_df_axis[, c("overall", "pot19", "prob", "passed", "n")]

# Heatmap 
ggplot(heat_df_axis, aes(x = overall, y = pot19, fill = prob)) + 
  geom_point(aes(x = overall, y = pot19), alpha = 0) +
  geom_tile() +
  labs(
    x = "Overall",
    y = "FIFA Potential",
    title = "Given your Fifa potential and overall score, what is the probability of reaching it?",
    fill = "Percentage %"
  ) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  theme_ipsum() 
#theme(legend.position = "bottom")
#ggMarginal(pl, type = "histogram", aes(fill = factor(head_df_axi$prob)), groupColour = TRUE, groupFill = TRUE)

### 9.1 What Is The Probability Of Making It? Pot19/Probs Barchart
pot19 <- sort(unique(scatter_df$pot19))
probs_bar_plot <- data.frame(pot = pot19, prob = NA)

for (i in min(pot19):(max(pot19))){
  probs_bar_plot[i - 63, 2] <- (scatter_df %>%
                                  filter(pot19 == i) %>%
                                  summarize(prob = round((sum(pass_pot_num) / n()) * 100), 2) )[1]
}

## Tidying
probs_bar_plot <- probs_bar_plot[complete.cases(probs_bar_plot), ]

ggplot(probs_bar_plot, aes(x = reorder(as.factor(pot), prob), y = prob)) + 
  geom_bar(stat = "identity", aes(fill = prob)) +
  #scale_x_discrete(limits = probs_bar_plot1$pot) +
  labs(
    x     = "FIFA Potential",
    y     = "Percent %",
    title = "Given you had your FIFA potential, what is the probability you actually reach it?",
    fill  = "sdsds"
  ) +
  scale_fill_gradient2(low = 'white', high = 'darkgreen') +
  geom_text(aes(label = paste(prob, "%", sep = ""), alpha = 0.9), 
            size = 3.5, position = position_dodge(width = 0), hjust = -0.1, vjust = 0.5) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") 

# ==============================================================================
# How Accurate Is FIFA At Predicting Future Stars?
# Historical Web Scraping Script (SoFIFA & Transfermarkt)
#
# Author: Amin Al-Ait
# Course: Sports Data Visualization Seminar, TU Dortmund University (Grade: 1.0)
# Grade: 1.0 (1/1)
# Published Article: https://kurt.digital/2022/05/11/footballs-future-stars-how-accurately-is-ea-fifa-predicting-them/
# Full Report PDF: https://github.com/AminAlAit/fifaaccuracy/blob/main/How%20Accurate%20Is%20FIFA%20At%20Predicting%20Future%20Stars.pdf
#
# Sampling Methodology:
# - Target age: exactly 19 years old (ael=19&aeh=19)
# - Ranked by potential descending (col=pt&sort=desc)
# - 60 players per page x 6 pages = 360 wonderkids per FIFA release
# - 15 releases: FIFA 07 to FIFA 21 (5,400 players total, 43,198 player-year records)
#
# Note: This is the original scraping pipeline executed in 2021 using RSelenium
# and rvest. Scraped datasets are preserved in data/fifatable.csv and data/tm_stats.csv.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(tidyverse)
  library(magrittr)
  library(purrr)
  library(dplyr)
  library(rvest)
  library(httr)
  library(qdapRegex)
})

# Optional audio notification and browser automation packages
if (requireNamespace("beepr", quietly = TRUE)) library(beepr)
if (requireNamespace("RSelenium", quietly = TRUE)) library(RSelenium)
if (requireNamespace("rJava", quietly = TRUE)) library(rJava)

### Looping variables and variables in loops
# year, page, general_link, player, specific_link, playeryear, player_url, player_id, player_link_name, player_year, player_loop_age
## Vars to be used inside the loop
#player_id = 0

# Creating necessary variables in addition to an empty table of 13 columns, and 100,000 rows
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# column numbers to be used as indexes when registering values to the dataframe
id      		<- 1
FifaIndex       <- 2
Fifa_year       <- 3
player_id_col   <- 4
sofifa_page     <- 5
pos_in_list     <- 6
SoFifaID        <- 7 
SoFifaName      <- 8

long_name       <- 9
short_name      <- 10

age             <- 11

nationality     <- 12
club            <- 13
position        <- 14
overall         <- 15
max_potential   <- 16

games_played    <- 17
points_per_game <- 18
goals           <- 19
assists         <- 20
minutes         <- 21

count           <- 0
rest_time       <- 0.15
cond            <- TRUE
trim			<- function (x) gsub("^\\s+|\\s+$", "", x)
path			<- "fifatable.csv" # if the working directory is set to Documents use path: "C:/Users/NEW/Desktop/fifatable.csv" 

#message(paste("Recovery number: ", count, " reached.", sep = ""))
#head(table)
#c("id", "FifaIndex", "Fifa_year", "player_id", "sofifa_page", "pos_in_list", "SoFifaID", "SoFifaName", 
#  "long_name", "short_name", "age", "nationality", "club", "position", "overall", 
#  "max_potential", "games_played", "points_per_game", "goals", "assists", "minutes")

# execute this only one time then comment it
{
	fifatable <- read.csv("~/fifatable.csv")
	table	  <- fifatable %>% select(-X)
	table <- rbind(table, setNames(data.frame(matrix(ncol = 21, nrow = 100000)), names(table)))
	write.csv(table, path)
}

tryCatch({
	suppressMessages({
		
		## Comment this section for the first attempt, but then use it for all other attempts
		{
		tryCatch({
			suppressMessages({
				# importing from Desktop
				#table <- read_csv(path, cols(X1 = col_skip(), 
				#										X1_2 = col_skip(), 
				#										X1_1 = col_skip(), 
				#										...1 = col_skip()))
				table <- read_csv("fifatable.csv", col_types = cols(...1 = col_skip()))
			})
		}, 
		error = function(e) {})
		}
		
		#view(table)
		beep(2)
		
		# Saving 
		write.csv(table, "fifatable.csv")
		beep(sound = 1)
		
		## Dataframe (csv) Recovery Options
		{#row 			<- max(table$id, na.rm = TRUE) - 1
		#row
		#row             <- match(max(table$player_id, na.rm = TRUE), table$player_id) #1
		#row
		#tempo_name		<- table[row, "long_name"]
		#tempo_name
		#year_min        <- table[row, FifaIndex][[1]] #7 
		#year_min
		#year_max        <- 21 #21
		#page_min        <- table[row, sofifa_page][[1]] #1
		#page_min
		#page_max        <- 6  #6
		#player_min      <- table[row, pos_in_list][[1]]  #1
		#player_min
		#player_max      <- 60  #60
		#career_year_max <- 21 #21 
		}
		
		
		Sys.sleep(rest_time)
		print("Step 0: Opening botted browser")
		################################## Same as before function
		## Opening website before hand
		# open server: open website
		rD <- rsDriver(port = floor(runif(1, 1, 9999)) %>% as.integer, browser = "firefox")
		remDr <- rD[["client"]]
		
		Sys.sleep(rest_time)
		print(paste("Step 1: Starting loop level 1: Starting year ", year_min, sep = ""))
		
		######## Level 1: looping over fifa years: 07 to 21 (each fifa year contains a different set of pages)
		for (year in year_min:year_max){
			
			Sys.sleep(rest_time)
			print(paste("Step 1.A: Starting loop level 2: Starting page ", page_min, sep = ""))
			
			######## Level 2: Looping over pages: 1 to 60 (each page contains 60 players)
			for (page in page_min:page_max){
				## Adding 0 to 7, 8, 9 => 07, 08, 09
				if(year < 10){
					general_link <- paste("https://sofifa.com/players?type=all&ael=19&aeh=19&r=0", year,"0001&set=true&col=pt&sort=desc&offset=", ((page - 1) * 60) , sep ="")
				} else{
					general_link <- paste("https://sofifa.com/players?type=all&ael=19&aeh=19&r=", year,"0001&set=true&col=pt&sort=desc&offset=",  ((page - 1) * 60) , sep ="")
				}                          
				
				Sys.sleep(rest_time)
				print(paste("Step 1.C: Starting loop level 3: Starting player ", player_min, sep = ""))
				
				######## Level 3: Looping over players: we have to click on each player 
				for (player in player_min:player_max){
					# follows a player's age in every loop
					#player_id = player_id + 1
					player_loop_age <- 19
					
					### The 3 scrapes: max_pot, player_nat and player_short_name will be later registered in the table dataframe inside the second loop
					### That is because we want these (same) values at every row of each player, and this loop can not give us this output. 
					### However, their extraction in THIS loop is necessary because I was not able to extract these 3 info from the player's individual page.
					### Scraping only worked at the general list page (general_link)
					
					## player short name
					player_short_name <- read_html(general_link) %>%
						html_nodes(paste('tr:nth-child(', player, ') .col-name .tooltip .bp3-text-overflow-ellipsis', sep = "")) %>%
						html_text(trim = TRUE)
					
					#Sys.sleep(rest_time)
					print(paste("Step 2.A: Extracing info on: ", player_short_name, sep = ""))
					
					## player potential
					# loop at child(X)
					max_pot <- as.numeric(
						read_html(general_link) %>%
							html_nodes(
								paste(
									'.list tr:nth-child(', player, ') .col-sort',
									sep = "")) %>%
							html_text(trim = TRUE)
					)
					
					#Sys.sleep(rest_time)
					print(paste("Step 2.B:", player_short_name, " | ", max_pot, sep = ""))
					
					## player nationally
					player_nat <- read_html(general_link) %>%
						html_nodes(xpath = paste('//tr[(((count(preceding-sibling::*) + 1) = ', player, ') and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "flag", " " ))]', sep = "")) %>%
						html_attr("title")
					
					if(length(list(player_nat)[[1]]) > 1){
						player_nat <- list(player_nat)[[1]][1]
					}
					
					# Sys.sleep(rest_time)
					print(paste("Step 3:", player_short_name, " | ", max_pot, " | ", player_nat, sep = ""))
					
					print(paste("Step 4.A: Entering ", player_short_name, "'s profile", sep = ""))
					
					#### Clicking on each individual player of the list
					# open website
					remDr$navigate(general_link)
					Sys.sleep(2)
					beep(5)
					
					## Click
					option <- remDr$findElement(using = "css selector", paste("tr:nth-child(", player, ") .col-name .tooltip .bp3-text-overflow-ellipsis", sep = ""))
					option$clickElement()
					
					#Sys.sleep(rest_time)
					print(paste("Step 4.B:", player_short_name, " | ", max_pot, sep = ""))
					
					## Getting player links (specific Link), player link ID, and player link name
					specific_link    <- remDr$getCurrentUrl() %>% unlist
					player_link_id   <- ex_between(specific_link, "https://sofifa.com/player/", "/") %>% unlist %>% as.numeric()
					player_link_name <- ex_between(specific_link, paste("https://sofifa.com/player/", player_link_id, "/", sep = ""), "/") %>% unlist
					player_year      <- ex_between(specific_link, paste("https://sofifa.com/player/", player_link_id, "/", player_link_name, "/", sep = ""), "/") %>% unlist %>% as.numeric()
					
					Sys.sleep(rest_time)
					print(paste("Step 5.A: Link name and ID have extracted from ", player_short_name, "'s profile URL", sep = ""))
					
					print(paste("Step 5.B: Starting loop 4: Starting year ", year, ", ", player_short_name, sep = ""))
					#remDr$refresh()
					## Loop 4: Looping over fifa years: "year" to 21 (each player has from "year" to 21 years of stats)
					for(playeryear in year:career_year_max){
						## Adding 0 to 7, 8, 9 => 07, 08, 09
						if(playeryear < 10){
							#                       https://sofifa.com/player/   153296           /   yoann-gourcuff     /0   8            0001/
							specific_link <- paste("https://sofifa.com/player/", player_link_id, "/", player_link_name, "/0", playeryear, "0001/", sep = "")
						} 
						else {
							#                       https://sofifa.com/player/   153296           /   yoann-gourcuff     /   15           0001/
							specific_link <- paste("https://sofifa.com/player/", player_link_id, "/", player_link_name, "/", playeryear, "0001/", sep = "")
						}
						
						#Sys.sleep(rest_time)
						print("Step 6: Specific link adjusted")
						
						# open the individual player's URL
						remDr$navigate(specific_link)
						beep(4)
						Sys.sleep(rest_time)
						print("Step 6.A: first")
						remDr$navigate(specific_link)
						
						#Sys.sleep(rest_time)
						print("Step 7: getting next page's url for comparison")
						
						# Getting link of the newly navigated page
						next_specific_link <- remDr$getCurrentUrl() %>% unlist
						
						# Checking if the player has not retired for this year by comparing URLs: if they are the same then the player has not retired / still available in FIFA of that year
						if(tolower(specific_link) == tolower(next_specific_link)){
							#remDr$refresh()
							# scrape name 
							# register long name, short name, nationality, age 
							
							#Sys.sleep(rest_time)
							print(paste("Step 8: player '", player_short_name, "' has stats in year", playeryear, sep = ""))
							
							##### Scraping and registering
							## player overall
							temp1 <- read_html(specific_link) %>%
								html_nodes('.spacing .col-3:nth-child(1) > div') %>%
								html_text()
							
							table[row, overall] <- as.numeric(substr(temp1, start = 1, stop = 2))
							
							#Sys.sleep(rest_time)
							print(paste("Step 9: year ", playeryear, ", ", player_short_name, " has overall: ", temp1, sep = ""))
							
							## player position
							temp2 <- read_html(specific_link) %>%
								html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "meta", " " ))]') %>%
								html_text(trim = TRUE)
							table[row, position] <- trim(substr(temp2, start = 1, stop = 3))
							
							#Sys.sleep(rest_time)
							print(paste("Step 10: year ", playeryear, ", ", player_short_name, " has position ", temp2, sep = ""))
							
							## club name
							#tempas1 <- (read_html(specific_link) %>%
							#					 	html_nodes('.col-3 a') %>%
							#					 	html_text(trim = TRUE))[1]
							
							table[row, club] <- (read_html(specific_link) %>%
												 	html_nodes('.col-3:nth-child(3) a') %>%
												 	html_text(trim = TRUE))[1]
							
							#tempas3 <- (read_html(specific_link) %>%
							#					 	html_nodes('.col-3:nth-child(4) a') %>%
							#					 	html_text(trim = TRUE))[1]
							
							Sys.sleep(rest_time)
							print(paste("Step 11: year ", playeryear, ", ", player_short_name, " has club ", table[row, club], sep = ""))
							
							## player long name
							table[row, long_name] <- read_html(specific_link) %>%
								html_nodes('div.bp3-card.player div.info h1') %>%
								html_text(trim = TRUE)
							
							Sys.sleep(rest_time)
							print(paste("Step 12.A: year ", playeryear, ", ", player_short_name, " has long name ", table[row, long_name], sep = ""))
							
							print("Step 12.B: Registering values to table")
							# Registering the same short_name, SoFifa's player's link name (lionel-messi), age, SoFifa's player's link ID, nationality, 
							# current carear year, max_potential, year and id (row) in every cycle for this player
							table[row, short_name]    <- player_short_name
							table[row, SoFifaName]    <- player_link_name
							table[row, age]           <- player_loop_age
							table[row, SoFifaID]      <- player_link_id
							table[row, nationality]   <- player_nat
							table[row, Fifa_year]	  <- playeryear
							table[row, max_potential] <- max_pot
							table[row, pos_in_list]   <- player
							table[row, sofifa_page]	  <- page
							table[row, FifaIndex]	  <- year
							table[row, id]			  <- row
							table[row, player_id_col] <- table[row, pos_in_list] + ((table[row, sofifa_page] - 1) * 60) + ((table[row, FifaIndex] - 7) * 360)
							
							# appending the index
							row = row + 1
						}
						# The player retired / not available
						else {
							#remDr$refresh()
							Sys.sleep(rest_time)
							print(paste("Step 12.A: player '", player_short_name, "' DOES NOT have stats in year", playeryear, sep = ""))
							print("Step 12.B: Registering values to table")
							
							# fill data frame cells with NAs
							table[row, overall]       <- NA
							table[row, position]      <- NA
							table[row, club]          <- NA
							table[row, long_name]     <- NA
							
							# fill with same values
							table[row, short_name]    <- player_short_name
							table[row, SoFifaName]    <- player_link_name
							table[row, age]           <- player_loop_age
							table[row, SoFifaID]      <- player_link_id
							table[row, nationality]   <- player_nat
							table[row, Fifa_year]	  <- playeryear
							table[row, max_potential] <- max_pot
							table[row, pos_in_list]   <- player
							table[row, sofifa_page]	  <- page
							table[row, FifaIndex]	  <- year
							table[row, id]			  <- row
							table[row, player_id_col] <- table[row, pos_in_list] + ((table[row, sofifa_page] - 1) * 60) + ((table[row, FifaIndex] - 7) * 360)
							
							# appending the index
							row = row + 1
						}
						
						# Resetting the loops variables to default: loop variables were edited so that the script can pick itself up from where it left off
						year_min        <- 7 
						year_max        <- 21
						page_min        <- 1
						page_max        <- 6
						player_min      <- 1
						player_max      <- 60 
						career_year_max <- 21 
						
						player_loop_age <- player_loop_age + 1
					}
					## WORK Extract real stats table
					# specific_link <- paste("https://sofifa.com/player/", player_link_id, "/", player_link_name, "/live", sep = "")
					
					#remDr$refresh()
					Sys.sleep(rest_time)
					print("End of Loop 4")
					
					# Saving 
					#write.csv(table, "fifatable.csv")
					write.csv(table, "fifatable.csv")
					beep(sound = 1)
					
				}
				#remDr$refresh()
				Sys.sleep(rest_time)
				print("End of Loop 3")
				
				# Saving 
				#write.csv(table, "fifatable.csv")
				write.csv(table, "fifatable.csv")
				beep(sound = 1)
				
			}
			#remDr$refresh()
			Sys.sleep(rest_time)
			print("End of Loop 2")
			
			# Saving 
			#write.csv(table, "fifatable.csv")
			write.csv(table, "fifatable.csv")
			beep(sound = 1)
			
		}
		#remDr$refresh()
		Sys.sleep(rest_time)
		print("End of Loop 1")
		Sys.sleep(rest_time)
		remDr$close()
		rD$server$stop() 
		rm(rD)
		gc()
		print(paste("Step 14 FINALE. Last player was: ", player_short_name, " | Year: ", playeryear, " | URL: ", specific_link, sep = ""))	
		
		count = count + 1
		print(paste("This is loop number: ", count, sep = ""))
		print(paste("Year: ", year, "/", year_max, ". Page: ", page, "/", page_max, ". Player: ", player, "/", player_max, ". PlayerYear: ", playeryear, "/", career_year_max, sep = ""))
		
		# Saving 
		#write.csv(table, "fifatable.csv")
		write.csv(table, "fifatable.csv")
		beep(sound = 1)
		
		
		print(paste("The while loop ended and did: ", count, " loops.", sep = ""))
		beep(sound = 10)
		beep(sound = 10)
		print(paste("Year: ", year, "/", year_max, ". Page: ", page, "/", page_max, ". Player: ", player, "/", player_max, ". PlayerYear: ", playeryear, "/", career_year_max, sep = ""))
		print(paste("Row: ", match(max(table$player_id, na.rm = TRUE), table$player_id), sep = "")) #1 #match(max(table$player_id, na.rm = TRUE), table$player_id)
		beep(sound = 1)
		
		# Saving 
		#write.csv(table, "fifatable.csv")
		write.csv(table, "fifatable.csv")
		beep(sound = 1)
		
	})
}, 
error = function(e) {
	print(paste("The while loop ended and did: ", count, " loops.", sep = ""))
	beep(sound = 10)
	beep(sound = 10)
	print(paste("Year: ", year, "/", year_max, ". Page: ", page, "/", page_max, ". Player: ", player, 
				"/", player_max, ". PlayerYear: ", playeryear, "/", career_year_max, sep = ""))
	print(paste("Row: ", match(max(table$player_id, na.rm = TRUE), table$player_id), sep = "")) #1 #match(max(table$player_id, na.rm = TRUE), table$player_id)
	beep(sound = 1)
	
	# Saving 
	#write.csv(table, "fifatable.csv")
	write.csv(table, "fifatable.csv")
	beep(sound = 1)
	
})



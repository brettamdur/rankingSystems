# File:			miscPlots.R
#	Author:		Brett Amdur
#	Project:	Measuring Ranking System Performance
# Notes:		Miscellaneous Plots


#### Finding Upsets ####
# generates plot of upset percentages in top 200 by year
combinedMatches <- mutate(combinedMatches, matchYear = substr(tourney_date, 1, 4)) # add a column for match year
ranksCombined <- combinedMatches[, c("winner_rank", "loser_rank", "winner_name", "loser_name", "matchYear")] # use this when just adding player names
ranksCombined <- mutate(ranksCombined, difference = loser_rank - winner_rank) # create difference column
ranksCombined <- filter(ranksCombined, winner_rank <= 200, loser_rank <= 200, matchYear >= 1983) # subset to matches of top 200 players
ranksCombined_1 <- mutate(ranksCombined, upsetTrue = difference < 0, upsetFalse = difference > 0)


	# plot pct of upsets by year
ranksCombined_1g <- group_by(ranksCombined_1, matchYear)
upsetPercents <- summarise(ranksCombined_1g, 
			   upsets = sum(upsetTrue), expecteds = sum(upsetFalse)) %>% 
		 mutate(., upsetPct = upsets / (upsets + expecteds)) # create upset % column
ggplot(upsetPercents, aes(x=matchYear, y = upsetPct)) + 
	geom_bar(stat="identity", fill = "cornflowerblue") + 
	xlab("Year") +
	ylab("Upset Percentage") +
	theme_classic() +
	theme(axis.title.x = element_text(size = 25), 
	      axis.title.y = element_text(size = 25),
	      axis.text.y = element_text(size = 15))

# combined men and women bar plot for upset percentage by year
ggplot(upsetPercents, aes(x=matchYear, y = upsetPct, fill = Gender)) + 
	geom_bar(stat="identity", position = "dodge") + 
	xlab("Year") +
	ylab("Upset Percentage") +
	theme_classic() +
	theme(axis.title.x = element_text(size = 25), 
	      axis.title.y = element_text(size = 25),
	      axis.text.y = element_text(size = 15))


# plot of average upset severity difference by year
upsetsOnly <- filter(ranksCombined_1g, upsetTrue == TRUE)
upsetDifByYear <- group_by(upsetsOnly, matchYear) %>% summarise(. , aveDiff = mean(difference))
# upsetDifByYear <- group_by(upsetsOnly, matchYear) %>% summarise(. , aveDiff = median(difference))
ggplot(upsetDifByYear, aes(x=as.numeric(matchYear), y = aveDiff)) + 
	# geom_bar(stat = "identity") 
	geom_point(size = 8, color = "cornflowerblue") +
	geom_smooth(method = lm) +
	theme_classic() +
	ylab("Loser Rank Minus Winner Rank") +
	xlab("Year") +
	theme(axis.title.x = element_text(size = 25), 
	      axis.title.y = element_text(size = 25),
	      axis.text.y = element_text(size = 15))
	

# plotting tournament type by year
x <- group_by(combinedMatches, 
	      ttype = as.factor(tourney_level), 
	      year = as.factor(matchYear)) %>% 
	summarise(., count = n())
ggplot(x, aes(year, count, color = ttype)) + 
	geom_line(aes(group=ttype), size = 1) +
	theme_classic() +
	ylab("Number of Matches") +
	xlab("Year") +
	title("Matches by Year and Tournament Type") +
	theme(axis.title.x = element_text(size = 25), 
	      axis.title.y = element_text(size = 25),
	      axis.text.y = element_text(size = 15))


#View of mean draw and count by year all type A tournaments
View(filter(combinedMatches, type = tourney_level == "A") %>% 
     	group_by(., tourney_name) %>% 
     	summarise(., avgDraw = mean(draw_size), n()))

# Draw and count by tourney type for players over rank 200
filter(combinedMatches, winner_rank = winner_rank <= 200, loser_rank <= 200) %>%
group_by(combinedMatches , tourney_level) %>% 
     	summarise(. , avgDraw = mean(draw_size), n())

# mean negative difference by type
combinedMatches_slice <- select(combinedMatches, tourney_level, 
				matchYear, 
				draw_size, 
				winner_rank,
				loser_rank)
filter(combinedMatches_slice , (! is.na(winner_rank)), (! is.na(loser_rank))) %>%
filter(. , winner_rank <= 200, loser_rank <= 200) %>%
mutate(. , difference = loser_rank - winner_rank) %>%
	filter(. , difference < 0) %>%
	group_by(.,  tourney_level) %>% 
	#select(., tourney_level, matchYear, draw_size, difference) %>%
	summarise(., avgDraw = mean(draw_size), 
		  count = n(), 
		  aveDiff = mean(difference)) %>%
	mutate(., pctTotal = count / nrow(combinedMatches_slice))

###############
#Ttype A diff by year
combinedMatches_slice <- select(combinedMatches, tourney_level, 
				matchYear, 
				draw_size, 
				winner_rank,
				loser_rank) %>%
	mutate(., difference = loser_rank - winner_rank)
x <- filter(combinedMatches_slice , (! is.na(winner_rank)), (! is.na(loser_rank))) %>%
	filter(. , winner_rank <= 200, loser_rank <= 200, tourney_level == "A") %>%
	group_by(. , matchYear) %>%
	summarise(. , aveDiff = mean(difference))

ggplot(x, aes(matchYear, aveDiff)) + geom_bar(stat = "identity")	
	



###
m_vs_w <- data.frame(category = c("men_rmse", 
				  "men_mad", 
				  "women_rmse", 
				  "women_mad"), 
			OneTo33 = c(men_1to33, w_1to33), 
			Oneto66 = c(men_1to66, w_1to66), 
			Oneto100 = c(men_1to100, w_1to100))
# category   OneTo33   Oneto66  Oneto100
# 1   m_rmse 0.2821630 0.2831247 0.2934435
# 2    m_mad 0.2451877 0.2418068 0.2402383
# 3   w_rmse 0.2612137 0.2636136 0.2908766
# 4    w_mad 0.2221296 0.2137522 0.2232933

# head of melted:
m_vs_w_melted <- melt(m_vs_w, id = "category")
# category variable     value
# 1   m_rmse  OneTo33 0.2821630
# 2    m_mad  OneTo33 0.2451877
# 3   w_rmse  OneTo33 0.2612137
# 4    w_mad  OneTo33 0.2221296
# 5   m_rmse  Oneto66 0.2831247
# 6    m_mad  Oneto66 0.2418068

names(m_vs_w_melted) <- c("GenderError", "Rank", "Error")
ggplot(m_vs_w_melted, aes(x = GenderError, y = Error, fill=Rank)) +
	geom_bar(position = "dodge", stat = "identity") +
	theme_classic() +
	xlab("Gender / Measurement") +
	ylab("Error") +
	theme(axis.title.x = element_text(size = 25), 
	      axis.title.y = element_text(size = 25),
	      axis.text = element_text(size = 15)) +
	scale_fill_brewer(palette = "Blues")
###############
# frequency heat map.  Change the 100 to get new map
# for different square ranking segment.
act100 <- actuals[[2]][1:100, 1:100]
act100$player <- c(1:100)
act100Melted <- melt(act100, id = "player")
ggplot(act100Melted, aes(x=player, y=variable)) + 
	geom_tile(aes(fill = value)) +
	xlab("Player 1 Rank") +
	ylab("Player 2 Rank") +
	theme_classic() +
	theme(axis.title = element_text(size = 25))

##########################
# plot 5 x 5 quasi binomial
x <- data.frame(c(NA,1,1,1,1), c(0,NA,1,1,1), c(0,0,NA,1,1), c(0,0,0, NA, 1), c(0,0,0,0,NA))
colnames(x) <- c(1:5)
x <- t(x)
colnames(x) <- c(1:5)
x <- as.data.frame(x)
x$player <- as.factor(c(1:5))
xMelted <- melt(x, id = "player")
names(xMelted) <- c("PlayerRank", "OpponentRank", "WinPercentage")

for(i in 1:nrow(xMelted)){
	if(xMelted[i,1] == 2){xMelted$WinPercentage[i] <- xMelted$WinPercentage[i] + 0.05}
	else if(xMelted[i,1] == 3) {xMelted$WinPercentage[i] <- xMelted$WinPercentage[i] + 0.1}
	else if(xMelted[i,1] == 4) {xMelted$WinPercentage[i] <- xMelted$WinPercentage[i] + 0.15}
	else if(xMelted[i,1] == 5) {xMelted$WinPercentage[i] <- xMelted$WinPercentage[i] + 0.2}
}

ggplot(xMelted, aes(x=as.factor(OpponentRank), 
		    y=WinPercentage, 
		    color = PlayerRank)) +
	geom_point(size = 6) +
	# geom_line() +
	xlab("Opponent Rank") +
	ylab("Win Percentage") +
	theme_classic() +
	theme(axis.text.y = element_blank())
	# geom_jitter(width = 0, height = .07, size = 6) 
# 	#geom_smooth(aes(group=PlayerRank),
# 		    method="lm", 
# 		    #family=quasibinomial(), 
# 		    method.args=list(family="quasibinomial"),
# 		    formula="y~x",
# 		    se = FALSE, 
# 		    size=1.5) 



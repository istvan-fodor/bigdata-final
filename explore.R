source("transform.R")

library(maps)
library(mapdata)

library(grDevices)
library(plyr)


#library(ggplot2)
#library(ggmap)

numerics <- college[,sapply(college,is.numeric)]
factors <- college[,!sapply(college,is.numeric)]


#Negative reation between SAT and age
plot(college_explore$actcm75, college_explore$share_25_older, xlab = "ACT 75th percentile",
     ylab = "Percentage of students 25+", main = "ACT75 - Age")
actfit <- glm(share_25_older ~ actcm75, data = college_explore)
summary(actfit)
actfit.r_squared <- 1 - actfit$deviance/actfit$null.deviance
actfit.r_squared
#Where are the high percentage?
morethan50 <- table(college_explore$carnegie_size_setting[college_explore$share_25_older > 0.50])
morethan50_dominant <- c("Small 2-year", "Very small 2-year", "Very small 4-year, primarily nonresidential")
p <- sum(morethan50[morethan50_dominant])
p <- p / sum(morethan50[2:length(morethan50)])
p

table(college_explore$carnegie_size_setting[college_explore$share_25_older < 0.50])

# per_state <- ddply(college_explore,~stabbr, summarise, mean=mean(ten_yrs_after_entry_median), sd=sd(ten_yrs_after_entry_median))
# per_state <- per_state[per_state$stabbr %in% state.abb,]
# rownames(per_state) <- per_state$stabbr
# per_state
# nrow(per_state)
# breaks <- hist(per_state[,2], plot = FALSE, breaks = 5)$breaks
# palette <- colorRampPalette(c("white", "black"))(length(breaks))
# palette <- rainbow(length(breaks))
# color <- sapply(per_state$mean, function(m) palette[max(which(m > breaks))])
# names(color) <- rownames(per_state)

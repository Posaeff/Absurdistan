library(RColorBrewer)
#display.brewer.all(colorblindFriendly = TRUE)
#display.brewer.all()


#yearpublished & counts plot
myColors <- c("#FFFF00","#FFA54F","#EE7942","#DD5B45","#8B3E2F")
barplot(rep(length(myColors),length(myColors)), col=c(myColors));myColors

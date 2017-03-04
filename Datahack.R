### Datahack
library(ggplot2)

setwd("~/Desktop/YINS Datahack")


rotatedAxisElementText = function(angle,position='x'){
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}


# Complaint Data
complaint <- read.csv("toy.complaint_data.csv", header=T)
head(complaint)
head(officer)
complaintCategory <- data.frame(table(complaint$complaintcategory))
complaintCategory <- complaintCategory[with(complaintCategory, order(-Freq)), ]
head(complaintCategory)
complaintCategory$Var1 <- factor(complaintCategory$Var1, levels = complaintCategory[order(complaintCategory$Var1), "Var1"])
complaintCategory$Var1 <- factor(complaintCategory$Var1, levels = complaintCategory$Var1)

ggplot(data = complaintCategory, aes(x=Var1,y=Freq))+geom_point(size=2,show.legend=F)+theme_minimal()+theme(axis.text.x = rotatedAxisElementText(90,'x'),axis.text.y = rotatedAxisElementText(90,'y'))
ggsave("distrib_of_complaints.png", width=10,height=6,units=c("in"))





# Officer Data
officer <- read.csv("toy.officer_data.csv", header=T)
head(officer)

officer$total_complaints <- officer$primary + officer$secondary + officer$tertiary
officer$gender <- factor(officer$gender)
officer$race <- factor(officer$race)
officer$rank <- factor(officer$rank)
officer$appointed.date <- as.Date(officer$appointed.date, format="%Y-%m-%d")
str(officer)
# to plot, race v year

ggplot(officer, aes(x=appointed.date, factor=race, color=race, fill=race)) + 
		geom_line(stat="density", alpha=0.2) + 
		geom_density(alpha=0.2) + 
		xlab("Date Appointed") + 
		ylab("Porportion of Hires") + 
		ggtitle("Race of Hires Over Time") +
		theme_minimal()
# ggsave("density_of_hires_over_time_race.png", width=10,height=6,units=c("in"))
ggplot(officer, aes(x=appointed.date, factor=gender, color=gender, fill= gender)) + 
		geom_line(stat="density", alpha=0.2) + 
		geom_density(alpha=0.2) + 
		xlab("Date Appointed") + 
		ylab("Porportion of Hires") + 
		ggtitle("Gender of Hires Over Time") +
		theme_minimal()
# ggsave("density_of_hires_over_time_gender.png", width=10,height=6,units=c("in"))
ggplot(officer, aes(x=appointed.date, y = age, color= age)) + 
		geom_point(alpha=0.5) +
		scale_colour_gradientn(colors = rainbow(7)) +
#		scale_color_distiller(palette="Spectral", direction=-1) + 
		xlab("Date Appointed") + 
		ylab("Porportion of Hires") + 
		ggtitle("Age of Hires Over Time") +
		theme_minimal()
ggsave("density_of_hires_over_time_age.png", width=10,height=6,units=c("in"))


officerPrimary <- data.frame(table(officer$primary))
complaintCategory <- complaintCategory[with(complaintCategory, order(-Freq)), ]
# complaintCategory$Var1 <- factor(complaintCategory$Var1, levels = complaintCategory[order(complaintCategory$Var1), "Var1"])
complaintCategory$Var1 <- factor(complaintCategory$Var1, levels = complaintCategory$Var1)

ggplot(data = complaintCategory, aes(x=Var1,y=Freq))+geom_point(size=2,show.legend=F)+theme_minimal()+theme(axis.text.x = rotatedAxisElementText(90,'x'),axis.text.y = rotatedAxisElementText(90,'y'))
ggsave("distrib_of_complaints.png", width=10,height=6,units=c("in"))


##
## andrew jaffe
## r coding stats 
library(lubridate)

## load data
load("myRstats_deidentified.rda")
me = me[year(me$dateModified)> 2008,]

## put in order
me = me[order(me$dateModified),]

## add era
eraCuts = ymd("2013-04-01","2012-01-01")
me$era = "faculty"
me$era[me$dateModified < eraCuts[1]] = "postdoc"
me$era[me$dateModified < eraCuts[2]] = "phd"
me$era = factor(me$era, levels=c("phd","postdoc","faculty"))

###############
## stat #####
dim(me)

## total bytes
sum(me$size)
sum(me$size)/1e6 # 13.4Mb

## total lines
sum(me$wc)/1e3 # 425k lines

##############3
## over time? ##
me$theMonth = floor_date(me$dateModified, "month")
monthStats = me[!duplicated(me[,c("theMonth", "era")]),
	c("theMonth", "era")]
rownames(monthStats) = NULL

monthStats$numFiles = as.numeric(table(me$theMonth))
monthStats$sumSize = as.numeric(
	tapply(me$size, me$theMonth, sum))
monthStats$meanSize = as.numeric(
	tapply(me$size, me$theMonth, mean))
monthStats$sumLine = as.numeric(
	tapply(me$wc, me$theMonth, sum))
monthStats$meanLine = as.numeric(
	tapply(me$wc, me$theMonth, mean))

############
## plots ###

plot(numFiles ~ theMonth, data=monthStats)
abline(v=eraCuts)
summary(lm(numFiles ~ theMonth + era, data=monthStats))$coef

plot(sumSize ~ theMonth, data=monthStats)
abline(v=eraCuts)
summary(lm(sumSize ~ theMonth + era, data=monthStats))$coef

plot(sumLine ~ theMonth, data=monthStats)
abline(v=eraCuts)
summary(lm(sumLine ~ theMonth + era, data=monthStats))$coef

summary(lm(numFiles ~ theMonth * era, data=monthStats))$coef
summary(lm(sumLine ~ theMonth * era, data=monthStats))$coef
summary(lm(sumSize ~ theMonth * era, data=monthStats))$coef

## means 
plot(meanSize ~ theMonth, data=monthStats)
abline(v=eraCuts)
summary(lm(meanSize ~ theMonth + era, data=monthStats))

plot(meanLine ~ theMonth, data=monthStats)
abline(v=eraCuts)
summary(lm(meanLine ~ theMonth + era, data=monthStats))

### nice figure
pdf("sizeVsMonth_rCode.pdf",h=5)
par(mar=c(5,6,2,2),cex.lab=2,cex.axis=2)
plot((sumSize/1e3) ~ theMonth, data=monthStats,
	pch = 21, bg="grey",xlab="",ylab="Total Size (kb)")
abline(v=eraCuts,lty=2:1)
dev.off()

##################
## time of day ###

me$theHour = hour(me$dateModified)
me$theHour = factor(me$theHour, c(5:23, 0:4))
hourStats = data.frame(theHour = as.numeric(levels(me$theHour)))

hourStats$numFiles = as.numeric(table(me$theHour))
hourStats$sumSize = as.numeric(
	tapply(me$size, me$theHour, sum))
hourStats$meanSize = as.numeric(
	tapply(me$size, me$theHour, mean))
hourStats$sumLine = as.numeric(
	tapply(me$wc, me$theHour, sum))
hourStats$meanLine = as.numeric(
	tapply(me$wc, me$theHour, mean))
hourStats[is.na(hourStats)] = 0

## time
hourStats$period = "morning"
hourStats$period[hourStats$theHour > 8 & 
	hourStats$theHour < 15] = "day"
hourStats$period[hourStats$theHour > 15 |
	hourStats$theHour < 5] = "night"
	
############
## plots ###
hourStats$plotHour = factor(hourStats$theHour,levels(me$theHour))
plot(numFiles ~ as.numeric(plotHour), data=hourStats)

pdf("sizeVsHour_rCode.pdf",h=5)
par(mar=c(5,6,2,2),cex.lab=2,cex.axis=2)
plot((sumSize/1e3) ~ as.numeric(plotHour), 
	data=hourStats,cex=2,xaxt="n",
	pch = 21, bg="grey",xlab="",ylab="Total Size (kb)")
axis(1, at = as.numeric(hourStats$plotHour),
	hourStats$plotHour)
abline(v=c(4.5,12.5))
abline(v=19.5, lty=2)
dev.off()

plot(sumLine ~ theHour, data=hourStats)

### by era
eraIndexes = split(1:nrow(me), me$era)
hourByEra = do.call("cbind", lapply(eraIndexes, function(ii) {
	o = cbind(as.numeric(table(me$theHour[ii])),
		as.numeric(tapply(me$size[ii], me$theHour[ii], sum)),
		as.numeric(	tapply(me$wc[ii], me$theHour[ii], sum)))
	colnames(o) = c("numFiles","sumSize","sumLine")
	o
}))
colnames(hourByEra) = paste0(colnames(hourByEra),"_",
	rep(names(eraIndexes), each=3))
hourByEra[is.na(hourByEra)] = 0
hourByEra = as.data.frame(hourByEra)

ind = c(2,5,8)
pdf("sizeVsHour_rCode_byEra.pdf",h=5)
par(mar=c(5,6,2,2),cex.lab=2,cex.axis=2,cex.main=1.5)
yl = range(hourByEra[,ind])
for(i in seq(along=ind)) {
	ii = ind[i]
	plot((hourByEra[,ii]/1e3) ~ as.numeric(plotHour), 
		data=hourStats,cex=2,xaxt="n",ylim = yl/1000,
		main = names(eraIndexes)[i],
		pch = 21, bg="grey",xlab="",ylab="Total Size (kb)")
	axis(1, at = as.numeric(hourStats$plotHour),
		hourStats$plotHour)
	abline(v=c(4.5,12.5))
	abline(v=19.5, lty=2)
}
dev.off()

##################
## day of week ###

me$theDay = wday(me$dateModified, label=TRUE)

dayStats = data.frame(theDay = levels(me$theDay))
dayStats$theDay = factor(dayStats$theDay, levels(me$theDay))
dayStats$numFiles = as.numeric(table(me$theDay))
dayStats$sumSize = as.numeric(
	tapply(me$size, me$theDay, sum))
dayStats$meanSize = as.numeric(
	tapply(me$size, me$theDay, mean))
dayStats$sumLine = as.numeric(
	tapply(me$wc, me$theDay, sum))
dayStats$meanLine = as.numeric(
	tapply(me$wc, me$theDay, mean))

############
## plots ###
plot(numFiles ~ as.numeric(theDay), data=dayStats)

pdf("sizeVsDay_rCode.pdf",h=5)
par(mar=c(5,6,2,2),cex.lab=2,cex.axis=2)
plot((sumSize/1e3) ~ as.numeric(theDay), 
	data=dayStats,cex=2,xaxt="n",
	pch = 21, bg="grey",xlab="",ylab="Total Size (kb)")
axis(1, at = 1:7, levels(dayStats$theDay))
dev.off()

#####
dayByEra = do.call("cbind", lapply(eraIndexes, function(ii) {
	o = cbind(as.numeric(table(me$theDay[ii])),
		as.numeric(tapply(me$size[ii], me$theDay[ii], sum)),
		as.numeric(	tapply(me$wc[ii], me$theDay[ii], sum)))
	colnames(o) = c("numFiles","sumSize","sumLine")
	o
}))
colnames(dayByEra) = paste0(colnames(dayByEra),"_",
	rep(names(eraIndexes), each=3))
dayByEra = as.data.frame(dayByEra)

ind = c(2,5,8)
pdf("sizeVsDay_rCode_byEra.pdf",h=5,w=5)
par(mar=c(6,6,2,2),cex.lab=2,cex.axis=2,cex.main=1.5)
yl = range(dayByEra[,ind])
for(i in seq(along=ind)) {
	ii = ind[i]
	plot((dayByEra[,ii]/1e3) ~ as.numeric(theDay), 
		data=dayStats,cex=2,xaxt="n",ylim = yl/1000,
		main = names(eraIndexes)[i],
		pch = 21, bg="grey",xlab="",ylab="Total Size (kb)")
	axis(1, at = 1:7, levels(dayStats$theDay),las=3)
}
dev.off()

#########################
#### number of lines ####
library(tm)
theCode = scan("allRcode.txt", sep="\n", 
	what="character",strip=TRUE,skipNul = TRUE)
length(theCode)/sum(me$wc)
sum(me$wc)-length(theCode) # whitespace
(sum(me$wc)-length(theCode))/sum(me$wc)

## make DF
codeDf = data.frame(code = theCode, stringsAsFactors=FALSE)
codeDf$isComment = grepl("^#", codeDf$code)
codeDf$hasComment = grepl("#", codeDf$code)

sum(codeDf$isComment) / sum(me$wc) # % comment
sum(codeDf$hasComment) - sum(codeDf$isComment) # % inline comment

allCode = strsplit(codeDf$code, " ")
allCode = unlist(allCode)
allCode = gsub(",","",allCode)
tt = sort(table(allCode),decreasing=TRUE)
fxn = c("plot(", "boxplot(", "table(", "lm(",
	"lmFit(","pdf(","grep(")
fxnHits = sapply(fxn, grepl, x=codeDf$code,fixed=TRUE)
colSums(fxnHits)

###
ss = function(x, pattern, slot=1,...) sapply(strsplit(x,pattern,...), "[", slot)
library(parallel)
library(lubridate)

# # ## paths to the files 
# paths = c("/home/epi/ajaffe", "/dcs01/ajaffe",
	# "/dcl01/lieber/ajaffe", "/nexsan2/disk3/ajaffe")
# names(paths) = c("home","dcs","dcl","nexsan")
# rFileList = mclapply(paths, list.files, pattern = "*\\.R$", 
	# full.names=TRUE, recursive = TRUE,mc.cores=4)
# save(rFileList, file="rFileList.rda")
load("rFileList.rda")

# combine
dat = data.frame(path = unlist(rFileList),
	dir = rep(names(rFileList), 
		times=sapply(rFileList,length)),
	stringsAsFactors=FALSE)
dat$file = sapply(strsplit(dat$path, "/"), function(x) x[length(x)])
dat = dat[,c("file", "dir", "path")]

## metrics
dim(dat)

## remove some that are from packages
dat = dat[!grepl("/anaconda/", dat$path),]
dat = dat[!grepl("/ballgown/", dat$path),]
dat = dat[!grepl("/gimmR/", dat$path),]
dat = dat[!grepl("rscharpf", dat$path),]
dat = dat[!grepl("kaggle/HH/", dat$path),]
dat = dat[!grepl("/software/", dat$path),]
dat = dat[!grepl("x86_64-unknown-linux-gnu-library/", dat$path),]
dat = dat[!grepl("x86_64-pc-linux-gnu-library/", dat$path),]
dat$isPerm = grepl("perm", dat$path)
dat$isSim = grepl("/newsim/", dat$path) | grepl("/sim/", dat$path)

### drop sim and perm
dat2 = dat[!dat$isPerm & !dat$isSim,] 
dim(dat2)

## add info 
paths = dat2$path
names(paths) = seq(along=dat2$path)
userList = mclapply(paths, function(x) {
	cat(".")
	ss(system(paste("ls -l", x), inter=TRUE)," ",3)
},mc.cores=12)
info = unlist(userList)
mm = as.numeric(names(info))
dat2$user[mm] = info

## only keep me as author
me = dat2[which(dat2$user == "ajaffe"),]

### get other stats
statList = mclapply(me$path, function(x) {
	cat(".")
	y = system(paste("stat", x), inter=TRUE)
	o = ss(y[c(2,6,7)], ": ", 2)
	o = ss(o, "\\.")
	o = ss(o, "  ")
	names(o) = c("size","modify","change")
	o
},mc.cores=4)
stats = do.call("rbind" ,statList)

## add
me$size = as.numeric(stats[,"size"])
me$dateModified = ymd_hms(stats[,"modify"])
me$dateChange = ymd_hms(stats[,"change"])

## add number of lines
pathsMe = me$path
names(pathsMe) = seq(along=me$path)
wcList = mclapply(pathsMe, function(x) {
	cat(".")
	ss(system(paste("wc -l", x), inter=TRUE)," ")
}, mc.cores=4)
wcList = wcList[sapply(wcList, length)==1]

wc = unlist(wcList)
mm = as.numeric(names(wc))
me$wc[mm] = as.numeric(wc)

### filter columns
me = me[!is.na(me$wc),c(1:3,7:ncol(me))]

save(me, file="myRstats.rda")

me = me[,4:7]
rownames(me) = NULL
save(me, file="myRstats_deidentified.rda")

## concantenate into 1 text file for word clouds
sapply(me$path, function(x) {
	cat(".")
	system(paste("cat", x, ">> allRcode.txt"))
})

cat(me$path, file="all_R_files.txt", sep="\n")
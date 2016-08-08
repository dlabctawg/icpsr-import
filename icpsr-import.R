#SCRAPED

scraped2RData<-function(src.dir,save.dir=src.dir){
	setwd(save.dir)
	names<-dir(src.dir)
	paths<-list.files(src.dir,full.names = T)
	files<-lapply(paths,function(x) {x<-unzip(x,list=T)[[1]];x[grep('^[^_]',x)]})
	obj<-gsub('([ -])|(th)','',sub('^(.+) Congress.+$','cong\\1',names))
	zip2list<-function(zfold){
		x<-unzip(zfold,list=T)[[1]]
		x<-x[grep('^[^_]',x)]
		ret<-list()
		for(i in x) {
			closeAllConnections()
			con<-unz(zfold,i)
			ret[[i]]<-readLines(con)
		}
		closeAllConnections()
		ret
	}


	for(i in 1:length(names)) {
		assign(obj[i],zip2list(paths[i]))
		save(list=obj[i],file=sub('zip','RData',names[i]))
		rm(list=obj[i])
	}
	names
}
if(F) files<-scraped2RData('/Volumes/Iomega_HDD/Scraped Hearings/')

# ICPSR
icpsr2RData.f<-function(src.dir,ncong=104:110){
	paths<-dir(src.dir,recursive = T,full.names = T)

	for(i in ncong){
		obj<-paste('icpsr.cong',i,sep='')
		n<-grep(i,paths,value = T)
		x<-sapply(n,read.table,sep='|',header=T,quote='',simplify=F,stringsAsFactors=T,colClasses=c(file='character',speech='character'))
		names(x)<-sub('^.+/','',names(x))
		assign(obj,x)
		rm(x)
		save(list=obj,file=paste(i,'th Congressional Record (ICPSR).RData',sep=''))
		rm(list=obj)
	}
}
if(F) icpsr2RData.f('/Volumes/Iomega_HDD/Scraped Hearings/txt')

icpsr2bow.f<-function(src.dir,save.dir=src.dir){
	require(data.table)
	require(tm)
	paths<-grep('ICPSR',dir(src.dir,full.names = T),value=T)
	speech<-list()
	speaker<-list()
	descr<-list()
	for(i in paths){
		l<-ls()
		cat('\nLoading ',i,'.',sep='')
		load(i)
		n<-setdiff(ls(),c(l,'l'))
		s<-function(nm) get(n)[[grep(nm,names(get(n)),ignore.case = T)]]
		cat()
		t0<-proc.time()
		speech[[n]]<-data.table(s('speech'))
		cat(' Loaded.\nPreprocessing. Changing to lower case.')
		speech[[n]][,speech:=tolower(speech)] # lower case
		cat(' Replacing non-alphas with space.')
		speech[[n]][,speech:=gsub('[^a-z ]',' ',speech)] # retain only alphas
		cat(' Splitting on spaces.')
		speech[[n]]<-speech[[n]][,strsplit(speech,'\\s+'),by=speechID] # split on spaces
		setnames(speech[[n]],2,'t')
		cat(' Removing english stopwords, single and double letters, and blanks.')
		setkey(speech[[n]],t)
		speech[[n]]<-speech[[n]][!unique(c('',unlist(strsplit(stopwords('english'),'[^a-z]+')),letters,apply(expand.grid(letters,letters),1,paste,collapse='')))] # fast remove stopwords and single and double letters
		cat(' Stemming in english.')
		speech[[n]][,t:=stemDocument(t)]
		cat(' Tabulating tokens.')
		speech[[n]]<-speech[[n]][,.N,by='speechID,t'] # tabulated tokens
		cat(' Storing characters as factors.')
		speech[[n]][,`:=`(speechID=factor(speechID),t=factor(t))]
		cat(' Keying table.')
		setkey(speech[[n]],speechID,t)
		t1<-proc.time()
		cat('\nPreprocessing finished in',round((t1-t0)/60,2)[3],'minutes.')
		cat('\nSaving covariate tables.')
		speaker[[n]]<-data.table(s('speaker'))
		descr[[n]]<-data.table(s('descr'))
		cat('\nRemoving source file.')
		rm(list=n)
	}
	cat('\nAppending and returning all tables.')
	list(
		speech=rbindlist(speech)
		,speaker=rbindlist(speaker)
		,descr=rbindlist(descr)
	)
}

if(F){
	icpsr2bow<-icpsr2bow.f(src.dir = '/Volumes/Iomega_HDD 1/Scraped Hearings')
	save(icpsr2bow,file='icpsr2bow.RData')
}

txt2pp.f<-function(x){
	require(tm)
	require(data.table)
	x<-data.table(t=x)
	cat('\nPreprocessing. Changing to lower case.')
	x[,t:=tolower(t)] # lower case
	cat(' Replacing non-alphas with space.')
	x[,t:=gsub('[^a-z ]',' ',t)] # retain only alphas
	cat(' Splitting on spaces.')
	x<-data.table(t=x[,unlist(strsplit(t,'\\s+'))]) # split on spaces
	cat(' Removing english stopwords, single and double letters, and blanks.')
	setkey(x,t)
	x<-x[!unique(c('',unlist(strsplit(stopwords('english'),'[^a-z]+')),letters,apply(expand.grid(letters,letters),1,paste,collapse='')))] # fast remove stopwords and single and double letters
	cat(' Stemming in english.')
	x[,t:=stemDocument(t)]
	x$t
}

if(F){
	# Scrape a security dictionary
	# load packages
	library(RCurl)
	library(XML)

	# download html
	dict <- getURL("https://myvocabulary.com//wordlist/alphalary_popup.php?wordlist_id=197", followlocation = TRUE)

	# parse html
	dict <- htmlParse(dict, asText=TRUE)
	dict <- xpathSApply(dict, "//span", xmlValue)

	#preprocess
	dict<-txt2pp.f(dict)

	# load saved 1-gram database
	load('icpsr1grambow.RData')
	# code year from speechID
	icpsr2bow$speech[,y:=factor(substr(speechID,4,7))]
	setkey(icpsr2bow$speech,t)
	ts<-merge(icpsr2bow$speech[dict,list(dict=sum(N)),by=y],icpsr2bow$speech[,list(base=sum(N)),by=y],by='y')
ts[,y:=as.integer(as.character(y))]
	#plot

png('security104-110.png',h=480,w=640)
	par(mar = c(3,5,3,5))
	with(ts, plot(y, dict/base, type="l", col="red3",xlab=NA,ylab='Proportion of All Terms',main='Security Terms in Floor Speeches of 104th-110th U.S. Congresses'))
	abline(v=c(1995,1999,2001),col='gray',lwd=3)
	par(new = T)
	with(ts, plot(y, dict, type='l',lty=2,col='black', axes=F, xlab=NA, ylab=NA))
	axis(side = 4)
	mtext(side = 4, line = 3, 'Count')
	legend("topleft",
				 legend=c('Prop.', "Count"),
				 lty=c(1,2), col=c("red3", "black"))
	mtext(c('Oklahoma City','Columbine','9/11'),at=c(1995,1999,2001),side=1,cex=.75,col='darkgray')
	dev.off()

}

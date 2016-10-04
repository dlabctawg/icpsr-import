# functions in development

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

icpsr2RData.f<-function(src.dir,ncong=104:110){
	paths<-dir(src.dir,recursive = T,full.names = T)

	for(i in ncong){
		obj<-paste('icpsr.cong',i,sep='')
		n<-grep(i,paths,value = T)
		x<-sapply(n,read.table,sep='|',header=T,quote='',simplify=F,stringsAsFactors=T,colClasses=c(file='character',speech='character',date='character'))
		names(x)<-sub('^.+/','',names(x))
		assign(obj,x)
		rm(x)
		save(list=obj,file=paste(i,'th Congressional Record (ICPSR).RData',sep=''))
		rm(list=obj)
	}
}


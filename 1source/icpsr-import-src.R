# production quality functions

#' Utility to convert text to pre-processed text.
#'
#' @param x The text to be processed.
#' @param split A regular expression on which to split, defaulting to contiguous whitespace. The match will be lost. Use NA to disable splitting.
u.txt2ppt<-function(x,split='\\s+'){
	require(tm)
	require(data.table)
	x<-data.table(t=x)
	cat('\nPreprocessing. Changing to lower case.')
	x[,t:=tolower(t)] # lower case
	cat(' Replacing non-alphas with space.')
	x[,t:=gsub('[^a-z ]',' ',t)] # retain only alphas
	if(!is.na(split)){
		cat(paste0(' Splitting on \"',split,"\"."))
		x<-data.table(t=x[,unlist(strsplit(t,split))]) # split on delimiter
	}
	cat(' Removing english stopwords, single and double letters, and blanks.')
	setkey(x,t)
	x<-x[!unique(c('',unlist(strsplit(stopwords('english'),'[^a-z]+')),letters,apply(expand.grid(letters,letters),1,paste,collapse='')))] # fast remove stopwords and single and double letters
	cat(' Stemming in english.\n\n')
	x[,t:=stemDocument(t)]
	x$t
}



#' Reformats ICPSR project 33501 into .RData compressed data tables. #https://www.r-bloggers.com/fast-csv-writing-for-r/
#'
#' @param src.dir Directory path containing ICPSR_33501.zip files corresponding to data objects 15, 19, and 20. http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/33501
#' @param out.dir Directory path to folder where .RData files will be written.
#' @param verbose Logical. Allow data.table to print (a lot of) extra information.
#' @param return Logical. Should the object be returned invisibly?
p1.icpsr2RData<-function(src.dir,out.dir,verbose=F,return=F){
	require(data.table) # a library that fixes some of base R's bad data practices!
	require(bit64) # required by data.table to deal efficiently with large integers
	require(magrittr) # brings piping to R, but we don't use it that much
	strppath<-function(x) sub(paste0('^.+',.Platform$file.sep),'',x) # a little utilty to strip a leading file path from file names
	t0<-proc.time() # a timestamp so we can measure how long importing takes
	for(z in dir(src.dir,full.names = T)){ # loop over each file in our source directory
		cat('\nDecompressing:\n',z,sep='') # File currently being processed
		td<-tempdir() # a path to a temporary location where we can decompress our archives and delete when finished
		cat('\nTemporary directory where data are unfortunately duplicated. Twice. Awaiting zip support for data.table::fread. :(\n',td,'\n',sep='') # a helpful-ish message
		zz<-grep('zip$',unzip(z,exdir=td,overwrite = T),value=T) # our first decompression, saving the file path to the temporary directory
		n<-sub('\\.zip','\\.RData',strppath(zz)) # a name for the file we will write to
		cat('\nDecompressed source files:\n') # a helpful message
		f<-unzip(zz,exdir=td) # our second decompression, now our data are really going to get big!
		cat(strppath(f)) # using our utility to strip file paths and print the names of the decompressed data files
		cat('\n\nFirst two lines of',strppath(f[1]),':\n') # a helpful message
		cat(readLines(f[1],n=2),sep='\n') # printing the first two lines so we can see what the raw data look like
		dt<-list() # create an empty container to put our data batches into
		cat('\nImporting:\n')
		for(i in f) { # loop over each of the data batch files
			cat(i,'\n',sep='')
			cc<-readLines(i,n=1) %>% strsplit('\\|') %>% unlist() # piping with magrittr! reading the first (header) row of data, and splitting on the delimitter
			cc<-cc[cc%in%c('id','speechID','speakerID','date')] # debugging showed us that these fields should be treated as characters even though they are numbers.
			names(cc)<-cc # naming the character strings after themselves...
			cc[]<-'character' # ...now we have a named list of variables to treat as character, as expected by fread()
			dt[[i]]<-fread(input=i,header=T,na.strings='',verbose=verbose,colClasses = cc) # fast reading from disk using data.table. Builds data table in C, bypassing inefficient R steps.
		} # few, let's do it all again on the next file!
		unlink(td) # forget our temporary folder. Your computer *should* delete it when it needs the space...
		dt<-rbindlist(dt) # append each imported batch file into a single data.table
		save(dt,file=paste(out.dir,n,sep=.Platform$file.sep)) # save all our hard work to disk
		if(verbose) cat('\n\n############################################################') # helpful message if we let data.table give us the play-by-play of the import process
		cat('\n\nIn memory size of data.table stored in file \"',n,'\"\n',sep='') # helpful message...
		tables() #...telling us how big our imported data are in memory
	}
	t1<-proc.time() #time stamp when it's all over
	cat('\nImported in',(t1-t0)[3],'seconds.\n') # how long did it take?
	if(return) { # optionally return in memory in addition to saving to disk
		dump<-as.list(dir(out.dir)) # make a container for each data table
		names(dump)<-dump # name them after the file they came from
		dump[[n]]<-dt # copy the table currently in memory
		for(i in setdiff(names(dump),n)) if(length(i)) { # if there are other tables...
			load(paste(out.dir,i,sep=.Platform$file.sep)) #...load them...
			dump[[i]]<-dt # ...and copy them to our container as well.
			rm(dt) #...remove the duplicate
		}
		return(dump) # return the container. I hope you assigned it!
	}
}


#' Pipeline to convert ICPSR 33501 data dump into a full text long database.
p2.icpsr2ftldb<-function(){
	require(data.table)

}

p3.ftldb2bow<-function(src.dir,save.dir=src.dir){
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
		speaker[[n]][,`:=`(id=as.character(id))]
		setkey(speaker[[n]],id)

		descr[[n]]<-data.table(s('descr'))
		descr[[n]][,`:=`(speakerID=as.character(speakerID),speechID=as.character(speechID))]
		setkey(descr[[n]],speakerID)

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

p4.bow2stm<-function(bow,id='speechID'){
	require(data.table)
	setnames(bow,id,'id')
	bow<-na.omit(bow)
	bow[,`:=`(id=droplevels(id),t=droplevels(t))]
	setkey(bow,id)
	bow2stm<-list()
	vocab<-levels(bow$t)
	f<-function(t,N) {x<-rbind(as.numeric(t),N);dimnames(x)<-NULL;matrix(as.integer(x),nrow=2)}
	bow2stm$documents<-bow[,list(documents=list(f(t,N))),keyby=id]$documents
	names(bow2stm$documents)<-levels(bow$id)
	bow2stm$vocab<-vocab
	bow2stm$meta<-list()
	bow2stm
}

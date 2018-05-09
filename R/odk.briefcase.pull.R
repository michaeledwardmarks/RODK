##################################################################################
#function to pull data from ODK Aggregate Server

odk.briefcase.pull<-
  function(
    aggregate.url=NULL,            #arg  -url   <url>
    odk.directory=NULL,           #arg         <path/to/dir>
    odk.username="admin",         #arg  -u     <username>
    form.id=NULL,                 #arg  -id    <form_id>
    export.dir=".",               #arg  -ed    <path/to/dir>
    storage.dir=".",              #arg  -sd    <path/to/dir>
    export.start.date=NULL,       #arg  -start <yyyy/MM/dd>
    export.end.date=NULL,         #arg -end   <yyyy/MM/dd>
    export.filename=NULL,         #arg  -f     <filename.csv>
    pem_file=NULL,                #arg -pf    <path/to/file.pem>
    exclude.media.export=FALSE,   #flag -em
    overwrite.csv.export=TRUE,   #flag -oc
    update.odk.briefcase=FALSE
            ){

    message("installing packages if needed")

    #check if getPass is installed and install it if not
  message("checking for new packages")
      list.of.packages <- c("getPass")

    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages))
      {
      install.packages(new.packages)
      message (paste("Installing ",new.packages))
    }
    message("loading getPass library")
    library(getPass)

    #check if update.odk.briefcase flag was active (default) and update odk briefcase
   message("Checking to see if Briefcase should be updated.")
      if(update.odk.briefcase==T)
      {
      message("Updating ODK Briefcase")
        download.file(url = "https://opendatakit.org/download/4476/",
          destfile = paste( "./odkbriefcase.jar",sep = ""), mode = "wb")
      }

    #check that only online or offline is being used
    if (!is.null(aggregate.url) & !is.null(odk.directory))
      {
      message("Please only specify one of aggregate.url and odk.directory")
      }


    #add flags for command line call
    if(!is.null(aggregate.url)){aggregate.url<-paste("-url ", aggregate.url, sep="")}            #arg  -url   <url>
    if(!is.null(odk.directory)){odk.directory<-paste("-od ", odk.directory, sep="")}
    odk.username<-paste("-u ", odk.username, sep="")
    form.id<-paste("-id ", form.id, sep="")
    export.dir<-paste("-ed ", export.dir, sep="")
    storage.dir<-paste("-sd ", storage.dir, sep="")
    if(!is.null(export.start.date)){export.start.date<-paste("-start ", export.start.date, sep="")}
    if(!is.null(export.end.date)){export.end.date<-paste("-end ", export.end.date, sep="")}
    if(!is.null(export.filename)){export.filename<-paste("-f ", export.filename, sep="")}
    if(!is.null(pem_file)){pem_file<-paste("-pf ", pem_file, sep="")}
    if(exclude.media.export==T){exclude.media.export<-"-em"}else{exclude.media.export<-""}
    if(overwrite.csv.export==T){overwrite.csv.export<-"-oc"}else{overwrite.csv.export<-""}

    #generate command for system call to odk briefcase

    password<- getPass(msg = "Enter Password for server")
    password<- paste("-p ",password,sep="")
    command<-paste("java -jar odkbriefcase.jar ",
                   aggregate.url,
                   odk.directory,
                   odk.username,
                   form.id,
                   export.dir,
                   export.start.date,
                   export.end.date,
                   export.filename,
                   storage.dir,
                   pem_file,
                   exclude.media.export,
                   overwrite.csv.export,
                   password,
                   sep=" ")
    message(command)

    system(command)
}


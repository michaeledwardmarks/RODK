##################################################################################
#function to get a new copy of ODK briefcase named odkbriefcase.jar
odk.briefcase.update<-function(folderforsave = ".")
{
  download.file(
      url = "https://opendatakit.org/download/4476/",
      destfile = paste(folderforsave, "/odkbriefcase.jar",sep = ""), mode = "wb")
}

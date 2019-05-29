
library(yaml)
check.version<-function() {
desc<-yaml.load_file("DESCRIPTION")
versio<-desc$Version
j000<-yaml.load_file("jamovi/0000.yaml")
j000$version
if (j000$version!=desc$Version) {
    cat("The version does not correspond in the jamovi e description file")
    cat(paste("jamovi:",j000$version))
    cat(paste("DESCRIPTION:",j000$version))
    return(FALSE)   
}
desc$Version
}


getVersion<-function() {
  desc<-yaml.load_file("DESCRIPTION")  
  j000<-yaml.load_file("jamovi/0000.yaml")
  if (desc$Version!=j000$version) {
    cat("Version mismatch\n")
    cat(paste("jamovi:",j000$version,"  "))
    cat(paste("R:",desc$Version))
    return(FALSE)    
  }
  cat(paste("preparing version",desc$Version))
  desc$Version    
}
version<-getVersion()
version
setwd("~/Skinner/Forge/jamovi/jamm/")
binaries<-"/home/marcello/Skinner/Forge/jamovi/binaries/"
cp<-system(paste0("cp jamm.jmo ",binaries,"/jamm_linux_",version,".jmo"))


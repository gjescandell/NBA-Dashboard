require(shiny)

folder_address = 'C:/Users/Gonzalo/Desktop/NBA'

x <- system("ipconfig", intern=TRUE)
z <- x[grep("IPv4", x)]
ip <- gsub(".*? ([[:digit:]])", "\\1", z)
print(paste0("the Shiny Web application runs on: http://", ip, ":1000/"))

runApp(folder_address, launch.browser=FALSE, port = 1000, host = ip)
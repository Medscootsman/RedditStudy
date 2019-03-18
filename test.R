install.packages("rjson")
library("rjson")
jsonfile = "_modified.json"

dataset = fromJSON(file=jsonfile)

print(dataset)

tcpdata <- as.data.frame(dataset)

occurences <- table(unlist(tcpdata))

print(occurences)


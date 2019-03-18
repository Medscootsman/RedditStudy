library("rjson")

library("NCmisc")

datafile="RC_2018-06.json"

file.split(datafile, size = 200000)

dataset = fromJSON(file="RC_2018-06_part1.json")

print(dataset)                           

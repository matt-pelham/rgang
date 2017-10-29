rm(list = ls())
#Source the file coxntaining the functions that load and parse the data
source("dataload.R")
source("defaultanalysis.R")
source("schooltypeanalysis.R")
source("geographicanalysis.R")

full_df <- load.data()

unflattened_df <- unflatten.data(full_df)

p1 <- densityplot.default.over.tuition(unflattened_df)
print(p1)
ggsave(filename = "densityplot.default.over.tuition.png", plot = p1, width = 6, height = 4, dpi = 600)

p2 <- densityplot.defaultrate.over.tuition(unflattened_df)
print(p2)
ggsave(filename = "densityplot.defaultrate.over.tuition2.png", plot = p2, width = 6, height = 4, dpi = 600)


p4 <- plot.default.rate.over.degree.and.tuition(unflattened_df) 
print(p4)
ggsave(filename = "defaultrate-degree-tuition.png", plot = p4, width = 6, height = 4, dpi = 600)

generate.summary.tables(unflattened_df)

p5 <- plot.default.over.tuition(unflattened_df)
print(p5)
ggsave(filename = "defaultrate-tuition-schooltype.png", plot = p5, width = 6, height = 4, dpi = 600)

p6 <- plot.default.over.tuition.typefacet(unflattened_df)
ggsave(filename = "defaultrate-over-schooltype.png", plot = p6, width = 7, height = 4, dpi = 600)


#An error handler function to use in conjunction with mapping state default rates
my.state.map.error.handler <- function(error){
  msg <- sprintf("Error generating state default rate plot.  Generating full map.")
  message(paste(msg,error," "))
  map <- generate.choropleth.maps(unflattened_df)
  map
}

#Call the map function with no state to get the entire map
map <- generate.choropleth.maps(unflattened_df)
print(map)

#Call the map function wrapped in a try catch in case the state provided is invalid
result <- tryCatch(generate.choropleth.maps(unflattened_df,"IA"), error = my.state.map.error.handler)
print(result)


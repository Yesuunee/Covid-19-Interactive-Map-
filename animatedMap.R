# Create a map of the lower 48 states of the USA as an SVG file
svg("usStates.svg")
k = maps::map('state', fill = TRUE, col = "white", main = "Cases By Day")
dev.off()


# Post-process the SVG file containing the map, finding the polygons for the different regions.
library(XML)
doc = xmlParse("usStates.svg")
paths = getNodeSet(doc, "//svg:path", "svg")

# Add an id attribute to each polygoon with the name of the state.
# id would be fine if we just used k$names directly as those values are unique.
mapply(function(node, state) {
          xmlAttrs(node)["id"] = state
}, paths, gsub(":.*", "", k$names))


# Add tooltips to each polygon to display the name.
invisible(mapply(function(node, stateName) newXMLNode("title", stateName, parent = node), paths, gsub(":.*", "", k$names)))

# Save the post-processed/modified SVG
saveXML(doc, "usStates.svg")



# The variable us is the one we construct from the NYTimes data
names(us$data) = sapply(us$data, `[[`, "display_name")
ws = sapply(us$data, `[[`, "region_type") == "state"
source("covidFuns.R")
states = lapply(us$data[ws], mkCounty)


r = range(sapply(states, function(x) range(x$date)))
r2 = as.Date(r, "1970-1-1")
dates = seq(r2[1], r2[2], by = 1)


d = lapply(states, `[[`, "cases")
d = lapply(d, function(x) { x[x < 0] = 0 ; x})


v = seq(0, 1, by = 1/max(unlist(d)))
cols = rgb(1, 1 - v, 1- v)

d.cols = lapply(d2, function(x)  cols[ x + 1])


names(d.cols) = tolower(names(d.cols))
cat(paste("var stateColors = ", toJSON(d.cols), ";"), 
	paste("var dateLabels = ", toJSON(format(states[[1]]$date, "%d %B")), ";"), 
    file = "stateColors.json")




# The time series plot as an SVG plot and then post-process to add the click event handlers
# to the points

w = sapply(us$data, `[[`, "region_type")
usa = mkCounty(us$data[[1]])

svg("usCases.svg")
with(usa, {
   plot(date, cases, type = "l", xlab = "Date", ylab = "Number of Cases",
        main = "Number of COVID Cases in USA by day")
   points(date, cases, pch = 20, col = "red")
 })
dev.off()


# Post-process the SVG
u = xmlParse('usCases.svg')

# Find the points
points = getNodeSet(u, "//svg:g[contains(@id,'surface')]/svg:path[not(following-sibling::svg:g[./svg:use])]", "svg")

# Add a tooltip and onclick attribute to each.
invisible(mapply(function(node, date, cases, idx) {
                    tip = paste(date, "-", cases, "cases")
                    newXMLNode("title", tip, parent = node)
					xmlAttrs(node)["onclick"] = sprintf("setStatesColorEmbedded(%d)", idx)
	 			}, points, usa$date, usa$cases, seq(along = points) - 1L))

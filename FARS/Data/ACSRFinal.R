data("fips.state")
data("fips.county")
mygeo <- geo.make(state = "*", county = "*")
x <- acs.fetch(endyear=2015, geography = mygeo, 
               table.number = "B01003", 
               key = "3dbabece4401ad72afa36a118e2cf777efa2afb3")
countypop <- as.data.frame(estimate(x))
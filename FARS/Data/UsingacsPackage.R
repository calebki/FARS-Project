x <- geo.make(state = "*", county = "*")
y <- acs.fetch(endyear=2015, geography=x, table.number="B01003", key = "17b6e09794a8f4a42664535f0e519179cc06f5a7")
z <- estimate(y)
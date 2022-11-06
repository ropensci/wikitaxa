library("vcr")
vcr::vcr_configure(
	dir = "../fixtures", 
	serialize_with = "json"
)

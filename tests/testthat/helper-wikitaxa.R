library("vcr")
vcr::vcr_configure(
	dir = "../fixtures/vcr_cassettes", 
	serialize_with = "json"
)

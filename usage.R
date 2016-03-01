
# Select google_api.R
source(file.choose())

origin = c("Via Paolo Emilio", "Vancouver BC", "Seattle")
destination =c("Piazzale Aldo Moro", "San Francisco", "Victoria BC")

# API Key 
api_key = "AIzaSyBgTslqWxmNPlZx9-LvmeDrGRnVq8AEXyI"

api_url = get_url(origin, destination, api_key)

data = get_data(api_url)

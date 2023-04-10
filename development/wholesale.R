costco <- read.csv("C:/Users/lwget/Downloads/costco-stores.csv", sep = "")

View(costco)

tidyr::separate_wider_delim(costco,
                            address,
                            "      ",
                            names = c("City","Address","Phone Number"),
                            too_many = "debug")



address_parts <- strsplit(costco$address, "+")
View(address_parts)

# extract the relevant information
costco$City <- sapply(address_parts, function(x) x[1])
costco$Address <- paste(sapply(address_parts, function(x) x[-c(1, length(x))]), collapse = " ")
costco$Phone_Number <- sapply(address_parts, function(x) x[length(x)])

# remove the "Â" character
costco <- data.frame(lapply(costco, function(x) gsub("Â", "", x)))

# display the result
costco |> View()









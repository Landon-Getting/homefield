# territory_maps
Creating an R package for quick visualization of territory maps.

## Example Map

![image](https://user-images.githubusercontent.com/91226307/221045102-38ed1dd5-76fa-4f1f-9752-bc87d2b3f3f6.png)

*Inspired by [u/CaptainScuttlebottom](https://www.reddit.com/user/CaptainScuttlebottom/submitted/) and [u/jloose128 on Reddit](https://www.reddit.com/user/jloose128/).

## Input:
Data frame where each element has latitude, longitude, and a file path to an image.

| Latitude | Longitude | Image |
| ----------- | ----------- | ----------- |
| 42.501171 | -94.169388 | C:/Users/lwget/Documents/PlotPictures/Dodgers.jpg |
| 42.026798 | -93.620178 | C:/Users/lwget/Documents/PlotPictures/Little-Cyclones.jpg |
| 41.644040 | -93.464610 | C:/Users/lwget/Documents/PlotPictures/Rams.jpg |

## Output:
Territory map where each element is represented on a map by the closest regions (counties/states/countries) to the input lat and long. This calculation is performed based on the centriod of each region. Regions are colored based on the most frequent color in the image or by a provided color in another column of the data frame (can preference a provided color if given and then resort to most frequent color). The image from each element is placed into its respective territory. The image does not touch the boundries of the territory and fits inside nicely.

## Use Cases:

**Athletic -** What teams or individuals are currently undefeated and who is their closest geographic competition?

**Economic -** Where is the closest wholesale store to each US county (Sam's Club vs CostCo)?

**Commerical -** Show the LinkedIn headshot for the closest sales rep to each state.

**Political -** What is the closest private high school to each Iowan county? what is the closest public high school to each Iowan county?



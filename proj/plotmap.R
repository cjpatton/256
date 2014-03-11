pmap = function(map,title,midpoint,df)
{
  library(ggmap)
  plot = ggmap(map)
  plot = plot + geom_point(aes(x= Longitude, y=Latitude, color = Median.House.Value), data=df, alpha=.9, size = .75) +scale_colour_gradient2(high="blue", low ="red", mid= "grey",midpoint=midpoint)
  plot = plot + ggtitle(title) 
}
#Example usage for california map
df = read.csv('~/R Scripts/cadata.csv')
cali = get_map('California', zoom = 6)
plot = pmap(cali,'California Median House Value',2.5e5,df)
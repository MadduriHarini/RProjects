Using the tidyverse package in R, I began by downloading and extracting a compressed dataset of flights from LAX to JFK. 
After loading the data with read_csv(), I cleaned and filtered it to remove missing or extreme delay values. 
Visualizations like boxplots and scatterplots were used to analyze patterns in arrival delays across different airlines, with Alaska Airlines showing relatively stable delay behavior. 
By calculating Pearson correlations, I found a strong positive linear relationship between departure and arrival delays, especially for variables like DepDelayMinutes and CarrierDelay.
I then grouped the data to summarize average delays by airline and day of the week, and visualized these insights through heatmaps and descriptive bar plots.
Correlation matrices and corrplot() helped me uncover multivariate relationships across different types of delays. 
Finally, I conducted ANOVA to test whether delay patterns significantly differ across specific airlines, confirming statistical differences in arrival delays.

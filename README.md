# Calculate Bilkollektivet cost

The respository for a Shiny app that calculates the total cost of renting a car with Bilkollektivet based on the duration and distance of the rental.

## Under the hood

The app scrapes the Bilkolletivet website for the prices [here](https://bilkollektivet.no/priser/) and performs a calculation similar to the examples shown at the bottom of the Bilkollektivet page.

## Future developments
Here's a list of potential future functionality and improvements if I find the time (_which I won't_)

- **A comparison tool**, so one can calculate the price difference between different car types. Differences are pretty small usually though.

- **Collapsing the days and hours selectors to automatically shift to days if the number of hours is larger than 7.** So this would then probably be a non-linear slider where the first part of the slider counts in hours, and then shifts to days from there on.

- **Design improvements**, obviously. The current layout is very Shiny-basic. Which is fine, but I think I can have some fun with CSS and try and make it look better and more professional.
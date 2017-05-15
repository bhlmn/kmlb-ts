# kmlb-ts
Using ML classification methods to predict summertime thunderstorm initiaion in Melbourne, FL.

## Background
### Numerical Weather Prediction
Numerical weather prediction (NWP) systems are, in short, amazing. Their use has dramatically improved our (human's) ability to explain and predict the future of the atmosphere. Yet they have their limitations, one of which is the ability to adequately predict the timing and location of summertime convection thunderstorms. The atmosphere is chaotic, and NWP systems might not ever be able to provide forecasts such as 'thunderstorms in your part of town will start at 1:30 pm,' but many are striving for that goal.

### NWP and machine learning
> Note: This literature review is still being built ... a good Google search is 'machine learning lightning forecast'.

[Model Output Statistics (MOS)](https://en.wikipedia.org/wiki/Model_output_statistics), the oldest known marriage of ML and NWP, uses multivariate linear regression to determine statistical relationships between observations (what actually occurs) and NWP forecasts (predictors). The MOS framework has been used since the early 1970s [(Glahn and Lowry 1972)](http://journals.ametsoc.org/doi/abs/10.1175/1520-0450(1972)011%3C1203:TUOMOS%3E2.0.CO;2) and works exceptionally well. In terms of thunderstorm prediction, MOS equations have been developed to provide 6 hr and 12 hr thunderstorm probabilities.

[Sá et al. (2011)](https://www.researchgate.net/publication/303773171_Lightning_Forecast_Using_Data_Mining_Techniques_On_Hourly_Evolution_Of_The_Convective_Available_Potential_Energy) used k-Nearest Neighbors and Decision Trees to classify the lightning activity at Belem Airport in Brazil. Hourly convective available potential energy (CAPE; a metric of the buoyancy of a rising air parcel) values were used to classify three scenarios: 1) no, 2) moderate, and 3) heavy lightning activity. The methods accurately predicted the lightning class 70% of the time.

## Data
Model data consist of GFS/AVN MOS forecasts provided by the [Iowa State University Mesonet](https://mesonet.agron.iastate.edu/mos/fe.phtml). Observations are provided by METARs at [Orlando Melbourne International Airport (KMLB)](https://en.wikipedia.org/wiki/Orlando_Melbourne_International_Airport) accessed through the R package `riem` [(Salmon 2016)](https://cran.r-project.org/web/packages/riem/index.html). These METAR reports are used to classify summertime (May through September) thunderstorm initiation into four categories:
* 0 - no thunderstorms on this given day
* 1 - thunderstorms observed before noon
* 2 - thunderstorms began in the afternoon (noon to 5pm)
* 3 - thunderstorms began in the evening (after 5 pm)

## Methods
This project will use the following classification methods:
* Logistic regression
* k-Nearest Neighbors
* Random Forests
* Support Vector Machines

More information coming soon ...

## Acknowledgements
I want to thank the [Iowa State University Mesonet](https://mesonet.agron.iastate.edu/) for providing such a wonderful data archive and rich APIs to grab weather data with ease!

## References
Glahn, H. R. and D. A. Lowry, 1972: The use of model output statistics (MOS) in objective weather forecasting. *J. Appl Meteor.*, **11**, 1203-1211.

Sá, A. J. S., A. C. Almeida, B. R. P. Rocha, M. A. S. Mota, J. R. S. Souza, and L. M. Dentel, 2011: Lightning Forecast Using Data Mining Techniques on Hourly Evolution of the Convective Available Potential Energy. *Brazilian Congress on Computational Intelligence, Fortaleza, November 2011*, 8-11.

Salmon, M, 2016: `riem`: Accesses Weather Data from the Iowa Environment Mesonet. R package version 0.1.1. https://CRAN.R-project.org/package=riem

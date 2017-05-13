# kmlb-ts
Using ML classification methods to predict summertime thunderstorm initiaion in Melbourne, FL.

## Background
### Numerical Weather Prediction
Numerical weather prediction (NWP) systems are, in short, amazing. Their use has dramatically improved our (human's) ability to explain and predict the future of the atmosphere. Yet they have their limitations, one of which is the ability to adequately predict the timing and location of summertime convection thunderstorms. The atmosphere is chaotic, and NWP systems might not ever be able to provide forecasts such as 'thunderstorms in your part of town will start at 1:30 pm,' but many are striving for that goal.

### NWP and machine learning
[Model Output Statistics (MOS)](https://en.wikipedia.org/wiki/Model_output_statistics) uses multivariate linear regression to determine statistical relationships between observations (what actually occurs) and NWP forecasts (predictors). The MOS framework has been used since the early 1970s (Glahn and Lowry 1972) and works exceptionally well. 

## Data
Model data consists of GFS/AVN MOS forecasts provided by the [Iowa State University Mesonet](https://mesonet.agron.iastate.edu/mos/fe.phtml). 

## Acknowledgements
I want to thank the [Iowa State University Mesonet](https://mesonet.agron.iastate.edu/) for providing such a wonderful data archive and rich APIs to grab weather data with ease!

## References
Glahn, H. R. and D. A. Lowry, 1972: The use of model output statistics (MOS) in objective weather forecasting. *J. Appl Meteor.*, **11**, 1203-1211.

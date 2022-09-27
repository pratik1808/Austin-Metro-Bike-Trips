# Austin-Metro-Bike-Trips
Forecasted demand for bikes at kiosks in Austin on monthly basis to increase profitability by improving supply logistics by creating multiple predictive models in R - Multiple Linear Regression, K-Nearest Neighbors, Random Forests, XGBoosting

The data consisted of 1.69 million rows where each entry corresponded to a MetroBike trip of a customer. We removed the incorrect and null entries. The data consisted of 13 columns out of which we considered columns Trip Duration Minutes, Checkout Kiosk, Checkout Date, and Membership Type, and aggregated the rest of the columns to get a new column that corresponded to Number of Trips.

Of all the models, bagging yielded the best results and it makes sense to utilize a non-parametric bagging model built based on location, month, and membership type to predict prospective trip volumes. This information can be utilized in two main ways:
Supply Management - Plan in advance for high traffic months (eg: March or October) for specific customer categories at high-performance locations 
Marketing - Identify low-performance months/locations with a good customer pool and see if sales can be boosted through advertisement campaigns

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.preprocessing import OneHotEncoder
import shap
from matplotlib import pyplot as plt

# In[]
#### Note ####
#### set PATH first ####
path = '/Users/'
date = '241119'

org_seed = my_seed = 100 # Root seed, resets at the start of each loop
# Set the main seed for reproducibility
main_seed = 100
np.random.seed(main_seed)

# Load the dataset
df_all = pd.read_csv(f'{path}/Empirical_moth_data.csv')
df_all.dropna(subset=['TTrange','Weight','B_length','W_length'], inplace=True)

# In[] If location includes 'All', run this section first
# Caluculate 'raletive' elevation distribution for integrated analysis
df_all['up_std'] = 0 # Initialize column for standardized upper elevation
df_all['low_std'] = 0 # Initialize column for standardized lower elevation
for loc_ in ['Malaysia', 'Taiwan', 'China']: # Loop through each location
  df_std = df_all[df_all.Location == loc_] # Filter dataset for current location
  if(loc_ == 'Malaysia'):
      vec_up_relative = df_std['Elev_up']/1959  # Normalize upper elevation for Cameron Highlands
      vec_low_relative = df_std['Elev_low']/1959 # Normalize lower elevation for Cameron Highlands
  elif(loc_ == 'Taiwan'):
    vec_up_relative = df_std['Elev_up']/3140  # Normalize upper elevation for Mt. Hehuan
    vec_low_relative = df_std['Elev_low']/3140 # Normalize lower elevation for Mt. Hehuan
  elif(loc_ == 'China'):
    vec_up_relative = df_std['Elev_up']/4152  # Normalize upper elevation for Mt. Jiajin
    vec_low_relative = df_std['Elev_low']/4152 # Normalize lower elevation for Mt. Jiajin
  # Assign normalized values to dataset
  df_all.up_std[df_all['Location'] == loc_] = vec_up_relative
  df_all.low_std[df_all['Location'] == loc_] = vec_low_relative
  
# In[] Random forest model for 'Elev_up' and 'Elev_low' across all locations
for loc in ['All', 'China','Taiwan','Malaysia']:
    if loc == 'All':
        df_rf = df_all # Use full dataset for 'All'
        y_columns = ['low_std', 'up_std'] # Define target variables
    else:
        df_rf = df_all[df_all.Location == loc] # Filter dataset for current location
        y_columns = ['Elev_low', 'Elev_up'] # Define target variables
    
    y_ = df_rf[y_columns] # Extract target columns
    x_ = df_rf[['Family', 'B_length', 'CTmax', 'CTmin']] # Define feature columns

    for coor in y_columns: # Loop through target variables
        my_seed = org_seed # Reset seed
                        
        df_rf.dropna(axis=0, how='any', inplace=True) # Drop rows with missing values
        
        y = y_[coor].values # Extract target values
        rfr_df = None # Initialize dataframe for storing results
        for i in range(10): # Perform 10 iterations
            print(f'Processing... {i+1}', end='\r') # Print progress
            
            rfr = RandomForestRegressor(n_estimators=500, max_samples=.7, oob_score=True, random_state=my_seed) # Initialize RandomForestRegressor
            ohe = OneHotEncoder(sparse=False) # Initialize one-hot encoder
            fam_ohe = ohe.fit_transform(df_rf.Family.values.reshape(-1,1)) # Apply one-hot encoding to 'Family' column
            
            x_cols = list(ohe.categories_[0]) + list(x_.columns[1:])  # Define feature column names
            
            x = np.concatenate([fam_ohe, x_.iloc[:,1:].values], axis=1) # Combine encoded categorical and numerical features
            
            
            rfr.fit(x, y) # Train the random forest model
            feature_importances_ = rfr.feature_importances_ * 100 # Compute feature importance
            rfr_dict = dict(zip(x_cols, feature_importances_)) # Store feature importance in a dictionary
            rfr_dict['r2_oob'] = rfr.oob_score_ # Store out-of-bag R-square score
            rfr_dict['r2_x'] = rfr.score(x, y) # Store model performance score
            
            explainer = shap.TreeExplainer(rfr) # Initialize SHAP explainer
            
            if rfr_df is None:
                rfr_df = pd.DataFrame(rfr_dict, index=[i]) # Create DataFrame from dictionary
            else:
                rfr_df = pd.concat([rfr_df, pd.DataFrame(rfr_dict, index=[i])]) # Append results
            if i < 9:
                shap_values = explainer.shap_values(x) # Compute SHAP values
                df_ = pd.DataFrame(shap_values) # Convert SHAP values to DataFrame
                df_.to_csv(f'{path}/10_rfr_{coor}_SHAP_value_{i}_{date}_{loc}_seed_{my_seed}.csv', index=False, sep=',') # Save SHAP values to CSV
            else:
                shap_values2 = explainer(x) # Compute SHAP values for the last iteration
                shap_values22 = shap_values2.values # Extract values
                df_ = pd.DataFrame(shap_values22) # Convert to DataFrame
                df_.to_csv(f'{path}/10_rfr_{coor}_SHAP_value_{i}_{date}_{loc}_seed_{my_seed}.csv', index=False, sep=',')  # Save to CSV
       
            print(f'Done. Seed: {my_seed}') # Print completion message
            feature_importances = rfr_df.iloc[:,:-2] # Extract all feature importance values except the R² scores
            pd.concat([rfr_df[feature_importances.mean().sort_values().index.values], rfr_df.iloc[:,-2:]], axis=1).to_csv(f'{path}/10_rfr_All_{coor}_{date}_{loc}.csv', index=False, sep=',') # Sort feature importances and save the results to a CSV file          
            my_seed = my_seed+1 # Increment the seed for the next iteration
            
          
        colname = list(ohe.categories_[0]) # Extract categorical feature names
        for m in ['Body length', 'CTmax', 'CTmin']: # Append additional numerical feature names
            colname.append(m)
    
        li = [] # Initialize list to store dataframes
        seed = org_seed # Reset seed
        for k in range(9): # Loop through 9 iterations
            df = pd.read_csv(f'{path}/10_rfr_{coor}_SHAP_value_{k}_{date}_{loc}_seed_{seed}.csv') # Load CSV data
            df.columns = colname # Assign column names
            li.append(df) # Append dataframe to list
            seed = seed+1 # Increment seed
    
        df_10 = pd.DataFrame(shap_values2.values) # Convert SHAP values to dataframe
        df_10.columns = colname # Assign column names
        li.append(df_10) # Append dataframe to list
    
        from functools import reduce # Import reduce function
        df_sum = reduce(lambda x, y: pd.DataFrame.add(x, y, fill_value=0), li) # Sum up SHAP values across all iterations
        df_sum = df_sum/10 # Compute average SHAP values
        colname_sum = [] # Generate column index list
        for n in range(0,len(colname)):
            colname_sum.append(n)
        df_sum.columns = colname_sum # Assign column indices
            
        shap_values2.values = df_sum # Update SHAP values
        shap_values2.feature_names = colname # Assign feature names
    
        ## plotting shap
        plt.title(f"{loc}_{coor}") # Set plot title
        shap.plots.bar(shap_values2, show=False) # Generate bar plot
        plt.tight_layout() # Adjust layout
        plt.savefig(f'{path}/{date}_{loc}_{coor}_bar.png', dpi=300) # Save plot as PNG
        plt.close() # Close plot

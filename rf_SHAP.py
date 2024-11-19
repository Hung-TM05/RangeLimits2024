import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.preprocessing import OneHotEncoder
import shap
from matplotlib import pyplot as plt

# In[]
#### Note ####
#### set PATH first!!
path = '/Users/tmhung/Library/CloudStorage/OneDrive-個人/ESSLAB/0. Analysis/TW+SJ+CA_TT_data/CTS_Result_20201022/231128_RangeLimits_STR/4. Finals'
date = '241119'

org_seed = my_seed = 100 # Root seed repeats at each loop starts
# Set main random seed
main_seed = 100
np.random.seed(main_seed)
df_all = pd.read_csv(f'{path}/spe_data_range_limits_230928.csv')
df_all.dropna(subset=['TTrange','Dry_Weight','B_length','W_length'], inplace=True)

# In[] If location includes 'All', run this first
# Caluculate 'raletive' elevation distribution for integrated analysis
df_all['up_std'] = 0
df_all['low_std'] = 0
for loc_ in ['Malaysia', 'Taiwan', 'China']:
  df_std = df_all[df_all.Location == loc_]
  if(loc_ == 'Malaysia'):
      vec_up_relative = df_std['Elev_up']/1959  # thorough elevation span of Cameron Heights
      vec_low_relative = df_std['Elev_low']/1959
  elif(loc_ == 'Taiwan'):
    vec_up_relative = df_std['Elev_up']/3140  # thorough elevation span of Mt. Hehuan
    vec_low_relative = df_std['Elev_low']/3140
  elif(loc_ == 'China'):
    vec_up_relative = df_std['Elev_up']/4152  # thorough elevation span of Mt. Jiajin
    vec_low_relative = df_std['Elev_low']/4152
  df_all.up_std[df_all['Location'] == loc_] = vec_up_relative
  df_all.low_std[df_all['Location'] == loc_] = vec_low_relative
  
# In[] Random forest model for 'Elev_up' and 'Elev_low' in all & 3 locs
for loc in ['All', 'China','Taiwan','Malaysia']:
    if loc == 'All':
        df_rf = df_all
        y_columns = ['low_std', 'up_std']
    else:
        df_rf = df_all[df_all.Location == loc]
        y_columns = ['Elev_low', 'Elev_up']
    
    y_ = df_rf[y_columns]
    x_ = df_rf[['Family', 'B_length', 'CTmax', 'CTmin']]

    for coor in y_columns:
        my_seed = org_seed
                        
        df_rf.dropna(axis=0, how='any', inplace=True)
        
        y = y_[coor].values
        rfr_df = None
        for i in range(10):
            print(f'Processing... {i+1}', end='\r')
            
            rfr = RandomForestRegressor(n_estimators=500, max_samples=.7, oob_score=True, random_state=my_seed)
            ohe = OneHotEncoder(sparse=False)
            fam_ohe = ohe.fit_transform(df_rf.Family.values.reshape(-1,1))
            
            x_cols = list(ohe.categories_[0]) + list(x_.columns[1:])
            
            x = np.concatenate([fam_ohe, x_.iloc[:,1:].values], axis=1)
            
            
            rfr.fit(x, y)
            feature_importances_ = rfr.feature_importances_ * 100
            rfr_dict = dict(zip(x_cols, feature_importances_))
            rfr_dict['r2_oob'] = rfr.oob_score_
            rfr_dict['r2_x'] = rfr.score(x, y)
            
            explainer = shap.TreeExplainer(rfr)
            
            if rfr_df is None:
                rfr_df = pd.DataFrame(rfr_dict, index=[i])
            else:
                rfr_df = pd.concat([rfr_df, pd.DataFrame(rfr_dict, index=[i])])
            if i < 9:
                shap_values = explainer.shap_values(x)
                df_ = pd.DataFrame(shap_values)
                df_.to_csv(f'{path}/10_rfr_{coor}_SHAP_value_{i}_{date}_{loc}_seed_{my_seed}.csv', index=False, sep=',')
            else:
                shap_values2 = explainer(x)
                shap_values22 = shap_values2.values
                df_ = pd.DataFrame(shap_values22)
                df_.to_csv(f'{path}/10_rfr_{coor}_SHAP_value_{i}_{date}_{loc}_seed_{my_seed}.csv', index=False, sep=',')
       
            print(f'Done. Seed: {my_seed}')
            feature_importances = rfr_df.iloc[:,:-2] 
            pd.concat([rfr_df[feature_importances.mean().sort_values().index.values], rfr_df.iloc[:,-2:]], axis=1).to_csv(f'{path}/10_rfr_All_{coor}_{date}_{loc}.csv', index=False, sep=',')           
            my_seed = my_seed+1
            
          
        colname = list(ohe.categories_[0])
        for m in ['Body length', 'CTmax', 'CTmin']:
            colname.append(m)
    
        li = []
        seed = org_seed
        for k in range(9):
            df = pd.read_csv(f'{path}/10_rfr_{coor}_SHAP_value_{k}_{date}_{loc}_seed_{seed}.csv')
            df.columns = colname
            li.append(df)
            seed = seed+1
    
        df_10 = pd.DataFrame(shap_values2.values)
        df_10.columns = colname
        li.append(df_10)
    
        from functools import reduce
        df_sum = reduce(lambda x, y: pd.DataFrame.add(x, y, fill_value=0), li)
        df_sum = df_sum/10
        colname_sum = []
        for n in range(0,len(colname)):
            colname_sum.append(n)
        df_sum.columns = colname_sum
            
        shap_values2.values = df_sum
        shap_values2.feature_names = colname
    
        ## plotting shap
        plt.title(f"{loc}_{coor}")
        shap.plots.bar(shap_values2, show=False)
        plt.tight_layout()
        plt.savefig(f'{path}/{date}_{loc}_{coor}_bar.png', dpi=300)
        plt.close()
        
# In[] Caluculate 'raletive' range size for integrated analysis
loc = 'All'
my_seed = org_seed

df_all['RS_std'] = 0
for loc_ in ['Malaysia', 'Taiwan', 'China']:
  df_std = df_all[df_all.Location == loc_]
  if(loc_ == 'Malaysia'):
      vec_rs_relative = df_std['RS']/1959
  elif(loc_ == 'Taiwan'):
    vec_rs_relative = df_std['RS']/3140
  elif(loc_ == 'China'):
    vec_rs_relative = df_std['RS']/4152
  df_all.RS_std[df_all['Location'] == loc_] = vec_rs_relative

df_rf = df_all

# In[] Random forest model for 'Range Size' in all locations
for coor in ['RS_std']:
    y_columns = ['RS_std']
                    
    df_rf.dropna(axis=0, how='any', inplace=True)
    
    y_ = df_rf[y_columns]
    x_ = df_rf[['Family', 'B_length', 'CTmax', 'CTmin', 'TTrange']]
    
    target = 'RS_std'
    # for target in targets:
    y = y_[target].values
    rfr_df = None
    for i in range(10):
        print(f'Processing... {i+1}', end='\r')
        
        rfr = RandomForestRegressor(n_estimators=500, max_samples=.7, oob_score=True, random_state=my_seed)
        ohe = OneHotEncoder(sparse=False)
        fam_ohe = ohe.fit_transform(df_rf.Family.values.reshape(-1,1))
        
        x_cols = list(ohe.categories_[0]) + list(x_.columns[1:])
        
        x = np.concatenate([fam_ohe, x_.iloc[:,1:].values], axis=1)
        
        
        rfr.fit(x, y)
        feature_importances_ = rfr.feature_importances_ * 100
        rfr_dict = dict(zip(x_cols, feature_importances_))
        rfr_dict['r2_oob'] = rfr.oob_score_
        rfr_dict['r2_x'] = rfr.score(x, y)
        
        explainer = shap.TreeExplainer(rfr)
        # explainer = shap.Explainer(rfr)
        
        if rfr_df is None:
            rfr_df = pd.DataFrame(rfr_dict, index=[i])
        else:
            rfr_df = pd.concat([rfr_df, pd.DataFrame(rfr_dict, index=[i])])
        if i < 9:
            shap_values = explainer.shap_values(x)
            df_ = pd.DataFrame(shap_values)
            df_.to_csv(f'{path}/10_rfr_{target}_SHAP_value_{i}_{date}_{loc}_seed_{my_seed}.csv', index=False, sep=',')
        else:
            shap_values2 = explainer(x)
            shap_values22 = shap_values2.values
            df_ = pd.DataFrame(shap_values22)
            df_.to_csv(f'{path}/10_rfr_{target}_SHAP_value_{i}_{date}_{loc}_seed_{my_seed}.csv', index=False, sep=',')
       
        print(f'Done. Seed: {my_seed}')
        feature_importances = rfr_df.iloc[:,:-2] 
        # rfr_df.to_csv(f'rfr_{target}.csv', index=False, sep=',')
        pd.concat([rfr_df[feature_importances.mean().sort_values().index.values], rfr_df.iloc[:,-2:]], axis=1).to_csv(f'{path}/10_rfr_All_{target}_{date}_{loc}.csv', index=False, sep=',')           
        my_seed = my_seed+1
        
      
    colname = list(ohe.categories_[0])
    # for n in range(1,21):
    #     fam_ = x_cols[n]
    #     colname.append(fam_)
    for m in ['Body length', 'CTmax', 'CTmin', 'TTrange']:
        colname.append(m)

    li = []
    seed = org_seed
    for k in range(9):
        df = pd.read_csv(f'{path}/10_rfr_{target}_SHAP_value_{k}_{date}_{loc}_seed_{seed}.csv')
        df.columns = colname
        li.append(df)
        seed = seed+1

    df_10 = pd.DataFrame(shap_values2.values)
    df_10.columns = colname
    li.append(df_10)

    from functools import reduce
    df_sum = reduce(lambda x, y: pd.DataFrame.add(x, y, fill_value=0), li)
    df_sum = df_sum/10
    colname_sum = []
    for n in range(0,len(colname)):
        colname_sum.append(n)
    df_sum.columns = colname_sum
        
    shap_values2.values = df_sum
    shap_values2.feature_names = colname

    ## workable plotting method
    plt.title(f"{loc}_RS")
    shap.plots.bar(shap_values2, show=False)
    plt.tight_layout()
    plt.savefig(f'{path}/{date}_{loc}_RSstd_bar.png', dpi=300)
    plt.close()

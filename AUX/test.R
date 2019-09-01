
o1=data_integrity_model(data, 'xgboost');print(o1)
o2=data_integrity_model(data, 'randomForest');print(o2)

o3=data_integrity_model(data, 'no_na');print(o3)

data_models
o1$model_selected
o1$checked

san.sancheck(o1)
model.sancheck(o1)



df_status2(data)


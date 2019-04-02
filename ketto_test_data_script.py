import pandas as pd
import pickle

def get_prediction(model_path=None,input_file=None,output_file=None):
    
    print('Loading input file >>>>')
    car_data=pd.read_excel(input_file,encoding='latin-1',parse_dates=['lastSeen'],dayfirst=True,infer_datetime_format=True)

    car_data['car_age']=car_data.lastSeen.dt.year - car_data.yearOfRegistration

    # outlier removal from data

    car_data = car_data[
            (car_data.yearOfRegistration <= 2016) 
          & (car_data.yearOfRegistration >= 1950) 
          & (car_data.powerPS >= car_data.powerPS.quantile(0.01)) 
          & (car_data.powerPS <= car_data.powerPS.quantile(0.99))]

    car_data.drop(car_data.loc[car_data['abtest']=='benzin',:].index,inplace=True)
    keep_these=['golf', 'andere', '3er', 'polo', 'corsa', 'astra', 'passat', 'a4', 'c_klasse',
           '5er', 'e_klasse', 'a3', 'a6', 'focus', 'fiesta', 'transporter', '2_reihe', 'twingo',
           'fortwo', 'vectra', 'a_klasse', '1er', 'clio', 'mondeo', '3_reihe', 'touran', 'punto', 
           'zafira', 'megane', 'ibiza', 'ka', 'lupo', 'x_reihe', 'octavia', 'cooper', 'fabia']
    char_col=['abtest', 'vehicleType', 'gearbox', 'fuelType', 'brand', 'notRepairedDamage','model']
    num_col=[ 'powerPS', 'car_age', 'kilometer']
    
    car_data.loc[~car_data.model.isin(keep_these),'model'] = 'other_cat'
    dummy_data = pd.get_dummies(data=car_data[char_col],columns=char_col,dummy_na=True)
    final_data=pd.concat(objs=[car_data[num_col],dummy_data],axis=1)

    # loading trained randomforest modelfrom disk

    with open(model_path, 'rb') as f:
        model = pickle.load(f)

    results=model.predict(final_data)

    # since i have taken sqrt of the target label

    results=results**2

    # writing results to csv
    print('writing outputs at {}'.format(output_file))
    pd.DataFrame(data={'Index_test_data':final_data.index,'car_price':results}).to_csv(output_file,index=False)
    
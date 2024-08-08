import pandas as pd
import numpy as np

from xgboost import XGBRegressor
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.preprocessing import StandardScaler
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler, OneHotEncoder

# Modelagem: import, cleaning, split & transf *\

def load_and_prepare_data(file_path):
    data_raw = pd.read_excel(file_path)
    X = data_raw.drop('price', axis = 1) 
    y = data_raw['price']
    return X, y

def split_data(X, y, test_size, random_state =123):
    return train_test_split(X, y, test_size = test_size, random_state = random_state)
    
def standardize_data(X_train, X_test):

    # Identificando colunas numéricas e categóricas
    numeric_features = X_train.select_dtypes(include = ['int64', 'float64']).columns
    categorical_features = X_train.select_dtypes(include = ['object', 'category']).columns

    # Transformações
    numeric_transformer = StandardScaler()
    categorical_transformer = OneHotEncoder(handle_unknown = 'ignore')

    # Aplicando as transformações
    
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', numeric_transformer, numeric_features),
            ('cat', categorical_transformer, categorical_features)
        ])
    
    # Ajustando e transformando

    X_train_scaled = preprocessor.fit_transform(X_train)
    X_test_scaled = preprocessor.transform(X_test)

    return X_train_scaled, X_test_scaled, preprocessor


def train_model(model_name, X_train, y_train, n_estimators = 100, random_state = 123):
    if model_name == "XGB":
        model = XGBRegressor(n_estimators = n_estimators, random_state = random_state)
    else:
        model = ExtraTreesRegressor(n_estimators = n_estimators, random_state = random_state)
    
    model.fit(X_train, y_train)
    return model

def evaluate_model(model, X_test, y_test):
    y_pred = model.predict(X_test)
    mse = mean_squared_error(y_test, y_pred)
    rmse = np.sqrt(mse)
    r2 = r2_score(y_test, y_pred)
    return rmse, r2

def feature_importance(model, X):
    feature_importances = model.feature_importances_
    features = X.columns
    importance_df = pd.DataFrame({'Feature': features, 'Importance': feature_importances})
    return importance_df.sort_values(by='Importance', ascending = False)

def predict_and_save(model, file_path, output_path, preprocessor):
    data_test = pd.read_excel(file_path)
    X_test_scaled = preprocessor.transform(data_test)
    predictions = model.predict(X_test_scaled)
    result = pd.DataFrame({'id': data_test.index, 'predicted_price': predictions})
    result.to_csv(output_path, index = False)


# Caminhos dos arquivos *\

train_file_path = "data_final.xlsx"
test_file_path = "data_clean_test.xlsx"
output_file_path = "predictions.csv"

# Parametrização: 

n_test = 0.25
model = "tree"

# Chamar Pipeline 

if __name__ == "__main__": 
    
    X, y = load_and_prepare_data(train_file_path)
    X_train, X_test, y_train, y_test = split_data(X, y, test_size = n_test)
    X_train_scaled, X_test_scaled, preprocessor = standardize_data(X_train, X_test)
    model = train_model(model, X_test_scaled, y_test)
    rmse, r2 = evaluate_model(model, X_test_scaled, y_test)
    print(f"RMSE: {rmse}")
    print(f"R^2: {r2}")

# Exibir importância das features

    importance_df = feature_importance(model, X)
    print(importance_df)

# Fazer predições no conjunto de dados de teste e salvar resultados

    predict_and_save(model, test_file_path, output_file_path, preprocessor)
    print(f"Predições salvas em {output_file_path}")    
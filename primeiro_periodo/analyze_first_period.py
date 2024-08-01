import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.metrics import mean_squared_error, r2_score
import numpy as np

# Função para carregar e preparar os dados

def load_and_prepare_data(file_path):
    data = pd.read_excel(file_path)
    X = data.drop('price', axis=1)
    y = data['price']
    return X, y

# Função para dividir os dados em treino e teste

def split_data(X, y, test_size = 0.25, random_state = 123):
    return train_test_split(X, y, test_size=test_size, random_state=random_state)

# Função para treinar o modelo

def train_model(X_train, y_train, n_estimators = 10, random_state = 123):
    model = ExtraTreesRegressor(n_estimators=n_estimators, random_state=random_state)
    model.fit(X_train, y_train)
    return model

# Função para avaliar o modelo

def evaluate_model(model, X_test, y_test):
    y_pred = model.predict(X_test)
    mse = mean_squared_error(y_test, y_pred)
    rmse = np.sqrt(mse)
    r2 = r2_score(y_test, y_pred)
    return rmse, r2

# Função para exibir a importância das features

def feature_importance(model, X):
    feature_importances = model.feature_importances_
    features = X.columns
    importance_df = pd.DataFrame({'Feature': features, 'Importance': feature_importances})
    return importance_df.sort_values(by='Importance', ascending=False)

# Função para carregar dados de teste e fazer predições

def predict_and_save(model, file_path, output_path):
    data_test = pd.read_excel(file_path)
    predictions = model.predict(data_test)
    result = pd.DataFrame({'id': data_test.index, 'predicted_price': predictions})
    result.to_csv(output_path, index=False)

# PIPELINE ///////////////////////////////////////////////////////////////////////////
# Caminhos dos arquivos

train_file_path = "data_clean_train.xlsx"
test_file_path = "data_clean_test.xlsx"
output_file_path = "predictions.csv"

# Carregar e preparar os dados

X, y = load_and_prepare_data(train_file_path)

# Dividir os dados em treino e teste

X_train, X_test, y_train, y_test = split_data(X, y)

# Treinar o modelo

model = train_model(X_train, y_train)

# Avaliar o modelo

rmse, r2 = evaluate_model(model, X_test, y_test)
print(f"RMSE: {rmse}")
print(f"R^2: {r2}")

# Exibir importância das features

importance_df = feature_importance(model, X)
print(importance_df)

# Fazer predições no conjunto de dados de teste e salvar resultados

predict_and_save(model, test_file_path, output_file_path)
print(f"Predições salvas em {output_file_path}")

#' @title CIS IEEE UNB - Mall Customer Segmentation Data
#' @author Luiz Paulo Tavares Gonçalves 

import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
import janitor

from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans

# Functions *\
# get_load_cleaned: imports, cleans, and organizes the dataset  

def get_load_cleaned(db: str) -> pd.DataFrame:
    data_raw = pd.read_csv(db)

    data_cleaned = data_raw \
                  .clean_names().drop(columns = ['customerid']) \
                  .rename(columns = {
                               "annual_income_k$_": "income", 
                               "spending_score_1_100_": "score" 
               }
            )
    return data_cleaned

# get_standardize: simply standardize the data 

def get_standardize(data_cleaned): 

    """"
    Standardize only numeric columns 
    """

    scaler = StandardScaler()

    numeric_cols = data_cleaned.select_dtypes(include = ['number']).columns
    data_cleaned[numeric_cols] = scaler.fit_transform(data_cleaned[numeric_cols])
    return data_cleaned
    
# Clustering: apply the Elbow method to determine the ideal number of clusters 

def get_clusters(data_clean, k_optimal): 
    kmeans = KMeans(n_clusters = k_optimal, random_state = 123)
    clusters = kmeans.fit_predict(data_clean)
    data_clean['cluster'] = clusters

    # Plotar o gráfico com os clusters

    plt.figure(figsize = (10, 8))

    sns.scatterplot(data = data_clean, 
                    x = 'income', 
                    y = 'score',
                    hue = 'cluster', palette = 'viridis', legend='full')
    
    plt.title('Clusters Formados pelo K-means')
    plt.xlabel('Renda')
    plt.ylabel('Score')
    plt.grid(True)
    plt.show()

    return data_clean


def get_sse(data_clean, k_max):

    sse = []
    k_range = range(1, k_max + 1)

    for k in k_range: 
        kmeans = KMeans(n_clusters = k, random_state = 123)
        kmeans.fit(data_clean)
        sse.append(kmeans.inertia_)

    return sse

# get_optimal_k: optimal point for the k 
# Forma analítica 

def get_optimal_k(sse): 
    
    """"
    Optimal point for the k
    Finding the second difference is the smallest 
    """
    second_diff = np.diff(np.diff(sse))
    optimal_k = np.argmin(second_diff) + 2
    return optimal_k



# Pipeline *\*




if __name__ == "__main__": 

    # Importing and visualizing (number 1)

    data_cleaned = get_load_cleaned(db = "mall_customers.csv")

    print(data_cleaned)
    print(data_cleaned.describe())

    sns.pairplot(data = data_cleaned, hue = 'gender')
    plt.show()
        
    # Standardizing and selecting variables (number 2)

    data_scaled = get_standardize(data_cleaned)
    data_select = data_scaled[["income", "score"]]
    
    # Clustering: K-means (number 3 e 4)

    k = 10
    sse_values = get_sse(data_clean = data_select, k_max = k)

    plt.plot(range(1, k+1), sse_values, marker='o')
    plt.xlabel('Número de Clusters')
    plt.ylabel('Soma dos Erros Quadráticos (SSE)')
    plt.title('Método do Cotovelo para Determinar o Número Ideal de Clusters')
    plt.grid(True)
    plt.show()

    # Clustering with k-max = 10

    get_clusters(data_clean = data_select, k_optimal = k)

    # Clustering: optimal point for the k 

    print(sse_values)
    optimal_k = get_optimal_k(sse_values)
    print(f"O número ideal de clusters é: {optimal_k}")

    # with the ideal number of clusters 

    get_clusters(data_clean = data_select, k_optimal = 3)
    
   

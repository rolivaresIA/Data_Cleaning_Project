# Limpieza y Transformación de datos usando R

## 📝 Contexto

Una gran parte del trabajo en ciencia de datos consiste en obtener datos crudos y prepararlos para el análisis. Algunos estiman que los científicos de datos dedican el 80% de su tiempo a limpiar y manipular datos, y solo el 20% a analizarlos o construir modelos a partir de ellos. Cuando recibimos datos en bruto, es necesario realizar varias tareas antes de poder analizarlos, lo que podría incluir:

<li> Diagnosticar la "limpieza" de los datos, es decir, cuánto trabajo de limpieza será necesario.
<li> Reorganizar los datos, asegurándonos de tener las filas y columnas adecuadas para un análisis efectivo.
<li> Combinar múltiples archivos en uno solo.
<li> Cambiar los tipos de valores, como corregir columnas donde los valores numéricos están almacenados como cadenas de texto.
<li> Eliminar o completar valores faltantes, manejando datos incompletos o ausentes.
<li> Manipular cadenas de texto para representar mejor la información contenida en los datos.

![](https://github.com/rolivaresIA/Data_Cleaning_Project/blob/main/Images/datacleaning.png)

## 📋 Descripción del Proyecto 

Este proyecto de limpieza y transformación de datos en R tiene como propósito central optimizar la calidad y precisión de los datos manejados por las organizaciones. En este caso, el enfoque está en Shein, un destacado ecommerce a nivel global. Dado que los datos son esenciales para tomar decisiones estratégicas y elaborar reportes confiables, es crucial asegurarse de que estén libres de errores, duplicidades e inconsistencias.

## 🎯 Ojetivo del Proyecto

Analizar las diferentes fuentes de datos de Shein, para luego, transformarlos, limpiarlos y generar una fuente de datos consolidada y fidedigna que por consecuencia, permitirá tomar decisiones óptimas al negocio.

## 💡 Desarrollo del Proyecto

### Paso 1: Recolección de datos 🗃️
- **Fuente de los datos:** Los datos fueron obtenidos de [Kaggle](https://www.kaggle.com/datasets/oleksiimartusiuk/e-commerce-data-shein/data) en el dataset "Dirty E-Commerce Data [80,000+ Products]"
- **Descripción:** El dataset incluye variables como `producto`, `precio`, `descuento`, `subcategoria del producto`, etc. 
- **Formato:** [21 archivos](https://github.com/rolivaresIA/Data_Cleaning_Project/tree/main/original_databases) .CSV los cuales se consolidaron en un total de 82,105 observaciones y 13 variables.

### Paso 2: Limpieza de datos 🧹
- **Modificación y estandarización de nombres de variables:** Nombres de variables con símbolos extras, mayúsculas, etc.
- **Modificación de las clases (tipos) de variables:** Variables con clases inadecuadas.
- **Análisis y transformación de valores faltantes en las variables:** Algunas variables presentaban 100% de NA, siendo eliminadas, otras fueron reemplazadas, etc.

### Paso 3: Eliminación de duplicados ✂️
- **Se identificaron y eliminaron observaciones duplicadas**
- **Productos duplicados con diferentes precios:** Se elige el precio mayor.

### Paso 4: Descarga archivo consolidado 🔗
- Una vez limpia y transformada la data se puede descargar de la carpeta [final_database](https://github.com/rolivaresIA/Data_Cleaning_Project/tree/main/final_datebase)
  

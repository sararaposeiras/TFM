# Comparación de metodologías para el modelado espacial: caso de estudio en el ámbito de la gestión pesquera
Repositorio de GitHub asociado al TFM del Máster en Técnicas Estadísticas titulado *Comparación de metodologías para el modelado espacial: caso de estudio en el ámbito de la gestión pesquera*.

## Descripción
En este repositorio se recogen el código y los resultados generados para el estudio de simulación llevado a cabo para la realización del TFM.

El objetivo del trabajo es comprender mejor el comportamiento de los modelos de regresión espacial ajustados mediante INLA. Para ello, se analizan distintos modelos espaciales bajo diferentes escenarios de simulación en los que se generan datos que presentan distintas estructuras de dependencia espacial. Se evalúa cómo la especificación de los efectos espaciales en el predictor del modelo afecta al ajuste, la capacidad predictiva y la identificación y estimación de la estructura espacial subyacente, así como el efecto de otros factores como el esquema de muestreo utilizado, el tamaño muestral o la presencia de dependencia espacial entre las componentes espaciales simuladas.

## Estructura
### SCRIPTS
Contiene todo el código desarrollado para el estudio.

La estructura interna se organiza por escenarios de simulación y, cuando es necesario, por casos específicos dentro de cada escenario.

Además, incluye una carpeta llamada *metodologia* en la que se incluyen los scripts utilizados para generar las figuras usadas en la sección metodológica de la memoria.

### OUTPUTS
Contiene todos los resultados generados por los scripts.

Se divide en tres subcarpetas:

* **HTMLS**: contiene las salidas en formato HTML.
* **PLOTS**: contiene las figuras generadas durante el análisis.
* **TABLES**: contiene las tablas de resultados generadas durante el análisis.

La organización interna de estas carpetas replica la estructura utilizada en Scripts, facilitando la asociación de cada resultado con el código que lo genera.

### Software utilizado
El código fue desarrollado en R utilizando el paquete R-INLA para la implementación práctica de la metodología INLA en el ajuste de modelos de regresión espaciales bayesianos.

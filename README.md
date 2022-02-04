

## Informe Proyecto de la asignatura de Simulación y Programación Declarativa

Autor: Yan Carlos González Blanco C-411

### 1-Ideas seguidas para la solución del problema

Para la solución del problema me basé en la bibliografía sobre agentes y las conferencias vistas en clase, en las cuales me apoyé para la modelación tanto de los agentes como del ambiente, los cuales serán explicados más adelante con profundidad su funcinamiento.

### 2-Modelos de Agentes

Para los robots de casa se utilizaron dos modelos, los cuales son reactivos diferenciándoses entre estos por la prioridad de sus categorías.

Para el primer agente al cual llamaremos CargaNino se siguieron el siguiente orden de prioridades para su implementación:

1- Si el robot está parado en una posición donde se encuentra una suciedad este pasa de forma inmediata a limpiarla.

2- Si el robot se encuantra cargando un niño y este puede llevarlo hacia el corral se mueve hacia la casilla que mas lo acerque a este.

3- Si el robot no está cargando a ningún niño y existe un niño al cual él puede llevar hacia el corral, entonces se mueve hacia el niño más cercano.

4- Si existe basura en el ambiente y el robot puede alcanzar alguna de estas, entonces este se mueve a la casilla que más lo acerque a esta. 

5- Si el robot no puede realizar niniguna de las anteriores se queda en su lugar.

Es decir que la función principal de este agente será tratar de llevar los niños primero al corral, para de esta forma eliminar la fuente de la suciedad, y mientras lleva a los niños si en el camino coincide con alguna casilla sucia este pasa a limpiarla. Si ya una vez que todos los niños se encuentre ubicados en el corral entonces pasaría a eliminar la basura de forma directa.

Para el segundo agente al cual llamaremos Limpiador se siguieron de forma similar al anterior el orden de prioridades solamente cambiando la prioridad 3 por la 4.

Este agente funciana contrario del anterior, el cual primero trata de eliminar la basura de forma directa y una vez que el ambiente este completamente limpio pasaría a tratar de llevar los niños al corral.

Los niños fueron modeldos realizando el movimiento de forma aleatoria a una de las 8 direcciones posibles de las casillas adyacentes a este.



### 3-Detalles de la implementación 

Como requisito del proyecto la implementación fue realizada en el lenguaje de programación Haskell basado en el paradigma funcional, para la faciliad de la creación y ejecución de este se utilizó la herramienta Stack. Para ejecutar el proyecto es necesario tenerlo instalado, una vez instalado, y descargado el proyecto, en el directorio de este ejecutar:

```bash
stack build && stack exec haskell-example-exe 
```

 Una vez corrido se mostrará el resulatado de las simulaciones realizadas.

Para la modelación del ambiente se creó un nuevo tipo de dato llamado Ambiente en el cual se encontrarán las dimensiones del tablero, una lista de niños, una de obstáculos, una de suciedad, una de los roboces, y una para el corral, para poder identificar las posiciones que ocuparían estos en el ambiente. Para crear el corral se utilizó un algoritmo BFS que se detiene una vez sean visitadas la cantidad de casillas que va ha tener el corral, de esta forma se garantiza que el corral este conectado y se cree de la forma más cuadrada posible. 

Para la implementación de los roboces se utilizó un algoritmo BFS para encontrar tanto los niños, la suciedad como el corral que se encuentre más cerca de este, para de esta forma garantizar que el robot tenga que moverse lo menos posible para cumplir su objetivo, ya sea de buscar al niño para llevarlo al corral, llevarlo directamente al corral o buscar la suciedad para limpiarla. Para llevar los niños al corral y poder garantizar que el corral no se cierre y queden posiciones vacias en el interior, el robot trata de llevar al niño a la posicion del corral alcanzable por él que más cerca este del centro de este, para grantizar que el corral se valla creando por niveles,es decir, hasta que un nivel no sea haya completado no se pase a poner niños en el otro nivel más externo.   

La variación del ambiente ocurre totalmente aleatoria, una vez que ocurre está todo el ambiente cambia,  moviendo todo de lugar, incluyendo el corral y los roboces,  la cantiad de basura que se encontraba antes de la variación se mantiene después de esta, lo que esta es cambiada de lugar. Todos los niños que se encuentren dentro del corral una vez ocurra esta son sacados del corral.

### 4-Simulaciones 

Para las simulaciones se realizaron 3 tipos de ambientes con diferentes parámetros, realizando para cada uno de estos 2 simulaciones con valores del parámetro t distinto (100,50) y con cada uno de los Agentes (CargaNiño,Limpiador),todas las simulaciones fueron realizadas con 1000 turnos.



Datos de la ejecución:

#### Ejecución #1

$7\ niños,5 \ suciedad,5\ obstaculos$

$t=100$

| Craga Niño         | Limpiador          |
| ------------------ | ------------------ |
| 25.28735632183908  | 25.0               |
| 26.436781609195403 | 33.333333333333336 |
| 18.39080459770115  | 29.41176470588235  |
| 11.494252873563218 | 16.27906976744186  |
| 6.896551724137931  | 41.1764705882353   |
| 2.2988505747126435 | 20.930232558139537 |
| 10.344827586206897 | 27.906976744186046 |
| 14.942528735632184 | 10.344827586206897 |
| 12.64367816091954  | 32.18390804597701  |
| 18.39080459770115  | 20.689655172413794 |
| 37.93103448275862  | 25.58139534883721  |
| 44.827586206896555 | 34.11764705882353  |
| 14.942528735632184 | 31.3953488372093   |
| 4.597701149425287  | 19.767441860465116 |
| 29.885057471264368 | 31.03448275862069  |
| 17.24137931034483  | 20.689655172413794 |
| 19.54022988505747  | 16.470588235294116 |
| 20.689655172413794 | 10.344827586206897 |
| 25.28735632183908  | 31.03448275862069  |
| 22.988505747126435 | 20.238095238095237 |
| 25.28735632183908  | 16.091954022988507 |
| 17.24137931034483  | 24.41860465116279  |
| 8.045977011494253  | 23.25581395348837  |
| 11.494252873563218 | 5.813953488372093  |
| 18.39080459770115  | 26.74418604651163  |
| 10.344827586206897 | 21.59090909090909  |
| 24.137931034482758 | 39.53488372093023  |
| 18.39080459770115  | 12.941176470588236 |
| 6.89655172413793   | 17.441860465116278 |
| 10.44483759620689  | 20.5090909090909   |
| 0/30               | 0/30               |



$t=50$



| Carga Niño         | Limpiador          |
| ------------------ | ------------------ |
| 53.48837209302326  | 49.397590361445786 |
| 55.294117647058826 | 57.83132530120482  |
| 50.588235294117645 | 65.06024096385542  |
| 58.8235294117647   | 36.470588235294116 |
| 51.76470588235294  | 49.397590361445786 |
| 50.0               | 47.61904761904762  |
| 54.651162790697676 | 52.94117647058823  |
| 48.83720930232558  | 44.705882352941174 |
| 62.35294117647059  | 48.78048780487805  |
| 58.333333333333336 | 56.09756097560975  |
| 50.0               | 40.963855421686745 |
| 57.47126436781609  | 58.02469135802469  |
| 67.44186046511628  | 44.04761904761905  |
| 53.48837209302326  | 48.19277108433735  |
| 57.142857142857146 | 62.19512195121951  |
| 57.142857142857146 | 48.19277108433735  |
| 48.83720930232558  | 48.78048780487805  |
| 70.93023255813954  | 45.88235294117647  |
| 61.904761904761905 | 49.397590361445786 |
| 60.46511627906977  | 53.01204819277108  |
| 54.21686746987952  | 42.68292682926829  |
| 54.76190476190476  | 48.80952380952381  |
| 65.11627906976744  | 49.397590361445786 |
| 57.47126436781609  | 53.65853658536585  |
| 60.0               | 51.19047619047619  |
| 55.294117647058826 | 56.626506024096386 |
| 60.0               | 50.0               |
| 57.83132530120482  | 37.34939759036145  |
| 60.97560975609756  | 51.19047619047619  |
| 54.284317647058826 | 56.736505025096376 |
| 7/30               | 2/30               |



#### Ejecución #2

$ 7 \ niños\ 10\ basuras\ 10\ obstáculos$

$t=100$



| Carga Niño         | Limpiador          |
| ------------------ | ------------------ |
| 19.51219512195122  | 12.048192771084338 |
| 9.75609756097561   | 22.22222222222222  |
| 8.536585365853659  | 23.75              |
| 13.414634146341463 | 29.62962962962963  |
| 29.26829268292683  | 13.414634146341463 |
| 28.048780487804876 | 25.301204819277107 |
| 24.390243902439025 | 12.195121951219512 |
| 30.48780487804878  | 26.25              |
| 7.317073170731708  | 16.25              |
| 6.097560975609756  | 13.25301204819277  |
| 24.390243902439025 | 27.848101265822784 |
| 43.20987654320987  | 22.22222222222222  |
| 13.414634146341463 | 8.641975308641975  |
| 26.829268292682926 | 20.987654320987655 |
| 10.975609756097562 | 6.097560975609756  |
| 20.73170731707317  | 28.048780487804876 |
| 4.878048780487805  | 20.987654320987655 |
| 25.609756097560975 | 24.390243902439025 |
| 24.390243902439025 | 29.62962962962963  |
| 23.170731707317074 | 24.390243902439025 |
| 19.51219512195122  | 32.142857142857146 |
| 30.48780487804878  | 20.481927710843372 |
| 0.0                | 23.170731707317074 |
| 26.829268292682926 | 25.0               |
| 17.073170731707318 | 29.62962962962963  |
| 0.0                | 21.25              |
| 10.975609756097562 | 32.926829268292686 |
| 9.75609756097561   | 20.73170731707317  |
| 14.634146341463415 | 19.51219512195122  |
| 12.314534147341462 | 24.170631707327075 |
| 0/30               | 0/30               |



$t = 50$

| CargaNiño          | Limpiador          |
| ------------------ | ------------------ |
| 56.09756097560975  | 46.05263157894737  |
| 60.75949367088607  | 47.43589743589744  |
| 62.96296296296296  | 47.36842105263158  |
| 57.5               | 40.78947368421053  |
| 58.22784810126582  | 49.35064935064935  |
| 56.25              | 52.5               |
| 58.97435897435897  | 47.36842105263158  |
| 60.75949367088607  | 50.63291139240506  |
| 55.12820512820513  | 52.56410256410256  |
| 55.69620253164557  | 50.63291139240506  |
| 53.75              | 36.36363636363637  |
| 58.02469135802469  | 62.33766233766234  |
| 61.72839506172839  | 53.246753246753244 |
| 55.0               | 45.0               |
| 58.75              | 51.89873417721519  |
| 63.75              | 55.84415584415584  |
| 48.717948717948715 | 52.5               |
| 54.43037974683544  | 49.35064935064935  |
| 59.49367088607595  | 31.57894736842105  |
| 56.09756097560975  | 57.5               |
| 53.164556962025316 | 40.25974025974026  |
| 62.0253164556962   | 48.10126582278481  |
| 51.89873417721519  | 48.717948717948715 |
| 61.53846153846154  | 35.526315789473685 |
| 50.617283950617285 | 54.54545454545455  |
| 55.55555555555556  | 52.56410256410256  |
| 56.79012345679013  | 45.45454545454545  |
| 57.5               | 50.63291139240506  |
| 55.0               | 57.69230769230769  |
| 47.617848517944712 | 52.63291439340507  |
| 7/30               | 1/30               |



#### Ejecución #3

 $10 \ niños \ 20 \ obstáculos \ 20 \ basuras$ 

$t=100$

| Carga Niño         | Limpiador          |
| ------------------ | ------------------ |
| 45.45454545454545  | 30.434782608695652 |
| 33.80281690140845  | 49.23076923076923  |
| 41.791044776119406 | 63.07692307692308  |
| 42.028985507246375 | 38.46153846153846  |
| 56.71641791044776  | 31.818181818181817 |
| 48.57142857142857  | 40.298507462686565 |
| 30.0               | 37.878787878787875 |
| 41.42857142857143  | 32.83582089552239  |
| 46.26865671641791  | 40.0               |
| 42.64705882352941  | 43.93939393939394  |
| 55.88235294117647  | 26.865671641791046 |
| 47.142857142857146 | 41.1764705882353   |
| 37.142857142857146 | 33.333333333333336 |
| 38.23529411764706  | 41.791044776119406 |
| 53.73134328358209  | 34.32835820895522  |
| 45.714285714285715 | 45.588235294117645 |
| 46.3768115942029   | 31.818181818181817 |
| 43.47826086956522  | 39.39393939393939  |
| 26.08695652173913  | 30.76923076923077  |
| 38.23529411764706  | 30.76923076923077  |
| 21.73913043478261  | 25.37313432835821  |
| 30.434782608695652 | 26.865671641791046 |
| 37.3134328358209   | 53.84615384615385  |
| 42.64705882352941  | 40.298507462686565 |
| 57.971014492753625 | 36.76470588235294  |
| 40.57971014492754  | 39.130434782608695 |
| 44.285714285714285 | 30.88235294117647  |
| 52.857142857142854 | 38.80597014925373  |
| 44.285714285714285 | 40.298507462686565 |
| 75.38461538461539  | 34.84848484848485  |
| 1/30               | 1/30               |



$t=50$

| Carga Niño         | Limpiador          |
| ------------------ | ------------------ |
| 64.0625            | 66.66666666666667  |
| 63.492063492063494 | 62.295081967213115 |
| 61.904761904761905 | 57.377049180327866 |
| 66.66666666666667  | 58.064516129032256 |
| 75.0               | 61.29032258064516  |
| 61.904761904761905 | 63.492063492063494 |
| 64.17910447761194  | 62.903225806451616 |
| 66.66666666666667  | 54.83870967741935  |
| 73.01587301587301  | 66.12903225806451  |
| 62.5               | 54.83870967741935  |
| 70.76923076923077  | 64.51612903225806  |
| 66.15384615384616  | 67.21311475409836  |
| 73.84615384615384  | 65.07936507936508  |
| 67.1875            | 63.9344262295082   |
| 70.3125            | 73.01587301587301  |
| 66.12903225806451  | 55.73770491803279  |
| 70.76923076923077  | 57.142857142857146 |
| 71.875             | 60.65573770491803  |
| 68.75              | 59.375             |
| 74.60317460317461  | 64.51612903225806  |
| 64.61538461538461  | 66.66666666666667  |
| 66.66666666666667  | 60.0               |
| 78.125             | 62.903225806451616 |
| 72.3076923076923   | 72.58064516129032  |
| 62.5               | 64.51612903225806  |
| 81.53846153846153  | 53.96825396825397  |
| 68.18181818181819  | 57.8125            |
| 72.58064516129032  | 65.57377049180327  |
| 65.07936507936508  | 63.9344262295082   |
| 67.1875            | 70.3125            |
| 30/30              | 21/30              |



Como podemos observar para valores más pequeños del parámetro t el robot encargado de primero llevar los niños al corral obtiene peores resultados que el que se encarga primero de limpiar, lo cual es algo lógico por la forma en que varia el ambiente ya que al variar este todos los niños son sacados del corral, por lo que el robot pasará todo el tiempo tratando de llevar a los niños al corral y estos saliendo por lo que no dedica mucho tiempo a limpiar de forma directa, para valores más grandes de t el encargado de llevar los niños primero obtiene generalmente mejores resultados ya que este tiene tiempo para llevar los niños al corral y comenzar a limpiar de forma directa y eliminar la fuente de suciedad, también podemos observar en la tercera ejecución que el segundo robot obtiene mejores resultados por lo que podemos ver que para un ambiente inicial con un mayor número de suciedad y obstáculos el robot encargado de Limpiar primero obtiene resultados un poco mejores que el otro robot.












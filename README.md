## Informe Proyecto de la asignatura de Simulacion y Programcion Declarativa

Autor: Yan Carlos Gonzalez Blanco C-411

### 1-Ideas seguidas para la solución del problema

Para la solucion del problema nos basamos en la blibiografia sobre agentes y las conferencias vistas en clase, en las cuales nos apyamos para la modelacion tanto de los agentes como del ambiente, los cuales serán explicado mas adelante con profundidad su funcinamiento.

### 2-Modelos de Agentes

Para los robots de casa se utilizaron dos modelos, los cuales son reactivos diferenciandoses entre estos por la prioridad de sus categorias.

Para el primer agente al cual llamaremos CargaNino se siguieron el siguiente orden de prioridades para su implementación:

1- Si el robot esta parado en una posicion donde se encuentra una suciedad este pasa de forma inmediata a limpiarla.

2- Si el robot se encuantra cargando un nino y este puede llevarlo hacia el corral se mueve hacia la casilla que mas lo acerque a este.

3- Si el robot no esta cargando a ningun nino y existe un nino al cual el puede llevar hacia el corral, entonces se mueve hacia el nino mas cercano.

4- Si existe basura en el ambiente y el robot puede alcanzar alguna de estas, entonces este se mueve a la casilla que mas lo acerque a esta. 

5- Si el robot no puede realizar niniguna de las anteriores se queda en su lugar.

Es decir que la funcion principal de este agente sera tratar de llevar los ninos primero al corral, para de esta forma eliminar la fuente de la suciedad, y mientras lleva a los ninos si en el camino coincide con alguna casilla sucia este pasa a limpiarla. Si ya una vez que todos los ninos se encuentre ubicados en el corral entonces pasaria a eliminar la basura de forma directa.

Para el segundo agente al cual llamaremos Limpiador se siguieron de forma similar al anterior el orden de prioridades solamente cambiando la prioridad 3 por la 4.

Este agente funciana contrario del anterior, el cual primero trata de elminar la basura de forma directa y una ves que el ambiente este completamente limpio pasaria a tratar de llevar los ninos al corral.

Los ninos fueron modeldos realizando el movimiento de forma aleatoria a una de las 8 direcciones posibles de las casillas adyacentes a este 

​       


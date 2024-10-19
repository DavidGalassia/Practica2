Hecho por David Arismendy

# Distribucion de herencia con porcentajes auto balanceados

El codigo en prolog es util para saber como se distribuye la herencia de alguien teniendo en cuenta filtros por niveles de consangunidad. Como los porcentajes se acumulan sumandose, si alguna vez pasan el 100%, estos se autobalancean para poder incluir a todas las personas de la herencia.

## Uso

1. Crear todas las relaciones familiares.
2. Crear las reglas de consangunidad (Cuanto porcentaje implica cada tipo de relacion).
1. Llama a la función principal para calcular la distribución de la herencia:

   ```prolog
   distributeInheritance(Individual, TotalInheritance, Distribution).

# Casos de uso:
1. 

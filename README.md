# Inheritance Distribution System

Este proyecto en Prolog implementa un sistema para calcular y distribuir una herencia entre familiares según sus niveles de consanguinidad. La distribución se basa en una estructura familiar definida y permite calcular porcentajes y montos específicos para cada individuo.

## Funcionalidades

- Define relaciones familiares (padres, hijos, abuelos, tíos, etc.).
- Calcula niveles de consanguinidad entre individuos.
- Distribuye una herencia total entre los miembros de la familia según los niveles de consanguinidad.
- Ajusta los porcentajes de herencia si el total supera el 100%.
- Imprime el monto que recibe cada individuo y su porcentaje del total.

## Estructura del Código

- **Relaciones familiares**: Define las relaciones entre los individuos de la familia usando hechos para padres e hijos.
- **Niveles de consanguinidad**: Se definen reglas para determinar el nivel de parentesco entre dos individuos.
- **Distribución de herencia**: Se calculan los montos a distribuir según los niveles de consanguinidad y se ajustan si es necesario.
- **Impresión de resultados**: Se formatean e imprimen los resultados de la distribución en un formato legible.

## Uso

1. Asegúrate de tener Prolog instalado en tu sistema.
2. Carga el archivo en tu entorno de Prolog.
3. Llama a la función principal para calcular la distribución de la herencia:

   ```prolog
   distributeInheritance(Individual, TotalInheritance, Distribution).

# devF_proyecto
Predicción de Fechas y Horarios de Alto Flujo.
Proyecto DevF, DATA SCIENCE

DESCRIPCION GENERAL
Análisis de una base de datos relacionada con las transacciones en las estaciones de servicio (bencineras), para detectar picos de flujo (Horarios y Fechas). Calcular la eficiencia de cada puesto de trabajo y realizar una predicción para fortalecer los turnos de trabajo en los momentos de mayor flujo.

ESPECIFICACIONES
Assets\Base de datos en formato csv, con transacciones diarias
Entorno RStudio
Generar branch -¡??=)(/&%!"#$%&/()=_:[-,GitHub https://github.com/PatoEche/devF_proyecto.git


CONTEXTO EMPRESARIAL

Es una empresa retail de tiendas de conveniencia y venta de combustible (Estación de Servicio de Combustibles/Store).
Funciona 365/24/7 en sus dos áreas Combustible y Tienda.
La cantidad de colaboradores (trabajadores) se establece principalmente en base a las ventas, específicamente el promedio mensual de ventas sobre el margen de utilidad. En promedio, una estación de servicio en el área de combustible trabaja entre 10 a 15 personas, mientras que en tienda existe un promedio de 8 a 12 personas.
En el país, existe leyes laborales estrictas respecto a la cantidad de horas que puede trabajar un colaborador, entre ellas deben ser consideradas las siguientes:
    Una persona no debe trabajar más de 45 horas semanales, las semanas son consideradas entre día libre y libre.
    El máximo de horas es de 12 diarias (Esto considera 2 horas extras, del horario normal de 10 horas, es decir, se consideran 2 horas con recargo del 100%)
    Durante su jornada diaria existe un descanso de 30 minutos cada 7 horas.
    En turnos rotativos (es el caso de la estación de servicio), el personal debe tener una separación entre turnos de al mínimo 12 horas. 
    Además debe tener al menos 2 domingos libres al mes.
    Los feriados deben ser compensados, por otros días dentro del mes en curso.
Lo anterior, junto a la incertidumbre respecto al flujo que se generará en cada jornada laboral, rigidiza la posibilidad de estructurar los turnos de los colaboradores de una manera eficiente.

OBJETIVO GENERAL
Necesidad de construir un modelo que nos permita predecir los picos de flujo en las transacciones (Horas, días, fechas especiales).

Consideración IMPPORTANTE [Solo desarrollaremos flujos en el área de combustible, por tanto deberemos eliminar las transacciones de tienda.]


INCOGNITAS INICALES

1.-¿Hora del día con mayor flujo?
2.-¿Días de la semana con mayores flujo?
3.-¿Días del Mes con mayores flujos?
4.-¿Promedio de venta diario por colaborador?
5.-¿Horas diarias por colaborador?
6.-¿Horas mensuales por colaborador?
7.-Calcular la eficiencia diaria del colaborador, en este caso, equivalente al valor del ticket por minuto del colaborador
    promedioColaboradorDiario = Total venta diaria del colaborador / transacciones diarias realizadas
    promedioTicket =Total transacción diaria / Número de colaboradores
    promedioColaboradorMinuto = promedioColaboradorDiario  / minutos diarios trabajados
    valorTicketMinutoAtendedor = promedioColaboradorMinuto / ventas totales colaborador
8.-¿Cantidad mínima de colaboradores necesarios para cubrir transacciones diarias?


use practica1;

SET @@SESSION.sql_mode='ALLOW_INVALID_DATES';

LOAD DATA INFILE 'D:\\Desarrollo\\Diplomado\\Base de datos\\07 Datos TXT para cargar tablas\\07_datosTXT\\articulos.txt'
INTO TABLE articulos
FIELDS TERMIANTED BY '|'
LINES TERMINATED BY '\n';

LOAD DATA INFILE 'D:\\Desarrollo\\Diplomado\\Base de datos\\07 Datos TXT para cargar tablas\\07_datosTXT\\clientes.txt'
INTO TABLE clientes
FIELDS TERMINATED BY '|'
LINES TERMINATED BY '\n';

LOAD DATA INFILE 'D:\\Desarrollo\\Diplomado\\Base de datos\\07 Datos TXT para cargar tablas\\07_datosTXT\\estados.txt'
INTO TABLE estados
FIELDS TERMINATED BY '|'
LINES TERMINATED BY '\n'
(estado, nombre);

LOAD DATA INFILE 'D:\\Desarrollo\\Diplomado\\Base de datos\\07 Datos TXT para cargar tablas\\07_datosTXT\\inventarios.txt'
INTO TABLE inventarios
FIELDS TERMINATED BY '|'
LINES TERMINATED BY '\n';

LOAD DATA INFILE 'D:\\Desarrollo\\Diplomado\\Base de datos\\07 Datos TXT para cargar tablas\\07_datosTXT\\ordenes.txt'
INTO TABLE ordenes
FIELDS TERMINATED BY '|'
LINES TERMINATED BY '\n'
(num_orden, @fecha_orden, num_cliente, instrucciones, disponible, num_pedido, @fecha_envio, peso_envio, cargo_envio, @fecha_pago)
SET fecha_orden = STR_TO_DATE(@fecha_orden, '%d/%m/%Y'),
    fecha_envio = STR_TO_DATE(@fecha_envio, '%d/%m/%Y'),
    fecha_pago = STR_TO_DATE(@fecha_pago, '%d/%m/%Y');

LOAD DATA INFILE 'D:\\Desarrollo\\Diplomado\\Base de datos\\07 Datos TXT para cargar tablas\\07_datosTXT\\proveedores.txt'
INTO TABLE proveedores
FIELDS TERMINATED BY '|'
LINES TERMINATED BY '\n';

SELECT count(*) FROM articulos;
SELECT count(*) FROM clientes;
SELECT count(*) FROM estados;
SELECT count(*) FROM inventarios;
SELECT count(*) FROM ordenes;
SELECT count(*) FROM proveedores;
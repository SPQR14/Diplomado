create database practica1;
use practica1;
create table if not exists estados(
estado char(2),
nombre char(15)
);
create table if not exists clientes(
num_cliente int,
nombre char(15),
apellidos char(15),
compania char(20),
direccion1 char(20),
direccion2 char(20),
ciudad char(15),
estado char(2),
cp char(5),
telefono char(18)
);
create table if not exists ordenes(
num_orden int,
fecha_orden date,
num_cliente int,
instrucciones char(40),
disponible char(1),
num_pedido char(10),
fecha_envio date,
peso_envio decimal(8,2),
cargo_envio decimal(6,2),
fecha_pago date
);
create table if not exists articulos(
num_articulo smallint,
num_orden int,
num_inventario smallint,
codigo_proveedor char(3),
cantidad smallint,
precio_total decimal(8,2)
);
create table if not exists inventarios(
num_inventario smallint,
codigo_proveedor char(3),
descripcion char(15),
precio_unitario decimal(6,2),
unidad char(4),
descripcion_unidad char(15)
);
create table if not exists proveedores(
codigo_proveedor char(3),
nombre_proveedor char(15)
);
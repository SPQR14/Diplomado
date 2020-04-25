-- Practica 9
use store;

select c.num_cliente, compania, num_orden
from clientes c join ordenes o
on c.num_cliente = o.num_cliente; -- Alberto Pico Lara


select c.num_cliente, compania, num_orden, fecha_orden
from clientes c join ordenes o
on c.num_cliente = o.num_cliente
order by 4 desc; -- Alberto Pico Lara

select p.nombre_proveedor, i.num_inventario, i.descripcion, i.unidad, i.precio_unitario
from proveedores p join inventarios i
on p.codigo_proveedor = i.codigo_proveedor; -- Alberto Pico Lara

-- 4
select a.num_orden, i.descripcion
from articulos a join inventarios i
on a.num_inventario = i.num_inventario and a.codigo_proveedor = i.codigo_proveedor
where num_orden = 1004; -- Alberto Pico Lara

-- 5
select clientes.nombre, clientes.apellidos, clientes.direccion1, clientes.ciudad, estados.nombre
from clientes join estados
on estados.estado = clientes.estado
where estados.nombre like "A%"
order by ciudad desc; -- Alberto Pico Lara 

-- 6
select clientes.nombre, clientes.apellidos, ordenes.num_orden, articulos.num_articulo
from clientes join ordenes
on clientes.num_cliente = ordenes.num_cliente
join articulos
on ordenes.num_orden = articulos.num_orden
where ordenes.num_orden = 1007; -- Alberto Pico Lara

-- 7
select ordenes.num_orden, articulos.num_articulo, articulos.num_inventario, proveedores.codigo_proveedor, proveedores.nombre_proveedor
from ordenes join articulos on ordenes.num_orden = articulos.num_orden 
join proveedores on proveedores.codigo_proveedor = articulos.codigo_proveedor
order by ordenes.num_orden and articulos.num_articulo; -- Alberto Pico Lara

-- 8
select estados.nombre, count(*)
from estados join clientes on estados.estado = clientes.estado
group by estados.nombre; -- Alberto Pico Lara

-- 9
select estados.nombre, count(*)
from estados join clientes on estados.estado = clientes.estado
group by estados.nombre 
having count(*) > 2; -- Alberto Pico Lara

-- 10
select ordenes.fecha_orden, avg(articulos.precio_total) prom
from ordenes join articulos
on ordenes.num_orden = articulos.num_orden
group by ordenes.fecha_orden
order by prom; -- Alberto Pico Lara

-- outer join
-- 1
select inventarios.*, articulos.num_orden
from inventarios left join articulos 
on inventarios.codigo_proveedor = articulos.codigo_proveedor and inventarios.num_inventario = articulos.num_inventario
order by inventarios.num_inventario; -- Alberto Pico Lara

-- 2
select proveedores.*, articulos.num_orden
from proveedores left join articulos
on proveedores.codigo_proveedor = articulos.codigo_proveedor; -- Alberto Pico Lara

-- 3
select clientes.nombre, clientes.apellidos, ordenes.num_orden, ordenes.fecha_orden
from clientes left join ordenes
on clientes.num_cliente = ordenes.num_cliente; -- Alberto Pico Lara

-- 4
select estados.nombre, clientes.num_cliente, clientes.nombre, clientes.apellidos, clientes.telefono
from estados left join clientes 
on estados.estado = clientes.estado
order by estados.nombre; -- Alberto Pico Lara






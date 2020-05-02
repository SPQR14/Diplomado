use store;
-- 1
select nombre, apellidos from clientes
where num_cliente in (select distinct num_cliente from ordenes); -- Alberto Pico Lara

-- 2
select * from articulos
where precio_total > any
	(select precio_total from articulos
		where num_orden = 1023); -- Alberto Pico Lara
        
select * from articulos
where precio_total > 
	(select min(precio_total) from articulos
		where num_orden = 1023); -- Alberto Pico Lara
        
-- 3
select * from clientes
where ciudad = (select ciudad from clientes where num_cliente = 104) -- Alberto Pico Lara

-- 4
select * from articulos
where precio_total > all
	(select precio_total from articulos
		where num_orden = 1023); -- Alberto Pico Lara

select * from articulos
where precio_total >
	(select max(precio_total) from articulos
		where num_orden = 1023); -- Alberto Pico Lara

-- Vistas
-- 1
create view vista1 as
select num_cliente, nombre, apellidos, estado from clientes
where estado = "CA"; -- Alberto Pico Lara

-- 2
select * from vista1; -- Alberto Pico Lara

-- 3
insert into vista1 values (131, 'Tony', 'Stark', 'TX');
select * from vista1; -- Alberto Pico Lara

-- 4
select * from clientes; -- Alberto Pico Lara

-- 5
create view vista2 as
select num_cliente, nombre, apellidos, estado from clientes
where estado = "CA"
with check option; -- Alberto Pico Lara

-- 6
create view vista3 as
select num_cliente as cliente, count(*) cuenta_orden from ordenes group by num_cliente having cuenta_orden > 1; -- Alberto Pico Lara
select * from vista3;






















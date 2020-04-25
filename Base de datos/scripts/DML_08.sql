-- Practica 8
use store;

begin work;

insert into estados values("MX", "Edo. Mexico"); -- Alberto Pico Lara

insert into clientes values (129, "Alberto Isaac", "Pico", "BBVA", "Mariano Escobedo 303", null, "México", "MX", "53150", "56-11269619"); -- Alberto Pico

update inventarios set precio_unitario = 45 where precio_unitario = 40; -- Alberto Pico

commit work;


delete from clientes where num_cliente = 129; -- Alberto Pico Lara
select * from clientes;

rollback work;

select num_cliente, nombre from clientes;

select sum(precio_total), num_orden from articulos group by num_orden having sum(precio_total) > 250; -- Alberto Pico Lara
















-- Entrega final de módulo de base de datos
-- Alberto Isaac Pico Lara
-- Última modificación el 2/05/2020

create database alberto_pico_final;
use alberto_pico_final;

create table if not exists asociados(
cod_asociado int,
nombre varchar(25),
apellidos varchar(40),
calle_num varchar(5),
colonia varchar(20),
cp varchar(5),
delegacion varchar(26),
telefono1 varchar(10),
telefono2 varchar(10),
correo varchar(30)
);

create table if not exists asociados_rep(
cod_asociado int,
nombre varchar(25),
apellidos varchar(40),
calle_num varchar(5),
colonia varchar(20),
cp varchar(5),
delegacion varchar(26),
telefono1 varchar(10),
telefono2 varchar(10),
correo varchar(30)
);

delimiter //
create trigger asociados_insert before insert
on asociados for each row
		begin
			insert into asociados_rep values(
				new.cod_asociado, -- int
                new.nombre, -- varchar 25
                new.apellidos, -- varchar 40
                new.calle_num, -- varchar 5
                new.colonia, -- varchar 20
                new.cp, -- varchar 5
                new.delegacion, -- varchar 26
                new.telefono1, -- varchar 10
                new.telefono2, -- varchar 10
                new.correo -- varchar 30
            );
		end;//
delimiter ;

begin work;
insert into asociados values(14, "Lilia", "Basaldúa González", "14", "San Andrés", "04460", "Azcapotzalco", "5611269619", NULL, "lilia.basaldua@gmail.com");
select * from asociados_rep;
commit work;

delimiter //
create trigger asociados_update before update
on asociados for each row
		begin update asociados_rep set
			cod_asociado = new.cod_asociado, 
			nombre =  new.nombre, 
			apellidos = new.apellidos, 
			calle_num = new.calle_num, 
			colonia = new.colonia,
			cp = new.cp, 
			delegacion = new.delegacion, 
			telefono1 = new.telefono1, 
			telefono2 = new.telefono2, 
			correo = new.correo
			where cod_asociado = old.cod_asociado;
		end;//
delimiter ;

begin work;
update asociados set telefono1 = "5539641019";
select * from asociados_rep;
commit work;

delimiter //
create trigger asociados_delete before delete
on asociados for each row
	begin delete from asociados_rep
		where cod_asociado = old.cod_asociado;
    end;//
    
delimiter ;

begin work;
delete from asociados where cod_asociado = 14;
select * from asociados_rep;
commit work;

--


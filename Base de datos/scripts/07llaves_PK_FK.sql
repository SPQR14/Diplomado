#Añadiendo las llaves primarias
ALTER TABLE estados ADD CONSTRAINT pk_estado PRIMARY KEY (estado);

ALTER TABLE clientes ADD CONSTRAINT pk_clientes PRIMARY KEY (num_cliente);

ALTER TABLE ordenes ADD CONSTRAINT pk_ordenes PRIMARY KEY (num_orden);

ALTER TABLE articulos ADD CONSTRAINT pk_articulos PRIMARY KEY (num_orden, num_articulo);

ALTER TABLE inventarios ADD CONSTRAINT pk_inventarios PRIMARY KEY (num_inventario, codigo_proveedor);

ALTER TABLE proveedores ADD CONSTRAINT pk_proveedores PRIMARY KEY (codigo_proveedor);


#Añadiendo las llaves foráneas
ALTER TABLE clientes ADD CONSTRAINT fk_clientes FOREIGN KEY (estado) REFERENCES estados(estado);

SELECT * FROM articulos WHERE num_orden NOT IN
    (SELECT num_orden FROM ordenes);

    DELETE FROM articulos WHERE num_orden=1024;

ALTER TABLE articulos ADD CONSTRAINT fk_articulos1 FOREIGN KEY (num_orden) REFERENCES ordenes(num_orden);

ALTER TABLE articulos ADD CONSTRAINT fk_articulos2 FOREIGN KEY (num_inventario, codigo_proveedor) REFERENCES inventarios(num_inventario, codigo_proveedor);

ALTER TABLE inventarios ADD CONSTRAINT fk_inventarios FOREIGN KEY (codigo_proveedor) REFERENCES proveedores(codigo_proveedor)




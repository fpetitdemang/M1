set serveroutput on size 5000
call dbms_java.set_output(5000);


-- procedure sans parametre

create or replace procedure Aff
as language java name ’MonTp.Affiche()’;
/

-- fonction avec parametre

-- create or replace function conv(e number) return number
-- as language java name ’MonTp.Convertir(double) return double’;
-- /


select object_name from user_objects;

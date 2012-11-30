DROP type T_Ligne FORCE;
DROP type T_Point FORCE;
DROP type T_Cercle FORCE;

CREATE OR REPLACE TYPE T_Point as OBJECT(
	x integer,
	y integer
);
/

/** LIGNE **/

/**
* declaration des signatures des methodes
* definition de l'interface de l'objet
**/
CREATE OR REPLACE TYPE T_Ligne as OBJECT(
	point1 T_Point,
	point2 T_Point,
	MEMBER FUNCTION longueur return NUMBER
);
/

/* implementation du corps des methodes declarer */
CREATE OR REPLACE TYPE BODY T_Ligne as MEMBER FUNCTION longueur RETURN NUMBER
AS BEGIN
RETURN SQRT(POWER((point1.x - point2.x),2) + POWER((point1.y - point2.y),2));
END longueur;
END;
/

/** FIN LIGNE **/

/** CERCLE **/

CREATE OR REPLACE TYPE T_Cercle as OBJECT(
	centre T_Point,
	rayon T_Ligne,
	MEMBER FUNCTION perimetre return NUMBER,
	MEMBER FUNCTION surface return NUMBER
);
/

CREATE OR REPLACE TYPE BODY T_Cercle as MEMBER FUNCTION perimetre RETURN NUMBER
AS BEGIN
RETURN PI(rayon * 2);
END perimetre;
END; 
/

CREATE OR REPLACE TYPE BODY T_Cercle as MEMBER FUNCTION surface RETURN NUMBER
AS BEGIN
RETURN PI(POWER(rayon,2));
END surface;
END; 
/

/** FIN CERCLE **/

/** RECTANGLE **/

CREATE OR REPLACE TYPE T_Rectangle as OBJECT(
	p1 T_point,
	p2 T_point,
	MEMBER FUNCTION perimetre RETURN NUMBER,
	MEMBER FUNCTION surface RETURN NUMBER
);
/

CREATE OR REPLACE TYPE BODY T_Rectangle as MEMBER FUNCTION perimetre RETURN NUMBER
AS BEGIN 
RETURN ( (SQRT(POWER((point1.x - point1.x),2) + POWER((point1.y - point2.y),2))) + 
	(SQRT(POWER((point2.x - point2.x),2) + POWER((point1.y - point2.y),2))));
END perimetre;
END;
/

CREATE OR REPLACE TYPE BODY T_Rectangle as MEMBER FUNCTION surface RETURN NUMBER
AS BEGIN 
RETURN 0;
END surface;
END;
/
/** FIN RECTANGLE **/


/** CREATION TABLE **/
CREATE TABLE points OF T_Point;
CREATE TABLE rectangles OF T_Rectangle;
CREATE TABLE cercles OF T_Cercle;



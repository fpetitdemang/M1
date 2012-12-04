CREATE OR REPLACE TYPE Annee_T AS OBJECT(
		valeur integer
	);
/

CREATE OR REPLACE TYPE Adresse_T 
	AS OBJECT(	
		numero integer,
		rue char(20),
		ville char(20)
	);
/

CREATE OR REPLACE TYPE Personne_T
	AS OBJECT(
		numero int,
		nom char(20),
		prenom char(20),
		annee Annee_T,
		adresse Adresse_T
	);
/

DROP TABLE Personne;

CREATE TABLE Personne OF Personne_T
  ( 
	numero PRIMARY KEY,
	UNIQUE (nom),
      	CHECK (annee.valeur > 1900),
	CHECK (annee.valeur < 2007)
 );

INSERT INTO Personne
	VALUES("franck",1990);

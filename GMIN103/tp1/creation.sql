--mettre dans l 'ordre inverse de la creation des tables
DROP TABLE RENCONTRE;
DROP TABLE SUBVENTION;
DROP TABLE GAIN;
DROP TABLE SPONSOR;
DROP TABLE JOUEUR;


CREATE TABLE JOUEUR
	(nom varchar(20),
	prenom varchar(20),
	age integer,
	nationalite varchar(20),
	primary key (nom));
	
CREATE TABLE SPONSOR
	(nom varchar(20),
	adresse varchar(60),
	chiffreAffaires integer,
	primary key (nom));

CREATE TABLE GAIN
	(nomJoueur varchar(20),
	annee varchar(4),
	prime integer,
	nomSponsor varchar(20),
	primary key (nomJoueur, annee)
	foreign key (nomJoueur) references JOUEUR(nom),);
	
CREATE TABLE SUBVENTION
	(nomTournoi varchar(20),
	annee varchar(4),
	nomSponsor varchar(20),
	montant integer,
	primary key(nomTournoi, annee, nomSponsor),
	FOREIGN KEY (nomSponsor) REFERENCES SPONSOR(nom));

CREATE TABLE RENCONTRE
	(nomGagnant varchar(20),
	nomPerdant varchar(20),
	nomTournoi varchar(20),
	dateTournoi date,
	score varchar(20),
	primary key (nomGagnant, nomPerdant, nomTournoi),
	foreign key (nomGagnant) references JOUEUR(nom),
	foreign key (nomPerdant) references JOUEUR(nom));
	


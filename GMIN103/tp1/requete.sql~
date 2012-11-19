--REQUETE 1
--select nomJoueur, prime from gain where nomSponsor = 'Peugeot' and annee >= 2006 and annee <= 2010;

--REQUETE 2
--union des joueurGagnant et joueurPerdant de RG en 2010
create or replace view participant as
	(select nomGagnant as nomJ
	from rencontre
	where nomTournoi = 'RG'
	and to_char(dateTournoi,'yyyy') = '2010')
	union
	(select nomPerdant as nomJ
	from rencontre
	where nomTournoi = 'RG'
	and to_char(dateTournoi,'yyyy') = '2010');

select nom, age 
from joueur, participant
where joueur.nom=participant.nomJ;

--REQUETE 3
create or replace view participant1 as
	select nomGagnant as participant
	from rencontre
	where nomTournoi = 'RG';

create or replace view sponsorPeugeot as
	(select participant
	from participant1)
	intersect
	(select nomJoueur
	from gain
	where nomSponsor = 'Peugeot');

select nom, nationalite
from sponsorPeugeot, joueur
where sponsorPeugeot.participant=joueur.nom;


--REQUETE 4
create or replace view listeJoueur1 as
	(select nomGagnant as nomJ
	from rencontre
	where nomTournoi='RG'
	and to_char(dateTournoi,'yyyy') = '2002')
	union
	(select nomPerdant as nomJ
	from rencontre
	where nomTournoi='RG'
	and to_char(dateTournoi,'yyyy') = '2002');

create or replace view listeJoueur2 as
	(select nomGagnant as nomJ
	from rencontre
	where nomTournoi='W'
	and to_char(dateTournoi,'yyyy') = '2002')
	union
	(select nomPerdant as nomJ
	from rencontre
	where nomTournoi='W'
	and to_char(dateTournoi,'yyyy') = '2002');

create or replace view listeJoueur3 as 
	(select nomJ 
	from listeJoueur1)
	intersect
	(select nomJ
	from listeJoueur2)

select nom, nationalite
from joueur, listeJoueur3
where joueur.nom=listeJoueur3.nomJ;


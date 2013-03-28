-- phpMyAdmin SQL Dump
-- version 3.4.10.1deb1
-- http://www.phpmyadmin.net
--
-- Client: localhost
-- Généré le : Jeu 21 Février 2013 à 20:29
-- Version du serveur: 5.5.29
-- Version de PHP: 5.3.10-1ubuntu3.5

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Base de données: `parametrage`
--

-- --------------------------------------------------------

--
-- Structure de la table `configuration`
--

CREATE TABLE IF NOT EXISTS `configuration` (
  `utilisateur` varchar(100) NOT NULL DEFAULT '',
  `source` text,
  `thesaurus` text,
  `fondsDoc` text,
  `thesaurus_fonds` text,
  PRIMARY KEY (`utilisateur`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

--
-- Contenu de la table `configuration`
--

INSERT INTO `configuration` (`utilisateur`, `source`, `thesaurus`, `fondsDoc`, `thesaurus_fonds`) VALUES
('admnin', 'thesaurus', 'ThauV3', 'Fond1', 'thesaurus2');

-- --------------------------------------------------------

--
-- Structure de la table `icones`
--

CREATE TABLE IF NOT EXISTS `icones` (
  `utilisateur` varchar(100) NOT NULL DEFAULT '',
  `iconGen` text,
  `iconSpec` text,
  `iconAsso` text,
  `iconExc` text,
  PRIMARY KEY (`utilisateur`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

--
-- Contenu de la table `icones`
--

INSERT INTO `icones` (`utilisateur`, `iconGen`, `iconSpec`, `iconAsso`, `iconExc`) VALUES
('admnin', 'conceptP', 'conceptI', 'conceptA', 'conceptE');

-- --------------------------------------------------------

--
-- Structure de la table `indexation`
--

CREATE TABLE IF NOT EXISTS `indexation` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `date` date NOT NULL,
  `Input_File_Path` varchar(150) NOT NULL,
  `Output_Folder_Path` varchar(150) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=9 ;

--
-- Contenu de la table `indexation`
--

INSERT INTO `indexation` (`id`, `date`, `Input_File_Path`, `Output_Folder_Path`) VALUES
(1, '2013-02-21', 'C:\\fakepath\\commune (1).sql', '/home/nidhal/workspaceJEE/.metadata/.plugins/org.eclipse.wst.server.core/tmp0/wtpwebapps/Senterritoire2/sortiesXML');

-- --------------------------------------------------------

--
-- Structure de la table `liens`
--

CREATE TABLE IF NOT EXISTS `liens` (
  `utilisateur` varchar(100) NOT NULL DEFAULT '',
  `label` text,
  `couleurLGen` text,
  `couleurLSpec` text,
  `couleurLAsso` text,
  `couleurLExc` text,
  PRIMARY KEY (`utilisateur`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

--
-- Contenu de la table `liens`
--

INSERT INTO `liens` (`utilisateur`, `label`, `couleurLGen`, `couleurLSpec`, `couleurLAsso`, `couleurLExc`) VALUES
('admnin', '1', '#0000BB', '#00BB00', '#CC6666', '#003366');

-- --------------------------------------------------------

--
-- Structure de la table `parametrage_simple`
--

CREATE TABLE IF NOT EXISTS `parametrage_simple` (
  `utilisateur` varchar(100) NOT NULL DEFAULT '',
  `generiques` text,
  `specifiques` text,
  `associes` text,
  `exclus` text,
  `niveau` int(11) DEFAULT NULL,
  PRIMARY KEY (`utilisateur`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

--
-- Contenu de la table `parametrage_simple`
--

INSERT INTO `parametrage_simple` (`utilisateur`, `generiques`, `specifiques`, `associes`, `exclus`, `niveau`) VALUES
('admnin', 'true', 'true', 'true', 'false', 1);

-- --------------------------------------------------------

--
-- Structure de la table `theme`
--

CREATE TABLE IF NOT EXISTS `theme` (
  `utilisateur` varchar(100) NOT NULL DEFAULT '',
  `theme` text,
  PRIMARY KEY (`utilisateur`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

--
-- Contenu de la table `theme`
--

INSERT INTO `theme` (`utilisateur`, `theme`) VALUES
('admnin', 'soria');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;

-- phpMyAdmin SQL Dump
-- version 3.4.10.1deb1
-- http://www.phpmyadmin.net
--
-- Client: localhost
-- Généré le : Jeu 21 Février 2013 à 18:19
-- Version du serveur: 5.5.29
-- Version de PHP: 5.3.10-1ubuntu3.5

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Base de données: `senterritoire`
--

-- --------------------------------------------------------

--
-- Structure de la table `acteur`
--

CREATE TABLE IF NOT EXISTS `acteur` (
  `id_acteur` int(11) NOT NULL AUTO_INCREMENT,
  `a_nom` varchar(100) NOT NULL DEFAULT '',
  `a_category` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`id_acteur`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `document`
--

CREATE TABLE IF NOT EXISTS `document` (
  `id_document` int(11) NOT NULL AUTO_INCREMENT,
  `file_name` varchar(100) NOT NULL DEFAULT '',
  `repertory` varchar(100) DEFAULT '',
  `file_type` varchar(30) DEFAULT '',
  `extension` varchar(5) DEFAULT '',
  `source` varchar(100) NOT NULL DEFAULT '',
  `id_notice` int(11) NOT NULL DEFAULT '-1',
  PRIMARY KEY (`id_document`),
  KEY `document_idNotice` (`id_notice`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `entity`
--

CREATE TABLE IF NOT EXISTS `entity` (
  `id_entity` int(11) NOT NULL AUTO_INCREMENT,
  `e_value` varchar(100) NOT NULL DEFAULT '',
  `e_type` varchar(100) NOT NULL DEFAULT '',
  `e_category` varchar(100) NOT NULL DEFAULT '',
  `id_term` varchar(14) NOT NULL DEFAULT '-1',
  PRIMARY KEY (`id_entity`),
  KEY `entity_idterm` (`id_term`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `fondsdocumentaire`
--

CREATE TABLE IF NOT EXISTS `fondsdocumentaire` (
  `fondsDocumentaire` varchar(50) NOT NULL DEFAULT '',
  `description` varchar(100) NOT NULL DEFAULT '',
  `repertory` varchar(150) NOT NULL DEFAULT '',
  `id_thesaurus` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`fondsDocumentaire`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Structure de la table `index_e`
--

CREATE TABLE IF NOT EXISTS `index_e` (
  `id_phrase` int(11) NOT NULL DEFAULT '0',
  `id_entity` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id_phrase`,`id_entity`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Structure de la table `index_se`
--

CREATE TABLE IF NOT EXISTS `index_se` (
  `id_phrase` int(11) NOT NULL DEFAULT '0',
  `id_spatialentity` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id_phrase`,`id_spatialentity`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Structure de la table `index_seC`
--

CREATE TABLE IF NOT EXISTS `index_seC` (
  `id_phrase` int(11) NOT NULL DEFAULT '0',
  `id_spatialentityC` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id_phrase`,`id_spatialentityC`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Structure de la table `index_te`
--

CREATE TABLE IF NOT EXISTS `index_te` (
  `id_phrase` int(11) NOT NULL DEFAULT '0',
  `id_temporalentity` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id_phrase`,`id_temporalentity`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Structure de la table `informationGeographique`
--

CREATE TABLE IF NOT EXISTS `informationGeographique` (
  `id_infoGeo` int(11) NOT NULL AUTO_INCREMENT,
  `nom` varchar(100) NOT NULL DEFAULT '',
  `id_temporalEntity` int(11) NOT NULL DEFAULT '-1',
  `id_spatialentity` int(11) NOT NULL DEFAULT '-1',
  `id_entity` int(11) NOT NULL DEFAULT '-1',
  PRIMARY KEY (`id_infoGeo`),
  KEY `informationGeographique_entity` (`id_entity`),
  KEY `informationGeographique_spatialentity` (`id_spatialentity`),
  KEY `informationGeographique_temporalEntity` (`id_temporalEntity`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `notice`
--

CREATE TABLE IF NOT EXISTS `notice` (
  `id_notice` int(11) NOT NULL AUTO_INCREMENT,
  `DOC_ID` varchar(10) DEFAULT NULL,
  `DOC_SOURCE` varchar(100) DEFAULT NULL,
  `DOC_AUTEUR` varchar(100) DEFAULT NULL,
  `DOC_REF` varchar(100) DEFAULT NULL,
  `DOC_ENQ` varchar(100) DEFAULT NULL,
  `DOC_DAT_CREAT` varchar(100) DEFAULT NULL,
  `DOC_TITRE` text,
  `DOC_SOUS_TITRE` text,
  `DOC_CONTEXT` varchar(100) DEFAULT NULL,
  `DOC_LANGUE` varchar(100) DEFAULT NULL,
  `DOC_STYLE` varchar(100) DEFAULT NULL,
  `DOC_EVAL_NBR` varchar(100) DEFAULT NULL,
  `DOC_NOTE` varchar(100) DEFAULT NULL,
  `DATM` varchar(100) DEFAULT '',
  `BNSA_GEOREF` varchar(100) DEFAULT '',
  `BNSA_THEME` varchar(100) DEFAULT '',
  `DOC_ANALYSE` varchar(100) DEFAULT '',
  `DOC_AUTEURSEC` varchar(100) DEFAULT '',
  `DOC_AUTMORAL` varchar(100) DEFAULT '',
  `DOC_COLLECTION` varchar(100) DEFAULT '',
  `DOC_COTE` varchar(100) DEFAULT '',
  `DOC_GEO` varchar(100) DEFAULT '',
  `DOC_DEE` varchar(100) DEFAULT '',
  `DOC_TYPE` varchar(100) DEFAULT '',
  `DOC_ORIG_REF` varchar(100) DEFAULT '',
  `DOC_ORIG_TITRE` varchar(100) DEFAULT '',
  `DOC_URL` varchar(100) DEFAULT '',
  `IMG_REF` varchar(100) DEFAULT '',
  `MIDR_NB_IMG` varchar(100) DEFAULT '',
  `MIDR_SOURCE` varchar(100) DEFAULT '',
  `MIDR_TYPE` varchar(100) DEFAULT '',
  `MIDR_DAT_EXACTE` varchar(100) DEFAULT '',
  `PHO_COPYRIGHT` varchar(100) DEFAULT '',
  `PHO_DROIT_IMG` varchar(100) DEFAULT '',
  `PHO_FONDS` varchar(100) DEFAULT '',
  `PHO_LEGEND` varchar(100) DEFAULT '',
  `PHO_NB_DOC` varchar(100) DEFAULT '',
  `PHO_PROC_TECH` varchar(100) DEFAULT '',
  `PHO_DAT_PHOTO` varchar(100) DEFAULT '',
  `USERM` varchar(100) DEFAULT '',
  `SEC` varchar(100) DEFAULT '',
  `FT_CID` varchar(100) DEFAULT '',
  `FT_DATE` varchar(100) DEFAULT '',
  `FT_FLIST` varchar(100) DEFAULT '',
  `FT_FORMAT` varchar(100) DEFAULT '',
  `FT_ORIGINAL_SIZE` varchar(100) DEFAULT '',
  `FT_OWNER` varchar(100) DEFAULT '',
  `FT_SFNAME` varchar(100) DEFAULT '',
  `FT_SUBJECT` varchar(100) DEFAULT '',
  `fondsDocumentaire` varchar(50) NOT NULL DEFAULT '',
  `documentFile` varchar(100) NOT NULL DEFAULT '',
  `xmlFile` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`id_notice`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `opinionActeurDocument`
--

CREATE TABLE IF NOT EXISTS `opinionActeurDocument` (
  `id_acteur` int(11) NOT NULL DEFAULT '-1',
  `id_document` int(11) NOT NULL DEFAULT '-1',
  `id_opinion` int(11) NOT NULL,
  `o_value` varchar(100) NOT NULL DEFAULT '',
  `o_pos` varchar(100) NOT NULL DEFAULT '',
  `o_neutre` varchar(100) NOT NULL DEFAULT '',
  `o_neg` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`id_acteur`,`id_document`,`id_opinion`),
  KEY `opinionActeurDocument_acteur` (`id_acteur`),
  KEY `opinionActeurDocument_document` (`id_document`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Structure de la table `opinionActeurParagraphe`
--

CREATE TABLE IF NOT EXISTS `opinionActeurParagraphe` (
  `id_acteur` int(11) NOT NULL DEFAULT '-1',
  `id_paragraphe` int(11) NOT NULL DEFAULT '-1',
  `id_opinion` int(11) NOT NULL,
  `o_value` varchar(100) NOT NULL DEFAULT '',
  `o_pos` varchar(100) NOT NULL DEFAULT '',
  `o_neutre` varchar(100) NOT NULL DEFAULT '',
  `o_neg` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`id_acteur`,`id_paragraphe`,`id_opinion`),
  KEY `opinionActeurParagraphe_acteur` (`id_acteur`),
  KEY `opinionActeurParagraphe_paragraphe` (`id_paragraphe`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Structure de la table `opinionActeurPhrase`
--

CREATE TABLE IF NOT EXISTS `opinionActeurPhrase` (
  `id_acteur` int(11) NOT NULL DEFAULT '-1',
  `id_phrase` int(11) NOT NULL DEFAULT '-1',
  `id_opinion` int(11) NOT NULL,
  `o_value` varchar(100) NOT NULL DEFAULT '',
  `o_pos` varchar(100) NOT NULL DEFAULT '',
  `o_neutre` varchar(100) NOT NULL DEFAULT '',
  `o_neg` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`id_acteur`,`id_phrase`,`id_opinion`),
  KEY `opinionActeurPhrase_acteur` (`id_acteur`),
  KEY `opinionActeurPhrase_paragraphe` (`id_phrase`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Structure de la table `paragraphe`
--

CREATE TABLE IF NOT EXISTS `paragraphe` (
  `id_paragraphe` int(11) NOT NULL AUTO_INCREMENT,
  `numero` int(5) NOT NULL DEFAULT '-1',
  `contenu` text,
  `id_document` int(11) NOT NULL DEFAULT '-1',
  PRIMARY KEY (`id_paragraphe`),
  KEY `paragraphe_document` (`id_document`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `phrase`
--

CREATE TABLE IF NOT EXISTS `phrase` (
  `id_phrase` int(11) NOT NULL AUTO_INCREMENT,
  `numero` int(5) NOT NULL DEFAULT '-1',
  `contenu` text,
  `id_paragraphe` int(11) NOT NULL DEFAULT '-1',
  PRIMARY KEY (`id_phrase`),
  KEY `phrase_paragraphe` (`id_paragraphe`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `rejected_entity`
--

CREATE TABLE IF NOT EXISTS `rejected_entity` (
  `id_rejectedEntity` int(11) NOT NULL AUTO_INCREMENT,
  `rejected_value` varchar(100) NOT NULL DEFAULT '',
  `id_entity` int(11) NOT NULL DEFAULT '-1',
  `id_spatialentity` int(11) NOT NULL DEFAULT '-1',
  `id_temporalEntity` int(11) NOT NULL DEFAULT '-1',
  PRIMARY KEY (`id_rejectedEntity`),
  KEY `rejectedentity_entity` (`id_entity`),
  KEY `rejectedentity_spatialentity` (`id_spatialentity`),
  KEY `rejectedentity_temporalEntity` (`id_temporalEntity`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `ressource`
--

CREATE TABLE IF NOT EXISTS `ressource` (
  `id_ressource` int(11) NOT NULL AUTO_INCREMENT,
  `nom` varchar(100) NOT NULL DEFAULT '',
  `source` varchar(100) NOT NULL DEFAULT '',
  `type` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`id_ressource`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 AUTO_INCREMENT=3 ;

--
-- Contenu de la table `ressource`
--

INSERT INTO `ressource` (`id_ressource`, `nom`, `source`, `type`) VALUES
(1, 'res1', 'res1', 'res1'),
(2, 'res2', 'res2', 'res2');

-- --------------------------------------------------------

--
-- Structure de la table `spatialentity`
--

CREATE TABLE IF NOT EXISTS `spatialentity` (
  `id_spatialentity` int(11) NOT NULL AUTO_INCREMENT,
  `te_nom` varchar(100) NOT NULL DEFAULT '',
  `se_valeur` varchar(100) NOT NULL DEFAULT '',
  `se_type` varchar(100) NOT NULL DEFAULT '',
  `se_category` varchar(100) NOT NULL DEFAULT '',
  `id_term` varchar(14) NOT NULL DEFAULT '-1',
  `id_ressource` int(11) NOT NULL,
  PRIMARY KEY (`id_spatialentity`),
  KEY `spatialentity_idterm` (`id_term`),
  KEY `ressource_id` (`id_ressource`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `spatialentityCandidat`
--

CREATE TABLE IF NOT EXISTS `spatialentityCandidat` (
  `id_spatialentityC` int(11) NOT NULL AUTO_INCREMENT,
  `te_nom` varchar(100) NOT NULL DEFAULT '',
  `se_valeur` varchar(100) NOT NULL DEFAULT '',
  `se_type` varchar(100) NOT NULL DEFAULT '',
  `se_category` varchar(100) NOT NULL DEFAULT '',
  `id_term` varchar(14) NOT NULL DEFAULT '-1',
  PRIMARY KEY (`id_spatialentityC`),
  KEY `spatialentity_idterm` (`id_term`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `temporalentity`
--

CREATE TABLE IF NOT EXISTS `temporalentity` (
  `id_temporalEntity` int(11) NOT NULL AUTO_INCREMENT,
  `te_nom` varchar(100) NOT NULL DEFAULT '',
  `te_valeur` varchar(100) NOT NULL DEFAULT '',
  `se_type` varchar(100) NOT NULL DEFAULT '',
  `te_category` varchar(100) NOT NULL DEFAULT '',
  `id_term` varchar(14) NOT NULL DEFAULT '-1',
  `id_ressource` int(11) NOT NULL,
  PRIMARY KEY (`id_temporalEntity`),
  KEY `temporalentity_idterm` (`id_term`),
  KEY `ressource_id` (`id_ressource`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;

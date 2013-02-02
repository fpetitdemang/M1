<?xml version="1.0" encoding="ISO-8859-1" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="html" encoding="ISO-8859-1" />
<xsl:template match="qcm">
   <html>
       <body>
       <h1> <xsl:value-of select="@matiere"/> </h1>
	<form name="input" action="#" method="get">
		<xsl:apply-templates select="question"/>
	<input type="submit" value="Submit"></input>
	</form>
	</body>
   </html>
</xsl:template>
<xsl:template match="question">
	<label> <xsl:value-of select="libelle"/> </label> 
	<xsl:apply-templates select="choix"/>
	
	<br/><input>
		<xsl:attribute name="name"><xsl:value-of select="count(preceding::question)"/></xsl:attribute>
		<xsl:attribute name="value">0</xsl:attribute>
		<xsl:choose>
				<xsl:when test="count(choix[@score > 0]) > 1">
					<xsl:attribute name="type">checkbox</xsl:attribute>
				</xsl:when>
				<xsl:otherwise>
					<xsl:attribute name="type">radio</xsl:attribute>
				</xsl:otherwise>
		</xsl:choose>
	</input><label>Je ne sais pas</label>
	<hr />
</xsl:template>
<xsl:template match="choix">
	<br/><input>
		<xsl:attribute name="name"><xsl:value-of select="count(preceding::question)"/></xsl:attribute>
		<xsl:attribute name="value"><xsl:value-of select="@score"/></xsl:attribute>
		<xsl:choose>
			<xsl:when test="count(../choix[@score > 0]) > 1">
				<xsl:attribute name="type">checkbox</xsl:attribute>
			</xsl:when>
			<xsl:otherwise>
				<xsl:attribute name="type">radio</xsl:attribute>
			</xsl:otherwise>
		</xsl:choose> 		
	</input>	
	<xsl:value-of select="."/>
</xsl:template>
</xsl:stylesheet>

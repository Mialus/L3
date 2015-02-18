<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="2.0" id="l3-2015-f2"
                xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="xsd">

  <xsl:output method="text" encoding="ISO-8859-1"/>
  <xsl:variable name="eol" select="'&#xA;'"/>
  <xsl:strip-space elements="*"/>

  <xsl:template match="operas" as="xsd:string">
    <xsl:variable name="operas-processed" as="xsd:string+">
	<xsl:apply-templates select="works/opera" />
		<xsl:for-each select="opera/title">
			<title/>
			<xsl:text> #xA </xsl:text>
 		</xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="$operas-processed" separator=""/>
  </xsl:template>

  <xsl:template match="person" as="xsd:string+">
   <xsl:sequence
     select="@lastname,' (',@firstname,') [',@born,'-',@dead,']',$eol"/>
  </xsl:template>

</xsl:stylesheet>

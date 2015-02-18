<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="2.0" id="grouping-plus"
                xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                extension-element-prefixes="xsd">

  <xsl:output method="xml" encoding="ISO-8859-1" indent="yes"/>
  <xsl:output method="text" encoding="ISO-8859-1" name="additional-text"/>
  <xsl:param name="verbose" select="false()" as="xsd:boolean"/>
  <xsl:variable name="eol" select="'&#10;'" as="xsd:string"/>

  <xsl:template match="books" as="element(items)">
    <xsl:variable name="the-stories" select="omnibus/story"
                  as="element(story)*"/>
    <items>
      <xsl:for-each-group select="$the-stories" group-by="year">
        <xsl:sort select="xsd:integer(year)"/>
        <xsl:if test="$verbose">
          <xsl:message>
            <xsl:value-of select="'Processing year:',year"/>
          </xsl:message>
        </xsl:if>
        <by-year year="{year}">
          <xsl:copy-of select="current-group()/title"/>
        </by-year>
      </xsl:for-each-group>
    </items>
    <xsl:result-document href="{@series}-years" format="additional-text">
      <xsl:variable name="all-the-years" select="$the-stories/year"
                    as="element(year)*"/>
      <xsl:variable name="the-distinct-years"
                    select="distinct-values($all-the-years)"
                    as="xsd:integer*"/>
      <xsl:if test="$verbose">
        <xsl:message>
          <xsl:value-of select="'All the years',$all-the-years"/>
          <xsl:value-of select="$eol"/>
          <xsl:value-of select="'Distinct ones',$the-distinct-years"/>
        </xsl:message>
      </xsl:if>
      <xsl:perform-sort select="$the-distinct-years">
        <xsl:sort select="xsd:integer(data(.))"/>
      </xsl:perform-sort>
    </xsl:result-document>
  </xsl:template>

</xsl:stylesheet>

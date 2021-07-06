<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:xhtml="http://www.w3.org/1999/xhtml">

  <xsl:strip-space elements="*"/>

  <xsl:template match="/">
    <chunk>
      <xsl:apply-templates/>
    </chunk>
  </xsl:template>

  <xsl:template match="//xhtml:title">
    <title><xsl:value-of select="."/></title>
  </xsl:template>

  <xsl:template match="//*[contains(@class,'entry-content')]">
    <content>
      <xsl:apply-templates/>
    </content>
  </xsl:template>

  <xsl:template match="//*[contains(@class,'entry-content')]//*[not(name()='scritp')]">
    <xsl:copy>
      <xsl:value-of select="text()" />
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="//xhtml:script"/>

  <!-- <xsl:template match="text()"/> -->

  <xsl:template match="*">
    <xsl:apply-templates/>
  </xsl:template>

</xsl:stylesheet>

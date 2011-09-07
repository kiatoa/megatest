;; Copyright 2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(define ods:dirs
  '("Configurations2"
    "Configurations2/toolpanel"
    "Configurations2/menubar"
    "Configurations2/toolbar"
    "Configurations2/progressbar"
    "Configurations2/floater"
    "Configurations2/images"
    "Configurations2/images/Bitmaps"
    "Configurations2/statusbar"
    "Configurations2/popupmenu"
    "Configurations2/accelerator"
    "META-INF"
    "Thumbnails"))

(define ods:0-len-files
  '("Configurations2/accelerator/current.xml"
    ;; "Thumbnails/thumbnail.png"
    "content.xml"
    ))

(define ods:files
  '(("META-INF/manifest.xml"
     ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\">"
      "<manifest:file-entry manifest:media-type=\"application/vnd.oasis.opendocument.spreadsheet\" manifest:version=\"1.2\" manifest:full-path=\"/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/statusbar/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/accelerator/current.xml\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/accelerator/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/floater/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/popupmenu/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/progressbar/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/toolpanel/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/menubar/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/toolbar/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/images/Bitmaps/\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Configurations2/images/\"/>"
      "<manifest:file-entry manifest:media-type=\"application/vnd.sun.xml.ui.configuration\" manifest:full-path=\"Configurations2/\"/>"
      "<manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"content.xml\"/>"
      "<manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"styles.xml\"/>"
      "<manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"meta.xml\"/>"
      ;; "<manifest:file-entry manifest:media-type=\"image/png\" manifest:full-path=\"Thumbnails/thumbnail.png\"/>"
      "<manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Thumbnails/\"/>"
      "<manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"settings.xml\"/>"))
    ("styles.xml"
     ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<office:document-styles xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\" xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\" xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\" xmlns:draw=\"urn:oasis:names:tc:opendocument:xmlns:drawing:1.0\" xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\" xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\" xmlns:presentation=\"urn:oasis:names:tc:opendocument:xmlns:presentation:1.0\" xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\" xmlns:chart=\"urn:oasis:names:tc:opendocument:xmlns:chart:1.0\" xmlns:dr3d=\"urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0\" xmlns:math=\"http://www.w3.org/1998/Math/MathML\" xmlns:form=\"urn:oasis:names:tc:opendocument:xmlns:form:1.0\" xmlns:script=\"urn:oasis:names:tc:opendocument:xmlns:script:1.0\" xmlns:ooo=\"http://openoffice.org/2004/office\" xmlns:ooow=\"http://openoffice.org/2004/writer\" xmlns:oooc=\"http://openoffice.org/2004/calc\" xmlns:dom=\"http://www.w3.org/2001/xml-events\" xmlns:rpt=\"http://openoffice.org/2005/report\" xmlns:of=\"urn:oasis:names:tc:opendocument:xmlns:of:1.2\" xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" xmlns:grddl=\"http://www.w3.org/2003/g/data-view#\" xmlns:tableooo=\"http://openoffice.org/2009/table\" xmlns:css3t=\"http://www.w3.org/TR/css3-text/\" office:version=\"1.2\" grddl:transformation=\"http://docs.oasis-open.org/office/1.2/xslt/odf2rdf.xsl\"><office:font-face-decls><style:font-face style:name=\"Arial\" svg:font-family=\"Arial\" style:font-family-generic=\"swiss\" style:font-pitch=\"variable\"/><style:font-face style:name=\"DejaVu Sans\" svg:font-family=\"&apos;DejaVu Sans&apos;\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/><style:font-face style:name=\"Droid Sans Fallback\" svg:font-family=\"&apos;Droid Sans Fallback&apos;\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/><style:font-face style:name=\"Lohit Hindi\" svg:font-family=\"&apos;Lohit Hindi&apos;\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/></office:font-face-decls><office:styles><style:default-style style:family=\"table-cell\"><style:paragraph-properties style:tab-stop-distance=\"0.5in\"/><style:text-properties style:font-name=\"Arial\" fo:language=\"en\" fo:country=\"US\" style:font-name-asian=\"DejaVu Sans\" style:language-asian=\"zh\" style:country-asian=\"CN\" style:font-name-complex=\"DejaVu Sans\" style:language-complex=\"hi\" style:country-complex=\"IN\"/></style:default-style><number:number-style style:name=\"N0\"><number:number number:min-integer-digits=\"1\"/></number:number-style><number:currency-style style:name=\"N104P0\" style:volatile=\"true\"><number:currency-symbol number:language=\"en\" number:country=\"US\">$</number:currency-symbol><number:number number:decimal-places=\"2\" number:min-integer-digits=\"1\" number:grouping=\"true\"/></number:currency-style><number:currency-style style:name=\"N104\"><style:text-properties fo:color=\"#ff0000\"/><number:text>-</number:text><number:currency-symbol number:language=\"en\" number:country=\"US\">$</number:currency-symbol><number:number number:decimal-places=\"2\" number:min-integer-digits=\"1\" number:grouping=\"true\"/><style:map style:condition=\"value()&gt;=0\" style:apply-style-name=\"N104P0\"/></number:currency-style><style:style style:name=\"Default\" style:family=\"table-cell\"><style:text-properties style:font-name-asian=\"Droid Sans Fallback\" style:font-name-complex=\"Lohit Hindi\"/></style:style><style:style style:name=\"Result\" style:family=\"table-cell\" style:parent-style-name=\"Default\"><style:text-properties fo:font-style=\"italic\" style:text-underline-style=\"solid\" style:text-underline-width=\"auto\" style:text-underline-color=\"font-color\" fo:font-weight=\"bold\"/></style:style><style:style style:name=\"Result2\" style:family=\"table-cell\" style:parent-style-name=\"Result\" style:data-style-name=\"N104\"/><style:style style:name=\"Heading\" style:family=\"table-cell\" style:parent-style-name=\"Default\"><style:table-cell-properties style:text-align-source=\"fix\" style:repeat-content=\"false\"/><style:paragraph-properties fo:text-align=\"center\"/><style:text-properties fo:font-size=\"16pt\" fo:font-style=\"italic\" fo:font-weight=\"bold\"/></style:style><style:style style:name=\"Heading1\" style:family=\"table-cell\" style:parent-style-name=\"Heading\"><style:table-cell-properties style:rotation-angle=\"90\"/></style:style></office:styles><office:automatic-styles><style:page-layout style:name=\"Mpm1\"><style:page-layout-properties style:writing-mode=\"lr-tb\"/><style:header-style><style:header-footer-properties fo:min-height=\"0.2957in\" fo:margin-left=\"0in\" fo:margin-right=\"0in\" fo:margin-bottom=\"0.0984in\"/></style:header-style><style:footer-style><style:header-footer-properties fo:min-height=\"0.2957in\" fo:margin-left=\"0in\" fo:margin-right=\"0in\" fo:margin-top=\"0.0984in\"/></style:footer-style></style:page-layout><style:page-layout style:name=\"Mpm2\"><style:page-layout-properties style:writing-mode=\"lr-tb\"/><style:header-style><style:header-footer-properties fo:min-height=\"0.2957in\" fo:margin-left=\"0in\" fo:margin-right=\"0in\" fo:margin-bottom=\"0.0984in\" fo:border=\"0.0346in solid #000000\" fo:padding=\"0.0071in\" fo:background-color=\"#c0c0c0\"><style:background-image/></style:header-footer-properties></style:header-style><style:footer-style><style:header-footer-properties fo:min-height=\"0.2957in\" fo:margin-left=\"0in\" fo:margin-right=\"0in\" fo:margin-top=\"0.0984in\" fo:border=\"0.0346in solid #000000\" fo:padding=\"0.0071in\" fo:background-color=\"#c0c0c0\"><style:background-image/></style:header-footer-properties></style:footer-style></style:page-layout></office:automatic-styles><office:master-styles><style:master-page style:name=\"Default\" style:page-layout-name=\"Mpm1\"><style:header><text:p><text:sheet-name>???</text:sheet-name></text:p></style:header><style:header-left style:display=\"false\"/><style:footer><text:p>Page <text:page-number>1</text:page-number></text:p></style:footer><style:footer-left style:display=\"false\"/></style:master-page><style:master-page style:name=\"Report\" style:page-layout-name=\"Mpm2\"><style:header><style:region-left><text:p><text:sheet-name>???</text:sheet-name> (<text:title>???</text:title>)</text:p></style:region-left><style:region-right><text:p><text:date style:data-style-name=\"N2\" text:date-value=\"2011-09-06\">09/06/2011</text:date>, <text:time>20:48:51</text:time></text:p></style:region-right></style:header><style:header-left style:display=\"false\"/><style:footer><text:p>Page <text:page-number>1</text:page-number> / <text:page-count>99</text:page-count></text:p></style:footer><style:footer-left style:display=\"false\"/></style:master-page></office:master-styles></office:document-styles>"))
    ("settings.xml"
     ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<office:document-settings xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:config=\"urn:oasis:names:tc:opendocument:xmlns:config:1.0\" xmlns:ooo=\"http://openoffice.org/2004/office\" office:version=\"1.2\"><office:settings><config:config-item-set config:name=\"ooo:view-settings\"><config:config-item config:name=\"VisibleAreaTop\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"VisibleAreaLeft\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"VisibleAreaWidth\" config:type=\"int\">4516</config:config-item><config:config-item config:name=\"VisibleAreaHeight\" config:type=\"int\">1799</config:config-item><config:config-item-map-indexed config:name=\"Views\"><config:config-item-map-entry><config:config-item config:name=\"ViewId\" config:type=\"string\">view1</config:config-item><config:config-item-map-named config:name=\"Tables\"><config:config-item-map-entry config:name=\"Sheet1\"><config:config-item config:name=\"CursorPositionX\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"CursorPositionY\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"HorizontalSplitMode\" config:type=\"short\">0</config:config-item><config:config-item config:name=\"VerticalSplitMode\" config:type=\"short\">0</config:config-item><config:config-item config:name=\"HorizontalSplitPosition\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"VerticalSplitPosition\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"ActiveSplitRange\" config:type=\"short\">2</config:config-item><config:config-item config:name=\"PositionLeft\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"PositionRight\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"PositionTop\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"PositionBottom\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"ZoomType\" config:type=\"short\">0</config:config-item><config:config-item config:name=\"ZoomValue\" config:type=\"int\">100</config:config-item><config:config-item config:name=\"PageViewZoomValue\" config:type=\"int\">60</config:config-item><config:config-item config:name=\"ShowGrid\" config:type=\"boolean\">true</config:config-item></config:config-item-map-entry><config:config-item-map-entry config:name=\"Sheet2\"><config:config-item config:name=\"CursorPositionX\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"CursorPositionY\" config:type=\"int\">4</config:config-item><config:config-item config:name=\"HorizontalSplitMode\" config:type=\"short\">0</config:config-item><config:config-item config:name=\"VerticalSplitMode\" config:type=\"short\">0</config:config-item><config:config-item config:name=\"HorizontalSplitPosition\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"VerticalSplitPosition\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"ActiveSplitRange\" config:type=\"short\">2</config:config-item><config:config-item config:name=\"PositionLeft\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"PositionRight\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"PositionTop\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"PositionBottom\" config:type=\"int\">0</config:config-item><config:config-item config:name=\"ZoomType\" config:type=\"short\">0</config:config-item><config:config-item config:name=\"ZoomValue\" config:type=\"int\">100</config:config-item><config:config-item config:name=\"PageViewZoomValue\" config:type=\"int\">60</config:config-item><config:config-item config:name=\"ShowGrid\" config:type=\"boolean\">true</config:config-item></config:config-item-map-entry></config:config-item-map-named><config:config-item config:name=\"ActiveTable\" config:type=\"string\">Sheet2</config:config-item><config:config-item config:name=\"HorizontalScrollbarWidth\" config:type=\"int\">270</config:config-item><config:config-item config:name=\"ZoomType\" config:type=\"short\">0</config:config-item><config:config-item config:name=\"ZoomValue\" config:type=\"int\">100</config:config-item><config:config-item config:name=\"PageViewZoomValue\" config:type=\"int\">60</config:config-item><config:config-item config:name=\"ShowPageBreakPreview\" config:type=\"boolean\">false</config:config-item><config:config-item config:name=\"ShowZeroValues\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"ShowNotes\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"ShowGrid\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"GridColor\" config:type=\"long\">12632256</config:config-item><config:config-item config:name=\"ShowPageBreaks\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"HasColumnRowHeaders\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"HasSheetTabs\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"IsOutlineSymbolsSet\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"IsSnapToRaster\" config:type=\"boolean\">false</config:config-item><config:config-item config:name=\"RasterIsVisible\" config:type=\"boolean\">false</config:config-item><config:config-item config:name=\"RasterResolutionX\" config:type=\"int\">1270</config:config-item><config:config-item config:name=\"RasterResolutionY\" config:type=\"int\">1270</config:config-item><config:config-item config:name=\"RasterSubdivisionX\" config:type=\"int\">1</config:config-item><config:config-item config:name=\"RasterSubdivisionY\" config:type=\"int\">1</config:config-item><config:config-item config:name=\"IsRasterAxisSynchronized\" config:type=\"boolean\">true</config:config-item></config:config-item-map-entry></config:config-item-map-indexed></config:config-item-set><config:config-item-set config:name=\"ooo:configuration-settings\"><config:config-item config:name=\"IsKernAsianPunctuation\" config:type=\"boolean\">false</config:config-item><config:config-item config:name=\"IsRasterAxisSynchronized\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"LinkUpdateMode\" config:type=\"short\">3</config:config-item><config:config-item config:name=\"SaveVersionOnClose\" config:type=\"boolean\">false</config:config-item><config:config-item config:name=\"AllowPrintJobCancel\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"HasSheetTabs\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"ShowPageBreaks\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"RasterResolutionX\" config:type=\"int\">1270</config:config-item><config:config-item config:name=\"PrinterSetup\" config:type=\"base64Binary\"/><config:config-item config:name=\"RasterResolutionY\" config:type=\"int\">1270</config:config-item><config:config-item config:name=\"LoadReadonly\" config:type=\"boolean\">false</config:config-item><config:config-item config:name=\"RasterSubdivisionX\" config:type=\"int\">1</config:config-item><config:config-item config:name=\"ShowNotes\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"ShowZeroValues\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"RasterSubdivisionY\" config:type=\"int\">1</config:config-item><config:config-item config:name=\"ApplyUserData\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"GridColor\" config:type=\"long\">12632256</config:config-item><config:config-item config:name=\"RasterIsVisible\" config:type=\"boolean\">false</config:config-item><config:config-item config:name=\"IsSnapToRaster\" config:type=\"boolean\">false</config:config-item><config:config-item config:name=\"PrinterName\" config:type=\"string\"/><config:config-item config:name=\"ShowGrid\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"CharacterCompressionType\" config:type=\"short\">0</config:config-item><config:config-item config:name=\"HasColumnRowHeaders\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"IsOutlineSymbolsSet\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"AutoCalculate\" config:type=\"boolean\">true</config:config-item><config:config-item config:name=\"IsDocumentShared\" config:type=\"boolean\">false</config:config-item><config:config-item config:name=\"UpdateFromTemplate\" config:type=\"boolean\">true</config:config-item></config:config-item-set></office:settings></office:document-settings>"))
    ("mimetype"
     ("application/vnd.oasis.opendocument.spreadsheet"))
    ("meta.xml"
     ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<office:document-meta xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\" xmlns:ooo=\"http://openoffice.org/2004/office\" xmlns:grddl=\"http://www.w3.org/2003/g/data-view#\" office:version=\"1.2\" grddl:transformation=\"http://docs.oasis-open.org/office/1.2/xslt/odf2rdf.xsl\"><office:meta><meta:initial-creator>Matt Welland</meta:initial-creator><meta:creation-date>2011-09-06T20:46:23</meta:creation-date><dc:date>2011-09-06T20:48:51</dc:date><dc:creator>Matt Welland</dc:creator><meta:editing-duration>PT2M29S</meta:editing-duration><meta:editing-cycles>1</meta:editing-cycles><meta:document-statistic meta:table-count=\"3\" meta:cell-count=\"10\" meta:object-count=\"0\"/><meta:generator>LibreOffice/3.3$Linux LibreOffice_project/330m19$Build-301</meta:generator></office:meta></office:document-meta>"))))

(define ods:content-header
  '("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<office:document-content xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\" xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\" xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\" xmlns:draw=\"urn:oasis:names:tc:opendocument:xmlns:drawing:1.0\" xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\" xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\" xmlns:presentation=\"urn:oasis:names:tc:opendocument:xmlns:presentation:1.0\" xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\" xmlns:chart=\"urn:oasis:names:tc:opendocument:xmlns:chart:1.0\" xmlns:dr3d=\"urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0\" xmlns:math=\"http://www.w3.org/1998/Math/MathML\" xmlns:form=\"urn:oasis:names:tc:opendocument:xmlns:form:1.0\" xmlns:script=\"urn:oasis:names:tc:opendocument:xmlns:script:1.0\" xmlns:ooo=\"http://openoffice.org/2004/office\" xmlns:ooow=\"http://openoffice.org/2004/writer\" xmlns:oooc=\"http://openoffice.org/2004/calc\" xmlns:dom=\"http://www.w3.org/2001/xml-events\" xmlns:xforms=\"http://www.w3.org/2002/xforms\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:rpt=\"http://openoffice.org/2005/report\" xmlns:of=\"urn:oasis:names:tc:opendocument:xmlns:of:1.2\" xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" xmlns:grddl=\"http://www.w3.org/2003/g/data-view#\" xmlns:tableooo=\"http://openoffice.org/2009/table\" xmlns:field=\"urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0\" xmlns:formx=\"urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0\" xmlns:css3t=\"http://www.w3.org/TR/css3-text/\" office:version=\"1.2\" grddl:transformation=\"http://docs.oasis-open.org/office/1.2/xslt/odf2rdf.xsl\">"
    "<office:scripts/>"
    "<office:font-face-decls>"
    "<style:font-face style:name=\"Arial\" svg:font-family=\"Arial\" style:font-family-generic=\"swiss\" style:font-pitch=\"variable\"/>"
    "<style:font-face style:name=\"DejaVu Sans\" svg:font-family=\"&apos;DejaVu Sans&apos;\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/>"
    "<style:font-face style:name=\"Droid Sans Fallback\" svg:font-family=\"&apos;Droid Sans Fallback&apos;\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/>"
    "<style:font-face style:name=\"Lohit Hindi\" svg:font-family=\"&apos;Lohit Hindi&apos;\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/>"
    "</office:font-face-decls>"
    "<office:automatic-styles>"
    "<style:style style:name=\"co1\" style:family=\"table-column\">"
    "<style:table-column-properties fo:break-before=\"auto\" style:column-width=\"0.8925in\"/>"
    "</style:style>"
    "<style:style style:name=\"ro1\" style:family=\"table-row\">"
    "<style:table-row-properties style:row-height=\"0.178in\" fo:break-before=\"auto\" style:use-optimal-row-height=\"true\"/>"
    "</style:style>"
    "<style:style style:name=\"ta1\" style:family=\"table\" style:master-page-name=\"Default\">"
    "<style:table-properties table:display=\"true\" style:writing-mode=\"lr-tb\"/>"
    "</style:style>"
    "</office:automatic-styles>"
    "<office:body>"
    "<office:spreadsheet>"))

(define ods:content-footer
  '("</table:table>"
    "</office:spreadsheet>"
    "</office:body>"
    "</office:document-content>"))

(define (ods:sheet sheetname content)
  (conc "<table:table table:name=\"" sheetname "\" table:style-name=\"ta1\" table:print=\"false\">"
	content
	"</table:table>"))

;; seems to be called once at top of each sheet, i.e. a column of rows
(define (ods:column)
  "<table:table-column table:style-name=\"co1\" table:number-columns-repeated=\"2\" table:default-cell-style-name=\"Default\"/>"))

;; cells is a list of <table:table-cell ..> ... </table:table-cell>
(define (ods:row cells)
  (apply conc
	 (cons "<table:table-row table:style-name=\"ro1\">"
	       (append cells (list "</table:table-row>")))))

;; types are "string" or "float"
(define (ods:cell value type)
  (conc "<table:table-cell office:value-type=\"" type "\">"
	"<text:p>" value "</text:p>"
	"</table:table-cell>"))

;; create the directories
(define (ods:construct-dir path)
  (for-each 
   (lambda (subdir)
     (system (conc "mkdir -p "  path "/" subdir)))
   ods:dirs))

;; populate the necessary, non-constructed, files
(define (ods:add-non-content-files path)
  ;; first the zero-length files, nb// the dir should already be created
  (for-each 
   (lambda (fname)
     (system (conc "touch " path "/" fname)))
   ods:0-len-files)
  ;; create the files with stuff in them
  (for-each
   (lambda (fdat)
     (let* ((name  (car fdat))
	    (lines (cadr fdat)))
       (with-output-to-file (conc path "/" name)
	 (lambda ()
	   (for-each 
	    (lambda (line)
	      (print line))
	    lines)))))
   ods:files))


		   
       
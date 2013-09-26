(define minimal-sxml
  '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
       (http://www.gnumeric.org/v10.dtd:Workbook
         (@ (http://www.w3.org/2001/XMLSchema-instance:schemaLocation
              "http://www.gnumeric.org/v9.xsd"))
         (http://www.gnumeric.org/v10.dtd:Version
           (@ (Minor "17") (Major "10") (Full "1.10.17") (Epoch "1")))
         (http://www.gnumeric.org/v10.dtd:Attributes
           (http://www.gnumeric.org/v10.dtd:Attribute
             (http://www.gnumeric.org/v10.dtd:type "4")
             (http://www.gnumeric.org/v10.dtd:name
               "WorkbookView::show_horizontal_scrollbar")
             (http://www.gnumeric.org/v10.dtd:value "TRUE"))
           (http://www.gnumeric.org/v10.dtd:Attribute
             (http://www.gnumeric.org/v10.dtd:type "4")
             (http://www.gnumeric.org/v10.dtd:name
               "WorkbookView::show_vertical_scrollbar")
             (http://www.gnumeric.org/v10.dtd:value "TRUE"))
           (http://www.gnumeric.org/v10.dtd:Attribute
             (http://www.gnumeric.org/v10.dtd:type "4")
             (http://www.gnumeric.org/v10.dtd:name
               "WorkbookView::show_notebook_tabs")
             (http://www.gnumeric.org/v10.dtd:value "TRUE"))
           (http://www.gnumeric.org/v10.dtd:Attribute
             (http://www.gnumeric.org/v10.dtd:type "4")
             (http://www.gnumeric.org/v10.dtd:name
               "WorkbookView::do_auto_completion")
             (http://www.gnumeric.org/v10.dtd:value "TRUE"))
           (http://www.gnumeric.org/v10.dtd:Attribute
             (http://www.gnumeric.org/v10.dtd:type "4")
             (http://www.gnumeric.org/v10.dtd:name
               "WorkbookView::is_protected")
             (http://www.gnumeric.org/v10.dtd:value "FALSE")))
         (urn:oasis:names:tc:opendocument:xmlns:office:1.0:document-meta
           (@ (urn:oasis:names:tc:opendocument:xmlns:office:1.0:version "1.2"))
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:meta
             (http://purl.org/dc/elements/1.1/:date "2013-07-26T05:41:51Z")
             (urn:oasis:names:tc:opendocument:xmlns:meta:1.0:creation-date
               "2013-07-26T05:41:10Z")))
         (http://www.gnumeric.org/v10.dtd:Calculation
           (@ (MaxIterations "100")
              (ManualRecalc "0")
              (IterationTolerance "0.001")
              (FloatRadix "2")
              (FloatDigits "53")
              (EnableIteration "1")))
         (http://www.gnumeric.org/v10.dtd:SheetNameIndex
           (http://www.gnumeric.org/v10.dtd:SheetName
             (@ (http://www.gnumeric.org/v10.dtd:Rows "65536")
                (http://www.gnumeric.org/v10.dtd:Cols "256"))
             "Sheet1"))
         (http://www.gnumeric.org/v10.dtd:Geometry
           (@ (Width "1440") (Height "647")))
         (http://www.gnumeric.org/v10.dtd:Sheets
           (http://www.gnumeric.org/v10.dtd:Sheet
             (@ (Visibility "GNM_SHEET_VISIBILITY_VISIBLE")
                (OutlineSymbolsRight "1")
                (OutlineSymbolsBelow "1")
                (HideZero "0")
                (HideRowHeader "0")
                (HideGrid "0")
                (HideColHeader "0")
                (GridColor "0:0:0")
                (DisplayOutlines "1")
                (DisplayFormulas "0"))
             (http://www.gnumeric.org/v10.dtd:Name "Sheet1")
             (http://www.gnumeric.org/v10.dtd:MaxCol "-1")
             (http://www.gnumeric.org/v10.dtd:MaxRow "-1")
             (http://www.gnumeric.org/v10.dtd:Zoom "1")
             (http://www.gnumeric.org/v10.dtd:Names
               (http://www.gnumeric.org/v10.dtd:Name
                 (http://www.gnumeric.org/v10.dtd:name "Print_Area")
                 (http://www.gnumeric.org/v10.dtd:value "#REF!")
                 (http://www.gnumeric.org/v10.dtd:position "A1"))
               (http://www.gnumeric.org/v10.dtd:Name
                 (http://www.gnumeric.org/v10.dtd:name "Sheet_Title")
                 (http://www.gnumeric.org/v10.dtd:value "\"Sheet1\"")
                 (http://www.gnumeric.org/v10.dtd:position "A1")))
             (http://www.gnumeric.org/v10.dtd:PrintInformation
               (http://www.gnumeric.org/v10.dtd:Margins
                 (http://www.gnumeric.org/v10.dtd:top
                   (@ (PrefUnit "mm") (Points "120")))
                 (http://www.gnumeric.org/v10.dtd:bottom
                   (@ (PrefUnit "mm") (Points "120")))
                 (http://www.gnumeric.org/v10.dtd:left
                   (@ (PrefUnit "mm") (Points "72")))
                 (http://www.gnumeric.org/v10.dtd:right
                   (@ (PrefUnit "mm") (Points "72")))
                 (http://www.gnumeric.org/v10.dtd:header
                   (@ (PrefUnit "mm") (Points "72")))
                 (http://www.gnumeric.org/v10.dtd:footer
                   (@ (PrefUnit "mm") (Points "72"))))
               (http://www.gnumeric.org/v10.dtd:Scale
                 (@ (type "percentage") (percentage "100")))
               (http://www.gnumeric.org/v10.dtd:vcenter (@ (value "0")))
               (http://www.gnumeric.org/v10.dtd:hcenter (@ (value "0")))
               (http://www.gnumeric.org/v10.dtd:grid (@ (value "0")))
               (http://www.gnumeric.org/v10.dtd:even_if_only_styles
                 (@ (value "0")))
               (http://www.gnumeric.org/v10.dtd:monochrome (@ (value "0")))
               (http://www.gnumeric.org/v10.dtd:draft (@ (value "0")))
               (http://www.gnumeric.org/v10.dtd:titles (@ (value "0")))
               (http://www.gnumeric.org/v10.dtd:do_not_print (@ (value "0")))
               (http://www.gnumeric.org/v10.dtd:print_range (@ (value "0")))
               (http://www.gnumeric.org/v10.dtd:order "d_then_r")
               (http://www.gnumeric.org/v10.dtd:orientation "portrait")
               (http://www.gnumeric.org/v10.dtd:Header
                 (@ (Right "") (Middle "&[TAB]") (Left "")))
               (http://www.gnumeric.org/v10.dtd:Footer
                 (@ (Right "") (Middle "Page &[PAGE]") (Left "")))
               (http://www.gnumeric.org/v10.dtd:paper "na_letter")
               (http://www.gnumeric.org/v10.dtd:comments "in_place")
               (http://www.gnumeric.org/v10.dtd:errors "as_displayed"))
             (http://www.gnumeric.org/v10.dtd:Styles
               (http://www.gnumeric.org/v10.dtd:StyleRegion
                 (@ (startRow "0")
                    (startCol "0")
                    (endRow "65535")
                    (endCol "255"))
                 (http://www.gnumeric.org/v10.dtd:Style
                   (@ (WrapText "0")
                      (VAlign "2")
                      (ShrinkToFit "0")
                      (Shade "0")
                      (Rotation "0")
                      (PatternColor "0:0:0")
                      (Locked "1")
                      (Indent "0")
                      (Hidden "0")
                      (HAlign "1")
                      (Format "General")
                      (Fore "0:0:0")
                      (Back "FFFF:FFFF:FFFF"))
                   (http://www.gnumeric.org/v10.dtd:Font
                     (@ (Unit "10")
                        (Underline "0")
                        (StrikeThrough "0")
                        (Script "0")
                        (Italic "0")
                        (Bold "0"))
                     "Sans"))))
             (http://www.gnumeric.org/v10.dtd:Cols (@ (DefaultSizePts "48")))
             (http://www.gnumeric.org/v10.dtd:Rows
               (@ (DefaultSizePts "12.75")))
             (http://www.gnumeric.org/v10.dtd:Selections
               (@ (CursorRow "0") (CursorCol "0"))
               (http://www.gnumeric.org/v10.dtd:Selection
                 (@ (startRow "0") (startCol "0") (endRow "0") (endCol "0"))))
             (http://www.gnumeric.org/v10.dtd:Cells)
             (http://www.gnumeric.org/v10.dtd:SheetLayout (@ (TopLeft "A1")))
             (http://www.gnumeric.org/v10.dtd:Solver
               (@ (ProgramR "0")
                  (ProblemType "0")
                  (NonNeg "1")
                  (ModelType "0")
                  (MaxTime "60")
                  (MaxIter "1000")
                  (Discr "0")
                  (AutoScale "0")))))
         (http://www.gnumeric.org/v10.dtd:UIData (@ (SelectedTab "0"))))))

(define sheet-meta
  '(http://www.gnumeric.org/v10.dtd:Sheet
  (@ (Visibility "GNM_SHEET_VISIBILITY_VISIBLE")
     (OutlineSymbolsRight "1")
     (OutlineSymbolsBelow "1")
     (HideZero "0")
     (HideRowHeader "0")
     (HideGrid "0")
     (HideColHeader "0")
     (GridColor "0:0:0")
     (DisplayOutlines "1")
     (DisplayFormulas "0"))
  (http://www.gnumeric.org/v10.dtd:MaxCol "8")
  (http://www.gnumeric.org/v10.dtd:MaxRow "18")
  (http://www.gnumeric.org/v10.dtd:Zoom "1")
  (http://www.gnumeric.org/v10.dtd:Names
    (http://www.gnumeric.org/v10.dtd:Name
      (http://www.gnumeric.org/v10.dtd:name "Print_Area")
      (http://www.gnumeric.org/v10.dtd:value "#REF!")
      (http://www.gnumeric.org/v10.dtd:position "A1"))
    (http://www.gnumeric.org/v10.dtd:Name
      (http://www.gnumeric.org/v10.dtd:name "Sheet_Title")
      (http://www.gnumeric.org/v10.dtd:value "\"First_Sheet\"")
      (http://www.gnumeric.org/v10.dtd:position "A1")))
  (http://www.gnumeric.org/v10.dtd:PrintInformation
    (http://www.gnumeric.org/v10.dtd:Margins
      (http://www.gnumeric.org/v10.dtd:top
        (@ (PrefUnit "mm") (Points "93.26")))
      (http://www.gnumeric.org/v10.dtd:bottom
        (@ (PrefUnit "mm") (Points "93.26")))
      (http://www.gnumeric.org/v10.dtd:left (@ (PrefUnit "mm") (Points "72")))
      (http://www.gnumeric.org/v10.dtd:right (@ (PrefUnit "mm") (Points "72")))
      (http://www.gnumeric.org/v10.dtd:header
        (@ (PrefUnit "mm") (Points "72")))
      (http://www.gnumeric.org/v10.dtd:footer
        (@ (PrefUnit "mm") (Points "72"))))
    (http://www.gnumeric.org/v10.dtd:Scale
      (@ (type "percentage") (percentage "100")))
    (http://www.gnumeric.org/v10.dtd:vcenter (@ (value "0")))
    (http://www.gnumeric.org/v10.dtd:hcenter (@ (value "0")))
    (http://www.gnumeric.org/v10.dtd:grid (@ (value "0")))
    (http://www.gnumeric.org/v10.dtd:even_if_only_styles (@ (value "0")))
    (http://www.gnumeric.org/v10.dtd:monochrome (@ (value "0")))
    (http://www.gnumeric.org/v10.dtd:draft (@ (value "0")))
    (http://www.gnumeric.org/v10.dtd:titles (@ (value "0")))
    (http://www.gnumeric.org/v10.dtd:do_not_print (@ (value "0")))
    (http://www.gnumeric.org/v10.dtd:print_range (@ (value "0")))
    (http://www.gnumeric.org/v10.dtd:order "d_then_r")
    (http://www.gnumeric.org/v10.dtd:orientation "portrait")
    (http://www.gnumeric.org/v10.dtd:Header
      (@ (Right "") (Middle "&[tab]") (Left "")))
    (http://www.gnumeric.org/v10.dtd:Footer
      (@ (Right "") (Middle "&[page]") (Left "")))
    (http://www.gnumeric.org/v10.dtd:paper "na_letter")
    (http://www.gnumeric.org/v10.dtd:comments "in_place")
    (http://www.gnumeric.org/v10.dtd:errors "as_displayed"))
  (http://www.gnumeric.org/v10.dtd:Styles
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "0") (startCol "0") (endRow "0") (endCol "1"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "General")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans")))
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "1") (startCol "0") (endRow "17") (endCol "1"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "hh\":\"mm\":\"ss AM/PM")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans")))
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "18") (startCol "0") (endRow "31") (endCol "2"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "General")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans")))
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "32") (startCol "0") (endRow "255") (endCol "7"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "General")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans")))
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "256") (startCol "0") (endRow "65535") (endCol "63"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "General")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans")))
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "0") (startCol "2") (endRow "1") (endCol "2"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "General")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans")))
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "2") (startCol "2") (endRow "17") (endCol "2"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "0")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans")))
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "0") (startCol "3") (endRow "31") (endCol "7"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "General")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans")))
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "0") (startCol "8") (endRow "255") (endCol "63"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "General")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans")))
    (http://www.gnumeric.org/v10.dtd:StyleRegion
      (@ (startRow "0") (startCol "64") (endRow "65535") (endCol "255"))
      (http://www.gnumeric.org/v10.dtd:Style
        (@ (WrapText "0")
           (VAlign "2")
           (ShrinkToFit "0")
           (Shade "0")
           (Rotation "0")
           (PatternColor "0:0:0")
           (Locked "1")
           (Indent "0")
           (Hidden "0")
           (HAlign "1")
           (Format "General")
           (Fore "0:0:0")
           (Back "FFFF:FFFF:FFFF"))
        (http://www.gnumeric.org/v10.dtd:Font
          (@ (Unit "10")
             (Underline "0")
             (StrikeThrough "0")
             (Script "0")
             (Italic "0")
             (Bold "0"))
          "Sans"))))
  (http://www.gnumeric.org/v10.dtd:Cols
    (@ (DefaultSizePts "48"))
    (http://www.gnumeric.org/v10.dtd:ColInfo (@ (Unit "48") (No "0")))
    (http://www.gnumeric.org/v10.dtd:ColInfo
      (@ (Unit "99") (No "1") (HardSize "1")))
    (http://www.gnumeric.org/v10.dtd:ColInfo
      (@ (Unit "64.01") (No "2") (Count "7"))))
  (http://www.gnumeric.org/v10.dtd:Rows
    (@ (DefaultSizePts "12.1"))
    (http://www.gnumeric.org/v10.dtd:RowInfo (@ (Unit "12.64") (No "0")))
    (http://www.gnumeric.org/v10.dtd:RowInfo
      (@ (Unit "13.5") (No "1") (Count "17")))
    (http://www.gnumeric.org/v10.dtd:RowInfo (@ (Unit "12.1") (No "18"))))
  (http://www.gnumeric.org/v10.dtd:Selections
    (@ (CursorRow "3") (CursorCol "1"))
    (http://www.gnumeric.org/v10.dtd:Selection
      (@ (startRow "3") (startCol "1") (endRow "3") (endCol "1"))))
  (http://www.gnumeric.org/v10.dtd:SheetLayout
    (@ (TopLeft "A2"))
    (http://www.gnumeric.org/v10.dtd:FreezePanes
      (@ (UnfrozenTopLeft "A2") (FrozenTopLeft "A1"))))
  (http://www.gnumeric.org/v10.dtd:Solver
    (@ (ProgramR "0")
       (ProblemType "0")
       (NonNeg "1")
       (ModelType "0")
       (MaxTime "60")
       (MaxIter "1000")
       (Discr "0")
       (AutoScale "0")))))

(define sheets-meta
  '((@ (http://www.w3.org/2001/XMLSchema-instance:schemaLocation
      "http://www.gnumeric.org/v9.xsd"))
 (http://www.gnumeric.org/v10.dtd:Version
   (@ (Minor "17") (Major "10") (Full "1.10.17") (Epoch "1")))
 (http://www.gnumeric.org/v10.dtd:Attributes
   (http://www.gnumeric.org/v10.dtd:Attribute
     (http://www.gnumeric.org/v10.dtd:type "4")
     (http://www.gnumeric.org/v10.dtd:name
       "WorkbookView::show_horizontal_scrollbar")
     (http://www.gnumeric.org/v10.dtd:value "TRUE"))
   (http://www.gnumeric.org/v10.dtd:Attribute
     (http://www.gnumeric.org/v10.dtd:type "4")
     (http://www.gnumeric.org/v10.dtd:name
       "WorkbookView::show_vertical_scrollbar")
     (http://www.gnumeric.org/v10.dtd:value "TRUE"))
   (http://www.gnumeric.org/v10.dtd:Attribute
     (http://www.gnumeric.org/v10.dtd:type "4")
     (http://www.gnumeric.org/v10.dtd:name "WorkbookView::show_notebook_tabs")
     (http://www.gnumeric.org/v10.dtd:value "TRUE"))
   (http://www.gnumeric.org/v10.dtd:Attribute
     (http://www.gnumeric.org/v10.dtd:type "4")
     (http://www.gnumeric.org/v10.dtd:name "WorkbookView::do_auto_completion")
     (http://www.gnumeric.org/v10.dtd:value "TRUE"))
   (http://www.gnumeric.org/v10.dtd:Attribute
     (http://www.gnumeric.org/v10.dtd:type "4")
     (http://www.gnumeric.org/v10.dtd:name "WorkbookView::is_protected")
     (http://www.gnumeric.org/v10.dtd:value "FALSE")))
 (urn:oasis:names:tc:opendocument:xmlns:office:1.0:document-meta
   (@ (urn:oasis:names:tc:opendocument:xmlns:office:1.0:version "1.2"))
   (urn:oasis:names:tc:opendocument:xmlns:office:1.0:meta
     (http://purl.org/dc/elements/1.1/:date "2013-07-26T04:47:02Z")
     (urn:oasis:names:tc:opendocument:xmlns:meta:1.0:creation-date
       "2013-07-26T04:46:14Z")))
 (http://www.gnumeric.org/v10.dtd:Calculation
   (@ (MaxIterations "100")
      (ManualRecalc "0")
      (IterationTolerance "0.001")
      (FloatRadix "2")
      (FloatDigits "53")
      (EnableIteration "1")))
 (http://www.gnumeric.org/v10.dtd:SheetNameIndex
   (http://www.gnumeric.org/v10.dtd:SheetName
     (@ (http://www.gnumeric.org/v10.dtd:Rows "65536")
        (http://www.gnumeric.org/v10.dtd:Cols "256"))
     "First_Sheet")
   (http://www.gnumeric.org/v10.dtd:SheetName
     (@ (http://www.gnumeric.org/v10.dtd:Rows "65536")
        (http://www.gnumeric.org/v10.dtd:Cols "256"))
     "Second-sheet")
   (http://www.gnumeric.org/v10.dtd:SheetName
     (@ (http://www.gnumeric.org/v10.dtd:Rows "65536")
        (http://www.gnumeric.org/v10.dtd:Cols "256"))
     "RunsToDo")
   (http://www.gnumeric.org/v10.dtd:SheetName
     (@ (http://www.gnumeric.org/v10.dtd:Rows "65536")
        (http://www.gnumeric.org/v10.dtd:Cols "256"))
     "RunsToLock"))
 (http://www.gnumeric.org/v10.dtd:Geometry (@ (Width "1440") (Height "647")))
 (http://www.gnumeric.org/v10.dtd:UIData (@ (SelectedTab "1")))))

(define workbook-meta
  '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")))


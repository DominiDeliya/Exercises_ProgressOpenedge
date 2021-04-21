&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustomer NO-UNDO LIKE Customer.
DEFINE TEMP-TABLE ttOrder    NO-UNDO LIKE Order.
DEFINE TEMP-TABLE ttInvoice  NO-UNDO LIKE Invoice.

DEFINE VARIABLE docName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE totalAmmount LIKE Invoice.TotalPaid NO-UNDO.
DEFINE VARIABLE printOption AS CHARACTER NO-UNDO. 


DEFINE TEMP-TABLE ttInvoicePrint NO-UNDO     
    FIELD customerNum       LIKE Customer.CustNum
    FIELD customerName      LIKE Customer.Name
    FIELD customerAddress   LIKE Customer.Address
    FIELD customerAddress2  LIKE Customer.Address2
    FIELD invoiceNum        LIKE Invoice.Invoicenum
    FIELD invoiceDate       LIKE Invoice.InvoiceDate
    FIELD invoiceAmmount    LIKE Invoice.Amount
    FIELD invoiceShipCharge LIKE Invoice.ShipCharge
    FIELD invoiceAdjustment LIKE Invoice.Adjustment
    FIELD invoiceTotalPaid  LIKE Invoice.TotalPaid
    FIELD orderNum          LIKE Invoice.OrderNum.
    
    
DEFINE TEMP-TABLE ttInvoicePrintTable NO-UNDO
    FIELD itemNum           LIKE OrderLine.Itemnum
    FIELD qty               LIKE OrderLine.Qty
    FIELD pricePerItem      LIKE OrderLine.Price
    FIELD price             LIKE OrderLine.Price
    FIELD discount          LIKE OrderLine.Discount
    FIELD extendedPrice     LIKE OrderLine.ExtendedPrice.
    //FIELD invoiceNum        LIKE Invoice.Invoicenum .

DEFINE DATASET dsOrderLog FOR ttCustomer, ttOrder, ttInvoice
  DATA-RELATION CustOrd FOR ttCustomer,
    ttOrder RELATION-FIELDS(CustNum,CustNum) NESTED
  DATA-RELATION OrdInv FOR ttOrder,
    ttInvoice RELATION-FIELDS(OrderNum,OrderNum) NESTED.
    
{ pdf_inc.i}    

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FrMain
&Scoped-define BROWSE-NAME brOrders

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Order Customer Invoice

/* Definitions for BROWSE brOrders                                      */
&Scoped-define FIELDS-IN-QUERY-brOrders Order.CustNum Order.Ordernum ~
Order.OrderDate Order.OrderStatus Order.PromiseDate 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brOrders 
&Scoped-define QUERY-STRING-brOrders FOR EACH Order OF Customer ~
      WHERE Order.OrderStatus = 'Shipped' NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brOrders OPEN QUERY brOrders FOR EACH Order OF Customer ~
      WHERE Order.OrderStatus = 'Shipped' NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brOrders Order
&Scoped-define FIRST-TABLE-IN-QUERY-brOrders Order


/* Definitions for FRAME FrMain                                         */
&Scoped-define FIELDS-IN-QUERY-FrMain Customer.Name Customer.CustNum ~
Invoice.CustNum Invoice.Amount Invoice.ShipCharge Invoice.OrderNum ~
Invoice.Adjustment Invoice.Invoicenum Invoice.TotalPaid Invoice.InvoiceDate 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FrMain Customer.CustNum 
&Scoped-define ENABLED-TABLES-IN-QUERY-FrMain Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FrMain Customer
&Scoped-define OPEN-BROWSERS-IN-QUERY-FrMain ~
    ~{&OPEN-QUERY-brOrders}
&Scoped-define QUERY-STRING-FrMain FOR EACH Customer SHARE-LOCK, ~
      EACH Invoice OF Customer SHARE-LOCK
&Scoped-define OPEN-QUERY-FrMain OPEN QUERY FrMain FOR EACH Customer SHARE-LOCK, ~
      EACH Invoice OF Customer SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FrMain Customer Invoice
&Scoped-define FIRST-TABLE-IN-QUERY-FrMain Customer
&Scoped-define SECOND-TABLE-IN-QUERY-FrMain Invoice


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.CustNum 
&Scoped-define ENABLED-TABLES Customer
&Scoped-define FIRST-ENABLED-TABLE Customer
&Scoped-Define ENABLED-OBJECTS rcOrders rcOrderLine brOrders 
&Scoped-Define DISPLAYED-FIELDS Customer.Name Customer.CustNum ~
Invoice.CustNum Invoice.Amount Invoice.ShipCharge Invoice.OrderNum ~
Invoice.Adjustment Invoice.Invoicenum Invoice.TotalPaid Invoice.InvoiceDate 
&Scoped-define DISPLAYED-TABLES Customer Invoice
&Scoped-define FIRST-DISPLAYED-TABLE Customer
&Scoped-define SECOND-DISPLAYED-TABLE Invoice
&Scoped-Define DISPLAYED-OBJECTS cbPrintOptions 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Invoice.CustNum Invoice.Amount Invoice.ShipCharge ~
Invoice.OrderNum Invoice.Adjustment Invoice.Invoicenum Invoice.TotalPaid ~
Invoice.InvoiceDate 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON buPrint 
     LABEL "Print" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbPrintOptions AS CHARACTER FORMAT "X(256)":U 
     LABEL "Select an option to Print" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "PDF","XML","EXCEL","JSON" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE rcOrderLine
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 103 BY 9.76.

DEFINE RECTANGLE rcOrders
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 103 BY 7.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brOrders FOR 
      Order SCROLLING.

DEFINE QUERY FrMain FOR 
      Customer, 
      Invoice SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brOrders C-Win _STRUCTURED
  QUERY brOrders NO-LOCK DISPLAY
      Order.CustNum FORMAT ">>>>9":U
      Order.Ordernum FORMAT "zzzzzzzzz9":U WIDTH 23.2
      Order.OrderDate FORMAT "99/99/99":U WIDTH 22.2
      Order.OrderStatus FORMAT "x(20)":U WIDTH 25.2
      Order.PromiseDate FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 100 BY 6.43 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FrMain
     Customer.Name AT ROW 4.19 COL 47 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 43 BY 1
     Customer.CustNum AT ROW 4.24 COL 14.6 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          FONT 0
     brOrders AT ROW 7.19 COL 6 WIDGET-ID 200
     Invoice.CustNum AT ROW 16.19 COL 19 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Invoice.Amount AT ROW 16.29 COL 79 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Invoice.ShipCharge AT ROW 18.1 COL 79 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Invoice.OrderNum AT ROW 18.19 COL 19 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Invoice.Adjustment AT ROW 20 COL 79 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Invoice.Invoicenum AT ROW 20.19 COL 19 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Invoice.TotalPaid AT ROW 21.91 COL 79 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Invoice.InvoiceDate AT ROW 22 COL 19 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     buPrint AT ROW 26.43 COL 72.8 WIDGET-ID 44
     cbPrintOptions AT ROW 26.48 COL 37.4 COLON-ALIGNED WIDGET-ID 42
     "          Invoice Print" VIEW-AS TEXT
          SIZE 32 BY 1.14 AT ROW 1.76 COL 40.8 WIDGET-ID 4
          BGCOLOR 7 FGCOLOR 14 FONT 6
     rcOrders AT ROW 6.48 COL 4 WIDGET-ID 6
     rcOrderLine AT ROW 14.81 COL 4 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.8 ROW 1.67
         SIZE 110.2 BY 27.43 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Invoice Print App"
         HEIGHT             = 28.19
         WIDTH              = 112.8
         MAX-HEIGHT         = 30.62
         MAX-WIDTH          = 124
         VIRTUAL-HEIGHT     = 30.62
         VIRTUAL-WIDTH      = 124
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FrMain
   FRAME-NAME                                                           */
/* BROWSE-TAB brOrders CustNum FrMain */
/* SETTINGS FOR FILL-IN Invoice.Adjustment IN FRAME FrMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Invoice.Amount IN FRAME FrMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON buPrint IN FRAME FrMain
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbPrintOptions IN FRAME FrMain
   NO-ENABLE                                                            */
ASSIGN 
       cbPrintOptions:HIDDEN IN FRAME FrMain           = TRUE.

/* SETTINGS FOR FILL-IN Invoice.CustNum IN FRAME FrMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Invoice.InvoiceDate IN FRAME FrMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Invoice.Invoicenum IN FRAME FrMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Customer.Name IN FRAME FrMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Invoice.OrderNum IN FRAME FrMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Invoice.ShipCharge IN FRAME FrMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Invoice.TotalPaid IN FRAME FrMain
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brOrders
/* Query rebuild information for BROWSE brOrders
     _TblList          = "sports2000.Order OF sports2000.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Order.OrderStatus = 'Shipped'"
     _FldNameList[1]   = sports2000.Order.CustNum
     _FldNameList[2]   > sports2000.Order.Ordernum
"Order.Ordernum" ? ? "integer" ? ? ? ? ? ? no ? no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > sports2000.Order.OrderDate
"Order.OrderDate" ? ? "date" ? ? ? ? ? ? no ? no no "22.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > sports2000.Order.OrderStatus
"Order.OrderStatus" ? ? "character" ? ? ? ? ? ? no ? no no "25.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = sports2000.Order.PromiseDate
     _Query            is OPENED
*/  /* BROWSE brOrders */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FrMain
/* Query rebuild information for FRAME FrMain
     _TblList          = "sports2000.Customer,sports2000.Invoice OF sports2000.Customer"
     _Query            is NOT OPENED
*/  /* FRAME FrMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Invoice Print App */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Invoice Print App */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brOrders
&Scoped-define SELF-NAME brOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brOrders C-Win
ON VALUE-CHANGED OF brOrders IN FRAME FrMain
DO:

    FIND Invoice WHERE Invoice.OrderNum = Order.Ordernum NO-LOCK NO-ERROR.
    IF AVAILABLE Invoice THEN 
    DO  WITH FRAME {&FRAME-NAME}:
        DISPLAY  buPrint cbPrintOptions.
        ENABLE buPrint cbPrintOptions.
        DISPLAY {&List-1} . //WITH FRAME {&FRAME-NAME} . 
        
    END.
    ELSE 
    DO:
        DISPLAY //''@ Invoice.CustNum 
                ''@  Invoice.Amount
                ''@  Invoice.ShipCharge  
                ''@  Invoice.OrderNum    
                ''@  Invoice.Adjustment  
                ''@ Invoice.Invoicenum  
                ''@ Invoice.TotalPaid   
                ''@ Invoice.InvoiceDate WITH FRAME {&FRAME-NAME}.
       
         
     END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buPrint C-Win
ON CHOOSE OF buPrint IN FRAME FrMain /* Print */
DO:
    IF Customer.CustNum = 0 THEN 
    DO:
        MESSAGE "Please enter the customer number"
        VIEW-AS ALERT-BOX.
    END.
    cbPrintOptions = "".    
    RUN FetchData.
       
    IF printOption = "PDF" THEN 
    DO:
        RUN PrintInvoicePdf.
        MESSAGE "Print the PDF Sussesfully"
        VIEW-AS ALERT-BOX.
        
    END.        
    ELSE IF printOption = "EXCEL" THEN 
    DO:
        docName = String(Invoice.OrderNum).
        RUN PrintExcel.
        MESSAGE "Print the Excel Succesffully"
        VIEW-AS ALERT-BOX.
        
    END.        
    ELSE IF printOption = "JSON" THEN 
    DO:
        RUN PrintJson.
        MESSAGE "Print the Json Succesffully"
        VIEW-AS ALERT-BOX.
        
    END.
        
    ELSE IF printOption = "XML" THEN 
    DO:
        RUN PrintXml.
        MESSAGE "Print the Xml Succesffully"
        VIEW-AS ALERT-BOX.
    END.       
    ELSE 
        MESSAGE "Please Select an option to print the invoice"
        VIEW-AS ALERT-BOX.
            
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbPrintOptions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbPrintOptions C-Win
ON VALUE-CHANGED OF cbPrintOptions IN FRAME FrMain /* Select an option to Print */
DO:
    ASSIGN cbPrintOptions.
    ASSIGN printOption = cbPrintOptions.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Customer.CustNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.CustNum C-Win
ON RETURN OF Customer.CustNum IN FRAME FrMain /* Cust Num */
DO:
    FIND Customer NO-LOCK
                  WHERE Customer.CustNum = INTEGER( Customer.CustNum:SCREEN-VALUE )
                  NO-ERROR.
    IF AVAILABLE Customer THEN DO:
        ASSIGN Customer.Name:SCREEN-VALUE = Customer.Name.
        {&OPEN-QUERY-brOrders}
        APPLY 'VALUE-CHANGED' TO brOrders.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cbPrintOptions 
      WITH FRAME FrMain IN WINDOW C-Win.
  IF AVAILABLE Customer THEN 
    DISPLAY Customer.Name Customer.CustNum 
      WITH FRAME FrMain IN WINDOW C-Win.
  IF AVAILABLE Invoice THEN 
    DISPLAY Invoice.CustNum Invoice.Amount Invoice.ShipCharge Invoice.OrderNum 
          Invoice.Adjustment Invoice.Invoicenum Invoice.TotalPaid 
          Invoice.InvoiceDate 
      WITH FRAME FrMain IN WINDOW C-Win.
  ENABLE rcOrders rcOrderLine Customer.CustNum brOrders 
      WITH FRAME FrMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FrMain}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FetchData C-Win 
PROCEDURE FetchData :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE ttInvoicePrint.
EMPTY TEMP-TABLE ttInvoicePrintTable.
CREATE  ttInvoicePrint.  
    ASSIGN  ttInvoicePrint.customerNum       = Customer.CustNum
            ttInvoicePrint.customerName      = Customer.Name
            ttInvoicePrint.customerAddress   = Customer.Address
            ttInvoicePrint.customerAddress2  = Customer.Address2
            ttInvoicePrint.invoiceNum        = Invoice.Invoicenum
            ttInvoicePrint.invoiceDate       = Invoice.InvoiceDate
            ttInvoicePrint.invoiceAmmount    = Invoice.Amount
            ttInvoicePrint.invoiceShipCharge = Invoice.ShipCharge
            ttInvoicePrint.invoiceAdjustment = Invoice.Adjustment
            ttInvoicePrint.invoiceTotalPaid  = Invoice.TotalPaid
            ttInvoicePrint.orderNum          = Invoice.OrderNum.
            
            
    FOR EACH OrderLine WHERE OrderLine.OrderNum = Invoice.OrderNum NO-LOCK:
        
        CREATE ttInvoicePrintTable.  
        ASSIGN  ttInvoicePrintTable.itemNum           = OrderLine.Itemnum
                ttInvoicePrintTable.qty               = OrderLine.Qty
                ttInvoicePrintTable.pricePerItem      = OrderLine.Price
                ttInvoicePrintTable.price             = OrderLine.Price
                ttInvoicePrintTable.discount          = OrderLine.Discount
                ttInvoicePrintTable.extendedPrice     = OrderLine.ExtendedPrice.
        
    END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintExcel C-Win 
PROCEDURE PrintExcel :
/*------------------------------------------------------------------------------
 Purpose: Print an excel file including details of selected invoice number
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hExcel        AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hWorkbook     AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hWorksheet    AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hBufferHandle AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cTableName    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCounter      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE hFieldHandle  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hQueryHandle  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE iRowNum       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE ColumnRange   AS CHARACTER  NO-UNDO.

    CREATE "Excel.Application" hExcel NO-ERROR .
    ASSIGN
        hExcel:VISIBLE = FALSE 
        hWorkbook      = hExcel:Workbooks:Add()
        hWorkSheet     = hExcel:Sheets:Item(1)
        cTableName     = "ttInvoicePrintTable"
        iRowNum        = 1.


    /* Pump field names as EXCEL column headers*/
    CREATE BUFFER  hBufferHandle FOR TABLE cTableName.
    DO iCounter = 1 TO hBufferHandle:NUM-FIELDS:
        hFieldHandle = hBufferHandle:BUFFER-FIELD(iCounter).
        hExcel:Worksheets("Sheet1"):Cells(iRowNum,iCounter ) = 
    hFieldHandle:NAME.
    END.
    
    /* Create dynamic query for the cTablename */
    CREATE QUERY  hQueryHandle.
    hQueryHandle:SET-BUFFERS(hBufferHandle).
    hQueryHandle:QUERY-PREPARE("for each " + cTableName).
    hQueryHandle:QUERY-OPEN. 

    /* Pump the table data into your worksheet */
    REPEAT:
        iRowNum = iRowNum + 1.
        hQueryHandle:GET-NEXT().
        IF hQueryHandle:QUERY-OFF-END THEN LEAVE.
        ELSE DO iCounter = 1 TO hBufferHandle:NUM-FIELDS:
            hFieldHandle = hBufferHandle:BUFFER-FIELD(iCounter).
            hExcel:Worksheets("Sheet1"):Cells(iRowNum,iCounter ) = hFieldHandle:BUFFER-VALUE.
        END. 
    END.

    iRowNum = iRowNum + 1.
    hExcel:Worksheets("Sheet1"):Cells(iRowNum,(iCounter - 2) ) = "Total".
    hExcel:Worksheets("Sheet1"):Cells(iRowNum,(iCounter - 1) ) = totalAmmount.
    
    /* Define populated column range and auto-format width */
    /* Hint: First Column is column "A" hence CHR(65) usage*/
    
    ASSIGN 
        ColumnRange = CHR(65) + ":" + CHR(65 + hBufferHandle:NUM-FIELDS - 1).
    
    hExcel:COLUMNS(ColumnRange):SELECT.
    hExcel:SELECTION:COLUMNS:AUTOFIT.
    
    hWorkbook:SaveAs("C:\Users\Domini\Progress\Developer Studio 4.9\workspace\InvoicePrint\" + docName + ".xlsx" , 51, "", "", false, false, ) NO-ERROR . /* 51 = xlOpenXMLWorkbook */
    /* Perform housekeeping and cleanup steps */
    hExcel:Application:Workbooks:CLOSE() NO-ERROR.
    hExcel:Application:QUIT NO-ERROR.
    RELEASE OBJECT hWorksheet.
    RELEASE OBJECT hWorkbook.
    RELEASE OBJECT hExcel.
    DELETE OBJECT hBufferHandle.
    DELETE OBJECT hQueryHandle.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintInvoicePdf C-Win 
PROCEDURE PrintInvoicePdf :
/*------------------------------------------------------------------------------
 Purpose:Print a PDF file including details of selected invoice number
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h_TT AS HANDLE NO-UNDO.
     
    FOR EACH  ttInvoicePrint NO-LOCK BREAK BY ttInvoicePrint.invoiceNum: //BREAK BY ttInvoicePrint.itemNum:

        IF FIRST-OF (ttInvoicePrint.invoiceNum ) THEN 
        DO:
            /* create a new pdf file, name the stream "Spdf" */
            RUN pdf_new("Spdf", "Invoice.pdf").
            
            /* create a new page in our pdf */
            RUN pdf_new_page("Spdf").
                     
            /* Don't want the table to be going until the end of the page or too near the 
            side */
            RUN pdf_set_LeftMargin("Spdf",50).
            RUN pdf_set_BottomMargin("Spdf",50).
            
            /* output texts */
            RUN pdf_set_font("Spdf","Helvetica",22).            
            RUN pdf_text_at("Spdf", "INVOICE", 1 ).                                RUN pdf_skipn ("Spdf",2).
            RUN pdf_set_font("Spdf","Helvetica",10).
            RUN pdf_text_at ("Spdf", "From : " ,1).
            RUN pdf_text_at ("Spdf", "Invoice Number     :" ,120). 
            RUN pdf_text_at ("Spdf", STRING (ttInvoicePrint.invoiceNum) ,160).      RUN pdf_skipn ("Spdf",2).
            RUN pdf_text_at ("Spdf", "BVL IT " ,4).
            RUN pdf_text_at ("Spdf", "Order Number       :" ,120).
            RUN pdf_text_at ("Spdf", STRING (ttInvoicePrint.orderNum) ,160).        RUN pdf_skipn ("Spdf",2).
            RUN pdf_text_at ("Spdf", "Kon. Elisabethlei 49 " ,4).
            RUN pdf_text_at ("Spdf", "Invoice Date          :" ,120).
            RUN pdf_text_at ("Spdf", TRIM (STRING (ttInvoicePrint.invoiceDate) ),160).     RUN pdf_skipn ("Spdf",2).
            RUN pdf_text_at ("Spdf", "2300 Turnhout " ,4).
            RUN pdf_text_at ("Spdf", "Total Amount      :" ,120).
            RUN pdf_text_at ("Spdf", STRING (ttInvoicePrint.invoiceAmmount) ,160).  RUN pdf_skipn ("Spdf",5).
            RUN pdf_text_at ("Spdf", "To: " ,1).                                    RUN pdf_skipn("Spdf",2).
            RUN pdf_text_at ("Spdf", STRING (ttInvoicePrint.customerName) ,4).      RUN pdf_skipn("Spdf",2).
            RUN pdf_text_at ("Spdf", STRING (ttInvoicePrint.customerAddress) ,4).   RUN pdf_skipn("Spdf",2).
            RUN pdf_text_at ("Spdf", STRING (ttInvoicePrint.customerAddress2) ,4).  RUN pdf_skipn("Spdf",2).
         
        END.
       
        h_TT = TEMP-TABLE ttInvoicePrintTable  :HANDLE.
        /* Add a Table */
        RUN pdf_tool_add("Spdf","TABLE","TABLE",h_TT).
        
        /* Define Table Column Headers */
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnHeader",1,"Item Number").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnHeader",2,"Quantity").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnHeader",3,"Item Price").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnHeader",4,"Price").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnHeader",5,"Discount").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnHeader",6,"Extended Price").
        /* Define Table Column Widths */
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnWidth",1,"15").
        //RUN pdf_set_tool_parameter("Spdf","TABLE","Outline",1,"10").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnWidth",2,"10").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnWidth",3,"15").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnWidth",4,"15").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnWidth",5,"10").
        RUN pdf_set_tool_parameter("Spdf","TABLE","ColumnWidth",6,"15").
               
        
        /*create table */
        RUN pdf_tool_create("Spdf","TABLE").
        
        /* clean the table */      
        RUN pdf_tool_destroy("Spdf","TABLE").
        
        RUN pdf_skipn("Spdf",4). 
        RUN pdf_text_at ("Spdf", "Total Amount :" ,140).       
        RUN pdf_text_at ("Spdf", STRING (ttInvoicePrint.invoiceAmmount) ,170). RUN pdf_skipn("Spdf",2).
         
         
        /* Load Logo File */
        RUN pdf_load_image ("Spdf","Logo-Design1","C:\Users\Domini\Progress\Developer Studio 4.9\workspace\InvoicePrint\Logo-Design1.jpg").
        
        /* place image in desired position on pdf*/
        RUN pdf_place_image ("Spdf","Logo-Design1",500,60,"50","50").
        
        //RUN pdf_text_boxed_xy("Spdf","",10,10,10,10,"Left,Right,Center",2).         
         /* close the file (this will create "invoice.pdf") */
        RUN pdf_close("Spdf").
                
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintJson C-Win 
PROCEDURE PrintJson :
/*------------------------------------------------------------------------------
 Purpose: print invoice details to a json file
 Notes:
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFormatted  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRetOK      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cEncoding AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lMinSchema AS LOGICAL   NO-UNDO.
    
    DEFINE DATA-SOURCE dsCustomer FOR Customer.
    DEFINE DATA-SOURCE dsOrder    FOR Order.
    DEFINE DATA-SOURCE dsInvoice  FOR Invoice.
    
     
    BUFFER ttCustomer:HANDLE:ATTACH-DATA-SOURCE(DATA-SOURCE dsCustomer:HANDLE).
    BUFFER ttOrder:HANDLE:ATTACH-DATA-SOURCE(DATA-SOURCE dsOrder:HANDLE).
    BUFFER ttInvoice:HANDLE:ATTACH-DATA-SOURCE(DATA-SOURCE dsInvoice:HANDLE).
    
    DATA-SOURCE dsCustomer:FILL-WHERE-STRING = "WHERE Customer.CustNum = "   + STRING (Customer.CustNum). 
    DATA-SOURCE dsOrder:FILL-WHERE-STRING    = "WHERE Order.OrderNum = "     + STRING (Order.OrderNum).
    DATA-SOURCE dsInvoice:FILL-WHERE-STRING  = "WHERE Invoice.InvoiceNum = " + STRING (Invoice.InvoiceNum).
    
    DATASET dsOrderLog:FILL().
    
    ASSIGN
      cTargetType = "file"
      cFile       = "Invoice.json"
      lFormatted  = TRUE.
  
    lRetOK = DATASET dsOrderLog:WRITE-JSON(cTargetType, cFile, lFormatted).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintXml C-Win 
PROCEDURE PrintXml :
/*------------------------------------------------------------------------------
 Purpose:print invoice details to a xml 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hDoc   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hDoc2   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hRoot  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hRow   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hRoot2 AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hRow2  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hField AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hText  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hField2 AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hText2  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hBuf   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hBuf2  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hDBFld AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hDBFld2 AS HANDLE  NO-UNDO.
    DEFINE VARIABLE ix     AS INTEGER NO-UNDO.
    
    CREATE X-DOCUMENT hDoc.
    CREATE X-DOCUMENT hDoc2.
    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hRow.
    CREATE X-NODEREF hRoot2.
    CREATE X-NODEREF hRow2.
    CREATE X-NODEREF hField.
    CREATE X-NODEREF hText.
    
    hBuf = BUFFER ttInvoicePrint:HANDLE.
    hBuf2 = BUFFER ttInvoicePrintTable:HANDLE.

    /* Set up a root node */
    hDoc:CREATE-NODE(hRoot,"Invoice","ELEMENT").
    hDoc:APPEND-CHILD(hRoot).
    //hDoc:CREATE-NODE(hRoot2,"Items","ELEMENT").
    //hDoc:APPEND-CHILD(hRoot2).
    
    FOR EACH ttInvoicePrint :
        
        FOR EACH ttInvoicePrintTable : //WHERE ttInvoicePrintTable.invoiceNum = ttInvoicePrint.invoiceNum :
        
            hDoc:CREATE-NODE(hRow,"Invoice","ELEMENT"). /* create a row node */
            hRoot:APPEND-CHILD(hRow).  /* put the row in the tree */
            hRow:SET-ATTRIBUTE("CustNum", STRING(ttInvoicePrint.CustomerNum)).
            //hRow:SET-ATTRIBUTE("InvoiceNum", STRING (ttInvoicePrint.invoiceNum)).
            
    /*        hDoc:CREATE-NODE(hRow2,"Items","ELEMENT"). /* create a row node */*/
    /*        hRoot:APPEND-CHILD(hRow2).  /* put the row in the tree */         */
    
            hRow:SET-ATTRIBUTE("ItemNum", STRING(ttInvoicePrintTable.itemNum)).
            hRow:SET-ATTRIBUTE("InvoiceNum", STRING(ttInvoicePrint.invoiceNum)).
           
              /* Add the other fields as tags in the xml */
            REPEAT ix = 1 TO hBuf2:NUM-FIELDS:
                
                hDBFld = hBuf2:BUFFER-FIELD(ix).
                IF hDBFld:NAME = "ItemNum" OR  hDBFld:NAME = "InvoiceNum" OR hDBFld:NAME = "CustNum"  THEN NEXT.
        
                /* Create a tag with the field name */
                hDoc:CREATE-NODE(hField, hDBFld:NAME, "ELEMENT").
        
                /* Put the new field as next child of row */
                hRow:APPEND-CHILD(hField).
        
                /* Add a node to hold field value. The empty string ("") represents the
                   value that will be set later. */
                hDoc:CREATE-NODE(hText, "", "TEXT").
        
                /* Attach the text to the field */
                hField:APPEND-CHILD(hText).
                hText:NODE-VALUE = STRING(hDBFld:BUFFER-VALUE).
            END.
        END.
    END.

    /* Write the XML node tree to an xml file */
    hDoc:SAVE("file","InvoiceDetails.xml").
    
    
    DELETE OBJECT hDoc.
    DELETE OBJECT hRoot.
    DELETE OBJECT hRow.
    DELETE OBJECT hField.
    DELETE OBJECT hText.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iNumber AS INTEGER NO-UNDO.
DEFINE VARIABLE lValid  AS LOGICAL NO-UNDO  INITIAL TRUE .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frEmployeeDetails

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Employee Department

/* Definitions for FRAME frEmployeeDetails                              */
&Scoped-define FIELDS-IN-QUERY-frEmployeeDetails Employee.EmpNum ~
Employee.WorkPhone Employee.FirstName Employee.Position Employee.LastName ~
Employee.PostalCode Employee.Address Employee.SickDaysLeft ~
Employee.Address2 Employee.StartDate Employee.Birthdate Employee.State ~
Employee.City Employee.VacationDaysLeft Employee.HomePhone 
&Scoped-define ENABLED-FIELDS-IN-QUERY-frEmployeeDetails Employee.EmpNum 
&Scoped-define ENABLED-TABLES-IN-QUERY-frEmployeeDetails Employee
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-frEmployeeDetails Employee
&Scoped-define QUERY-STRING-frEmployeeDetails FOR EACH Employee SHARE-LOCK, ~
      EACH Department OF Employee SHARE-LOCK
&Scoped-define OPEN-QUERY-frEmployeeDetails OPEN QUERY frEmployeeDetails FOR EACH Employee SHARE-LOCK, ~
      EACH Department OF Employee SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-frEmployeeDetails Employee Department
&Scoped-define FIRST-TABLE-IN-QUERY-frEmployeeDetails Employee
&Scoped-define SECOND-TABLE-IN-QUERY-frEmployeeDetails Department


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Employee.EmpNum 
&Scoped-define ENABLED-TABLES Employee
&Scoped-define FIRST-ENABLED-TABLE Employee
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-5 buAdd buClose 
&Scoped-Define DISPLAYED-FIELDS Employee.EmpNum Employee.WorkPhone ~
Employee.FirstName Employee.Position Employee.LastName Employee.PostalCode ~
Employee.Address Employee.SickDaysLeft Employee.Address2 Employee.StartDate ~
Employee.Birthdate Employee.State Employee.City Employee.VacationDaysLeft ~
Employee.HomePhone 
&Scoped-define DISPLAYED-TABLES Employee
&Scoped-define FIRST-DISPLAYED-TABLE Employee


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON buAdd 
     LABEL "Add" 
     SIZE 15 BY 1.14
     BGCOLOR 1 FGCOLOR 3 .

DEFINE BUTTON buCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 1 .

DEFINE BUTTON buClose 
     LABEL "Close" 
     SIZE 15 BY 1.14
     BGCOLOR 1 .

DEFINE BUTTON buDelete 
     LABEL "Delete" 
     SIZE 15 BY 1.14
     BGCOLOR 1 .

DEFINE BUTTON buSave 
     LABEL "Save" 
     SIZE 15 BY 1.14
     BGCOLOR 1 .

DEFINE BUTTON buUpdate 
     LABEL "Update" 
     SIZE 15 BY 1.14
     BGCOLOR 1 .

DEFINE VARIABLE cbDeptCode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dept Code" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    ROUNDED 
     SIZE 90 BY 14.05
     FGCOLOR 0 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    ROUNDED 
     SIZE 90 BY 4.52
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 1.43
     BGCOLOR 3 FGCOLOR 3 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY frEmployeeDetails FOR 
      Employee, 
      Department SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frEmployeeDetails
     Employee.EmpNum AT ROW 4 COL 20.2 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          FGCOLOR 0 
     Employee.WorkPhone AT ROW 5.05 COL 66.2 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Employee.FirstName AT ROW 5.62 COL 17 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Employee.Position AT ROW 6.33 COL 66.2 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Employee.LastName AT ROW 7.1 COL 17 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     Employee.PostalCode AT ROW 7.62 COL 66.2 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Employee.Address AT ROW 8.52 COL 17 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 29.6 BY 1
     Employee.SickDaysLeft AT ROW 9.05 COL 66.2 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     Employee.Address2 AT ROW 9.91 COL 17 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
     Employee.StartDate AT ROW 10.52 COL 66.2 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Employee.Birthdate AT ROW 11.29 COL 17 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Employee.State AT ROW 11.95 COL 66.2 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Employee.City AT ROW 12.71 COL 17 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     Employee.VacationDaysLeft AT ROW 13.33 COL 66.2 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     cbDeptCode AT ROW 14.1 COL 17 COLON-ALIGNED WIDGET-ID 64
     Employee.HomePhone AT ROW 14.71 COL 66.2 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     buAdd AT ROW 17.14 COL 8 WIDGET-ID 34
     buDelete AT ROW 17.14 COL 38.4 WIDGET-ID 36
     buUpdate AT ROW 17.24 COL 66 WIDGET-ID 38
     buSave AT ROW 19.1 COL 66 WIDGET-ID 40
     buCancel AT ROW 19.14 COL 8 WIDGET-ID 48
     buClose AT ROW 19.14 COL 38.4 WIDGET-ID 50
     "       Employee Details" VIEW-AS TEXT
          SIZE 26 BY 1.05 AT ROW 1.29 COL 32 WIDGET-ID 44
          BGCOLOR 3 FGCOLOR 0 
     RECT-2 AT ROW 1.95 COL 2 WIDGET-ID 46
     RECT-3 AT ROW 16.52 COL 2 WIDGET-ID 52
     RECT-5 AT ROW 1.1 COL 31 WIDGET-ID 58
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.4 BY 20.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


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
         TITLE              = "Employee Management System"
         HEIGHT             = 20.67
         WIDTH              = 93
         MAX-HEIGHT         = 21.33
         MAX-WIDTH          = 116.2
         VIRTUAL-HEIGHT     = 21.33
         VIRTUAL-WIDTH      = 116.2
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("adeicon/admin%.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/admin%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frEmployeeDetails
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Employee.Address IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.Address2 IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.Birthdate IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON buCancel IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON buDelete IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON buSave IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON buUpdate IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbDeptCode IN FRAME frEmployeeDetails
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cbDeptCode:HIDDEN IN FRAME frEmployeeDetails           = TRUE.

/* SETTINGS FOR FILL-IN Employee.City IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.FirstName IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.HomePhone IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.LastName IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.Position IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.PostalCode IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.SickDaysLeft IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.StartDate IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.State IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.VacationDaysLeft IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Employee.WorkPhone IN FRAME frEmployeeDetails
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frEmployeeDetails
/* Query rebuild information for FRAME frEmployeeDetails
     _TblList          = "sports2000.Employee,sports2000.Department OF sports2000.Employee"
     _Query            is NOT OPENED
*/  /* FRAME frEmployeeDetails */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Employee Management System */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
DO:
    /* This case occurs when the user presses the "Esc" key.
       In a persistently run window, just ignore this.  If we did not, the
       application would exit. */
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Employee Management System */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buAdd C-Win
ON CHOOSE OF buAdd IN FRAME frEmployeeDetails /* Add */
DO: 
       
    FIND LAST Employee  NO-ERROR.
    iNumber = ( IF AVAILABLE Employee
        THEN Employee.EmpNum
        ELSE 0 ) + 1.

    DO WITH FRAME {&FRAME-NAME}:
        
        RUN getDeptCodes.
      
        ENABLE buSave buCancel.
        DISABLE buUpdate buDelete.
    
        ENABLE {&FIELDS-IN-QUERY-{&FRAME-NAME}}.
        DISPLAY iNumber @ Employee.empnum.
        DISABLE Employee.empnum.
/*        HIDE Employee.DeptCode.*/
        ENABLE cbDeptCode.
        DISPLAY cbDeptCode.
              
        DISPLAY '' @ Employee.FirstName 
                '' @ Employee.LastName 
                '' @ Employee.Address
                '' @ Employee.Address2                            
                '' @ Employee.Birthdate          
                '' @ Employee.City               
                //'' @ Employee.DeptCode           
                '' @ Employee.FirstName          
                '' @ Employee.LastName           
                '' @ Employee.HomePhone          
                '' @ Employee.Position           
                '' @ Employee.PostalCode         
                '' @ Employee.SickDaysLeft                      
                '' @ Employee.StartDate          
                '' @ Employee.State              
                '' @ Employee.VacationDaysLeft   
                '' @ Employee.WorkPhone.          
        
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buCancel C-Win
ON CHOOSE OF buCancel IN FRAME frEmployeeDetails /* Cancel */
DO :
    RUN ReturnToHomeScreen.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buClose C-Win
ON CHOOSE OF buClose IN FRAME frEmployeeDetails /* Close */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buDelete C-Win
ON CHOOSE OF buDelete IN FRAME frEmployeeDetails /* Delete */
DO:
    DEFINE VARIABLE lConfirmed      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iEmployeeNum    AS INTEGER NO-UNDO.
       
    IF INTEGER (Employee.EmpNum:SCREEN-VALUE) <> 0 THEN 
    DO:
        FIND Employee WHERE Employee.EmpNum = INTEGER( Employee.EmpNum:SCREEN-VALUE )
                      NO-ERROR.
        IF AVAILABLE Employee THEN 
        DO: 
            ASSIGN iEmployeeNum =   Employee.EmpNum.         
            MESSAGE "Are you sure you want to delete " Employee.EmpNum  
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lConfirmed.
            
            IF lConfirmed THEN DO:
                DELETE Employee.
                MESSAGE "succesfully deleted the employee number"  iEmployeeNum
                VIEW-AS ALERT-BOX.
                RUN ReturnToHomeScreen. 
                DISABLE Employee.EmpNum.               
                                
            END.
              
        END. 
        ELSE 
        DO:
            MESSAGE "No records available with Employee number you entered"
            VIEW-AS ALERT-BOX.
            
        END.  
    END.
    ELSE
    MESSAGE "Enter an employee number to delete"
    VIEW-AS ALERT-BOX.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buSave C-Win
ON CHOOSE OF buSave IN FRAME frEmployeeDetails /* Save */
DO:
        
    IF INTEGER (Employee.EmpNum:SCREEN-VALUE) <> 0 THEN
    DO:
        RUN ValidateInput.
        

        IF lValid = TRUE THEN DO :
            
            FIND Employee WHERE Employee.EmpNum = INTEGER( Employee.EmpNum:SCREEN-VALUE )
                        NO-ERROR.
            IF AVAILABLE Employee THEN
            DO:                
                ASSIGN  Employee.Address            
                        Employee.Address2          
                        Employee.Birthdate          
                        Employee.City                              
                        Employee.FirstName          
                        Employee.LastName           
                        Employee.Position           
                        Employee.PostalCode         
                        Employee.StartDate          
                        Employee.SickDaysLeft       
                        Employee.HomePhone          
                        Employee.WorkPhone          
                        Employee.State              
                        Employee.VacationDaysLeft   
                        Employee.DeptCode           = cbDeptCode.
        
                MESSAGE "Modified the employee number " Employee.EmpNum
                VIEW-AS ALERT-BOX.
                DO WITH FRAME {&FRAME-NAME}:
                    DISPLAY {&FIELDS-IN-QUERY-{&FRAME-NAME}}.
                    DISPLAY cbDeptCode.
                    //HIDE Employee.DeptCode.
                    DISABLE buSave
                        Employee.EmpNum      
                        Employee.Address            
                        Employee.Address2           
                        Employee.Birthdate          
                        Employee.City 
                        Employee.FirstName         
                        Employee.LastName           
                        Employee.Position          
                        Employee.PostalCode         
                        Employee.StartDate          
                        Employee.SickDaysLeft       
                        Employee.HomePhone          
                        Employee.WorkPhone          
                        Employee.State              
                        Employee.VacationDaysLeft                          
                        cbDeptCode.
                    ENABLE buAdd buDelete buUpdate.
                        
                END.
                //RUN ReturnToHomeScreen.
             
            END.
            ELSE 
            DO :
                CREATE  Employee.
                ASSIGN  Employee.EmpNum             
                        Employee.Address            
                        Employee.Address2           
                        Employee.Birthdate          
                        Employee.City 
                        Employee.FirstName         
                        Employee.LastName           
                        Employee.Position          
                        Employee.PostalCode         
                        Employee.StartDate          
                        Employee.SickDaysLeft       
                        Employee.HomePhone          
                        Employee.WorkPhone          
                        Employee.State              
                        Employee.VacationDaysLeft                          
                        Employee.DeptCode           = cbDeptCode.
                
                MESSAGE "Created new employee with employee number" Employee.EmpNum
                VIEW-AS ALERT-BOX.
                DO WITH FRAME {&FRAME-NAME}:
                    DISPLAY {&FIELDS-IN-QUERY-{&FRAME-NAME}}.
                    DISPLAY cbDeptCode.
                    //HIDE Employee.DeptCode.
                    DISABLE buSave
                            Employee.EmpNum      
                            Employee.Address            
                            Employee.Address2           
                            Employee.Birthdate          
                            Employee.City 
                            Employee.FirstName         
                            Employee.LastName           
                            Employee.Position          
                            Employee.PostalCode         
                            Employee.StartDate          
                            Employee.SickDaysLeft       
                            Employee.HomePhone          
                            Employee.WorkPhone          
                            Employee.State              
                            Employee.VacationDaysLeft                          
                            cbDeptCode.
                    ENABLE buAdd buDelete buUpdate.
                        
                END.
                   
            END. 
        END.
        

   END. 
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buUpdate C-Win
ON CHOOSE OF buUpdate IN FRAME frEmployeeDetails /* Update */
DO:
    IF INTEGER (Employee.EmpNum:SCREEN-VALUE) <> 0 THEN 
    DO WITH FRAME {&FRAME-NAME}:
        RUN getDeptCodes.
        ENABLE buSave buCancel.
                
        FIND Employee NO-LOCK
                    WHERE Employee.EmpNum = INTEGER( Employee.EmpNum:SCREEN-VALUE )
                    NO-ERROR.
        IF AVAILABLE Employee THEN 
        DO:            
            DISPLAY {&FIELDS-IN-QUERY-{&FRAME-NAME}} WITH FRAME {&FRAME-NAME}.
                                   
        END.  
        
        ENABLE {&FIELDS-IN-QUERY-{&FRAME-NAME}}.           
        DISABLE Employee.EmpNum buDelete BuAdd buUpdate.
/*        HIDE Employee.DeptCode.*/
        ENABLE cbDeptCode.
        ASSIGN cbDeptCode = Employee.DeptCode . 
        DISPLAY cbDeptCode.
  
    END.
    ELSE 
    MESSAGE "Enter an employee number to modify"
    VIEW-AS ALERT-BOX.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDeptCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDeptCode C-Win
ON VALUE-CHANGED OF cbDeptCode IN FRAME frEmployeeDetails /* Dept Code */
DO:
   ASSIGN cbDeptCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Employee.EmpNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Employee.EmpNum C-Win
ON RETURN OF Employee.EmpNum IN FRAME frEmployeeDetails /* Emp No */
DO:
    FIND Employee NO-LOCK
                    WHERE Employee.EmpNum = INTEGER( Employee.EmpNum:SCREEN-VALUE )
                    NO-ERROR.
    IF AVAILABLE Employee THEN 
    DO WITH FRAME {&FRAME-NAME}:
        
        DISPLAY {&FIELDS-IN-QUERY-{&FRAME-NAME}}.
        RUN GetDeptCodes.
/*        HIDE Employee.DeptCode.*/
        ASSIGN   cbDeptCode = Employee.DeptCode.
        DISPLAY  cbDeptCode. 
        ENABLE buDelete buUpdate buCancel.
            
    END.  
    ELSE 
    MESSAGE "No record is available for Employee number" Employee.EmpNum:SCREEN-VALUE
    VIEW-AS ALERT-BOX.
  
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
  IF AVAILABLE Employee THEN 
    DISPLAY Employee.EmpNum Employee.WorkPhone Employee.FirstName 
          Employee.Position Employee.LastName Employee.PostalCode 
          Employee.Address Employee.SickDaysLeft Employee.Address2 
          Employee.StartDate Employee.Birthdate Employee.State Employee.City 
          Employee.VacationDaysLeft Employee.HomePhone 
      WITH FRAME frEmployeeDetails IN WINDOW C-Win.
  ENABLE RECT-2 RECT-3 RECT-5 Employee.EmpNum buAdd buClose 
      WITH FRAME frEmployeeDetails IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frEmployeeDetails}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDeptCodes C-Win 
PROCEDURE getDeptCodes :
/*------------------------------------------------------------------------------
 Purpose: To get available department codes in employee table and assign
          to combo box items list
 Notes:
------------------------------------------------------------------------------*/
/*    DEFINE VARIABLE cDeptCodes AS CHARACTER  NO-UNDO.*/
    
        
    DO WITH FRAME {&FRAME-NAME}: END.
    
    cbDeptCode:LIST-ITEM-PAIRS = ','.
    
    FOR EACH Department NO-LOCK BREAK BY Department.DeptCode:
        
        IF FIRST-OF (Department.DeptCode) THEN 
        DO:  
             
            cbDeptCode:ADD-LAST (Department.DeptName, Department.DeptCode).
 
        END. 
        
    END.
    cbDeptCode:DELETE( 1 ).
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturnToHomeScreen C-Win 
PROCEDURE ReturnToHomeScreen :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    
    ENABLE Employee.EmpNum.
    HIDE cbDeptCode.
    DISABLE BuDelete buUpdate buSave.

    DISPLAY ''@ Employee.EmpNum
            ''@ Employee.FirstName
            ''@ Employee.LastName
            ''@ Employee.Address
            ''@ Employee.Address2
            ''@ Employee.Birthdate
            ''@ Employee.City
/*            ''@ Employee.DeptCode*/
            ''@ Employee.FirstName
            ''@ Employee.LastName
            ''@ Employee.HomePhone
            ''@ Employee.Position
            ''@ Employee.PostalCode
            ''@ Employee.SickDaysLeft
            ''@ Employee.StartDate
            ''@ Employee.State
            ''@ Employee.VacationDaysLeft
            ''@ Employee.WorkPhone.
            
    DISABLE Employee.FirstName
            Employee.LastName
            Employee.Address
            Employee.Address2
            Employee.Birthdate
            Employee.City
/*            Employee.DeptCode*/
            Employee.FirstName
            Employee.LastName
            Employee.HomePhone
            Employee.Position
            Employee.PostalCode
            Employee.SickDaysLeft
            Employee.StartDate
            Employee.State
            Employee.VacationDaysLeft
            Employee.WorkPhone
            cbDeptCode.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateInput C-Win 
PROCEDURE ValidateInput :
/*------------------------------------------------------------------------------
 Purpose: validate user input for update and add employee fucntions
 Notes:Only include validation to check null for mandatory fields and dates
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
        
    IF Employee.FirstName:SCREEN-VALUE = '' THEN 
    DO:
        MESSAGE "Please enter the first name of employee"
        VIEW-AS ALERT-BOX.
        lValid = FALSE.
        RETURN. 
        
    END.
    ELSE    
    IF Employee.Address:SCREEN-VALUE = '' THEN
    DO:
        MESSAGE "Please enter the address"
        VIEW-AS ALERT-BOX.
        lValid = FALSE. 
        RETURN.
                        
    END. 
    ELSE 
    IF Employee.Birthdate:SCREEN-VALUE = '' THEN 
    DO:
        MESSAGE "please enter the birthdate"
        VIEW-AS ALERT-BOX.
        lValid = FALSE. 
        RETURN.
      
    END.
    ELSE IF DATE(Employee.Birthdate:SCREEN-VALUE) >= TODAY THEN 
    DO:
        MESSAGE "Invalid birthdate "
        VIEW-AS ALERT-BOX.
        lValid = FALSE.
        RETURN. 
        
    END.
    ELSE IF INT(YEAR(Employee.Startdate) - YEAR(DATE(Employee.Birthdate:SCREEN-VALUE) )) < 18 THEN 
    DO:       
        MESSAGE "Age of Employee should be greater than or equal 18"
        VIEW-AS ALERT-BOX.
        lValid = FALSE.
        RETURN. 
        
    END.
    ELSE         
    IF Employee.City:SCREEN-VALUE = '' THEN  
    DO:
         MESSAGE "please Enter the city"
        VIEW-AS ALERT-BOX.
        lValid = FALSE. 
        RETURN.
        
    END.  
    
    ELSE         
    IF cbDeptCode = '' THEN 
    DO:
        MESSAGE "please enter the department code"
        VIEW-AS ALERT-BOX.
        lValid = FALSE. 
        RETURN.
        
    END.
    ELSE            
    IF Employee.LastName:SCREEN-VALUE = '' THEN 
    DO:
        MESSAGE "Please enter the last name of employee"
            VIEW-AS ALERT-BOX.
    
        lValid = FALSE.
        RETURN. 
    END.
    ELSE                   
    IF Employee.PostalCode:SCREEN-VALUE = '' THEN 
    DO:
        lValid = FALSE. 
        MESSAGE "Please enter the postal code"
        VIEW-AS ALERT-BOX.
        RETURN.
    END.
    ELSE 
    IF Employee.StartDate:SCREEN-VALUE = '' THEN 
    DO:
        lValid = FALSE. 
        MESSAGE  "Please enter the start date of employee"
        VIEW-AS ALERT-BOX.
        RETURN.
            
    END.
    ELSE IF DATE (Employee.StartDate:SCREEN-VALUE) >= TODAY THEN 
    DO:
        lValid = FALSE. 
        MESSAGE  "Invalid start date"
        VIEW-AS ALERT-BOX.
        RETURN.
        
    END.
    ELSE
    lValid = TRUE . 
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


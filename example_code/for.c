
/* MAIN MODULE */

#define MAIN_MENU_INDEX_C ( sizeof MainMenu / sizeof MainMenu[0] )
#define MENU_B_INDEX_C    ( sizeof MenuB / sizeof MenuB[0] )
#define MENU_C_INDEX_C    ( sizeof MenuC / sizeof MenuC[0] )
#define MENU_E_INDEX_C    ( sizeof MenuE / sizeof MenuE[0] )
#define MENU_L_INDEX_C    ( sizeof MenuL / sizeof MenuL[0] )
#define MENU_M_INDEX_C    ( sizeof MenuM / sizeof MenuM[0] )
#define MENU_N_INDEX_C    ( sizeof MenuN / sizeof MenuN[0] )
#define MENU_O_INDEX_C    ( sizeof MenuO / sizeof MenuO[0] )
#define MENU_P_INDEX_C    ( sizeof MenuP / sizeof MenuP[0] )
#define MENU_R_INDEX_C    ( sizeof MenuR / sizeof MenuR[0] )
#define MENU_S_INDEX_C    ( sizeof MenuS / sizeof MenuS[0] )
#define MENU_T_INDEX_C    ( sizeof MenuT / sizeof MenuT[0] )

#define RET_C                   (0xD)                /* return   */

#include "e2khanqx.cin"

int argc;
char ** argv;
const struct main_hlp__t {
    const char  text[43];
} MainMenu[] = {
    { "\n\n\n\n\n\tBRM MONITORING           \n\n\r" },
    { "\t ? ..... HELP / MENU                 \n\r" },
    { "\t B ..... BTS SPECIFIC INFO           \n\r" },
    { "\t C ..... CELL STATUS FILE            \n\r" },
/*    { "\t DB..... E2K DEBUG-MODE OPTIONS  \n\r\n\r" },   */
/*    { "\t E ..... Eb/No Table                 \n\r" },   */
/*    { "\t L ..... BRMPRB TEST LOG INTERFACE   \n\r" },   */
    { "\t M ..... MS STATUS FILE              \n\r" },
    { "\t N ..... NRT QUEUEING INFO           \n\r" },
    { "\t O ..... ONLINE MONITORING           \n\r" },
    { "\t P ..... RNC PARAMETERS              \n\r" },
/*    { "\t R ..... RT QUEUEING INFO            \n\r" },   */
/*    { "\t S ..... SEND RNP PARAMETERS TO BRM  \n\r" }    */
    { "\t T ..... CODE TREE                   \n\r" },
    { "                                     \t\n\r" },    
    { "\t Z ..... RETURN TO MAIN LEVEL        \n\r" },
    { "                                     \t\n\r" }
};


/*   B ..... BTS Specific data */

const struct B_hlp__t {
    const char  text[80];
} MenuB[] = {
          { "                                                                          \n\n\r" },
          { " B(C)(D):BTS_ID,OUTPUT_TYPE;                                                \n\r" },
          { "   !  !  !      !                                                           \n\r" },
          { "   !  !  !      0 = All menu commands data (default)                        \n\r" },
          { "   !  !  !      1 = BTS CELL ID LIST                                        \n\r" },
          { "   !  !  !      2 = GENERAL INFO OF BTS                                     \n\r" },
          { "   !  !  !      3 = BTS SPECIFIC PARAMETERS                                 \n\r" },
          { "   !  !  !      4 = BTS CAPABILITY INFORMATION                              \n\r" },
          { "   !  !  !      5 = BTS SPECIFIC OVERLOAD CONTROL CANDIDATE LISTS (EOCB)    \n\r" },
          { "   !  !  !      6 = COMMON MEAS OVER IUR INFO                               \n\r" },
          { "   !  !  !      7 = LOCAL CELL GROUP INFORMATION                            \n\r" },
          { "   !  !  !      8 = BTS PENALTY TIMER (PIT) LISTS                           \n\r" },
          { "   !  !  !      9 = BTS HSPA RELATED INFORMATION                            \n\r" },
          { "   !  !  !     10 = TEST PARAMETERS                                         \n\r" },
          { "   !  !  !     11 = BTS RELATED PROCESS PID INFORMATION                     \n\r" },
          { "   !  !  !                                                                  \n\r" },
          { "   !  !  Selected BTS id (HEX/DEC)                                          \n\r" },
          { "   !  !                                                                     \n\r" },
          { "   !  D = Give BTS_ID parameter as DEC                                      \n\r" },
          { "   !                                                                        \n\r" },
          { "   C = Continuous output, interrupted by Ctrl-A or Ctrl-C               \n\r\n\r" }
};


/*   C ..... CELL STATUS FILE HELP TEXT */

const struct C_hlp__t {
    const char  text[80];
} MenuC[] = {
          { "                                                                          \n\n\r" },
          { " C(C)(D):BTS_ID,CELL_ID,OUTPUT_TYPE;                                        \n\r" },
          { "   !  !  !      !       !                                                   \n\r" },
          { "   !  !  !      !       0 = All menu commands data (default)                \n\r" },
          { "   !  !  !      !       1 = GENERAL INFO OF CELL                            \n\r" },
          { "   !  !  !      !       2 = REALTIME LOAD INFORMATION                       \n\r" },
          { "   !  !  !      !       3 = RADIO NETWORK PLANNING PARAMETERS               \n\r" },
          { "   !  !  !      !       4 = LIST OF HIGH BIT RATE BEARERS                   \n\r" },
          { "   !  !  !      !       5 = DECREASED BITRATE LIST                          \n\r" },
          { "   !  !  !      !       6 = SCCPCH AND PCH IDs AND PARAMETERS               \n\r" },
          { "   !  !  !      !       7 = PRACH ID AND PARAMETERS                         \n\r" },
          { "   !  !  !      !       8 = EXTENDED REALTIME LOAD INFORMATION OF CELL      \n\r" },
          { "   !  !  !      !       9 = CELL MEASUREMENT INFO                           \n\r" },
          { "   !  !  !      !      10 = OVERLOAD CONTROL PRIORITY LISTS                 \n\r" },
          { "   !  !  !      !      11 = OVERLOAD CONTROL CANDIDATE LISTS                \n\r" },
          { "   !  !  !      !      12 = PS REJECTION RATE INFORMATION                   \n\r" },
          { "   !  !  !      !      13 = SLHO CELL INFORMATION                           \n\r" },
          { "   !  !  !      !      14 = SLHO CANDIDATE LISTS                            \n\r" },
          { "   !  !  !      !      15 = SLHO INTERFERENCE LOAD INFO                     \n\r" },
          { "   !  !  !      !      16 = SLHO PS NRT CAPA REQ REJECTION RATE LOAD INFO   \n\r" },
          { "   !  !  !      !      17 = SLHO DL SC RESERVATION RATE LOAD INFO           \n\r" },
          { "   !  !  !      !      18 = SLHO HARD BLOCKING LOAD INFO                    \n\r" },
          { "   !  !  !      !      19 = HSPA INFORMATION OF THE CELL                    \n\r" },
          { "   !  !  !      !      20 = PRE-EMPTION CANDIDATE LIST                      \n\r" },
          { "   !  !  !      !      21 = CELL CURRENT CAPABILITY                         \n\r" },
          { "   !  !  !      !      22 = DYNAMIC POWER ALLOCATION                        \n\r" },   /* RN30_DPOW */
          { "   !  !  !      !      23 = DYNAMIC POWER ALLOCATION STATISTICS             \n\r" },   /* RN30_DPOW */
          { "   !  !  !      !      24 = LOAD BASED AMR SF128 LOAD AVE WINDOW            \n\r" },   /* RN40_LBAMR */
          { "   !  !  !      !      25 = CELL SPECIFIC ICR DATA                          \n\r" },   /* ICR */
          { "   !  !  !      !                                                           \n\r" },
          { "   !  !  !      Selected CELL ID (HEX/DEC) / 0 = all / W = working          \n\r" },
          { "   !  !  !                                                                  \n\r" },
          { "   !  !  Selected BTS ID (HEX/DEC)                                          \n\r" },
          { "   !  !                                                                     \n\r" },
          { "   !  D = Give BTS ID and CELL ID parameters as DEC                         \n\r" },
          { "   !                                                                        \n\r" },
          { "   C = Continuous output, interrupted by Ctrl-A or Ctrl-C               \n\r\n\r" }
};


/*   E ..... Eb/No OUTPUT  */

const struct E_hlp__t {
    const char  text[80];
} MenuE[] = {
          { "                                                                          \n\n\r" },
          { " Output Eb/No values for active MS:                                         \n\r" },
          { " EA(C)(D):BTS_ID,MS_ID;                                                     \n\r" },
          { "    !  !  !      !                                                          \n\r" },
          { "    !  !  !      0   = All active MS records (default)                      \n\r" },
          { "    !  !  !      DEC = Selected MS ID                                       \n\r" },
          { "    !  !  Selected BTS ID (HEX/DEC)                                         \n\r" },
          { "    !  D = Give BTS ID parameter as DEC                                     \n\r" },
          { "    C = Continuous output, interrupted by Ctrl-A or Ctrl-C                  \n\r" },
          { "                                                                            \n\r" },
          { " Output Eb/No RNP table:                                                    \n\r" },
          { " ET(C)(D):BTS_ID,DCH_TYPE,Eb/No_SET_ID;                                     \n\r" },
          { "    !  !  !      !        !                                                 \n\r" },
          { "    !  !  !      !        Eb/No Set Identifier(0 to 15)                     \n\r" },
          { "    !  !  !      1 = Signalling Link                                        \n\r" },
          { "    !  !  !      2 = AMR speech, sub flow type A                            \n\r" },
          { "    !  !  !      3 = AMR speech, sub flow type B                            \n\r" },
          { "    !  !  !      4 = AMR speech, sub flow type B                            \n\r" },
          { "    !  !  !      5 = data services(CS domain and PS domain)                 \n\r" },
          { "    !  !  Selected BTS ID (HEX/DEC)                                         \n\r" },
          { "    !  D = Give BTS ID parameter as DEC                                     \n\r" },
          { "    C = Continuous output, interrupted by Ctrl-A or Ctrl-C              \n\r\n\r" }
};


/*   L ..... BRMPRB TEST LOG INTERFACE      */

const struct L_hlp__t {
    const char  text[80];
} MenuL[] = {
          { "                                                                          \n\n\r" },
          { "  L(D):BTS_ID,MAIN_GROUP,SUB_GROUP,LEVEL;   Modify log output parameters    \n\r" },
          { "    !   !      !         !         !                                        \n\r" },
          { "    !   !      !         !         Selected log output level                \n\r" },
          { "    !   !      !         !         0 = No output (default)                  \n\r" },
          { "    !   !      !         !         1 = Most limited output level            \n\r" },
          { "    !   !      !         !         2..4 = More extensive output level       \n\r" },
          { "    !   !      !         !         5 = Most extensive output level          \n\r" },
          { "    !   !      !         !                                                  \n\r" },
          { "    !   !      !       ? = List of subgroups of the selected maingroup(s)   \n\r" },
          { "    !   !      !       0 = All subgroups of the selected maingroup (default)\n\r" },
          { "    !   !      !       1...n = Selected subgroup (hex)                      \n\r" },
          { "    !   !      !                                                            \n\r" },
          { "    !   !      ? = List of maingroups                                       \n\r" },
          { "    !   !      0 = All maingroups (default)                                 \n\r" },
          { "    !   !      1...n = Selected maingroup (hex)                             \n\r" },
          { "    !   !                                                                   \n\r" },
          { " LS(D):BTS_ID,TIME;    Start log output with specified timelimit            \n\r" },
          { "    !   !      !                                                            \n\r" },
          { "    !   !      0..3600 = Log output time in sec                             \n\r" },
          { "    !   !      default = 60 sec                                             \n\r" },
          { "    !   !                                                                   \n\r" },
          { " LI(D):BTS_ID; Interrogate log output status by BTS id (1..n hex)           \n\r" }, 
          { "    !                                                                       \n\r" },  
          { "    D = Give BTS_ID parameter as DEC                                    \n\r\n\r" }      
};


/*   M ..... MS STATUS FILE HELP TEXT   */

const struct M_hlp__t {
    const char  text[80];
} MenuM[] = {
          { "                                                                          \n\n\r" },
          { " M(C)(D):BTS_ID,OUTPUT_TYPE,MS_ID,CELL_ID,CURR_DATA;                        \n\r" },
          { "   !  !  !      !           !     !       !                                 \n\r" },
          { "   !  !  !      !           !     !       0 = Output active calls (Default) \n\r" },
          { "   !  !  !      !           !     !       1 = Output all current data       \n\r" },
          { "   !  !  !      !           !     !                                         \n\r" },
          { "   !  !  !      !           !     0   = All CELLs (default)                 \n\r" },
          { "   !  !  !      !           !     HEX/DEC = Selected CELL_ID                \n\r" },
          { "   !  !  !      !           !    (Parameter affects only with output_type 8)\n\r" },
          { "   !  !  !      !           !                                               \n\r" },
          { "   !  !  !      !           0   = All active MS records (default)           \n\r" },
          { "   !  !  !      !           DEC = Selected MS_ID                            \n\r" },
          { "   !  !  !      !                                                           \n\r" },
          { "   !  !  !      0 = All menu commands data                                  \n\r" },
          { "   !  !  !      1 = GENERAL MS INFORMATION LIST OF CURRENT MS DATA (default)\n\r" },
          { "   !  !  !      2 = GENERAL MS INFORMATION LIST OF NEW MS DATA              \n\r" },
          { "   !  !  !      3 = DETAILED CURRENT MS SPECIFIC DATA                       \n\r" },
          { "   !  !  !      4 = DETAILED NEW MS SPECIFIC DATA                           \n\r" },
          { "   !  !  !      5 = COMPRESSED MODE RELATED DATA                            \n\r" },
          { "   !  !  !      6 = GENERAL DCH INFORMATION LIST OF CURRENT MS DATA         \n\r" },
          { "   !  !  !      7 = GENERAL DCH INFORMATION LIST OF NEW MS DATA             \n\r" },
          { "   !  !  !      8 = MS RL INFORMATION LISTED BY CELLS (CURRENT MS DATA)     \n\r" },
          { "   !  !  !      9 = MS RL MEASUREMENT INFORMATION                           \n\r" },
          { "   !  !  !     10 = MS SLHO INFORMATION                                     \n\r" },
          { "   !  !  !     11 = HSPA CHANNEL INFORMATION LIST OF CURRENT MS DATA        \n\r" },   /* RN30_HSDPA_RIS3_JT */
          { "   !  !  !     12 = HSPA CHANNEL INFORMATION LIST OF NEW MS DATA            \n\r" },   /* RN30_HSDPA_RIS3_JT */
          { "   !  !  !                                                                  \n\r" },
          { "   !  !  Selected BTS id (HEX/DEC)                                          \n\r" },
          { "   !  !                                                                     \n\r" },
          { "   !  D = Give BTS ID and CELL ID parameters as DEC                         \n\r" },
          { "   !                                                                        \n\r" },
          { "   C = Continuous output, interrupted by Ctrl-A or Ctrl-C               \n\r\n\r" }
};


/*   N ..... NRT QUEUE INFORMATION      */

const struct N_hlp__t {
    const char  text[80];
} MenuN[] = {
          { "                                                                          \n\n\r" },
          { " N(C)(D):BTS_ID,CELL_ID,OUTPUT_TYPE,MS_ID;                                  \n\r" },
          { "   !  !  !      !       !           !                                       \n\r" },
          { "   !  !  !      !       !           0   = All (default)                     \n\r" },
          { "   !  !  !      !       !           DEC = Selected MS_ID                    \n\r" },
          { "   !  !  !      !       !                                                   \n\r" },
          { "   !  !  !      !       0 = Output all queues (default)                     \n\r" },
          { "   !  !  !      !       1 = CELL NRT ORDER QUEUE                            \n\r" },
          { "   !  !  !      !       2 = BTS NRT CAPACITY REQUEST TABLE                  \n\r" },
          { "   !  !  !      !       3 = BTS NRT CAPACITY REQUEST LIST AND ORDER QUEUES  \n\r" },
          { "   !  !  !      !       4 = BTS NRT CAPACITY REQUEST LIST (active records)  \n\r" },
          { "   !  !  !      !                                                           \n\r" },
          { "   !  !  !      Selected CELL ID (HEX/DEC) / 0 = all / W = working          \n\r" },
          { "   !  !  !                                                                  \n\r" },
          { "   !  !  Selected BTS ID (HEX/DEC)                                          \n\r" },
          { "   !  !                                                                     \n\r" },
          { "   !  D = Give BTS ID and CELL ID parameters as DEC                         \n\r" },
          { "   !                                                                        \n\r" },
          { "   C = Continuous output, interrupted by Ctrl-A or Ctrl-C               \n\r\n\r" }
};


/*   O ..... ONLINE MONITORING  */

const struct O_hlp__t {
    const char  text[80];
} MenuO[] = {
          { "                                                                          \n\n\r" },
          { " O(C)(D):BTS_ID,CELL_ID;                                                    \n\r" },
          { "   !  !  !      !                                                           \n\r" },
          { "   !  !  !      Selected CELL ID (HEX/DEC) / 0 = all / W = working          \n\r" },
          { "   !  !  !                                                                  \n\r" },
          { "   !  !  Selected BTS ID (HEX/DEC)                                          \n\r" },
          { "   !  !                                                                     \n\r" },
          { "   !   D = Give BTS ID and CELL ID parameters as DEC                        \n\r" },
          { "   !                                                                        \n\r" },
          { "   C = Continuous output, interrupted by Ctrl-A or Ctrl-C               \n\r\n\r" }
};


/*   P..... RNP PARAMETERS HELP TEXT */

const struct P_hlp__t {
    const char  text[80];
} MenuP[] = {
          { "                                                                          \n\n\r" },
          { " P(C)(D):BTS_ID;                                                            \n\r" },
          { "   !  !  !                                                                  \n\r" },
          { "   !  !  Selected BTS ID (HEX/DEC)                                          \n\r" },
          { "   !  !                                                                     \n\r" },
          { "   !  D = Give BTS ID parameter as DEC                                      \n\r" },
          { "   !                                                                        \n\r" },
          { "   C = Continuous output, interrupted by Ctrl-A or Ctrl-C               \n\r\n\r" }
};


/*   R ..... RT QUEUE INFORMATION       */

const struct R_hlp__t {
    const char  text[80];
} MenuR[] = {
          { "                                                                         \n\n\r" },
          { " R(D):BTS_ID,CELL_ID,OUTPUT_TYPE;                                        \n\r" },
          { "   !  !      !       !                                                   \n\r" },
          { "   !  !      !       1 = Data of all RT Queues order data                \n\r" },
          { "   !  !      !       2 = RT RRC queue info                               \n\r" },
          { "   !  !      !       3 = RAB queue info                                  \n\r" },
          { "   !  !      !       4 = RT SHO queue info                               \n\r" },
          { "   !  !      !       5 = RT non critical HHO queue info                  \n\r" },
          { "   !  !      !       6 = RT critical HHO queue info                      \n\r" },
          { "   !  !      !       7 = Priority queue info                             \n\r" },
          { "   !  !      !       8 = Queue priority table                            \n\r" },
          { "   !  !      !                                                           \n\r" },
          { "   !  !      Selected CELL ID (HEX/DEC) / 0 = all / W = working          \n\r" },
          { "   !  !                                                                  \n\r" },
          { "   !  Selected BTS ID (HEX/DEC)                                          \n\r" },
          { "   !                                                                     \n\r" },
          { "   D = Give BTS ID and CELL ID parameters as DEC                         \n\r" }
};


/*   S ..... SEND RNP PARAMETERS TO BRM HAND        */

const struct S_hlp__t {
    const char  text[80];
} MenuS[] = {
          { "                                                                         \n\n\r" },
          { " Send rnc_reconfigure_s to ALL BRM hands:                                \n\r" },
          { " ----------------------------------------                                \n\r" },
          { " SR(D):BTS_ID;                                                           \n\r" },
          { "    !  !                                                                 \n\r" },
          { "    !  Selected BTS ID (HEX/DEC)                                         \n\r" },
          { "    !                                                                    \n\r" },
          { "    D = Give BTS ID parameters as DEC                                    \n\r" },
          { "                                                                         \n\r" },
          { " Send bts_reconfigure_s to BRM hand:                                     \n\r" },
          { " -----------------------------------                                     \n\r" },
          { " SB(D):BTS_ID;                                                           \n\r" },
          { "    !  !                                                                 \n\r" },
          { "    !  Selected BTS ID (HEX/DEC)                                         \n\r" },
          { "    !                                                                    \n\r" },
          { "    D = Give BTS ID parameters as DEC                                    \n\r" },
          { "                                                                         \n\r" },
          { " Send cell_reconfigure_s to BRM hand:                                    \n\r" },
          { " ------------------------------------                                    \n\r" },
          { " SC(D):BTS_ID,CELL_ID;                                                   \n\r" },
          { "    !  !      !                                                          \n\r" },
          { "    !  !      Selected CELL ID (HEX/DEC) / 0 = all                       \n\r" },
          { "    !  !                                                                 \n\r" },
          { "    !  Selected BTS ID (HEX/DEC)                                         \n\r" },
          { "    !                                                                    \n\r" },
          { "    D = Give BTS ID and CELL ID parameters as DEC                        \n\r" }
};

/*   T ..... CODE TREE HELP TEXT        */

const struct T_hlp__t {
    const char  text[80];
} MenuT[] = {
          { "                                                                          \n\n\r" },
          { " T(C)(D):BTS_ID,CELL_ID,SF,SEARCH_KEY,CODE_ID/MS_ID;                        \n\r" },
          { "   !  !  !      !       !  !          !                                     \n\r" },
          { "   !  !  !      !       !  !          0    = All codes (default)            \n\r" },
          { "   !  !  !      !       !  !          DEC  = Selected CODE_ID/MS_ID         \n\r" },
          { "   !  !  !      !       !  !         (SEARCH_KEY must be '11' or '12')      \n\r" },
          { "   !  !  !      !       !  0 = output all codes (default)                   \n\r" },
          { "   !  !  !      !       !  1 = CODE STATUS: FREE                            \n\r" },
          { "   !  !  !      !       !  2 = CODE STATUS: RESERVED                        \n\r" },
          { "   !  !  !      !       !  3 = CODE STATUS: OCCUPIED FROM UP                \n\r" },
          { "   !  !  !      !       !  4 = CODE STATUS: OCCUPIED FROM DOWN              \n\r" },
          { "   !  !  !      !       !  5 = CODE STATUS: NOT IN USE                      \n\r" },
          { "   !  !  !      !       !  6 = CONNECTION TYPE: RT code                     \n\r" },
          { "   !  !  !      !       !  7 = CONNECTION TYPE: NRT codes                   \n\r" },
          { "   !  !  !      !       !  8 = CONNECTION TYPE: Common codes                \n\r" },
          { "   !  !  !      !       !  9 = CONNECTION TYPE: RT and NRT                  \n\r" },
          { "   !  !  !      !       ! 10 = CONNECTION TYPE: Signaling                   \n\r" },
          { "   !  !  !      !       ! 11 = Search code by MS_ID                         \n\r" },
          { "   !  !  !      !       ! 12 = Search code by CODE_ID                       \n\r" },
          { "   !  !  !      !       Output only codes of selected spreading factor      \n\r" },
          { "   !  !  !      !       0 = ALL SF levels(default),4,8,16,32,64,128,256,512 \n\r" },
          { "   !  !  !      Selected CELL ID (HEX/DEC) / 0 = all / W = working          \n\r" },
          { "   !  !  Selected BTS ID (HEX/DEC)                                          \n\r" },
          { "   !  D = Give BTS ID and CELL ID parameters as DEC                         \n\r" },
          { "   C = Continuous output, interrupted by Ctrl-A or Ctrl-C               \n\r\n\r" }
};


/* DEBUG CODE SECTION START */
#if COMPILE_DEBUG_CODE

/*   DB ..... DEBUG  */
const char hlp_DB01[]="\n\n\r   To change current DEBUG output data:                                \n\r";
const char hlp_DB02[]=      "   DB:DEBUG_PARAMETER;                                                 \n\r";
const char hlp_DB03[]=      "      !                                                                \n\r";
const char hlp_DB04[]=      "      Activate/de-activate selected DEBUG output                       \n\r";
const char hlp_DB05[]=      "      0 = Turn OFF ALL DEBUG outputs                                   \n\r";
const char hlp_DB06[]=      "      1 = OUTPUT OF EMPTY ARRAYS                                       \n\r";
const char hlp_DB07[]=      "      2 = OUTPUT CHECK_REC_STATE-FUNCTION RESULTS                      \n\r";
const char hlp_DB08[]=      "      3 = SHOW VISITED FUNCTIONS                                       \n\r";
const char hlp_DB09[]=      "      4 = SHOW USERS INPUT PARAMETERS                                  \n\r";
const char hlp_DB10[]=      "      5 = OUTPUT BRM PID SELECTION INFORMATION                         \n\r";
const char hlp_DB11[]=      "      6 = OUTPUT GENERAL FUNCTION SPECIFIC DATA                        \n\r";
const char hlp_DB12[]=      "      7 = Turn ON ALL DEBUG outputs                                    \n\r";
const char hlp_DB13[]=      "                                                                       \n\r";
const char hlp_DB14[]=      "    To output current status of DEBUG outputs:                         \n\r";
const char hlp_DB15[]=      "    DB;                                                                \n\r";
const char hlp_DB16[]=      "                                                                       \n\r";
const char hlp_DB17[]=      "   \n\r";

#endif

/* Program global variables */

byte            error;                     /* error variable */
byte            G_continuous_output;       /* when TRUE, user has selected continuous output mode, Reseted after each command */
byte            G_read_input_as_dec;       /* when TRUE, BTS_ID and CELL ID is read as decimal instead of hex */
byte            G_table_all_empty;         /* when outputing only active records of any table, this var is used to determine, if any records are output at all */
word            debug_mode;                /* bitstruct that tells which debug outputs are admitted (if TRUE, then debug information is outputed to screen) */
pid_struct_t    brm_pid;                   /* current BRM hand pid (refreshed in before every given command)*/

msg_cec7__t		  bts_pid_tbl;							 /* BRM PID-table */	
msg_cbc5__t     bts_cell_id_list;          /* contains information list of CELL ID */
e2k_msg_cecb__t e2k_msg_cecb;              /* contains indormation of cell status file */
e2k_cell_data__t  e2k_csf_rec;             /* E2K_CCD_Design:e2k_cell_data replaced with e2k_csf_rec */
byte            in_msg[MSG_BUFFER_SIZE];   /* Receive message buffer,  */
calendar_time_t time_calendar;             /* date and time for output headers */

/*
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
*/

/************************************************************************
 *  FUNCTION: do_help
 ***********************************************************************/
void near do_help()
{
  byte index;

/* MAIN MENU HELP TEXT */

  error=ERR_NO_ERROR_C;

  if (lookat_next_char() == RET_C || lookat_next_char() == ';')
  {
      for ( index = 0; index < MAIN_MENU_INDEX_C ; index++ )
      {
         text_out((byte*)MainMenu[index].text);
      }
  }
  else
  {
      switch (next_char())
      {
        case 'B':

            for ( index = 0; index < MENU_B_INDEX_C ; index++ )
            {
                text_out((byte*)MenuB[index].text);
            }
            break;

  /* DEBUG CODE SECTION START */
  #if COMPILE_DEBUG_CODE
        case 'D':
             if(next_char() == 'B')
             {
              text_out((byte*)hlp_DB01);
              text_out((byte*)hlp_DB02);
              text_out((byte*)hlp_DB03);
              text_out((byte*)hlp_DB04);
              text_out((byte*)hlp_DB05);
              text_out((byte*)hlp_DB06);
              text_out((byte*)hlp_DB07);
              text_out((byte*)hlp_DB08);
              text_out((byte*)hlp_DB09);
              text_out((byte*)hlp_DB10);
              text_out((byte*)hlp_DB11);
              text_out((byte*)hlp_DB12);
              text_out((byte*)hlp_DB13);
              text_out((byte*)hlp_DB14);
              text_out((byte*)hlp_DB15);
              text_out((byte*)hlp_DB16);
              text_out((byte*)hlp_DB17);
             }
            else
              error=ERR_SYNTAX_ERROR_C;

            break;

  #endif

        case 'C':

             for ( index = 0; index < MENU_C_INDEX_C ; index++ )
             {
                 text_out((byte*)MenuC[index].text);
             }
             break;
/*
        case 'E':

             for ( index = 0; index < MENU_E_INDEX_C ; index++ )
             {
                 text_out((byte*)MenuE[index].text);
             }
             break;
*/
        case 'L':

            for ( index = 0; index < MENU_L_INDEX_C ; index++ )
            {
                text_out((byte*)MenuL[index].text);
            }
            break;

        case 'M':

            for ( index = 0; index < MENU_M_INDEX_C ; index++ )
            {
                text_out((byte*)MenuM[index].text);
            }
            break;

        case 'N':

            for ( index = 0; index < MENU_N_INDEX_C ; index++ )
            {
                text_out((byte*)MenuN[index].text);
            }
            break;

        case 'O':

            for ( index = 0; index < MENU_O_INDEX_C ; index++ )
            {
                text_out((byte*)MenuO[index].text);
            }
            break;


        case 'P':

             for ( index = 0; index < MENU_P_INDEX_C ; index++ )
             {
                text_out((byte*)MenuP[index].text);
             }
             break;

        case 'R':

            for ( index = 0; index < MENU_R_INDEX_C ; index++ )
            {
                text_out((byte*)MenuR[index].text);
            }
            break;

        case 'S':

            for ( index = 0; index < MENU_S_INDEX_C ; index++ )
            {
                text_out((byte*)MenuS[index].text);
            }
            break;

        case 'T':

            for ( index = 0; index < MENU_T_INDEX_C ; index++ )
            {
                 text_out((byte*)MenuT[index].text);
            }
            break;

        default:
            error=ERR_SYNTAX_ERROR_C;
            break;
       }
    }
}

/*
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
*/


#if COMPILE_DEBUG_CODE
/***********************************************************************
 *  FUNCTION: do_command_DB    (DEBUG ACTIVATION)
 **********************************************************************/
void near do_command_DB()
{
  word          debug_parameter;                  /*  given db-parameter */
  byte          next_mark;
  byte          use_defaults;                   /*  use default values */
  byte          current_value_default;          /*  given parameter was ',' */

/* DB-COMMAND VALUES */

#define LIM_DB_L 0x0
#define LIM_DB_U 0x7

/* module global variables initialization */

  G_continuous_output           = 0;
  G_table_all_empty             = 0;
  G_read_input_as_dec           = 0;
  debug_mode                    = 0;  /* DE-activate debug-mode at command start*/
  error                         = ERR_NO_ERROR_C;


#if COMPILE_DEBUG_CODE    /* DEBUG CODE SECTION START */
  if(debug_mode&DEBUG_SHOW_IN_WHAT_FUNCTION_WE_ARE)
  {
          text_out((byte*)DEBUG_MESSAGE_C);
          text_out((byte*)"\n\r...in do_command_DB\n\r");
  }
#endif        /* DEBUG CODE SECTION ENDS */

  debug_parameter               = 0;
  use_defaults                  = 0;
  current_value_default         = 0;

  next_mark = next_char();
  if (next_mark == 'B')
          next_mark = next_char();
  else
    error = ERR_SYNTAX_ERROR_C;

  if ( (next_mark == ':' || next_mark == ';') && !error)
  {
    if(next_mark == ':')        /* change parameters */
    {
       error = is_line_buffer_ok();            /*  check that line is not empty */
       if(!error)
       {
          decode_word_parameter(&debug_parameter);
          error = check_limit(debug_parameter, LIM_DB_L, LIM_DB_U, ERR_LIMIT_DB_C);
          if(lookat_next_char() != ';')
              step_comma();
       }
       if(!(correctly_terminated()) && (!error))   /*  check that command was ended correctly */
           error = ERR_SYNTAX_ERROR_C;
    }
    if(next_mark == ';')          /* only to view parameters */
    {
       if(!(correctly_terminated()) && (!error))   /*  check that command was ended correctly */
           error = ERR_SYNTAX_ERROR_C;

       debug_parameter = 0xFF;
    }

    if(!error)
    {
      /* change debug state to zero, or change one bit at a time */
        switch (debug_parameter)
        {
          case 0x0: debug_mode  = 0;          break;  /* clear all */
          case 0x1: debug_mode ^= DEBUG_OUTPUT_ALSO_EMPTY_ARRAYS;   break;  /* change state of one bit */
          case 0x2: debug_mode ^= DEBUG_CHECK_RECORD_STATE_OUTPUT;    break;
          case 0x3: debug_mode ^= DEBUG_SHOW_IN_WHAT_FUNCTION_WE_ARE; break;
          case 0x4: debug_mode ^= DEBUG_OUTPUT_USERS_GIVEN_PARAMETERS;  break;
          case 0x5: debug_mode ^= DEBUG_OUTPUT_BRM_PID_SELECTION;   break;
          case 0x6: debug_mode ^= DEBUG_OUTPUT_GENERAL_FUNCTION_DATA; break;
          case 0x7: debug_mode =  DEBUG_SELECT_ALL;       break;
          default:  /* when only in output mode */        break;
        }

        /* output debug mode situation to user */

        chars_out('=', LINE_LENGTH_C);

        text_out((byte*)"\n\r\n\r                     E2K DEBUG INFORMATION OUTPUT ");
        text_out((byte*)    "\n\r                    ==============================\n\r");

        text_out((byte*)    "\n\r                       READ BRM PID FROM DATABASE        "); output_bool(PAC_COMPILE_ENV_MT);

        text_out((byte*)"\n\r\n\r");
        text_out((byte*)"\n\rDEBUG PARAMETER NAME                             NUMBER       STATUS\n\r");
        chars_out('-', LINE_LENGTH_C);
        text_out((byte*)"\n\rOUTPUT OF EMPTY ARRAYS                           1            ");
        output_bool(debug_mode & DEBUG_OUTPUT_ALSO_EMPTY_ARRAYS);

        text_out((byte*)"\n\rOUTPUT CHECK_REC_STATE-FUNCTION RESULTS          2            ");
        output_bool(debug_mode & DEBUG_CHECK_RECORD_STATE_OUTPUT);

        text_out((byte*)"\n\rSHOW VISITED FUNCTIONS                           3            ");
        output_bool(debug_mode & DEBUG_SHOW_IN_WHAT_FUNCTION_WE_ARE);

        text_out((byte*)"\n\rSHOW USERS INPUT PARAMETERS                      4            ");
        output_bool(debug_mode & DEBUG_OUTPUT_USERS_GIVEN_PARAMETERS);

        text_out((byte*)"\n\rOUTPUT BRM PID SELECTION INFORMATION             5            ");
        output_bool(debug_mode & DEBUG_OUTPUT_BRM_PID_SELECTION);

        text_out((byte*)"\n\rOUTPUT GENERAL FUNCTION SPECIFIC DATA            6            ");
        output_bool(debug_mode & DEBUG_OUTPUT_GENERAL_FUNCTION_DATA);

        text_out((byte*)"\n\r");
        chars_out('=', LINE_LENGTH_C);
        text_out((byte*)"\n\r\n\r");
     }
  }
  else
  {
     error = ERR_SYNTAX_ERROR_C;             /*  ':' not found in command line */
  }
}
#endif  /* DEBUG CODE SECTION ENDS */

/*
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
_.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._.·´¯`·._
*/

/***********************************************************************
 *  FUNCTION: main
 **********************************************************************/
#if (MCRNC || ADA)
/*void _near_ main()*/
void  e2k_main(void *dummy, __start_parameters_t * params)
#else
void _near_ main()
#endif
{
/* initialization phase */
  byte          cmd_first_char;

  debug_mode    = 0;  /* DE-activate debug-mode at startup*/
  G_continuous_output = 0;
  G_table_all_empty = 0;
  error=ERR_NO_ERROR_C;

  for(;;)
  {
     cmd_first_char = next_char();
#if (MCRNC || ADA)
     if (viewed__own_unit_state() == UNIT_STATE_T_WOEX_C) /* E2K use allowed only in ICSU WO-EX state */
#else
     if (own_unit_state == UNIT_STATE_T_WOEX_C) /* E2K use allowed only in ICSU WO-EX state */
#endif
     {
       switch(cmd_first_char)
       {
        case '?':  /*Help*/
             do_help();
             break;

        case 'A':
             do_command_A();
             break;

        case 'B':
             do_command_B();
             break;

        case 'C':
             do_command_C();
             break;

  /* DEBUG CODE SECTION START */
  #if COMPILE_DEBUG_CODE

          case 'D':
             do_command_DB();
             break;


  #endif  /* DEBUG CODE SECTION ENDS */
/*
        case 'E':

           text_out((byte*)"\n\r*****COMMAND DISABLED IN THIS RELEASE*****\n\r\n\r");

             do_command_E();
             break;
*/
        case 'L':
             do_command_L();
             break;

        case 'M':
            do_command_M();
            break;

        case 'N':
             do_command_N();
             break;

        case 'O':
             do_command_O();
             break;

        case 'P':
             do_command_P();
             break;

        case 'R':
             do_command_R();
             break;

        case 'S':
             do_command_S();
             break;

        case 'T':
             do_command_T();
             break;

        case RET_C:                        /*  only 'enter' */

        case ';':                          /*  only ';' */
             error=ERR_NO_ERROR_C;
             break;

        case 'Z':                          /*  Quit E2K */
             return;

        default:
             error=ERR_SYNTAX_ERROR_C;
             break;
        }
     }
     else
     {
       switch(cmd_first_char)
       {
     	 case 'Z': 
     	   return;
     	 	 	    
         case RET_C:                        /*  only 'enter' */
         case ';':                          /*  only ';' or 'enter' */
           error=ERR_NO_ERROR_C;
           break;
              
     	 default:
     	   error=ERR_ICSU_UNIT_NOT_WOEX__C;
     	   break;
       }
     }
     test_error(error);                 /*  check for errors */
     read_command_line((byte*)prompt);
  }
}
#if (MCRNC || ADA)

static const struct __tnsdl_process_data __family[] =
{
   {
#ifndef __E2KMAS_OPTIONS
#define __E2KMAS_OPTIONS __DEF_MASTER_OPT
#endif
   0, e2k_main, 0, 0, __E2KMAS_OPTIONS, 0, "clipro"
   }
};

int __tnsdl_main (int in_argc, char *in_argv[])
{
  lib_main_r(0, __family);
    return 0;
}
#endif

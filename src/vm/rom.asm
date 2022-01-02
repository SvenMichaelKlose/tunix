;***********************************************************************************;
;***********************************************************************************;
;
; The almost completely commented VIC 20 ROM disassembly. V1.01 Lee Davison 2005-2012.
; With enhancements by Simon Rowe <srowe@mose.org.uk>.

; This is a bit correct assembly listing for the VIC 20 BASIC and KERNAL ROMs as one 16K
; ROM. You should be able to assemble the VIC ROMs from this with most 6502 assemblers,
; as no macros or 'special' features were used. This has been tested using Michal
; Kowalski's 6502 Simulator assemble function. See http://exifpro.com/utils.html for
; this program.

; Many references were used to complete this disassembly including, but not limited to,
; "Mapping the VIC 20", "Mapping the C64", "VIC 20 Programmers Reference", "VIC 20 User
; Guide", "The Complete Commodore Inner Space Anthology", "VIC Revealed" and various
; text files, pictures and other documents.

    .include "constants.asm"

.export COLDST, WARMST, STMDSP, FUNDSP, OPTAB, LAB_C09B, RESLST, ERRSTR01, ERRSTR02, ERRSTR03, ERRSTR04, ERRSTR05, ERRSTR06, ERRSTR07, ERRSTR08, ERRSTR09, ERRSTR0A, ERRSTR0B, ERRSTR0C, ERRSTR0D, ERRSTR0E, ERRSTR0F, ERRSTR10, ERRSTR11, ERRSTR12, ERRSTR13, ERRSTR14, ERRSTR15, ERRSTR16, ERRSTR17, ERRSTR18, ERRSTR19, ERRSTR1A, ERRSTR1B, ERRSTR1C, ERRSTR1D, BMSGS, OKSTR, ERRORSTR, INSTR, READYSTR, CRLFBRK, BREAKSTR, SCNSTK, LAB_C38F, LAB_C3A4, LAB_C3B0, LAB_C3B7, MAKSPC, MOVEBL, LAB_C3DC, LAB_C3E8, LAB_C3EC, LAB_C3F3, STKSPC, RAMSPC, LAB_C412, LAB_C416, LAB_C421, LAB_C434, MEMERR, ERROR, ERROR2, LAB_C456, PRDY, READY, MAIN, MAIN2, NEWLIN, LAB_C4D7, LAB_C4DF, LAB_C4ED, LAB_C508, LAB_C522, LAB_C52A, LNKPRG, LAB_C53C, LAB_C544, LAB_C55F, GETLIN, LAB_C562, LAB_C576, CRNCH, CRNCH2, LAB_C582, LAB_C58E, LAB_C5A4, LAB_C5AC, LAB_C5B6, LAB_C5B8, LAB_C5C7, LAB_C5C9, LAB_C5DC, LAB_C5DE, LAB_C5E5, LAB_C5EE, LAB_C5F5, LAB_C5F9, LAB_C609, FINLIN, LAB_C617, LAB_C62E, LAB_C637, LAB_C640, LAB_C641, NEW, LAB_C644, LAB_C659, CLR, LAB_C660, LAB_C663, LAB_C677, LAB_C67A, LAB_C68D, STXTPT, LIST, LAB_C6A4, LAB_C6BB, LAB_C6C9, LAB_C6E6, LAB_C6E8, LAB_C6EF, LAB_C6F3, LAB_C700, LAB_C714, LAB_C717, QPLOP, LAB_C72C, LAB_C72F, LAB_C737, FOR, LAB_C753, LAB_C78B, LAB_C79F, NEWSTT, LAB_C7BE, LAB_C7CE, LAB_C7E1, GONE, LAB_C7ED, LAB_C7EF, LAB_C804, LAB_C807, LAB_C80B, LAB_C80E, RESTORE, LAB_C827, LAB_C82B, TSTSTOP, BSTOP, END, LAB_C832, LAB_C849, LAB_C84B, LAB_C854, CONT, LAB_C862, LAB_C870, RUN, LAB_C87D, GOSUB, LAB_C897, GOTO, LAB_C8BC, LAB_C8C0, LAB_C8D1, RETURN, LAB_C8E3, LAB_C8E8, LAB_C8EB, SKIPST, BUMPTP, LAB_C905, FIND2, LAB_C909, LAB_C911, LAB_C919, IF, LAB_C937, REM, LAB_C940, LAB_C948, ON, LAB_C953, LAB_C957, LAB_C95F, LAB_C96A, DECBIN, LAB_C971, LAB_C99F, LET, LET2, LAB_C9D6, LAB_C9D9, LET5, LAB_C9ED, LAB_CA07, LAB_CA1D, LAB_CA24, LAB_CA27, LET9, LAB_CA3D, LAB_CA4B, LAB_CA52, LAB_CA68, PRINTN, CMD, LAB_CA90, PRT1, LAB_CA9D, PRINT, LAB_CAA2, LAB_CACA, LAB_CAD7, LAB_CAE5, LAB_CAE7, PRT6, LAB_CAEE, PRT7, LAB_CB0E, LAB_CB0F, LAB_CB10, LAB_CB13, LAB_CB19, PRTSTR, LAB_CB21, LAB_CB28, PRTOS, LAB_CB42, LAB_CB45, LAB_CB47, IGRERR, LAB_CB57, LAB_CB5B, LAB_CB5F, LAB_CB62, LAB_CB6B, GET, LAB_CB92, INPUTN, LAB_CBB5, LAB_CBB7, INPUT, LAB_CBCE, LAB_CBD6, LAB_CBEA, LAB_CBF9, LAB_CC03, READ, LAB_CC0D, LAB_CC0F, LAB_CC15, LAB_CC41, LAB_CC4A, LAB_CC4D, LAB_CC51, LAB_CC65, LAB_CC71, LAB_CC72, LAB_CC7D, LAB_CC89, LAB_CC91, LAB_CC9D, LAB_CCB8, LAB_CCD1, LAB_CCDF, LAB_CCEA, LAB_CCFB, EXTRA, LAB_CD0C, NEXT, LAB_CD24, LAB_CD27, LAB_CD32, LAB_CD35, LAB_CD75, LAB_CD78, TYPCHK, LAB_CD8D, LAB_CD8F, LAB_CD90, LAB_CD96, LAB_CD97, LAB_CD99, FRMEVL, LAB_CDA4, LAB_CDA9, LAB_CDB8, LAB_CDBB, LAB_CDD7, LAB_CDE8, LAB_CDF0, LAB_CDF9, LAB_CDFA, LAB_CE07, LAB_CE11, LAB_CE19, LAB_CE20, LAB_CE30, LAB_CE33, LAB_CE38, LAB_CE43, LAB_CE58, LAB_CE5B, LAB_CE5D, LAB_CE64, LAB_CE66, LAB_CE80, EVAL, FEVAL, LAB_CE8A, LAB_CE8F, LAB_CE92, LAB_CE9A, PIVAL, LAB_CEAD, LAB_CEBD, LAB_CEC6, LAB_CECC, EQUAL, LAB_CEE3, LAB_CEEA, PAREXP, RPACHK, LPACHK, COMCHK, SYNCHR, LAB_CF08, FACT10, LAB_CF0F, VARRANGE, LAB_CF27, FACT12, LAB_CF5C, LAB_CF5D, LAB_CF6E, LAB_CF84, LAB_CF92, LAB_CFA0, FACT17, LAB_CFD1, LAB_CFD6, ORR, ANDD, COMPAR, CMPST, LAB_D056, LAB_D05B, LAB_D061, LAB_D066, LAB_D072, LAB_D07B, LAB_D07E, DIM, EVLVAR, LAB_D090, LAB_D092, LAB_D09C, LAB_D09F, LAB_D0AF, LAB_D0B0, LAB_D0BA, LAB_D0C4, LAB_D0D4, LAB_D0DB, FNDVAR, LAB_D0EF, LAB_D0F1, LAB_D0FB, LAB_D109, CHRTST, LAB_D11C, MAKVAR, LAB_D123, LAB_D128, LAB_D138, LAB_D13B, LAB_D143, LAB_D159, RETVP, LAB_D18F, ARYHED, LAB_D1A0, MAXINT, INTIDX, GETSUB, LAB_D1B8, MAKINT, LAB_D1CC, LAB_D1CE, ARY, LAB_D1DB, LAB_D21C, LAB_D228, LAB_D237, BADSUB, ILQUAN, LAB_D24A, ARY2, ARY6, LAB_D274, LAB_D27D, LAB_D286, LAB_D296, LAB_D2B9, LAB_D2C8, LAB_D2CD, ARY14, LAB_D2F2, LAB_D308, LAB_D30B, LAB_D30E, LAB_D30F, LAB_D320, LAB_D331, LAB_D337, LAB_D34B, M16, LAB_D355, LAB_D35F, LAB_D378, FRE, LAB_D384, MAKFP, POS, LAB_D3A2, NODIRM, UNDEF, DEF, FN, EVALFN, LAB_D418, LAB_D449, EVFN3, STR, LAB_D46F, ALC1, LAB_D47D, MAKSTR, LAB_D48D, LAB_D497, LAB_D4A4, LAB_D4A8, LAB_D4A9, LAB_D4B5, LAB_D4BF, LAB_D4CA, LAB_D4D2, LAB_D4D5, ALCSPAC, LAB_D4F6, LAB_D501, LAB_D50B, LAB_D516, GRBCOL, LAB_D52A, LAB_D544, LAB_D54D, LAB_D559, LAB_D561, LAB_D566, LAB_D56E, LAB_D572, LAB_D57D, LAB_D5AE, LAB_D5B0, LAB_D5B8, GCOL13, LAB_D5C7, LAB_D5DC, LAB_D5E6, LAB_D5F6, LAB_D601, COLLECT, ADDSTR, LAB_D65D, XFERSTR, LAB_D688, LAB_D68C, LAB_D690, LAB_D699, LAB_D6A2, DELST, LAB_D6A6, LAB_D6AA, LAB_D6D5, LAB_D6D6, DELTSD, LAB_D6EB, CHR, LEFT, LAB_D706, LAB_D70C, LAB_D70D, LAB_D70E, LAB_D725, RIGHT, MID, LAB_D748, FINLMR, LEN, GSINFO, ASC, LAB_D798, GETBYT, LAB_D79E, LAB_D7A1, VAL, LAB_D7B5, LAB_D7CD, LAB_D7E2, GETAD, LAB_D7F1, MAKADR, PEEK, POKE, WAIT, LAB_D83C, LAB_D840, LAB_D848, ADD05, LAMIN, SUB, PLUS1, LAPLUS, PLUS, LAB_D86F, LAB_D877, LAB_D893, LAB_D897, LAB_D8A3, LAB_D8AF, LAB_D8D2, LAB_D8D7, LAB_D8DB, ZERFAC, LAB_D8F9, LAB_D8FB, NORMLZ, LAB_D91D, LAB_D929, LAB_D936, LAB_D938, LAB_D946, COMFAC, LAB_D94D, LAB_D96F, LAB_D97D, OVERFL, ASRRES, LAB_D985, LAB_D999, LAB_D9A6, LAB_D9AC, LAB_D9B0, LAB_D9BA, FPC1, LOGCON, LAB_D9D6, LAB_D9DB, LAB_D9E0, LAB_D9E5, LOG, LAB_D9F1, LAB_D9F4, TIMES, MULT, LAB_DA30, TIMES3, LAB_DA5E, LAB_DA61, LAB_DA7D, LAB_DA8B, LODARG, MULDIV, LAB_DAB9, LAB_DAC4, LAB_DACF, LAB_DAD4, LAB_DADA, LAB_DADF, MULTEN, LAB_DAED, LAB_DAF8, FPCTEN, DIVTEN, LAB_DB07, LADIV, DIVIDE, LAB_DB29, LAB_DB3F, LAB_DB4C, LAB_DB4F, LAB_DB5D, LAB_DB7A, LAB_DB7E, LAB_DB8A, LAB_DB8F, LODFAC, FACTF2, FACTF1, FACTFP, STORFAC, ATOF, LAB_DBFE, LAB_DC02, RFTOA, FTOA, LAB_DC11, LAB_DC1A, ROUND, LAB_DC23, SGNFAC, LAB_DC2F, LAB_DC31, LAB_DC38, SGN, INTFP, INTFP1, LAB_DC49, LAB_DC4F, ABS, CMPFAC, LAB_DC5D, LAB_DC92, LAB_DC98, FPINT, LAB_DCAF, LAB_DCBA, LAB_DCBB, INT, FILFAC, LAB_DCF2, ASCFLT, LAB_DCF7, LAB_DD06, LAB_DD0A, LAB_DD0D, LAB_DD0F, LAB_DD2E, LAB_DD30, LAB_DD33, LAB_DD35, LAB_DD41, LAB_DD47, LAB_DD49, LAB_DD52, LAB_DD5B, LAB_DD62, LAB_DD67, LAB_DD6A, LAB_DD71, ASCI8, LAB_DD91, LAB_DDA0, LAB_DDAE, FPC12, LAB_DDB8, LAB_DDBD, PRTIN, PRTFIX, LAB_DDDA, FLTASC, LAB_DDDF, LAB_DDE7, LAB_DDF8, LAB_DE00, LAB_DE09, LAB_DE0B, LAB_DE16, LAB_DE21, LAB_DE28, LAB_DE2F, LAB_DE32, LAB_DE47, LAB_DE48, LAB_DE53, LAB_DE64, LAB_DE66, LAB_DE68, LAB_DE6A, LAB_DE8E, LAB_DE90, LAB_DE97, LAB_DEB2, LAB_DEC4, LAB_DEC6, LAB_DED3, LAB_DEE3, LAB_DEEF, LAB_DF04, LAB_DF07, LAB_DF0C, FLP05, NULLVAR, FLTCON, HMSCON, LAB_DF52, SQR, EXPONT, LAB_DF84, LAB_DF9E, NEGFAC, LAB_DFBE, EXPCON, LAB_DFC4, EXP, LAB_DFFD, LAB_E008, LAB_E00B, LAB_E01B, SEREVL, SER2, LAB_E05A, LAB_E069, LAB_E06D, LAB_E07A, RNDC1, LAB_E08F, RND, LAB_E0BB, LAB_E0D0, LAB_E0E0, LAB_E0F3, PATCHBAS, LAB_E101, LAB_E106, LAB_E109, LAB_E10F, LAB_E115, LAB_E11B, LAB_E121, SYSTEM, LAB_E144, BSAVE, BVERIF, BLOAD, LAB_E187, LAB_E194, LAB_E195, LAB_E1A1, LAB_E1B5, BOPEN, BCLOSE, LAB_E1CE, PARSL, LAB_E1FD, IFCHRG, LAB_E20A, SKPCOM, CHRERR, PAROC, LAB_E23C, LAB_E254, COS, SIN, LAB_E29A, LAB_E29D, LAB_E2AA, TAN, LAB_E2D9, FPC20, LAB_E2E2, LAB_E2E7, LAB_E2EC, ATN, LAB_E313, LAB_E321, LAB_E334, LAB_E33A, ATNCON, COLDBA, CGIMAG, LAB_E38D, LAB_E39E, INITBA, LAB_E3C4, LAB_E403, FREMSG, BFREMSG, BASMSG, BASVCTRS, INITVCTRS, LAB_E45D, WARMBAS, PATCHER, SEROUT1, SEROUT0, SERGET, PATCH1, PATCH2, LAB_E4CC, PATCH3, LAB_E4D7, FIOBASE, FSCREEN, FPLOT, LAB_E513, INITSK, LAB_E536, CLSR, LAB_E568, LAB_E570, LAB_E57B, HOME, SETSLINK, LAB_E58B, LAB_E597, LAB_E5A8, LAB_E5B2, SETIODEF, INITVIC, LAB_E5C5, LP2, LAB_E5D4, GETQUE, LAB_E5E8, LAB_E602, LAB_E60E, GET2RTN, LAB_E621, LAB_E62A, GETSCRN, LAB_E657, LAB_E67E, LAB_E684, LAB_E688, LAB_E691, LAB_E6A3, LAB_E6A6, LAB_E6A8, LAB_E6B6, QUOTECK, LAB_E6C4, SETCHAR, LAB_E6C7, LAB_E6CB, LAB_E6CD, LAB_E6D3, LAB_E6DC, LAB_E6E4, SCROLL, LAB_E701, LAB_E70E, LAB_E715, LAB_E719, LAB_E720, LAB_E723, LAB_E72C, RETREAT, LAB_E737, SCRNOUT, LAB_E756, LAB_E75D, LAB_E769, LAB_E76B, LAB_E771, LAB_E778, LAB_E785, LAB_E78E, LAB_E79F, LAB_E7AA, LAB_E7B1, LAB_E7B7, LAB_E7BE, LAB_E7D4, LAB_E7D6, LAB_E7D9, LAB_E7EC, LAB_E7F4, LAB_E7F7, LAB_E7FA, LAB_E800, LAB_E81D, LAB_E82A, LAB_E831, LAB_E845, LAB_E84C, LAB_E851, LAB_E86D, LAB_E870, LAB_E874, LAB_E879, LAB_E88E, LAB_E893, LAB_E89B, LAB_E8AB, LAB_E8B1, LAB_E8B8, LAB_E8BB, NXTLINE, LAB_E8C7, LAB_E8CF, RTRN, BACKUP, LAB_E8EC, LAB_E8F7, FORWARD, LAB_E8FE, LAB_E909, LAB_E911, COLORSET, LAB_E914, LAB_E91D, COLORTBL, LAB_E928, SCRL, LAB_E981, LAB_E989, LAB_E99D, LAB_E9A2, LAB_E9AC, LAB_E9D6, LAB_E9DF, OPENLIN, LAB_E9F0, LAB_EA08, LAB_EA16, LAB_EA2C, LAB_EA31, LAB_EA3F, LAB_EA44, MOVLIN, LAB_EA60, LAB_EA62, SETADDR, LINPTR, CLRALINE, LAB_EA95, SYNPRT, PUTSCRN, COLORSYN, IRQ, LAB_EAEA, LAB_EAEF, LAB_EB01, LAB_EB0A, LAB_EB12, FSCNKEY, LAB_EB40, LAB_EB4A, LAB_EB60, LAB_EB62, LAB_EB63, LAB_EB71, LAB_EB74, LAB_EB84, LAB_EB8F, LAB_EBA1, LAB_EBAB, LAB_EBBA, LAB_EBD6, SETKEYS, LAB_EC0F, LAB_EC18, LAB_EC43, KEYVCTRS, NORMKEYS, SHFTKEYS, LOGOKEYS, CHARSET, GRAPHMODE, LAB_ED3C, LAB_ED3F, LAB_ED4D, WRAPLINE, WHATKEYS, CTRLKEYS, VICINIT, RUNTB, LDTB2, FTALK, FLISTEN, LIST1, LAB_EE2B, LAB_EE38, LAB_EE40, SRSEND, LAB_EE5A, LAB_EE60, LAB_EE66, LAB_EE73, LAB_EE88, LAB_EE8B, LAB_EEA5, SRBAD, LAB_EEB7, LAB_EEB9, FSECOND, SCATN, FTKSA, LAB_EED3, LAB_EEDD, FCIOUT, LAB_EEED, LAB_EEF2, FUNTLK, FUNLSN, LAB_EF09, LAB_EF0C, LAB_EF0F, FACPTR, LAB_EF21, LAB_EF29, LAB_EF2E, LAB_EF3C, LAB_EF45, LAB_EF54, LAB_EF58, LAB_EF66, LAB_EF7F, SRCLKHI, SRCLKLO, WAITABIT, LAB_EF9B, RSNXTBIT, LAB_EFB0, LAB_EFB9, RSPRTY, LAB_EFCE, LAB_EFCF, LAB_EFDA, LAB_EFDE, LAB_EFE4, RSSTOPS, RSNXTBYT, LAB_EFFB, RSMISSNG, LAB_F019, LAB_F021, RSCPTBIT, LAB_F031, LAB_F035, RSINBIT, LAB_F04A, RSSTPBIT, LAB_F04D, RSPREPIN, RSSTRBIT, RSINBYTE, LAB_F081, LAB_F089, RSPRTYER, RSOVERUN, RSBREAK, RSFRAMER, LAB_F0B3, RSDVCERR, RSOPNOUT, LAB_F0CD, LAB_F0D4, LAB_F0E1, LAB_F0E8, LAB_F0EB, RSOUTSAV, RSPREPOT, RSOPNIN, LAB_F12B, LAB_F138, LAB_F13F, LAB_F144, LAB_F146, RSNXTIN, LAB_F15D, RSPAUSE, LAB_F166, LAB_F172, KMSGTBL, KM_IOERR, KM_SRCHG, KM_FOR, KM_PRPLY, KM_REcpy, KM_LODNG, KM_SAVNG, KM_VFYNG, KM_FOUND, KM_OK, SPMSG, KMSGSHOW, LAB_F1F3, FGETIN, LAB_F201, LAB_F205, FCHRIN, LAB_F21D, LAB_F22A, LAB_F244, LAB_F24A, LAB_F24D, CHRINTP2, LAB_F260, CHRINSR, LAB_F26A, LAB_F26B, LAB_F26C, CHRINRS, LAB_F279, FCHROUT, LAB_F285, LAB_F28B, CHROUTTP, LAB_F2AA, LAB_F2AF, LAB_F2B8, LAB_F2B9, FCHKIN, LAB_F2CF, LAB_F2E3, LAB_F2EC, LAB_F2F0, LAB_F2FE, LAB_F301, FCHKOUT, LAB_F311, LAB_F318, LAB_F31B, LAB_F328, LAB_F32E, LAB_F332, LAB_F33F, LAB_F342, FCLOSE, LAB_F351, LAB_F37F, LAB_F384, LAB_F38D, LAB_F39E, LAB_F3AE, LAB_F3B1, LAB_F3B2, LAB_F3CD, LAB_F3CE, FNDFLNO, LAB_F3D4, LAB_F3D6, SETFLCH, LAB_F3EE, FCLALL, FCLRCHN, LAB_F3FC, LAB_F403, FOPEN, LAB_F411, LAB_F419, LAB_F422, LAB_F444, LAB_F44B, LAB_F453, LAB_F46C, LAB_F46F, LAB_F478, LAB_F482, LAB_F491, LAB_F493, LAB_F494, SERNAME, LAB_F4B2, LAB_F4B8, LAB_F4C2, LAB_F4C5, OPENRS, LAB_F4D9, LAB_F4E7, LAB_F4F4, LAB_F51B, LAB_F533, LAB_F53C, FLOAD, FLOAD2, LAB_F553, LAB_F556, LAB_F563, LAB_F58A, LAB_F598, LAB_F5B3, LAB_F5B5, LAB_F5BB, LAB_F5C7, LAB_F5CA, LOADTP, LAB_F5D9, LAB_F5E1, LAB_F5EE, LAB_F5F5, LAB_F604, LAB_F611, LAB_F615, LAB_F641, LAB_F646, SRCHING, FILENAME, LAB_F65F, LAB_F669, LDVRMSG, LAB_F672, FSAVE, FSAVE2, LAB_F689, LAB_F68C, LAB_F69D, LAB_F6BC, LAB_F6CB, LAB_F6D2, LAB_F6D7, LAB_F6DA, LAB_F6EF, SAVETP, LAB_F6F8, LAB_F70F, LAB_F726, LAB_F727, SAVING, FUDTIM, LAB_F740, LAB_F755, FRDTIM, FSETTIM, FSTOP, LAB_F77D, FE_2MNYF, FE_ALOPN, FE_NTOPN, FE_NTFND, FE_DVNTP, FE_NTINP, FE_NTOUT, FE_MISFN, FE_ILDEV, LAB_F7AC, FAH, LAB_F7CE, LAB_F7DA, LAB_F7E4, LAB_F7E6, TAPEH, LAB_F7FE, LAB_F822, LAB_F834, LAB_F84C, TPBUFA, LDAD1, FNDHDR, LAB_F874, LAB_F888, LAB_F889, JTP20, CSTEL, LAB_F89B, LAB_F89E, CS10, LAB_F8B5, CSTE2, RDTPBLKS, RBLK, WBLK, LAB_F8E6, LAB_F8EA, LAB_F8ED, TAPE, LAB_F923, LAB_F925, LAB_F92F, TSTOP, LAB_F957, LAB_F95C, STT1, LAB_F972, LAB_F979, READT, LAB_F9C3, LAB_F9E2, LAB_F9E5, LAB_F9ED, LAB_F9F1, LAB_F9F3, LAB_FA06, LAB_FA19, LAB_FA22, LAB_FA25, LAB_FA2E, LAB_FA30, LAB_FA47, LAB_FA60, LAB_FA68, LAB_FA6C, LAB_FA91, LAB_FAA0, LAB_FAAA, TPSTORE, LAB_FABD, LAB_FAD3, LAB_FAD7, LAB_FADA, LAB_FAF0, LAB_FAF6, LAB_FB07, LAB_FB0D, LAB_FB1B, LAB_FB23, LAB_FB38, LAB_FB55, LAB_FB7C, LAB_FB80, LAB_FB87, LAB_FB90, LAB_FB95, LAB_FB97, LAB_FBA0, LAB_FBAC, LAB_FBB6, LAB_FBCF, RD300, NEWCH, TPTOGLE, LAB_FBF1, LAB_FBF3, LAB_FBF5, BLKEND, WRITE, LAB_FC21, LAB_FC2E, LAB_FC47, LAB_FC4A, LAB_FC54, LAB_FC6A, LAB_FC6E, LAB_FC7D, LAB_FC8C, LAB_FC92, WRTN1, LAB_FC9C, WRTZ, TNIF, LAB_FCF4, BSIV, LAB_FCFB, TNOFF, VPRTY, WRT62, LAB_FD21, START, LAB_FD2F, CHKAUTO, LAB_FD41, LAB_FD4C, A0CBM, FRESTOR, FVECTOR, LAB_FD5D, LAB_FD64, VECTORS, INITMEM, LAB_FD90, LAB_FDAF, LAB_FDB5, LAB_FDCF, LAB_FDD2, LAB_FDDE, LAB_FDEB, IRQVCTRS, INITVIA, LAB_FE39, FSETNAM, FSETLFS, FREADST, FSETMSG, READIOST, ORIOST, FSETTMO, FMEMTOP, LAB_FE75, LAB_FE7B, FMEMBOT, LAB_FE8A, TSTMEM, LAB_FEA4, NMI, NMI2, LAB_FEC7, BREAK, RSNMI, LAB_FEFF, LAB_FF02, LAB_FF2C, LAB_FF38, _RTI, BAUDTBL, IRQROUT, LAB_FF82, RESTOR, VECTOR, SETMSG, SECOND, TKSA, MEMTOP, MEMBOT, SCNKEY, SETTMO, ACPTR, CIOUT, UNTLK, UNLSN, LISTEN, TALK, READST, SETLFS, SETNAM, OPEN, CLOSE, CHKIN, CHKOUT, CLRCHN, CHRIN, CHROUT, LOAD, SAVE, SETTIM, RDTIM, STOP, GETIN, CLALL, UDTIM, SCREEN, PLOT, IOBASE

    .org $C000

;***********************************************************************************;
;***********************************************************************************;
;
; BASIC ROM start

COLDST:
    .word   COLDBA      ; BASIC cold start entry point

WARMST:
    .word   WARMBAS     ; BASIC warm start entry point

;CBMBASIC
    .byte   "CBMBASIC"  ; ROM name, unreferenced


;***********************************************************************************;
;
; Action addresses for primary commands. These are called by pushing the address
; onto the stack and doing an rts so the actual address - 1 needs to be pushed.

STMDSP:
    .word   END-1       ; perform END
    .word   FOR-1       ; perform FOR
    .word   NEXT-1      ; perform NEXT
    .word   SKIPST-1    ; perform DATA
    .word   INPUTN-1    ; perform INPUT#
    .word   INPUT-1     ; perform INPUT
    .word   DIM-1       ; perform DIM
    .word   READ-1      ; perform READ

    .word   LET-1       ; perform LET
    .word   GOTO-1      ; perform GOTO
    .word   RUN-1       ; perform RUN
    .word   IF-1        ; perform IF
    .word   RESTORE-1   ; perform RESTORE
    .word   GOSUB-1     ; perform GOSUB
    .word   RETURN-1    ; perform RETURN
    .word   REM-1       ; perform REM

    .word   BSTOP-1     ; perform STOP
    .word   ON-1        ; perform ON
    .word   WAIT-1      ; perform WAIT
    .word   BLOAD-1     ; perform LOAD
    .word   BSAVE-1     ; perform SAVE
    .word   BVERIF-1    ; perform VERIFY
    .word   DEF-1       ; perform DEF
    .word   POKE-1      ; perform POKE

    .word   PRINTN-1    ; perform PRINT#
    .word   PRINT-1     ; perform PRINT
    .word   CONT-1      ; perform CONT
    .word   LIST-1      ; perform LIST
    .word   CLR-1       ; perform CLR
    .word   CMD-1       ; perform CMD
    .word   SYSTEM-1    ; perform SYS
    .word   BOPEN-1     ; perform OPEN

    .word   BCLOSE-1    ; perform CLOSE
    .word   GET-1       ; perform GET
    .word   NEW-1       ; perform NEW


;***********************************************************************************;
;
; action addresses for functions

FUNDSP:
    .word   SGN         ; perform SGN()
    .word   INT         ; perform INT()
    .word   ABS         ; perform ABS()
    .word   USRPPOK     ; perform USR()

    .word   FRE         ; perform FRE()
    .word   POS         ; perform POS()
    .word   SQR         ; perform SQR()
    .word   RND         ; perform RND()
    .word   LOG         ; perform LOG()
    .word   EXP         ; perform EXP()
    .word   COS         ; perform COS()
    .word   SIN         ; perform SIN()

    .word   TAN         ; perform TAN()
    .word   ATN         ; perform ATN()
    .word   PEEK        ; perform PEEK()
    .word   LEN         ; perform LEN()
    .word   STR         ; perform STR$()
    .word   VAL         ; perform VAL()
    .word   ASC         ; perform ASC()
    .word   CHR         ; perform CHR$()

    .word   LEFT        ; perform LEFT$()
    .word   RIGHT       ; perform RIGHT$()
    .word   MID         ; perform MID$()


;***********************************************************************************;
;
; Precedence byte and action addresses for operators. Like the primary commands these
; are called by pushing the address onto the stack and doing an rts, so again the actual
; address - 1 needs to be pushed.

OPTAB:
    .byte   $79
    .word   PLUS-1      ; +
    .byte   $79
    .word   SUB-1       ; -
    .byte   $7B
    .word   MULT-1      ; *
    .byte   $7B
    .word   DIVIDE-1    ; /
    .byte   $7F
    .word   EXPONT-1    ; ^
    .byte   $50
    .word   ANDD-1      ; and
    .byte   $46
    .word   ORR-1       ; OR
    .byte   $7D
    .word   NEGFAC-1    ; >
    .byte   $5A
    .word   EQUAL-1     ; =
LAB_C09B:
    .byte   $64
    .word   COMPAR-1    ; <


;***********************************************************************************;
;
; BASIC keywords. Each word has b7 set in its last character as an end marker,
; even the one character keywords such as "<" or "=".

; first are the primary command keywords, only these can start a statement

RESLST:
    .byte   "EN",'D'+$80        ; END
    .byte   "FO",'R'+$80        ; FOR
    .byte   "NEX",'T'+$80       ; NEXT
    .byte   "DAT",'A'+$80       ; DATA
    .byte   "INPUT",'#'+$80     ; INPUT#
    .byte   "INPU",'T'+$80      ; INPUT
    .byte   "DI",'M'+$80        ; DIM
    .byte   "REA",'D'+$80       ; READ

    .byte   "LE",'T'+$80        ; LET
    .byte   "GOT",'O'+$80       ; GOTO
    .byte   "RU",'N'+$80        ; RUN
    .byte   "I",'F'+$80         ; IF
    .byte   "RESTOR",'E'+$80    ; RESTORE
    .byte   "GOSU",'B'+$80      ; GOSUB
    .byte   "RETUR",'N'+$80     ; RETURN
    .byte   "RE",'M'+$80        ; REM

    .byte   "STO",'P'+$80       ; STOP
    .byte   "O",'N'+$80         ; ON
    .byte   "WAI",'T'+$80       ; WAIT
    .byte   "LOA",'D'+$80       ; LOAD
    .byte   "SAV",'E'+$80       ; SAVE
    .byte   "VERIF",'Y'+$80     ; VERIFY
    .byte   "DE",'F'+$80        ; DEF
    .byte   "POK",'E'+$80       ; POKE

    .byte   "PRINT",'#'+$80     ; PRINT#
    .byte   "PRIN",'T'+$80      ; PRINT
    .byte   "CON",'T'+$80       ; CONT
    .byte   "LIS",'T'+$80       ; LIST
    .byte   "CL",'R'+$80        ; CLR
    .byte   "CM",'D'+$80        ; CMD
    .byte   "SY",'S'+$80        ; SYS
    .byte   "OPE",'N'+$80       ; OPEN

    .byte   "CLOS",'E'+$80      ; CLOSE
    .byte   "GE",'T'+$80        ; GET
    .byte   "NE",'W'+$80        ; NEW

; next are the secondary command keywords, these can not start a statement

    .byte   "TAB",'('+$80       ; TAB(
    .byte   "T",'O'+$80         ; TO
    .byte   "F",'N'+$80         ; FN
    .byte   "SPC",'('+$80       ; SPC(
    .byte   "THE",'N'+$80       ; THEN

    .byte   "NO",'T'+$80        ; NOT
    .byte   "STE",'P'+$80       ; STEP

; the operators

    .byte   '+'+$80         ; +
    .byte   '-'+$80         ; -
    .byte   '*'+$80         ; *
    .byte   '/'+$80         ; /
    .byte   '^'+$80         ; ^
    .byte   "AN",'D'+$80    ; and

    .byte   "O",'R'+$80     ; OR
    .byte   '>'+$80         ; >
    .byte   '='+$80         ; =
    .byte   '<'+$80         ; <

; the functions

    .byte   "SG",'N'+$80    ; SGN
    .byte   "IN",'T'+$80    ; INT
    .byte   "AB",'S'+$80    ; ABS
    .byte   "US",'R'+$80    ; USR

    .byte   "FR",'E'+$80    ; FRE
    .byte   "PO",'S'+$80    ; POS
    .byte   "SQ",'R'+$80    ; SQR
    .byte   "RN",'D'+$80    ; RND
    .byte   "LO",'G'+$80    ; LOG
    .byte   "EX",'P'+$80    ; EXP
    .byte   "CO",'S'+$80    ; COS
    .byte   "SI",'N'+$80    ; SIN

    .byte   "TA",'N'+$80    ; TAN
    .byte   "AT",'N'+$80    ; ATN
    .byte   "PEE",'K'+$80   ; PEEK
    .byte   "LE",'N'+$80    ; LEN
    .byte   "STR",'$'+$80   ; STR$
    .byte   "VA",'L'+$80    ; VAL
    .byte   "AS",'C'+$80    ; ASC
    .byte   "CHR",'$'+$80   ; CHR$

    .byte   "LEFT",'$'+$80  ; LEFT$
    .byte   "RIGHT",'$'+$80 ; RIGHT$
    .byte   "MID",'$'+$80   ; MID$

; lastly is GO, this is an add on so that GO TO, as well as GOTO, will work

    .byte   "G",'O'+$80     ; GO

    .byte   $00             ; end marker


;***********************************************************************************;
;
; error messages

ER_2MANYF       = $01
ER_FOPEN        = $02
ER_FNOTOPEN     = $03
ER_FNOTFND      = $04
ER_DEVNOTP      = $05
ER_NOTINF       = $06
ER_NOTOUTF      = $07
ER_MISSFNAM     = $08
ER_ILLDEVN      = $09
ER_NXTWOFOR     = $0A
ER_SYNtax       = $0B
ER_RETWOGSB     = $0C
ER_OODATA       = $0D
ER_ILLQUAN      = $0E
ER_OVFLOW       = $0F
ER_OOMEM        = $10
ER_UNDSMNT      = $11
ER_BADSSCPT     = $12
ER_REDIMARY     = $13
ER_DIVBY0       = $14
ER_ILLDIR       = $15
ER_TYPMSMCH     = $16
ER_STR2LONG     = $17
ER_FDATA        = $18
ER_FMLA2CPLX    = $19
ER_CANTCONT     = $1A
ER_UNDEFUN      = $1B
ER_VERIFY       = $1C
ER_LOAD         = $1D
ER_BREAK        = $1E

ERRSTR01:   .byte "TOO MANY FILE",'S'+$80
ERRSTR02:   .byte "FILE OPE",'N'+$80
ERRSTR03:   .byte "FILE NOT OPE",'N'+$80
ERRSTR04:   .byte "FILE NOT FOUN",'D'+$80
ERRSTR05:   .byte "DEVICE NOT PRESEN",'T'+$80
ERRSTR06:   .byte "NOT INPUT FIL",'E'+$80
ERRSTR07:   .byte "NOT OUTPUT FIL",'E'+$80
ERRSTR08:   .byte "MISSING FILE NAM",'E'+$80
ERRSTR09:   .byte "ILLEGAL DEVICE NUMBE",'R'+$80
ERRSTR0A:   .byte "NEXT WITHOUT FO",'R'+$80
ERRSTR0B:   .byte "SYNTA",'X'+$80
ERRSTR0C:   .byte "RETURN WITHOUT GOSU",'B'+$80
ERRSTR0D:   .byte "OUT OF DAT",'A'+$80
ERRSTR0E:   .byte "ILLEGAL QUANTIT",'Y'+$80
ERRSTR0F:   .byte "OVERFLO",'W'+$80
ERRSTR10:   .byte "OUT OF MEMOR",'Y'+$80
ERRSTR11:   .byte "UNDEF'D staTEMEN",'T'+$80
ERRSTR12:   .byte "BAD SUBSCRIP",'T'+$80
ERRSTR13:   .byte "REDIM'D ARRA",'Y'+$80
ERRSTR14:   .byte "DIVISION BY ZER",'O'+$80
ERRSTR15:   .byte "ILLEGAL DIREC",'T'+$80
ERRSTR16:   .byte "TYPE MISMATC",'H'+$80
ERRSTR17:   .byte "STRING TOO LON",'G'+$80
ERRSTR18:   .byte "FILE DAT",'A'+$80
ERRSTR19:   .byte "FORMULA TOO COMPLE",'X'+$80
ERRSTR1A:   .byte "CAN'T CONTINU",'E'+$80
ERRSTR1B:   .byte "UNDEF'D FUNCTIO",'N'+$80
ERRSTR1C:   .byte "VERIF",'Y'+$80
ERRSTR1D:   .byte "LOA",'D'+$80

; error message pointer table

BMSGS:
    .word  ERRSTR01 ; $01   TOO MANY FILES
    .word  ERRSTR02 ; $02   FILE OPEN
    .word  ERRSTR03 ; $03   FILE NOT OPEN
    .word  ERRSTR04 ; $04   FILE NOT FOUND
    .word  ERRSTR05 ; $05   DEVICE NOT PRESENT
    .word  ERRSTR06 ; $06   NOT INPUT FILE
    .word  ERRSTR07 ; $07   NOT OUTPUT FILE
    .word  ERRSTR08 ; $08   MISSING FILE NAME
    .word  ERRSTR09 ; $09   ILLEGAL DEVICE NUMBER
    .word  ERRSTR0A ; $0A   NEXT WITHOUT FOR
    .word  ERRSTR0B ; $0B   SYNtax
    .word  ERRSTR0C ; $0C   RETURN WITHOUT GOSUB
    .word  ERRSTR0D ; $0D   OUT OF DATA
    .word  ERRSTR0E ; $0E   ILLEGAL QUANTITY
    .word  ERRSTR0F ; $0F   OVERFLOW
    .word  ERRSTR10 ; $10   OUT OF MEMORY
    .word  ERRSTR11 ; $11   UNDEF'D staTEMENT
    .word  ERRSTR12 ; $12   BAD SUBSCRIPT
    .word  ERRSTR13 ; $13   REDIM'D ARRAY
    .word  ERRSTR14 ; $14   DIVISION BY ZERO
    .word  ERRSTR15 ; $15   ILLEGAL DIRECT
    .word  ERRSTR16 ; $16   TYPE MISMATCH
    .word  ERRSTR17 ; $17   STRING TOO LONG
    .word  ERRSTR18 ; $18   FILE DATA
    .word  ERRSTR19 ; $19   FORMULA TOO COMPLEX
    .word  ERRSTR1A ; $1A   CAN'T CONTINUE
    .word  ERRSTR1B ; $1B   UNDEF'D FUNCTION
    .word  ERRSTR1C ; $1C   VERIFY
    .word  ERRSTR1D ; $1D   LOAD
    .word  BREAKSTR ; $1E   BREAK


;***********************************************************************************;
;
; BASIC messages

OKSTR:      .byte $0D,"OK",$0D,$00
ERRORSTR:   .byte $0D," ERROR",$00
INSTR:      .byte " IN ",$00
READYSTR:   .byte $0D,$0A,"READY.",$0D,$0A,$00
CRLFBRK:    .byte $0D,$0A
BREAKSTR:   .byte "BREAK",$00


;***********************************************************************************;
;
; spare byte, not referenced

;LAB_C389
    .byte $A0


;***********************************************************************************;
;
; search the stack for FOR or GOSUB activity
; return Zb=1 if FOR variable found

SCNSTK:
    tsx             ; copy stack pointer
    inx             ; +1 pass return address
    inx             ; +2 pass return address
    inx             ; +3 pass calling routine return address
    inx             ; +4 pass calling routine return address
LAB_C38F:
    lda STACK+1,X   ; get token byte from stack
    cmp #TK_FOR     ; is it FOR token
    bne LAB_C3B7    ; exit if not FOR token

                    ; was FOR token
    lda FORPNT+1    ; get FOR/NEXT variable pointer high byte
    bne LAB_C3A4    ; branch if not null

    lda STACK+2,X   ; get FOR variable pointer low byte
    sta FORPNT      ; save FOR/NEXT variable pointer low byte
    lda STACK+3,X   ; get FOR variable pointer high byte
    sta FORPNT+1    ; save FOR/NEXT variable pointer high byte
LAB_C3A4:
    cmp STACK+3,X   ; compare variable pointer with stacked variable pointer
                    ; high byte
    bne LAB_C3B0    ; branch if no match

    lda FORPNT      ; get FOR/NEXT variable pointer low byte
    cmp STACK+2,X   ; compare variable pointer with stacked variable pointer
                    ; low byte
    beq LAB_C3B7    ; exit if match found

LAB_C3B0:
    txa             ; copy index
    clc             ; clear carry for add
    adc #$12        ; add FOR stack use size
    tax             ; copy back to index
    bne LAB_C38F    ; loop if not at start of stack

LAB_C3B7:
    rts


;***********************************************************************************;
;
; open up space in memory, set end of arrays

MAKSPC:
    jsr RAMSPC      ; check available memory, do out of memory error if no room
    sta STREND      ; set end of arrays low byte
    sty STREND+1    ; set end of arrays high byte

; open up space in memory, don't set array end

MOVEBL:
    sec             ; set carry for subtract
    lda GEN2PTR     ; get block end low byte
    sbc TMPPTR      ; subtract block start low byte
    sta INDEX       ; save MOD(block length/$100) byte
    tay             ; copy MOD(block length/$100) byte to .Y
    lda GEN2PTR+1   ; get block end high byte
    sbc TMPPTR+1    ; subtract block start high byte
    tax             ; copy block length high byte to .X
    inx             ; +1 to allow for count=0 exit
    tya             ; copy block length low byte to .A
    beq LAB_C3F3    ; branch if length low byte=0

                    ; block is (.X-1)*$100+.Y bytes, do the .Y bytes first
    lda GEN2PTR     ; get block end low byte
    sec             ; set carry for subtract
    sbc INDEX       ; subtract MOD(block length/$100) byte
    sta GEN2PTR     ; save corrected old block end low byte
    bcs LAB_C3DC    ; if no underflow skip the high byte decrement

    dec GEN2PTR+1   ; else decrement block end high byte
    sec             ; set carry for subtract
LAB_C3DC:
    lda GENPTR      ; get destination end low byte
    sbc INDEX       ; subtract MOD(block length/$100) byte
    sta GENPTR      ; save modified new block end low byte
    bcs LAB_C3EC    ; if no underflow skip the high byte decrement

    dec GENPTR+1    ; else decrement block end high byte
    bcc LAB_C3EC    ; branch always

LAB_C3E8:
    lda (GEN2PTR),Y ; get byte from source
    sta (GENPTR),Y  ; copy byte to destination
LAB_C3EC:
    dey             ; decrement index
    bne LAB_C3E8    ; loop until .Y=0

                    ; now do .Y=0 indexed byte
    lda (GEN2PTR),Y ; get byte from source
    sta (GENPTR),Y  ; save byte to destination
LAB_C3F3:
    dec GEN2PTR+1   ; decrement source pointer high byte
    dec GENPTR+1    ; decrement destination pointer high byte
    dex             ; decrement block count
    bne LAB_C3EC    ; loop until count = $0

    rts


;***********************************************************************************;
;
; check there is room on the stack for .A bytes
; if the stack is too deep do an out of memory error

STKSPC:
    asl             ; *2
    adc #$3E        ; need at least $3E bytes free
    bcs MEMERR      ; if overflow go do out of memory error then warm start

    sta INDEX       ; save result in temp byte
    tsx             ; copy stack
    cpx INDEX       ; compare new limit with stack
    bcc MEMERR      ; if stack < limit do out of memory error then warm start

    rts


;***********************************************************************************;
;
; check available memory, do out of memory error if no room

RAMSPC:
    cpy FRETOP+1    ; compare with bottom of string space high byte
    bcc LAB_C434    ; if less then exit (is ok)

    bne LAB_C412    ; skip next test if greater (tested <)

                    ; high byte was =, now do low byte
    cmp FRETOP      ; compare with bottom of string space low byte
    bcc LAB_C434    ; if less then exit (is ok)

                    ; address is > string storage ptr (oops!)
LAB_C412:
    pha             ; push address low byte
    ldx #$09        ; set index to save TEMPF3 to TMPPTR+1 inclusive
    tya             ; copy address high byte (to push on stack)

                    ; save misc numeric work area
LAB_C416:
    pha             ; push byte
    lda TEMPF3,X    ; get byte from TEMPF3 to TMPPTR+1
    dex             ; decrement index
    bpl LAB_C416    ; loop until all done

    jsr GRBCOL      ; do garbage collection routine

                    ; restore misc numeric work area
    ldx #$F7        ; set index to restore bytes
LAB_C421:
    pla             ; pop byte
    sta TMPPTR+2,X  ; save byte to TEMPF3 to TMPPTR+2
    inx             ; increment index
    bmi LAB_C421    ; loop while -ve

    pla             ; pop address high byte
    tay             ; copy back to .Y
    pla             ; pop address low byte
    cpy FRETOP+1    ; compare with bottom of string space high byte
    bcc LAB_C434    ; if less then exit (is ok)

    bne MEMERR      ; if greater do out of memory error then warm start

                    ; high byte was =, now do low byte
    cmp FRETOP      ; compare with bottom of string space low byte
    bcs MEMERR      ; if >= do out of memory error then warm start

                    ; ok exit, carry clear
LAB_C434:
    rts


;***********************************************************************************;
;
; do out of memory error then warm start

MEMERR:
    ldx #ER_OOMEM   ; error code $10, out of memory error

; do error #.X then warm start

ERROR:
    jmp (IERROR)    ; do error message

; do error #.X then warm start, the error message vector is initialised to point here

ERROR2:
    txa             ; copy error number
    asl             ; *2
    tax             ; copy to index
    lda BMSGS-2,X   ; get error message pointer low byte
    sta INDEX       ; save it
    lda BMSGS-1,X   ; get error message pointer high byte
    sta INDEX+1     ; save it
    jsr CLRCHN      ; close input and output channels
    lda #$00        ; clear .A
    sta CHANNL      ; clear current I/O channel, flag default
    jsr LAB_CAD7    ; print CR/LF
    jsr LAB_CB45    ; print "?"
    ldy #$00        ; clear index
LAB_C456:
    lda (INDEX),Y   ; get byte from message
    pha             ; save status
    and #$7F        ; mask 0xxx xxxx, clear b7
    jsr LAB_CB47    ; output character
    INY             ; increment index
    pla             ; restore status
    bpl LAB_C456    ; loop if character was not end marker

    jsr LAB_C67A    ; flush BASIC stack and clear continue pointer
    lda #<ERRORSTR  ; set " ERROR" pointer low byte
    ldy #>ERRORSTR  ; set " ERROR" pointer high byte


;***********************************************************************************;
;
; print string and do warm start, break entry

PRDY:
    jsr PRTSTR      ; print null terminated string
    ldy CURLIN+1    ; get current line number high byte
    INY             ; increment it
    beq READY       ; branch if was in immediate mode

    jsr PRTIN       ; do " IN " line number message


;***********************************************************************************;
;
; do warm start

READY:
    lda #<READYSTR  ; set "READY." pointer low byte
    ldy #>READYSTR  ; set "READY." pointer high byte
    jsr PRTSTR      ; print null terminated string
    lda #$80        ; set for control messages only
    jsr SETMSG      ; control KERNAL messages
MAIN:
    jmp (IMAIN)     ; do BASIC warm start


;***********************************************************************************;
;
; BASIC warm start, the warm start vector is initialised to point here

MAIN2:
    jsr GETLIN      ; call for BASIC input
    stx CHRGOT+1    ; save BASIC execute pointer low byte
    sty CHRGOT+2    ; save BASIC execute pointer high byte
    jsr CHRGET      ; increment and scan memory
    tax             ; copy byte to set flags
    beq MAIN        ; loop if no input

; got to interpret input line now ...

    ldx #$FF        ; current line high byte to -1, indicates immediate mode
    stx CURLIN+1    ; set current line number high byte
    bcc NEWLIN      ; if numeric character go handle new BASIC line

                    ; no line number .. immediate mode
    jsr CRNCH       ; crunch keywords into BASIC tokens
    jmp LAB_C7E1    ; go scan and interpret code

; handle new BASIC line

NEWLIN:
    jsr DECBIN      ; get fixed-point number into temporary integer
    jsr CRNCH       ; crunch keywords into BASIC tokens
    sty COUNT       ; save index pointer to end of crunched line
    jsr FINLIN      ; search BASIC for temporary integer line number
    bcc LAB_C4ED    ; if not found skip the line delete

                    ; line # already exists so delete it
    ldy #$01        ; set index to next line pointer high byte
    lda (TMPPTR),Y  ; get next line pointer high byte
    sta INDEX+1     ; save it
    lda VARTAB      ; get start of variables low byte
    sta INDEX       ; save it
    lda TMPPTR+1    ; get found line pointer high byte
    sta INDEX+3     ; save it
    lda TMPPTR      ; get found line pointer low byte
    dey             ; decrement index
    sbc (TMPPTR),Y  ; subtract next line pointer low byte
    clc             ; clear carry for add
    adc VARTAB      ; add start of variables low byte
    sta VARTAB      ; set start of variables low byte
    sta INDEX+2     ; save destination pointer low byte
    lda VARTAB+1    ; get start of variables high byte
    adc #$FF        ; -1 + carry
    sta VARTAB+1    ; set start of variables high byte
    sbc TMPPTR+1    ; subtract found line pointer high byte
    tax             ; copy to block count
    sec             ; set carry for subtract
    lda TMPPTR      ; get found line pointer low byte
    sbc VARTAB      ; subtract start of variables low byte
    tay             ; copy to bytes in first block count
    bcs LAB_C4D7    ; if no underflow skip the high byte decrement

    inx             ; increment block count, correct for = 0 loop exit
    dec INDEX+3     ; decrement destination high byte
LAB_C4D7:
    clc             ; clear carry for add
    adc INDEX       ; add source pointer low byte
    bcc LAB_C4DF    ; if no underflow skip the high byte decrement

    dec INDEX+1     ; else decrement source pointer high byte
    clc             ; clear carry

                    ; close up memory to delete old line
LAB_C4DF:
    lda (INDEX),Y   ; get byte from source
    sta (INDEX+2),Y ; copy to destination
    INY             ; increment index
    bne LAB_C4DF    ; while <> 0 do this block

    inc INDEX+1     ; increment source pointer high byte
    inc INDEX+3     ; increment destination pointer high byte
    dex             ; decrement block count
    bne LAB_C4DF    ; loop until all done

                    ; got new line in buffer and no existing same #
LAB_C4ED:
    jsr LAB_C659    ; reset execution to start, clear variables, flush stack
                    ; and return
    jsr LNKPRG      ; rebuild BASIC line chaining
    lda BUF         ; get first byte from buffer
    beq MAIN        ; if no line go do BASIC warm start

                    ; else insert line into memory
    clc             ; clear carry for add
    lda VARTAB      ; get start of variables low byte
    sta GEN2PTR     ; save as source end pointer low byte
    adc COUNT       ; add index pointer to end of crunched line
    sta GENPTR      ; save as destination end pointer low byte
    ldy VARTAB+1    ; get start of variables high byte
    sty GEN2PTR+1   ; save as source end pointer high byte
    bcc LAB_C508    ; if no carry skip the high byte increment

    INY             ; else increment the high byte
LAB_C508:
    sty GENPTR+1    ; save as destination end pointer high byte
    jsr MAKSPC      ; open up space in memory

; Most of what remains to do is copy the crunched line into the space opened up in memory,
; however, before the crunched line comes the next line pointer and the line number. The
; line number is retrieved from the temporary integer and stored in memory, this
; overwrites the bottom two bytes on the stack. Next the line is copied and the next line
; pointer is filled with whatever was in two bytes above the line number in the stack.
; This is ok because the line pointer gets fixed in the line chain re-build.

    lda LINNUM      ; get line number low byte
    ldy LINNUM+1    ; get line number high byte
    sta PREVLN      ; save line number low byte before crunched line
    sty PREVLN+1    ; save line number high byte before crunched line
    lda STREND      ; get end of arrays low byte
    ldy STREND+1    ; get end of arrays high byte
    sta VARTAB      ; set start of variables low byte
    sty VARTAB+1    ; set start of variables high byte
    ldy COUNT       ; get index to end of crunched line
    dey             ; -1
LAB_C522:
    lda CHNLNK,Y    ; get byte from crunched line
    sta (TMPPTR),Y  ; save byte to memory
    dey             ; decrement index
    bpl LAB_C522    ; loop while more to do

; reset execution, clear variables, flush stack, rebuild BASIC chain and do warm start

LAB_C52A:
    jsr LAB_C659    ; reset execution to start, clear variables and flush stack
    jsr LNKPRG      ; rebuild BASIC line chaining
    jmp MAIN        ; go do BASIC warm start


;***********************************************************************************;
;
; rebuild BASIC line chaining

LNKPRG:
    lda TXTTAB      ; get start of memory low byte
    ldy TXTTAB+1    ; get start of memory high byte
    sta INDEX       ; set line start pointer low byte
    sty INDEX+1     ; set line start pointer high byte
    clc             ; clear carry for add
LAB_C53C:
    ldy #$01        ; set index to pointer to next line high byte
    lda (INDEX),Y   ; get pointer to next line high byte
    beq LAB_C55F    ; exit if null, [EOT]

    ldy #$04        ; point to first code byte of line
                    ; there is always 1 byte + [EOL] as null entries are deleted
LAB_C544:
    INY             ; next code byte
    lda (INDEX),Y   ; get byte
    bne LAB_C544    ; loop if not [EOL]

    INY             ; point to byte past [EOL], start of next line
    tya             ; copy it
    adc INDEX       ; add line start pointer low byte
    tax             ; copy to .X
    ldy #$00        ; clear index, point to this line's next line pointer
    sta (INDEX),Y   ; set next line pointer low byte
    lda INDEX+1     ; get line start pointer high byte
    adc #$00        ; add any overflow
    INY             ; increment index to high byte
    sta (INDEX),Y   ; set next line pointer high byte
    stx INDEX       ; set line start pointer low byte
    sta INDEX+1     ; set line start pointer high byte
    bcc LAB_C53C    ; go do next line, branch always

LAB_C55F:
    rts


;***********************************************************************************;
;
; call for BASIC input

GETLIN:
    ldx #$00        ; set channel $00, keyboard
LAB_C562:
    jsr LAB_E10F    ; input character from channel with error check
    cmp #$0D        ; compare with [CR]
    beq LAB_C576    ; if [CR] set .X.Y to BUF - 1, print [CR] and exit

                    ; character was not [CR]
    sta BUF,X       ; save character to buffer
    inx             ; increment buffer index
    cpx #$59        ; compare with max+1
    bcc LAB_C562    ; branch if < max+1

    ldx #ER_STR2LONG    ; error $17, string too long error
    jmp ERROR       ; do error #.X then warm start

LAB_C576:
    jmp LAB_CACA    ; set .X.Y to BUF - 1 and print [CR]


;***********************************************************************************;
;
; crunch BASIC tokens vector

CRNCH:
    jmp (ICRNCH)    ; do crunch BASIC tokens


;***********************************************************************************;
;
; crunch BASIC tokens, the crunch BASIC tokens vector is initialised to point here

CRNCH2:
    ldx CHRGOT+1    ; get BASIC execute pointer low byte
    ldy #$04        ; set save index
    sty GARBFL      ; clear open quote/DATA flag
LAB_C582:
    lda BUF,X       ; get a byte from the input buffer
    bpl LAB_C58E    ; if b7 clear go do crunching

    cmp #TK_PI      ; compare with the token for PI, this token is input
                    ; directly from the keyboard as the PI character.
    beq LAB_C5C9    ; if PI save byte then continue crunching

                    ; this is the bit of code that stops you being able to enter
                    ; some keywords as just single shifted characters. If this
                    ; dropped through you would be able to enter GOTO as just
                    ; [SHIFT]G

    inx             ; increment read index
    bne LAB_C582    ; loop if more to do, branch always

LAB_C58E:
    cmp #' '        ; compare with [SPACE]
    beq LAB_C5C9    ; if [SPACE] save byte then continue crunching

    sta ENDCHR      ; save buffer byte as search character
    cmp #$22        ; compare with quote character
    beq LAB_C5EE    ; if quote go copy quoted string

    bit GARBFL      ; get open quote/DATA token flag
    BVS LAB_C5C9    ; branch if b6 of Oquote set, was DATA
                    ; go save byte then continue crunching

    cmp #'?'        ; compare with "?" character
    bne LAB_C5A4    ; if not "?" continue crunching

    lda #TK_PRINT   ; else set the token for PRINT
    bne LAB_C5C9    ; go save byte then continue crunching, branch always

LAB_C5A4:
    cmp #'0'        ; compare with "0"
    bcc LAB_C5AC    ; if < "0" continue crunching

    cmp #'<'        ; compare with "<"
    bcc LAB_C5C9    ; if <, 0123456789:; go save byte then continue crunching

                    ; gets here with next character not numeric, ";" or ":"
LAB_C5AC:
    sty FBUFPT      ; copy save index
    ldy #$00        ; clear table pointer
    sty COUNT       ; clear word index
    dey             ; adjust for pre increment loop
    stx CHRGOT+1    ; save BASIC execute pointer low byte, buffer index
    dex             ; adjust for pre increment loop
LAB_C5B6:
    INY             ; next table byte
    inx             ; next buffer byte
LAB_C5B8:
    lda BUF,X       ; get byte from input buffer
    sec             ; set carry for subtract
    sbc RESLST,Y    ; subtract table byte
    beq LAB_C5B6    ; go compare next if match

    cmp #$80        ; was it end marker match ?
    bne LAB_C5F5    ; if not go try the next keyword

                    ; actually this works even if the input buffer byte is the
                    ; end marker, i.e. a shifted character. As you can't enter
                    ; any keywords as a single shifted character, see above,
                    ; you can enter keywords in shorthand by shifting any
                    ; character after the first. so RETURN can be entered as
                    ; R[SHIFT]E, RE[SHIFT]T, RET[SHIFT]U or RETU[SHIFT]R.
                    ; RETUR[SHIFT]N however will not work because the [SHIFT]N
                    ; will match the RETURN end marker so the routine will try
                    ; to match the next character.

                    ; else found keyword
    ora COUNT       ; OR with word index, +$80 in .A makes token
LAB_C5C7:
    ldy FBUFPT      ; restore save index

; save byte then continue crunching

LAB_C5C9:
    inx             ; increment buffer read index
    INY             ; increment save index
    sta BUF-5,Y     ; save byte to output
    lda BUF-5,Y     ; get byte from output, set flags
    beq LAB_C609    ; branch if was null [EOL]

                    ; .A holds the token here
    sec             ; set carry for subtract
    sbc #':'        ; subtract ":"
    beq LAB_C5DC    ; branch if it was (is now $00)

                    ; .A now holds token-':'
    cmp #TK_DATA-':'        ; compare with the token for DATA-':'
    bne LAB_C5DE    ; if not DATA go try REM

                    ; token was : or DATA
LAB_C5DC:
    sta GARBFL      ; save token-':'
LAB_C5DE:
    sec             ; set carry for subtract
    sbc #TK_REM-':' ; subtract the token for REM-':'
    bne LAB_C582    ; if wasn't REM go crunch next bit of line

    sta ENDCHR      ; else was REM so set search for [EOL]

                    ; loop for "..." etc.
LAB_C5E5:
    lda BUF,X       ; get byte from input buffer
    beq LAB_C5C9    ; if null [EOL] save byte then continue crunching

    cmp ENDCHR      ; compare with stored character
    beq LAB_C5C9    ; if match save byte then continue crunching

LAB_C5EE:
    INY             ; increment save index
    sta BUF-5,Y     ; save byte to output
    inx             ; increment buffer index
    bne LAB_C5E5    ; loop while <> 0, should never reach 0

                    ; not found keyword this go
LAB_C5F5:
    ldx CHRGOT+1    ; restore BASIC execute pointer low byte
    inc COUNT       ; increment word index (next word)

                    ; now find end of this word in the table
LAB_C5F9:
    INY             ; increment table index
    lda RESLST-1,Y  ; get table byte
    bpl LAB_C5F9    ; loop if not end of word yet

    lda RESLST,Y    ; get byte from keyword table
    bne LAB_C5B8    ; go test next word if not zero byte, end of table

                    ; reached end of table with no match
    lda BUF,X       ; restore byte from input buffer
    bpl LAB_C5C7    ; branch always, all unmatched bytes in the buffer are
                    ; $00 to $7F, go save byte in output and continue crunching

                    ; reached [EOL]
LAB_C609:
    sta BUF-3,Y     ; save [EOL]
    dec CHRGOT+2    ; decrement BASIC execute pointer high byte
    lda #$FF        ; point to start of buffer - 1
    sta CHRGOT+1    ; set BASIC execute pointer low byte
    rts


;***********************************************************************************;
;
; search BASIC for temporary integer line number

FINLIN:
    lda TXTTAB      ; get start of memory low byte
    ldx TXTTAB+1    ; get start of memory high byte

; search BASIC for temp integer line number from .A.X
; returns carry set if found

LAB_C617:
    ldy #$01        ; set index to next line pointer high byte
    sta TMPPTR      ; save low byte as current
    stx TMPPTR+1    ; save high byte as current
    lda (TMPPTR),Y  ; get next line pointer high byte from address
    beq LAB_C640    ; pointer was zero so done, exit

    INY             ; increment index ...
    INY             ; ... to line # high byte
    lda LINNUM+1    ; get temporary integer high byte
    cmp (TMPPTR),Y  ; compare with line # high byte
    bcc LAB_C641    ; exit if temp < this line, target line passed

    beq LAB_C62E    ; go check low byte if =

    dey             ; else decrement index
    bne LAB_C637    ; branch always

LAB_C62E:
    lda LINNUM      ; get temporary integer low byte
    dey             ; decrement index to line # low byte
    cmp (TMPPTR),Y  ; compare with line # low byte
    bcc LAB_C641    ; exit if temp < this line, target line passed

    beq LAB_C641    ; exit if temp = (found line#)

                    ; not quite there yet
LAB_C637:
    dey             ; decrement index to next line pointer high byte
    lda (TMPPTR),Y  ; get next line pointer high byte
    tax             ; copy to .X
    dey             ; decrement index to next line pointer low byte
    lda (TMPPTR),Y  ; get next line pointer low byte
    bcs LAB_C617    ; go search for line # in temporary integer
                    ; from .A.X, carry always set

LAB_C640:
    clc             ; clear found flag
LAB_C641:
    rts


;***********************************************************************************;
;
; perform NEW

NEW:
    bne LAB_C641    ; exit if following byte to allow syntax error

LAB_C644:
    lda #$00        ; clear .A
    tay             ; clear index
    sta (TXTTAB),Y  ; clear pointer to next line low byte
    INY             ; increment index
    sta (TXTTAB),Y  ; clear pointer to next line high byte, erase program

    lda TXTTAB      ; get start of memory low byte
    clc             ; clear carry for add
    adc #$02        ; add null program length
    sta VARTAB      ; set start of variables low byte
    lda TXTTAB+1    ; get start of memory high byte
    adc #$00        ; add carry
    sta VARTAB+1    ; set start of variables high byte

; reset execute pointer and do CLR

LAB_C659:
    jsr STXTPT      ; set BASIC execute pointer to start of memory - 1
    lda #$00        ; set Zb for CLR entry


;***********************************************************************************;
;
; perform CLR

CLR:
    bne LAB_C68D    ; exit if following byte to allow syntax error

LAB_C660:
    jsr CLALL       ; close all channels and files
LAB_C663:
    lda MEMSIZ      ; get end of memory low byte
    ldy MEMSIZ+1    ; get end of memory high byte
    sta FRETOP      ; set bottom of string space low byte, clear strings
    sty FRETOP+1    ; set bottom of string space high byte
    lda VARTAB      ; get start of variables low byte
    ldy VARTAB+1    ; get start of variables high byte
    sta ARYTAB      ; set end of variables low byte, clear variables
    sty ARYTAB+1    ; set end of variables high byte
    sta STREND      ; set end of arrays low byte, clear arrays
    sty STREND+1    ; set end of arrays high byte


;***********************************************************************************;
;
; do RESTORE and clear the stack

LAB_C677:
    jsr RESTORE     ; perform RESTORE

; flush BASIC stack and clear the continue pointer

LAB_C67A:
    ldx #TEMPST     ; get descriptor stack start
    stx TEMPPT      ; set descriptor stack pointer
    pla             ; pull return address low byte
    tay             ; copy it
    pla             ; pull return address high byte
    ldx #$FA        ; set cleared stack pointer
    txs             ; set stack
    pha             ; push return address high byte
    tya             ; restore return address low byte
    pha             ; push return address low byte
    lda #$00        ; clear .A
    sta OLDTXT+1    ; clear continue pointer high byte
    sta SUBFLG      ; clear subscript/FNx flag
LAB_C68D:
    rts


;***********************************************************************************;
;
; set BASIC execute pointer to start of memory - 1

STXTPT:
    clc             ; clear carry for add
    lda TXTTAB      ; get start of memory low byte
    adc #$FF        ; add -1 low byte
    sta CHRGOT+1    ; set BASIC execute pointer low byte
    lda TXTTAB+1    ; get start of memory high byte
    adc #$FF        ; add -1 high byte
    sta CHRGOT+2    ; save BASIC execute pointer high byte
    rts


;***********************************************************************************;
;
; perform LIST

LIST:
    bcc LAB_C6A4    ; branch if next character not token (LIST n...)

    beq LAB_C6A4    ; branch if next character [NULL] (LIST)

    cmp #TK_MINUS   ; compare with token for -
    bne LAB_C68D    ; exit if not - (LIST -m)

                    ; LIST [[n][-m]]
                    ; this bit sets the n, if present, as the start and end
LAB_C6A4:
    jsr DECBIN      ; get fixed-point number into temporary integer
    jsr FINLIN      ; search BASIC for temporary integer line number
    jsr CHRGOT      ; scan memory
    beq LAB_C6BB    ; branch if no more chrs

                    ; this bit checks the - is present
    cmp #TK_MINUS   ; compare with "-"
    bne LAB_C641    ; return if not "-" (will be SN error)

                    ; LIST [n]-m
                    ; the - was there so set m as the end value
    jsr CHRGET      ; increment and scan memory
    jsr DECBIN      ; get fixed-point number into temporary integer
    bne LAB_C641    ; exit if not ok

LAB_C6BB:
    pla             ; dump return address low byte, exit via warm start
    pla             ; dump return address high byte
    lda LINNUM      ; get temporary integer low byte
    ora LINNUM+1    ; OR temporary integer high byte
    bne LAB_C6C9    ; branch if start set

    lda #$FF        ; set for -1
    sta LINNUM      ; set temporary integer low byte
    sta LINNUM+1    ; set temporary integer high byte
LAB_C6C9:
    ldy #$01        ; set index for line
    sty GARBFL      ; clear open quote flag
    lda (TMPPTR),Y  ; get next line pointer high byte
    beq LAB_C714    ; if null all done so exit

    jsr TSTSTOP     ; do STOP check vector
    jsr LAB_CAD7    ; print CR/LF
    INY             ; increment index for line
    lda (TMPPTR),Y  ; get line number low byte
    tax             ; copy to .X
    INY             ; increment index
    lda (TMPPTR),Y  ; get line number high byte
    cmp LINNUM+1    ; compare with temporary integer high byte
    bne LAB_C6E6    ; branch if no high byte match

    cpx LINNUM      ; compare with temporary integer low byte
    beq LAB_C6E8    ; branch if = last line to do, < will pass next branch

LAB_C6E6:           ; else ...
    bcs LAB_C714    ; if greater all done so exit

LAB_C6E8:
    sty FORPNT      ; save index for line
    jsr PRTFIX      ; print .X.A as unsigned integer
    lda #' '        ; space is the next character
LAB_C6EF:
    ldy FORPNT      ; get index for line
    and #$7F        ; mask top out bit of character
LAB_C6F3:
    jsr LAB_CB47    ; go print the character
    cmp #$22        ; was it " character
    bne LAB_C700    ; if not skip the quote handle

                    ; we are either entering or leaving a pair of quotes
    lda GARBFL      ; get open quote flag
    eor #$FF        ; toggle it
    sta GARBFL      ; save it back
LAB_C700:
    INY             ; increment index
    beq LAB_C714    ; line too long so just bail out and do a warm start

    lda (TMPPTR),Y  ; get next byte
    bne LAB_C717    ; if not [EOL] (go print character)

                    ; was [EOL]
    tay             ; else clear index
    lda (TMPPTR),Y  ; get next line pointer low byte
    tax             ; copy to .X
    INY             ; increment index
    lda (TMPPTR),Y  ; get next line pointer high byte
    stx TMPPTR      ; set pointer to line low byte
    sta TMPPTR+1    ; set pointer to line high byte
    bne LAB_C6C9    ; go do next line if not [EOT]
                    ; else ...
LAB_C714:
    jmp READY       ; do warm start


;***********************************************************************************;
;
LAB_C717:
    jmp (IQPLOP)    ; do uncrunch BASIC tokens


;***********************************************************************************;
;
; uncrunch BASIC tokens, the uncrunch BASIC tokens vector is initialised to point here

QPLOP:
    bpl LAB_C6F3    ; just go print it if not token byte
                    ; else was token byte so uncrunch it

    cmp #TK_PI      ; compare with the token for PI. in this case the token
                    ; is the same as the PI character so it just needs printing
    beq LAB_C6F3    ; just print it if so

    bit GARBFL      ; test the open quote flag
    bmi LAB_C6F3    ; just go print character if open quote set

    sec             ; else set carry for subtract
    sbc #$7F        ; reduce token range to 1 to whatever
    tax             ; copy token # to .X
    sty FORPNT      ; save index for line
    ldy #$FF        ; start from -1, adjust for pre increment
LAB_C72C:
    dex             ; decrement token #
    beq LAB_C737    ; if now found go do printing

LAB_C72F:
    INY             ; else increment index
    lda RESLST,Y    ; get byte from keyword table
    bpl LAB_C72F    ; loop until keyword end marker

    bmi LAB_C72C    ; go test if this is required keyword, branch always

                    ; found keyword, it's the next one
LAB_C737:
    INY             ; increment keyword table index
    lda RESLST,Y    ; get byte from table
    bmi LAB_C6EF    ; go restore index, mask byte and print if
                    ; byte was end marker

    jsr LAB_CB47    ; else go print the character
    bne LAB_C737    ; go get next character, branch always


;***********************************************************************************;
;
; perform FOR

FOR:
    lda #$80        ; set FNx
    sta SUBFLG      ; set subscript/FNx flag
    jsr LET         ; perform LET
    jsr SCNSTK      ; search the stack for FOR or GOSUB activity
    bne LAB_C753    ; branch if FOR, this variable, not found

                    ; FOR, this variable, was found so first we dump the old one
    txa             ; copy index
    adc #$0F        ; add FOR structure size-2
    tax             ; copy to index
    txs             ; set stack (dump FOR structure (-2 bytes))
LAB_C753:
    pla             ; pull return address
    pla             ; pull return address
    lda #$09        ; we need 18d bytes !
    jsr STKSPC      ; check room on stack for 2*.A bytes
    jsr FIND2       ; scan for next BASIC statement ([:] or [EOL])
    clc             ; clear carry for add
    tya             ; copy index to .A
    adc CHRGOT+1    ; add BASIC execute pointer low byte
    pha             ; push onto stack
    lda CHRGOT+2    ; get BASIC execute pointer high byte
    adc #$00        ; add carry
    pha             ; push onto stack
    lda CURLIN+1    ; get current line number high byte
    pha             ; push onto stack
    lda CURLIN      ; get current line number low byte
    pha             ; push onto stack
    lda #TK_TO      ; set "TO" token
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
    jsr LAB_CD8D    ; check if source is numeric, else do type mismatch
    jsr TYPCHK      ; evaluate expression and check is numeric, else do
                    ; type mismatch
    lda FAC1+FAC_SIGN       ; get FAC1 sign (b7)
    ora #$7F        ; set all non sign bits
    and FAC1+FAC_MANT       ; and FAC1 mantissa 1
    sta FAC1+FAC_MANT       ; save FAC1 mantissa 1
    lda #<LAB_C78B  ; set return address low byte
    ldy #>LAB_C78B  ; set return address high byte
    sta INDEX       ; save return address low byte
    sty INDEX+1     ; save return address high byte
    jmp LAB_CE43    ; round FAC1 and put on stack, returns to next instruction

LAB_C78B:
    lda #<FPC1      ; set 1 pointer low address, default step size
    ldy #>FPC1      ; set 1 pointer high address
    jsr LODFAC      ; unpack memory (.A.Y) into FAC1
    jsr CHRGOT      ; scan memory
    cmp #TK_STEP    ; compare with STEP token
    bne LAB_C79F    ; branch if not "STEP"

                    ; was step so ...
    jsr CHRGET      ; increment and scan memory
    jsr TYPCHK      ; evaluate expression and check is numeric, else do
                    ; type mismatch
LAB_C79F:
    jsr SGNFAC      ; get FAC1 sign, return .A = $FF -ve, .A = $01 +ve
    jsr LAB_CE38    ; push sign, round FAC1 and put on stack
    lda FORPNT+1    ; get FOR/NEXT variable pointer high byte
    pha             ; push on stack
    lda FORPNT      ; get FOR/NEXT variable pointer low byte
    pha             ; push on stack
    lda #TK_FOR     ; get FOR token
    pha             ; push on stack


;***********************************************************************************;
;
; interpreter inner loop

NEWSTT:
    jsr TSTSTOP         ; do STOP check vector
    lda CHRGOT+1        ; get BASIC execute pointer low byte
    ldy CHRGOT+2        ; get BASIC execute pointer high byte
    cpy #$02            ; compare with $02xx
    NOP                 ; unused byte                           ##
    beq LAB_C7BE        ; if immediate mode skip the continue pointer save

    sta OLDTXT          ; save the continue pointer low byte
    sty OLDTXT+1        ; save the continue pointer high byte
LAB_C7BE:
    ldy #$00            ; clear the index
    lda (CHRGOT+1),Y    ; get BASIC byte
    bne LAB_C807        ; if not [EOL] go test for ":"

    ldy #$02            ; else set the index
    lda (CHRGOT+1),Y    ; get next line pointer high byte
    clc                 ; clear carry for no "BREAK" message
    bne LAB_C7CE        ; branch if not end of program

    jmp LAB_C84B        ; else go to immediate mode,was immediate or [EOT] marker

LAB_C7CE:
    INY                 ; increment index
    lda (CHRGOT+1),Y    ; get line number low byte
    sta CURLIN          ; save current line number low byte
    INY                 ; increment index
    lda (CHRGOT+1),Y    ; get line # high byte
    sta CURLIN+1        ; save current line number high byte
    tya                 ; .A now = 4
    adc CHRGOT+1        ; add BASIC execute pointer low byte, now points to code
    sta CHRGOT+1        ; save BASIC execute pointer low byte
    bcc LAB_C7E1        ; if no overflow skip the high byte increment

    inc CHRGOT+2        ; else increment BASIC execute pointer high byte
LAB_C7E1:
    jmp (IGONE)         ; do start new BASIC code


;***********************************************************************************;
;
; start new BASIC code, the start new BASIC code vector is initialised to point here

GONE:
    jsr CHRGET      ; increment and scan memory
    jsr LAB_C7ED    ; go interpret BASIC code from BASIC execute pointer
    jmp NEWSTT      ; loop


;***********************************************************************************;
;
; go interpret BASIC code from BASIC execute pointer

LAB_C7ED:
    beq LAB_C82B    ; if the first byte is null just exit

LAB_C7EF:
    sbc #$80        ; normalise the token
    bcc LAB_C804    ; if wasn't token go do LET

    cmp #TK_TAB-$80 ; compare with token for TAB(-$80
    bcs LAB_C80E    ; branch if >= TAB(

    asl             ; *2 bytes per vector
    tay             ; copy to index
    lda STMDSP+1,Y  ; get vector high byte
    pha             ; push on stack
    lda STMDSP,Y    ; get vector low byte
    pha             ; push on stack
    jmp CHRGET      ; increment and scan memory and return. the return in
                    ; this case calls the command code, the return from
                    ; that will eventually return to the interpreter inner
                    ; loop above

LAB_C804:
    jmp LET         ; perform LET

                    ; was not [EOL]
LAB_C807:
    cmp #':'        ; compare with ":"
    beq LAB_C7E1    ; if ":" go execute new code

                    ; else ...
LAB_C80B:
    jmp LAB_CF08    ; do syntax error then warm start

                    ; token was >= TAB(
LAB_C80E:
    cmp #TK_GO-$80  ; compare with token for GO
    bne LAB_C80B    ; if not "GO" do syntax error then warm start

                    ; else was "GO"
    jsr CHRGET      ; increment and scan memory
    lda #TK_TO      ; set "TO" token
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
    jmp GOTO        ; perform GOTO


;***********************************************************************************;
;
; perform RESTORE

RESTORE:
    sec             ; set carry for subtract
    lda TXTTAB      ; get start of memory low byte
    sbc #$01        ; -1
    ldy TXTTAB+1    ; get start of memory high byte
    bcs LAB_C827    ; if no rollunder skip the high byte decrement

    dey             ; else decrement high byte
LAB_C827:
    sta DATPTR      ; set DATA pointer low byte
    sty DATPTR+1    ; set DATA pointer high byte
LAB_C82B:
    rts


;***********************************************************************************;
;
; do STOP check vector

TSTSTOP:
    jsr STOP        ; scan stop key


;***********************************************************************************;
;
; perform STOP

BSTOP:
    bcs LAB_C832    ; if carry set do BREAK instead of just END


;***********************************************************************************;
;
; perform END

END:
    clc             ; clear carry
LAB_C832:
    bne LAB_C870    ; return if wasn't STOP

    lda CHRGOT+1    ; get BASIC execute pointer low byte
    ldy CHRGOT+2    ; get BASIC execute pointer high byte
    ldx CURLIN+1    ; get current line number high byte
    inx             ; increment it
    beq LAB_C849    ; branch if was immediate mode

    sta OLDTXT      ; save continue pointer low byte
    sty OLDTXT+1    ; save continue pointer high byte
    lda CURLIN      ; get current line number low byte
    ldy CURLIN+1    ; get current line number high byte
    sta OLDLIN      ; save break line number low byte
    sty OLDLIN+1    ; save break line number high byte
LAB_C849:
    pla             ; dump return address low byte
    pla             ; dump return address high byte
LAB_C84B:
    lda #<CRLFBRK   ; set [CR][LF]"BREAK" pointer low byte
    ldy #>CRLFBRK   ; set [CR][LF]"BREAK" pointer high byte
    bcc LAB_C854    ; branch if was program end

    jmp PRDY        ; print string and do warm start

LAB_C854:
    jmp READY       ; do warm start


;***********************************************************************************;
;
; perform CONT

CONT:
    bne LAB_C870    ; exit if following byte to allow syntax error

    ldx #ER_CANTCONT    ; error code $1A, can't continue error
    ldy OLDTXT+1    ; get continue pointer high byte
    bne LAB_C862    ; go do continue if we can

    jmp ERROR       ; else do error #.X then warm start

                    ; we can continue so ...
LAB_C862:
    lda OLDTXT      ; get continue pointer low byte
    sta CHRGOT+1    ; save BASIC execute pointer low byte
    sty CHRGOT+2    ; save BASIC execute pointer high byte
    lda OLDLIN      ; get break line low byte
    ldy OLDLIN+1    ; get break line high byte
    sta CURLIN      ; set current line number low byte
    sty CURLIN+1    ; set current line number high byte
LAB_C870:
    rts


;***********************************************************************************;
;
; perform RUN

RUN:
    php             ; save status
    lda #$00        ; no control or KERNAL messages
    jsr SETMSG      ; control KERNAL messages
    plp             ; restore status
    bne LAB_C87D    ; branch if RUN n

    jmp LAB_C659    ; reset execution to start, clear variables, flush stack
                    ; and return
LAB_C87D:
    jsr LAB_C660    ; go do CLR
    jmp LAB_C897    ; get n and do GOTO n


;***********************************************************************************;
;
; perform GOSUB

GOSUB:
    lda #$03        ; need 6 bytes for GOSUB
    jsr STKSPC      ; check room on stack for 2*.A bytes
    lda CHRGOT+2    ; get BASIC execute pointer high byte
    pha             ; save it
    lda CHRGOT+1    ; get BASIC execute pointer low byte
    pha             ; save it
    lda CURLIN+1    ; get current line number high byte
    pha             ; save it
    lda CURLIN      ; get current line number low byte
    pha             ; save it
    lda #TK_GOSUB   ; token for GOSUB
    pha             ; save it
LAB_C897:
    jsr CHRGOT      ; scan memory
    jsr GOTO        ; perform GOTO
    jmp NEWSTT      ; go do interpreter inner loop


;***********************************************************************************;
;
; perform GOTO

GOTO:
    jsr DECBIN      ; get fixed-point number into temporary integer
    jsr LAB_C909    ; scan for next BASIC line
    sec             ; set carry for subtract
    lda CURLIN      ; get current line number low byte
    sbc LINNUM      ; subtract temporary integer low byte
    lda CURLIN+1    ; get current line number high byte
    sbc LINNUM+1    ; subtract temporary integer high byte
    bcs LAB_C8BC    ; if current line number >= temporary integer, go search
                    ; from the start of memory

    tya             ; else copy line index to .A
    sec             ; set carry (+1)
    adc CHRGOT+1    ; add BASIC execute pointer low byte
    ldx CHRGOT+2    ; get BASIC execute pointer high byte
    bcc LAB_C8C0    ; if no overflow skip the high byte increment

    inx             ; increment high byte
    bcs LAB_C8C0    ; go find the line, branch always


;***********************************************************************************;
;
; search for line number in temporary integer from start of memory pointer

LAB_C8BC:
    lda TXTTAB      ; get start of memory low byte
    ldx TXTTAB+1    ; get start of memory high byte

; search for line # in temporary integer from (.A.X)

LAB_C8C0:
    jsr LAB_C617    ; search BASIC for temp integer line number from .A.X
    bcc LAB_C8E3    ; if carry clear go do undefined statement error

                    ; carry all ready set for subtract
    lda TMPPTR      ; get pointer low byte
    sbc #$01        ; -1
    sta CHRGOT+1    ; save BASIC execute pointer low byte
    lda TMPPTR+1    ; get pointer high byte
    sbc #$00        ; subtract carry
    sta CHRGOT+2    ; save BASIC execute pointer high byte
LAB_C8D1:
    rts


;***********************************************************************************;
;
; perform RETURN

RETURN:
    bne LAB_C8D1    ; exit if following token to allow syntax error

    lda #$FF        ; set byte so no match possible
    sta FORPNT+1    ; save FOR/NEXT variable pointer high byte
    jsr SCNSTK      ; search the stack for FOR or GOSUB activity,
                    ; get token off stack
    txs             ; correct the stack
    cmp #TK_GOSUB   ; compare with GOSUB token
    beq LAB_C8EB    ; if matching GOSUB go continue RETURN

    ldx #ER_RETWOGSB    ; else error code $0C, return without gosub error
    .byte   $2C     ; makes next line bit $11A2
LAB_C8E3:
    ldx #ER_UNDSMNT ; error code $11, undefined statement error
    jmp ERROR       ; do error #.X then warm start

LAB_C8E8:
    jmp LAB_CF08    ; do syntax error then warm start

                    ; was matching GOSUB token
LAB_C8EB:
    pla             ; dump token byte
    pla             ; pull return line low byte
    sta CURLIN      ; save current line number low byte
    pla             ; pull return line high byte
    sta CURLIN+1    ; save current line number high byte
    pla             ; pull return address low byte
    sta CHRGOT+1    ; save BASIC execute pointer low byte
    pla             ; pull return address high byte
    sta CHRGOT+2    ; save BASIC execute pointer high byte


;***********************************************************************************;
;
; perform DATA

SKIPST:
    jsr FIND2       ; scan for next BASIC statement ([:] or [EOL])

; add .Y to the BASIC execute pointer

BUMPTP:
    tya             ; copy index to .A
    clc             ; clear carry for add
    adc CHRGOT+1    ; add BASIC execute pointer low byte
    sta CHRGOT+1    ; save BASIC execute pointer low byte
    bcc LAB_C905    ; skip increment if no carry

    inc CHRGOT+2    ; else increment BASIC execute pointer high byte
LAB_C905:
    rts


;***********************************************************************************;
;
; scan for next BASIC statement ([:] or [EOL])
; returns .Y as index to [:] or [EOL]

FIND2:
    ldx #':'        ; set look for character = ":"
    .byte   $2C     ; makes next line bit $00A2

; scan for next BASIC line
; returns .Y as index to [EOL]

LAB_C909:
    ldx #$00        ; set alternate search character = [EOL]
    stx CHARAC      ; store alternate search character
    ldy #$00        ; set search character = [EOL]
    sty ENDCHR      ; save the search character
LAB_C911:
    lda ENDCHR      ; get search character
    ldx CHARAC      ; get alternate search character
    sta CHARAC      ; make search character = alternate search character
    stx ENDCHR      ; make alternate search character = search character
LAB_C919:
    lda (CHRGOT+1),Y    ; get BASIC byte
    beq LAB_C905    ; exit if null [EOL]

    cmp ENDCHR      ; compare with search character
    beq LAB_C905    ; exit if found

    INY             ; else increment index
    cmp #$22        ; compare current character with open quote
    bne LAB_C919    ; if found go swap search character for alternate search
                    ; character

    beq LAB_C911    ; loop for next character, branch always


;***********************************************************************************;
;
; perform IF

IF:
    jsr FRMEVL      ; evaluate expression
    jsr CHRGOT      ; scan memory
    cmp #TK_GOTO    ; compare with "GOTO" token
    beq LAB_C937    ; if it was the token for GOTO go do IF ... GOTO

                    ; wasn't IF ... GOTO so must be IF ... THEN
    lda #TK_THEN    ; $A7 = "THEN" token
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
LAB_C937:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    bne LAB_C940    ; if result was non zero continue execution
                    ; else REM the rest of the line


;***********************************************************************************;
;
; perform REM

REM:
    jsr LAB_C909    ; scan for next BASIC line
    beq BUMPTP      ; add .Y to the BASIC execute pointer and return, branch
                    ; always


;***********************************************************************************;
;
; IF continued .. result was non zero so do rest of line

LAB_C940:
    jsr CHRGOT      ; scan memory
    bcs LAB_C948    ; if not numeric character, is variable or keyword

    jmp GOTO        ; else perform GOTO n

                    ; is variable or keyword
LAB_C948:
    jmp LAB_C7ED    ; interpret BASIC code from BASIC execute pointer


;***********************************************************************************;
;
; perform ON

ON:
    jsr LAB_D79E    ; get byte parameter
    pha             ; push next character
    cmp #TK_GOSUB   ; compare with GOSUB token
    beq LAB_C957    ; if GOSUB go see if it should be executed

LAB_C953:
    cmp #TK_GOTO    ; compare with GOTO token
    bne LAB_C8E8    ; if not GOTO do syntax error then warm start

; next character was GOTO or GOSUB, see if it should be executed

LAB_C957:
    dec FAC1+4      ; decrement the byte value
    bne LAB_C95F    ; if not zero go see if another line number exists

    pla             ; pull keyword token
    jmp LAB_C7EF    ; go execute it

LAB_C95F:
    jsr CHRGET      ; increment and scan memory
    jsr DECBIN      ; get fixed-point number into temporary integer
                    ; skip this n
    cmp #','        ; compare next character with ","
    beq LAB_C957    ; loop if ","

    pla             ; else pull keyword token, ran out of options
LAB_C96A:
    rts


;***********************************************************************************;
;
; get fixed-point number into temporary integer

DECBIN:
    ldx #$00        ; clear .X
    stx LINNUM      ; clear temporary integer low byte
    stx LINNUM+1    ; clear temporary integer high byte
LAB_C971:
    bcs LAB_C96A    ; return if carry set, end of scan, character was not 0-9

    sbc #$2F        ; subtract $30, $2F+carry, from byte
    sta CHARAC      ; store #
    lda LINNUM+1    ; get temporary integer high byte
    sta INDEX       ; save it for now
    cmp #$19        ; compare with $19
    bcs LAB_C953    ; branch if >= this makes the maximum line number 63999
                    ; because the next bit does $1900 * $0A = $FA00 = 64000
                    ; decimal. the branch target is really the SYNtax error
                    ; at LAB_C8E8 but that is too far so an intermediate
                    ; compare and branch to that location is used. the problem
                    ; with this is that line number that gives a partial result
                    ; from $8900 to $89FF, 35072x to 35327x, will pass the new
                    ; target compare and will try to execute the remainder of
                    ; the ON n GOTO/GOSUB. a solution to this is to copy the
                    ; byte in .A before the branch to .X and then branch to
                    ; LAB_C955 skipping the second compare

    lda LINNUM      ; get temporary integer low byte
    asl             ; *2 low byte
    ROL INDEX       ; *2 high byte
    asl             ; *2 low byte
    ROL INDEX       ; *2 high byte (*4)
    adc LINNUM      ; + low byte (*5)
    sta LINNUM      ; save it
    lda INDEX       ; get high byte temp
    adc LINNUM+1    ; + high byte (*5)
    sta LINNUM+1    ; save it
    asl LINNUM      ; *2 low byte (*10d)
    ROL LINNUM+1    ; *2 high byte (*10d)
    lda LINNUM      ; get low byte
    adc CHARAC      ; add #
    sta LINNUM      ; save low byte
    bcc LAB_C99F    ; if no overflow skip high byte increment

    inc LINNUM+1    ; else increment high byte
LAB_C99F:
    jsr CHRGET      ; increment and scan memory
    jmp LAB_C971    ; loop for next character


;***********************************************************************************;
;
; perform LET

LET:
    jsr EVLVAR      ; get variable address
    sta FORPNT      ; save variable address low byte
    sty FORPNT+1    ; save variable address high byte
    lda #TK_EQUAL   ; $B2 is "=" token
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
    lda INTFLG      ; get data type flag, $80 = integer, $00 = float
    pha             ; push data type flag
    lda VALTYP      ; get data type flag, $FF = string, $00 = numeric
    pha             ; push data type flag
    jsr FRMEVL      ; evaluate expression
    pla             ; pop data type flag
    ROL             ; string bit into carry
    jsr LAB_CD90    ; do type match check
    bne LAB_C9D9    ; if string go assign a string value

    pla             ; pop integer/float data type flag

; assign value to numeric variable

LET2:
    bpl LAB_C9D6    ; if float go assign a floating value

                    ; expression is numeric integer
    jsr ROUND       ; round FAC1
    jsr MAKINT      ; evaluate integer expression, no sign check
    ldy #$00        ; clear index
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    sta (FORPNT),Y  ; save as integer variable low byte
    INY             ; increment index
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    sta (FORPNT),Y  ; save as integer variable high byte
    rts

LAB_C9D6:
    jmp FACTFP      ; pack FAC1 into variable pointer and return

; assign value to string variable

LAB_C9D9:
    pla             ; dump integer/float data type flag
LET5:
    ldy FORPNT+1    ; get variable pointer high byte
    cpy #>NULLVAR   ; was it TI$ pointer
    bne LET9        ; branch if not

                    ; else it's TI$ = <expr$>
    jsr LAB_D6A6    ; pop string off descriptor stack, or from top of string
                    ; space returns with .A = length, .X = pointer low byte,
                    ; .Y = pointer high byte
    cmp #$06        ; compare length with 6
    bne LAB_CA24    ; if length not 6 do illegal quantity error then warm start

    ldy #$00        ; clear index
    sty FAC1+FAC_EXPT   ; clear FAC1 exponent
    sty FAC1+FAC_SIGN   ; clear FAC1 sign (b7)
LAB_C9ED:
    sty FBUFPT      ; save index
    jsr LAB_CA1D    ; check and evaluate numeric digit
    jsr MULTEN      ; multiply FAC1 by 10
    inc FBUFPT      ; increment index
    ldy FBUFPT      ; restore index
    jsr LAB_CA1D    ; check and evaluate numeric digit
    jsr RFTOA       ; round and copy FAC1 to FAC2
    tax             ; copy FAC1 exponent
    beq LAB_CA07    ; branch if FAC1 zero

    inx             ; increment index, * 2
    txa             ; copy back to .A
    jsr LAB_DAED    ; FAC1 = (FAC1 + (FAC2 * 2)) * 2 = FAC1 * 6
LAB_CA07:
    ldy FBUFPT      ; get index
    INY             ; increment index
    cpy #$06        ; compare index with 6
    bne LAB_C9ED    ; loop if not 6

    jsr MULTEN      ; multiply FAC1 by 10
    jsr FPINT       ; convert FAC1 floating to fixed
    ldx FAC1+FAC_MANT+2     ; get FAC1 mantissa 3
    ldy FAC1+FAC_MANT+1     ; get FAC1 mantissa 2
    lda FAC1+FAC_MANT+3     ; get FAC1 mantissa 4
    jmp SETTIM      ; set real time clock and return

; check and evaluate numeric digit

LAB_CA1D:
    lda (INDEX),Y   ; get byte from string
    jsr CHRSPC      ; clear Cb if numeric. this call should be to CHRSPC+4
                    ; as the code from CHRSPC first compares the byte with
                    ; [SPACE] and does a BASIC increment and get if it is
    bcc LAB_CA27    ; branch if numeric

LAB_CA24:
    jmp ILQUAN      ; do illegal quantity error then warm start

LAB_CA27:
    sbc #$2F        ; subtract $2F + carry to convert ASCII to binary
    jmp ASCI8       ; evaluate new ASCII digit and return

; assign value to string variable, but not TI$

LET9:
    ldy #$02        ; index to string pointer high byte
    lda (FAC1+3),Y  ; get string pointer high byte
    cmp FRETOP+1    ; compare with bottom of string space high byte
    bcc LAB_CA4B    ; branch if string pointer high byte is less than bottom
                    ; of string space high byte

    bne LAB_CA3D    ; branch if string pointer high byte is greater than
                    ; bottom of string space high byte

                    ; else high bytes were equal
    dey             ; decrement index to string pointer low byte
    lda (FAC1+3),Y  ; get string pointer low byte
    cmp FRETOP      ; compare with bottom of string space low byte
    bcc LAB_CA4B    ; branch if string pointer low byte is less than bottom
                    ; of string space low byte

LAB_CA3D:
    ldy FAC1+4      ; get descriptor pointer high byte
    cpy VARTAB+1    ; compare with start of variables high byte
    bcc LAB_CA4B    ; branch if less, is on string stack

    bne LAB_CA52    ; if greater make space and copy string

                    ; else high bytes were equal
    lda FAC1+3      ; get descriptor pointer low byte
    cmp VARTAB      ; compare with start of variables low byte
    bcs LAB_CA52    ; if greater or equal make space and copy string

LAB_CA4B:
    lda FAC1+3      ; get descriptor pointer low byte
    ldy FAC1+4      ; get descriptor pointer high byte
    jmp LAB_CA68    ; go copy descriptor to variable

LAB_CA52:
    ldy #$00        ; clear index
    lda (FAC1+3),Y  ; get string length
    jsr ALC1        ; copy descriptor pointer and make string space .A bytes long
    lda DSCPTN      ; copy old descriptor pointer low byte
    ldy DSCPTN+1    ; copy old descriptor pointer high byte
    sta ARISGN      ; save old descriptor pointer low byte
    sty FACOV       ; save old descriptor pointer high byte
    jsr XFERSTR     ; copy string from descriptor to utility pointer
    lda #<FAC1      ; get descriptor pointer low byte
    ldy #>FAC1      ; get descriptor pointer high byte
LAB_CA68:
    sta DSCPTN      ; save descriptor pointer low byte
    sty DSCPTN+1    ; save descriptor pointer high byte
    jsr DELTSD      ; clean descriptor stack, .Y.A = pointer
    ldy #$00        ; clear index
    lda (DSCPTN),Y  ; get string length from new descriptor
    sta (FORPNT),Y  ; copy string length to variable
    INY             ; increment index
    lda (DSCPTN),Y  ; get string pointer low byte from new descriptor
    sta    (FORPNT),Y   ; copy string pointer low byte to variable
    INY             ; increment index
    lda (DSCPTN),Y  ; get string pointer high byte from new descriptor
    sta (FORPNT),Y  ; copy string pointer high byte to variable
    rts


;***********************************************************************************;
;
; perform PRINT#

PRINTN:
    jsr CMD         ; perform CMD
    jmp LAB_CBB5    ; close input and output channels and return


;***********************************************************************************;
;
; perform CMD

CMD:
    jsr LAB_D79E    ; get byte parameter
    beq LAB_CA90    ; branch if following byte is ":" or [EOT]

    lda #','        ; set ","
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
LAB_CA90:
    php             ; save status
    stx CHANNL      ; set current I/O channel
    jsr LAB_E115    ; open channel for output with error check
    plp             ; restore status
    jmp PRINT       ; perform PRINT


;***********************************************************************************;
;
; print string, scan memory and continue PRINT

PRT1:
    jsr LAB_CB21    ; print string from utility pointer

; scan memory and continue PRINT

LAB_CA9D:
    jsr CHRGOT      ; scan memory


;***********************************************************************************;
;
; perform PRINT

PRINT:
    beq LAB_CAD7    ; if nothing following just print CR/LF

LAB_CAA2:
    beq LAB_CAE7    ; if nothing following exit, end of PRINT branch

    cmp #TK_TAB     ; compare with token for TAB(
    beq PRT7        ; if TAB( go handle it

    cmp #TK_SPC     ; compare with token for SPC(
    clc             ; flag SPC(
    beq PRT7        ; if SPC( go handle it

    cmp #','        ; compare with ","
    beq PRT6        ; if "," go skip to the next TAB position

    cmp #$3B        ; compare with ";"
    beq LAB_CB13    ; if ";" go continue the print loop

    jsr FRMEVL      ; evaluate expression
    bit VALTYP      ; test data type flag, $FF = string, $00 = numeric
    bmi PRT1        ; if string go print string, scan memory and continue PRINT

    jsr FLTASC      ; convert FAC1 to ASCII string result in (.A.Y)
    jsr MAKSTR      ; print " terminated string to utility pointer
    jsr LAB_CB21    ; print string from utility pointer
    jsr PRTOS       ; print [SPACE] or [CURSOR RIGHT]
    bne LAB_CA9D    ; go scan memory and continue PRINT, branch always


;***********************************************************************************;
;
; set .X.Y to BUF - 1 and print [CR]

LAB_CACA:
    lda #$00        ; clear .A
    sta BUF,X       ; clear first byte of input buffer
    ldx #<(BUF-1)   ; BUF - 1 low byte
    ldy #>(BUF-1)   ; BUF - 1 high byte
    lda CHANNL      ; get current I/O channel
    bne LAB_CAE7    ; exit if not default channel


;***********************************************************************************;
;
; print CR/LF

LAB_CAD7:
    lda #$0D        ; set [CR]
    jsr LAB_CB47    ; print the character
    bit CHANNL      ; test current I/O channel
    bpl LAB_CAE5    ; if the AutoLF bit is not set skip the LF
    lda #$0A        ; set [LF]
    jsr LAB_CB47    ; print the character


;***********************************************************************************;
;
; invert .A

LAB_CAE5:
    eor #$FF        ; ones complement .A
LAB_CAE7:
    rts


;***********************************************************************************;
;
; continuing PRINT, the character was ","

PRT6:
    sec             ; set Cb for read cursor position
    jsr PLOT        ; read/set X,Y cursor position
    tya             ; copy cursor .Y
    sec             ; set carry for subtract
LAB_CAEE:
    sbc #$0B        ; subtract one TAB length
    bcs LAB_CAEE    ; loop if result was +ve

    eor #$FF        ; complement it
    adc #$01        ; +1, twos complement
    bne LAB_CB0E    ; print .A spaces, branch always, result is never $00


;***********************************************************************************;
;
; handle TAB( or SPC(

PRT7:
    php             ; save TAB( or SPC( status
    sec             ; set Cb for read cursor position
    jsr PLOT        ; read/set X,Y cursor position
    sty TRMPOS      ; save current cursor position
    jsr GETBYT      ; scan and get byte parameter
    cmp #$29        ; compare with ")"
    bne LAB_CB5F    ; if not ")" do syntax error

    plp             ; restore TAB( or SPC( status
    bcc LAB_CB0F    ; branch if was SPC(

                    ; else was TAB(
    txa             ; copy TAB() byte to .A
    sbc TRMPOS      ; subtract current cursor position
    bcc LAB_CB13    ; go loop for next if already past required position

LAB_CB0E:
    tax             ; copy [SPACE] count to .X
LAB_CB0F:
    inx             ; increment count
LAB_CB10:
    dex             ; decrement count
    bne LAB_CB19    ; branch if count was not zero

                    ; was ";" or [SPACES] printed
LAB_CB13:
    jsr CHRGET      ; increment and scan memory
    jmp LAB_CAA2    ; continue print loop

LAB_CB19:
    jsr PRTOS       ; print [SPACE] or [CURSOR RIGHT]
    bne LAB_CB10    ; loop, branch always


;***********************************************************************************;
;
; print null terminated string

PRTSTR:
    jsr MAKSTR      ; print " terminated string to utility pointer

; print string from utility pointer

LAB_CB21:
    jsr LAB_D6A6    ; pop string off descriptor stack, or from top of string
                    ; space returns with .A = length, .X = pointer low byte,
                    ; .Y = pointer high byte
    tax             ; copy length
    ldy #$00        ; clear index
    inx             ; increment length, for pre decrement loop
LAB_CB28:
    dex             ; decrement length
    beq LAB_CAE7    ; exit if done

    lda (INDEX),Y   ; get byte from string
    jsr LAB_CB47    ; print the character
    INY             ; increment index
    cmp #$0D        ; compare byte with [CR]
    bne LAB_CB28    ; loop if not [CR]

    jsr LAB_CAE5    ; toggle .A, eor #$FF. what is the point of this ??
    jmp LAB_CB28    ; loop


;***********************************************************************************;
;
; print [SPACE] or [CURSOR RIGHT]

PRTOS:
    lda CHANNL      ; get current I/O channel
    beq LAB_CB42    ; if default channel go output [CURSOR RIGHT]

    lda #' '        ; else output [SPACE]
    .byte   $2C     ; makes next line bit $1DA9
LAB_CB42:
    lda #$1D        ; set [CURSOR RIGHT]
    .byte   $2C     ; makes next line bit $3FA9


;***********************************************************************************;
;
; print "?"

LAB_CB45:
    lda #'?'        ; set "?"


;***********************************************************************************;
;
; print a character

LAB_CB47:
    jsr LAB_E109    ; output character to channel with error check
    and #$FF        ; set the flags on .A
    rts


;***********************************************************************************;
;
; bad input routine

IGRERR:
    lda INPFLG      ; get INPUT mode flag, $00 = INPUT, $40 = GET, $98 = READ
    beq LAB_CB62    ; branch if INPUT

    bmi LAB_CB57    ; branch if READ

                    ; else was GET
    ldy #$FF        ; set current line high byte to -1, indicate immediate mode
    bne LAB_CB5B    ; branch always

LAB_CB57:
    lda DATLIN      ; get current DATA line number low byte
    ldy DATLIN+1    ; get current DATA line number high byte
LAB_CB5B:
    sta CURLIN      ; set current line number low byte
    sty CURLIN+1    ; set current line number high byte
LAB_CB5F:
    jmp LAB_CF08    ; do syntax error then warm start

                    ; was INPUT
LAB_CB62:
    lda CHANNL      ; get current I/O channel
    beq LAB_CB6B    ; if default channel go do "?REDO FROM START" message

    ldx #ER_FDATA   ; else error $18, file data error
    jmp ERROR       ; do error #.X then warm start

LAB_CB6B:
    lda #<LAB_CD0C  ; set "?REDO FROM START" pointer low byte
    ldy #>LAB_CD0C  ; set "?REDO FROM START" pointer high byte
    jsr PRTSTR      ; print null terminated string
    lda OLDTXT      ; get continue pointer low byte
    ldy OLDTXT+1    ; get continue pointer high byte
    sta CHRGOT+1    ; save BASIC execute pointer low byte
    sty CHRGOT+2    ; save BASIC execute pointer high byte
    rts


;***********************************************************************************;
;
; perform GET

GET:
    jsr NODIRM      ; check not Direct, back here if ok
    cmp #'#'        ; compare with "#"
    bne LAB_CB92    ; branch if not GET#

    jsr CHRGET      ; increment and scan memory
    jsr LAB_D79E    ; get byte parameter
    lda #','        ; set ","
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
    stx CHANNL      ; set current I/O channel
    jsr LAB_E11B    ; open channel for input with error check
LAB_CB92:
    ldx #<(BUF+1)   ; set BUF+1 pointer low byte
    ldy #>(BUF+1)   ; set BUF+1 pointer high byte
    lda #$00        ; clear .A
    sta BUF+1       ; ensure null terminator
    lda #$40        ; input mode = GET
    jsr LAB_CC0F    ; perform GET part of READ
    ldx CHANNL      ; get current I/O channel
    bne LAB_CBB7    ; if not default channel go do channel close and return

    rts


;***********************************************************************************;
;
; perform INPUT#

INPUTN:
    jsr LAB_D79E    ; get byte parameter
    lda #','        ; set ","
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
    stx CHANNL      ; set current I/O channel
    jsr LAB_E11B    ; open channel for input with error check
    jsr LAB_CBCE    ; perform INPUT with no prompt string

; close input and output channels

LAB_CBB5:
    lda CHANNL      ; get current I/O channel
LAB_CBB7:
    jsr CLRCHN      ; close input and output channels
    ldx #$00        ; clear .X
    stx CHANNL      ; clear current I/O channel, flag default
    rts


;***********************************************************************************;
;
; perform INPUT

INPUT:
    cmp #$22        ; compare next byte with open quote
    bne LAB_CBCE    ; if no prompt string just do INPUT

    jsr LAB_CEBD    ; print "..." string
    lda #$3B        ; load .A with ";"
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
    jsr LAB_CB21    ; print string from utility pointer

                    ; done with prompt, now get data
LAB_CBCE:
    jsr NODIRM      ; check not Direct, back here if ok
    lda #','        ; set ","
    sta BUF-1       ; save to start of buffer - 1
LAB_CBD6:
    jsr LAB_CBF9    ; print "? " and get BASIC input
    lda CHANNL      ; get current I/O channel
    beq LAB_CBEA    ; branch if default I/O channel

    jsr READST      ; read I/O status word
    and #$02        ; mask no DSR/timeout
    beq LAB_CBEA    ; branch if not error

    jsr LAB_CBB5    ; close input and output channels
    jmp SKIPST      ; perform DATA

LAB_CBEA:
    lda BUF         ; get first byte in input buffer
    bne LAB_CC0D    ; branch if not null

                    ; else ..
    lda CHANNL      ; get current I/O channel
    bne LAB_CBD6    ; if not default channel go get BASIC input

    jsr FIND2       ; scan for next BASIC statement ([:] or [EOL])
    jmp BUMPTP      ; add .Y to the BASIC execute pointer and return


;***********************************************************************************;
;
; print "? " and get BASIC input

LAB_CBF9:
    lda CHANNL      ; get current I/O channel
    bne LAB_CC03    ; skip "?" prompt if not default channel

    jsr LAB_CB45    ; print "?"
    jsr PRTOS       ; print [SPACE] or [CURSOR RIGHT]
LAB_CC03:
    jmp GETLIN      ; call for BASIC input and return


;***********************************************************************************;
;
; perform READ

READ:
    ldx DATPTR      ; get DATA pointer low byte
    ldy DATPTR+1    ; get DATA pointer high byte
    lda #$98        ; set input mode = READ
    .byte   $2C     ; makes next line bit $00A9
LAB_CC0D:
    lda #$00        ; set input mode = INPUT


;***********************************************************************************;
;
; perform GET

LAB_CC0F:
    sta INPFLG      ; set input mode flag, $00 = INPUT, $40 = GET, $98 = READ
    stx INPPTR      ; save READ pointer low byte
    sty INPPTR+1    ; save READ pointer high byte

                    ; READ, GET or INPUT next variable from list
LAB_CC15:
    jsr EVLVAR      ; get variable address
    sta FORPNT      ; save address low byte
    sty FORPNT+1    ; save address high byte
    lda CHRGOT+1    ; get BASIC execute pointer low byte
    ldy CHRGOT+2    ; get BASIC execute pointer high byte
    sta OPPTR       ; save BASIC execute pointer low byte
    sty OPPTR+1     ; save BASIC execute pointer high byte
    ldx INPPTR      ; get READ pointer low byte
    ldy INPPTR+1    ; get READ pointer high byte
    stx CHRGOT+1    ; save as BASIC execute pointer low byte
    sty CHRGOT+2    ; save as BASIC execute pointer high byte
    jsr CHRGOT      ; scan memory
    bne LAB_CC51    ; branch if not null

                    ; pointer was to null entry
    bit INPFLG      ; test input mode flag, $00 = INPUT, $40 = GET, $98 = READ
    BVC LAB_CC41    ; branch if not GET

                    ; else was GET
    jsr LAB_E121    ; get character from input device with error check
    sta BUF         ; save to buffer
    ldx #<(BUF-1)   ; set BUF-1 pointer low byte
    ldy #>(BUF-1)   ; set BUF-1 pointer high byte
    bne LAB_CC4D    ; go interpret single character

LAB_CC41:
    bmi LAB_CCB8    ; if READ go get some DATA

; else it was INPUT

    lda CHANNL      ; get current I/O channel
    bne LAB_CC4A    ; skip "?" prompt if not default channel

    jsr LAB_CB45    ; print "?"
LAB_CC4A:
    jsr LAB_CBF9    ; print "? " and get BASIC input
LAB_CC4D:
    stx CHRGOT+1    ; save BASIC execute pointer low byte
    sty CHRGOT+2    ; save BASIC execute pointer high byte
LAB_CC51:
    jsr CHRGET      ; increment and scan memory, execute pointer now points to
                    ; start of next data or null terminator
    bit VALTYP      ; test data type flag, $FF = string, $00 = numeric
    bpl LAB_CC89    ; branch if numeric

                    ; type is string
    bit INPFLG      ; test INPUT mode flag, $00 = INPUT, $40 = GET, $98 = READ
    BVC LAB_CC65    ; branch if not GET

                    ; else do string GET
    inx             ; clear .X ??
    stx CHRGOT+1    ; save BASIC execute pointer low byte
    lda #$00        ; clear .A
    sta CHARAC      ; clear search character
    beq LAB_CC71    ; branch always

                    ; is string INPUT or string READ
LAB_CC65:
    sta CHARAC      ; save search character
    cmp #$22        ; compare with "
    beq LAB_CC72    ; if quote only search for "..." string

                    ; else the string is not in quotes so ":", "," or $00 are
                    ; the termination characters
    lda #':'        ; set ":"
    sta CHARAC      ; set search character
    lda #','        ; set ","
LAB_CC71:
    clc             ; clear carry for add
LAB_CC72:
    sta ENDCHR      ; set scan quotes flag
    lda CHRGOT+1    ; get BASIC execute pointer low byte
    ldy CHRGOT+2    ; get BASIC execute pointer high byte
    adc #$00        ; add to pointer low byte. this add increments the pointer
                    ; if the mode is INPUT or READ and the data is a "..."
                    ; string
    bcc LAB_CC7D    ; if no rollover skip the high byte increment

    INY             ; else increment pointer high byte
LAB_CC7D:
    jsr LAB_D48D    ; print string to utility pointer
    jsr LAB_D7E2    ; restore BASIC execute pointer from temp
    jsr LET5        ; perform string LET
    jmp LAB_CC91    ; continue processing command

                    ; GET, INPUT or READ is numeric
LAB_CC89:
    jsr ASCFLT      ; get FAC1 from string
    lda INTFLG      ; get data type flag, $80 = integer, $00 = float
    jsr LET2        ; assign value to numeric variable
LAB_CC91:
    jsr CHRGOT      ; scan memory
    beq LAB_CC9D    ; if ":" or [EOL] go handle the string end

    cmp #','        ; compare with ","
    beq LAB_CC9D    ; if "," go handle the string end

    jmp IGRERR      ; else go do bad input routine

                    ; string terminated with ":", "," or $00
LAB_CC9D:
    lda CHRGOT+1    ; get BASIC execute pointer low byte
    ldy CHRGOT+2    ; get BASIC execute pointer high byte
    sta INPPTR      ; save READ pointer low byte
    sty INPPTR+1    ; save READ pointer high byte
    lda OPPTR       ; get saved BASIC execute pointer low byte
    ldy OPPTR+1     ; get saved BASIC execute pointer high byte
    sta CHRGOT+1    ; restore BASIC execute pointer low byte
    sty CHRGOT+2    ; restore BASIC execute pointer high byte
    jsr CHRGOT      ; scan memory
    beq LAB_CCDF    ; branch if ":" or [EOL]

    jsr COMCHK      ; scan for ",", else do syntax error then warm start
    jmp LAB_CC15    ; go READ or INPUT next variable from list

                    ; was READ
LAB_CCB8:
    jsr FIND2       ; scan for next BASIC statement ([:] or [EOL])
    INY             ; increment index to next byte
    tax             ; copy byte to .X
    bne LAB_CCD1    ; if ":" go look for the next DATA

    ldx #ER_OODATA  ; else set error $0D, out of data error
    INY             ; increment index to next line pointer high byte
    lda (CHRGOT+1),Y    ; get next line pointer high byte
    beq LAB_CD32    ; if program end go do error, eventually does error .X

    INY             ; increment index
    lda (CHRGOT+1),Y    ; get next line # low byte
    sta DATLIN      ; save current DATA line low byte
    INY             ; increment index
    lda (CHRGOT+1),Y    ; get next line # high byte
    INY             ; increment index
    sta DATLIN+1    ; save current DATA line high byte
LAB_CCD1:
    jsr BUMPTP      ; add .Y to the BASIC execute pointer
    jsr CHRGOT      ; scan memory
    tax             ; copy byte
    cpx #TK_DATA    ; compare with token for DATA
    bne LAB_CCB8    ; loop if not DATA

    jmp LAB_CC51    ; continue evaluating READ

LAB_CCDF:
    lda INPPTR      ; get READ pointer low byte
    ldy INPPTR+1    ; get READ pointer high byte
    ldx INPFLG      ; get INPUT mode flag, $00 = INPUT, $40 = GET, $98 = READ
    bpl LAB_CCEA    ; if INPUT or GET go exit or ignore extra input

    jmp LAB_C827    ; else set data pointer and exit

LAB_CCEA:
    ldy #$00        ; clear index
    lda (INPPTR),Y  ; get READ byte
    beq LAB_CCFB    ; exit if [EOL]

    lda CHANNL      ; get current I/O channel
    bne LAB_CCFB    ; exit if not default channel

    lda #<EXTRA     ; set "?EXTRA IGNORED" pointer low byte
    ldy #>EXTRA     ; set "?EXTRA IGNORED" pointer high byte
    jmp PRTSTR      ; print null terminated string

LAB_CCFB:
    rts


;***********************************************************************************;
;
; input error messages

EXTRA:
    .byte   "?EXTRA IGNORED",$0D,$00

LAB_CD0C:
    .byte   "?REDO FROM START",$0D,$00


;***********************************************************************************;
;
; perform NEXT

NEXT:
    bne LAB_CD24    ; if NEXT variable go find the variable

    ldy #$00        ; else clear .Y
    beq LAB_CD27    ; use any variable, branch always

; NEXT variable

LAB_CD24:
    jsr EVLVAR      ; get variable address
LAB_CD27:
    sta FORPNT      ; save FOR/NEXT variable pointer low byte
    sty FORPNT+1    ; save FOR/NEXT variable pointer high byte
                    ; (high byte cleared if no variable defined)
    jsr SCNSTK      ; search the stack for FOR or GOSUB activity
    beq LAB_CD35    ; if FOR found continue

    ldx #ER_NXTWOFOR    ; else set error $0A, next without for error
LAB_CD32:
    jmp ERROR       ; do error #.X then warm start

; found this FOR variable

LAB_CD35:
    txs             ; update stack pointer
    txa             ; copy stack pointer
    clc             ; clear carry for add
    adc #$04        ; point to STEP value
    pha             ; save it
    adc #$06        ; point to TO value
    sta INDEX+2     ; save pointer to TO variable for compare
    pla             ; restore pointer to STEP value
    ldy #$01        ; point to stack page
    jsr LODFAC      ; unpack memory (.A.Y) into FAC1
    tsx             ; get stack pointer back
    lda STACK+9,X   ; get step sign
    sta FAC1+FAC_SIGN   ; save FAC1 sign (b7)
    lda FORPNT      ; get FOR/NEXT variable pointer low byte
    ldy FORPNT+1    ; get FOR/NEXT variable pointer high byte
    jsr LAPLUS      ; add FOR variable to FAC1
    jsr FACTFP      ; pack FAC1 into FOR variable
    ldy #$01        ; point to stack page
    jsr LAB_DC5D    ; compare FAC1 with TO value
    tsx             ; get stack pointer back
    sec             ; set carry for subtract
    sbc STACK+9,X   ; subtract step sign
    beq LAB_CD78    ; if = loop complete, go unstack the FOR

                    ; loop back and do it all again
    lda STACK+$0F,X ; get FOR line low byte
    sta CURLIN      ; save current line number low byte
    lda STACK+$10,X ; get FOR line high byte
    sta CURLIN+1    ; save current line number high byte
    lda STACK+$12,X ; get BASIC execute pointer low byte
    sta CHRGOT+1    ; save BASIC execute pointer low byte
    lda STACK+$11,X ; get BASIC execute pointer high byte
    sta CHRGOT+2    ; save BASIC execute pointer high byte
LAB_CD75:
    jmp NEWSTT      ; go do interpreter inner loop

; NEXT loop complete

LAB_CD78:
    txa             ; stack copy to .A
    adc #$11        ; add $12, $11 + carry, to dump FOR structure
    tax             ; copy back to index
    txs             ; copy to stack pointer
    jsr CHRGOT      ; scan memory
    cmp #','        ; compare with ","
    bne LAB_CD75    ; if not "," go do interpreter inner loop

                    ; was "," so another NEXT variable to do
    jsr CHRGET      ; increment and scan memory
    jsr LAB_CD24    ; do NEXT variable


;***********************************************************************************;
;
; evaluate expression and check type mismatch

TYPCHK:
    jsr FRMEVL      ; evaluate expression

; check if source and destination are numeric

LAB_CD8D:
    clc
    .byte   $24     ; makes next line bit $38

; check if source and destination are string

LAB_CD8F:
    sec             ; destination is string

; type match check, set Cb for string, clear Cb for numeric

LAB_CD90:
    bit VALTYP      ; test data type flag, $FF = string, $00 = numeric
    bmi LAB_CD97    ; if string go check string is required

; type found is numeric, check required

    bcs LAB_CD99    ; if string is required go do type mismatch error
LAB_CD96:
    rts

; type found is string, check required

LAB_CD97:
    bcs LAB_CD96    ; exit if string is required

; do type mismatch error

LAB_CD99:
    ldx #ER_TYPMSMCH    ; error code $16, type mismatch error
    jmp ERROR       ; do error #.X then warm start


;***********************************************************************************;
;
; evaluate expression

FRMEVL:
    ldx CHRGOT+1    ; get BASIC execute pointer low byte
    bne LAB_CDA4    ; skip next if not zero

    dec CHRGOT+2    ; else decrement BASIC execute pointer high byte
LAB_CDA4:
    dec CHRGOT+1    ; decrement BASIC execute pointer low byte
    ldx #$00        ; set null precedence, flag done
    .byte   $24     ; makes next line bit $48
LAB_CDA9:
    pha             ; push compare evaluation byte if branch to here
    txa             ; copy precedence byte
    pha             ; push precedence byte
    lda #$01        ; 2 bytes
    jsr STKSPC      ; check room on stack for .A*2 bytes
    jsr EVAL        ; get value from line
    lda #$00        ; clear .A
    sta OPMASK      ; clear comparison evaluation flag
LAB_CDB8:
    jsr CHRGOT      ; scan memory
LAB_CDBB:
    sec             ; set carry for subtract
    sbc #TK_GT      ; subtract token for ">"
    bcc LAB_CDD7    ; if < ">" skip comparison test check

    cmp #$03        ; compare with ">" to +3
    bcs LAB_CDD7    ; if >= 3 skip comparison test check

                    ; was token for ">" "=" or "<"
    cmp #$01        ; compare with token for =
    ROL             ; *2, b0 = carry (=1 if token was = or <)
    eor #$01        ; toggle b0
    eor OPMASK      ; XOR with comparison evaluation flag
    cmp OPMASK      ; compare with comparison evaluation flag
    bcc LAB_CE30    ; if < saved flag do syntax error then warm start

    sta OPMASK      ; save new comparison evaluation flag
    jsr CHRGET      ; increment and scan memory
    jmp LAB_CDBB    ; go do next character

LAB_CDD7:
    ldx OPMASK      ; get comparison evaluation flag
    bne LAB_CE07    ; if compare function flagged go evaluate right hand side

    bcs LAB_CE58    ; go do functions

                    ; else was < TK_GT so is operator or lower
    adc #$07        ; add # of operators (+, -, *, /, ^, and or OR)
    bcc LAB_CE58    ; if < + operator go do the function

                    ; carry was set so token was +, -, *, /, ^, and or OR
    adc VALTYP      ; add data type flag, $FF = string, $00 = numeric
    bne LAB_CDE8    ; if not string or not + token skip concatenate

                    ; will only be $00 if type is string and token was +
    jmp ADDSTR      ; add strings, string 1 is in the descriptor, string 2
                    ; is in line, and return

LAB_CDE8:
    adc #$FF        ; -1 (corrects for carry add)
    sta INDEX       ; save it
    asl             ; *2
    adc INDEX       ; *3
    tay             ; copy to index
LAB_CDF0:
    pla             ; pull previous precedence
    cmp OPTAB,Y     ; compare with precedence byte
    bcs LAB_CE5D    ; if .A >= go do the function

    jsr LAB_CD8D    ; check if source is numeric, else do type mismatch
LAB_CDF9:
    pha             ; save precedence
LAB_CDFA:
    jsr LAB_CE20    ; get vector, execute function then continue evaluation
    pla             ; restore precedence
    ldy OPPTR       ; get precedence stacked flag
    bpl LAB_CE19    ; if stacked values go check the precedence

    tax             ; copy precedence, set flags
    beq LAB_CE5B    ; exit if done

    bne LAB_CE66    ; else pop FAC2 and return, branch always

LAB_CE07:
    lsr VALTYP      ; clear data type flag, $FF = string, $00 = numeric
    txa             ; copy compare function flag
    ROL             ; <<1, shift data type flag into b0, 1 = string, 0 = num
    ldx CHRGOT+1    ; get BASIC execute pointer low byte
    bne LAB_CE11    ; if no underflow skip the high byte decrement

    dec CHRGOT+2    ; else decrement BASIC execute pointer high byte
LAB_CE11:
    dec CHRGOT+1    ; decrement BASIC execute pointer low byte
    ldy #LAB_C09B-OPTAB
                    ; set offset to = operator precedence entry
    sta OPMASK      ; save new comparison evaluation flag
    bne LAB_CDF0    ; branch always

LAB_CE19:
    cmp OPTAB,Y     ; compare with stacked function precedence
    bcs LAB_CE66    ; if .A >=, pop FAC2 and return

    bcc LAB_CDF9    ; else go stack this one and continue, branch always


;***********************************************************************************;
;
; get vector, execute function then continue evaluation

LAB_CE20:
    lda OPTAB+2,Y   ; get function vector high byte
    pha             ; onto stack
    lda OPTAB+1,Y   ; get function vector low byte
    pha             ; onto stack
                    ; now push sign, round FAC1 and put on stack
    jsr LAB_CE33    ; function will return here, then the next rts will call
                    ; the function
    lda OPMASK      ; get comparison evaluation flag
    jmp LAB_CDA9    ; continue evaluating expression

LAB_CE30:
    jmp LAB_CF08    ; do syntax error then warm start

LAB_CE33:
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    ldx OPTAB,Y     ; get precedence byte


;***********************************************************************************;
;
; push sign, round FAC1 and put on stack

LAB_CE38:
    tay             ; copy sign
    pla             ; get return address low byte
    sta INDEX       ; save it
    inc INDEX       ; increment it as return - 1 is pushed
                    ; note, no check is made on the high byte so if the calling
                    ; routine ever assembles to a page edge then this all goes
                    ; horribly wrong!
    pla             ; get return address high byte
    sta INDEX+1     ; save it
    tya             ; restore sign
    pha             ; push sign


;***********************************************************************************;
;
; round FAC1 and put on stack

LAB_CE43:
    jsr ROUND           ; round FAC1
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    pha                 ; save it
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    pha                 ; save it
    lda FAC1+FAC_MANT+1 ; get FAC1 mantissa 2
    pha                 ; save it
    lda FAC1+FAC_MANT   ; get FAC1 mantissa 1
    pha                 ; save it
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    pha                 ; save it
    jmp (INDEX)         ; return, sort of


;***********************************************************************************;
;
; do functions

LAB_CE58:
    ldy #$FF            ; flag function
    pla             ; pull precedence byte
LAB_CE5B:
    beq LAB_CE80        ; exit if done

LAB_CE5D:
    cmp #$64            ; compare previous precedence with $64
    beq LAB_CE64        ; if was $64 (< function) skip the type check

    jsr LAB_CD8D        ; check if source is numeric, else do type mismatch
LAB_CE64:
    sty OPPTR           ; save precedence stacked flag

                        ; pop FAC2 and return
LAB_CE66:
    pla                 ; pop byte
    lsr                 ; shift out comparison evaluation lowest bit
    sta TANSGN          ; save the comparison evaluation flag
    pla                 ; pop exponent
    sta FAC2+FAC_EXPT   ; save FAC2 exponent
    pla                 ; pop mantissa 1
    sta FAC2+FAC_MANT   ; save FAC2 mantissa 1
    pla                 ; pop mantissa 2
    sta FAC2+FAC_MANT+1 ; save FAC2 mantissa 2
    pla                 ; pop mantissa 3
    sta FAC2+FAC_MANT+2 ; save FAC2 mantissa 3
    pla                 ; pop mantissa 4
    sta FAC2+FAC_MANT+3 ; save FAC2 mantissa 4
    pla                 ; pop sign
    sta FAC2+FAC_SIGN   ; save FAC2 sign (b7)
    eor FAC1+FAC_SIGN   ; XOR FAC1 sign (b7)
    sta ARISGN          ; save sign compare (FAC1 XOR FAC2)
LAB_CE80:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    rts


;***********************************************************************************;
;
; get value from line

EVAL:
    jmp (IEVAL)     ; get arithmetic element


;***********************************************************************************;
;
; get arithmetic element, the get arithmetic element vector is initialised to point here

FEVAL:
    lda #$00        ; clear byte
    sta VALTYP      ; clear data type flag, $FF = string, $00 = numeric
LAB_CE8A:
    jsr CHRGET      ; increment and scan memory
    bcs LAB_CE92    ; if not numeric character continue

; else numeric string found (e.g. 123)

LAB_CE8F:
    jmp ASCFLT      ; get FAC1 from string and return

; get value from line .. continued, wasn't a number so ...

LAB_CE92:
    jsr CHRTST      ; check byte, return Cb = 0 if <"A" or >"Z"
    bcc LAB_CE9A    ; if not variable name continue

    jmp FACT12      ; variable name set-up and return

; get value from line .. continued, wasn't a variable name so ...

LAB_CE9A:
    cmp #TK_PI      ; compare with token for PI
    bne LAB_CEAD    ; if not PI continue

; else return PI in FAC1

    lda #<PIVAL     ; get PI pointer low byte
    ldy #>PIVAL     ; get PI pointer high byte
    jsr LODFAC      ; unpack memory (.A.Y) into FAC1
    jmp CHRGET      ; increment and scan memory and return


;***********************************************************************************;
;
; PI as floating number

PIVAL:
    .byte   $82,$49,$0F,$DA,$A1 ; 3.141592653


;***********************************************************************************;
;
; get value from line .. continued, wasn't PI so ...

LAB_CEAD:
    cmp #'.'        ; compare with "."
    beq LAB_CE8F    ; if so get FAC1 from string and return, e.g. was .123

                    ; wasn't .123 so ...
    cmp #TK_MINUS   ; compare with token for -
    beq FACT10      ; if - token, do set-up for functions

                    ; wasn't -123 so ...
    cmp #TK_PLUS    ; compare with token for +
    beq LAB_CE8A    ; if + token ignore the leading +, +1 = 1

                    ; it wasn't any sort of number so ...
    cmp #$22        ; compare with "
    bne LAB_CECC    ; if not open quote continue

                    ; was open quote so get the enclosed string

; print "..." string to string utility area

LAB_CEBD:
    lda CHRGOT+1    ; get BASIC execute pointer low byte
    ldy CHRGOT+2    ; get BASIC execute pointer high byte
    adc #$00        ; add carry to low byte
    bcc LAB_CEC6    ; branch if no overflow

    INY             ; increment high byte
LAB_CEC6:
    jsr MAKSTR      ; print " terminated string to utility pointer
    jmp LAB_D7E2    ; restore BASIC execute pointer from temp and return

; get value from line .. continued, wasn't a string so ...

LAB_CECC:
    cmp #TK_NOT     ; compare with token for NOT
    bne LAB_CEE3    ; if not token for NOT continue

; was NOT token

    ldy #$18        ; offset to NOT function
    bne LAB_CF0F    ; do set-up for function then execute, branch always

; do = compare

EQUAL:
    jsr MAKINT      ; evaluate integer expression, no sign check
    lda FAC1+FAC_MANT+3     ; get FAC1 mantissa 4
    eor #$FF        ; invert it
    tay             ; copy it
    lda FAC1+FAC_MANT+2     ; get FAC1 mantissa 3
    eor #$FF        ; invert it
    jmp MAKFP       ; convert fixed integer .A.Y to float FAC1 and return

; get value from line .. continued, wasn't NOT so ...

LAB_CEE3:
    cmp #TK_FN      ; compare with token for FN
    bne LAB_CEEA    ; if not token for FN continue

    jmp EVALFN      ; else go evaluate FNx

; get value from line .. continued, wasn't FN so ...

LAB_CEEA:
    cmp #TK_SGN     ; compare with token for SGN
    bcc PAREXP      ; if less than SGN token go evaluate expression in ()

                    ; else was a function token
    jmp FACT17      ; go set up function references, branch always


;***********************************************************************************;
;
; get value from line .. continued
; if here it can only be something in brackets so ...

; evaluate expression within parentheses

PAREXP:
    jsr LPACHK      ; scan for "(", else do syntax error then warm start
    jsr FRMEVL      ; evaluate expression


;***********************************************************************************;
;
; all the 'scan for' routines return the character after the sought character

; scan for ")", else do syntax error then warm start

RPACHK:
    lda #$29        ; load .A with ")"
    .byte   $2C     ; makes next line bit $28A9

; scan for "(", else do syntax error then warm start

LPACHK:
    lda #$28        ; load .A with "("
    .byte   $2C     ; makes next line bit $2CA9

; scan for ",", else do syntax error then warm start

COMCHK:
    lda #','        ; load .A with ","

; scan for CHR$(.A), else do syntax error then warm start

SYNCHR:
    ldy #$00        ; clear index
    cmp (CHRGOT+1),Y    ; compare with BASIC byte
    bne LAB_CF08    ; if not expected byte do syntax error then warm start

    jmp CHRGET      ; else increment and scan memory and return


;***********************************************************************************;
;
; syntax error then warm start

LAB_CF08:
    ldx #ER_SYNtax  ; error code $0B, syntax error
    jmp ERROR       ; do error #.X then warm start

FACT10:
    ldy #$15        ; set offset from base to > operator
LAB_CF0F:
    pla             ; dump return address low byte
    pla             ; dump return address high byte
    jmp LAB_CDFA    ; execute function then continue evaluation


;***********************************************************************************;
;
; check address range, return Cb = 1 if address in BASIC ROM

VARRANGE:
    sec             ; set carry for subtract
    lda FAC1+3      ; get variable address low byte
    sbc #$00        ; subtract $C000 low byte
    lda FAC1+4      ; get variable address high byte
    sbc #$C0        ; subtract $C000 high byte
    bcc LAB_CF27    ; exit if address < $C000

    lda #<CGIMAG    ; get end of BASIC marker low byte
    sbc FAC1+3      ; subtract variable address low byte
    lda #>CGIMAG    ; get end of BASIC marker high byte
    sbc FAC1+4      ; subtract variable address high byte
LAB_CF27:
    rts


;***********************************************************************************;
;
; variable name set-up

FACT12:
    jsr EVLVAR      ; get variable address
    sta FAC1+3      ; save variable pointer low byte
    sty FAC1+4      ; save variable pointer high byte
    ldx VARNAM      ; get current variable name first character
    ldy VARNAM+1    ; get current variable name second character
    lda VALTYP      ; get data type flag, $FF = string, $00 = numeric
    beq LAB_CF5D    ; if numeric go handle a numeric variable

; variable is string

    lda #$00        ; else clear .A
    sta FACOV       ; clear FAC1 rounding byte
    jsr VARRANGE    ; check address range
    bcc LAB_CF5C    ; exit if not in BASIC ROM

    cpx #'T'        ; compare variable name first character with "T"
    bne LAB_CF5C    ; exit if not "T"

    cpy #'I'+$80    ; compare variable name second character with "I$"
    bne LAB_CF5C    ; exit if not "I$"

                    ; variable name was "TI$"
    jsr LAB_CF84    ; read real time clock into FAC1 mantissa, 0HML
    sty EXPCNT      ; clear exponent count adjust
    dey             ; .Y = $FF
    sty FBUFPT      ; set output string index, -1 to allow for pre increment
    ldy #$06        ; HH:MM:SS is six digits
    sty LAB_5D      ; set number of characters before the decimal point
    ldy #HMSCON-FLTCON
                    ; index to jiffy conversion table
    jsr LAB_DE68    ; convert jiffy count to string
    jmp LAB_D46F    ; exit via STR$() code tail

LAB_CF5C:
    rts

; variable name set-up, variable is numeric

LAB_CF5D:
    bit INTFLG      ; test data type flag, $80 = integer, $00 = float
    bpl LAB_CF6E    ; if float go handle float

; else handle integer variable

    ldy #$00        ; clear index
    lda (FAC1+3),Y  ; get integer variable low byte
    tax             ; copy to .X
    INY             ; increment index
    lda (FAC1+3),Y  ; get integer variable high byte
    tay             ; copy to .Y
    txa             ; copy loa byte to .A
    jmp MAKFP       ; convert fixed integer .A.Y to float FAC1 and return

; variable name set-up, variable is float

LAB_CF6E:
    jsr VARRANGE    ; check address range
    bcc LAB_CFA0    ; if not in BASIC ROM get pointer and unpack into FAC1

    cpx #'T'        ; compare variable name first character with "T"
    bne LAB_CF92    ; if not "T" skip Tx variables

    cpy #'I'        ; compare variable name second character with "I"
    bne LAB_CFA0    ; if not "I" go do plain float

                    ; variable name was "TI"
    jsr LAB_CF84    ; read real time clock into FAC1 mantissa, 0HML
    tya             ; clear .A
    ldx #$A0        ; set exponent to 32 bit value
    jmp LAB_DC4F    ; set exponent = .X and normalise FAC1


;***********************************************************************************;
;
; read real time clock into FAC1 mantissa, 0HML

LAB_CF84:
    jsr RDTIM           ; read real time clock
    stx FAC1+FAC_MANT+2 ; save jiffy clock mid byte as FAC1 mantissa 3
    sty FAC1+FAC_MANT+1 ; save jiffy clock high byte as FAC1 mantissa 2
    sta FAC1+FAC_MANT+3 ; save jiffy clock low byte as FAC1 mantissa 4
    ldy #$00            ; clear .Y
    sty FAC1+FAC_MANT   ; clear FAC1 mantissa 1
    rts


;***********************************************************************************;
;
; variable name set-up, variable is float and not "Tx"

LAB_CF92:
    cpx #'S'        ; compare variable name first character with "S"
    bne LAB_CFA0    ; if not "S" go do normal floating variable

    cpy #'T'        ; compare variable name second character with "T"
    bne LAB_CFA0    ; if not "T" go do normal floating variable

                    ; variable name was "ST"
    jsr READST      ; read I/O status word
    jmp INTFP       ; save .A as integer byte and return

; variable is plain float

LAB_CFA0:
    lda FAC1+3      ; get variable pointer low byte
    ldy FAC1+4      ; get variable pointer high byte
    jmp LODFAC      ; unpack memory (.A.Y) into FAC1


;***********************************************************************************;
;
; get value from line continued
; only functions left so ..

; set up function references

FACT17:
    asl             ; *2 (2 bytes per function address)
    pha             ; save function offset
    tax             ; copy function offset
    jsr CHRGET      ; increment and scan memory
    cpx #$8F        ; compare function offset to CHR$ token offset+1
    bcc LAB_CFD1    ; if < LEFT$ (can not be =) go do function setup

; get value from line .. continued
; was LEFT$, RIGHT$ or MID$ so..

    jsr LPACHK      ; scan for "(", else do syntax error then warm start
    jsr FRMEVL      ; evaluate, should be string, expression
    jsr COMCHK      ; scan for ",", else do syntax error then warm start
    jsr LAB_CD8F    ; check if source is string, else do type mismatch
    pla             ; restore function offset
    tax             ; copy it
    lda FAC1+4      ; get descriptor pointer high byte
    pha             ; push string pointer high byte
    lda FAC1+3      ; get descriptor pointer low byte
    pha             ; push string pointer low byte
    txa             ; restore function offset
    pha             ; save function offset
    jsr LAB_D79E    ; get byte parameter
    pla             ; restore function offset
    tay             ; copy function offset
    txa             ; copy byte parameter to .A
    pha             ; push byte parameter
    jmp LAB_CFD6    ; go call function

; get value from line .. continued
; was SGN() to CHR$() so..

LAB_CFD1:
    jsr PAREXP      ; evaluate expression within parentheses
    pla             ; restore function offset
    tay             ; copy to index
LAB_CFD6:
    lda FUNDSP-$68,Y    ; get function jump vector low byte
    sta JMPER+1     ; save functions jump vector low byte
    lda FUNDSP-$67,Y    ; get function jump vector high byte
    sta JMPER+2     ; save functions jump vector high byte
    jsr JMPER       ; do function call
    jmp LAB_CD8D    ; check if source is numeric and rts, else do type mismatch
                    ; string functions avoid this by dumping the return address


;***********************************************************************************;
;
; perform OR
; this works because NOT(NOT(x) and NOT(y)) = x OR y

ORR:
    ldy #$FF        ; set .Y for OR
    .byte   $2C     ; makes next line bit $00A0


;***********************************************************************************;
;
; perform and

ANDD:
    ldy #$00            ; clear .Y for and
    sty COUNT           ; set and/OR invert value
    jsr MAKINT          ; evaluate integer expression, no sign check
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    eor COUNT           ; XOR low byte
    sta CHARAC          ; save it
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    eor COUNT           ; XOR high byte
    sta ENDCHR          ; save it
    jsr ATOF            ; copy FAC2 to FAC1, get 2nd value in expression
    jsr MAKINT          ; evaluate integer expression, no sign check
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    eor COUNT           ; XOR high byte
    and ENDCHR          ; and with expression 1 high byte
    eor COUNT           ; XOR result high byte
    tay                 ; save in .Y
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    eor COUNT           ; XOR low byte
    and CHARAC          ; and with expression 1 low byte
    eor COUNT           ; XOR result low byte
    jmp MAKFP           ; convert fixed integer .A.Y to float FAC1 and return


;***********************************************************************************;
;
; perform comparisons

; do < compare

COMPAR:
    jsr LAB_CD90        ; type match check, set Cb for string
    bcs CMPST           ; if string go do string compare

                        ; do numeric < compare
    lda FAC2+FAC_SIGN   ; get FAC2 sign (b7)
    ora #$7F            ; set all non sign bits
    and FAC2+FAC_MANT   ; and FAC2 mantissa 1 (and in sign bit)
    sta FAC2+FAC_MANT   ; save FAC2 mantissa 1
    lda #<FAC2          ; set pointer low byte to FAC2
    ldy #>FAC2          ; set pointer high byte to FAC2
    jsr CMPFAC          ; compare FAC1 with (.A.Y)
    tax                 ; copy the result
    jmp LAB_D061        ; go evaluate result

; do string < compare

CMPST:
    lda #$00        ; clear byte
    sta VALTYP      ; clear data type flag, $FF = string, $00 = numeric
    dec OPMASK      ; clear < bit in comparison evaluation flag
    jsr LAB_D6A6    ; pop string off descriptor stack, or from top of string
                    ; space returns with .A = length, .X = pointer low byte,
                    ; .Y = pointer high byte
    sta FAC1        ; save length
    stx FAC1+1      ; save string pointer low byte
    sty FAC1+2      ; save string pointer high byte
    lda FAC2+3      ; get descriptor pointer low byte
    ldy FAC2+4      ; get descriptor pointer high byte
    jsr LAB_D6AA    ; pop (.Y.A) descriptor off stack or from top of string space
                    ; returns with .A = length, .X = pointer low byte,
                    ; .Y = pointer high byte
    stx FAC2+3      ; save string pointer low byte
    sty FAC2+4      ; save string pointer high byte
    tax             ; copy length
    sec             ; set carry for subtract
    sbc FAC1        ; subtract string 1 length
    beq LAB_D056    ; if str 1 length = string 2 length go compare the strings

    lda #$01        ; set str 1 length > string 2 length
    bcc LAB_D056    ; if so return +1 if otherwise equal

    ldx FAC1        ; get string 1 length
    lda #$FF        ; set str 1 length < string 2 length
LAB_D056:
    sta FAC1+5      ; save length compare
    ldy #$FF        ; set index
    inx             ; adjust for loop
LAB_D05B:
    INY             ; increment index
    dex             ; decrement count
    bne LAB_D066    ; if still bytes to do go compare them

    ldx FAC1+5      ; get length compare back
LAB_D061:
    bmi LAB_D072    ; branch if str 1 < str 2

    clc             ; flag str 1 <= str 2
    bcc LAB_D072    ; go evaluate result, branch always

LAB_D066:
    lda (FAC2+3),Y  ; get string 2 byte
    cmp (FAC1+1),Y  ; compare with string 1 byte
    beq LAB_D05B    ; loop if bytes =

    ldx #$FF        ; set str 1 < string 2
    bcs LAB_D072    ; branch if so

    ldx #$01        ; set str 1 > string 2
LAB_D072:
    inx             ; x = 0, 1 or 2
    txa             ; copy to .A
    ROL             ; * 2 (1, 2 or 4)
    and TANSGN      ; and with the comparison evaluation flag
    beq LAB_D07B    ; branch if 0 (compare is false)

    lda #$FF        ; else set result true
LAB_D07B:
    jmp INTFP       ; save .A as integer byte and return

LAB_D07E:
    jsr COMCHK      ; scan for ",", else do syntax error then warm start


;***********************************************************************************;
;
; perform DIM

DIM:
    tax             ; copy "DIM" flag to .X
    jsr LAB_D090    ; search for variable
    jsr CHRGOT      ; scan memory
    bne LAB_D07E    ; scan for "," and loop if not null

    rts


;***********************************************************************************;
;
; search for variable

EVLVAR:
    ldx #$00        ; set DIM flag = $00
    jsr CHRGOT      ; scan memory, 1st character
LAB_D090:
    stx DIMFLG      ; save DIM flag
LAB_D092:
    sta VARNAM      ; save 1st character
    jsr CHRGOT      ; scan memory
    jsr CHRTST      ; check byte, return Cb = 0 if <"A" or >"Z"
    bcs LAB_D09F    ; if ok continue

LAB_D09C:
    jmp LAB_CF08    ; else syntax error then warm start

; was variable name so ...

LAB_D09F:
    ldx #$00        ; clear 2nd character temp
    stx VALTYP      ; clear data type flag, $FF = string, $00 = numeric
    stx INTFLG      ; clear data type flag, $80 = integer, $00 = float
    jsr CHRGET      ; increment and scan memory, 2nd character
    bcc LAB_D0AF    ; if character = "0"-"9" (ok) go save 2nd character

                    ; 2nd character wasn't "0" to "9" so ...
    jsr CHRTST      ; check byte, return Cb = 0 if <"A" or >"Z"
    bcc LAB_D0BA    ; if <"A" or >"Z" go check if string

LAB_D0AF:
    tax             ; copy 2nd character

                    ; ignore further (valid) characters in the variable name
LAB_D0B0:
    jsr CHRGET      ; increment and scan memory, 3rd character
    bcc LAB_D0B0    ; loop if character = "0"-"9" (ignore)

    jsr CHRTST      ; check byte, return Cb = 0 if <"A" or >"Z"
    bcs LAB_D0B0    ; loop if character = "A"-"Z" (ignore)

                    ; check if string variable
LAB_D0BA:
    cmp #'$'        ; compare with "$"
    bne LAB_D0C4    ; if not string go check integer

                    ; type is string
    lda #$FF        ; set data type = string
    sta VALTYP      ; set data type flag, $FF = string, $00 = numeric
    bne LAB_D0D4    ; branch always

LAB_D0C4:
    cmp #$25        ; compare with "%"
    bne LAB_D0DB    ; if not integer go check for an array

    lda SUBFLG      ; get subscript/FNx flag
    bne LAB_D09C    ; if ?? do syntax error then warm start

    lda #$80        ; set integer type
    sta INTFLG      ; set data type = integer
    ora VARNAM      ; OR current variable name first byte
    sta VARNAM      ; save current variable name first byte
LAB_D0D4:
    txa             ; get 2nd character back
    ora #$80        ; set top bit, indicate string or integer variable
    tax             ; copy back to 2nd character temp
    jsr CHRGET      ; increment and scan memory
LAB_D0DB:
    stx VARNAM+1    ; save 2nd character
    sec             ; set carry for subtract
    ora SUBFLG      ; or with subscript/FNx flag - or FN name
    sbc #$28        ; subtract "("
    bne FNDVAR      ; if not "(" go find a plain numeric variable

    jmp ARY         ; else go find, or make, array

; either find or create variable

                    ; variable name wasn't xx(... so look for plain variable
FNDVAR:
    ldy #$00        ; clear .Y
    sty SUBFLG      ; clear subscript/FNx flag
    lda VARTAB      ; get start of variables low byte
    ldx VARTAB+1    ; get start of variables high byte
LAB_D0EF:
    stx TMPPTR+1    ; save search address high byte
LAB_D0F1:
    sta TMPPTR      ; save search address low byte
    cpx ARYTAB+1    ; compare with end of variables high byte
    bne LAB_D0FB    ; skip next compare if <>

                    ; high addresses were = so compare low addresses
    cmp ARYTAB      ; compare low address with end of variables low byte
    beq MAKVAR      ; if not found go make new variable

LAB_D0FB:
    lda VARNAM      ; get 1st character of variable to find
    cmp (TMPPTR),Y  ; compare with variable name 1st character
    bne LAB_D109    ; if no match go try the next variable

                    ; 1st characters match so compare 2nd character
    lda VARNAM+1    ; get 2nd character of variable to find
    INY             ; index to point to variable name 2nd character
    cmp (TMPPTR),Y  ; compare with variable name 2nd character
    beq RETVP       ; if match go return the variable

    dey             ; else decrement index (now = $00)
LAB_D109:
    clc             ; clear carry for add
    lda TMPPTR      ; get search address low byte
    adc #$07        ; +7, offset to next variable name
    bcc LAB_D0F1    ; loop if no overflow to high byte

    inx             ; else increment high byte
    bne LAB_D0EF    ; loop always, RAM doesn't extend to $FFFF


;***********************************************************************************;
;
; check byte, return Cb = 0 if<"A" or >"Z"

CHRTST:
    cmp #$41        ; compare with "A"
    bcc LAB_D11C    ; exit if less

                    ; carry is set
    sbc #$5B        ; subtract "Z"+1
    sec             ; set carry
    sbc #$A5        ; subtract $A5 (restore byte)
                    ; carry clear if byte > $5A
LAB_D11C:
    rts


;***********************************************************************************;
;
                    ; reached end of variable memory without match
                    ; ... so create new variable
MAKVAR:
    pla             ; pop return address low byte
    pha             ; push return address low byte
    cmp #<FACT12+2  ; compare with expected calling routine return low byte
    bne LAB_D128    ; if not get variable go create new variable

; This will only drop through if the call was from FACT12 and is only called
; from there if it is searching for a variable from the right hand side of a LET a=b
; statement, it prevents the creation of variables not assigned a value.

; Value returned by this is either numeric zero, exponent byte is $00, or null string,
; descriptor length byte is $00. in fact a pointer to any $00 byte would have done.

                    ; else return dummy null value
LAB_D123:
    lda #<NULLVAR   ; set result pointer low byte
    ldy #>NULLVAR   ; set result pointer high byte
    rts

                    ; create new numeric variable
LAB_D128:
    lda VARNAM      ; get variable name first character
    ldy VARNAM+1    ; get variable name second character
    cmp #'T'        ; compare first character with "T"
    bne LAB_D13B    ; if not "T" continue

    cpy #'I'+$80    ; compare second character with "I$"
    beq LAB_D123    ; if "I$" return null value

    cpy #'I'        ; compare second character with "I"
    bne LAB_D13B    ; if not "I" continue

                    ; if name is "TI" do syntax error
LAB_D138:
    jmp LAB_CF08    ; do syntax error then warm start

LAB_D13B:
    cmp #'S'        ; compare first character with "S"
    bne LAB_D143    ; if not "S" continue

    cpy #'T'        ; compare second character with "T"
    beq LAB_D138    ; if name is "ST" do syntax error

LAB_D143:
    lda ARYTAB      ; get end of variables low byte
    ldy ARYTAB+1    ; get end of variables high byte
    sta TMPPTR      ; save old block start low byte
    sty TMPPTR+1    ; save old block start high byte
    lda STREND      ; get end of arrays low byte
    ldy STREND+1    ; get end of arrays high byte
    sta GEN2PTR     ; save old block end low byte
    sty GEN2PTR+1   ; save old block end high byte
    clc             ; clear carry for add
    adc #$07        ; +7, space for one variable
    bcc LAB_D159    ; if no overflow skip the high byte increment

    INY             ; else increment high byte
LAB_D159:
    sta GENPTR      ; set new block end low byte
    sty GENPTR+1    ; set new block end high byte
    jsr MAKSPC      ; open up space in memory
    lda GENPTR      ; get new start low byte
    ldy GENPTR+1    ; get new start high byte (-$100)
    INY             ; correct high byte
    sta ARYTAB      ; set end of variables low byte
    sty ARYTAB+1    ; set end of variables high byte
    ldy #$00        ; clear index
    lda VARNAM      ; get variable name 1st character
    sta (TMPPTR),Y  ; save variable name 1st character
    INY             ; increment index
    lda VARNAM+1    ; get variable name 2nd character
    sta (TMPPTR),Y  ; save variable name 2nd character
    lda #$00        ; clear .A
    INY             ; increment index
    sta (TMPPTR),Y  ; initialise variable byte
    INY             ; increment index
    sta (TMPPTR),Y  ; initialise variable byte
    INY             ; increment index
    sta (TMPPTR),Y  ; initialise variable byte
    INY             ; increment index
    sta (TMPPTR),Y  ; initialise variable byte
    INY             ; increment index
    sta (TMPPTR),Y  ; initialise variable byte

                    ; found a match for variable
RETVP:
    lda TMPPTR      ; get variable address low byte
    clc             ; clear carry for add
    adc #$02        ; +2, offset past variable name bytes
    ldy TMPPTR+1    ; get variable address high byte
    bcc LAB_D18F    ; if no overflow skip the high byte increment

    INY             ; else increment high byte
LAB_D18F:
    sta VARPNT      ; save current variable pointer low byte
    sty VARPNT+1    ; save current variable pointer high byte
    rts


;***********************************************************************************;
;
; set-up array pointer to first element in array

ARYHED:
    lda COUNT       ; get # of dimensions (1, 2 or 3)
    asl             ; *2 (also clears the carry !)
    adc #$05        ; +5 (result is 7, 9 or 11 here)
    adc TMPPTR      ; add array start pointer low byte
    ldy TMPPTR+1    ; get array pointer high byte
    bcc LAB_D1A0    ; if no overflow skip the high byte increment

    INY             ; else increment high byte
LAB_D1A0:
    sta GENPTR      ; save array data pointer low byte
    sty GENPTR+1    ; save array data pointer high byte
    rts


;***********************************************************************************;
;
; -32768 as floating value

MAXINT:
    .byte   $90,$80,$00,$00,$00 ; -32768


;***********************************************************************************;
;
; convert float to fixed

INTIDX:
    jsr MAKINT      ; evaluate integer expression, no sign check
    lda FAC1+3      ; get result low byte
    ldy FAC1+4      ; get result high byte
    rts


;***********************************************************************************;
;
; evaluate integer expression

GETSUB:
    jsr CHRGET          ; increment and scan memory
    jsr FRMEVL          ; evaluate expression

; evaluate integer expression, sign check

LAB_D1B8:
    jsr LAB_CD8D        ; check if source is numeric, else do type mismatch
    lda FAC1+FAC_SIGN       ; get FAC1 sign (b7)
    bmi LAB_D1CC        ; do illegal quantity error if -ve

; evaluate integer expression, no sign check

MAKINT:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    cmp #$90            ; compare with exponent = 2^16 (n>2^15)
    bcc LAB_D1CE        ; if n<2^16 go convert FAC1 floating to fixed and return

    lda #<MAXINT        ; set pointer low byte to -32768
    ldy #>MAXINT        ; set pointer high byte to -32768
    jsr CMPFAC          ; compare FAC1 with (.A.Y)
LAB_D1CC:
    bne ILQUAN          ; if <> do illegal quantity error then warm start

LAB_D1CE:
    jmp FPINT           ; convert FAC1 floating to fixed and return


;***********************************************************************************;
;
; an array is stored as follows
;
; array name                ; two bytes with the following patterns for different types
;                   ; 1st char  2nd char
;                   ;   b7        b7        type            element size
;                   ; --------  --------    -----           ------------
;                   ;   0         0     floating point      5
;                   ;   0         1     string          3
;                   ;   1         1     integer         2
; offset to next array          ; word
; dimension count           ; byte
; 1st dimension size            ; word, this is the number of elements including 0
; 2nd dimension size            ; word, only here if the array has a second dimension
; 2nd dimension size            ; word, only here if the array has a third dimension
;                   ; note: the dimension size word is in high byte low byte
;                   ; format, unlike most 6502 words
; then for each element the required number of bytes given as the element size above

; find or make array

ARY:
    lda DIMFLG      ; get DIM flag
    ora INTFLG      ; OR with data type flag
    pha             ; push it
    lda VALTYP      ; get data type flag, $FF = string, $00 = numeric
    pha             ; push it
    ldy #$00        ; clear dimensions count

; now get the array dimension(s) and stack it (them) before the data type and DIM flag

LAB_D1DB:
    tya             ; copy dimensions count
    pha             ; save it
    lda VARNAM+1    ; get array name 2nd byte
    pha             ; save it
    lda VARNAM      ; get array name 1st byte
    pha             ; save it
    jsr GETSUB      ; evaluate integer expression
    pla             ; pull array name 1st byte
    sta VARNAM      ; restore array name 1st byte
    pla             ; pull array name 2nd byte
    sta VARNAM+1    ; restore array name 2nd byte
    pla             ; pull dimensions count
    tay             ; restore it
    tsx             ; copy stack pointer
    lda STACK+2,X   ; get DIM flag
    pha             ; push it
    lda STACK+1,X   ; get data type flag
    pha             ; push it
    lda FAC1+3      ; get this dimension size high byte
    sta STACK+2,X   ; stack before flag bytes
    lda FAC1+4      ; get this dimension size low byte
    sta STACK+1,X   ; stack before flag bytes
    INY             ; increment dimensions count
    jsr CHRGOT      ; scan memory
    cmp #','        ; compare with ","
    beq LAB_D1DB    ; if found go do next dimension

    sty COUNT       ; store dimensions count
    jsr RPACHK      ; scan for ")", else do syntax error then warm start
    pla             ; pull data type flag
    sta VALTYP      ; restore data type flag, $FF = string, $00 = numeric
    pla             ; pull data type flag
    sta INTFLG      ; restore data type flag, $80 = integer, $00 = float
    and #$7F        ; mask dim flag
    sta DIMFLG      ; restore DIM flag
    ldx ARYTAB      ; set end of variables low byte
                    ; (array memory start low byte)
    lda ARYTAB+1    ; set end of variables high byte
                    ; (array memory start high byte)

; now check to see if we are at the end of array memory, we would be if there were
; no arrays.

LAB_D21C:
    stx TMPPTR      ; save as array start pointer low byte
    sta TMPPTR+1    ; save as array start pointer high byte
    cmp STREND+1    ; compare with end of arrays high byte
    bne LAB_D228    ; if not reached array memory end continue searching

    cpx STREND      ; else compare with end of arrays low byte
    beq ARY6        ; go build array if not found

                    ; search for array
LAB_D228:
    ldy #$00        ; clear index
    lda (TMPPTR),Y  ; get array name first byte
    INY             ; increment index to second name byte
    cmp VARNAM      ; compare with this array name first byte
    bne LAB_D237    ; if no match go try the next array

    lda VARNAM+1    ; else get this array name second byte
    cmp (TMPPTR),Y  ; compare with array name second byte
    beq ARY2        ; array found so branch

                    ; no match
LAB_D237:
    INY             ; increment index
    lda (TMPPTR),Y  ; get array size low byte
    clc             ; clear carry for add
    adc TMPPTR      ; add array start pointer low byte
    tax             ; copy low byte to .X
    INY             ; increment index
    lda (TMPPTR),Y  ; get array size high byte
    adc TMPPTR+1    ; add array memory pointer high byte
    bcc LAB_D21C    ; if no overflow go check next array

; do bad subscript error

BADSUB:
    ldx #ER_BADSSCPT    ; error $12, bad subscript error
    .byte   $2C         ; makes next line bit $0EA2


;***********************************************************************************;
;
; do illegal quantity error

ILQUAN:
    ldx #ER_ILLQUAN ; error $0E, illegal quantity error
LAB_D24A:
    jmp ERROR       ; do error #.X then warm start


;***********************************************************************************;
;
; array found

ARY2:
    ldx #ER_REDIMARY    ; set error $13, double dimension error
    lda DIMFLG          ; get DIM flag
    bne LAB_D24A        ; if we are trying to dimension it do error #.X then warm
                        ; start

; found the array and we're not dimensioning it so we must find an element in it

    jsr ARYHED          ; set-up array pointer to first element in array
    lda COUNT           ; get dimensions count
    ldy #$04            ; set index to array's # of dimensions
    cmp (TMPPTR),Y      ; compare with no of dimensions
    bne BADSUB          ; if wrong do bad subscript error

    jmp ARY14           ; found array so go get element

                        ; array not found, so build it
ARY6:
    jsr ARYHED          ; set-up array pointer to first element in array
    jsr RAMSPC          ; check available memory, do out of memory error if no room
    ldy #$00            ; clear .Y
    sty FBUFPT+1        ; clear array data size high byte
    ldx #$05            ; set default element size
    lda VARNAM          ; get variable name 1st byte
    sta (TMPPTR),Y      ; save array name 1st byte
    bpl LAB_D274        ; branch if not string or floating point array

    dex                 ; decrement element size, $04
LAB_D274:
    INY                 ; increment index
    lda VARNAM+1        ; get variable name 2nd byte
    sta (TMPPTR),Y      ; save array name 2nd byte
    bpl LAB_D27D        ; branch if not integer or string

    dex                 ; decrement element size, $03
    dex                 ; decrement element size, $02
LAB_D27D:
    stx FBUFPT          ; save element size
    lda COUNT           ; get dimensions count
    INY                 ; increment index ..
    INY                 ; .. to array  ..
    INY                 ; .. dimension count
    sta (TMPPTR),Y      ; save array dimension count
LAB_D286:
    ldx #$0B            ; set default dimension size low byte
    lda #$00            ; set default dimension size high byte
    bit DIMFLG          ; test DIM flag
    BVC LAB_D296        ; if default to be used don't pull a dimension

    pla                 ; pull dimension size low byte
    clc                 ; clear carry for add
    adc #$01            ; add 1, allow for zeroeth element
    tax                 ; copy low byte to .X
    pla                 ; pull dimension size high byte
    adc #$00            ; add carry to high byte
LAB_D296:
    INY                 ; incement index to dimension size high byte
    sta (TMPPTR),Y      ; save dimension size high byte
    INY                 ; incement index to dimension size low byte
    txa                 ; copy dimension size low byte
    sta (TMPPTR),Y      ; save dimension size low byte
    jsr M16             ; compute array size
    stx FBUFPT          ; save result low byte
    sta FBUFPT+1        ; save result high byte
    ldy INDEX           ; restore index
    dec COUNT           ; decrement dimensions count
    bne LAB_D286        ; loop if not all done

    adc GENPTR+1        ; add array data pointer high byte
    bcs LAB_D30B        ; if overflow do out of memory error then warm start

    sta GENPTR+1        ; save array data pointer high byte
    tay                 ; copy array data pointer high byte
    txa                 ; copy array size low byte
    adc GENPTR          ; add array data pointer low byte
    bcc LAB_D2B9        ; if no rollover skip the high byte increment

    INY                 ; else increment next array pointer high byte
    beq LAB_D30B        ; if rolled over do out of memory error then warm start

LAB_D2B9:
    jsr RAMSPC          ; check available memory, do out of memory error if no room
    sta STREND          ; set end of arrays low byte
    sty STREND+1        ; set end of arrays high byte

; now the array is created we need to zero all the elements in it

    lda #$00            ; clear .A for array clear
    inc FBUFPT+1        ; increment array size high byte, now block count
    ldy FBUFPT          ; get array size low byte, now index to block
    beq LAB_D2CD        ; if $00 go do the high byte decrement
LAB_D2C8:
    dey                 ; decrement index, do 0 to n - 1
    sta (GENPTR),Y      ; clear array element byte
    bne LAB_D2C8        ; loop until this block done

LAB_D2CD:
    dec GENPTR+1        ; decrement array pointer high byte
    dec FBUFPT+1        ; decrement block count high byte
    bne LAB_D2C8        ; loop until all blocks done

    inc GENPTR+1        ; correct for last loop
    sec                 ; set carry for subtract
    lda STREND          ; get end of arrays low byte
    sbc TMPPTR          ; subtract array start low byte
    ldy #$02            ; index to array size low byte
    sta (TMPPTR),Y      ; save array size low byte
    lda STREND+1        ; get end of arrays high byte
    INY                 ; index to array size high byte
    sbc TMPPTR+1        ; subtract array start high byte
    sta (TMPPTR),Y      ; save array size high byte
    lda DIMFLG          ; get default DIM flag
    bne LAB_D34B        ; exit if this was a DIM command

                        ; else, find element
    INY                 ; set index to # of dimensions, the dimension indices
                        ; are on the stack and will be removed as the position
                        ; of the array element is calculated

ARY14:
    lda (TMPPTR),Y      ; get array's dimension count
    sta COUNT           ; save it
    lda #$00            ; clear byte
    sta FBUFPT          ; clear array data pointer low byte
LAB_D2F2:
    sta FBUFPT+1        ; save array data pointer high byte
    INY                 ; increment index, point to array bound high byte
    pla                 ; pull array index low byte
    tax                 ; copy to .X
    sta FAC1+FAC_MANT+2 ; save index low byte to FAC1 mantissa 3
    pla                 ; pull array index high byte
    sta FAC1+FAC_MANT+3 ; save index high byte to FAC1 mantissa 4
    cmp (TMPPTR),Y      ; compare with array bound high byte
    bcc LAB_D30E        ; if within bounds continue

    bne LAB_D308        ; if outside bounds do bad subscript error

                        ; else high byte was = so test low bytes
    INY                 ; index to array bound low byte
    txa                 ; get array index low byte
    cmp (TMPPTR),Y      ; compare with array bound low byte
    bcc LAB_D30F        ; if within bounds continue

LAB_D308:
    jmp BADSUB          ; do bad subscript error

LAB_D30B:
    jmp MEMERR          ; do out of memory error then warm start

LAB_D30E:
    INY                 ; index to array bound low byte
LAB_D30F:
    lda FBUFPT+1        ; get array data pointer high byte
    ora FBUFPT          ; OR with array data pointer low byte
    clc                 ; clear carry for either add, carry always clear here ??
    beq LAB_D320        ; if array data pointer = null skip the multiply

    jsr M16             ; compute array size
    txa                 ; get result low byte
    adc FAC1+FAC_MANT+2 ; add index low byte from FAC1 mantissa 3
    tax                 ; save result low byte
    tya                 ; get result high byte
    ldy INDEX           ; restore index
LAB_D320:
    adc FAC1+FAC_MANT+3 ; add index high byte from FAC1 mantissa 4
    stx FBUFPT          ; save array data pointer low byte
    dec COUNT           ; decrement dimensions count
    bne LAB_D2F2        ; loop if dimensions still to do

    sta FBUFPT+1        ; save array data pointer high byte
    ldx #$05            ; set default element size
    lda VARNAM          ; get variable name 1st byte
    bpl LAB_D331        ; branch if not string or floating point array

    dex                 ; decrement element size, $04
LAB_D331:
    lda VARNAM+1        ; get variable name 2nd byte
    bpl LAB_D337        ; branch if not integer or string

    dex                 ; decrement element size, $03
    dex                 ; decrement element size, $02
LAB_D337:
    stx RESHO+2         ; save dimension size low byte
    lda #$00            ; clear dimension size high byte
    jsr LAB_D355        ; compute array size
    txa                 ; copy array size low byte
    adc GENPTR          ; add array data start pointer low byte
    sta VARPNT          ; save as current variable pointer low byte
    tya                 ; copy array size high byte
    adc GENPTR+1        ; add array data start pointer high byte
    sta VARPNT+1        ; save as current variable pointer high byte
    tay                 ; copy high byte to .Y
    lda VARPNT          ; get current variable pointer low byte
                        ; pointer to element is now in .A.Y
LAB_D34B:
    rts


;***********************************************************************************;
;
; compute array size, result in .X.Y

M16:
    sty INDEX       ; save index
    lda (TMPPTR),Y  ; get dimension size low byte
    sta RESHO+2     ; save dimension size low byte
    dey             ; decrement index
    lda (TMPPTR),Y  ; get dimension size high byte
LAB_D355:
    sta RESHO+3     ; save dimension size high byte
    lda #$10        ; count = $10 (16 bit multiply)
    sta LAB_5D      ; save bit count
    ldx #$00        ; clear result low byte
    ldy #$00        ; clear result high byte
LAB_D35F:
    txa             ; get result low byte
    asl             ; *2
    tax             ; save result low byte
    tya             ; get result high byte
    ROL             ; *2
    tay             ; save result high byte
    bcs LAB_D30B    ; if overflow go do "Out of memory" error

    asl FBUFPT      ; shift element size low byte
    ROL FBUFPT+1    ; shift element size high byte
    bcc LAB_D378    ; skip add if no carry

    clc             ; else clear carry for add
    txa             ; get result low byte
    adc RESHO+2     ; add dimension size low byte
    tax             ; save result low byte
    tya             ; get result high byte
    adc RESHO+3     ; add dimension size high byte
    tay             ; save result high byte
    bcs LAB_D30B    ; if overflow go do "Out of memory" error

LAB_D378:
    dec LAB_5D      ; decrement bit count
    bne LAB_D35F    ; loop until all done

    rts


;***********************************************************************************;
;
; perform FRE()

FRE:
    lda VALTYP      ; get data type flag, $FF = string, $00 = numeric
    beq LAB_D384    ; if numeric don't pop the string

    jsr LAB_D6A6    ; pop string off descriptor stack, or from top of string
                    ; space returns with .A = length, .X=$71=pointer low byte,
                    ; .Y=$72=pointer high byte

                    ; FRE(n) was numeric so do this
LAB_D384:
    jsr GRBCOL      ; go do garbage collection
    sec             ; set carry for subtract
    lda FRETOP      ; get bottom of string space low byte
    sbc STREND      ; subtract end of arrays low byte
    tay             ; copy result to .Y
    lda FRETOP+1    ; get bottom of string space high byte
    sbc STREND+1    ; subtract end of arrays high byte


;***********************************************************************************;
;
; convert fixed integer .A.Y to float FAC1

MAKFP:
    ldx #$00            ; set type = numeric
    stx VALTYP          ; clear data type flag, $FF = string, $00 = numeric
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1
    sty FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    ldx #$90            ; set exponent=2^16 (integer)
    jmp INTFP1          ; set exp = .X, clear FAC1 3 and 4, normalise and return


;***********************************************************************************;
;
; perform POS()

POS:
    sec             ; set Cb for read cursor position
    jsr PLOT        ; read/set X,Y cursor position
LAB_D3A2:
    lda #$00        ; clear high byte
    beq MAKFP       ; convert fixed integer .A.Y to float FAC1, branch always


;***********************************************************************************;
;
; check not Direct, used by DEF and INPUT

NODIRM:
    ldx CURLIN+1    ; get current line number high byte
    inx             ; increment it
    bne LAB_D34B    ; return if not direct mode

                    ; else do illegal direct error
    ldx #ER_ILLDIR  ; error $15, illegal direct error
    .byte   $2C     ; makes next line bit $1BA2
UNDEF:
    ldx #ER_UNDEFUN ; error $1B, undefined function error
    jmp ERROR       ; do error #.X then warm start


;***********************************************************************************;
;
; perform DEF

DEF:
    jsr FN          ; check FNx syntax
    jsr NODIRM      ; check not direct, back here if ok
    jsr LPACHK      ; scan for "(", else do syntax error then warm start
    lda #$80        ; set flag for FNx
    sta SUBFLG      ; save subscript/FNx flag
    jsr EVLVAR      ; get variable address
    jsr LAB_CD8D    ; check if source is numeric, else do type mismatch
    jsr RPACHK      ; scan for ")", else do syntax error then warm start
    lda #TK_EQUAL   ; get = token
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
    pha             ; push next character
    lda VARPNT+1    ; get current variable pointer high byte
    pha             ; push it
    lda VARPNT      ; get current variable pointer low byte
    pha             ; push it
    lda CHRGOT+2    ; get BASIC execute pointer high byte
    pha             ; push it
    lda CHRGOT+1    ; get BASIC execute pointer low byte
    pha             ; push it
    jsr SKIPST      ; perform DATA
    jmp EVFN3       ; put execute pointer and variable pointer into function
                    ; and return


;***********************************************************************************;
;
; check FNx syntax

FN:
    lda #TK_FN      ; set FN token
    jsr SYNCHR      ; scan for CHR$(.A), else do syntax error then warm start
    ora #$80        ; set FN flag bit
    sta SUBFLG      ; save FN name
    jsr LAB_D092    ; search for FN variable
    sta DEFPNT      ; save function pointer low byte
    sty DEFPNT+1    ; save function pointer high byte
    jmp LAB_CD8D    ; check if source is numeric and return, else do type
                    ; mismatch


;***********************************************************************************;
;
; Evaluate FNx

EVALFN:
    jsr FN          ; check FNx syntax
    lda DEFPNT+1    ; get function pointer high byte
    pha             ; push it
    lda DEFPNT      ; get function pointer low byte
    pha             ; push it
    jsr PAREXP      ; evaluate expression within parentheses
    jsr LAB_CD8D    ; check if source is numeric, else do type mismatch
    pla             ; pop function pointer low byte
    sta DEFPNT      ; restore it
    pla             ; pop function pointer high byte
    sta DEFPNT+1    ; restore it
    ldy #$02        ; index to variable pointer high byte
    lda (DEFPNT),Y  ; get variable address low byte
    sta VARPNT      ; save current variable pointer low byte
    tax             ; copy address low byte
    INY             ; index to variable address high byte
    lda (DEFPNT),Y  ; get variable pointer high byte
    beq UNDEF       ; if high byte zero go do undefined function error

    sta VARPNT+1    ; save current variable pointer high byte
    INY             ; index to mantissa 3

                    ; now stack the function variable value before use
LAB_D418:
    lda (VARPNT),Y  ; get byte from variable
    pha             ; stack it
    dey             ; decrement index
    bpl LAB_D418    ; loop until variable stacked

    ldy VARPNT+1    ; get current variable pointer high byte
    jsr STORFAC     ; pack FAC1 into (.X.Y)
    lda CHRGOT+2    ; get BASIC execute pointer high byte
    pha             ; push it
    lda CHRGOT+1    ; get BASIC execute pointer low byte
    pha             ; push it
    lda (DEFPNT),Y  ; get function execute pointer low byte
    sta CHRGOT+1    ; save BASIC execute pointer low byte
    INY             ; index to high byte
    lda (DEFPNT),Y  ; get function execute pointer high byte
    sta CHRGOT+2    ; save BASIC execute pointer high byte
    lda VARPNT+1    ; get current variable pointer high byte
    pha             ; push it
    lda VARPNT      ; get current variable pointer low byte
    pha             ; push it
    jsr TYPCHK      ; evaluate expression and check is numeric, else do
                    ; type mismatch
    pla             ; pull variable address low byte
    sta DEFPNT      ; save variable address low byte
    pla             ; pull variable address high byte
    sta DEFPNT+1    ; save variable address high byte
    jsr CHRGOT      ; scan memory
    beq LAB_D449    ; if null (should be [EOL] marker) continue

    jmp LAB_CF08    ; else syntax error then warm start

; restore BASIC execute pointer and function variable from stack

LAB_D449:
    pla             ; pull BASIC execute pointer low byte
    sta CHRGOT+1    ; save BASIC execute pointer low byte
    pla             ; pull BASIC execute pointer high byte
    sta CHRGOT+2    ; save BASIC execute pointer high byte

; put execute pointer and variable pointer into function

EVFN3:
    ldy #$00        ; clear index
    pla             ; pull BASIC execute pointer low byte
    sta (DEFPNT),Y  ; save to function
    pla             ; pull BASIC execute pointer high byte
    INY             ; increment index
    sta (DEFPNT),Y  ; save to function
    pla             ; pull current variable address low byte
    INY             ; increment index
    sta (DEFPNT),Y  ; save to function
    pla             ; pull current variable address high byte
    INY             ; increment index
    sta (DEFPNT),Y  ; save to function
    pla             ; pull ??
    INY             ; increment index
    sta (DEFPNT),Y  ; save to function
    rts


;***********************************************************************************;
;
; perform STR$()

STR:
    jsr LAB_CD8D    ; check if source is numeric, else do type mismatch
    ldy #$00        ; set string index
    jsr LAB_DDDF    ; convert FAC1 to string
    pla             ; dump return address (skip type check)
    pla             ; dump return address (skip type check)
LAB_D46F:
    lda #<BASZPT    ; set result string low pointer
    ldy #>BASZPT    ; set result string high pointer
    beq MAKSTR      ; print null terminated string to utility pointer


;***********************************************************************************;
;
; do string vector
; copy descriptor pointer and make string space .A bytes long

ALC1:
    ldx FAC1+3      ; get descriptor pointer low byte
    ldy FAC1+4      ; get descriptor pointer high byte
    stx DSCPTN      ; save descriptor pointer low byte
    sty DSCPTN+1    ; save descriptor pointer high byte


;***********************************************************************************;
;
; make string space .A bytes long

LAB_D47D:
    jsr ALCSPAC     ; make space in string memory for string .A long
    stx FAC1+1      ; save string pointer low byte
    sty FAC1+2      ; save string pointer high byte
    sta FAC1        ; save length
    rts


;***********************************************************************************;
;
; scan, set up string
; print " terminated string to utility pointer

MAKSTR:
    ldx #$22        ; set terminator to "
    stx CHARAC      ; set search character, terminator 1
    stx ENDCHR      ; set terminator 2

; print search or alternate terminated string to utility pointer
; source is .A.Y

LAB_D48D:
    sta ARISGN      ; store string start low byte
    sty FACOV       ; store string start high byte
    sta FAC1+1      ; save string pointer low byte
    sty FAC1+2      ; save string pointer high byte
    ldy #$FF        ; set length to -1
LAB_D497:
    INY             ; increment length
    lda (ARISGN),Y  ; get byte from string
    beq LAB_D4A8    ; exit loop if null byte [EOS]

    cmp CHARAC      ; compare with search character, terminator 1
    beq LAB_D4A4    ; branch if terminator

    cmp ENDCHR      ; compare with terminator 2
    bne LAB_D497    ; loop if not terminator 2

LAB_D4A4:
    cmp #$22        ; compare with "
    beq LAB_D4A9    ; branch if " (carry set if = !)

LAB_D4A8:
    clc             ; clear carry for add (only if [EOL] terminated string)
LAB_D4A9:
    sty FAC1+FAC_EXPT   ; save length in FAC1 exponent
    tya             ; copy length to .A
    adc ARISGN      ; add string start low byte
    sta FBUFPT      ; save string end low byte
    ldx FACOV       ; get string start high byte
    bcc LAB_D4B5    ; if no low byte overflow skip the high byte increment

    inx             ; else increment high byte
LAB_D4B5:
    stx FBUFPT+1    ; save string end high byte
    lda FACOV       ; get string start high byte
    beq LAB_D4BF    ; branch if in utility area

    cmp #$02        ; compare with input buffer memory high byte
    bne LAB_D4CA    ; branch if not in input buffer memory

                    ; string in input buffer or utility area, move to string
                    ; memory
LAB_D4BF:
    tya             ; copy length to .A
    jsr ALC1        ; copy descriptor pointer and make string space .A bytes long
    ldx ARISGN      ; get string start low byte
    ldy FACOV       ; get string start high byte
    jsr LAB_D688    ; store string .A bytes long from .X.Y to utility pointer

; check for space on descriptor stack then ...
; put string address and length on descriptor stack and update stack pointers

LAB_D4CA:
    ldx TEMPPT      ; get descriptor stack pointer
    cpx #$22        ; compare with max+1
    bne LAB_D4D5    ; branch if space on string stack

                    ; else do string too complex error
    ldx #ER_FMLA2CPLX   ; error $19, formula too complex error
LAB_D4D2:
    jmp ERROR       ; do error #.X then warm start

; put string address and length on descriptor stack and update stack pointers

LAB_D4D5:
    lda FAC1        ; get string length
    sta $00,X       ; put on string stack
    lda FAC1+1      ; get string pointer low byte
    sta $01,X       ; put on string stack
    lda FAC1+2      ; get string pointer high byte
    sta $02,X       ; put on string stack
    ldy #$00        ; clear .Y
    stx FAC1+3      ; save string descriptor pointer low byte
    sty FAC1+4      ; save string descriptor pointer high byte, always $00
    sty FACOV       ; clear FAC1 rounding byte
    dey             ; .Y = $FF
    sty VALTYP      ; save data type flag, $FF = string
    stx LASTPT      ; save current descriptor stack item pointer low byte
    inx             ; update stack pointer
    inx             ; update stack pointer
    inx             ; update stack pointer
    stx TEMPPT      ; set new descriptor stack pointer
    rts

; make space in string memory for string .A long
; return .X = pointer low byte, .Y = pointer high byte

ALCSPAC:
    lsr GARBFL      ; clear garbage collected flag (b7)

                    ; make space for string .A long
LAB_D4F6:
    pha             ; save string length
    eor #$FF        ; complement it
    sec             ; set carry for subtract, two's complement add
    adc FRETOP      ; add bottom of string space low byte, subtract length
    ldy FRETOP+1    ; get bottom of string space high byte
    bcs LAB_D501    ; skip decrement if no underflow

    dey             ; decrement bottom of string space high byte
LAB_D501:
    cpy STREND+1    ; compare with end of arrays high byte
    bcc LAB_D516    ; do out of memory error if less

    bne LAB_D50B    ; if not = skip next test

    cmp STREND      ; compare with end of arrays low byte
    bcc LAB_D516    ; do out of memory error if less

LAB_D50B:
    sta FRETOP      ; save bottom of string space low byte
    sty FRETOP+1    ; save bottom of string space high byte
    sta FRESPC      ; save string utility ptr low byte
    sty FRESPC+1    ; save string utility ptr high byte
    tax             ; copy low byte to .X
    pla             ; get string length back
    rts

LAB_D516:
    ldx #$10        ; error code $10, out of memory error
    lda GARBFL      ; get garbage collected flag
    bmi LAB_D4D2    ; if set then do error code .X

    jsr GRBCOL      ; else go do garbage collection
    lda #$80        ; flag for garbage collected
    sta GARBFL      ; set garbage collected flag
    pla             ; pull length
    bne LAB_D4F6    ; go try again (loop always, length should never be = $00)


;***********************************************************************************;
;
; garbage collection routine

GRBCOL:
    ldx MEMSIZ      ; get end of memory low byte
    lda MEMSIZ+1    ; get end of memory high byte

; re-run routine from last ending

LAB_D52A:
    stx FRETOP      ; set bottom of string space low byte
    sta FRETOP+1    ; set bottom of string space high byte
    ldy #$00        ; clear index
    sty DEFPNT+1    ; clear working pointer high byte
    sty DEFPNT      ; clear working pointer low byte
    lda STREND      ; get end of arrays low byte
    ldx STREND+1    ; get end of arrays high byte
    sta TMPPTR      ; save as highest uncollected string pointer low byte
    stx TMPPTR+1    ; save as highest uncollected string pointer high byte
    lda #TEMPST     ; set descriptor stack pointer
    ldx #$00        ; clear .X
    sta INDEX       ; save descriptor stack pointer low byte
    stx INDEX+1     ; save descriptor stack pointer high byte ($00)
LAB_D544:
    cmp TEMPPT      ; compare with descriptor stack pointer
    beq LAB_D54D    ; branch if =

    jsr LAB_D5C7    ; check string salvageability
    beq LAB_D544    ; loop always

                    ; done stacked strings, now do string variables
LAB_D54D:
    lda #$07        ; set step size = $07, collecting variables
    sta FOUR6       ; save garbage collection step size
    lda VARTAB      ; get start of variables low byte
    ldx VARTAB+1    ; get start of variables high byte
    sta INDEX       ; save as pointer low byte
    stx INDEX+1     ; save as pointer high byte
LAB_D559:
    cpx ARYTAB+1    ; compare end of variables high byte,
                    ; start of arrays high byte
    bne LAB_D561    ; branch if no high byte match

    cmp ARYTAB      ; else compare end of variables low byte,
                    ; start of arrays low byte
    beq LAB_D566    ; branch if = variable memory end

LAB_D561:
    jsr GCOL13      ; check variable salvageability
    beq LAB_D559    ; loop always

                    ; done string variables, now do string arrays
LAB_D566:
    sta GENPTR      ; save start of arrays low byte as working pointer
    stx GENPTR+1    ; save start of arrays high byte as working pointer
    lda #$03        ; set step size, collecting descriptors
    sta FOUR6       ; save step size
LAB_D56E:
    lda GENPTR      ; get pointer low byte
    ldx GENPTR+1    ; get pointer high byte
LAB_D572:
    cpx STREND+1    ; compare with end of arrays high byte
    bne LAB_D57D    ; branch if not at end

    cmp STREND      ; else compare with end of arrays low byte
    bne LAB_D57D    ; branch if not at end

    jmp COLLECT     ; collect string, tidy up and exit if at end ??

LAB_D57D:
    sta INDEX       ; save pointer low byte
    stx INDEX+1     ; save pointer high byte
    ldy #$00        ; set index
    lda (INDEX),Y   ; get array name first byte
    tax             ; copy it
    INY             ; increment index
    lda (INDEX),Y   ; get array name second byte
    php             ; push the flags
    INY             ; increment index
    lda (INDEX),Y   ; get array size low byte
    adc GENPTR      ; add start of this array low byte
    sta GENPTR      ; save start of next array low byte
    INY             ; increment index
    lda (INDEX),Y   ; get array size high byte
    adc GENPTR+1    ; add start of this array high byte
    sta GENPTR+1    ; save start of next array high byte
    plp             ; restore the flags
    bpl LAB_D56E    ; skip if not string array

; was possibly string array so ...

    txa             ; get name first byte back
    bmi LAB_D56E    ; skip if not string array

    INY             ; increment index
    lda (INDEX),Y   ; get # of dimensions
    ldy #$00        ; clear index
    asl             ; *2
    adc #$05        ; +5 (array header size)
    adc INDEX       ; add pointer low byte
    sta INDEX       ; save pointer low byte
    bcc LAB_D5AE    ; if no rollover skip the high byte increment

    inc INDEX+1     ; else increment pointer hgih byte
LAB_D5AE:
    ldx INDEX+1     ; get pointer high byte
LAB_D5B0:
    cpx GENPTR+1    ; compare pointer high byte with end of this array high byte
    bne LAB_D5B8    ; branch if not there yet

    cmp GENPTR      ; compare pointer low byte with end of this array low byte
    beq LAB_D572    ; if at end of this array go check next array

LAB_D5B8:
    jsr LAB_D5C7    ; check string salvageability
    beq LAB_D5B0    ; loop

; check variable salvageability

GCOL13:
    lda (INDEX),Y   ; get variable name first byte
    bmi LAB_D5F6    ; add step and exit if not string

    INY             ; increment index
    lda (INDEX),Y   ; get variable name second byte
    bpl LAB_D5F6    ; add step and exit if not string

    INY             ; increment index

; check string salvageability

LAB_D5C7:
    lda (INDEX),Y   ; get string length
    beq LAB_D5F6    ; add step and exit if null string

    INY             ; increment index
    lda (INDEX),Y   ; get string pointer low byte
    tax             ; copy to .X
    INY             ; increment index
    lda (INDEX),Y   ; get string pointer high byte
    cmp FRETOP+1    ; compare string pointer high byte with bottom of string
                    ; space high byte
    bcc LAB_D5DC    ; if bottom of string space greater go test against highest
                    ; uncollected string

    bne LAB_D5F6    ; if bottom of string space less string has been collected
                    ; so go update pointers, step to next and return

                    ; high bytes were equal so test low bytes
    cpx FRETOP      ; compare string pointer low byte with bottom of string
                    ; space low byte
    bcs LAB_D5F6    ; if bottom of string space less string has been collected
                    ; so go update pointers, step to next and return

                    ; else test string against highest uncollected string so far
LAB_D5DC:
    cmp TMPPTR+1    ; compare string pointer high byte with highest uncollected
                    ; string high byte
    bcc LAB_D5F6    ; if highest uncollected string is greater then go update
                    ; pointers, step to next and return

    bne LAB_D5E6    ; if highest uncollected string is less then go set this
                    ; string as highest uncollected so far

                    ; high bytes were equal so test low bytes
    cpx TMPPTR      ; compare string pointer low byte with highest uncollected
                    ; string low byte
    bcc LAB_D5F6    ; if highest uncollected string is greater then go update
                    ; pointers, step to next and return

                    ; else set current string as highest uncollected string
LAB_D5E6:
    stx TMPPTR      ; save string pointer low byte as highest uncollected string
                    ; low byte
    sta TMPPTR+1    ; save string pointer high byte as highest uncollected
                    ; string high byte
    lda INDEX       ; get descriptor pointer low byte
    ldx INDEX+1     ; get descriptor pointer high byte
    sta DEFPNT      ; save working pointer high byte
    stx DEFPNT+1    ; save working pointer low byte
    lda FOUR6       ; get step size
    sta JMPER+1     ; copy step size
LAB_D5F6:
    lda FOUR6       ; get step size
    clc             ; clear carry for add
    adc INDEX       ; add pointer low byte
    sta INDEX       ; save pointer low byte
    bcc LAB_D601    ; if no rollover skip the high byte increment

    inc INDEX+1     ; else increment pointer high byte
LAB_D601:
    ldx INDEX+1     ; get pointer high byte
    ldy #$00        ; flag not moved
    rts


;***********************************************************************************;
;
; collect string

COLLECT:
    lda DEFPNT+1    ; get working pointer low byte
    ora DEFPNT      ; OR working pointer high byte
    beq LAB_D601    ; exit if nothing to collect

    lda JMPER+1     ; get copied step size
    and #$04        ; mask step size, $04 for variables, $00 for array or stack
    lsr             ; >> 1
    tay             ; copy to index
    sta JMPER+1     ; save offset to descriptor start
    lda (DEFPNT),Y  ; get string length low byte
    adc TMPPTR      ; add string start low byte
    sta GEN2PTR     ; set block end low byte
    lda TMPPTR+1    ; get string start high byte
    adc #$00        ; add carry
    sta GEN2PTR+1   ; set block end high byte
    lda FRETOP      ; get bottom of string space low byte
    ldx FRETOP+1    ; get bottom of string space high byte
    sta GENPTR      ; save destination end low byte
    stx GENPTR+1    ; save destination end high byte
    jsr MOVEBL      ; open up space in memory, don't set array end. this
                    ; copies the string from where it is to the end of the
                    ; uncollected string memory
    ldy JMPER+1     ; restore offset to descriptor start
    INY             ; increment index to string pointer low byte
    lda GENPTR      ; get new string pointer low byte
    sta (DEFPNT),Y  ; save new string pointer low byte
    tax             ; copy string pointer low byte
    inc GENPTR+1    ; increment new string pointer high byte
    lda GENPTR+1    ; get new string pointer high byte
    INY             ; increment index to string pointer high byte
    sta (DEFPNT),Y  ; save new string pointer high byte
    jmp LAB_D52A    ; re-run routine from last ending, .X.A holds new bottom
                    ; of string memory pointer


;***********************************************************************************;
;
; concatenate
; add strings, the first string is in the descriptor, the second string is in line

ADDSTR:
    lda FAC1+4      ; get descriptor pointer high byte
    pha             ; put on stack
    lda FAC1+3      ; get descriptor pointer low byte
    pha             ; put on stack
    jsr EVAL        ; get value from line
    jsr LAB_CD8F    ; check if source is string, else do type mismatch
    pla             ; get descriptor pointer low byte back
    sta ARISGN      ; set pointer low byte
    pla             ; get descriptor pointer high byte back
    sta FACOV       ; set pointer high byte
    ldy #$00        ; clear index
    lda (ARISGN),Y  ; get length of first string from descriptor
    clc             ; clear carry for add
    adc (FAC1+3),Y  ; add length of second string
    bcc LAB_D65D    ; if no overflow continue

    ldx #ER_STR2LONG    ; else error $17, string too long error
    jmp ERROR       ; do error #.X then warm start

LAB_D65D:
    jsr ALC1        ; copy descriptor pointer and make string space .A bytes long
    jsr XFERSTR     ; copy string from descriptor to utility pointer
    lda DSCPTN      ; get descriptor pointer low byte
    ldy DSCPTN+1    ; get descriptor pointer high byte
    jsr LAB_D6AA    ; pop (.Y.A) descriptor off stack or from top of string space
                    ; returns with .A = length, .X = pointer low byte,
                    ; .Y = pointer high byte
    jsr LAB_D68C    ; store string from pointer to utility pointer
    lda ARISGN      ; get descriptor pointer low byte
    ldy FACOV       ; get descriptor pointer high byte
    jsr LAB_D6AA    ; pop (.Y.A) descriptor off stack or from top of string space
                    ; returns with .A = length, .X = pointer low byte,
                    ; .Y = pointer high byte
    jsr LAB_D4CA    ; check space on descriptor stack then put string address
                    ; and length on descriptor stack and update stack pointers
    jmp LAB_CDB8    ; continue evaluation


;***********************************************************************************;
;
; copy string from descriptor to utility pointer

XFERSTR:
    ldy #$00        ; clear index
    lda (ARISGN),Y  ; get string length
    pha             ; save it
    INY             ; increment index
    lda (ARISGN),Y  ; get string pointer low byte
    tax             ; copy to .X
    INY             ; increment index
    lda (ARISGN),Y  ; get string pointer high byte
    tay             ; copy to .Y
    pla             ; get length back
LAB_D688:
    stx INDEX       ; save string pointer low byte
    sty INDEX+1     ; save string pointer high byte


;***********************************************************************************;
;
; store string from pointer to utility pointer

LAB_D68C:
    tay             ; copy length as index
    beq LAB_D699    ; branch if null string

    pha             ; save length
LAB_D690:
    dey             ; decrement length/index
    lda (INDEX),Y   ; get byte from string
    sta (FRESPC),Y  ; save byte to destination
    tya             ; copy length/index
    bne LAB_D690    ; loop if not all done yet

    pla             ; restore length
LAB_D699:
    clc             ; clear carry for add
    adc FRESPC      ; add string utility ptr low byte
    sta FRESPC      ; save string utility ptr low byte
    bcc LAB_D6A2    ; if no rollover skip the high byte increment

    inc FRESPC+1    ; increment string utility ptr high byte
LAB_D6A2:
    rts


;***********************************************************************************;
;
; evaluate string

DELST:
    jsr LAB_CD8F    ; check if source is string, else do type mismatch

; pop string off descriptor stack, or from top of string space
; returns with .A = length, .X = pointer low byte, .Y = pointer high byte

LAB_D6A6:
    lda FAC1+3      ; get descriptor pointer low byte
    ldy FAC1+4      ; get descriptor pointer high byte

; pop (.Y.A) descriptor off stack or from top of string space
; returns with .A = length, .X = pointer low byte, .Y = pointer high byte

LAB_D6AA:
    sta INDEX       ; save string pointer low byte
    sty INDEX+1     ; save string pointer high byte
    jsr DELTSD      ; clean descriptor stack, .Y.A = pointer
    php             ; save status flags
    ldy #$00        ; clear index
    lda (INDEX),Y   ; get length from string descriptor
    pha             ; put on stack
    INY             ; increment index
    lda (INDEX),Y   ; get string pointer low byte from descriptor
    tax             ; copy to .X
    INY             ; increment index
    lda (INDEX),Y   ; get string pointer high byte from descriptor
    tay             ; copy to .Y
    pla             ; get string length back
    plp             ; restore status
    bne LAB_D6D6    ; branch if pointer <> last_sl,last_sh

    cpy FRETOP+1    ; compare with bottom of string space high byte
    bne LAB_D6D6    ; branch if <>

    cpx FRETOP      ; else compare with bottom of string space low byte
    bne LAB_D6D6    ; branch if <>

    pha             ; save string length
    clc             ; clear carry for add
    adc FRETOP      ; add bottom of string space low byte
    sta FRETOP      ; set bottom of string space low byte
    bcc LAB_D6D5    ; skip increment if no overflow

    inc FRETOP+1    ; increment bottom of string space high byte
LAB_D6D5:
    pla             ; restore string length
LAB_D6D6:
    stx INDEX       ; save string pointer low byte
    sty INDEX+1     ; save string pointer high byte
    rts


;***********************************************************************************;
;
; clean descriptor stack, .Y.A = pointer
; checks if .A.Y is on the descriptor stack, if so does a stack discard

DELTSD:
    cpy LASTPT+1    ; compare high byte with current descriptor stack item
                    ; pointer high byte
    bne LAB_D6EB    ; exit if <>

    cmp LASTPT      ; compare low byte with current descriptor stack item
                    ; pointer low byte
    bne LAB_D6EB    ; exit if <>

    sta TEMPPT      ; set descriptor stack pointer
    sbc #$03        ; update last string pointer low byte
    sta LASTPT      ; save current descriptor stack item pointer low byte
    ldy #$00        ; clear high byte
LAB_D6EB:
    rts


;***********************************************************************************;
;
; perform CHR$()

CHR:
    jsr LAB_D7A1    ; evaluate byte expression, result in .X
    txa             ; copy to .A
    pha             ; save character
    lda #$01        ; string is single byte
    jsr LAB_D47D    ; make string space A bytes long
    pla             ; get character back
    ldy #$00        ; clear index
    sta (FAC1+1),Y  ; save byte in string - byte IS string!
    pla             ; dump return address (skip type check)
    pla             ; dump return address (skip type check)
    jmp LAB_D4CA    ; check space on descriptor stack then put string address
                    ; and length on descriptor stack and update stack pointers


;***********************************************************************************;
;
; perform LEFT$()

LEFT:
    jsr FINLMR      ; pull string data and byte parameter from stack
                    ; return pointer in descriptor, byte in .A (and .X), .Y=0
    cmp (DSCPTN),Y  ; compare byte parameter with string length
    tya             ; clear .A
LAB_D706:
    bcc LAB_D70C    ; branch if string length > byte parameter

    lda (DSCPTN),Y  ; else make parameter = length
    tax             ; copy to byte parameter copy
    tya             ; clear string start offset
LAB_D70C:
    pha             ; save string start offset
LAB_D70D:
    txa             ; copy byte parameter (or string length if <)
LAB_D70E:
    pha             ; save string length
    jsr LAB_D47D    ; make string space .A bytes long
    lda DSCPTN      ; get descriptor pointer low byte
    ldy DSCPTN+1    ; get descriptor pointer high byte
    jsr LAB_D6AA    ; pop (.Y.A) descriptor off stack or from top of string space
                    ; returns with .A = length, .X = pointer low byte,
                    ; .Y = pointer high byte
    pla             ; get string length back
    tay             ; copy length to .Y
    pla             ; get string start offset back
    clc             ; clear carry for add
    adc INDEX       ; add start offset to string start pointer low byte
    sta INDEX       ; save string start pointer low byte
    bcc LAB_D725    ; if no overflow skip the high byte increment

    inc INDEX+1     ; else increment string start pointer high byte
LAB_D725:
    tya             ; copy length to .A
    jsr LAB_D68C    ; store string from pointer to utility pointer
    jmp LAB_D4CA    ; check space on descriptor stack then put string address
                    ; and length on descriptor stack and update stack pointers


;***********************************************************************************;
;
; perform RIGHT$()

RIGHT:
    jsr FINLMR      ; pull string data and byte parameter from stack
                    ; return pointer in descriptor, byte in .A (and .X), .Y=0
    clc             ; clear carry for add - 1
    sbc (DSCPTN),Y  ; subtract string length
    eor #$FF        ; invert it (.A=LEN(expression$)-l)
    jmp LAB_D706    ; go do rest of LEFT$()


;***********************************************************************************;
;
; perform MID$()

MID:
    lda #$FF        ; set default length = 255
    sta FAC1+4      ; save default length
    jsr CHRGOT      ; scan memory
    cmp #$29        ; compare with ")"
    beq LAB_D748    ; branch if = ")" (skip second byte get)

    jsr COMCHK      ; scan for ",", else do syntax error then warm start
    jsr LAB_D79E    ; get byte parameter
LAB_D748:
    jsr FINLMR      ; pull string data and byte parameter from stack
                    ; return pointer in descriptor, byte in .A (and .X), .Y=0
    beq LAB_D798    ; if null do illegal quantity error then warm start

    dex             ; decrement start index
    txa             ; copy to .A
    pha             ; save string start offset
    clc             ; clear carry for sub - 1
    ldx #$00        ; clear output string length
    sbc (DSCPTN),Y  ; subtract string length
    bcs LAB_D70D    ; if start>string length go do null string

    eor #$FF        ; complement -length
    cmp FAC1+4      ; compare byte parameter
    bcc LAB_D70E    ; if length>remaining string go do RIGHT$

    lda FAC1+4      ; get length byte
    bcs LAB_D70E    ; go do string copy, branch always


;***********************************************************************************;
;
; pull string data and byte parameter from stack
; return pointer in descriptor, byte in .A (and .X), .Y=0

FINLMR:
    jsr RPACHK      ; scan for ")", else do syntax error then warm start
    pla             ; pull return address low byte
    tay             ; save return address low byte
    pla             ; pull return address high byte
    sta JMPER+1     ; save return address high byte
    pla             ; dump call to function vector low byte
    pla             ; dump call to function vector high byte
    pla             ; pull byte parameter
    tax             ; copy byte parameter to .X
    pla             ; pull string pointer low byte
    sta DSCPTN      ; save it
    pla             ; pull string pointer high byte
    sta DSCPTN+1    ; save it
    lda JMPER+1     ; get return address high byte
    pha             ; back on stack
    tya             ; get return address low byte
    pha             ; back on stack
    ldy #$00        ; clear index
    txa             ; copy byte parameter
    rts


;***********************************************************************************;
;
; perform LEN()

LEN:
    jsr GSINFO      ; evaluate string, get length in .A (and .Y)
    jmp LAB_D3A2    ; convert .Y to byte in FAC1 and return


;***********************************************************************************;
;
; evaluate string, get length in .Y

GSINFO:
    jsr DELST       ; evaluate string
    ldx #$00        ; set data type = numeric
    stx VALTYP      ; clear data type flag, $FF = string, $00 = numeric
    tay             ; copy length to .Y
    rts


;***********************************************************************************;
;
; perform ASC()

ASC:
    jsr GSINFO      ; evaluate string, get length in .A (and .Y)
    beq LAB_D798    ; if null do illegal quantity error then warm start

    ldy #$00        ; set index to first character
    lda (INDEX),Y   ; get byte
    tay             ; copy to .Y
    jmp LAB_D3A2    ; convert .Y to byte in FAC1 and return


;***********************************************************************************;
;
; do illegal quantity error then warm start

LAB_D798:
    jmp ILQUAN      ; do illegal quantity error then warm start


;***********************************************************************************;
;
; scan and get byte parameter

GETBYT:
    jsr CHRGET      ; increment and scan memory

; get byte parameter

LAB_D79E:
    jsr TYPCHK      ; evaluate expression and check is numeric, else do
                    ; type mismatch

; evaluate byte expression, result in .X

LAB_D7A1:
    jsr LAB_D1B8        ; evaluate integer expression, sign check

    ldx FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    bne LAB_D798        ; if not null do illegal quantity error then warm start

    ldx FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    jmp CHRGOT          ; scan memory and return


;***********************************************************************************;
;
; perform VAL()

VAL:
    jsr GSINFO      ; evaluate string, get length in .A (and .Y)
    bne LAB_D7B5    ; if not a null string go evaluate it

                    ; string was null so set result = $00
    jmp ZERFAC      ; clear FAC1 exponent and sign and return

LAB_D7B5:
    ldx CHRGOT+1    ; get BASIC execute pointer low byte
    ldy CHRGOT+2    ; get BASIC execute pointer high byte
    stx FBUFPT      ; save BASIC execute pointer low byte
    sty FBUFPT+1    ; save BASIC execute pointer high byte
    ldx INDEX       ; get string pointer low byte
    stx CHRGOT+1    ; save BASIC execute pointer low byte
    clc             ; clear carry for add
    adc INDEX       ; add string length
    sta INDEX+2     ; save string end low byte
    ldx INDEX+1     ; get string pointer high byte
    stx CHRGOT+2    ; save BASIC execute pointer high byte
    bcc LAB_D7CD    ; if no rollover skip the high byte increment

    inx             ; increment string end high byte
LAB_D7CD:
    stx INDEX+3     ; save string end high byte
    ldy #$00        ; set index to $00
    lda (INDEX+2),Y ; get string end byte
    pha             ; push it
    tya             ; clear .A
    sta (INDEX+2),Y ; terminate string with $00
    jsr CHRGOT      ; scan memory
    jsr ASCFLT      ; get FAC1 from string
    pla             ; restore string end byte
    ldy #$00        ; clear index
    sta (INDEX+2),Y ; put string end byte back

; restore BASIC execute pointer from temp

LAB_D7E2:
    ldx FBUFPT      ; get BASIC execute pointer low byte back
    ldy FBUFPT+1    ; get BASIC execute pointer high byte back
    stx CHRGOT+1    ; save BASIC execute pointer low byte
    sty CHRGOT+2    ; save BASIC execute pointer high byte
    rts


;***********************************************************************************;
;
; get parameters for POKE/WAIT

GETAD:
    jsr TYPCHK      ; evaluate expression and check is numeric, else do
                    ; type mismatch
    jsr MAKADR      ; convert FAC1 to integer in temporary integer
LAB_D7F1:
    jsr COMCHK      ; scan for ",", else do syntax error then warm start
    jmp LAB_D79E    ; get byte parameter and return


;***********************************************************************************;
;
; convert FAC1 to integer in temporary integer

MAKADR:
    lda FAC1+FAC_SIGN   ; get FAC1 sign
    bmi LAB_D798        ; if -ve do illegal quantity error then warm start

    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    cmp #$91            ; compare with exponent = 2^16
    bcs LAB_D798        ; if >= do illegal quantity error then warm start

    jsr FPINT           ; convert FAC1 floating to fixed
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    ldy FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    sty LINNUM          ; save temporary integer low byte
    sta LINNUM+1        ; save temporary integer high byte
    rts


;***********************************************************************************;
;
; perform PEEK()

PEEK:
    lda LINNUM+1    ; get line number high byte
    pha             ; save line number high byte
    lda LINNUM      ; get line number low byte
    pha             ; save line number low byte
    jsr MAKADR      ; convert FAC1 to integer in temporary integer
    ldy #$00        ; clear index
    lda (LINNUM),Y  ; read byte
    tay             ; copy byte to .A
    pla             ; pull byte
    sta LINNUM      ; restore line number low byte
    pla             ; pull byte
    sta LINNUM+1    ; restore line number high byte
    jmp LAB_D3A2    ; convert .Y to byte in FAC1 and return


;***********************************************************************************;
;
; perform POKE

POKE:
    jsr GETAD       ; get parameters for POKE/WAIT
    txa             ; copy byte to .A
    ldy #$00        ; clear index
    sta (LINNUM),Y  ; write byte
    rts


;***********************************************************************************;
;
; perform WAIT

WAIT:
    jsr GETAD       ; get parameters for POKE/WAIT
    stx FORPNT      ; save byte
    ldx #$00        ; clear mask
    jsr CHRGOT      ; scan memory
    beq LAB_D83C    ; skip if no third argument

    jsr LAB_D7F1    ; scan for "," and get byte, else syntax error then
                    ; warm start
LAB_D83C:
    stx FORPNT+1    ; save XOR argument
    ldy #$00        ; clear index
LAB_D840:
    lda (LINNUM),Y  ; get byte via temporary integer    (address)
    eor FORPNT+1    ; XOR with second argument      (mask)
    and FORPNT      ; and with first argument       (byte)
    beq LAB_D840    ; loop if result is zero

LAB_D848:
    rts


;***********************************************************************************;
;
; add 0.5 to FAC1 (round FAC1)

ADD05:
    lda #<FLP05     ; set 0.5 pointer low byte
    ldy #>FLP05     ; set 0.5 pointer high byte
    jmp LAPLUS      ; add (.A.Y) to FAC1


;***********************************************************************************;
;
; perform subtraction, FAC1 from (AY)

LAMIN:
    jsr LODARG      ; unpack memory (.A.Y) into FAC2

; perform subtraction, FAC1 from FAC2

SUB:
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    eor #$FF            ; complement it
    sta FAC1+FAC_SIGN   ; save FAC1 sign (b7)
    eor FAC2+FAC_SIGN   ; XOR with FAC2 sign (b7)
    sta ARISGN          ; save sign compare (FAC1 XOR FAC2)
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    jmp PLUS            ; add FAC2 to FAC1 and return

PLUS1:
    jsr LAB_D999        ; shift FAC.X .A times right (>8 shifts)
    bcc LAB_D8A3        ; go subtract the mantissas, branch always


;***********************************************************************************;
;
; add (.A.Y) to FAC1

LAPLUS:
    jsr LODARG      ; unpack memory (.A.Y) into FAC2

; add FAC2 to FAC1
PLUS:
    bne LAB_D86F    ; if FAC1 is not zero go do the add

    jmp ATOF        ; FAC1 was zero so copy FAC2 to FAC1 and return

; FAC1 is non zero
LAB_D86F:
    ldx FACOV       ; get FAC1 rounding byte
    stx JMPER+2     ; save as FAC2 rounding byte
    ldx #FAC2       ; set index to FAC2 exponent address
    lda FAC2        ; get FAC2 exponent
LAB_D877:
    tay             ; copy exponent
    beq LAB_D848    ; exit if zero

    sec                 ; set carry for subtract
    sbc FAC1+FAC_EXPT   ; subtract FAC1 exponent
    beq LAB_D8A3        ; if equal go add mantissas

    bcc LAB_D893        ; if FAC2 < FAC1 then go shift FAC2 right

                        ; else FAC2 > FAC1
    sty FAC1+FAC_EXPT   ; save FAC1 exponent
    ldy FAC2+FAC_SIGN   ; get FAC2 sign (b7)
    sty FAC1+FAC_SIGN   ; save FAC1 sign (b7)
    eor #$FF            ; complement .A
    adc #$00            ; +1, twos complement, carry is set
    ldy #$00            ; clear .Y
    sty JMPER+2         ; clear FAC2 rounding byte
    ldx #FAC1+FAC_EXPT  ; set index to FAC1 exponent address
    bne LAB_D897        ; branch always

; FAC2 < FAC1
LAB_D893:
    ldy #$00            ; clear .Y
    sty FACOV           ; clear FAC1 rounding byte
LAB_D897:
    cmp #$F9            ; compare exponent diff with $F9
    bmi PLUS1           ; branch if range $79-$F8

    tay                 ; copy exponent difference to .Y
    lda FACOV           ; get FAC1 rounding byte
    lsr FAC_MANT,X      ; shift FAC.X mantissa 1
    jsr LAB_D9B0        ; shift FAC.X .Y times right

; exponents are equal now do mantissa subtract
LAB_D8A3:
    bit ARISGN          ; test sign compare (FAC1 XOR FAC2)
    bpl NORMLZ          ; if = add FAC2 mantissa to FAC1 mantissa and return

    ldy #FAC1+FAC_EXPT  ; set index to FAC1 exponent address
    cpx #FAC2+FAC_EXPT  ; compare .X to FAC2 exponent address
    beq LAB_D8AF        ; branch if =

    ldy #FAC2+FAC_EXPT  ; else set index to FAC2 exponent address

; subtract smaller from bigger (take sign of bigger)
LAB_D8AF:
    sec                 ; set carry for subtract
    eor #$FF            ; ones complement .A
    adc JMPER+2         ; add FAC2 rounding byte
    sta FACOV           ; save FAC1 rounding byte
    lda FAC_MANT+3,Y    ; get FAC.Y mantissa 4
    sbc FAC_MANT+3,X    ; subtract FACX mantissa 4
    sta FAC1+FAC_MANT+3 ; save FAC1 mantissa 4
    lda FAC_MANT+2,Y    ; get FAC.Y mantissa 3
    sbc FAC_MANT+2,X    ; subtract FAC.X mantissa 3
    sta FAC1+FAC_MANT+2 ; save FAC1 mantissa 3
    lda FAC_MANT+1,Y    ; get FAC.Y mantissa 2
    sbc FAC_MANT+1,X    ; subtract FAC.X mantissa 2
    sta FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    lda FAC_MANT,Y      ; get FAC.Y mantissa 1
    sbc FAC_MANT,X      ; subtract FAC.X mantissa 1
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1


;***********************************************************************************;
;
; do ABS and normalise FAC1

LAB_D8D2:
    bcs LAB_D8D7        ; branch if number is +ve

    jsr COMFAC          ; negate FAC1

; normalise FAC1

LAB_D8D7:
    ldy #$00            ; clear .Y
    tya                 ; clear .A
    clc                 ; clear carry for add
LAB_D8DB:
    ldx FAC1+FAC_MANT   ; get FAC1 mantissa 1
    bne LAB_D929        ; if not zero normalise FAC1

    ldx FAC1+FAC_MANT+1 ; get FAC1 mantissa 2
    stx FAC1+FAC_MANT   ; save FAC1 mantissa 1
    ldx FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    stx FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    ldx FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    stx FAC1+FAC_MANT+2 ; save FAC1 mantissa 3
    ldx FACOV           ; get FAC1 rounding byte
    stx FAC1+FAC_MANT+3 ; save FAC1 mantissa 4
    sty FACOV           ; clear FAC1 rounding byte
    adc #$08            ; add x to exponent offset
    cmp #$20            ; compare with $20, max offset, all bits would be = 0
    bne LAB_D8DB        ; loop if not max


;***********************************************************************************;
;
; clear FAC1 exponent and sign

ZERFAC:
    lda #$00            ; clear .A
LAB_D8F9:
    sta FAC1+FAC_EXPT   ; set FAC1 exponent

; save FAC1 sign

LAB_D8FB:
    sta FAC1+FAC_SIGN   ; save FAC1 sign (b7)
    rts


;***********************************************************************************;
;
; add FAC2 mantissa to FAC1 mantissa

NORMLZ:
    adc JMPER+2         ; add FAC2 rounding byte
    sta FACOV           ; save FAC1 rounding byte
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    adc FAC2+FAC_MANT+3 ; add FAC2 mantissa 4
    sta FAC1+FAC_MANT+3 ; save FAC1 mantissa 4
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    adc FAC2+FAC_MANT+2 ; add FAC2 mantissa 3
    sta FAC1+FAC_MANT+2 ; save FAC1 mantissa 3
    lda FAC1+FAC_MANT+1 ; get FAC1 mantissa 2
    adc FAC2+FAC_MANT+1 ; add FAC2 mantissa 2
    sta FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    lda FAC1+FAC_MANT   ; get FAC1 mantissa 1
    adc FAC2+FAC_MANT   ; add FAC2 mantissa 1
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1
    jmp LAB_D936        ; test and normalise FAC1 for Cb=0/1

LAB_D91D:
    adc #$01            ; add 1 to exponent offset
    asl FACOV           ; shift FAC1 rounding byte
    ROL FAC1+FAC_MANT+3 ; shift FAC1 mantissa 4
    ROL FAC1+FAC_MANT+2 ; shift FAC1 mantissa 3
    ROL FAC1+FAC_MANT+1 ; shift FAC1 mantissa 2
    ROL FAC1+FAC_MANT   ; shift FAC1 mantissa 1


;***********************************************************************************;
;
; normalise FAC1

LAB_D929:
    bpl LAB_D91D        ; loop if not normalised

    sec                 ; set carry for subtract
    sbc FAC1+FAC_EXPT   ; subtract FAC1 exponent
    bcs ZERFAC          ; branch if underflow (set result = $0)

    eor #$FF            ; complement exponent
    adc #$01            ; +1 (twos complement)
    sta FAC1+FAC_EXPT   ; save FAC1 exponent

; test and normalise FAC1 for Cb=0/1

LAB_D936:
    bcc LAB_D946        ; exit if no overflow

; normalise FAC1 for Cb=1

LAB_D938:
    inc FAC1+FAC_EXPT   ; increment FAC1 exponent
    beq OVERFL          ; if zero do overflow error then warm start

    ror FAC1+FAC_MANT   ; shift FAC1 mantissa 1
    ror FAC1+FAC_MANT+1 ; shift FAC1 mantissa 2
    ror FAC1+FAC_MANT+2 ; shift FAC1 mantissa 3
    ror FAC1+FAC_MANT+3 ; shift FAC1 mantissa 4
    ror FACOV           ; shift FAC1 rounding byte
LAB_D946:
    rts


;***********************************************************************************;
;
; negate FAC1

COMFAC:
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    eor #$FF            ; complement it
    sta FAC1+FAC_SIGN   ; save FAC1 sign (b7)

; twos complement FAC1 mantissa

LAB_D94D:
    lda FAC1+FAC_MANT   ; get FAC1 mantissa 1
    eor #$FF            ; complement it
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1
    lda FAC1+FAC_MANT+1 ; get FAC1 mantissa 2
    eor #$FF            ; complement it
    sta FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    eor #$FF            ; complement it
    sta FAC1+FAC_MANT+2 ; save FAC1 mantissa 3
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    eor #$FF            ; complement it
    sta FAC1+FAC_MANT+3 ; save FAC1 mantissa 4
    lda FACOV           ; get FAC1 rounding byte
    eor #$FF            ; complement it
    sta FACOV           ; save FAC1 rounding byte
    inc FACOV           ; increment FAC1 rounding byte
    bne LAB_D97D        ; exit if no overflow

; increment FAC1 mantissa

LAB_D96F:
    inc FAC1+FAC_MANT+3 ; increment FAC1 mantissa 4
    bne LAB_D97D        ; finished if no rollover

    inc FAC1+FAC_MANT+2 ; increment FAC1 mantissa 3
    bne LAB_D97D        ; finished if no rollover

    inc FAC1+FAC_MANT+1 ; increment FAC1 mantissa 2
    bne LAB_D97D        ; finished if no rollover

    inc FAC1+FAC_MANT   ; increment FAC1 mantissa 1
LAB_D97D:
    rts


;***********************************************************************************;
;
; do overflow error then warm start

OVERFL:
    ldx #ER_OVFLOW      ; error $0F, overflow error
    jmp ERROR           ; do error #.X then warm start


;***********************************************************************************;
;
; shift FACtemp << A+8 times

ASRRES:
    ldx #$25            ; set offset to FACtemp
LAB_D985:
    ldy FAC_MANT+3,X    ; get FAC.X mantissa 4
    sty FACOV           ; save as FAC1 rounding byte
    ldy FAC_MANT+2,X    ; get FAC.X mantissa 3
    sty FAC_MANT+3,X    ; save FAC.X mantissa 4
    ldy FAC_MANT+1,X    ; get FAC.X mantissa 2
    sty FAC_MANT+2,X    ; save FAC.X mantissa 3
    ldy FAC_MANT,X      ; get FAC.X mantissa 1
    sty FAC_MANT+1,X    ; save FACX mantissa 2
    ldy BITS            ; get FAC1 overflow byte
    sty FAC_MANT,X      ; save FAC.X mantissa 1

; shift FAC.X -.A times right (> 8 shifts)

LAB_D999:
    adc #$08            ; add 8 to shift count
    bmi LAB_D985        ; go do 8 shift if still -ve

    beq LAB_D985        ; go do 8 shift if zero

    sbc #$08            ; else subtract 8 again
    tay                 ; save count to .Y
    lda FACOV           ; get FAC1 rounding byte
    bcs LAB_D9BA

LAB_D9A6:
    asl FAC_MANT,X      ; shift FAC.X mantissa 1
    bcc LAB_D9AC        ; branch if +ve

    inc FAC_MANT,X      ; this sets b7 eventually
LAB_D9AC:
    ror FAC_MANT,X      ; shift FAC.X mantissa 1 (correct for asl)
    ror FAC_MANT,X      ; shift FAC.X mantissa 1 (put carry in b7)

; shift FAC.X .Y times right

LAB_D9B0:
    ror FAC_MANT+1,X    ; shift FAC.X mantissa 2
    ror FAC_MANT+2,X    ; shift FAC.X mantissa 3
    ror FAC_MANT+3,X    ; shift FAC.X mantissa 4
    ror                 ; shift FAC.X rounding byte
    INY                 ; increment exponent diff
    bne LAB_D9A6        ; branch if range adjust not complete

LAB_D9BA:
    clc                 ; just clear it
    rts


;***********************************************************************************;
;
; constants and series for LOG(n)

FPC1:   .byte $81,$00,$00,$00,$00 ; 1

LOGCON: .byte $03         ; series counter
        .byte $7F,$5E,$56,$CB,$79
        .byte $80,$13,$9B,$0B,$64
        .byte $80,$76,$38,$93,$16
        .byte $82,$38,$AA,$3B,$20

LAB_D9D6: .byte $80,$35,$04,$F3,$34 ; 0.70711   1/root 2
LAB_D9DB: .byte $81,$35,$04,$F3,$34 ; 1.41421   root 2
LAB_D9E0: .byte $80,$80,$00,$00,$00 ; -0.5      1/2
LAB_D9E5: .byte $80,$31,$72,$17,$F8 ; 0.69315   LOG(2)


;***********************************************************************************;
;
; perform LOG()

LOG:
    jsr SGNFAC          ; test sign and zero
    beq LAB_D9F1        ; if zero do illegal quantity error then warm start

    bpl LAB_D9F4        ; skip error if +ve

LAB_D9F1:
    jmp ILQUAN          ; do illegal quantity error then warm start

LAB_D9F4:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    sbc #$7F            ; normalise it
    pha                 ; save it
    lda #$80            ; set exponent to zero
    sta FAC1+FAC_EXPT   ; save FAC1 exponent
    lda #<LAB_D9D6      ; pointer to 1/root 2 low byte
    ldy #>LAB_D9D6      ; pointer to 1/root 2 high byte
    jsr LAPLUS          ; add (.A.Y) to FAC1 (1/root2)
    lda #<LAB_D9DB      ; pointer to root 2 low byte
    ldy #>LAB_D9DB      ; pointer to root 2 high byte
    jsr LADIV           ; convert .A.Y and do (.A.Y)/FAC1 (root2/(x+(1/root2)))
    lda #<FPC1          ; pointer to 1 low byte
    ldy #>FPC1          ; pointer to 1 high byte
    jsr LAMIN           ; subtract FAC1 ((root2/(x+(1/root2)))-1) from (.A.Y)
    lda #<LOGCON        ; pointer to series for LOG(n) low byte
    ldy #>LOGCON        ; pointer to series for LOG(n) high byte
    jsr SEREVL          ; ^2 then series evaluation
    lda #<LAB_D9E0      ; pointer to -0.5 low byte
    ldy #>LAB_D9E0      ; pointer to -0.5 high byte
    jsr LAPLUS          ; add (.A.Y) to FAC1
    pla                 ; restore FAC1 exponent
    jsr ASCI8           ; evaluate new ASCII digit
    lda #<LAB_D9E5      ; pointer to LOG(2) low byte
    ldy #>LAB_D9E5      ; pointer to LOG(2) high byte

; do convert .A.Y, FAC1*(.A.Y)

TIMES:
    jsr LODARG          ; unpack memory (.A.Y) into FAC2
MULT:
    bne LAB_DA30        ; multiply FAC1 by FAC2 ??

    jmp LAB_DA8B        ; exit if zero

LAB_DA30:
    jsr MULDIV          ; test and adjust accumulators
    lda #$00            ; clear .A
    sta RESHO           ; clear temp mantissa 1
    sta RESHO+1         ; clear temp mantissa 2
    sta RESHO+2         ; clear temp mantissa 3
    sta RESHO+3         ; clear temp mantissa 4
    lda FACOV           ; get FAC1 rounding byte
    jsr TIMES3          ; go do shift/add FAC2
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    jsr TIMES3          ; go do shift/add FAC2
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    jsr TIMES3          ; go do shift/add FAC2
    lda FAC1+FAC_MANT+1 ; get FAC1 mantissa 2
    jsr TIMES3          ; go do shift/add FAC2
    lda FAC1+FAC_MANT   ; get FAC1 mantissa 1
    jsr LAB_DA5E        ; go do shift/add FAC2
    jmp LAB_DB8F        ; copy temp to FAC1, normalise and return

TIMES3:
    bne LAB_DA5E        ; branch if byte <> zero

    jmp ASRRES          ; shift FACtemp << .A+8 times

                        ; else do shift and add
LAB_DA5E:
    lsr                 ; shift byte
    ora #$80            ; set top bit (mark for 8 times)
LAB_DA61:
    tay                 ; copy result
    bcc LAB_DA7D        ; skip next if bit was zero

    clc                 ; clear carry for add
    lda RESHO+3         ; get temp mantissa 4
    adc FAC2+FAC_MANT+3 ; add FAC2 mantissa 4
    sta RESHO+3         ; save temp mantissa 4
    lda RESHO+2         ; get temp mantissa 3
    adc FAC2+FAC_MANT+2 ; add FAC2 mantissa 3
    sta RESHO+2         ; save temp mantissa 3
    lda RESHO+1         ; get temp mantissa 2
    adc FAC2+FAC_MANT+1 ; add FAC2 mantissa 2
    sta RESHO+1         ; save temp mantissa 2
    lda RESHO           ; get temp mantissa 1
    adc FAC2+FAC_MANT   ; add FAC2 mantissa 1
    sta RESHO           ; save temp mantissa 1
LAB_DA7D:
    ror RESHO           ; shift temp mantissa 1
    ror RESHO+1         ; shift temp mantissa 2
    ror RESHO+2         ; shift temp mantissa 3
    ror RESHO+3         ; shift temp mantissa 4
    ror FACOV           ; shift temp rounding byte
    tya                 ; get byte back
    lsr                 ; shift byte
    bne LAB_DA61        ; loop if all bits not done

LAB_DA8B:
    rts


;***********************************************************************************;
;
; unpack memory (.A.Y) into FAC2

LODARG:
    sta INDEX           ; save pointer low byte
    sty INDEX+1         ; save pointer high byte
    ldy #$04            ; 5 bytes to get (0-4)
    lda (INDEX),Y       ; get mantissa 4
    sta FAC2+FAC_MANT+3 ; save FAC2 mantissa 4
    dey                 ; decrement index
    lda (INDEX),Y       ; get mantissa 3
    sta FAC2+FAC_MANT+2 ; save FAC2 mantissa 3
    dey                 ; decrement index
    lda (INDEX),Y       ; get mantissa 2
    sta FAC2+FAC_MANT+1 ; save FAC2 mantissa 2
    dey                 ; decrement index
    lda (INDEX),Y       ; get mantissa 1 + sign
    sta FAC2+FAC_SIGN   ; save FAC2 sign (b7)
    eor FAC1+FAC_SIGN   ; XOR with FAC1 sign (b7)
    sta ARISGN          ; save sign compare (FAC1 XOR FAC2)
    lda FAC2+FAC_SIGN   ; recover FAC2 sign (b7)
    ora #$80            ; set 1xxx xxx (set normal bit)
    sta FAC2+FAC_MANT   ; save FAC2 mantissa 1
    dey                 ; decrement index
    lda (INDEX),Y       ; get exponent byte
    sta FAC2+FAC_EXPT   ; save FAC2 exponent
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    rts


;***********************************************************************************;
;
; test and adjust accumulators

MULDIV:
    lda FAC2+FAC_EXPT   ; get FAC2 exponent

LAB_DAB9:
    beq LAB_DADA        ; branch if FAC2 = $00 (handle underflow)

    clc                 ; clear carry for add
    adc FAC1+FAC_EXPT   ; add FAC1 exponent
    bcc LAB_DAC4        ; branch if sum of exponents < $0100

    bmi LAB_DADF        ; do overflow error

    clc                 ; clear carry for the add
    .byte   $2C         ; makes next line bit $1410
LAB_DAC4:
    bpl LAB_DADA        ; if +ve go handle underflow

    adc #$80            ; adjust exponent
    sta FAC1+FAC_EXPT   ; save FAC1 exponent
    bne LAB_DACF        ; branch if not zero

    jmp LAB_D8FB        ; save FAC1 sign and return


LAB_DACF:
    lda ARISGN          ; get sign compare (FAC1 XOR FAC2)
    sta FAC1+FAC_SIGN   ; save FAC1 sign (b7)
    rts


;***********************************************************************************;
;
; handle overflow and underflow

LAB_DAD4:
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    eor #$FF            ; complement it
    bmi LAB_DADF        ; do overflow error

; handle underflow
LAB_DADA:
    pla                 ; pop return address low byte
    pla                 ; pop return address high byte
    jmp ZERFAC          ; clear FAC1 exponent and sign and return

LAB_DADF:
    jmp OVERFL          ; do overflow error then warm start


;***********************************************************************************;
;
; multiply FAC1 by 10

MULTEN:
    jsr RFTOA           ; round and copy FAC1 to FAC2
    tax                 ; copy exponent (set the flags)
    beq LAB_DAF8        ; exit if zero

    clc                 ; clear carry for add
    adc #$02            ; add two to exponent (*4)
    bcs LAB_DADF        ; do overflow error if > $FF

; FAC1 = (FAC1 + FAC2) * 2

LAB_DAED:
    ldx #$00            ; clear byte
    stx ARISGN          ; clear sign compare (FAC1 XOR FAC2)
    jsr LAB_D877        ; add FAC2 to FAC1 (*5)
    inc FAC1+FAC_EXPT       ; increment FAC1 exponent (*10)
    beq LAB_DADF        ; if exponent now zero go do overflow error

LAB_DAF8:
    rts


;***********************************************************************************;
;
; 10 as a floating value

FPCTEN: .byte $84,$20,$00,$00,$00 ; 10


;***********************************************************************************;
;
; divide FAC1 by 10

DIVTEN:
    jsr RFTOA       ; round and copy FAC1 to FAC2
    lda #<FPCTEN    ; set 10 pointer low byte
    ldy #>FPCTEN    ; set 10 pointer high byte
    ldx #$00        ; clear sign

; divide by (.A.Y) (X=sign)

LAB_DB07:
    stx ARISGN      ; save sign compare (FAC1 XOR FAC2)
    jsr LODFAC      ; unpack memory (.A.Y) into FAC1
    jmp DIVIDE      ; do FAC2/FAC1

                    ; Perform divide-by

; convert .A.Y and do (.A.Y)/FAC1

LADIV:
    jsr LODARG          ; unpack memory (.A.Y) into FAC2
DIVIDE:
    beq LAB_DB8A        ; if zero go do /0 error

    jsr ROUND           ; round FAC1
    lda #$00            ; clear .A
    sec                 ; set carry for subtract
    sbc FAC1+FAC_EXPT   ; subtract FAC1 exponent (2s complement)
    sta FAC1+FAC_EXPT   ; save FAC1 exponent
    jsr MULDIV          ; test and adjust accumulators
    inc FAC1+FAC_EXPT   ; increment FAC1 exponent
    beq LAB_DADF        ; if zero do overflow error

    ldx #$FC            ; set index to FAC temp
    lda #$01            ; set byte
LAB_DB29:
    ldy FAC2+FAC_MANT   ; get FAC2 mantissa 1
    cpy FAC1+FAC_MANT   ; compare FAC1 mantissa 1
    bne LAB_DB3F        ; if <> go use the result

    ldy FAC2+FAC_MANT+1 ; get FAC2 mantissa 2
    cpy FAC1+FAC_MANT+1 ; compare FAC1 mantissa 2
    bne LAB_DB3F        ; if <> go use the result

    ldy FAC2+FAC_MANT+2 ; get FAC2 mantissa 3
    cpy FAC1+FAC_MANT+2 ; compare FAC1 mantissa 3
    bne LAB_DB3F        ; if <> go use the result

    ldy FAC2+FAC_MANT+3 ; get FAC2 mantissa 4
    cpy FAC1+FAC_MANT+3 ; compare FAC1 mantissa 4
LAB_DB3F:
    php                 ; save the FAC2-FAC1 compare status
    ROL                 ; shift byte
    bcc LAB_DB4C        ; skip next if no carry

    inx                 ; increment index to FAC temp
    sta RESHO+3,X
    beq LAB_DB7A

    bpl LAB_DB7E

    lda #$01
LAB_DB4C:
    plp                 ; restore FAC2-FAC1 compare status
    bcs LAB_DB5D        ; if FAC2 >= FAC1 then do subtract

; FAC2 = FAC2*2
LAB_DB4F:
    asl FAC2+FAC_MANT+3 ; shift FAC2 mantissa 4
    ROL FAC2+FAC_MANT+2 ; shift FAC2 mantissa 3
    ROL FAC2+FAC_MANT+1 ; shift FAC2 mantissa 2
    ROL FAC2+FAC_MANT   ; shift FAC2 mantissa 1
    bcs LAB_DB3F        ; loop with no compare

    bmi LAB_DB29        ; loop with compare

    bpl LAB_DB3F        ; loop always with no compare

LAB_DB5D:
    tay                 ; save FAC2-FAC1 compare status
    lda FAC2+FAC_MANT+3 ; get FAC2 mantissa 4
    sbc FAC1+FAC_MANT+3 ; subtract FAC1 mantissa 4
    sta FAC2+FAC_MANT+3 ; save FAC2 mantissa 4
    lda FAC2+FAC_MANT+2 ; get FAC2 mantissa 3
    sbc FAC1+FAC_MANT+2 ; subtract FAC1 mantissa 3
    sta FAC2+FAC_MANT+2 ; save FAC2 mantissa 3
    lda FAC2+FAC_MANT+1 ; get FAC2 mantissa 2
    sbc FAC1+FAC_MANT+1 ; subtract FAC1 mantissa 2
    sta FAC2+FAC_MANT+1 ; save FAC2 mantissa 2
    lda FAC2+FAC_MANT   ; get FAC2 mantissa 1
    sbc FAC1+FAC_MANT   ; subtract FAC1 mantissa 1
    sta FAC2+FAC_MANT   ; save FAC2 mantissa 1
    tya                 ; restore FAC2-FAC1 compare status
    jmp LAB_DB4F        ; go shift FAC2

LAB_DB7A:
    lda #$40
    bne LAB_DB4C        ; branch always

; do .A<<6, save as FAC1 rounding byte, normalise and return

LAB_DB7E:
    asl
    asl
    asl
    asl
    asl
    asl
    sta FACOV           ; save FAC1 rounding byte
    plp                 ; dump FAC2-FAC1 compare status
    jmp LAB_DB8F        ; copy temp to FAC1, normalise and return

; do "Divide by zero" error

LAB_DB8A:
    ldx #ER_DIVBY0      ; error $14, divide by zero error
    jmp ERROR           ; do error #.X then warm start

LAB_DB8F:
    lda RESHO           ; get temp mantissa 1
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1
    lda RESHO+1         ; get temp mantissa 2
    sta FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    lda RESHO+2         ; get temp mantissa 3
    sta FAC1+FAC_MANT+2 ; save FAC1 mantissa 3
    lda RESHO+3         ; get temp mantissa 4
    sta FAC1+FAC_MANT+3 ; save FAC1 mantissa 4
    jmp LAB_D8D7        ; normalise FAC1 and return


;***********************************************************************************;
;
; unpack memory (.A.Y) into FAC1

LODFAC:
    sta INDEX           ; save pointer low byte
    sty INDEX+1         ; save pointer high byte
    ldy #$04            ; 5 bytes to do
    lda (INDEX),Y       ; get fifth byte
    sta FAC1+FAC_MANT+3 ; save FAC1 mantissa 4
    dey                 ; decrement index
    lda (INDEX),Y       ; get fourth byte
    sta FAC1+FAC_MANT+2 ; save FAC1 mantissa 3
    dey                 ; decrement index
    lda (INDEX),Y       ; get third byte
    sta FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    dey                 ; decrement index
    lda (INDEX),Y       ; get second byte
    sta FAC1+FAC_SIGN   ; save FAC1 sign (b7)
    ora #$80            ; set 1xxx xxxx (add normal bit)
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1
    dey                 ; decrement index
    lda (INDEX),Y       ; get first byte (exponent)
    sta FAC1+FAC_EXPT   ; save FAC1 exponent
    sty FACOV           ; clear FAC1 rounding byte
    rts


;***********************************************************************************;
;
; pack FAC1 into LAB_5C

FACTF2:
    ldx #<LAB_5C        ; set pointer low byte
    .byte   $2C         ; makes next line bit $57A2

; pack FAC1 into TEMPF3

FACTF1:
    ldx #<TEMPF3        ; set pointer low byte
    ldy #>TEMPF3        ; set pointer high byte
    beq STORFAC         ; pack FAC1 into (.X.Y) and return, branch always

; pack FAC1 into variable pointer

FACTFP:
    ldx FORPNT          ; get destination pointer low byte
    ldy FORPNT+1        ; get destination pointer high byte

; pack FAC1 into (.X.Y)

STORFAC:
    jsr ROUND           ; round FAC1
    stx INDEX           ; save pointer low byte
    sty INDEX+1         ; save pointer high byte
    ldy #$04            ; set index
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    sta (INDEX),Y       ; store in destination
    dey                 ; decrement index
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    sta (INDEX),Y       ; store in destination
    dey                 ; decrement index
    lda FAC1+FAC_MANT+1 ; get FAC1 mantissa 2
    sta (INDEX),Y       ; store in destination
    dey                 ; decrement index
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    ora #$7F            ; set bits x111 1111
    and FAC1+FAC_MANT   ; and in FAC1 mantissa 1
    sta (INDEX),Y       ; store in destination
    dey                 ; decrement index
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    sta (INDEX),Y       ; store in destination
    sty FACOV           ; clear FAC1 rounding byte
    rts


;***********************************************************************************;
;
; copy FAC2 to FAC1

ATOF:
    lda FAC2+FAC_SIGN   ; get FAC2 sign (b7)

; save FAC1 sign and copy ABS(FAC2) to FAC1

LAB_DBFE:
    sta FAC1+FAC_SIGN   ; save FAC1 sign (b7)
    ldx #$05            ; 5 bytes to copy
LAB_DC02:
    lda FAC2-1,X        ; get byte from FAC2,X
    sta FAC1-1,X        ; save byte at FAC1,X
    dex                 ; decrement count
    bne LAB_DC02        ; loop if not all done

    stx FACOV           ; clear FAC1 rounding byte
    rts


;***********************************************************************************;
;
; round and copy FAC1 to FAC2

RFTOA:
    jsr ROUND       ; round FAC1

; copy FAC1 to FAC2

FTOA:
    ldx #$06        ; 6 bytes to copy
LAB_DC11:
    lda FAC1-1,X    ; get byte from FAC1,X
    sta FAC2-1,X    ; save byte at FAC2,X
    dex             ; decrement count
    bne LAB_DC11    ; loop if not all done

    stx FACOV       ; clear FAC1 rounding byte
LAB_DC1A:
    rts


;***********************************************************************************;
;
; round FAC1

ROUND:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    beq LAB_DC1A        ; exit if zero

    asl FACOV           ; shift FAC1 rounding byte
    bcc LAB_DC1A        ; exit if no overflow

; round FAC1 (no check)

LAB_DC23:
    jsr LAB_D96F        ; increment FAC1 mantissa
    bne LAB_DC1A        ; branch if no overflow

    jmp LAB_D938        ; normalise FAC1 for Cb=1 and return

; get FAC1 sign
; return .A = $FF, Cb = 1/-ve .A = $01, Cb = 0/+ve, .A = $00, Cb = ?/0

SGNFAC:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    beq LAB_DC38        ; exit if zero (already correct SGN(0)=0)

; return .A = $FF, Cb = 1/-ve .A = $01, Cb = 0/+ve
; no = 0 check

LAB_DC2F:
    lda FAC1+FAC_SIGN   ; else get FAC1 sign (b7)

; return .A = $FF, Cb = 1/-ve .A = $01, Cb = 0/+ve
; no = 0 check, sign in .A

LAB_DC31:
    ROL                 ; move sign bit to carry
    lda #$FF            ; set byte for -ve result
    bcs LAB_DC38        ; return if sign was set (-ve)

    lda #$01            ; else set byte for +ve result
LAB_DC38:
    rts


;***********************************************************************************;
;
; perform SGN()

SGN:
    jsr SGNFAC          ; get FAC1 sign, return .A = $FF -ve, .A = $01 +ve

; save .A as integer byte

INTFP:
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1
    lda #$00            ; clear A
    sta FAC1+FAC_MANT+1 ; clear FAC1 mantissa 2
    ldx #$88            ; set exponent

; set exponent = .X, clear FAC1 3 and 4 and normalise

INTFP1:
    lda FAC1+FAC_MANT   ; get FAC1 mantissa 1
    eor #$FF            ; complement it
    ROL                 ; sign bit into carry

; set exponent = .X, clear mantissa 4 and 3 and normalise FAC1

LAB_DC49:
    lda #$00            ; clear .A
    sta FAC1+FAC_MANT+3 ; clear FAC1 mantissa 4
    sta FAC1+FAC_MANT+2 ; clear FAC1 mantissa 3

; set exponent = .X and normalise FAC1

LAB_DC4F:
    stx FAC1+FAC_EXPT   ; set FAC1 exponent
    sta FACOV           ; clear FAC1 rounding byte
    sta FAC1+FAC_SIGN   ; clear FAC1 sign (b7)
    jmp LAB_D8D2        ; do ABS and normalise FAC1

; perform ABS()

ABS:
    lsr FAC1+FAC_SIGN   ; clear FAC1 sign, put zero in b7
    rts


;***********************************************************************************;
;
; compare FAC1 with (.A.Y)
; returns .A=$00 if FAC1 = (.A.Y)
; returns .A=$01 if FAC1 > (.A.Y)
; returns .A=$FF if FAC1 < (.A.Y)

CMPFAC:
    sta INDEX+2         ; save pointer low byte
LAB_DC5D:
    sty INDEX+3         ; save pointer high byte
    ldy #$00            ; clear index
    lda (INDEX+2),Y     ; get exponent
    INY                 ; increment index
    tax                 ; copy (.A.Y) exponent to .X
    beq SGNFAC          ; branch if (.A.Y) exponent=0 and get FAC1 sign
                        ; .A = $FF, Cb = 1/-ve .A = $01, Cb = 0/+ve

    lda (INDEX+2),Y     ; get (.A.Y) mantissa 1, with sign
    eor FAC1+FAC_SIGN   ; XOR FAC1 sign (b7)
    bmi LAB_DC2F        ; if signs <> do return .A = $FF, Cb = 1/-ve
                        ; .A = $01, Cb = 0/+ve and return

    cpx FAC1+FAC_EXPT   ; compare (.A.Y) exponent with FAC1 exponent
    bne LAB_DC92        ; branch if different

    lda (INDEX+2),Y     ; get (.A.Y) mantissa 1, with sign
    ora #$80            ; normalise top bit
    cmp FAC1+FAC_MANT   ; compare with FAC1 mantissa 1
    bne LAB_DC92        ; branch if different

    INY                 ; increment index
    lda (INDEX+2),Y     ; get mantissa 2
    cmp FAC1+FAC_MANT+1 ; compare with FAC1 mantissa 2
    bne LAB_DC92        ; branch if different

    INY                 ; increment index
    lda (INDEX+2),Y     ; get mantissa 3
    cmp FAC1+FAC_MANT+2 ; compare with FAC1 mantissa 3
    bne LAB_DC92        ; branch if different

    INY                 ; increment index
    lda #$7F            ; set for 1/2 value rounding byte
    cmp FACOV           ; compare with FAC1 rounding byte (set carry)
    lda (INDEX+2),Y     ; get mantissa 4
    sbc FAC1+FAC_MANT+3 ; subtract FAC1 mantissa 4
    beq LAB_DCBA        ; exit if mantissa 4 equal

; gets here if number <> FAC1

LAB_DC92:
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    bcc LAB_DC98        ; branch if FAC1 > (.A.Y)

    eor #$FF            ; else toggle FAC1 sign
LAB_DC98:
    jmp LAB_DC31        ; return .A = $FF, Cb = 1/-ve .A = $01, Cb = 0/+ve


;***********************************************************************************;
;
; convert FAC1 floating to fixed

FPINT:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    beq FILFAC          ; if zero go clear FAC1 and return

    sec                 ; set carry for subtract
    sbc #$A0            ; subtract maximum integer range exponent
    bit FAC1+FAC_SIGN   ; test FAC1 sign (b7)
    bpl LAB_DCAF        ; branch if FAC1 +ve

                        ; FAC1 was -ve
    tax                 ; copy subtracted exponent
    lda #$FF            ; overflow for -ve number
    sta BITS            ; set FAC1 overflow byte
    jsr LAB_D94D        ; twos complement FAC1 mantissa
    txa                 ; restore subtracted exponent
LAB_DCAF:
    ldx #FAC1           ; set index to FAC1
    cmp #$F9            ; compare exponent result
    bpl LAB_DCBB        ; if < 8 shifts shift FAC1 .A times right and return

    jsr LAB_D999        ; shift FAC1 .A times right (> 8 shifts)
    sty BITS            ; clear FAC1 overflow byte
LAB_DCBA:
    rts


;***********************************************************************************;
;
; shift FAC1 .A times right

LAB_DCBB:
    tay                 ; copy shift count
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    and #$80            ; mask sign bit only (x000 0000)
    lsr FAC1+FAC_MANT   ; shift FAC1 mantissa 1
    ora FAC1+FAC_MANT   ; OR sign in b7 FAC1 mantissa 1
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1
    jsr LAB_D9B0        ; shift FAC1 .Y times right
    sty BITS            ; clear FAC1 overflow byte
    rts


;***********************************************************************************;
;
; perform INT()

INT:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    cmp #$A0            ; compare with max int
    bcs LAB_DCF2        ; exit if >= (already int, too big for fractional part!)

    jsr FPINT           ; convert FAC1 floating to fixed
    sty FACOV           ; save FAC1 rounding byte
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    sty FAC1+FAC_SIGN   ; save FAC1 sign (b7)
    eor #$80            ; toggle FAC1 sign
    ROL                 ; shift into carry
    lda #$A0            ; set new exponent
    sta FAC1+FAC_EXPT   ; save FAC1 exponent
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    sta CHARAC          ; save FAC1 mantissa 4 for power function
    jmp LAB_D8D2        ; do ABS and normalise FAC1


;***********************************************************************************;
;
; clear FAC1 and return

FILFAC:
    sta FAC1+FAC_MANT   ; clear FAC1 mantissa 1
    sta FAC1+FAC_MANT+1 ; clear FAC1 mantissa 2
    sta FAC1+FAC_MANT+2 ; clear FAC1 mantissa 3
    sta FAC1+FAC_MANT+3 ; clear FAC1 mantissa 4
    tay                 ; clear .Y
LAB_DCF2:
    rts


;***********************************************************************************;
;
; get FAC1 from string

ASCFLT:
    ldy #$00        ; clear .Y
    ldx #$0A        ; set index
LAB_DCF7:
    sty LAB_5D,X    ; clear byte
    dex             ; decrement index
    bpl LAB_DCF7    ; loop until numexp to negnum (and FAC1) = $00

    bcc LAB_DD0D    ; branch if first character is numeric

    cmp #'-'        ; else compare with "-"
    bne LAB_DD06    ; branch if not "-"

    stx SGNFLG      ; set flag for -ve n (negnum = $FF)
    beq LAB_DD0A    ; branch always

LAB_DD06:
    cmp #'+'        ; else compare with "+"
    bne LAB_DD0F    ; branch if not "+"

LAB_DD0A:
    jsr CHRGET      ; increment and scan memory
LAB_DD0D:
    bcc LAB_DD6A    ; branch if numeric character

LAB_DD0F:
    cmp #'.'        ; else compare with "."
    beq LAB_DD41    ; branch if "."

    cmp #'E'        ; else compare with "E"
    bne LAB_DD47    ; branch if not "E"

                    ; was "E" so evaluate exponential part
    jsr CHRGET      ; increment and scan memory
    bcc LAB_DD33    ; branch if numeric character

    cmp #TK_MINUS   ; else compare with token for -
    beq LAB_DD2E    ; branch if token for -

    cmp #'-'        ; else compare with "-"
    beq LAB_DD2E    ; branch if "-"

    cmp #TK_PLUS    ; else compare with token for +
    beq LAB_DD30    ; branch if token for +

    cmp #'+'        ; else compare with "+"
    beq LAB_DD30    ; branch if "+"

    bne LAB_DD35    ; branch always

LAB_DD2E:
    ror TMPPTR+1    ; set exponent -ve flag (C, which=1, into b7)
LAB_DD30:
    jsr CHRGET      ; increment and scan memory
LAB_DD33:
    bcc LAB_DD91    ; branch if numeric character

LAB_DD35:
    bit TMPPTR+1    ; test exponent -ve flag
    bpl LAB_DD47    ; if +ve go evaluate exponent

                    ; else do exponent = -exponent
    lda #$00        ; clear result
    sec             ; set carry for subtract
    sbc EXPCNT      ; subtract exponent byte
    jmp LAB_DD49    ; go evaluate exponent

LAB_DD41:
    ror TMPPTR      ; set decimal point flag
    bit TMPPTR      ; test decimal point flag
    BVC LAB_DD0A    ; branch if only one decimal point so far

                    ; evaluate exponent
LAB_DD47:
    lda EXPCNT      ; get exponent count byte
LAB_DD49:
    sec             ; set carry for subtract
    sbc LAB_5D      ; subtract numerator exponent
    sta EXPCNT      ; save exponent count byte
    beq LAB_DD62    ; branch if no adjustment

    bpl LAB_DD5B    ; else if +ve go do FAC1*10^expcnt

                    ; else go do FAC1/10^(0-expcnt)
LAB_DD52:
    jsr DIVTEN      ; divide FAC1 by 10
    inc EXPCNT      ; increment exponent count byte
    bne LAB_DD52    ; loop until all done

    beq LAB_DD62    ; branch always

LAB_DD5B:
    jsr MULTEN      ; multiply FAC1 by 10
    dec EXPCNT      ; decrement exponent count byte
    bne LAB_DD5B    ; loop until all done

LAB_DD62:
    lda SGNFLG      ; get -ve flag
    bmi LAB_DD67    ; if -ve do - FAC1 and return

    rts

; do - FAC1 and return

LAB_DD67:
    jmp NEGFAC      ; do - FAC1

; do unsigned FAC1*10+number

LAB_DD6A:
    pha             ; save character
    bit TMPPTR      ; test decimal point flag
    bpl LAB_DD71    ; skip exponent increment if not set

    inc LAB_5D      ; else increment number exponent
LAB_DD71:
    jsr MULTEN      ; multiply FAC1 by 10
    pla             ; restore character
    sec             ; set carry for subtract
    sbc #'0'        ; convert to binary
    jsr ASCI8       ; evaluate new ASCII digit
    jmp LAB_DD0A    ; go do next character

; evaluate new ASCII digit
; multiply FAC1 by 10 then (ABS) add in new digit

ASCI8:
    pha                 ; save digit
    jsr RFTOA           ; round and copy FAC1 to FAC2
    pla                 ; restore digit
    jsr INTFP           ; save .A as integer byte
    lda FAC2+FAC_SIGN   ; get FAC2 sign (b7)
    eor FAC1+FAC_SIGN   ; toggle with FAC1 sign (b7)
    sta ARISGN          ; save sign compare (FAC1 XOR FAC2)
    ldx FAC1+FAC_EXPT   ; get FAC1 exponent
    jmp PLUS            ; add FAC2 to FAC1 and return

; evaluate next character of exponential part of number

LAB_DD91:
    lda EXPCNT          ; get exponent count byte
    cmp #$0A            ; compare with 10 decimal
    bcc LAB_DDA0        ; branch if less

    lda #$64            ; make all -ve exponents = -100 decimal (causes underflow)
    bit TMPPTR+1        ; test exponent -ve flag
    bmi LAB_DDAE        ; branch if -ve

    jmp OVERFL          ; else do overflow error then warm start

LAB_DDA0:
    asl                 ; *2
    asl                 ; *4
    clc                 ; clear carry for add
    adc EXPCNT          ; *5
    asl                 ; *10
    clc                 ; clear carry for add
    ldy #$00            ; set index
    adc (CHRGOT+1),Y    ; add character (will be $30 too much!)
    sec                 ; set carry for subtract
    sbc #'0'            ; convert character to binary
LAB_DDAE:
    sta EXPCNT          ; save exponent count byte
    jmp LAB_DD30        ; go get next character


;***********************************************************************************;
;
; 99999999.90625, maximum value with at least one decimal
FPC12:      .byte $9B,$3E,$BC,$1F,$FD
; 999999999.25, maximum value before scientific notation
LAB_DDB8:   .byte $9E,$6E,$6B,$27,$FD
; 1000000000
LAB_DDBD:   .byte $9E,$6E,$6B,$28,$00


;***********************************************************************************;
;
; do " IN " line number message

PRTIN:
    lda #<INSTR     ; set " IN " pointer low byte
    ldy #>INSTR     ; set " IN " pointer high byte
    jsr LAB_DDDA    ; print null terminated string
    lda CURLIN+1    ; get the current line number high byte
    ldx CURLIN      ; get the current line number low byte


;***********************************************************************************;
;
; print .X.A as unsigned integer

PRTFIX:
    sta FAC1+FAC_MANT   ; save high byte as FAC1 mantissa 1
    stx FAC1+FAC_MANT+1 ; save low byte as FAC1 mantissa 2
    ldx #$90            ; set exponent to 16d bits
    sec                 ; set integer is +ve flag
    jsr LAB_DC49        ; set exponent = .X, clear mantissa 4 and 3 and normalise
                        ; FAC1
    jsr LAB_DDDF        ; convert FAC1 to string
LAB_DDDA:
    jmp PRTSTR          ; print null terminated string


;***********************************************************************************;
;
; convert FAC1 to ASCII string result in (.A.Y)

FLTASC:
    ldy #$01            ; set index = 1
LAB_DDDF:
    lda #' '            ; character = " " (assume +ve)
    bit FAC1+FAC_SIGN   ; test FAC1 sign (b7)
    bpl LAB_DDE7        ; if +ve skip the - sign set

    lda #'-'            ; else character = "-"
LAB_DDE7:
    sta BASZPT,Y        ; save leading character (" " or "-")
    sta FAC1+FAC_SIGN   ; save FAC1 sign (b7)
    sty FBUFPT          ; save the index
    INY                 ; increment index
    lda #'0'            ; set character = "0"
    ldx FAC1+FAC_EXPT   ; get FAC1 exponent
    bne LAB_DDF8        ; if FAC1<>0 go convert it

                        ; exponent was $00 so FAC1 is 0
    jmp LAB_DF04        ; save last character, [EOT] and exit

; FAC1 is some non zero value

LAB_DDF8:
    lda #$00        ; clear (number exponent count)
    cpx #$80        ; compare FAC1 exponent with $80 (<1.00000)
    beq LAB_DE00    ; branch if 0.5 <= FAC1 < 1.0

    bcs LAB_DE09    ; branch if FAC1=>1

LAB_DE00:
    lda #<LAB_DDBD  ; set 1000000000 pointer low byte
    ldy #>LAB_DDBD  ; set 1000000000 pointer high byte
    jsr TIMES       ; do convert .A.Y, FAC1*(.A.Y)
    lda #$F7        ; set number exponent count
LAB_DE09:
    sta LAB_5D      ; save number exponent count
LAB_DE0B:
    lda #<LAB_DDB8  ; set 999999999.25 pointer low byte (max before sci note)
    ldy #>LAB_DDB8  ; set 999999999.25 pointer high byte
    jsr CMPFAC      ; compare FAC1 with (.A.Y)
    beq LAB_DE32    ; exit if FAC1 = (.A.Y)

    bpl LAB_DE28    ; go do /10 if FAC1 > (.A.Y)

; FAC1 < (.A.Y)
LAB_DE16:
    lda #<FPC12     ; set 99999999.90625 pointer low byte
    ldy #>FPC12     ; set 99999999.90625 pointer high byte
    jsr CMPFAC      ; compare FAC1 with (.A.Y)
    beq LAB_DE21    ; branch if FAC1 = (.A.Y) (allow decimal places)

    bpl LAB_DE2F    ; branch if FAC1 > (.A.Y) (no decimal places)

; FAC1 <= (.A.Y)
LAB_DE21:
    jsr MULTEN      ; multiply FAC1 by 10
    dec LAB_5D      ; decrement number exponent count
    bne LAB_DE16    ; go test again, branch always

LAB_DE28:
    jsr DIVTEN      ; divide FAC1 by 10
    inc LAB_5D      ; increment number exponent count
    bne LAB_DE0B    ; go test again, branch always

; now we have just the digits to do

LAB_DE2F:
    jsr ADD05       ; add 0.5 to FAC1 (round FAC1)
LAB_DE32:
    jsr FPINT       ; convert FAC1 floating to fixed
    ldx #$01        ; set default digits before dp = 1
    lda LAB_5D      ; get number exponent count
    clc             ; clear carry for add
    adc #$0A        ; up to 9 digits before point
    bmi LAB_DE47    ; if -ve then 1 digit before dp

    cmp #$0B        ; .A>=$0B if n>=1E9
    bcs LAB_DE48    ; branch if >= $0B

                    ; carry is clear
    adc #$FF        ; take 1 from digit count
    tax             ; copy to .X
    lda #$02        ; set the exponent adjust
LAB_DE47:
    sec             ; set carry for subtract
LAB_DE48:
    sbc #$02        ; -2
    sta EXPCNT      ; save the exponent adjust
    stx LAB_5D      ; save digits before dp count
    txa             ; copy digits before dp count to .A
    beq LAB_DE53    ; if no digits before the dp go do the "."

    bpl LAB_DE66    ; if there are digits before the dp go do them

LAB_DE53:
    ldy FBUFPT      ; get the output string index
    lda #'.'        ; character "."
    INY             ; increment the index
    sta STACK-1,Y   ; save the "." to the output string
    txa             ; copy digits before dp count to .A
    beq LAB_DE64    ; if no digits before the dp skip the "0"

    lda #'0'        ; character "0"
    INY             ; increment index
    sta STACK-1,Y   ; save the "0" to the output string
LAB_DE64:
    sty FBUFPT      ; save the output string index
LAB_DE66:
    ldy #$00        ; clear the powers of 10 index (point to -100,000,000)
LAB_DE68:
    ldx #$80        ; clear the digit, set the test sense
LAB_DE6A:
    lda FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    clc                 ; clear carry for add
    adc FLTCON+3,Y      ; add byte 4, least significant
    sta FAC1+FAC_MANT+3 ; save FAC1 mantissa 4
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    adc FLTCON+2,Y      ; add byte 3
    sta FAC1+FAC_MANT+2 ; save FAC1 mantissa 3
    lda FAC1+FAC_MANT+1 ; get FAC1 mantissa 2
    adc FLTCON+1,Y      ; add byte 2
    sta FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    lda FAC1+FAC_MANT   ; get FAC1 mantissa 1
    adc FLTCON+0,Y      ; add byte 1, most significant
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1
    inx             ; increment the digit, set the sign on the test sense bit
    bcs LAB_DE8E    ; if the carry is set go test if the result was positive

                    ; else the result needs to be negative
    bpl LAB_DE6A    ; not -ve so try again

    bmi LAB_DE90    ; else done so return the digit

LAB_DE8E:
    bmi LAB_DE6A    ; not +ve so try again

; else done so return the digit

LAB_DE90:
    txa             ; copy the digit
    bcc LAB_DE97    ; if Cb=0 just use it

    eor #$FF        ; else make the 2's complement ..
    adc #$0A        ; .. and subtract it from 10
LAB_DE97:
    adc #'0'-1      ; add "0"-1 to result
    INY             ; increment ..
    INY             ; .. index to..
    INY             ; .. next less ..
    INY             ; .. power of ten
    sty VARPNT      ; save the powers of ten table index
    ldy FBUFPT      ; get output string index
    INY             ; increment output string index
    tax             ; copy character to .X
    and #$7F        ; mask out top bit
    sta STACK-1,Y   ; save to output string
    dec LAB_5D      ; decrement # of characters before the dp
    bne LAB_DEB2    ; if still characters to do skip the decimal point

                    ; else output the point
    lda #'.'        ; character "."
    INY             ; increment output string index
    sta STACK-1,Y   ; save to output string
LAB_DEB2:
    sty FBUFPT      ; save the output string index
    ldy VARPNT      ; get the powers of ten table index
    txa             ; get the character back
    eor #$FF        ; toggle the test sense bit
    and #$80        ; clear the digit
    tax             ; copy it to the new digit
    cpy #HMSCON-FLTCON
                    ; compare the table index with the max for decimal numbers
    beq LAB_DEC4    ; if at the max exit the digit loop

    cpy #LAB_DF52-FLTCON
                    ; compare the table index with the max for time
    bne LAB_DE6A    ; loop if not at the max

; now remove trailing zeroes

LAB_DEC4:
    ldy FBUFPT      ; restore the output string index
LAB_DEC6:
    lda STACK-1,Y   ; get character from output string
    dey             ; decrement output string index
    cmp #'0'        ; compare with "0"
    beq LAB_DEC6    ; loop until non "0" character found

    cmp #'.'        ; compare with "."
    beq LAB_DED3    ; branch if was dp

                    ; restore last character
    INY             ; increment output string index
LAB_DED3:
    lda #'+'        ; character "+"
    ldx EXPCNT      ; get exponent count
    beq LAB_DF07    ; if zero go set null terminator and exit

                    ; exponent isn't zero so write exponent
    bpl LAB_DEE3    ; branch if exponent count +ve

    lda #$00        ; clear .A
    sec             ; set carry for subtract
    sbc EXPCNT      ; subtract exponent count adjust (convert -ve to +ve)
    tax             ; copy exponent count to .X
    lda #'-'        ; character "-"
LAB_DEE3:
    sta STACK+1,Y   ; save to output string
    lda #'E'        ; character "E"
    sta STACK,Y     ; save exponent sign to output string
    txa             ; get exponent count back
    ldx #$2F        ; one less than "0" character
    sec             ; set carry for subtract
LAB_DEEF:
    inx             ; increment 10's character
    sbc #$0A        ; subtract 10 from exponent count
    bcs LAB_DEEF    ; loop while still >= 0

    adc #':'        ; add character ":" ($30+$0A, result is 10 less that value)
    sta STACK+3,Y   ; save to output string
    txa             ; copy 10's character
    sta STACK+2,Y   ; save to output string
    lda #$00        ; set null terminator
    sta STACK+4,Y   ; save to output string
    beq LAB_DF0C    ; go set string pointer (.A.Y) and exit, branch always

                    ; save last character, [EOT] and exit
LAB_DF04:
    sta STACK-1,Y   ; save last character to output string

                    ; set null terminator and exit
LAB_DF07:
    lda #$00        ; set null terminator
    sta STACK,Y     ; save after last character

                    ; set string pointer (.A.Y) and exit
LAB_DF0C:
    lda #<STACK     ; set result string pointer low byte
    ldy #>STACK     ; set result string pointer high byte
    rts


;***********************************************************************************;
;

FLP05:      .byte $80,$00       ; 0.5, first two bytes
NULLVAR:    .byte $00,$00,$00   ; null return for undefined variables

; decimal conversion tables

FLTCON: .byte $FA,$0A,$1F,$00 ; -100000000
        .byte $00,$98,$96,$80 ;  +10000000
        .byte $FF,$F0,$BD,$C0 ;   -1000000
        .byte $00,$01,$86,$A0 ;    +100000
        .byte $FF,$FF,$D8,$F0 ;     -10000
        .byte $00,$00,$03,$E8 ;      +1000
        .byte $FF,$FF,$FF,$9C ;   -100
        .byte $00,$00,$00,$0A ;    +10
        .byte $FF,$FF,$FF,$FF ;     -1

; jiffy count conversion table

HMSCON: .byte $FF,$DF,$0A,$80 ; -2160000  10s hours
        .byte $00,$03,$4B,$C0 ;  +216000      hours
        .byte $FF,$FF,$73,$60 ;   -36000  10s mins
        .byte $00,$00,$0E,$10 ;    +3600      mins
        .byte $FF,$FF,$FD,$A8 ;     -600  10s secs
        .byte $00,$00,$00,$3C ;      +60      secs
LAB_DF52:


;***********************************************************************************;
;
; spare bytes, not referenced

    .byte $BF,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
    .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA


;***********************************************************************************;
;
; perform SQR()

SQR:
    jsr RFTOA       ; round and copy FAC1 to FAC2
    lda #<FLP05     ; set 0.5 pointer low address
    ldy #>FLP05     ; set 0.5 pointer high address
    jsr LODFAC      ; unpack memory (.A.Y) into FAC1


;***********************************************************************************;
;
; perform power function

EXPONT:
    beq EXP             ; perform EXP()

    lda FAC2+FAC_EXPT   ; get FAC2 exponent
    bne LAB_DF84        ; branch if FAC2<>0

    jmp LAB_D8F9        ; clear FAC1 exponent and sign and return

LAB_DF84:
    ldx #<DEFPNT        ; set destination pointer low byte
    ldy #>DEFPNT        ; set destination pointer high byte
    jsr STORFAC         ; pack FAC1 into (.X.Y)
    lda FAC2+FAC_SIGN       ; get FAC2 sign (b7)
    bpl LAB_DF9E        ; branch if FAC2>0

                        ; else FAC2 is -ve and can only be raised to an
                        ; integer power which gives an x + j0 result
    jsr INT             ; perform INT()
    lda #<DEFPNT        ; set source pointer low byte
    ldy #>DEFPNT        ; set source pointer high byte
    jsr CMPFAC          ; compare FAC1 with (.A.Y)
    bne LAB_DF9E        ; branch if FAC1 <> (.A.Y) to allow Function Call error
                        ; this will leave FAC1 -ve and cause a Function Call
                        ; error when LOG() is called

    tya                 ; clear sign b7
    ldy CHARAC          ; get FAC1 mantissa 4 from INT() function as sign in
                        ; .Y for possible later negation, b0 only needed
LAB_DF9E:
    jsr LAB_DBFE        ; save FAC1 sign and copy ABS(FAC2) to FAC1
    tya                 ; copy sign back ..
    pha                 ; .. and save it
    jsr LOG             ; perform LOG()
    lda #<DEFPNT        ; set pointer low byte
    ldy #>DEFPNT        ; set pointer high byte
    jsr TIMES           ; do convert .A.Y, FAC1*(.A.Y)
    jsr EXP             ; perform EXP()
    pla                 ; pull sign from stack
    lsr                 ; b0 is to be tested
    bcc LAB_DFBE        ; if no bit then exit

; do - FAC1

NEGFAC:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    beq LAB_DFBE        ; exit if FAC1_e = $00

    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    eor #$FF            ; complement it
    sta FAC1+FAC_SIGN   ; save FAC1 sign (b7)
LAB_DFBE:
    rts


;***********************************************************************************;
;
; exp(n) constant and series

EXPCON:
    .byte   $81,$38,$AA,$3B,$29 ; 1.443

LAB_DFC4:
    .byte   $07                 ; series count
    .byte   $71,$34,$58,$3E,$56 ; 2.14987637E-5
    .byte   $74,$16,$7E,$B3,$1B ; 1.43523140E-4
    .byte   $77,$2F,$EE,$E3,$85 ; 1.34226348E-3
    .byte   $7A,$1D,$84,$1C,$2A ; 9.61401701E-3
    .byte   $7C,$63,$59,$58,$0A ; 5.55051269E-2
    .byte   $7E,$75,$FD,$E7,$C6 ; 2.40226385E-1
    .byte   $80,$31,$72,$18,$10 ; 6.93147186E-1
    .byte   $81,$00,$00,$00,$00 ; 1.00000000


;***********************************************************************************;
;
; perform EXP()

EXP:
    lda #<EXPCON    ; set 1.443 pointer low byte
    ldy #>EXPCON    ; set 1.443 pointer high byte
    jsr TIMES       ; do convert .A.Y, FAC1*(.A.Y)
    lda FACOV       ; get FAC1 rounding byte
    adc #$50        ; +$50/$100
    bcc LAB_DFFD    ; skip rounding if no carry

    jsr LAB_DC23        ; round FAC1 (no check)
LAB_DFFD:
    sta JMPER+2         ; save FAC2 rounding byte
    jsr FTOA            ; copy FAC1 to FAC2
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    cmp #$88            ; compare with EXP limit (256)
    bcc LAB_E00B        ; branch if less

LAB_E008:
    jsr LAB_DAD4    ; handle overflow and underflow
LAB_E00B:
    jsr INT         ; perform INT()
    lda CHARAC      ; get mantissa 4 from INT()
    clc             ; clear carry for add
    adc #$81        ; normalise +1
    beq LAB_E008    ; if $00 result has overflowed so go handle it

    sec             ; set carry for subtract
    sbc #$01        ; exponent now correct
    pha             ; save FAC2 exponent
                    ; swap FAC1 and FAC2
    ldx #$05        ; 4 bytes to do
LAB_E01B:
    lda FAC2,X      ; get FAC2,X
    ldy FAC1,X      ; get FAC1,X
    sta FAC1,X      ; save FAC1,X
    sty FAC2,X      ; save FAC2,X
    dex             ; decrement count/index
    bpl LAB_E01B    ; loop if not all done

    lda JMPER+2     ; get FAC2 rounding byte
    sta FACOV       ; save as FAC1 rounding byte
    jsr SUB         ; perform subtraction, FAC2 from FAC1
    jsr NEGFAC      ; do - FAC1
    lda #<LAB_DFC4  ; set counter pointer low byte
    ldy #>LAB_DFC4  ; set counter pointer high byte
    jsr SER2        ; go do series evaluation
    lda #$00        ; clear .A
    sta ARISGN      ; clear sign compare (FAC1 XOR FAC2)
    pla             ; pull the saved FAC2 exponent
    jsr LAB_DAB9    ; test and adjust accumulators
    rts


;***********************************************************************************;
;
; ^2 then series evaluation

SEREVL:
    sta FBUFPT      ; save count pointer low byte
    sty FBUFPT+1    ; save count pointer high byte
    jsr FACTF1      ; pack FAC1 into LAB_57
    lda #<TEMPF3    ; set pointer low byte (.Y already $00)
    jsr TIMES       ; do convert .A.Y, FAC1*(.A.Y)
    jsr LAB_E05A    ; go do series evaluation
    lda #<TEMPF3    ; pointer to original # low byte
    ldy #>TEMPF3    ; pointer to original # high byte
    jmp TIMES       ; do convert .A.Y, FAC1*(.A.Y)


;***********************************************************************************;
;
; do series evaluation

SER2:
    sta FBUFPT      ; save count pointer low byte
    sty FBUFPT+1    ; save count pointer high byte

; do series evaluation

LAB_E05A:
    jsr FACTF2      ; pack FAC1 into LAB_5C
    lda (FBUFPT),Y  ; get constants count
    sta SGNFLG      ; save constants count
    ldy FBUFPT      ; get count pointer low byte
    INY             ; increment it (now constants pointer)
    tya             ; copy it
    bne LAB_E069    ; skip next if no overflow

    inc FBUFPT+1    ; else increment high byte
LAB_E069:
    sta FBUFPT      ; save low byte
    ldy FBUFPT+1    ; get high byte
LAB_E06D:
    jsr TIMES       ; do convert .A.Y, FAC1*(.A.Y)
    lda FBUFPT      ; get constants pointer low byte
    ldy FBUFPT+1    ; get constants pointer high byte
    clc             ; clear carry for add
    adc #$05        ; +5 to low pointer (5 bytes per constant)
    bcc LAB_E07A    ; skip next if no overflow

    INY             ; increment high byte
LAB_E07A:
    sta FBUFPT      ; save pointer low byte
    sty FBUFPT+1    ; save pointer high byte
    jsr LAPLUS      ; add (.A.Y) to FAC1
    lda #<LAB_5C    ; set pointer low byte to partial
    ldy #>LAB_5C    ; set pointer high byte to partial
    dec SGNFLG      ; decrement constants count
    bne LAB_E06D    ; loop until all done

    rts


;***********************************************************************************;
;
; RND values

RNDC1:      .byte $98,$35,$44,$7A,$00 ; 11879546 multiplier
LAB_E08F:   .byte $68,$28,$B1,$46,$00 ; 3.927677739E-8 offset


;***********************************************************************************;
;
; perform RND()

RND:
    jsr SGNFAC          ; get FAC1 sign
                        ; return .A = $FF -ve, .A = $01 +ve
    bmi LAB_E0D0        ; if n<0 copy byte swapped FAC1 into RND() seed

    bne LAB_E0BB        ; if n>0 get next number in RND() sequence

                        ; else n=0 so get the RND() number from VIA 1 timers
    jsr IOBASE          ; return base address of I/O devices
    stx INDEX           ; save pointer low byte
    sty INDEX+1         ; save pointer high byte
    ldy #$04            ; set index to T1 low byte
    lda (INDEX),Y       ; get T1 low byte
    sta FAC1+FAC_MANT   ; save FAC1 mantissa 1
    INY                 ; increment index
    lda (INDEX),Y       ; get T1 high byte
    sta FAC1+FAC_MANT+2 ; save FAC1 mantissa 3
    ldy #$08            ; set index to T2 low byte
    lda (INDEX),Y       ; get T2 low byte
    sta FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    INY                 ; increment index
    lda (INDEX),Y       ; get T2 high byte
    sta FAC1+FAC_MANT+3 ; save FAC1 mantissa 4
    jmp LAB_E0E0        ; set exponent and exit

LAB_E0BB:
    lda #<RNDX          ; set seed pointer low address
    ldy #>RNDX          ; set seed pointer high address
    jsr LODFAC          ; unpack memory (.A.Y) into FAC1
    lda #<RNDC1         ; set 11879546 pointer low byte
    ldy #>RNDC1         ; set 11879546 pointer high byte
    jsr TIMES           ; do convert .A.Y, FAC1*(.A.Y)
    lda #<LAB_E08F      ; set 3.927677739E-8 pointer low byte
    ldy #>LAB_E08F      ; set 3.927677739E-8 pointer high byte
    jsr LAPLUS          ; add (.A.Y) to FAC1
LAB_E0D0:
    ldx FAC1+FAC_MANT+3 ; get FAC1 mantissa 4
    lda FAC1+FAC_MANT   ; get FAC1 mantissa 1
    sta FAC1+FAC_MANT+3 ; save FAC1 mantissa 4
    stx FAC1+FAC_MANT   ; save FAC1 mantissa 1
    ldx FAC1+FAC_MANT+1 ; get FAC1 mantissa 2
    lda FAC1+FAC_MANT+2 ; get FAC1 mantissa 3
    sta FAC1+FAC_MANT+1 ; save FAC1 mantissa 2
    stx FAC1+FAC_MANT+2 ; save FAC1 mantissa 3
LAB_E0E0:
    lda #$00            ; clear byte
    sta FAC1+FAC_SIGN   ; clear FAC1 sign (always +ve)
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    sta FACOV           ; save FAC1 rounding byte
    lda #$80            ; set exponent = $80
    sta FAC1+FAC_EXPT   ; save FAC1 exponent
    jsr LAB_D8D7        ; normalise FAC1
    ldx #<RNDX          ; set seed pointer low address
    ldy #>RNDX          ; set seed pointer high address


;***********************************************************************************;
;
; pack FAC1 into (.X.Y)

LAB_E0F3:
    jmp STORFAC         ; pack FAC1 into (.X.Y)


;***********************************************************************************;
;
; handle BASIC I/O error

PATCHBAS:
    cmp #$F0        ; compare error with $F0
    bne LAB_E101    ; branch if not $F0

    sty MEMSIZ+1    ; set end of memory high byte
    stx MEMSIZ      ; set end of memory low byte
    jmp LAB_C663    ; clear from start to end and return

                    ; error was not $F0
LAB_E101:
    tax             ; copy error #
    bne LAB_E106    ; branch if not $00

    ldx #ER_BREAK   ; else error $1E, break error
LAB_E106:
    jmp ERROR       ; do error #.X then warm start


;***********************************************************************************;
;
; output character to channel with error check

LAB_E109:
    jsr CHROUT      ; output character to channel
    bcs PATCHBAS    ; if error go handle BASIC I/O error

    rts


;***********************************************************************************;
;
; input character from channel with error check

LAB_E10F:
    jsr CHRIN       ; input character from channel
    bcs PATCHBAS    ; if error go handle BASIC I/O error

    rts


;***********************************************************************************;
;
; open channel for output with error check

LAB_E115:
    jsr CHKOUT      ; open channel for output
    bcs PATCHBAS    ; if error go handle BASIC I/O error

    rts


;***********************************************************************************;
;
; open channel for input with error check

LAB_E11B:
    jsr CHKIN       ; open channel for input
    bcs PATCHBAS    ; if error go handle BASIC I/O error

    rts


;***********************************************************************************;
;
; get character from input device with error check

LAB_E121:
    jsr GETIN       ; get character from input device
    bcs PATCHBAS    ; if error go handle BASIC I/O error

    rts


;***********************************************************************************;
;
; perform SYS

SYSTEM:
    jsr TYPCHK      ; evaluate expression and check is numeric, else do
                    ; type mismatch
    jsr MAKADR      ; convert FAC1 to integer in temporary integer
    lda #>(LAB_E144-1)    ; get return address high byte
    pha             ; push as return address
    lda #<(LAB_E144-1)    ; get return address low byte
    pha             ; push as return address
    lda SPREG       ; get saved status register
    pha             ; put on stack
    lda SAREG       ; get saved .A
    ldx SXREG       ; get saved .X
    ldy SYREG       ; get saved .Y
    plp             ; pull processor status
    jmp (LINNUM)    ; call SYS address

LAB_E144:
    php             ; save status
    sta SAREG       ; save returned .A
    stx SXREG       ; save returned .X
    sty SYREG       ; save returned .Y
    pla             ; restore saved status
    sta SPREG       ; save status
    rts


;***********************************************************************************;
;
; perform SAVE

BSAVE:
    jsr PARSL       ; get parameters for LOAD/SAVE
                    ; NOTE: VICE monitor will not stop on return here if parametres
                    ; are missing.
    ldx VARTAB      ; get start of variables low byte
    ldy VARTAB+1    ; get start of variables high byte
    lda #TXTTAB     ; index to start of program memory
    jsr SAVE        ; save RAM to device, .A = index to start address, .X.Y = end
                    ; address low/high
    bcs PATCHBAS    ; if error go handle BASIC I/O error

    rts


;***********************************************************************************;
;
; perform VERIFY

BVERIF:
    lda #$01        ; flag verify
    .byte   $2C     ; makes next line bit $00A9


;***********************************************************************************;
;
; perform LOAD

BLOAD:
    lda #$00        ; flag load
    sta VERCHK      ; set load/verify flag
    jsr PARSL       ; get parameters for LOAD/SAVE
                    ; NOTE: VICE monitor will not stop on return here if parametres
                    ; are missing.
    lda VERCHK      ; get load/verify flag
    ldx TXTTAB      ; get start of memory low byte
    ldy TXTTAB+1    ; get start of memory high byte
    jsr LOAD        ; load RAM from a device
    bcs LAB_E1CE    ; if error go handle BASIC I/O error

    lda VERCHK      ; get load/verify flag
    beq LAB_E195    ; branch if load

    ldx #ER_VERIFY  ; error $1C, verify error
    jsr READST      ; read I/O status word
    and #$10        ; mask for tape read error
    beq LAB_E187    ; branch if no read error

    jmp ERROR       ; do error #.X then warm start

LAB_E187:
    lda CHRGOT+1    ; get BASIC execute pointer low byte
                    ; is this correct ?? won't this mean the "OK" prompt
                    ; when doing a load from within a program ?
    cmp #$02
    beq LAB_E194    ; if ?? skip "OK" prompt

    lda #<OKSTR     ; set "OK" pointer low byte
    ldy #>OKSTR     ; set "OK" pointer high byte
    jmp PRTSTR      ; print null terminated string

LAB_E194:
    rts


;***********************************************************************************;
;
; do READY return to BASIC ??

LAB_E195:
    jsr READST      ; read I/O status word
    and #$BF        ; mask x0xx xxxx, clear read error
    beq LAB_E1A1    ; branch if no errors

    ldx #ER_LOAD    ; error $1D, load error
    jmp ERROR       ; do error #.X then warm start

LAB_E1A1:
    lda CHRGOT+2    ; get BASIC execute pointer high byte
    cmp #$02        ; compare with $02xx
    bne LAB_E1B5    ; branch if not immediate mode

    stx VARTAB      ; set start of variables low byte
    sty VARTAB+1    ; set start of variables high byte
    lda #<READYSTR  ; set "READY." pointer low byte
    ldy #>READYSTR  ; set "READY." pointer high byte
    jsr PRTSTR      ; print null terminated string
    jmp LAB_C52A    ; reset execution, clear variables, flush stack,
                    ; rebuild BASIC chain and do warm start

LAB_E1B5:
    jsr STXTPT      ; set BASIC execute pointer to start of memory - 1
    jmp PATCHER     ; rebuild BASIC line chaining, do RESTORE and return


;***********************************************************************************;
;
; perform OPEN

BOPEN:
    jsr PAROC       ; get parameters for OPEN/CLOSE
    jsr OPEN        ; open a logical file
    bcs LAB_E1CE    ; branch if error

    rts


;***********************************************************************************;
;
; perform CLOSE

BCLOSE:
    jsr PAROC       ; get parameters for OPEN/CLOSE
    lda FORPNT      ; get logical file number
    jsr CLOSE       ; close a specified logical file
    bcc LAB_E194    ; exit if no error

LAB_E1CE:
    jmp PATCHBAS    ; go handle BASIC I/O error


;***********************************************************************************;
;
; get parameters for LOAD/SAVE

PARSL:
    ; Set default params for LOAD from cassette without name.
    lda #$00        ; clear file name length
    jsr SETNAM      ; clear filename
    ldx #$01        ; set default device number, cassette
    ldy #$00        ; set default command
    jsr SETLFS      ; set logical, first and second addresses

    jsr IFCHRG      ; exit function if [EOT] or ":"
    jsr LAB_E254    ; set filename
    jsr IFCHRG      ; exit function if [EOT] or ":"
    jsr LAB_E1FD    ; scan and get byte, else do syntax error then warm start
    ldy #$00        ; clear command
    stx FORPNT      ; save device number
    jsr SETLFS      ; set logical, first and second addresses
    jsr IFCHRG      ; exit function if [EOT] or ":"
    jsr LAB_E1FD    ; scan and get byte, else do syntax error then warm start
    txa             ; copy command to .A
    tay             ; copy command to .Y
    ldx FORPNT      ; get device number back
    jmp SETLFS      ; set logical, first and second addresses and return


;***********************************************************************************;
;
; scan and get byte, else do syntax error then warm start

LAB_E1FD:
    jsr SKPCOM      ; scan for ",byte", else do syntax error then warm start
    jmp LAB_D79E    ; get byte parameter and return


;***********************************************************************************;
;
; exit function if [EOT] or ":"

IFCHRG:
    jsr CHRGOT      ; scan memory
    bne LAB_E20A    ; branch if not [EOL] or ":"

    pla             ; dump return address low byte
    pla             ; dump return address high byte
LAB_E20A:
    rts


;***********************************************************************************;
;
; scan for ",valid byte", else do syntax error then warm start

SKPCOM:
    jsr COMCHK      ; scan for ",", else do syntax error then warm start

; scan for valid byte, not [EOL] or ":", else do syntax error then warm start

CHRERR:
    jsr CHRGOT      ; scan memory
    bne LAB_E20A    ; exit if following byte

    jmp LAB_CF08    ; else do syntax error then warm start


;***********************************************************************************;
;
; get parameters for OPEN/CLOSE

PAROC:
    lda #$00        ; clear file name length
    jsr SETNAM      ; clear filename
    jsr CHRERR      ; scan for valid byte, else do syntax error then warm start
    jsr LAB_D79E    ; get byte parameter, logical file number
    stx FORPNT      ; save logical file number
    txa             ; copy logical file number to .A
    ldx #$01        ; set default device number, cassette
    ldy #$00        ; set default command
    jsr SETLFS      ; set logical, first and second addresses
    jsr IFCHRG      ; exit function if [EOT] or ":"
    jsr LAB_E1FD    ; scan and get byte, else do syntax error then warm start
    stx FORPNT+1    ; save device number
    ldy #$00        ; clear command
    lda FORPNT      ; get logical file number
    cpx #$03        ; compare device number with screen
    bcc LAB_E23C    ; branch if less than screen

    dey             ; else decrement command
LAB_E23C:
    jsr SETLFS      ; set logical, first and second addresses
    jsr IFCHRG      ; exit function if [EOT] or ":"
    jsr LAB_E1FD    ; scan and get byte, else do syntax error then warm start
    txa             ; copy command to .A
    tay             ; copy command to .Y
    ldx FORPNT+1    ; get device number
    lda FORPNT      ; get logical file number
    jsr SETLFS      ; set logical, first and second addresses
    jsr IFCHRG      ; exit function if [EOT] or ":"
    jsr SKPCOM      ; scan for ",byte", else do syntax error then warm start


;***********************************************************************************;
;
; set filename

LAB_E254:
    jsr FRMEVL      ; evaluate expression
    jsr DELST       ; evaluate string
    ldx INDEX       ; get string pointer low byte
    ldy INDEX+1     ; get string pointer high byte
    jmp SETNAM      ; set filename and return


;***********************************************************************************;
;
; perform COS()

COS:
    lda #<FPC20     ; set pi/2 pointer low byte
    ldy #>FPC20     ; set pi/2 pointer high byte
    jsr LAPLUS      ; add (.A.Y) to FAC1


;***********************************************************************************;
;
; perform SIN()

SIN:
    jsr RFTOA           ; round and copy FAC1 to FAC2
    lda #<LAB_E2E2      ; set 2*pi pointer low byte
    ldy #>LAB_E2E2      ; set 2*pi pointer high byte
    ldx FAC2+FAC_SIGN   ; get FAC2 sign (b7)
    jsr LAB_DB07        ; divide by (.A.Y) (.X=sign)
    jsr RFTOA           ; round and copy FAC1 to FAC2
    jsr INT             ; perform INT()
    lda #$00            ; clear byte
    sta ARISGN          ; clear sign compare (FAC1 XOR FAC2)
    jsr SUB             ; perform subtraction, FAC2 from FAC1
    lda #<LAB_E2E7      ; set 0.25 pointer low byte
    ldy #>LAB_E2E7      ; set 0.25 pointer high byte
    jsr LAMIN           ; perform subtraction, FAC1 from (.A.Y)
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    pha                 ; save FAC1 sign
    bpl LAB_E29A        ; branch if +ve

    ; FAC1 sign was -ve
    jsr ADD05           ; add 0.5 to FAC1 (round FAC1)
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    bmi LAB_E29D        ; branch if -ve

    lda TANSGN          ; get the comparison evaluation flag
    eor #$FF            ; toggle flag
    sta TANSGN          ; save the comparison evaluation flag
LAB_E29A:
    jsr NEGFAC          ; do - FAC1
LAB_E29D:
    lda #<LAB_E2E7      ; set 0.25 pointer low byte
    ldy #>LAB_E2E7      ; set 0.25 pointer high byte
    jsr LAPLUS          ; add (.A.Y) to FAC1
    pla                 ; restore FAC1 sign
    bpl LAB_E2AA        ; branch if was +ve

                        ; else correct FAC1
    jsr NEGFAC          ; do - FAC1
LAB_E2AA:
    lda #<LAB_E2EC      ; set pointer low byte to counter
    ldy #>LAB_E2EC      ; set pointer high byte to counter
    jmp SEREVL          ; ^2 then series evaluation and return


;***********************************************************************************;
;
; perform TAN()

TAN:
    jsr FACTF1          ; pack FAC1 into LAB_57
    lda #$00            ; clear .A
    sta TANSGN          ; clear the comparison evaluation flag
    jsr SIN             ; perform SIN()
    ldx #<DEFPNT        ; set sin(n) pointer low byte
    ldy #>DEFPNT        ; set sin(n) pointer high byte
    jsr LAB_E0F3        ; pack FAC1 into (.X.Y)
    lda #<TEMPF3        ; set n pointer low byte
    ldy #>TEMPF3        ; set n pointer high byte
    jsr LODFAC          ; unpack memory (.A.Y) into FAC1
    lda #$00            ; clear byte
    sta FAC1+FAC_SIGN   ; clear FAC1 sign (b7)
    lda TANSGN          ; get the comparison evaluation flag
    jsr LAB_E2D9        ; save flag and go do series evaluation
    lda #<DEFPNT        ; set sin(n) pointer low byte
    ldy #>DEFPNT        ; set sin(n) pointer high byte
    jmp LADIV           ; convert .A.Y and do (.A.Y)/FAC1


;***********************************************************************************;
;
; save comparison flag and do series evaluation

LAB_E2D9:
    pha             ; save comparison flag
    jmp LAB_E29A    ; add 0.25, ^2 then series evaluation


;***********************************************************************************;
;
; constants and series for SIN/COS(n)

FPC20:      .byte $81,$49,$0F,$DA,$A2   ; 1.570796371, pi/2, as floating number
LAB_E2E2:   .byte $83,$49,$0F,$DA,$A2   ; 6.28319, 2*pi, as floating number
LAB_E2E7:   .byte $7F,$00,$00,$00,$00   ; 0.25

LAB_E2EC:   .byte $05                   ; series counter
            .byte $84,$E6,$1A,$2D,$1B   ; -14.3813907
            .byte $86,$28,$07,$FB,$F8   ;  42.0077971
            .byte $87,$99,$68,$89,$01   ; -76.7041703
            .byte $87,$23,$35,$DF,$E1   ;  81.6052237
            .byte $86,$A5,$5D,$E7,$28   ; -41.3417021
            .byte $83,$49,$0F,$DA,$A2   ;  6.28318531


;***********************************************************************************;
;
; perform ATN()

ATN:
    lda FAC1+FAC_SIGN   ; get FAC1 sign (b7)
    pha                 ; save sign
    bpl LAB_E313        ; branch if +ve

    jsr NEGFAC          ; else do - FAC1
LAB_E313:
    lda FAC1+FAC_EXPT   ; get FAC1 exponent
    pha                 ; push exponent
    cmp #$81            ; compare with 1
    bcc LAB_E321        ; branch if FAC1 < 1

    lda #<FPC1          ; pointer to 1 low byte
    ldy #>FPC1          ; pointer to 1 high byte
    jsr LADIV           ; convert .A.Y and do (.A.Y)/FAC1
LAB_E321:
    lda #<ATNCON        ; pointer to series low byte
    ldy #>ATNCON        ; pointer to series high byte
    jsr SEREVL          ; ^2 then series evaluation
    pla                 ; restore old FAC1 exponent
    cmp #$81            ; compare with 1
    bcc LAB_E334        ; branch if FAC1 < 1

    lda #<FPC20         ; pointer to (pi/2) low byte
    ldy #>FPC20         ; pointer to (pi/2) low byte
    jsr LAMIN           ; perform subtraction, FAC1 from (.A.Y)
LAB_E334:
    pla                 ; restore FAC1 sign
    bpl LAB_E33A        ; exit if was +ve

    jmp NEGFAC          ; else do - FAC1 and return

LAB_E33A:
    rts


;***********************************************************************************;
;
; series for ATN(n)

ATNCON: .byte $0B                   ; series counter
        .byte $76,$B3,$83,$BD,$D3   ;-6.84793912e-04
        .byte $79,$1E,$F4,$A6,$F5   ; 4.85094216e-03
        .byte $7B,$83,$FC,$B0,$10   ;-0.0161117015
        .byte $7C,$0C,$1F,$67,$CA   ; 0.034209638
        .byte $7C,$DE,$53,$CB,$C1   ;-0.054279133
        .byte $7D,$14,$64,$70,$4C   ; 0.0724571965
        .byte $7D,$B7,$EA,$51,$7A   ;-0.0898019185
        .byte $7D,$63,$30,$88,$7E   ; 0.110932413
        .byte $7E,$92,$44,$99,$3A   ;-0.142839808
        .byte $7E,$4C,$CC,$91,$C7   ; 0.19999912
        .byte $7F,$AA,$AA,$AA,$13   ;-0.333333316
        .byte $81,$00,$00,$00,$00   ; 1.000000000


;***********************************************************************************;
;
; BASIC cold start entry point

COLDBA:
    jsr INITVCTRS   ; initialise BASIC vector table
    jsr INITBA      ; initialise BASIC RAM locations
    jsr FREMSG      ; print start up message and initialise memory pointers
    ldx #$FB        ; value for start stack
    txs             ; set stack pointer
    jmp READY       ; do "READY." warm start


;***********************************************************************************;
;
; character get subroutine for zero page

; the target address for the lda LAB_EA60 becomes the BASIC execute pointer once the
; block is copied to it's destination, any non zero page address will do at assembly
; time, to assemble a three byte instruction.

; page 0 initialisation table from CHRGET
; increment and scan memory

CGIMAG:
    inc CHRGOT+1    ; increment BASIC execute pointer low byte
    bne LAB_E38D    ; branch if no carry
                    ; else
    inc CHRGOT+2    ; increment BASIC execute pointer high byte

; page 0 initialisation table from CHRGOT
; scan memory

LAB_E38D:
    lda LAB_EA60    ; get byte to scan, address set by call routine
    cmp #':'        ; compare with ":"
    bcs LAB_E39E    ; exit if >=

; page 0 initialisation table from CHRSPC
; clear Cb if numeric

    cmp #' '        ; compare with " "
    beq CGIMAG      ; if " " go do next

    sec             ; set carry for sbc
    sbc #'0'        ; subtract "0"
    sec             ; set carry for sbc
    sbc #$D0        ; subtract -"0"
                    ; clear carry if byte = "0"-"9"
LAB_E39E:
    rts


;***********************************************************************************;
;
; spare bytes, not referenced

;LAB_E39F
    .byte $80,$4F,$C7,$52,$58   ; 0.811635157


;***********************************************************************************;
;
; initialise BASIC RAM locations

INITBA:
    lda #$4C        ; opcode for jmp
    sta JMPER       ; save for functions vector jump
    sta USRPPOK     ; save for USR() vector jump
                    ; set USR() vector to illegal quantity error
    lda #<ILQUAN    ; set USR() vector low byte
    ldy #>ILQUAN    ; set USR() vector high byte
    sta ADDPRC      ; save USR() vector low byte
    sty ADDPRC+1    ; save USR() vector high byte
    lda #<MAKFP     ; set fixed to float vector low byte
    ldy #>MAKFP     ; set fixed to float vector high byte
    sta ADRAY2      ; save fixed to float vector low byte
    sty ADRAY2+1    ; save fixed to float vector high byte
    lda #<INTIDX    ; set float to fixed vector low byte
    ldy #>INTIDX    ; set float to fixed vector high byte
    sta ADRAY1      ; save float to fixed vector low byte
    sty ADRAY1+1    ; save float to fixed vector high byte

; copy block from CGIMAG to CHRGET

    ldx #$1C        ; set byte count
LAB_E3C4:
    lda CGIMAG,X    ; get byte from table
    sta CHRGET,X    ; save byte in page zero
    dex             ; decrement count
    bpl LAB_E3C4    ; loop if not all done

    lda #$03        ; set step size, collecting descriptors
    sta FOUR6       ; save garbage collection step size
    lda #$00        ; clear .A
    sta BITS        ; clear FAC1 overflow byte
    sta CHANNL      ; clear current I/O channel, flag default
    sta LASTPT+1    ; clear current descriptor stack item pointer high byte
    ldx #$01        ; set .X
    stx CHNLNK+1    ; set chain link pointer low byte
    stx CHNLNK      ; set chain link pointer high byte
    ldx #TEMPST     ; initial value for descriptor stack
    stx TEMPPT      ; set descriptor stack pointer
    sec             ; set Cb = 1 to read the bottom of memory
    jsr MEMBOT      ; read/set the bottom of memory
    stx TXTTAB      ; save start of memory low byte
    sty TXTTAB+1    ; save start of memory high byte
    sec             ; set Cb = 1 to read the top of memory
    jsr MEMTOP      ; read/set the top of memory
    stx MEMSIZ      ; save end of memory low byte
    sty MEMSIZ+1    ; save end of memory high byte
    stx FRETOP      ; set bottom of string space low byte
    sty FRETOP+1    ; set bottom of string space high byte
    ldy #$00        ; clear index
    tya             ; clear .A
    sta (TXTTAB),Y  ; clear first byte of memory
    inc TXTTAB      ; increment start of memory low byte
    bne LAB_E403    ; branch if no rollover

    inc TXTTAB+1    ; increment start of memory high byte
LAB_E403:
    rts


;***********************************************************************************;
;
; print start up message and initialise memory pointers

FREMSG:
    lda TXTTAB      ; get start of memory low byte
    ldy TXTTAB+1    ; get start of memory high byte
    jsr RAMSPC      ; check available memory, do out of memory error if no room
    lda #<BASMSG    ; set "**** CBM BASIC V2 ****" pointer low byte
    ldy #>BASMSG    ; set "**** CBM BASIC V2 ****" pointer high byte
    jsr PRTSTR      ; print null terminated string
    lda MEMSIZ      ; get end of memory low byte
    sec             ; set carry for subtract
    sbc TXTTAB      ; subtract start of memory low byte
    tax             ; copy result to .X
    lda MEMSIZ+1    ; get end of memory high byte
    sbc TXTTAB+1    ; subtract start of memory high byte
    jsr PRTFIX      ; print .X.A as unsigned integer
    lda #<BFREMSG   ; set " BYTES FREE" pointer low byte
    ldy #>BFREMSG   ; set " BYTES FREE" pointer high byte
    jsr PRTSTR      ; print null terminated string
    jmp LAB_C644    ; do NEW, CLEAR, RESTORE and return


;***********************************************************************************;
;
BFREMSG:
    .byte   " BYTES FREE",$0D,$00
BASMSG:
    .byte   $93,"**** CBM BASIC V2 ****",$0D,$00


;***********************************************************************************;
;
; BASIC vectors, these are copied to RAM from $0300 onwards

BASVCTRS:
    .word   ERROR2  ; error message             IERROR
    .word   MAIN2   ; BASIC warm start          IMAIN
    .word   CRNCH2  ; crunch BASIC tokens           ICRNCH
    .word   QPLOP   ; uncrunch BASIC tokens         IQPLOP
    .word   GONE    ; start new BASIC code          IGONE
    .word   FEVAL   ; get arithmetic element        IEVAL


;***********************************************************************************;
;
; initialise BASIC vectors

INITVCTRS:
    ldx #$0B        ; set byte count
LAB_E45D:
    lda BASVCTRS,X  ; get byte from table
    sta IERROR,X    ; save byte to RAM
    dex             ; decrement index
    bpl LAB_E45D    ; loop if more to do

    rts


;***********************************************************************************;
;
; BASIC warm start entry point

WARMBAS:
    jsr CLRCHN      ; close input and output channels
    lda #$00        ; clear .A
    sta CHANNL      ; set current I/O channel, flag default
    jsr LAB_C67A    ; flush BASIC stack and clear continue pointer
    cli             ; enable interrupts
    jmp READY       ; do warm start


;***********************************************************************************;
;
; checksum byte, not referenced

;LAB_E475
.ifdef PAL
    .byte   $E8
.endif
.ifdef NTSC
    .byte   $41
.endif


;***********************************************************************************;
;
; rebuild BASIC line chaining and do RESTORE

PATCHER:
    jsr LNKPRG      ; rebuild BASIC line chaining
    jmp LAB_C677    ; do RESTORE, clear stack and return


;***********************************************************************************;
;
; spare bytes, not referenced

;LAB_E47C
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$FF,$FF,$FF


;***********************************************************************************;
;
; set serial data out high

SEROUT1:
    lda VIA2PCR     ; get VIA 2 PCR
    and #$DF        ; set CB2 low, serial data out high
    sta VIA2PCR     ; set VIA 2 PCR
    rts


;***********************************************************************************;
;
; set serial data out low

SEROUT0:
    lda VIA2PCR     ; get VIA 2 PCR
    ora #$20        ; set CB2 high, serial data out low
    sta VIA2PCR     ; set VIA 2 PCR
    rts


;***********************************************************************************;
;
; get serial clock status

SERGET:
    lda VIA1PA2     ; get VIA 1 DRA, no handshake
    cmp VIA1PA2     ; compare with self
    bne SERGET      ; loop if changing

    lsr             ; shift serial clock to Cb
    rts


;***********************************************************************************;
;
; get secondary address and print "Searching..."

PATCH1:
    ldx SA          ; get secondary address
    jmp SRCHING     ; print "Searching..." and return


;***********************************************************************************;
;
; set LOAD address if secondary address = 0

PATCH2:
    txa             ; copy secondary address
    bne LAB_E4CC    ; load location not set in LOAD call, so
                    ; continue with load
    lda MEMUSS      ; get load address low byte
    sta EAL         ; save program start address low byte
    lda MEMUSS+1    ; get load address high byte
    sta EAL+1       ; save program start address high byte
LAB_E4CC:
    jmp LDVRMSG     ; display "LOADING" or "VERIFYING" and return


;***********************************************************************************;
;
; patch for CLOSE

PATCH3:
    jsr WBLK        ; initiate tape write
    bcc LAB_E4D7    ; branch if no error

    pla             ; else dump stacked exit code
    lda #$00        ; clear exit code
LAB_E4D7:
    jmp LAB_F39E    ; go do I/O close


;***********************************************************************************;
;
; spare bytes, not referenced

    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$FF,$FF,$FF,$FF,$FF


;***********************************************************************************;
;
; return base address of I/O devices

; This routine will set .X.Y to the address of the memory section where the memory
; mapped I/O devices are located. This address can then be used with an offset to
; access the memory mapped I/O devices in the computer.

FIOBASE:
    ldx #<VIA1PB    ; get I/O base address low byte
    ldy #>VIA1PB    ; get I/O base address high byte
    rts


;***********************************************************************************;
;
; return X,Y organisation of screen

; this routine returns the x,y organisation of the screen in .X,.Y

FSCREEN:
    ldx #$16        ; get screen X, 22 columns
    ldy #$17        ; get screen Y, 23 rows
    rts


;***********************************************************************************;
;
; read/set X,Y cursor position, Cb = 1 to read, Cb = 0 to set

; This routine, when called with the carry flag set, loads the current position of
; the cursor on the screen into the .X and .Y registers. .X is the column number of
; the cursor location and .Y is the row number of the cursor. A call with the carry
; bit clear moves the cursor to the position determined by the .X and .Y registers.

FPLOT:
    bcs LAB_E513    ; if read cursor skip the set cursor

    stx TBLX        ; save cursor row
    sty PNTR        ; save cursor column
    jsr SETSLINK    ; set screen pointers for cursor row, column
LAB_E513:
    ldx TBLX        ; get cursor row
    ldy PNTR        ; get cursor column
    rts


;***********************************************************************************;
;
; initialise hardware

INITSK:
    jsr SETIODEF    ; set default devices and initialise VIC chip
    lda HIBASE      ; get screen memory page
    and #$FD        ; mask xxxx xx0x, all but va9
    asl             ; << 1 xxxx x0x0
    asl             ; << 2 xxxx 0x00
    ora #$80        ; set  1xxx 0x00
    sta VICCR5      ; set screen and character memory location
    lda HIBASE      ; get screen memory page
    and #$02        ; mask bit 9
    beq LAB_E536    ; if zero just go normalise screen

                    ; else set va9 in VIC chip
    lda #$80        ; set b7
    ora VICCR2      ; OR in as video address 9
    sta VICCR2      ; save new va9

                    ; now normalise screen
LAB_E536:
    lda #$00        ; clear .A
    sta MODE        ; clear shift mode switch
    sta BLNON       ; clear cursor blink phase
    lda #<SETKEYS   ; get keyboard decode logic pointer low byte
    sta KEYLOG      ; set keyboard decode logic pointer low byte
    lda #>SETKEYS   ; get keyboard decode logic pointer high byte
    sta KEYLOG+1    ; set keyboard decode logic pointer high byte
    lda #$0A        ; 10d
    sta XMAX        ; set maximum size of keyboard buffer
    sta DELAY       ; set repeat delay counter
    lda #$06        ; colour blue
    sta COLOR       ; set current colour code
    lda #$04        ; speed 4
    sta KOUNT       ; set repeat speed counter
    lda #$0C        ; cursor flash timing
    sta BLNCT       ; set cursor timing countdown
    sta BLNSW       ; set cursor enable, $00 = flash cursor

; clear screen

CLSR:
    lda HIBASE      ; get screen memory page
    ora #$80        ; set high bit, flag every line is logical line start
    tay             ; copy to .Y
    lda #$00        ; clear line start low byte
    tax             ; clear index
LAB_E568:
    sty LDTB1,X     ; save start of line .X pointer high byte
    clc             ; clear carry for add
    adc #$16        ; add line length to low byte
    bcc LAB_E570    ; if no rollover skip the high byte increment

    INY             ; else increment high byte
LAB_E570:
    inx             ; increment line index
    cpx #$18        ; compare with number of lines + 1
    bne LAB_E568    ; loop if not all done

    lda #$FF        ; end of table marker ??
    sta LDTB1,X     ; mark end of table
    ldx #$16        ; set line count, 23 lines to do, 0 to 22
LAB_E57B:
    jsr CLRALINE    ; clear screen line .X
    dex             ; decrement count
    bpl LAB_E57B    ; loop if more to do

; home cursor

HOME:
    ldy #$00        ; clear .Y
    sty PNTR        ; clear cursor column
    sty TBLX        ; clear cursor row

; set screen pointers for cursor row, column

SETSLINK:
    ldx TBLX        ; get cursor row
    lda PNTR        ; get cursor column
LAB_E58B:
    ldy LDTB1,X     ; get start of line X pointer high byte
    bmi LAB_E597    ; continue if logical line start

    clc             ; else clear carry for add
    adc #$16        ; add one line length
    sta PNTR        ; save cursor column
    dex             ; decrement cursor row
    bpl LAB_E58B    ; loop, branch always

LAB_E597:
    lda LDTB1,X     ; get start of line .X pointer high byte
    and #$03        ; mask 0000 00xx, line memory page
    ora HIBASE      ; OR with screen memory page
    sta PNT+1       ; set current screen line pointer high byte
    lda LDTB2,X     ; get start of line low byte from ROM table
    sta PNT         ; set current screen line pointer low byte
    lda #$15        ; set line length
    inx             ; increment cursor row
LAB_E5A8:
    ldy LDTB1,X     ; get start of line .X pointer high byte
    bmi LAB_E5B2    ; exit if logical line start

    clc             ; else clear carry for add
    adc #$16        ; add one line length to current line length
    inx             ; increment cursor row
    bpl LAB_E5A8    ; loop, branch always

LAB_E5B2:
    sta LNMX        ; save current screen line length
    rts


;***********************************************************************************;
;
; set default devices, initialise VIC chip and home cursor
;
; unreferenced code

;UNUSDNMI
    jsr SETIODEF    ; set default devices and initialise VIC chip
    jmp HOME        ; home cursor and return


;***********************************************************************************;
;
; set default devices and initialise VIC chip

SETIODEF:
    lda #$03        ; set screen
    sta DFLTO       ; set output device number
    lda #$00        ; set keyboard
    sta DFLTN       ; set input device number

; initialise VIC chip

INITVIC:
    ldx #$10        ; set byte count
LAB_E5C5:
    lda VICINIT-1,X ; get byte from setup table
    sta VICCR0-1,X  ; save byte to VIC chip
    dex             ; decrement count/index
    bne LAB_E5C5    ; loop if more to do

    rts


;***********************************************************************************;
;
; input from keyboard buffer

LP2:
    ldy KEYD        ; get current character from buffer
    ldx #$00        ; clear index
LAB_E5D4:
    lda KEYD+1,X    ; get next character,.X from buffer
    sta KEYD,X      ; save as current character,.X in buffer
    inx             ; increment index
    cpx NDX         ; compare with keyboard buffer index
    bne LAB_E5D4    ; loop if more to do

    dec NDX         ; decrement keyboard buffer index
    tya             ; copy key to .A
    cli             ; enable interrupts
    clc             ; flag got byte
    rts


;***********************************************************************************;
;
; write character and wait for key

GETQUE:
    jsr SCRNOUT     ; output character

; wait for key from keyboard

LAB_E5E8:
    lda NDX         ; get keyboard buffer index
    sta BLNSW       ; cursor enable, $00 = flash cursor, $xx = no flash
    sta AUTODN      ; screen scrolling flag, $00 = scroll, $xx = no scroll
                    ; this disables both the cursor flash and the screen scroll
                    ; while there are characters in the keyboard buffer
    beq LAB_E5E8    ; loop if buffer empty

    sei             ; disable interrupts
    lda BLNON       ; get cursor blink phase
    beq LAB_E602    ; branch if cursor phase

                    ; else character phase
    lda CDBLN       ; get character under cursor
    ldx GDCOL       ; get colour under cursor
    ldy #$00        ; clear .Y
    sty BLNON       ; clear cursor blink phase
    jsr SYNPRT      ; print character .A and colour .X
LAB_E602:
    jsr LP2         ; input from keyboard buffer
    cmp #$83        ; compare with [SHIFT][RUN]
    bne GET2RTN     ; branch if not [SHIFT][RUN]

                    ; keys are [SHIFT][RUN] so put "LOAD",$0D,"RUN",$0D into
                    ; the buffer
    ldx #$09        ; set byte count
    sei             ; disable interrupts
    stx NDX         ; set keyboard buffer index
LAB_E60E:
    lda RUNTB-1,X   ; get byte from auto load/run table
    sta KEYD-1,X    ; save to keyboard buffer
    dex             ; decrement count/index
    bne LAB_E60E    ; loop while more to do

    beq LAB_E5E8    ; loop for next key, branch always

                    ; was not [SHIFT][RUN]
GET2RTN:
    cmp #$0D        ; compare with [CR]
    bne GETQUE      ; if not [CR] print character and get next key

                    ; was [CR]
    ldy LNMX        ; get current screen line length
    sty CRSW        ; input from keyboard or screen, $xx = screen,
                    ; $00 = keyboard
LAB_E621:
    lda (PNT),Y     ; get character from current screen line
    cmp #' '        ; compare with [SPACE]
    bne LAB_E62A    ; branch if not [SPACE]

    dey             ; else eliminate the space, decrement end of input line
    bne LAB_E621    ; loop, branch always

LAB_E62A:
    INY             ; increment past last non space character on line
    sty INDX        ; save input [EOL] pointer
    ldy #$00        ; clear .Y
    sty AUTODN      ; clear screen scrolling flag, $00 = scroll, $xx = no scroll
    sty PNTR        ; clear cursor column
    sty QTSW        ; clear cursor quote flag, $xx = quote, $00 = no quote
    lda LXSP        ; get input cursor row
    bmi LAB_E657

    ldx TBLX        ; get cursor row
    jsr LAB_E719    ; find and set pointers for start of logical line
    cpx LXSP        ; compare with input cursor row
    bne LAB_E657

    bne LAB_E657    ; ?? what's this? just to make sure or something

    lda LXSP+1      ; get input cursor column
    sta PNTR        ; save cursor column
    cmp INDX        ; compare with input [EOL] pointer
    bcc LAB_E657    ; branch if less, cursor is in line

    bcs LAB_E691    ; else cursor is beyond the line end, branch always


;***********************************************************************************;
;
; input from screen or keyboard

GETSCRN:
    tya             ; copy .Y
    pha             ; save .Y
    txa             ; copy .X
    pha             ; save .X
    lda CRSW        ; input from keyboard or screen, $xx = screen,
                    ; $00 = keyboard
    beq LAB_E5E8    ; if keyboard go wait for key

LAB_E657:
    ldy PNTR        ; get cursor column
    lda (PNT),Y     ; get character from the current screen line
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    sta ASCII       ; save temporary last character
    and #$3F        ; mask key bits
    asl ASCII       ; << temporary last character
    bit ASCII       ; test it
    bpl LAB_E67E    ; branch if not [NO KEY]

    ora #$80
LAB_E67E:
    bcc LAB_E684

    ldx QTSW        ; get cursor quote flag, $xx = quote, $00 = no quote
    bne LAB_E688    ; branch if in quote mode

LAB_E684:
    BVS LAB_E688

    ora #$40
LAB_E688:
    inc PNTR        ; increment cursor column
    jsr QUOTECK     ; if open quote toggle cursor quote flag
    cpy INDX        ; compare with input [EOL] pointer
    bne LAB_E6A8    ; branch if not at line end

LAB_E691:
    lda #$00
    sta CRSW        ; clear input from keyboard or screen, $xx = screen,
                    ; $00 = keyboard
    lda #$0D        ; set character [CR]
    ldx DFLTN       ; get input device number
    cpx #$03        ; compare with screen
    beq LAB_E6A3    ; branch if screen

    ldx DFLTO       ; get output device number
    cpx #$03        ; compare with screen
    beq LAB_E6A6    ; branch if screen

LAB_E6A3:
    jsr SCRNOUT     ; output character
LAB_E6A6:
    lda #$0D        ; set character [CR]
LAB_E6A8:
    sta ASCII       ; save character
    pla             ; pull .X
    tax             ; restore .X
    pla             ; pull .Y
    tay             ; restore .Y
    lda ASCII       ; restore character
    cmp #$DE
    bne LAB_E6B6

    lda #$FF
LAB_E6B6:
    clc             ; flag ok
    rts


;***********************************************************************************;
;
; if open quote toggle cursor quote flag

QUOTECK:
    cmp #$22        ; compare byte with "
    bne LAB_E6C4    ; exit if not "

    lda QTSW        ; get cursor quote flag, $xx = quote, $00 = no quote
    eor #$01        ; toggle it
    sta QTSW        ; save cursor quote flag
    lda #$22        ; restore the "
LAB_E6C4:
    rts


;***********************************************************************************;
;
; insert uppercase/graphic character

SETCHAR:
    ora #$40        ; change to uppercase/graphic
LAB_E6C7:
    ldx RVS         ; get reverse flag
    beq LAB_E6CD    ; branch if not reverse

                    ; else ..
; insert reversed character

LAB_E6CB:
    ora #$80        ; reverse character
LAB_E6CD:
    ldx INSRT       ; get insert count
    beq LAB_E6D3    ; branch if none

    dec INSRT       ; else decrement insert count
LAB_E6D3:
    ldx COLOR       ; get current colour code
    jsr SYNPRT      ; print character .A and colour .X
    jsr SCROLL      ; advance cursor

; restore registers, set quote flag and exit

LAB_E6DC:
    pla             ; pull .Y
    tay             ; restore .Y
    lda INSRT       ; get insert count
    beq LAB_E6E4    ; skip quote flag clear if inserts to do

    lsr QTSW        ; clear cursor quote flag, $xx = quote, $00 = no quote
LAB_E6E4:
    pla             ; pull .X
    tax             ; restore .X
    pla             ; restore .A
    clc
    cli             ; enable interrupts
    rts


;***********************************************************************************;
;
; advance cursor

SCROLL:
    jsr FORWARD     ; test for line increment
    inc PNTR        ; increment cursor column
    lda LNMX        ; get current screen line length
    cmp PNTR        ; compare with cursor column
    bcs LAB_E72C    ; exit if line length >= cursor column

    cmp #$57        ; compare with max length
    beq LAB_E723    ; if at max clear column, back cursor up and do newline

    lda AUTODN      ; get autoscroll flag
    beq LAB_E701    ; branch if autoscroll on

    jmp LAB_E9F0    ; else open space on screen

LAB_E701:
    ldx TBLX        ; get cursor row
    cpx #$17        ; compare with max + 1
    bcc LAB_E70E    ; if less than max + 1 go add this row to the current
                    ; logical line

    jsr SCRL        ; else scroll screen
    dec TBLX        ; decrement cursor row
    ldx TBLX        ; get cursor row

; add this row to the current logical line

LAB_E70E:
    asl LDTB1,X     ; shift start of line .X pointer high byte
    lsr LDTB1,X     ; shift start of line .X pointer high byte back,
                    ; clear b7, start of logical line
    jmp WRAPLINE    ; make next screen line start of logical line, increment
                    ; line length and set pointers

; add one line length and set pointers for start of line

LAB_E715:
    adc #$16        ; add one line length
    sta LNMX        ; save current screen line length

; find and set pointers for start of logical line

LAB_E719:
    lda LDTB1,X     ; get start of line .X pointer high byte
    bmi LAB_E720    ; exit loop if start of logical line

    dex             ; else back up one line
    bne LAB_E719    ; loop if not on first line

LAB_E720:
    jmp LINPTR      ; set start of line .X and return

; clear cursor column, back cursor up one line and do newline

LAB_E723:
    dec TBLX        ; decrement cursor row. if the cursor was incremented past
                    ; the last line then this decrement and the scroll will
                    ; leave the cursor one line above the botom of the screen
    jsr NXTLINE     ; do newline
    lda #$00        ; clear .A
    sta PNTR        ; clear cursor column
LAB_E72C:
    rts

; back onto previous line if possible

RETREAT:
    ldx TBLX        ; get cursor row
    bne LAB_E737    ; branch if not top row

    stx PNTR        ; clear cursor column
    pla             ; dump return address low byte
    pla             ; dump return address high byte
    bne LAB_E6DC    ; restore registers, set quote flag and exit, branch always

LAB_E737:
    dex             ; decrement cursor row
    stx TBLX        ; save cursor row
    jsr SETSLINK    ; set screen pointers for cursor row, column
    ldy LNMX        ; get current screen line length
    sty PNTR        ; save as cursor column
    rts


;***********************************************************************************;
;
;## output character to screen

SCRNOUT:
    pha             ; save character
    sta ASCII       ; save temporary last character
    txa             ; copy .X
    pha             ; save .X
    tya             ; copy .Y
    pha             ; save .Y
    lda #$00        ; clear .A
    sta CRSW        ; clear input from keyboard or screen, $xx = screen,
                    ; $00 = keyboard
    ldy PNTR        ; get cursor column
    lda ASCII       ; restore last character
    bpl LAB_E756    ; branch if unshifted

    jmp LAB_E800    ; do shifted characters and return

LAB_E756:
    cmp #$0D        ; compare with [CR]
    bne LAB_E75D    ; branch if not [CR]

    jmp RTRN        ; else output [CR] and return

LAB_E75D:
    cmp #' '        ; compare with [SPACE]
    bcc LAB_E771    ; branch if < [SPACE]

    cmp #$60
    bcc LAB_E769    ; branch if $20 to $5F

                    ; character is $60 or greater
    and #$DF
    bne LAB_E76B

LAB_E769:
    and #$3F
LAB_E76B:
    jsr QUOTECK     ; if open quote toggle cursor direct/programmed flag
    jmp LAB_E6C7

                    ; character was < [SPACE] so is a control character
                    ; of some sort
LAB_E771:
    ldx INSRT       ; get insert count
    beq LAB_E778    ; branch if no characters to insert

    jmp LAB_E6CB    ; insert reversed character

LAB_E778:
    cmp #$14        ; compare with [INSERT]/[DELETE]
    bne LAB_E7AA    ; branch if not [INSERT]/[DELETE]

    tya
    bne LAB_E785

    jsr RETREAT     ; back onto previous line if possible
    jmp LAB_E79F

LAB_E785:
    jsr BACKUP      ; test for line decrement

                    ; now close up the line
    dey             ; decrement index to previous character
    sty PNTR        ; save cursor column
    jsr COLORSYN    ; calculate pointer to colour RAM
LAB_E78E:
    INY             ; increment index to next character
    lda (PNT),Y     ; get character from current screen line
    dey             ; decrement index to previous character
    sta (PNT),Y     ; save character to current screen line
    INY             ; increment index to next character
    lda (USER),Y    ; get colour RAM byte
    dey             ; decrement index to previous character
    sta (USER),Y    ; save colour RAM byte
    INY             ; increment index to next character
    cpy LNMX        ; compare with current screen line length
    bne LAB_E78E    ; loop if not there yet

LAB_E79F:
    lda #' '        ; set [SPACE]
    sta (PNT),Y     ; clear last character on current screen line
    lda COLOR       ; get current colour code
    sta (USER),Y    ; save to colour RAM
    bpl LAB_E7F7    ; branch always

LAB_E7AA:
    ldx QTSW        ; get cursor quote flag, $xx = quote, $00 = no quote
    beq LAB_E7B1    ; branch if not quote mode

    jmp LAB_E6CB    ; insert reversed character

LAB_E7B1:
    cmp #$12        ; compare with [RVS ON]
    bne LAB_E7B7    ; branch if not [RVS ON]

    sta RVS         ; set reverse flag
LAB_E7B7:
    cmp #$13        ; compare with [CLR HOME]
    bne LAB_E7BE    ; branch if not [CLR HOME]

    jsr HOME        ; home cursor
LAB_E7BE:
    cmp #$1D        ; compare with [CURSOR RIGHT]
    bne LAB_E7D9    ; branch if not [CURSOR RIGHT]

    INY             ; increment cursor column
    jsr FORWARD     ; test for line increment
    sty PNTR        ; save cursor column
    dey             ; decrement cursor column
    cpy LNMX        ; compare cursor column with current screen line length
    bcc LAB_E7D6    ; exit if less

                    ; else the cursor column is >= the current screen line
                    ; length so back onto the current line and do a newline
    dec TBLX        ; decrement cursor row
    jsr NXTLINE     ; do newline
    ldy #$00        ; clear cursor column
LAB_E7D4:
    sty PNTR        ; save cursor column
LAB_E7D6:
    jmp LAB_E6DC    ; restore registers, set quote flag and exit

LAB_E7D9:
    cmp #$11        ; compare with [CURSOR DOWN]
    bne LAB_E7FA    ; branch if not [CURSOR DOWN]

    clc             ; clear carry for add
    tya             ; copy cursor column
    adc #$16        ; add one line
    tay             ; copy back to .A
    inc TBLX        ; increment cursor row
    cmp LNMX        ; compare cursor column with current screen line length
    bcc LAB_E7D4    ; save cursor column and exit if less

    beq LAB_E7D4    ; save cursor column and exit if equal

                    ; else the cursor has moved beyond the end of this line
                    ; so back it up until it's on the start of the logical line
    dec TBLX        ; decrement cursor row
LAB_E7EC:
    sbc #$16        ; subtract one line
    bcc LAB_E7F4    ; exit loop if on previous line

    sta PNTR        ; else save cursor column
    bne LAB_E7EC    ; loop if not at start of line

LAB_E7F4:
    jsr NXTLINE     ; do newline
LAB_E7F7:
    jmp LAB_E6DC    ; restore registers, set quote flag and exit

LAB_E7FA:
    jsr COLORSET    ; set the colour from the character in .A
    jmp CHARSET

LAB_E800:
    NOP             ; just a few wasted cycles
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    and #$7F        ; mask 0xxx xxxx, clear b7
    cmp #$7F        ; was it $FF before the mask
    bne LAB_E81D    ; branch if not

    lda #$5E        ; else make it $5E
LAB_E81D:
    NOP             ; just a few wasted cycles
    NOP
    NOP
    NOP
    NOP
    NOP
    cmp #' '        ; compare with [SPACE]
    bcc LAB_E82A    ; branch if < [SPACE]

    jmp SETCHAR     ; insert uppercase/graphic character and return

                    ; character was $80 to $9F and is now $00 to $1F
LAB_E82A:
    cmp #$0D        ; compare with [CR]
    bne LAB_E831    ; branch if not [CR]

    jmp RTRN        ; else output [CR] and return

                    ; was not [CR]
LAB_E831:
    ldx QTSW        ; get cursor quote flag, $xx = quote, $00 = no quote
    bne LAB_E874    ; branch if quote mode

    cmp #$14        ; compare with [INSERT DELETE]
    bne LAB_E870    ; branch if not [INSERT DELETE]

    ldy LNMX        ; get current screen line length
    lda (PNT),Y     ; get character from current screen line
    cmp #' '        ; compare with [SPACE]
    bne LAB_E845    ; branch if not [SPACE]

    cpy PNTR        ; compare current column with cursor column
    bne LAB_E84C    ; if not cursor column go open up space on line

LAB_E845:
    cpy #$57        ; compare current column with max line length
    beq LAB_E86D    ; exit if at line end

    jsr OPENLIN     ; else open space on screen
                    ; now open up space on the line to insert a character
LAB_E84C:
    ldy LNMX        ; get current screen line length
    jsr COLORSYN    ; calculate pointer to colour RAM
LAB_E851:
    dey             ; decrement index to previous character
    lda (PNT),Y     ; get character from current screen line
    INY             ; increment index to next character
    sta (PNT),Y     ; save character to current screen line
    dey             ; decrement index to previous character
    lda (USER),Y    ; get current screen line colour RAM byte
    INY             ; increment index to next character
    sta (USER),Y    ; save current screen line colour RAM byte
    dey             ; decrement index to previous character
    cpy PNTR        ; compare with cursor column
    bne LAB_E851    ; loop if not there yet

    lda #' '        ; set [SPACE]
    sta (PNT),Y     ; clear character at cursor position on current screen line
    lda COLOR       ; get current colour code
    sta (USER),Y    ; save to cursor position on current screen line colour RAM
    inc INSRT       ; increment insert count
LAB_E86D:
    jmp LAB_E6DC    ; restore registers, set quote flag and exit

LAB_E870:
    ldx INSRT       ; get insert count
    beq LAB_E879    ; branch if no insert space

LAB_E874:
    ora #$40        ; change to uppercase/graphic
    jmp LAB_E6CB    ; insert reversed character

LAB_E879:
    cmp #$11        ; compare with [CURSOR UP]
    bne LAB_E893    ; branch if not [CURSOR UP]

    ldx TBLX        ; get cursor row
    beq LAB_E8B8    ; branch if on top line

    dec TBLX        ; decrement cursor row
    lda PNTR        ; get cursor column
    sec             ; set carry for subtract
    sbc #$16        ; subtract one line length
    bcc LAB_E88E    ; branch if stepped back to previous line

    sta PNTR        ; else save cursor column ..
    bpl LAB_E8B8    ; .. and exit, branch always

LAB_E88E:
    jsr SETSLINK    ; set screen pointers for cursor row, column ..
    bne LAB_E8B8    ; .. and exit, branch always

LAB_E893:
    cmp #$12        ; compare with [RVS OFF]
    bne LAB_E89B    ; branch if not [RVS OFF]

    lda #$00        ; clear .A
    sta RVS         ; clear reverse flag
LAB_E89B:
    cmp #$1D        ; compare with [CURSOR LEFT]
    bne LAB_E8B1    ; branch if not [CURSOR LEFT]

    tya             ; copy cursor column
    beq LAB_E8AB    ; branch if at start of line

    jsr BACKUP      ; test for line decrement
    dey             ; decrement cursor column
    sty PNTR        ; save cursor column
    jmp LAB_E6DC    ; restore registers, set quote flag and exit

LAB_E8AB:
    jsr RETREAT     ; back onto previous line if possible
    jmp LAB_E6DC    ; restore registers, set quote flag and exit

LAB_E8B1:
    cmp #$13        ; compare with [CLR]
    bne LAB_E8BB    ; branch if not [CLR]

    jsr CLSR        ; clear screen
LAB_E8B8:
    jmp LAB_E6DC    ; restore registers, set quote flag and exit

LAB_E8BB:
    ora #$80        ; restore b7, colour can only be black, cyan, magenta
                    ; or yellow
    jsr COLORSET    ; set the colour from the character in .A
    jmp GRAPHMODE


;***********************************************************************************;
;
; do newline

NXTLINE:
    lsr LXSP        ; shift >> input cursor row
    ldx TBLX        ; get cursor row
LAB_E8C7:
    inx             ; increment row
    cpx #$17        ; compare with last row + 1
    bne LAB_E8CF    ; branch if not last row + 1

    jsr SCRL        ; else scroll screen
LAB_E8CF:
    lda LDTB1,X     ; get start of line .X pointer high byte
    bpl LAB_E8C7    ; loop if not start of logical line

    stx TBLX        ; else save cursor row
    jmp SETSLINK    ; set screen pointers for cursor row, column and return


;***********************************************************************************;
;
; output [CR]

RTRN:
    ldx #$00        ; clear .X
    stx INSRT       ; clear insert count
    stx RVS         ; clear reverse flag
    stx QTSW        ; clear cursor quote flag, $xx = quote, $00 = no quote
    stx PNTR        ; clear cursor column
    jsr NXTLINE     ; do newline
    jmp LAB_E6DC    ; restore registers, set quote flag and exit


;***********************************************************************************;
;
; test for line decrement

BACKUP:
    ldx #$04        ; set count
    lda #$00        ; set column
LAB_E8EC:
    cmp PNTR        ; compare with cursor column
    beq LAB_E8F7    ; branch if at start of line

    clc             ; else clear carry for add
    adc #$16        ; increment to next line
    dex             ; decrement loop count
    bne LAB_E8EC    ; loop if more to test

    rts

LAB_E8F7:
    dec TBLX        ; else decrement cursor row
    rts


;***********************************************************************************;
;
; test for line increment. if at end of line, but not at end of last line, increment the
; cursor row

FORWARD:
    ldx #$04        ; set count
    lda #$15        ; set column
LAB_E8FE:
    cmp PNTR        ; compare with cursor column
    beq LAB_E909    ; if at end of line test and possibly increment cursor row

    clc             ; else clear carry for add
    adc #$16        ; increment to next line
    dex             ; decrement loop count
    bne LAB_E8FE    ; loop if more to test

    rts

                    ; cursor is at end of line
LAB_E909:
    ldx TBLX        ; get cursor row
    cpx #$17        ; compare with end of screen
    beq LAB_E911    ; exit if end of screen

    inc TBLX        ; else increment cursor row
LAB_E911:
    rts


;***********************************************************************************;
;
; set colour code. enter with the colour character in .A. if .A does not contain a
; colour character this routine exits without changing the colour

COLORSET:
    ldx #LAB_E928-COLORTBL
                    ; set colour code count
LAB_E914:
    cmp COLORTBL,X  ; compare the character with the table code
    beq LAB_E91D    ; if a match go save the colour and exit

    dex             ; else decrement the index
    bpl LAB_E914    ; loop if more to do

    rts

LAB_E91D:
    stx COLOR       ; set current colour code
    rts


;***********************************************************************************;
;
; ASCII colour code table
                    ; CHR$()    colour
COLORTBL:           ; ------    ------
    .byte   $90     ;  144      black
    .byte   $05     ;    5      white
    .byte   $1C     ;   28      red
    .byte   $9F     ;  159      cyan
    .byte   $9C     ;  156      magenta
    .byte   $1E     ;   30      green
    .byte   $1F     ;   31      blue
LAB_E928:
    .byte   $9E     ;  158      yellow


;***********************************************************************************;
;
; code conversion, these don't seem to be used anywhere

;CNVRTCD
    .byte   $EF,$A1,$DF,$A6,$E1,$B1,$E2,$B2,$E3,$B3,$E4,$B4,$E5,$B5,$E6,$B6
    .byte   $E7,$B7,$E8,$B8,$E9,$B9,$FA,$BA,$FB,$BB,$FC,$BC,$EC,$BD,$FE,$BE
    .byte   $84,$BF,$F7,$C0,$F8,$DB,$F9,$DD,$EA,$DE,$5E,$E0,$5B,$E1,$5D,$E2
    .byte   $40,$B0,$61,$B1,$78,$DB,$79,$DD,$66,$B6,$77,$C0,$70,$F0,$71,$F1
    .byte   $72,$F2,$73,$F3,$74,$F4,$75,$F5,$76,$F6,$7D,$FD


;***********************************************************************************;
;
; scroll screen

SCRL:
    lda SAL         ; copy tape buffer start pointer
    pha             ; save it
    lda SAL+1       ; copy tape buffer start pointer
    pha             ; save it
    lda EAL         ; copy tape buffer end pointer
    pha             ; save it
    lda EAL+1       ; copy tape buffer end pointer
    pha             ; save it
LAB_E981:
    ldx #$FF        ; set to -1 for pre increment loop
    dec TBLX        ; decrement cursor row
    dec LXSP        ; decrement input cursor row
    dec LLNKSV      ; decrement screen row marker
LAB_E989:
    inx             ; increment line number
    jsr LINPTR      ; set start of line .X
    cpx #$16        ; compare with last line
    bcs LAB_E99D    ; branch if >= $16

    lda LDTB2+1,X   ; get start of next line pointer low byte
    sta SAL         ; save next line pointer low byte
    lda LDTB1+1,X   ; get start of next line pointer high byte
    jsr MOVLIN      ; shift screen line up
    bmi LAB_E989    ; loop, branch always

LAB_E99D:
    jsr CLRALINE    ; clear screen line .X

                    ; now shift up the start of logical line bits
    ldx #$00        ; clear index
LAB_E9A2:
    lda LDTB1,X     ; get start of line .X pointer high byte
    and #$7F        ; clear line .X start of logical line bit
    ldy LDTB1+1,X   ; get start of next line pointer high byte
    bpl LAB_E9AC    ; branch if next line not start of line

    ora #$80        ; set line .X start of logical line bit
LAB_E9AC:
    sta LDTB1,X     ; set start of line .X pointer high byte
    inx             ; increment line number
    cpx #$16        ; compare with last line
    bne LAB_E9A2    ; loop if not last line

    lda LDTB1+$16   ; get start of last line pointer high byte
    ora #$80        ; mark as start of logical line
    sta LDTB1+$16   ; set start of last line pointer high byte
    lda LDTB1       ; get start of first line pointer high byte
    bpl LAB_E981    ; if not start of logical line loop back and
                    ; scroll the screen up another line

    inc TBLX        ; increment cursor row
    inc LLNKSV      ; increment screen row marker
    lda #$FB        ; set keyboard column c2
    sta VIA2PB      ; set VIA 2 DRB, keyboard column
    lda VIA2PA1     ; get VIA 2 DRA, keyboard row
    cmp #$FE        ; compare with row r0 active, [CTRL]
    php             ; save status
    lda #$F7        ; set keyboard column c3
    sta VIA2PB      ; set VIA 2 DRB, keyboard column
    plp             ; restore status
    bne LAB_E9DF    ; skip delay if [CTRL] not pressed

                    ; first time round the inner loop .X will be $16
    ldy #$00        ; clear delay outer loop count, do this 256 times
LAB_E9D6:
    NOP             ; waste cycles
    dex             ; decrement inner loop count
    bne LAB_E9D6    ; loop if not all done

    dey             ; decrement outer loop count
    bne LAB_E9D6    ; loop if not all done

    sty NDX         ; clear keyboard buffer index
LAB_E9DF:
    ldx TBLX        ; get cursor row
    pla             ; pull tape buffer end pointer
    sta EAL+1       ; restore it
    pla             ; pull tape buffer end pointer
    sta EAL         ; restore it
    pla             ; pull tape buffer pointer
    sta SAL+1       ; restore it
    pla             ; pull tape buffer pointer
    sta SAL         ; restore it
    rts


;***********************************************************************************;
;
; open space on screen

OPENLIN:
    ldx TBLX        ; get cursor row
LAB_E9F0:
    inx             ; increment row
    lda LDTB1,X     ; get start of line .X pointer high byte
    bpl LAB_E9F0    ; branch if not start of logical line

    stx LLNKSV      ; set screen row marker
    cpx #$16        ; compare with last line
    beq LAB_EA08    ; branch if = last line

    bcc LAB_EA08    ; branch if < last line

                    ; else was > last line
    jsr SCRL        ; else scroll screen
    ldx LLNKSV      ; get screen row marker
    dex             ; decrement screen row marker
    dec TBLX        ; decrement cursor row
    jmp LAB_E70E    ; add this row to the current logical line and return

LAB_EA08:
    lda SAL         ; copy tape buffer pointer
    pha             ; save it
    lda SAL+1       ; copy tape buffer pointer
    pha             ; save it
    lda EAL         ; copy tape buffer end pointer
    pha             ; save it
    lda EAL+1       ; copy tape buffer end pointer
    pha             ; save it
    ldx #$17        ; set to end line + 1 for predecrement loop
LAB_EA16:
    dex             ; decrement line number
    jsr LINPTR      ; set start of line .X
    cpx LLNKSV      ; compare with screen row marker
    bcc LAB_EA2C    ; branch if < screen row marker

    beq LAB_EA2C    ; branch if = screen row marker

    lda LDTB2-1,X   ; else get start of previous line low byte from ROM table
    sta SAL         ; save previous line pointer low byte
    lda LDTB1-1,X   ; get start of previous line pointer high byte
    jsr MOVLIN      ; shift screen line down
    bmi LAB_EA16    ; loop, branch always

LAB_EA2C:
    jsr CLRALINE    ; clear screen line .X
    ldx #$15
LAB_EA31:
    cpx LLNKSV      ; compare with screen row marker
    bcc LAB_EA44

    lda LDTB1+1,X
    and #$7F
    ldy LDTB1,X     ; get start of line X pointer high byte
    bpl LAB_EA3F

    ora #$80
LAB_EA3F:
    sta LDTB1+1,X
    dex
    bne LAB_EA31

LAB_EA44:
    ldx LLNKSV      ; get screen row marker
    jsr LAB_E70E    ; add this row to the current logical line
    pla             ; pull tape buffer end pointer
    sta EAL+1       ; restore it
    pla             ; pull tape buffer end pointer
    sta EAL         ; restore it
    pla             ; pull tape buffer pointer
    sta SAL+1       ; restore it
    pla             ; pull tape buffer pointer
    sta SAL         ; restore it
    rts


;***********************************************************************************;
;
; shift screen line up/down

MOVLIN:
    and #$03        ; mask 0000 00xx, line memory page
    ora HIBASE      ; OR with screen memory page
    sta SAL+1       ; save next/previous line pointer high byte
    jsr SETADDR     ; calculate pointers to screen lines colour RAM
LAB_EA60:
    ldy #$15        ; set column count
LAB_EA62:
    lda (SAL),Y     ; get character from next/previous screen line
    sta (PNT),Y     ; save character to current screen line
    lda (EAL),Y     ; get colour from next/previous screen line colour RAM
    sta (USER),Y    ; save colour to current screen line colour RAM
    dey             ; decrement column index/count
    bpl LAB_EA62    ; loop if more to do

    rts


;***********************************************************************************;
;
; calculate pointers to screen lines colour RAM

SETADDR:
    jsr COLORSYN    ; calculate pointer to current screen line colour RAM
    lda SAL         ; get next screen line pointer low byte
    sta EAL         ; save next screen line colour RAM pointer low byte
    lda SAL+1       ; get next screen line pointer high byte
    and #$03        ; mask 0000 00xx, line memory page
    ora #$94        ; set  1001 01xx, colour memory page
    sta EAL+1       ; save next screen line colour RAM pointer high byte
    rts


;***********************************************************************************;
;
; set start of line .X

LINPTR:
    lda LDTB2,X     ; get start of line low byte from ROM table
    sta PNT         ; set current screen line pointer low byte
    lda LDTB1,X     ; get start of line high byte from RAM table
    and #$03        ; mask 0000 00xx, line memory page
    ora HIBASE      ; OR with screen memory page
    sta PNT+1       ; set current screen line pointer high byte
    rts


;***********************************************************************************;
;
; clear screen line .X

CLRALINE:
    ldy #$15        ; set number of columns to clear
    jsr LINPTR      ; set start of line .X
    jsr COLORSYN    ; calculate pointer to colour RAM
LAB_EA95:
    lda #' '        ; set [SPACE]
    sta (PNT),Y     ; clear character in current screen line
    lda #$01        ; set colour, blue on white
    sta (USER),Y    ; set colour RAM in current screen line
    dey             ; decrement index
    bpl LAB_EA95    ; loop if more to do

    rts


;***********************************************************************************;
;
; print character .A and colour .X to screen

SYNPRT:
    tay             ; copy character
    lda #$02        ; set count to $02, usually $14 ??
    sta BLNCT       ; set cursor countdown
    jsr COLORSYN    ; calculate pointer to colour RAM
    tya             ; get character back

; save character and colour to screen @ cursor

PUTSCRN:
    ldy PNTR        ; get cursor column
    sta (PNT),Y     ; save character from current screen line
    txa             ; copy colour to .A
    sta (USER),Y    ; save to colour RAM
    rts


;***********************************************************************************;
;
; calculate pointer to colour RAM

COLORSYN:
    lda PNT         ; get current screen line pointer low byte
    sta USER        ; save pointer to colour RAM low byte
    lda PNT+1       ; get current screen line pointer high byte
    and #$03        ; mask 0000 00xx, line memory page
    ora #$94        ; set  1001 01xx, colour memory page
    sta USER+1      ; save pointer to colour RAM high byte
    rts


;***********************************************************************************;
;
; update the clock, flash the cursor, control the cassette and scan the keyboard

; IRQ handler

IRQ:
    jsr UDTIM       ; increment real time clock
    lda BLNSW       ; get cursor enable
    bne LAB_EAEF    ; branch if not flash cursor

    dec BLNCT       ; else decrement cursor timing countdown
    bne LAB_EAEF    ; branch if not done

    lda #$14        ; set count
    sta BLNCT       ; save cursor timing countdown
    ldy PNTR        ; get cursor column
    lsr BLNON       ; shift b0 cursor blink phase into carry
    ldx GDCOL       ; get colour under cursor
    lda (PNT),Y     ; get character from current screen line
    bcs LAB_EAEA    ; branch if cursor phase b0 was 1

    inc BLNON       ; set cursor blink phase to 1
    sta CDBLN       ; save character under cursor
    jsr COLORSYN    ; calculate pointer to colour RAM
    lda (USER),Y    ; get colour RAM byte
    sta GDCOL       ; save colour under cursor
    ldx COLOR       ; get current colour code
    lda CDBLN       ; get character under cursor
LAB_EAEA:
    eor #$80        ; toggle b7 of character under cursor
    jsr PUTSCRN     ; save character and colour to screen @ cursor
LAB_EAEF:
    lda VIA1PA2     ; get VIA 1 DRA, no handshake
    and #$40        ; mask cassette switch sense
    beq LAB_EB01    ; branch if cassette sense low

                    ; cassette sense was high so turn off motor and clear
                    ; the interlock
    ldy #$00        ; clear .Y
    sty CAS1        ; clear the tape motor interlock
    lda VIA1PCR     ; get VIA 1 PCR
    ora #$02        ; set CA2 high, turn off motor
    bne LAB_EB0A    ; branch always

                    ; cassette sense was low so turn on motor, perhaps
LAB_EB01:
    lda CAS1        ; get tape motor interlock
    bne LAB_EB12    ; if cassette interlock <> 0 don't turn on motor

    lda VIA1PCR     ; get VIA 1 PCR
    and #$FD        ; set CA2 low, turn on motor
LAB_EB0A:
    bit VIA1IER     ; test VIA 1 IER
    BVS LAB_EB12    ; if T1 interrupt enabled don't change motor state

    sta VIA1PCR     ; set VIA 1 PCR, set CA2 high/low
LAB_EB12:
    jsr FSCNKEY     ; scan keyboard
    bit VIA2T1CL    ; test VIA 2 T1C_l, clear the timer interrupt flag
    pla             ; pull .Y
    tay             ; restore .Y
    pla             ; pull .X
    tax             ; restore .X
    pla             ; restore .A
    rti


;***********************************************************************************;
;
; scan keyboard performs the following ..
;
; 1)    check if key pressed, if not then exit the routine
;
; 2)    init I/O ports of VIA 2 for keyboard scan and set pointers to decode table 1.
;   clear the character counter
;
; 3)    set one line of port B low and test for a closed key on port A by shifting the
;   byte read from the port. if the carry is clear then a key is closed so save the
;   count which is incremented on each shift. check for SHIFT/STOP/C= keys and
;   flag if closed
;
; 4)    repeat step 3 for the whole matrix
;
; 5)    evaluate the SHIFT/CTRL/C= keys, this may change the decode table selected
;
; 6)    use the key count saved in step 3 as an index into the table selected in step 5
;
; 7)    check for key repeat operation
;
; 8)    save the decoded key to the buffer if first press or repeat

; scan keyboard

; This routine will scan the keyboard and check for pressed keys. It is the same
; routine called by the interrupt handler. If a key is down, its ASCII value is
; placed in the keyboard queue.

FSCNKEY:
    lda #$00        ; clear .A
    sta SHFLAG      ; clear keyboard shift/control/C= flag
    ldy #$40        ; set no key
    sty SFDX        ; save which key
    sta VIA2PB      ; clear VIA 2 DRB, keyboard column
    ldx VIA2PA1     ; get VIA 2 DRA, keyboard row
    cpx #$FF        ; compare with all bits set
    beq LAB_EB8F    ; if no key pressed clear current key and exit (does
                    ; further beq to LAB_EBBA)

    lda #$FE        ; set column 0 low
    sta VIA2PB      ; set VIA 2 DRB, keyboard column
    ldy #$00        ; clear key count
    lda #<NORMKEYS  ; get decode table low byte
    sta KEYTAB      ; set keyboard pointer low byte
    lda #>NORMKEYS  ; get decode table high byte
    sta KEYTAB+1    ; set keyboard pointer high byte
LAB_EB40:
    ldx #$08        ; set row count
    lda VIA2PA1     ; get VIA 2 DRA, keyboard row
    cmp VIA2PA1     ; compare with itself
    bne LAB_EB40    ; loop if changing

LAB_EB4A:
    lsr             ; shift row to Cb
    bcs LAB_EB63    ; if no key closed on this row go do next row

    pha             ; save row
    lda (KEYTAB),Y  ; get character from decode table
    cmp #$05        ; compare with $05, there is no $05 key but the control
                    ; keys are all less than $05
    bcs LAB_EB60    ; if not shift/control/C=/stop go save key count

                    ; else was shift/control/C=/stop key
    cmp #$03        ; compare with $03, stop
    beq LAB_EB60    ; if stop go save key count and continue

                    ; character is $01 - shift, $02 - C= or $04 - control
    ora SHFLAG      ; OR keyboard shift/control/C= flag
    sta SHFLAG      ; save keyboard shift/control/C= flag
    bpl LAB_EB62    ; skip save key, branch always

LAB_EB60:
    sty SFDX        ; save key count
LAB_EB62:
    pla             ; restore row
LAB_EB63:
    INY             ; increment key count
    cpy #$41        ; compare with max+1
    bcs LAB_EB71    ; exit loop if >= max+1

                    ; else still in matrix
    dex             ; decrement row count
    bne LAB_EB4A    ; loop if more rows to do

    sec             ; set carry for keyboard column shift
    ROL VIA2PB      ; shift VIA 2 DRB, keyboard column
    bne LAB_EB40    ; loop for next column, branch always

LAB_EB71:
    jmp (KEYLOG)    ; evaluate the SHIFT/CTRL/C= keys, SETKEYS

; key decoding continues here after the SHIFT/CTRL/C= keys are evaluated

LAB_EB74:
    ldy SFDX        ; get saved key count
    lda (KEYTAB),Y  ; get character from decode table
    tax             ; copy character to .X
    cpy LSTX        ; compare key count with last key count
    beq LAB_EB84    ; if this key = current key, key held, go test repeat

    ldy #$10        ; set repeat delay count
    sty DELAY       ; save repeat delay count
    bne LAB_EBBA    ; go save key to buffer and exit, branch always

LAB_EB84:
    and #$7F        ; clear b7
    bit RPTFLG      ; test key repeat
    bmi LAB_EBA1    ; branch if repeat all

    BVS LAB_EBD6    ; branch if repeat none

    cmp #$7F        ; compare with end marker
LAB_EB8F:
    beq LAB_EBBA    ; if $00/end marker go save key to buffer and exit

    cmp #$14        ; compare with [INSERT]/[DELETE]
    beq LAB_EBA1    ; if [INSERT]/[DELETE] go test for repeat

    cmp #' '        ; compare with [SPACE]
    beq LAB_EBA1    ; if [SPACE] go test for repeat

    cmp #$1D        ; compare with [CURSOR RIGHT]
    beq LAB_EBA1    ; if [CURSOR RIGHT] go test for repeat

    cmp #$11        ; compare with [CURSOR DOWN]
    bne LAB_EBD6    ; if not [CURSOR DOWN] just exit

                    ; was one of the cursor movement keys, insert/delete
                    ; key or the space bar so always do repeat tests
LAB_EBA1:
    ldy DELAY       ; get repeat delay counter
    beq LAB_EBAB    ; branch if delay expired

    dec DELAY       ; else decrement repeat delay counter
    bne LAB_EBD6    ; branch if delay not expired

                    ; repeat delay counter has expired
LAB_EBAB:
    dec KOUNT       ; decrement repeat speed counter
    bne LAB_EBD6    ; branch if repeat speed count not expired

    ldy #$04        ; set for 4/60ths of a second
    sty KOUNT       ; set repeat speed counter
    ldy NDX         ; get keyboard buffer index
    dey             ; decrement it
    bpl LAB_EBD6    ; if the buffer isn't empty just exit

                    ; else repeat the key immediately

; Possibly save the key to the keyboard buffer. If there was no key pressed or the key
; was not found during the scan (possibly due to key bounce) then .X will be $FF here.

LAB_EBBA:
    ldy SFDX        ; get the key count
    sty LSTX        ; save as the current key count
    ldy SHFLAG      ; get keyboard shift/control/C= flag
    sty LSTSHF      ; save as last keyboard shift pattern
    cpx #$FF        ; compare character with table end marker or no key
    beq LAB_EBD6    ; if table end marker or no key just exit

    txa             ; copy character to .A
    ldx NDX         ; get keyboard buffer index
    cpx XMAX        ; compare with keyboard buffer size
    bcs LAB_EBD6    ; if buffer full just exit

    sta KEYD,X      ; save character to keyboard buffer
    inx             ; increment index
    stx NDX         ; save keyboard buffer index
LAB_EBD6:
    lda #$F7        ; enable column 3 for stop key
    sta VIA2PB      ; set VIA 2 DRB, keyboard column
    rts

; evaluate SHIFT/CTRL/C= keys
;
; 0 $00 EC5E
; 1 $02 EC9F
; 2 $04 ECE0
; 3 ..  ....
; 4 $06 EDA3
; 5 $06 EDA3
; 6 $06 EDA3
; 7 $06 EDA3

SETKEYS:
    lda SHFLAG      ; get keyboard shift/control/C= flag
    cmp #$03        ; compare with [SHIFT][C=]
    bne LAB_EC0F    ; branch if not [SHIFT][C=]

    cmp LSTSHF      ; compare with last
    beq LAB_EBD6    ; exit if still the same

    lda MODE        ; get shift mode switch $00 = enabled, $80 = locked
    bmi LAB_EC43    ; if locked continue keyboard decode

    NOP             ; just a few wasted cycles
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP

    ; toggle text mode
    lda VICCR5      ; get start of character memory, ROM
    eor #$02        ; toggle $8000,$8800
    sta VICCR5      ; set start of character memory, ROM
    NOP
    NOP
    NOP
    NOP
    jmp LAB_EC43    ; continue keyboard decode

    ; was not [SHIFT][C=] but could be any other combination
LAB_EC0F:
    asl             ; << 1
    cmp #$08        ; compare with [CTRL]
    bcc LAB_EC18    ; branch if not [CTRL] pressed

    lda #$06        ; else [CTRL] was pressed so make index = $06
    NOP
    NOP
LAB_EC18:
    NOP             ; just a few wasted cycles
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    tax                 ; copy index to .X
    lda KEYVCTRS,X      ; get decode table pointer low byte
    sta KEYTAB          ; save decode table pointer low byte
    lda KEYVCTRS+1,X    ; get decode table pointer high byte
    sta KEYTAB+1        ; save decode table pointer high byte
LAB_EC43:
    jmp LAB_EB74        ; continue keyboard decode


;***********************************************************************************;
;
; keyboard decode table pointers

KEYVCTRS:
    .word   NORMKEYS    ; unshifted
    .word   SHFTKEYS    ; shifted
    .word   LOGOKEYS    ; commodore
    .word   CTRLKEYS    ; control
    .word   NORMKEYS    ; unshifted
    .word   SHFTKEYS    ; shifted
    .word   WHATKEYS    ; shfited
    .word   CTRLKEYS    ; control
    .word   CHARSET     ; graphics/text control
    .word   WHATKEYS    ; shifted
    .word   WHATKEYS    ; shifted
    .word   CTRLKEYS    ; control

; keyboard decode table - unshifted

NORMKEYS:
    .byte $31,$33,$35,$37,$39,$2B,$5C,$14
    .byte $5F,$57,$52,$59,$49,$50,$2A,$0D
    .byte $04,$41,$44,$47,$4A,$4C,$3B,$1D
    .byte $03,$01,$58,$56,$4E,$2C,$2F,$11
    .byte $20,$5A,$43,$42,$4D,$2E,$01,$85
    .byte $02,$53,$46,$48,$4B,$3A,$3D,$86
    .byte $51,$45,$54,$55,$4F,$40,$5E,$87
    .byte $32,$34,$36,$38,$30,$2D,$13,$88
    .byte $FF

; keyboard decode table - shifted

SHFTKEYS:
    .byte $21,$23,$25,$27,$29,$DB,$A9,$94
    .byte $5F,$D7,$D2,$D9,$C9,$D0,$C0,$8D
    .byte $04,$C1,$C4,$C7,$CA,$CC,$5D,$9D
    .byte $83,$01,$D8,$D6,$CE,$3C,$3F,$91
    .byte $A0,$DA,$C3,$C2,$CD,$3E,$01,$89
    .byte $02,$D3,$C6,$C8,$CB,$5B,$3D,$8A
    .byte $D1,$C5,$D4,$D5,$CF,$BA,$DE,$8B
    .byte $22,$24,$26,$28,$30,$DD,$93,$8C
    .byte $FF

; keyboard decode table - commodore

LOGOKEYS:
    .byte $21,$23,$25,$27,$29,$A6,$A8,$94
    .byte $5F,$B3,$B2,$B7,$A2,$AF,$DF,$8D
    .byte $04,$B0,$AC,$A5,$B5,$B6,$5D,$9D
    .byte $83,$01,$BD,$BE,$AA,$3C,$3F,$91
    .byte $A0,$AD,$BC,$BF,$A7,$3E,$01,$89
    .byte $02,$AE,$BB,$B4,$A1,$5B,$3D,$8A
    .byte $AB,$B1,$A3,$B8,$B9,$A4,$DE,$8B
    .byte $22,$24,$26,$28,$30,$DC,$93,$8C
    .byte $FF


;***********************************************************************************;
;
;## graphics/text control

CHARSET:
    cmp #$0E        ; compare with [SWITCH TO LOWER CASE]
    bne GRAPHMODE   ; branch if not [SWITCH TO LOWER CASE]

    lda #$02        ; set for $8800, lower case characters
    ora VICCR5      ; OR with start of character memory, ROM
    sta VICCR5      ; save start of character memory, ROM
    jmp LAB_E6DC    ; restore registers, set quote flag and exit

GRAPHMODE:
    cmp #$8E        ; compare with [SWITCH TO UPPER CASE]
    bne LAB_ED3F    ; branch if not [SWITCH TO UPPER CASE]

    lda #$FD        ; set for $8000, upper case characters
    and VICCR5      ; and with start of character memory, ROM
    sta VICCR5      ; save start of character memory, ROM

LAB_ED3C:
    jmp LAB_E6DC    ; restore registers, set quote flag and exit

LAB_ED3F:
    cmp #$08        ; compare with disable [SHIFT][C=]
    bne LAB_ED4D    ; branch if not disable [SHIFT][C=]

    lda #$80        ; set to lock shift mode switch
    ora MODE        ; OR with shift mode switch, $00 = enabled, $80 = locked
    sta MODE        ; save shift mode switch
    bmi LAB_ED3C    ; branch always

LAB_ED4D:
    cmp #$09        ; compare with enable [SHIFT][C=]
    bne LAB_ED3C    ; exit if not enable [SHIFT][C=]

    lda #$7F        ; set to unlock shift mode switch
    and MODE        ; and with shift mode switch, $00 = enabled, $80 = locked
    sta MODE        ; save shift mode switch
    bpl LAB_ED3C    ; branch always

; make next screen line start of logical line, increment line length and set pointers

WRAPLINE:
    inx             ; increment screen row
    lda LDTB1,X     ; get start of line X pointer high byte
    ora #$80        ; mark as start of logical line
    sta LDTB1,X     ; set start of line X pointer high byte
    dex             ; restore screen row
    lda LNMX        ; get current screen line length
    clc             ; clear carry for add
    jmp LAB_E715    ; add one line length, set pointers for start of line and
                    ; return


;***********************************************************************************;
;
; keyboard decode table - shifted

WHATKEYS:
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$04,$FF,$FF,$FF,$FF,$FF,$E2
    .byte $9D,$83,$01,$FF,$FF,$FF,$FF,$FF
    .byte $91,$A0,$FF,$FF,$FF,$FF,$EE,$01
    .byte $89,$02,$FF,$FF,$FF,$FF,$E1,$FD
    .byte $8A,$FF,$FF,$FF,$FF,$FF,$B0,$E0
    .byte $8B,$F2,$F4,$F6,$FF,$F0,$ED,$93
    .byte $8C,$FF

; keyboard decode table - control

CTRLKEYS:
    .byte $90,$1C,$9C,$1F,$12,$FF,$FF,$FF
    .byte $06,$FF,$12,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $05,$9F,$1E,$9E,$92,$FF,$FF,$FF
    .byte $FF


;***********************************************************************************;
;
; initial values for VIC registers

VICINIT:
.ifdef PAL
    .byte   $0C     ; interlace and horizontal origin [PAL]
.endif
.ifdef NTSC
    .byte   $05     ; interlace and horizontal origin [NTSC]
.endif
                    ; bit   function
                    ; ---   --------
                    ;  7    interlace / non interlace
                    ; 6-0   horizontal origin
.ifdef PAL
    .byte   $26     ; vertical origin [PAL]
.endif
.ifdef NTSC
    .byte   $19     ; vertical origin [NTSC]
.endif
    .byte   $16     ; video address and columns, $9400 for colour RAM
                    ; bit   function
                    ; ---   --------
                    ;  7    video memory address va9
                    ; 6-0   number of columns
    .byte   $2E     ; screen rows and character height
                    ; bit   function
                    ; ---   --------
                    ;  7    raster line b0
                    ; 6-1   number of rows
                    ;  0    character height (8/16 bits)
    .byte   $00     ; b8-b1 raster line
    .byte   $C0     ; video memory addresses, RAM $1000, ROM $8000
                    ; bit   function
                    ; ---   --------
                    ; 7-4   video memory address va13-va10
                    ; 3-0   character memory address va13-va10

                    ; 0000 ROM  $8000   set 1 - we use this
                    ; 0001  "   $8400
                    ; 0010  "   $8800   set 2
                    ; 0011  "   $8C00
                    ; 1100 RAM  $1000
                    ; 1101  "   $1400
                    ; 1110  "   $1800
                    ; 1111  "   $1C00

    .byte   $00     ; light pen horizontal position
    .byte   $00     ; light pen vertical position

    .byte   $00     ; paddle X
    .byte   $00     ; paddle Y
    .byte   $00     ; oscillator 1 frequency
    .byte   $00     ; oscillator 2 frequency
    .byte   $00     ; oscillator 3 frequency
    .byte   $00     ; white noise frequency
    .byte   $00     ; auxiliary colour and volume
                    ; bit   function
                    ; ---   --------
                    ; 7-4   auxiliary colour
                    ; 3-0   volume
    .byte   $1B     ; background and border colour
                    ; bit   function
                    ; ---   --------
                    ; 7-4   background colour
                    ;  3    reverse video
                    ; 2-0   border colour


;***********************************************************************************;
;
; keyboard buffer for auto load/run

RUNTB:
    .byte "LOAD",$0D,"RUN",$0D


;***********************************************************************************;
;
; low byte screen line addresses

LDTB2:
    .byte $00,$16,$2C,$42
    .byte $58,$6E,$84,$9A
    .byte $B0,$C6,$DC,$F2
    .byte $08,$1E,$34,$4A
    .byte $60,$76,$8C,$A2
    .byte $B8,$CE,$E4


;***********************************************************************************;
;
; command a serial bus device to TALK

; To use this routine the accumulator must first be loaded with a device number
; between 4 and 30. When called this routine converts this device number to a talk
; address. Then this data is transmitted as a command on the Serial bus.

FTALK:
    ora #$40        ; OR with the TALK command
    .byte $2C       ; makes next line bit $2009


;***********************************************************************************;
;
; command devices on the serial bus to LISTEN

; This routine will command a device on the serial bus to receive data. The
; accumulator must be loaded with a device number between 4 and 31 before calling
; this routine. LISTEN convert this to a listen address then transmit this data as
; a command on the serial bus. The specified device will then go into listen mode
; and be ready to accept information.

FLISTEN:
    ora #$20        ; OR with the LISTEN command
    jsr RSPAUSE     ; check RS-232 bus idle


;***********************************************************************************;
;
; send control character

LIST1:
    pha             ; save device address
    bit C3PO        ; test deferred character flag
    bpl LAB_EE2B    ; branch if no deferred character

    sec             ; flag EOI
    ror PCNTR       ; rotate into EOI flag byte
    jsr SRSEND      ; Tx byte on serial bus
    lsr C3PO        ; clear deferred character flag
    lsr PCNTR       ; clear EOI flag
LAB_EE2B:
    pla             ; restore device address
    sta BSOUR       ; save as serial deferred character
    jsr SEROUT1     ; set serial data out high
    cmp #$3F        ; compare read byte with $3F
    bne LAB_EE38    ; branch if not $3F, this branch will always be taken as
                    ; after VIA 2's PCR is read it is anded with $DF, so the
                    ; result can never be $3F

    jsr SRCLKHI     ; set serial clock high
LAB_EE38:
    lda VIA1PA2     ; get VIA 1 DRA, no handshake
    ora #$80        ; set serial ATN low
    sta VIA1PA2     ; set VIA 1 DRA, no handshake


;***********************************************************************************;
;
; if the code drops through to here the serial clock is low and the serial data has been
; released so the following code will have no effect apart from delaying the first byte
; by 1ms

;## set clk/data, wait and Tx byte on serial bus

LAB_EE40:
    jsr SRCLKLO     ; set serial clock low
    jsr SEROUT1     ; set serial data out high
    jsr WAITABIT    ; 1ms delay


;***********************************************************************************;
;
; Tx byte on serial bus

SRSEND:
    sei             ; disable interrupts
    jsr SEROUT1     ; set serial data out high
    jsr SERGET      ; get serial clock status
    lsr             ; shift serial data to Cb
    bcs SRBAD       ; if data high do device not present

    jsr SRCLKHI     ; set serial clock high
    bit PCNTR       ; test EOI flag
    bpl LAB_EE66    ; branch if not EOI

; I think this is the EOI sequence so the serial clock has been released and the serial
; data is being held low by the peripherals. First up wait for the serial data to rise.

LAB_EE5A:
    jsr SERGET      ; get serial clock status
    lsr             ; shift serial data to Cb
    bcc LAB_EE5A    ; loop if data low

; Now the data is high, EOI is signalled by waiting for at least 200us without pulling
; the serial clock line low again. The listener should respond by pulling the serial
; data line low.

LAB_EE60:
    jsr SERGET      ; get serial clock status
    lsr             ; shift serial data to Cb
    bcs LAB_EE60    ; loop if data high

; The serial data has gone low ending the EOI sequence, now just wait for the serial
; data line to go high again or, if this isn't an EOI sequence, just wait for the serial
; data to go high the first time.

LAB_EE66:
    jsr SERGET      ; get serial clock status
    lsr             ; shift serial data to Cb
    bcc LAB_EE66    ; loop if data low

; serial data is high now pull the clock low, preferably within 60us

    jsr SRCLKLO     ; set serial clock low

; Now the VIC has to send the eight bits, LSB first. First it sets the serial data line
; to reflect the bit in the byte, then it sets the serial clock to high. The serial
; clock is left high for 26 cycles, 23us on a PAL VIC, before it is again pulled low
; and the serial data is allowed high again.

    lda #$08        ; eight bits to do
    sta CNTDN       ; set serial bus bit count
LAB_EE73:
    lda VIA1PA2     ; get VIA 1 DRA, no handshake
    cmp VIA1PA2     ; compare with self
    bne LAB_EE73    ; loop if changing

    lsr             ; serial clock to carry
    lsr             ; serial data to carry
    bcc LAB_EEB7    ; if data low do timeout on serial bus

    ror BSOUR       ; rotate transmit byte
    bcs LAB_EE88    ; branch if bit = 1

    jsr SEROUT0     ; else set serial data out low
    bne LAB_EE8B    ; branch always

LAB_EE88:
    jsr SEROUT1     ; set serial data out high
LAB_EE8B:
    jsr SRCLKHI     ; set serial clock high
    NOP             ; waste ..
    NOP             ; .. a ..
    NOP             ; .. cycle ..
    NOP             ; .. or two
    lda VIA2PCR     ; get VIA 2 PCR
    and #$DF        ; set CB2 low, serial data out high
    ora #$02        ; set CA2 high, serial clock out low
    sta VIA2PCR     ; save VIA 2 PCR
    dec CNTDN       ; decrement serial bus bit count
    bne LAB_EE73    ; loop if not all done

; Now all eight bits have been sent it's up to the peripheral to signal the byte was
; received by pulling the serial data low. This should be done within one millisecond.

    lda #$04        ; wait for up to about 1ms
    sta VIA2T2CH    ; set VIA 2 T2C_h
LAB_EEA5:
    lda VIA2IFR     ; get VIA 2 IFR
    and #$20        ; mask T2 interrupt
    bne LAB_EEB7    ; if T2 interrupt do timeout on serial bus

    jsr SERGET      ; get serial clock status
    lsr             ; shift serial data to Cb
    bcs LAB_EEA5    ; if data high go wait some more

    cli             ; enable interrupts
    rts


;***********************************************************************************;
;
; device not present

SRBAD:
    lda #$80        ; error $80, device not present
    .byte   $2C     ; makes next line bit $03A9


;***********************************************************************************;
;
; timeout on serial bus

LAB_EEB7:
    lda #$03        ; error $03, write timeout
LAB_EEB9:
    jsr ORIOST      ; OR into serial status byte
    cli             ; enable interrupts
    clc             ; clear for branch
    bcc LAB_EF09    ; ATN high, delay, clock high then data high, branch always


;***********************************************************************************;
;
; send secondary address after LISTEN

; This routine is used to send a secondary address to an I/O device after a call to
; the LISTEN routine is made and the device commanded to LISTEN. The routine cannot
; be used to send a secondary address after a call to the TALK routine.

; A secondary address is usually used to give set-up information to a device before
; I/O operations begin.

; When a secondary address is to be sent to a device on the serial bus the address
; must first be ORed with $60.

FSECOND:
    sta BSOUR       ; save deferred byte
    jsr LAB_EE40    ; set clk/data, wait and Tx byte on serial bus

; set serial ATN high

SCATN:
    lda VIA1PA2     ; get VIA 1 DRA, no handshake
    and #$7F        ; set serial ATN high
    sta VIA1PA2     ; set VIA 1 DRA, no handshake
    rts


;***********************************************************************************;
;
; send secondary address after TALK

; This routine transmits a secondary address on the serial bus for a TALK device.
; This routine must be called with a number between 4 and 31 in the accumulator.
; The routine will send this number as a secondary address command over the serial
; bus. This routine can only be called after a call to the TALK routine. It will
; not work after a LISTEN.

; A secondary address is usually used to give set-up information to a device before
; I/O operations begin.

; When a secondary address is to be sent to a device on the serial bus the address
; must first be ORed with $60.

FTKSA:
    sta BSOUR       ; save the secondary address byte to transmit
    jsr LAB_EE40    ; set clk/data, wait and Tx byte on serial bus


;***********************************************************************************;
;
; wait for bus end after send

LAB_EED3:
    sei             ; disable interrupts
    jsr SEROUT0     ; set serial data out low
    jsr SCATN       ; set serial ATN high
    jsr SRCLKHI     ; set serial clock high
LAB_EEDD:
    jsr SERGET      ; get serial clock status
    bcs LAB_EEDD    ; branch if clock high

    cli             ; enable interrupts
    rts


;***********************************************************************************;
;
; output a byte to the serial bus

; This routine is used to send information to devices on the serial bus. A call to
; this routine will put a data byte onto the serial bus using full handshaking.
; Before this routine is called the LISTEN routine must be used to command a device
; on the serial bus to get ready to receive data.

; The accumulator is loaded with a byte to output as data on the serial bus. A
; device must be listening or the status word will return a timeout. This routine
; always buffers one character. So when a call to the UNLSN routine is made to end
; the data transmission, the buffered character is sent with EOI set. Then the
; UNLISTEN command is sent to the device.

FCIOUT:
    bit C3PO        ; test deferred character flag
    bmi LAB_EEED    ; branch if deferred character

    sec             ; set carry
    ror C3PO        ; shift into deferred character flag
    bne LAB_EEF2    ; save byte and exit, branch always

LAB_EEED:
    pha             ; save byte
    jsr SRSEND      ; Tx byte on serial bus
    pla             ; restore byte
LAB_EEF2:
    sta BSOUR       ; save deferred byte
    clc             ; flag ok
    rts


;***********************************************************************************;
;
; command the serial bus to UNTALK

; This routine will transmit an UNTALK command on the serial bus. All devices
; previously set to TALK will stop sending data when this command is received.

FUNTLK:
    jsr SRCLKLO     ; set serial clock low
    lda VIA1PA2     ; get VIA 1 DRA, no handshake
    ora #$80        ; set serial ATN low
    sta VIA1PA2     ; set VIA 1 DRA, no handshake

    lda #$5F        ; set the UNTALK command
    .byte   $2C     ; makes next line bit $3FA9


;***********************************************************************************;
;
; command the serial bus to UNLISTEN

; This routine commands all devices on the serial bus to stop receiving data from
; the computer. Calling this routine results in an UNLISTEN command being transmitted
; on the serial bus. Only devices previously commanded to listen will be affected.

; This routine is normally used after the computer is finished sending data to
; external devices. Sending the UNLISTEN will command the listening devices to get
; off the serial bus so it can be used for other purposes.

FUNLSN:
    lda #$3F        ; set the UNLISTEN command
    jsr LIST1       ; send control character

; ATN high, delay, clock high then data high

LAB_EF09:
    jsr SCATN       ; set serial ATN high

; 1ms delay, clock high then data high

LAB_EF0C:
    txa             ; save device number
    ldx #$0B        ; short delay
LAB_EF0F:
    dex             ; decrement count
    bne LAB_EF0F    ; loop if not all done

    tax             ; restore device number
    jsr SRCLKHI     ; set serial clock high
    jmp SEROUT1     ; set serial data out high and return


;***********************************************************************************;
;
; input a byte from the serial bus

; This routine reads a byte of data from the serial bus using full handshaking. The
; data is returned in the accumulator. Before using this routine the TALK routine
; must have been called first to command the device on the serial bus to send data on
; the bus. If the input device needs a secondary command it must be sent by using the
; TKSA routine before calling this routine.

; Errors are returned in the status word which can be read by calling the READST
; routine.

FACPTR:
    sei             ; disable interrupts
    lda #$00        ; clear .A
    sta CNTDN       ; clear serial bus bit count
    jsr SRCLKHI     ; set serial clock high
LAB_EF21:
    jsr SERGET      ; get serial clock status
    bcc LAB_EF21    ; loop while clock low

    jsr SEROUT1     ; set serial data out high
LAB_EF29:
    lda #$01        ; set timeout count high byte
    sta VIA2T2CH    ; set VIA 2 T2C_h
LAB_EF2E:
    lda VIA2IFR     ; get VIA 2 IFR
    and #$20        ; mask T2 interrupt
    bne LAB_EF3C    ; branch if T2 interrupt

    jsr SERGET      ; get serial clock status
    bcs LAB_EF2E    ; loop if clock high

    bcc LAB_EF54    ; else go set 8 bits to do, branch always

                    ; T2 timed out
LAB_EF3C:
    lda CNTDN       ; get serial bus bit count
    beq LAB_EF45    ; if not already EOI then go flag EOI

    lda #$02        ; error $02, read timeout
    jmp LAB_EEB9    ; set serial status and exit

LAB_EF45:
    jsr SEROUT0     ; set serial data out low
    jsr LAB_EF0C    ; 1ms delay, clock high then data high
    lda #$40        ; set EOI
    jsr ORIOST      ; OR into serial status byte
    inc CNTDN       ; increment serial bus bit count, do error on next timeout
    bne LAB_EF29    ; go try again

LAB_EF54:
    lda #$08        ; 8 bits to do
    sta CNTDN       ; set serial bus bit count
LAB_EF58:
    lda VIA1PA2     ; get VIA 1 DRA, no handshake
    cmp VIA1PA2     ; compare with self
    bne LAB_EF58    ; loop if changing

    lsr             ; serial clock into carry
    bcc LAB_EF58    ; loop while serial clock low

    lsr             ; serial data into carry
    ror FIRT        ; shift data bit into receive byte
LAB_EF66:
    lda VIA1PA2     ; get VIA 1 DRA, no handshake
    cmp VIA1PA2     ; compare with self
    bne LAB_EF66    ; loop if changing

    lsr             ; serial clock into carry
    bcs LAB_EF66    ; loop while serial clock high

    dec CNTDN       ; decrement serial bus bit count
    bne LAB_EF58    ; loop if not all done

    jsr SEROUT0     ; set serial data out low
    lda STATUS      ; get serial status byte
    beq LAB_EF7F    ; branch if no error

    jsr LAB_EF0C    ; 1ms delay, clock high then data high
LAB_EF7F:
    lda FIRT        ; get receive byte
    cli             ; enable interrupts
    clc
    rts


;***********************************************************************************;
;
; set serial clock high

SRCLKHI:
    lda VIA2PCR     ; get VIA 2 PCR
    and #$FD        ; set CA2 low, serial clock out high
    sta VIA2PCR     ; set VIA 2 PCR
    rts


;***********************************************************************************;
;
; set serial clock low

SRCLKLO:
    lda VIA2PCR     ; get VIA 2 PCR
    ora #$02        ; set CA2 high, serial clock out low
    sta VIA2PCR     ; set VIA 2 PCR
    rts


;***********************************************************************************;
;
; 1ms delay

WAITABIT:
    lda #$04        ; set for 1024 cycles
    sta VIA2T2CH    ; set VIA 2 T2C_h
LAB_EF9B:
    lda VIA2IFR     ; get VIA 2 IFR
    and #$20        ; mask T2 interrupt
    beq LAB_EF9B    ; loop until T2 interrupt

    rts


;***********************************************************************************;
;
; RS-232 Tx NMI routine

RSNXTBIT:
    lda BITTS       ; get RS-232 bit count
    beq RSNXTBYT    ; if zero go setup next RS-232 Tx byte and return

    bmi RSSTOPS     ; if -ve go do stop bit(s)

                    ; else bit count is non zero and +ve
    lsr RODATA      ; shift RS-232 output byte buffer
    ldx #$00        ; set $00 for bit = 0
    bcc LAB_EFB0    ; branch if bit was 0

    dex             ; set $FF for bit = 1
LAB_EFB0:
    txa             ; copy bit to .A
    eor ROPRTY      ; XOR with RS-232 parity byte
    sta ROPRTY      ; save RS-232 parity byte
    dec BITTS       ; decrement RS-232 bit count
    beq RSPRTY      ; if RS-232 bit count now zero go do parity bit

; save bit and exit

LAB_EFB9:
    txa             ; copy bit to .A
    and #$20        ; mask for CB2 control bit
    sta NXTBIT      ; save RS-232 next bit to send
    rts

; do RS-232 parity bit, enters with RS-232 bit count = 0

RSPRTY:
    lda #$20        ; mask 00x0 0000, parity enable bit
    bit M51CDR      ; test pseudo 6551 command register
    beq LAB_EFDA    ; branch if parity disabled

    bmi LAB_EFE4    ; branch if fixed mark or space parity

    BVS LAB_EFDE    ; branch if even parity

                    ; else odd parity
    lda ROPRTY      ; get RS-232 parity byte
    bne LAB_EFCF    ; if parity not zero leave parity bit = 0

LAB_EFCE:
    dex             ; make parity bit = 1
LAB_EFCF:
    dec BITTS       ; decrement RS-232 bit count, 1 stop bit
    lda M51CTR      ; get pseudo 6551 control register
    bpl LAB_EFB9    ; if 1 stop bit save parity bit and exit

                    ; else two stop bits ..
    dec BITTS       ; decrement RS-232 bit count, 2 stop bits
    bne LAB_EFB9    ; save bit and exit, branch always

                    ; parity is disabled so the parity bit becomes the first,
                    ; and possibly only, stop bit. to do this increment the bit
                    ; count which effectively decrements the stop bit count.
LAB_EFDA:
    inc BITTS       ; increment RS-232 bit count, = -1 stop bit
    bne LAB_EFCE    ; set stop bit = 1 and exit

                    ; do even parity
LAB_EFDE:
    lda ROPRTY      ; get RS-232 parity byte
    beq LAB_EFCF    ; if parity zero leave parity bit = 0

    bne LAB_EFCE    ; else make parity bit = 1, branch always

                    ; fixed mark or space parity
LAB_EFE4:
    BVS LAB_EFCF    ; if fixed space parity leave parity bit = 0

    BVC LAB_EFCE    ; else fixed mark parity make parity bit = 1, branch always

; decrement stop bit count, set stop bit = 1 and exit. $FF is one stop bit, $FE is two
; stop bits

RSSTOPS:
    inc BITTS       ; decrement RS-232 bit count
    ldx #$FF        ; set stop bit = 1
    bne LAB_EFB9    ; save stop bit and exit, branch always

; setup next RS-232 Tx byte

RSNXTBYT:
    lda M51CDR      ; get 6551 pseudo command register
    lsr             ; handshake bit into Cb
    bcc LAB_EFFB    ; branch if 3 line interface

    bit VIA2PB      ; test VIA 2 DRB, this is wrong, the address should be
                    ; VIA1PB which is VIA 1 which is where the DSR and
                    ; CTS inputs really are ##

    bpl RSMISSNG    ; if DSR = 0 set DSR signal not present and exit

    BVC LAB_F019    ; if CTS = 0 set CTS signal not present and exit

                    ; was 3 line interface
LAB_EFFB:
    lda #$00        ; clear .A
    sta ROPRTY      ; clear RS-232 parity byte
    sta NXTBIT      ; clear RS-232 next bit to send
    ldx BITNUM      ; get number of bits to be sent/received
    stx BITTS       ; set RS-232 bit count
    ldy RODBS       ; get index to Tx buffer start
    cpy RODBE       ; compare with index to Tx buffer end
    beq LAB_F021    ; if all done go disable T1 interrupt and return

    lda (ROBUF),Y   ; else get byte from buffer
    sta RODATA      ; save to RS-232 output byte buffer
    inc RODBS       ; increment index to Tx buffer start
    rts


;***********************************************************************************;
;
;## exit or quit
; set DSR signal not present

RSMISSNG:
    lda #$40        ; set DSR signal not present
    .byte   $2C     ; makes next line bit $10A9

; set CTS signal not present

LAB_F019:
    lda #$10        ; set CTS signal not present
    ora RSSTAT      ; OR with RS-232 status register
    sta RSSTAT      ; save RS-232 status register

; disable T1 interrupt

LAB_F021:
    lda #$40        ; disable T1 interrupt
    sta VIA1IER     ; set VIA 1 IER
    rts


;***********************************************************************************;
;
; compute bit count

RSCPTBIT:
    ldx #$09        ; set bit count to 9, 8 data + 1 stop bit
    lda #$20        ; mask for 8/7 data bits
    bit M51CTR      ; test pseudo 6551 control register
    beq LAB_F031    ; branch if 8 bits

    dex             ; else decrement count for 7 data bits
LAB_F031:
    BVC LAB_F035    ; branch if 7 bits

    dex             ; else decrement count ..
    dex             ; .. for 5 data bits
LAB_F035:
    rts


;***********************************************************************************;
;
; RS-232 Rx NMI

RSINBIT:
    ldx RINONE      ; get start bit check flag
    bne RSSTRBIT    ; branch if no start bit received

    dec BITCI       ; decrement receiver bit count in
    beq RSINBYTE

    bmi LAB_F04D

    lda INBIT       ; get receiver input bit temporary storage
    eor RIPRTY
    sta RIPRTY
    lsr INBIT       ; shift receiver input bit temporary storage
    ror RIDATA
LAB_F04A:
    rts

RSSTPBIT:
    dec BITCI       ; decrement receiver bit count in
LAB_F04D:
    lda INBIT       ; get receiver input bit temporary storage
    beq LAB_F0B3

    lda M51CTR      ; get pseudo 6551 control register
    asl
    lda #$01
    adc BITCI       ; add receiver bit count in
    bne LAB_F04A


;***********************************************************************************;
;
;## setup to Rx

RSPREPIN:
    lda #$90        ; enable CB1 interrupt
    sta VIA1IER     ; set VIA 1 IER
    sta RINONE      ; set start bit check flag, set no start bit received
    lda #$20        ; disable T2 interrupt
    sta VIA1IER     ; set VIA 1 IER
    rts


;***********************************************************************************;
;
; no RS-232 start bit received

RSSTRBIT:
    lda INBIT       ; get receiver input bit temporary storage
    bne RSPREPIN

    sta RINONE      ; set start bit check flag, set start bit received
    rts


;***********************************************************************************;
;
; ??

RSINBYTE:
    ldy RIDBE       ; get index to Rx buffer end
    INY             ; increment index
    cpy RIDBS       ; compare with index to Rx buffer start
    beq RSOVERUN    ; if buffer full go do Rx overrun error

    sty RIDBE       ; save index to Rx buffer end
    dey             ; decrement index
    lda RIDATA      ; get assembled byte
    ldx BITNUM      ; get bit count
LAB_F081:
    cpx #$09        ; compare with byte + stop
    beq LAB_F089    ; branch if all nine bits received

    lsr             ; else shift byte
    inx             ; increment bit count
    bne LAB_F081    ; loop, branch always

LAB_F089:
    sta (RIBUF),Y   ; save received byte to Rx buffer
    lda #$20        ; mask 00x0 0000, parity enable bit
    bit M51CDR      ; test pseudo 6551 command register
    beq RSSTPBIT    ; branch if parity disabled

    bmi LAB_F04A    ; branch if mark or space parity

    lda INBIT       ; get receiver input bit temporary storage
    eor RIPRTY
    beq RSPRTYER

    BVS LAB_F04A

    .byte   $2C     ; makes next line bit $AB50
RSPRTYER:
    BVC LAB_F04A

    lda #$01        ; set Rx parity error
    .byte   $2C     ; makes next line bit $04A9

RSOVERUN:
    lda #$04        ; set Rx overrun error
    .byte   $2C     ; makes next line bit $80A9

RSBREAK:
    lda #$80        ; Rx break error
    .byte   $2C     ; makes next line bit $02A9

RSFRAMER:
    lda #$02        ; Rx frame error
    ora RSSTAT      ; OR with RS-232 status byte
    sta RSSTAT      ; save RS-232 status byte
    jmp RSPREPIN

LAB_F0B3:
    lda RIDATA
    bne RSFRAMER    ; if ?? do frame error

    beq RSBREAK     ; else do break error, branch always


;***********************************************************************************;
;
; do illegal device number

RSDVCERR:
    jmp FE_ILDEV    ; do illegal device number and return


;***********************************************************************************;
;
; open RS-232 channel for output

RSOPNOUT:
    sta DFLTO       ; save output device number
    lda M51CDR      ; get pseudo 6551 command register
    lsr             ; shift handshake bit to carry
    bcc LAB_F0EB    ; branch if 3 line interface

    lda #$02        ; mask for rts out
    bit VIA1PB      ; test VIA 1 DRB
    bpl LAB_F0E8    ; if DSR = 0 set DSR not present and exit

    bne LAB_F0EB    ; if rts = 1 just exit

LAB_F0CD:
    lda VIA1IER     ; get VIA 1 IER
    and #$30        ; mask 00xx 0000, T2 and CB1 interrupts
    bne LAB_F0CD    ; loop while either enabled

LAB_F0D4:
    bit VIA1PB      ; test VIA 1 DRB
    BVS LAB_F0D4    ; loop while CTS high

    lda VIA1PB      ; get VIA 1 DRB
    ora #$02        ; set rts high
    sta VIA1PB      ; save VIA 1 DRB
LAB_F0E1:
    bit VIA1PB      ; test VIA 1 DRB
    BVS LAB_F0EB    ; exit if CTS high

    bmi LAB_F0E1    ; loop while DSR high

LAB_F0E8:
    jsr RSMISSNG    ; set DSR signal not present
LAB_F0EB:
    clc             ; flag ok
    rts


;***********************************************************************************;
;
; send byte to RS-232 buffer

RSOUTSAV:
    ldy RODBE       ; get index to Tx buffer end
    INY             ; + 1
    cpy RODBS       ; compare with index to Tx buffer start
    beq RSOUTSAV    ; loop while buffer full

    sty RODBE       ; set index to Tx buffer end
    dey             ; index to available buffer byte
    sta (ROBUF),Y   ; save byte to buffer
    bit VIA1IER     ; test VIA 1 IER
    BVC RSPREPOT    ; branch if T1 not enabled

    rts

RSPREPOT:
    lda BAUDOF      ; get baud rate bit time low byte
    sta VIA1T1CL    ; set VIA 1 T1C_l
    lda BAUDOF+1    ; get baud rate bit time high byte
    sta VIA1T1CH    ; set VIA 1 T1C_h
    lda #$C0        ; enable T1 interrupt
    sta VIA1IER     ; set VIA 1 IER
    jmp RSNXTBYT    ; setup next RS-232 Tx byte and return


;***********************************************************************************;
;
; input from RS-232 buffer

RSOPNIN:
    sta DFLTN       ; save input device number
    lda M51CDR      ; get pseudo 6551 command register
    LSR
    bcc LAB_F146    ; branch if 3 line interface

    and #$08        ; mask duplex bit, pseudo 6551 command is >> 1
    beq LAB_F146    ; branch if full duplex

    lda #$02
    bit VIA1PB      ; test VIA 1 DRB
    bpl LAB_F0E8

    beq LAB_F144

LAB_F12B:
    bit VIA1IER     ; test VIA 1 IER
    BVS LAB_F12B    ; loop while T1 interrupt enabled

    lda VIA1PB      ; get VIA 1 DRB
    and #$FD        ; mask xxxx xx0x, clear rts out
    sta VIA1PB      ; save VIA 1 DRB
LAB_F138:
    lda VIA1PB      ; get VIA 1 DRB
    and #$04        ; mask xxxx x1xx, DTR
    beq LAB_F138    ; loop while DTR low

LAB_F13F:
    lda #$90        ; enable CB1 interrupt
    sta VIA1IER     ; set VIA 1 IER
LAB_F144:
    clc
    rts

LAB_F146:
    lda VIA1IER     ; get VIA 1 IER
    and #$30        ; mask 0xx0 0000, T1 and T2 interrupts
    beq LAB_F13F    ; if both interrupts disabled go enable CB1
                    ; interrupt and exit

    clc
    rts


;***********************************************************************************;
;
; get byte from RS-232 buffer

RSNXTIN:
    ldy RIDBS       ; get index to Rx buffer start
    cpy RIDBE       ; compare with index to Rx buffer end
    beq LAB_F15D    ; return null if buffer empty

    lda (RIBUF),Y   ; get byte from Rx buffer
    inc RIDBS       ; increment index to Rx buffer start
    rts

LAB_F15D:
    lda #$00        ; return null
    rts


;***********************************************************************************;
;
; check RS-232 bus idle

RSPAUSE:
    pha             ; save .A
    lda VIA1IER     ; get VIA 1 IER
    beq LAB_F172    ; branch if no interrupts enabled. this branch will
                    ; never be taken as b7 of IER always reads as 1
                    ; according to the 6522 data sheet
LAB_F166:
    lda VIA1IER     ; get VIA 1 IER
    and #$60        ; mask 0xx0 0000, T1 and T2 interrupts
    bne LAB_F166    ; loop if T1 or T2 active

    lda #$10        ; disable CB1 interrupt
    sta VIA1IER     ; set VIA 1 IER
LAB_F172:
    pla             ; restore .A
    rts


;***********************************************************************************;
;
; KERNAL I/O messages

KMSGTBL:
KM_IOERR:   .byte   $0D,"I/O ERROR ",'#'+$80
KM_SRCHG:   .byte   $0D,"SEARCHING",' '+$80
KM_FOR:     .byte   "FOR",' '+$80
KM_PRPLY:   .byte   $0D,"PRESS plaY ON TAP",'E'+$80
KM_REcpy:   .byte   "PRESS RECORD & plaY ON TAP",'E'+$80
KM_LODNG:   .byte   $0D,"LOADIN",'G'+$80
KM_SAVNG:   .byte   $0D,"SAVING",' '+$80
KM_VFYNG:   .byte   $0D,"VERIFYIN",'G'+$80
KM_FOUND:   .byte   $0D,"FOUND",' '+$80
KM_OK:      .byte   $0D,"OK",$0D+$80


;***********************************************************************************;
;
; display control I/O message if in direct mode

SPMSG:
    bit MSGFLG      ; test message mode flag
    bpl LAB_F1F3    ; exit if control messages off

; display KERNAL I/O message

KMSGSHOW:
    lda KMSGTBL,Y   ; get byte from message table
    php             ; save status
    and #$7F        ; clear b7
    jsr CHROUT      ; output character to channel
    INY             ; increment index
    plp             ; restore status
    bpl KMSGSHOW    ; loop if not end of message

LAB_F1F3:
    clc
    rts


;***********************************************************************************;
;
; get a character from the input device

; In practice this routine operates identically to the CHRIN routine for all devices
; except for the keyboard. If the keyboard is the current input device this routine
; will get one character from the keyboard buffer. It depends on the IRQ routine to
; read the keyboard and put characters into the buffer.

; If the keyboard buffer is empty the value returned in the accumulator will be zero.

FGETIN:
    lda DFLTN       ; get input device number
    bne LAB_F201    ; branch if not keyboard

                    ; input device was keyboard
    lda NDX         ; get keyboard buffer length
    beq LAB_F26A    ; if buffer empty go flag no byte and return

    sei             ; disable interrupts
    jmp LP2         ; input from keyboard buffer and return

                    ; input device was not keyboard
LAB_F201:
    cmp #$02        ; compare device with RS-232 device
    bne LAB_F21D    ; branch if not RS-232 device

                    ; input device is RS-232 device
LAB_F205:
    sty XSAV        ; save .Y
    jsr RSNXTIN     ; get byte from RS-232 buffer
    ldy XSAV        ; restore .Y
    clc             ; flag no error
    rts


;***********************************************************************************;
;
; input character from channel

; This routine will get a byte of data from the channel already set up as the input
; channel by the CHKIN routine.

; If CHKIN has not been used to define another input channel the data is expected to be
; from the keyboard. the data byte is returned in the accumulator. The channel remains
; open after the call.

; Input from the keyboard is handled in a special way. First, the cursor is turned on
; and it will blink until a carriage return is typed on the keyboard. All characters
; on the logical line, up to 88 characters, will be stored in the BASIC input buffer.
; Then the characters can be returned one at a time by calling this routine once for
; each character. When the carriage return is returned the entire line has been
; processed. the next time this routine is called the whole process begins again.

FCHRIN:
    lda DFLTN       ; get input device number
    bne LAB_F21D    ; if it's not the keyboard continue

                    ; the input device is the keyboard
    lda PNTR        ; get cursor column
    sta LXSP+1      ; set input cursor column
    lda TBLX        ; get cursor row
    sta LXSP        ; set input cursor row
    jmp GETSCRN     ; go get input from the keyboard

; the input device was not the keyboard

LAB_F21D:
    cmp #$03        ; compare device number with screen
    bne LAB_F22A    ; if it's not the screen continue

                    ; the input device is the screen
    sta CRSW        ; input from keyboard or screen, $xx = screen,
                    ; $00 = keyboard
    lda LNMX        ; get current screen line length
    sta INDX        ; save input [EOL] pointer
    jmp GETSCRN     ; go get input from the screen

; the input device was not the screen

LAB_F22A:
    bcs CHRINSR     ; if input device is the serial bus go handle it

; the input device is < the screen so must be the RS-232 or tape device

    cmp #$02        ; compare device with RS-232 device
    beq CHRINRS     ; if it's the RS-232 device go handle it

; else there's only the tape device left ..

    stx XSAV        ; save .X
    jsr CHRINTP2    ; get byte from tape
    bcs LAB_F24D    ; exit if error

    pha             ; save byte
    jsr CHRINTP2    ; get next byte from tape
    bcs LAB_F24A    ; exit if error

    bne LAB_F244    ; branch if end reached

    lda #$40        ; set [EOF] bit
    jsr ORIOST      ; OR into serial status byte
LAB_F244:
    dec BUFPNT      ; decrement tape buffer index
    ldx XSAV        ; restore .X
    pla             ; restore saved byte
    rts

; error exit from input character

LAB_F24A:
    tax             ; copy error byte
    pla             ; dump saved byte
    txa             ; restore error byte
LAB_F24D:
    ldx XSAV        ; restore .X
    rts


;***********************************************************************************;
;
; get byte from tape

CHRINTP2:
    jsr JTP20       ; bump tape pointer
    bne LAB_F260    ; if not end get next byte and exit

    jsr RDTPBLKS    ; initiate tape read
    bcs LAB_F26B    ; exit if error flagged

    lda #$00        ; clear .A
    sta BUFPNT      ; clear tape buffer index
    beq CHRINTP2    ; loop, branch always

LAB_F260:
    lda (TAPE1),Y   ; get next byte from buffer
    clc             ; flag no error
    rts


;***********************************************************************************;
;
; the input device was the serial bus

CHRINSR:
    lda STATUS      ; get serial status byte
    beq LAB_F26C    ; if no errors flagged go input byte and return

    lda #$0D        ; else return [EOL]
LAB_F26A:
    clc             ; flag no error
LAB_F26B:
    rts

LAB_F26C:
    jmp FACPTR      ; input a byte from the serial bus and return

                    ; input device was RS-232 device
CHRINRS:
    jsr LAB_F205    ; get byte from RS-232 device
    bcs LAB_F279    ; branch if error, this doesn't get taken as the last
                    ; instruction in the get byte from RS-232 device routine
                    ; is clc
    cmp #$00        ; compare with null
    beq CHRINRS     ; loop if null

    clc             ; flag no error
LAB_F279:
    rts


;***********************************************************************************;
;
; output a character to channel

; This routine will output a character to an already opened channel. Use the OPEN
; routine, OPEN, and the CHKOUT routine, to set up the output channel before calling
; this routine. If these calls are omitted, data will be sent to the default output
; device, device 3, the screen. The data byte to be output is loaded into the accumlator,
; and this routine is called. The data is then sent to the specified output device.
; The channel is left open after the call.

; NOTE: Care must be taken when using routine to send data to a serial device since
; data will be sent to all open output channels on the bus. Unless this is desired,
; all open output channels on the serial bus other than the actually intended
; destination channel must be closed by a call to the KERNAL close channel routine.

FCHROUT:
    pha             ; save the character to send
    lda DFLTO       ; get output device number
    cmp #$03        ; compare device number with screen
    bne LAB_F285    ; if output device not screen continue

; the output device is the screen

    pla             ; restore character to send
    jmp SCRNOUT     ; output character and return

; the output device was not the screen

LAB_F285:
    bcc LAB_F28B    ; if output device < screen continue

; the output device was > screen so it is a serial bus device

    pla             ; restore character to send
    jmp FCIOUT      ; output a byte to the serial bus and return

; the output device is < screen

LAB_F28B:
    cmp #$02        ; compare the device with RS-232 device
    beq LAB_F2B9    ; if output device is RS-232 device go handle it

; else the output device is the cassette

    pla             ; restore the character to send


;***********************************************************************************;
;
; output a character to the cassette

CHROUTTP:
    sta PTR1        ; save character to character buffer
    pha             ; save .A
    txa             ; copy .X
    pha             ; save .X
    tya             ; copy .Y
    pha             ; save .Y
    jsr JTP20       ; bump tape pointer
    bne LAB_F2AA    ; if not end save next byte and exit

    jsr WBLK        ; initiate tape write
    bcs LAB_F2AF    ; exit if error

    lda #$02        ; set data block file type
    ldy #$00        ; clear index
    sta (TAPE1),Y   ; save file type to tape buffer
    INY             ; increment index
    sty BUFPNT      ; save tape buffer index
LAB_F2AA:
    lda PTR1        ; restore character from character buffer
    sta (TAPE1),Y   ; save to buffer
    clc             ; flag no error
LAB_F2AF:
    pla             ; pull .Y
    tay             ; restore .Y
    pla             ; pull .X
    tax             ; restore .X
    pla             ; restore .A
    bcc LAB_F2B8    ; exit if no error

    lda #$00        ; else clear .A
LAB_F2B8:
    rts


;***********************************************************************************;
;
; the output device is RS-232 device

LAB_F2B9:
    pla             ; restore character to send
    stx XSAV        ; save .X
    sty PTR1        ; save .Y
    jsr RSOUTSAV    ; send byte to RS-232 buffer
    ldx XSAV        ; restore .Y
    ldy PTR1        ; restore .X
    clc             ; flag ok
    rts


;***********************************************************************************;
;
; open a channel for input

; Any logical file that has already been opened by the OPEN routine can be defined as
; an input channel by this routine. The device on the channel must be an input device
; or an error will occur and the routine will abort.

; If you are getting data from anywhere other than the keyboard, this routine must be
; called before using either the CHRIN routine or the GETIN routine. If you are
; getting data from the keyboard and no other input channels are open then the calls
; to this routine and to the OPEN routine are not needed.

; When used with a device on the serial bus this routine will automatically send the
; listen address specified by the OPEN routine and any secondary address.

; Possible errors are:
;
;   3 : file not open
;   5 : device not present
;   6 : file is not an input file

FCHKIN:
    jsr FNDFLNO     ; find file
    beq LAB_F2CF    ; branch if file opened

    jmp FE_NTOPN    ; do file not open error and return

LAB_F2CF:
    jsr SETFLCH     ; set file details from table,X
    lda FA          ; get device number
    beq LAB_F2EC    ; if device was keyboard save device #, flag ok and exit

    cmp #$03        ; compare device number with screen
    beq LAB_F2EC    ; if device was screen save device #, flag ok and exit

    bcs LAB_F2F0    ; branch if serial bus device

    cmp #$02        ; compare device with RS-232 device
    bne LAB_F2E3    ; branch if not RS-232 device

    jmp RSOPNIN     ; else get input from RS-232 buffer and return

LAB_F2E3:
    ldx SA          ; get secondary address
    cpx #$60        ; compare with read
    beq LAB_F2EC    ; branch if read

    jmp FE_NTINP    ; do not input file error and return

LAB_F2EC:
    sta DFLTN       ; save input device number
    clc             ; flag ok
    rts

                    ; device was serial bus device
LAB_F2F0:
    tax             ; copy device number to .X
    jsr FTALK       ; command a serial bus device to TALK
    lda SA          ; get secondary address
    bpl LAB_F2FE

    jsr LAB_EED3    ; wait for bus end after send
    jmp LAB_F301

LAB_F2FE:
    jsr FTKSA       ; send secondary address after TALK
LAB_F301:
    txa             ; copy device back to .A
    bit STATUS      ; test serial status byte
    bpl LAB_F2EC    ; if device present save device number and exit

    jmp FE_DVNTP    ; do device not present error and return


;***********************************************************************************;
;
; open a channel for output

; Any logical file that has already been opened by the OPEN routine can be defined
; as an output channel by this routine the device on the channel must be an output
; output device or an error will occur and the routine will abort.

; If you are sending data to anywhere other than the screen this routine must be
; called before using the CHROUT routine. If you are sending data to the screen and
; no other output channels are open then the calls to this routine and to the OPEN
; routine are not needed.

; When used with a device on the serial bus this routine will automatically send the
; listen address specified by the OPEN routine and any secondary address.

; Possible errors are:
;
;   3 : file not open
;   5 : device not present
;   7 : file is not an output file

FCHKOUT:
    jsr FNDFLNO     ; find file
    beq LAB_F311    ; branch if file found

    jmp FE_NTOPN    ; do file not open error and return

LAB_F311:
    jsr SETFLCH     ; set file details from table,X
    lda FA          ; get device number
    bne LAB_F31B    ; branch if device is not keyboard

LAB_F318:
    jmp FE_NTOUT    ; do not output file error and return

LAB_F31B:
    cmp #$03        ; compare device number with screen
    beq LAB_F32E    ; if screen save output device number and exit

    bcs LAB_F332    ; branch if > screen, serial bus device

    cmp #$02        ; compare device with RS-232 device
    bne LAB_F328    ; branch if not RS-232 device, must be tape

    jmp RSOPNOUT    ; open RS-232 channel for output

                    ; open tape channel for output
LAB_F328:
    ldx SA          ; get secondary address
    cpx #$60        ; compare with read
    beq LAB_F318    ; if read do not output file error and return

LAB_F32E:
    sta DFLTO       ; save output device number
    clc             ; flag ok
    rts

LAB_F332:
    tax             ; copy device number
    jsr FLISTEN     ; command devices on the serial bus to LISTEN
    lda SA          ; get secondary address
    bpl LAB_F33F    ; branch if address to send

    jsr SCATN       ; else set serial ATN high
    bne LAB_F342    ; branch always

LAB_F33F:
    jsr FSECOND     ; send secondary address after LISTEN
LAB_F342:
    txa             ; copy device number back to .A
    bit STATUS      ; test serial status byte
    bpl LAB_F32E    ; if device present save output device number and exit

    jmp FE_DVNTP    ; else do device not present error and return


;***********************************************************************************;
;
; close a specified logical file

; This routine is used to close a logical file after all I/O operations have been
; completed on that file. This routine is called after the accumulator is loaded
; with the logical file number to be closed, the same number used when the file was
; opened using the OPEN routine.

FCLOSE:
    jsr LAB_F3D4    ; find file .A
    beq LAB_F351    ; if the file is found go close it

    clc             ; else the file was closed so just flag ok
    rts

; found the file so close it

LAB_F351:
    jsr SETFLCH     ; set file details from table,X
    txa             ; copy file index to .A
    pha             ; save file index
    lda FA          ; get device number
    beq LAB_F3B1    ; if $00, keyboard, restore index and close file

    cmp #$03        ; compare device number with screen
    beq LAB_F3B1    ; if screen restore index and close file

    bcs LAB_F3AE    ; if > screen go do serial bus device close

    cmp #$02        ; compare device with RS-232 device
    bne LAB_F38D    ; branch if not RS-232 device

                    ; else close RS-232 device
    pla             ; restore file index
    jsr LAB_F3B2    ; close file index .X
    lda #$7D        ; disable T1, T2, CB1, CB2, SR and CA2
    sta VIA1IER     ; set VIA 1 IER
    lda #$06        ; set DTR and rts high
    sta VIA1PB      ; set VIA 1 DRB
    lda #$EE        ; CB2 high, CB1 -ve edge, CA2 high, CA1 -ve edge
    sta VIA1PCR     ; set VIA 1 PCR
    jsr LAB_FE75    ; read the top of memory
    lda RIBUF+1     ; get RS-232 input buffer pointer high byte
    beq LAB_F37F    ; branch if no RS-232 input buffer

    INY             ; else reclaim RS-232 input buffer memory
LAB_F37F:
    lda ROBUF+1     ; get RS-232 output buffer pointer high byte
    beq LAB_F384    ; branch if no RS-232 output buffer

    INY             ; else reclaim RS-232 output buffer memory
LAB_F384:
    lda #$00        ; clear .A
    sta RIBUF+1     ; clear RS-232 input buffer pointer high byte
    sta ROBUF+1     ; clear RS-232 output buffer pointer high byte
    jmp LAB_F53C    ; go set top of memory and exit

LAB_F38D:
    lda SA          ; get secondary address
    and #$0F        ; mask the OPEN CHANNEL command
    beq LAB_F3B1    ; if read restore index and close file

    jsr TPBUFA      ; get tape buffer start pointer in .X.Y
    lda #$00        ; character $00
    jsr CHROUTTP    ; output character to cassette
    jmp PATCH3      ; go do CLOSE tail

LAB_F39E:
    bcs LAB_F3CE    ; just exit if error

    lda SA          ; get secondary address
    cmp #$62        ; compare with end of tape flag
    bne LAB_F3B1    ; if not end of tape restore index and close file

    lda #$05        ; set logical end of the tape
    jsr TAPEH       ; write tape header
    jmp LAB_F3B1    ; restore index and close file


;***********************************************************************************;
;
; do serial bus device file close

LAB_F3AE:
    jsr LAB_F6DA    ; close serial bus device
LAB_F3B1:
    pla             ; restore file index


;***********************************************************************************;
;
; close file index .X

LAB_F3B2:
    tax             ; copy index to file to close
    dec LDTND       ; decrement open file count
    cpx LDTND       ; compare index with open file count
    beq LAB_F3CD    ; exit if equal, last entry was closing file

                    ; else entry was not last in list so copy last table entry
                    ; file details over the details of the closing one
    ldy LDTND       ; get open file count as index
    lda LAT,Y       ; get last+1 logical file number from logical file table
    sta LAT,X       ; save logical file number over closed file
    lda FAT,Y       ; get last+1 device number from device number table
    sta FAT,X       ; save device number over closed file
    lda SAT,Y       ; get last+1 secondary address from secondary address table
    sta SAT,X       ; save secondary address over closed file
LAB_F3CD:
    clc
LAB_F3CE:
    rts


;***********************************************************************************;
;
; find file

FNDFLNO:
    lda #$00        ; clear .A
    sta STATUS      ; clear serial status byte
    txa             ; copy logical file number to .A

; find file .A

LAB_F3D4:
    ldx LDTND       ; get open file count
LAB_F3D6:
    dex             ; decrement count to give index
    bmi LAB_F3EE    ; exit if no files

    cmp LAT,X       ; compare logical file number with table logical file number
    bne LAB_F3D6    ; loop if no match

    rts


;***********************************************************************************;
;
; set file details from table,X

SETFLCH:
    lda LAT,X       ; get logical file from logical file table
    sta LA          ; set logical file
    lda FAT,X       ; get device number from device number table
    sta FA          ; set device number
    lda SAT,X       ; get secondary address from secondary address table
    sta SA          ; set secondary address
LAB_F3EE:
    rts


;***********************************************************************************;
;
; close all channels and files

; This routine closes all open files. When this routine is called, the pointers into
; the open file table are reset, closing all files. Also the routine automatically
; resets the I/O channels.

FCLALL:
    lda #$00        ; clear .A
    sta LDTND       ; clear open file count


;***********************************************************************************;
;
; close input and output channels

; This routine is called to clear all open channels and restore the I/O channels to
; their original default values. It is usually called after opening other I/O
; channels and using them for input/output operations. The default input device is
; 0, the keyboard. The default output device is 3, the screen.

; If one of the channels to be closed is to the serial bus, an UNTALK signal is sent
; first to clear the input channel or an UNLISTEN is sent to clear the output channel.
; By not calling this routine and leaving listener(s) active on the serial bus,
; several devices can receive the same data from the VIC at the same time. One way to
; take advantage of this would be to command the printer to LISTEN and the disk to
; TALK. This would allow direct printing of a disk file.

FCLRCHN:
    ldx #$03        ; set .X to screen
    cpx DFLTO       ; compare output device number with screen
    bcs LAB_F3FC    ; branch if >= screen

                    ; else was serial bus
    jsr FUNLSN      ; command the serial bus to UNLISTEN
LAB_F3FC:
    cpx DFLTN       ; compare input device number with screen
    bcs LAB_F403    ; branch if >= screen

                    ; else was serial bus
    jsr FUNTLK      ; command the serial bus to UNTALK
LAB_F403:
    stx DFLTO       ; set output device number to screen
    lda #$00        ; set for keyboard
    sta DFLTN       ; set input device number to keyboard
    rts


;***********************************************************************************;
;
; open a logical file

; This routine is used to open a logical file. Once the logical file is set up it
; can be used for input/output operations. Most of the I/O KERNAL routines call on
; this routine to create the logical files to operate on. No arguments need to be
; set up to use this routine, but both the SETLFS and SETNAM KERNAL routines must
; be called before using this routine.

FOPEN:
    ldx LA          ; get logical file number
    bne LAB_F411    ; branch if there is a file

    jmp FE_NTINP    ; else do not input file error and return

LAB_F411:
    jsr FNDFLNO     ; find file
    bne LAB_F419    ; branch if file not found

    jmp FE_ALOPN    ; else do file already open error and return

LAB_F419:
    ldx LDTND       ; get open file count
    cpx #$0A        ; compare with max
    bcc LAB_F422    ; branch if less

    jmp FE_2MNYF    ; else do too many files error and return

LAB_F422:
    inc LDTND       ; increment open file count
    lda LA          ; get logical file number
    sta LAT,X       ; save to logical file table
    lda SA          ; get secondary address
    ora #$60        ; OR with the OPEN CHANNEL command
    sta SA          ; set secondary address
    sta SAT,X       ; save to secondary address table
    lda FA          ; get device number
    sta FAT,X       ; save to device number table
    beq LAB_F493    ; do ok exit if keyboard

    cmp #$03        ; compare device number with screen
    beq LAB_F493    ; do ok exit if screen

    bcc LAB_F444    ; branch if < screen, tape or RS-232

                    ; else is serial bus device
    jsr SERNAME     ; send secondary address and filename
    bcc LAB_F493    ; do ok exit

LAB_F444:
    cmp #$02        ; compare device with RS-232 device
    bne LAB_F44B    ; branch if not RS-232 device, must be tape

    jmp OPENRS      ; go open RS-232 device and return

LAB_F44B:
    jsr TPBUFA      ; get tape buffer start pointer in .X.Y
    bcs LAB_F453    ; branch if >= $0200

    jmp FE_ILDEV    ; do illegal device number and return

LAB_F453:
    lda SA          ; get secondary address
    and #$0F        ; mask the OPEN CHANNEL command
    bne LAB_F478    ; branch if write

    jsr CSTEL       ; wait for plaY
    bcs LAB_F494    ; exit if STOP was pressed

    jsr SRCHING     ; print "Searching..."
    lda FNLEN       ; get file name length
    beq LAB_F46F    ; if null file name just go find header

    jsr FNDHDR      ; find specific tape header
    bcc LAB_F482    ; branch if no error

    beq LAB_F494    ; branch always

LAB_F46C:
    jmp FE_NTFND    ; do file not found error and return

LAB_F46F:
    jsr FAH         ; find tape header, exit with header in buffer
    beq LAB_F494    ; exit if end of tape found

    bcc LAB_F482    ; branch if no error

    bcs LAB_F46C    ; branch if error

LAB_F478:
    jsr CSTE2       ; wait for plaY/RECORD
    bcs LAB_F494    ; exit if STOP was pressed

    lda #$04        ; set data file header
    jsr TAPEH       ; write tape header
LAB_F482:
    lda #$BF        ; set tape buffer length
    ldy SA          ; get secondary address
    cpy #$60        ; compare with read
    beq LAB_F491    ; branch if read

    ldy #$00        ; clear index
    lda #$02        ; set data file block file type
    sta (TAPE1),Y   ; save file type to tape buffer
    tya             ; clear .A
LAB_F491:
    sta BUFPNT      ; save tape buffer index
LAB_F493:
    clc             ; flag ok
LAB_F494:
    rts


;***********************************************************************************;
;
; send secondary address and filename

SERNAME:
    lda SA          ; get secondary address
    bmi LAB_F4C5    ; ok exit if -ve

    ldy FNLEN       ; get file name length
    beq LAB_F4C5    ; ok exit if null

    lda FA          ; get device number
    jsr FLISTEN     ; command devices on the serial bus to LISTEN
    lda SA          ; get the secondary address
    ora #$F0        ; OR with the OPEN command
    jsr FSECOND     ; send secondary address after LISTEN
    lda STATUS      ; get serial status byte
    bpl LAB_F4B2    ; branch if device present

    pla             ; else dump calling address low byte
    pla             ; dump calling address high byte
    jmp FE_DVNTP    ; do device not present error and return

LAB_F4B2:
    lda FNLEN       ; get file name length
    beq LAB_F4C2    ; branch if null name

    ldy #$00        ; clear index
LAB_F4B8:
    lda (FNADR),Y   ; get file name byte
    jsr FCIOUT      ; output a byte to the serial bus
    INY             ; increment index
    cpy FNLEN       ; compare with file name length
    bne LAB_F4B8    ; loop if not all done

LAB_F4C2:
    jsr FUNLSN      ; command the serial bus to UNLISTEN
LAB_F4C5:
    clc             ; flag ok
    rts


;***********************************************************************************;
;
; open RS-232

OPENRS:
    lda #$06        ; IIII IOOI, DTR and rts only as outputs
    sta VIA1DDRB    ; set VIA 1 DDRB
    sta VIA1PB      ; set VIA 1 DRB, DTR and rts high
    lda #$EE        ; CB2 high, CB1 -ve edge, CA2 high, CA1 -ve edge
    sta VIA1PCR     ; set VIA 1 PCR
    ldy #$00        ; clear index
    sty RSSTAT      ; clear RS-232 status byte
LAB_F4D9:
    cpy FNLEN       ; compare with file name length
    beq LAB_F4E7    ; exit loop if done

    lda (FNADR),Y   ; get file name byte
    sta M51CTR,Y    ; copy to 6551 register set
    INY             ; increment index
    cpy #$04        ; compare with $04
    bne LAB_F4D9    ; loop if not to 4 yet

LAB_F4E7:
    jsr RSCPTBIT    ; compute bit count
    stx BITNUM      ; save bit count
    lda M51CTR      ; get pseudo 6551 control register
    and #$0F        ; mask 0000 xxxx, baud rate
    bne LAB_F4F4    ; branch nowhere. perhaps there was going to be some
                    ; error trapping for unimplemented baud rates but
                    ; this was ever done
LAB_F4F4:
    asl             ; * 2
    tax             ; copy to index
    lda BAUDTBL-2,X ; get timer constant low byte
    asl             ; * 2
    tay             ; copy to .Y
    lda BAUDTBL-1,X ; get timer constant high byte
    ROL             ; * 2
    pha             ; save it
    tya             ; get timer constant low byte back
    adc #$C8        ; + $C8, carry cleared by previous ROL
    sta BAUDOF      ; save bit cell time low byte
    pla             ; restore high byte
    adc #$00        ; add carry
    sta BAUDOF+1    ; save bit cell time high byte
    lda M51CDR      ; get pseudo 6551 command register
    lsr             ; shift b0 into Cb
    bcc LAB_F51B    ; branch if 3 line interface

    lda VIA2PB      ; get VIA 2 DRB, this is wrong, the address should be
                    ; VIA1PB which is VIA 1 which is where the DSR input
                    ; really is
    asl             ; shift DSR into Cb
    bcs LAB_F51B    ; branch if DSR = 1

    jmp RSMISSNG    ; set DSR signal not present and return

LAB_F51B:
    lda RIDBE       ; get index to Rx buffer end
    sta RIDBS       ; set index to Rx buffer start, clear Rx buffer
    lda RODBE       ; get index to Tx buffer end
    sta RODBS       ; set index to Tx buffer start, clear Tx buffer
    jsr LAB_FE75    ; read the top of memory
    lda RIBUF+1     ; get Rx buffer pointer high byte
    bne LAB_F533    ; branch if buffer already set

    dey             ; decrement top of memory high byte, 256 byte buffer
    sty RIBUF+1     ; set Rx buffer pointer high byte
    stx RIBUF       ; set Rx buffer pointer low byte
LAB_F533:
    lda ROBUF+1     ; get Tx buffer pointer high byte
    bne LAB_F53C    ; branch if buffer already set

    dey             ; decrement Rx buffer pointer high byte, 256 byte buffer
    sty ROBUF+1     ; set Tx buffer pointer high byte
    stx ROBUF       ; set Tx buffer pointer low byte
LAB_F53C:
    SEC
    lda #$F0
    jmp LAB_FE7B    ; set the top of memory and return


;***********************************************************************************;
;
; load RAM from a device

; This routine will load data bytes from any input device directly into the memory
; of the computer. It can also be used for a verify operation comparing data from a
; device with the data already in memory, leaving the data stored in RAM unchanged.

; The accumulator must be set to 0 for a load operation or 1 for a verify. If the
; input device was OPENed with a secondary address of 0 the header information from
; device will be ignored. In this case .X.Y must contain the starting address for the
; load. If the device was addressed with a secondary address of 1 or 2 the data will
; load into memory starting at the location specified by the header. This routine
; returns the address of the highest RAM location which was loaded.

; Before this routine can be called, the SETLFS and SETNAM routines must be called.

FLOAD:
    stx MEMUSS      ; Save desired start address.
    sty MEMUSS+1    ; Ignored if secondary address is not 0.
    jmp (ILOAD)     ; do LOAD vector, usually points to FLOAD2


;***********************************************************************************;
;
; load

FLOAD2:
    sta VERCK       ; save load/verify flag
    lda #$00        ; clear .A
    sta STATUS      ; clear serial status byte
    lda FA          ; get device number
    bne LAB_F556    ; branch if not keyboard

                    ; can't load from keyboard so ..
LAB_F553:
    jmp FE_ILDEV    ; do illegal device number and return

LAB_F556:
    cmp #$03        ; compare device number with screen
    beq LAB_F553    ; if screen go do illegal device number and return

    bcc LAB_F5CA    ; branch if less than screen

                    ; else is serial bus device
    ldy FNLEN       ; get file name length
    bne LAB_F563    ; branch if not null name

    jmp FE_MISFN    ; else do missing file name error and return

LAB_F563:
    jsr PATCH1      ; get secondary address and print "Searching..."
    lda #$60
    sta SA          ; save the secondary address
    jsr SERNAME     ; send secondary address and filename
    lda FA          ; get device number
    jsr FTALK       ; command a serial bus device to TALK
    lda SA          ; get secondary address
    jsr FTKSA       ; send secondary address after TALK
    jsr FACPTR      ; input a byte from the serial bus
    sta EAL         ; save program start address low byte
    lda STATUS      ; get serial status byte
    lsr             ; shift time out read ..
    lsr             ; .. into carry bit
    bcs LAB_F5C7    ; if timed out go do file not found error and return

    jsr FACPTR      ; input a byte from the serial bus
    sta EAL+1       ; save program start address high byte
    jsr PATCH2      ; Override LOAD address in EAL if secondary address is 0.
LAB_F58A:
    lda #$FD        ; mask xxxx xx0x, clear time out read bit
    and STATUS      ; mask serial status byte
    sta STATUS      ; set serial status byte
    jsr STOP        ; scan stop key, return Zb = 1 = [STOP]
    bne LAB_F598    ; branch if not [STOP]

    jmp LAB_F6CB    ; else close the serial bus device and flag stop

LAB_F598:
    jsr FACPTR      ; input a byte from the serial bus
    tax             ; copy byte
    lda STATUS      ; get serial status byte
    lsr             ; shift time out read ..
    lsr             ; .. into carry bit
    bcs LAB_F58A    ; if timed out go ??

    txa             ; copy received byte back
    ldy VERCK       ; get load/verify flag
    beq LAB_F5B3    ; branch if load

                    ; else is verify
    ldy #$00        ; clear index
    cmp (EAL),Y     ; compare byte with previously loaded byte
    beq LAB_F5B5    ; branch if match

    lda #$10        ; flag read error
    jsr ORIOST      ; OR into serial status byte
    .byte   $2C     ; makes next line bit $AE91
LAB_F5B3:
    sta (EAL),Y     ; save byte to memory
LAB_F5B5:
    inc EAL         ; increment save pointer low byte
    bne LAB_F5BB    ; if no rollover skip the high byte increment

    inc EAL+1       ; else increment save pointer high byte
LAB_F5BB:
    bit STATUS      ; test serial status byte
    BVC LAB_F58A    ; loop if not end of file

    jsr FUNTLK      ; command the serial bus to UNTALK
    jsr LAB_F6DA    ; close serial bus device
    bcc LAB_F641    ; if no error go flag ok and exit

LAB_F5C7:
    jmp FE_NTFND    ; do file not found error and return

LAB_F5CA:
    cmp #$02        ; compare device with RS-232 device
    bne LOADTP      ; if not RS-232 device continue

    jmp RSDVCERR    ; else do illegal device number and return

LOADTP:
    jsr TPBUFA      ; get tape buffer start pointer in .X.Y
    bcs LAB_F5D9    ; branch if >= $0200

    jmp FE_ILDEV    ; do illegal device number and return

LAB_F5D9:
    jsr CSTEL       ; wait for plaY
    bcs LAB_F646    ; exit if STOP was pressed

    jsr SRCHING     ; print "Searching..."
LAB_F5E1:
    lda FNLEN       ; get file name length
    beq LAB_F5EE
    jsr FNDHDR      ; find specific tape header
    bcc LAB_F5F5    ; if no error continue

    beq LAB_F646    ; exit if end of tape found

    bcs LAB_F5C7    ; exit on error

LAB_F5EE:
    jsr FAH         ; find tape header, exit with header in buffer
    beq LAB_F646    ; exit if end of tape found

    bcs LAB_F5C7    ; exit on error

LAB_F5F5:
    lda STATUS      ; get serial status byte
    and #$10        ; mask 000x 0000, read error
    sec             ; flag fail
    bne LAB_F646    ; if read error just exit

    cpx #$01        ; compare file type with relocatable program
    beq LAB_F611    ; branch if relocatable program

    cpx #$03        ; compare file type with non relocatable program
    bne LAB_F5E1    ; branch if non relocatable program

LAB_F604:
    ldy #$01        ; index to start address
    lda (TAPE1),Y   ; get start address low byte
    sta MEMUSS      ; save start address low byte
    INY             ; increment index
    lda (TAPE1),Y   ; get start address high byte
    sta MEMUSS+1    ; set start address high byte
    bcs LAB_F615    ; branch always

LAB_F611:
    lda SA          ; get secondary address
    bne LAB_F604    ; branch if non relocatable program

LAB_F615:
    ldy #$03        ; index to end address low byte
    lda (TAPE1),Y   ; get end address low byte
    ldy #$01        ; index to start address low byte
    sbc (TAPE1),Y   ; subtract start address low byte
    tax             ; copy file length low byte
    ldy #$04        ; index to end address high byte
    lda (TAPE1),Y   ; get end address high byte
    ldy #$02        ; index to start address high byte
    sbc (TAPE1),Y   ; subtract start address high byte
    tay             ; copy file length high byte
    clc             ; clear carry for add
    txa             ; get file length low byte back
    adc MEMUSS      ; add KERNAL setup pointer low byte
    sta EAL         ; save LOAD end pointer low byte
    tya             ; get file length high byte back
    adc MEMUSS+1    ; add KERNAL setup pointer high byte
    sta EAL+1       ; save LOAD end pointer high byte
    lda MEMUSS      ; get KERNAL setup pointer low byte
    sta STAL        ; save I/O start addresses low byte
    lda MEMUSS+1    ; get KERNAL setup pointer high byte
    sta STAL+1      ; save I/O start addresses high byte
    jsr LDVRMSG     ; display "LOADING" or "VERIFYING"
    jsr RBLK        ; do the tape read
    .byte   $24     ; makes next line bit $18, keep the error flag in Cb
LAB_F641:
    clc             ; flag ok
    ldx EAL         ; get the LOAD end pointer low byte
    ldy EAL+1       ; get the LOAD end pointer high byte
LAB_F646:
    rts


;***********************************************************************************;
;
; print "searching"

SRCHING:
    lda MSGFLG      ; get message mode flag
    bpl LAB_F669    ; exit if control messages off

    ldy #KM_SRCHG-KMSGTBL
                    ; index to "SEARCHING "
    jsr KMSGSHOW    ; display KERNAL I/O message
    lda FNLEN       ; get file name length
    beq LAB_F669    ; exit if null name

    ldy #KM_FOR-KMSGTBL
                    ; else index to "FOR "
    jsr KMSGSHOW    ; display KERNAL I/O message

; print file name

FILENAME:
    ldy FNLEN       ; get file name length
    beq LAB_F669    ; exit if null file name

    ldy #$00        ; clear index
LAB_F65F:
    lda (FNADR),Y   ; get file name byte
    jsr CHROUT      ; output character to channel
    INY             ; increment index
    cpy FNLEN       ; compare with file name length
    bne LAB_F65F    ; loop if more to do

LAB_F669:
    rts

; display "LOADING" or "VERIFYING"

LDVRMSG:
    ldy #KM_LODNG-KMSGTBL
                    ; point to "LOADING"
    lda VERCK       ; get load/verify flag
    beq LAB_F672    ; branch if load

    ldy #KM_VFYNG-KMSGTBL
                    ; point to "VERIFYING"
LAB_F672:
    jmp SPMSG       ; display KERNAL I/O message if in direct mode and return


;***********************************************************************************;
;
; save RAM to device, .A = index to start address, .X.Y = end address low/high

; This routine saves a section of memory. Memory is saved from an indirect address
; on page 0 specified by A, to the address stored in .X.Y, to a logical file. The
; SETLFS and SETNAM routines must be used before calling this routine. However, a
; file name is not required to SAVE to device 1, the cassette. Any attempt to save to
; other devices without using a file name results in an error.

; NOTE: device 0, the keyboard, and device 3, the screen, cannot be SAVEd to. If
; the attempt is made, an error will occur, and the SAVE stopped.

FSAVE:
    stx EAL         ; save end address low byte
    sty EAL+1       ; save end address high byte
    tax             ; copy index to start pointer
    lda $00,X       ; get start address low byte
    sta STAL        ; set I/O start addresses low byte
    lda $01,X       ; get start address high byte
    sta STAL+1      ; set I/O start addresses high byte
    jmp (ISAVE)     ; go save, usually points to FSAVE2


;***********************************************************************************;
;
; save

FSAVE2:
    lda FA          ; get device number
    bne LAB_F68C    ; branch if not keyboard

                    ; else ..
LAB_F689:
    jmp FE_ILDEV    ; do illegal device number and return

LAB_F68C:
    cmp #$03        ; compare device number with screen
    beq LAB_F689    ; if screen do illegal device number and return

    bcc SAVETP      ; branch if < screen

                    ; is greater than screen so is serial bus
    lda #$61        ; set secondary address to $01
                    ; when a secondary address is to be sent to a device on
                    ; the serial bus the address must first be ORed with $60
    sta SA          ; save secondary address
    ldy FNLEN       ; get file name length
    bne LAB_F69D    ; branch if filename not null

    jmp FE_MISFN    ; else do missing file name error and return

LAB_F69D:
    jsr SERNAME     ; send secondary address and filename
    jsr SAVING      ; print saving [file name]
    lda FA          ; get device number
    jsr FLISTEN     ; command devices on the serial bus to LISTEN
    lda SA          ; get secondary address
    jsr FSECOND     ; send secondary address after LISTEN
    ldy #$00        ; clear index
    jsr RD300       ; copy I/O start address to buffer address
    lda SAL         ; get buffer address low byte
    jsr FCIOUT      ; output a byte to the serial bus
    lda SAL+1       ; get buffer address high byte
    jsr FCIOUT      ; output a byte to the serial bus
LAB_F6BC:
    jsr VPRTY       ; check read/write pointer, return Cb = 1 if pointer >= end
    bcs LAB_F6D7    ; go do UNLISTEN if at end

    lda (SAL),Y     ; get byte from buffer
    jsr FCIOUT      ; output a byte to the serial bus
    jsr STOP        ; scan stop key
    bne LAB_F6D2    ; if stop not pressed go increment pointer and loop for next

                    ; else ..

; close the serial bus device and flag stop

LAB_F6CB:
    jsr LAB_F6DA    ; close serial bus device
    lda #$00
    sec             ; flag stop
    rts

LAB_F6D2:
    jsr WRT62       ; increment read/write pointer
    bne LAB_F6BC    ; loop, branch always


;***********************************************************************************;
;
; ??

LAB_F6D7:
    jsr FUNLSN      ; command the serial bus to UNLISTEN

; close the serial bus device

LAB_F6DA:
    bit SA          ; test the secondary address
    bmi LAB_F6EF    ; if already closed just exit

    lda FA          ; get the device number
    jsr FLISTEN     ; command devices on the serial bus to LISTEN
    lda SA          ; get secondary address
    and #$EF        ; mask the channel number
    ora #$E0        ; OR with the CLOSE command
    jsr FSECOND     ; send secondary address after LISTEN
    jsr FUNLSN      ; command the serial bus to UNLISTEN
LAB_F6EF:
    clc             ; flag ok
    rts

SAVETP:
    cmp #$02        ; compare device with RS-232 device
    bne LAB_F6F8    ; branch if not RS-232 device

    jmp RSDVCERR    ; else do illegal device number and return

LAB_F6F8:
    jsr TPBUFA      ; get tape buffer start pointer in .X.Y
    bcc LAB_F689    ; if < $0200 do illegal device number and return

    jsr CSTE2       ; wait for plaY/RECORD
    bcs LAB_F727    ; exit if STOP was pressed

    jsr SAVING      ; print saving [file name]
    ldx #$03        ; set header for a non relocatable program file
    lda SA          ; get secondary address
    and #$01        ; mask non relocatable bit
    bne LAB_F70F    ; branch if non relocatable program

    ldx #$01        ; else set header for a relocatable program file
LAB_F70F:
    txa             ; copy header type to .A
    jsr TAPEH       ; write tape header
    bcs LAB_F727    ; exit if error

    jsr LAB_F8E6    ; do tape write, 20 cycle count
    bcs LAB_F727    ; exit if error

    lda SA          ; get secondary address
    and #$02        ; mask end of tape flag
    beq LAB_F726    ; branch if not end of tape

    lda #$05        ; else set logical end of the tape
    jsr TAPEH       ; write tape header
    .byte   $24     ; makes next line bit $18 so Cb is not changed
LAB_F726:
    clc             ; flag ok
LAB_F727:
    rts


;***********************************************************************************;
;
; print saving [file name]

SAVING:
    lda MSGFLG      ; get message mode flag
    bpl LAB_F727    ; exit if control messages off

    ldy #KM_SAVNG-KMSGTBL
                    ; index to "SAVING "
    jsr KMSGSHOW    ; display KERNAL I/O message
    jmp FILENAME    ; print file name and return


;***********************************************************************************;
;
; increment real time clock

; This routine updates the system clock. Normally this routine is called by the
; normal KERNAL interrupt routine every 1/60th of a second. If the user program
; processes its own interrupts this routine must be called to update the time. Also,
; the STOP key routine must be called if the stop key is to remain functional.

FUDTIM:
    ldx #$00        ; clear .X
    inc TIME+2      ; increment jiffy low byte
    bne LAB_F740    ; if no rollover skip the mid byte increment

    inc TIME+1      ; increment jiffy mid byte
    bne LAB_F740    ; if no rollover skip the high byte increment

    inc TIME        ; increment jiffy high byte

                    ; now subtract a days worth of jiffies from current count
                    ; and remember only the Cb result
LAB_F740:
    sec             ; set carry for subtract
    lda TIME+2      ; get jiffy clock low byte
    sbc #$01        ; subtract $4F1A01 low byte
    lda TIME+1      ; get jiffy clock mid byte
    sbc #$1A        ; subtract $4F1A01 mid byte
    lda TIME        ; get jiffy clock high byte
    sbc #$4F        ; subtract $4F1A01 high byte
    bcc LAB_F755    ; branch if less than $4F1A01 jiffies

                    ; else ..
    stx TIME        ; clear jiffies high byte
    stx TIME+1      ; clear jiffies mid byte
    stx TIME+2      ; clear jiffies low byte
                    ; this is wrong, there are $4F1A00 jiffies in a day so
                    ; the reset to zero should occur when the value reaches
                    ; $4F1A00 and not $4F1A01. this would give an extra jiffy
                    ; every day and a possible TI value of 24:00:00
LAB_F755:
    lda VIA2PA2     ; get VIA 2 DRA, keyboard row, no handshake
    cmp VIA2PA2     ; compare with self
    bne LAB_F755    ; loop if changing

    sta STKEY       ; save VIA 2 DRA, keyboard row
    rts


;***********************************************************************************;
;
; read the real time clock

; This routine returns the time, in jiffies, in .A.X.Y. The accumulator contains the
; most significant byte.

FRDTIM:
    sei             ; disable interrupts
    lda TIME+2      ; get jiffy clock low byte
    ldx TIME+1      ; get jiffy clock mid byte
    ldy TIME        ; get jiffy clock high byte


;***********************************************************************************;
;
; set the real time clock

; The system clock is maintained by an interrupt routine that updates the clock
; every 1/60th of a second. The clock is three bytes long which gives the capability
; to count from zero up to 5,184,000 jiffies - 24 hours plus one jiffy. At that point
; the clock resets to zero. Before calling this routine to set the clock the new time,
; in jiffies, should be in .Y.X.A, the accumulator containing the most significant byte.

FSETTIM:
    sei             ; disable interrupts
    sta TIME+2      ; save jiffy clock low byte
    stx TIME+1      ; save jiffy clock mid byte
    sty TIME        ; save jiffy clock high byte
    cli             ; enable interrupts
    rts


;***********************************************************************************;
;
; scan stop key, return Zb = 1 = [STOP]

; If the STOP key on the keyboard is pressed when this routine is called the Z flag
; will be set. All other flags remain unchanged. If the STOP key is not pressed then
; the accumulator will contain a byte representing the last row of the keyboard scan.

; The user can also check for certain other keys this way.

FSTOP:
    lda STKEY       ; get keyboard row
    cmp #$FE        ; compare with r0 down
    bne LAB_F77D    ; branch if not just r0

    php             ; save status
    jsr CLRCHN      ; close input and output channels
    sta NDX         ; save keyboard buffer length
    plp             ; restore status
LAB_F77D:
    rts


;***********************************************************************************;
;
; file error messages

FE_2MNYF:
    lda #$01        ; too many files
    .byte   $2C     ; makes next line bit $02A9
FE_ALOPN:
    lda #$02        ; file already open
    .byte   $2C     ; makes next line bit $03A9
FE_NTOPN:
    lda #$03        ; file not open
    .byte   $2C     ; makes next line bit $04A9
FE_NTFND:
    lda #$04        ; file not found
    .byte   $2C     ; makes next line bit $05A9
FE_DVNTP:
    lda #$05        ; device not present
    .byte   $2C     ; makes next line bit $06A9
FE_NTINP:
    lda #$06        ; not input file
    .byte   $2C     ; makes next line bit $07A9
FE_NTOUT:
    lda #$07        ; not output file
    .byte   $2C     ; makes next line bit $08A9
FE_MISFN:
    lda #$08        ; missing file name
    .byte   $2C     ; makes next line bit $09A9
FE_ILDEV:
    lda #$09        ; illegal device number

    pha             ; save error #
    jsr CLRCHN      ; close input and output channels
    ldy #KM_IOERR-KMSGTBL
                    ; index to "I/O ERROR #"
    bit MSGFLG      ; test message mode flag
    BVC LAB_F7AC    ; exit if KERNAL messages off

    jsr KMSGSHOW    ; display KERNAL I/O message
    pla             ; restore error #
    pha             ; copy error #
    ora #'0'        ; convert to ASCII
    jsr CHROUT      ; output character to channel
LAB_F7AC:
    pla             ; pull error number
    sec             ; flag error
    rts


;***********************************************************************************;
;
; find tape header, exit with header in buffer

FAH:
    lda VERCK       ; get load/verify flag
    pha             ; save load/verify flag
    jsr RDTPBLKS    ; initiate tape read
    pla             ; restore load/verify flag
    sta VERCK       ; save load/verify flag
    bcs LAB_F7E6    ; exit if error

    ldy #$00        ; clear index
    lda (TAPE1),Y   ; read first byte from tape buffer
    cmp #$05        ; compare with logical end of the tape
    beq LAB_F7E6    ; exit if end of the tape

    cmp #$01        ; compare with header for a relocatable program file
    beq LAB_F7CE    ; branch if program file header

    cmp #$03        ; compare with header for a non relocatable program file
    beq LAB_F7CE    ; branch if program file header

    cmp #$04        ; compare with data file header
    bne FAH         ; if data file loop to find tape header

                    ; was program file header
LAB_F7CE:
    tax             ; copy header type
    bit MSGFLG      ; get message mode flag
    bpl LAB_F7E4    ; exit if control messages off

    ldy #KM_FOUND-KMSGTBL
                    ; index to "FOUND "
    jsr KMSGSHOW    ; display KERNAL I/O message
    ldy #$05        ; index to tape filename
LAB_F7DA:
    lda (TAPE1),Y   ; get byte from tape buffer
    jsr CHROUT      ; output character to channel
    INY             ; increment index
    cpy #$15        ; compare with end+1
    bne LAB_F7DA    ; loop if more to do

LAB_F7E4:
    clc             ; flag no error
    dey             ; decrement index
LAB_F7E6:
    rts


;***********************************************************************************;
;
; write tape header

TAPEH:
    sta PTR1        ; save header type
    jsr TPBUFA      ; get tape buffer start pointer in .X.Y
    bcc LAB_F84C    ; exit if < $0200

    lda STAL+1      ; get I/O start address high byte
    pha             ; save it
    lda STAL        ; get I/O start address low byte
    pha             ; save it
    lda EAL+1       ; get tape end address high byte
    pha             ; save it
    lda EAL         ; get tape end address low byte
    pha             ; save it

    ldy #$BF        ; index to header end
    lda #' '        ; clear byte, [SPACE]
LAB_F7FE:
    sta (TAPE1),Y   ; clear header byte
    dey             ; decrement index
    bne LAB_F7FE    ; loop if more to do

    lda PTR1        ; get header type back
    sta (TAPE1),Y   ; write to header
    INY             ; increment index
    lda STAL        ; get I/O start address low byte
    sta (TAPE1),Y   ; write to header
    INY             ; increment index
    lda STAL+1      ; get I/O start address high byte
    sta (TAPE1),Y   ; write to header
    INY             ; increment index
    lda EAL         ; get tape end address low byte
    sta (TAPE1),Y   ; write to header
    INY             ; increment index
    lda EAL+1       ; get tape end address high byte
    sta (TAPE1),Y   ; write to header
    INY             ; increment index
    sty PTR2        ; save index
    ldy #$00        ; clear .Y
    sty PTR1        ; clear name index
LAB_F822:
    ldy PTR1        ; get name index
    cpy FNLEN       ; compare with file name length
    beq LAB_F834    ; exit loop if all done

    lda (FNADR),Y   ; get file name byte
    ldy PTR2        ; get buffer index
    sta (TAPE1),Y   ; save file name byte to buffer
    inc PTR1        ; increment file name index
    inc PTR2        ; increment tape buffer index
    bne LAB_F822    ; loop, branch always

LAB_F834:
    jsr LDAD1       ; set tape buffer start and end pointers
    lda #$69        ; set write lead cycle count
    sta RIPRTY      ; save write lead cycle count
    jsr LAB_F8EA    ; do tape write, no cycle count set
    tay
    pla             ; pull tape end address low byte
    sta EAL         ; restore it
    pla             ; pull tape end address high byte
    sta EAL+1       ; restore it
    pla             ; pull I/O start addresses low byte
    sta STAL        ; restore it
    pla             ; pull I/O start addresses high byte
    sta STAL+1      ; restore it
    tya
LAB_F84C:
    rts


;***********************************************************************************;
;
; get tape buffer start pointer

TPBUFA:
    ldx TAPE1       ; get tape buffer start pointer low byte
    ldy TAPE1+1     ; get tape buffer start pointer high byte
    cpy #$02        ; compare high byte with $02xx
    rts


;***********************************************************************************;
;
; set tape buffer start and end pointers

LDAD1:
    jsr TPBUFA      ; get tape buffer start pointer in .X.Y
    txa             ; copy tape buffer start pointer low byte
    sta STAL        ; save as I/O address pointer low byte
    clc             ; clear carry for add
    adc #$C0        ; add buffer length low byte
    sta EAL         ; save tape buffer end pointer low byte
    tya             ; copy tape buffer start pointer high byte
    sta STAL+1      ; save as I/O address pointer high byte
    adc #$00        ; add buffer length high byte
    sta EAL+1       ; save tape buffer end pointer high byte
    rts


;***********************************************************************************;
;
; find specific tape header

FNDHDR:
    jsr FAH         ; find tape header, exit with header in buffer
    bcs LAB_F889    ; just exit if error

    ldy #$05        ; index to name
    sty PTR2        ; save as tape buffer index
    ldy #$00        ; clear .Y
    sty PTR1        ; save as name buffer index
LAB_F874:
    cpy FNLEN       ; compare with file name length
    beq LAB_F888    ; ok exit if match

    lda (FNADR),Y   ; get file name byte
    ldy PTR2        ; get index to tape buffer
    cmp (TAPE1),Y   ; compare with tape header name byte
    bne FNDHDR      ; if no match go get next header

    inc PTR1        ; else increment name buffer index
    inc PTR2        ; increment tape buffer index
    ldy PTR1        ; get name buffer index
    bne LAB_F874    ; loop, branch always

LAB_F888:
    clc             ; flag ok
LAB_F889:
    rts


;***********************************************************************************;
;
; bump tape pointer

JTP20:
    jsr TPBUFA      ; get tape buffer start pointer in .X.Y
    inc BUFPNT      ; increment tape buffer index
    ldy BUFPNT      ; get tape buffer index
    cpy #$C0        ; compare with buffer length
    rts


;***********************************************************************************;
;
; wait for plaY

CSTEL:
    jsr CS10        ; return cassette sense in Zb
    beq LAB_F8B5    ; exit if switch closed

                    ; cassette switch was open
    ldy #KM_PRPLY-KMSGTBL
                    ; index to "PRESS plaY ON TAPE"
LAB_F89B:
    jsr KMSGSHOW    ; display KERNAL I/O message
LAB_F89E:
    jsr TSTOP       ; scan stop key and flag abort if pressed
                    ; note if STOP was pressed the return is to the
                    ; routine that called this one and not here
    jsr CS10        ; return cassette sense in Zb
    bne LAB_F89E    ; loop if cassette switch open

    ldy #KM_OK-KMSGTBL
                    ; index to "OK"
    jmp KMSGSHOW    ; display KERNAL I/O message and return


;***********************************************************************************;
;
; return cassette sense in Zb

CS10:
    lda #$40        ; mask for cassette switch
    bit VIA1PA2     ; test VIA 1 DRA, no handshake
    bne LAB_F8B5    ; branch if cassette sense high

    bit VIA1PA2     ; test VIA 1 DRA again
LAB_F8B5:
    clc
    rts


;***********************************************************************************;
;
; wait for plaY/RECORD

CSTE2:
    jsr CS10        ; return cassette sense in Zb
    beq LAB_F8B5    ; exit if switch closed

                    ; cassette switch was open
    ldy #KM_REcpy-KMSGTBL
                    ; index to "PRESS RECORD & plaY ON TAPE"
    bne LAB_F89B    ; display message and wait for switch, branch always


;***********************************************************************************;
;
; initiate tape read

RDTPBLKS:
    lda #$00        ; clear .A
    sta STATUS      ; clear serial status byte
    sta VERCK       ; clear the load/verify flag
    jsr LDAD1       ; set tape buffer start and end pointers
RBLK:
    jsr CSTEL       ; wait for plaY
    bcs LAB_F8ED    ; exit if STOP was pressed, uses further bcs at target
                    ; address to reach final target at LAB_F957

    sei             ; disable interrupts
    lda #$00        ; clear .A
    sta RIDATA
    sta BITTS
    sta CMP0        ; clear tape timing constant min byte
    sta PTR1        ; clear tape pass 1 error log/char buffer
    sta PTR2        ; clear tape pass 2 error log corrected
    sta DPSW        ; clear byte received flag
    lda #$82        ; enable CA1 interrupt
    ldx #$0E        ; set index for tape read vector
    bne TAPE        ; go do tape read/write, branch always


;***********************************************************************************;
;
; initiate tape write

WBLK:
    jsr LDAD1       ; set tape buffer start and end pointers

; do tape write, 20 cycle count

LAB_F8E6:
    lda #$14        ; set write lead cycle count
    sta RIPRTY      ; save write lead cycle count

; do tape write, no cycle count set

LAB_F8EA:
    jsr CSTE2       ; wait for plaY/RECORD
LAB_F8ED:
    bcs LAB_F957    ; if STOPped clear save IRQ address and exit

    sei             ; disable interrupts
    lda #$A0        ; enable VIA 2 T2 interrupt
    ldx #$08        ; set index for tape write tape leader vector


;***********************************************************************************;
;
; tape read/write

TAPE:
    ldy #$7F        ; disable all interrupts
    sty VIA2IER     ; set VIA 2 IER, disable interrupts
    sta VIA2IER     ; set VIA 2 IER, enable interrupts according to .A
    jsr RSPAUSE     ; check RS-232 bus idle
    lda CINV        ; get IRQ vector low byte
    sta IRQTMP      ; save IRQ vector low byte
    lda CINV+1      ; get IRQ vector high byte
    sta IRQTMP+1    ; save IRQ vector high byte
    jsr LAB_FCFB    ; set tape vector
    lda #$02        ; set copies count. the first copy is the load copy, the
                    ; second copy is the verify copy
    sta FSBLK       ; save copies count
    jsr NEWCH       ; new tape byte setup
    lda VIA1PCR     ; get VIA 1 PCR
    and #$FD        ; CA2 low, turn on tape motor
    ora #$0C        ; manual output mode
    sta VIA1PCR     ; set VIA 1 PCR
    sta CAS1        ; set tape motor interlock

                    ; 326656 cycle delay, allow tape motor speed to stabilise
    ldx #$FF        ; outer loop count
LAB_F923:
    ldy #$FF        ; inner loop count
LAB_F925:
    dey             ; decrement inner loop count
    bne LAB_F925    ; loop if more to do

    dex             ; decrement outer loop count
    bne LAB_F923    ; loop if more to do

    sta VIA2T2CH    ; set VIA 2 T2C_h
    cli             ; enable tape interrupts
LAB_F92F:
    lda IRQTMP+1    ; get saved IRQ high byte
    cmp CINV+1      ; compare with the current IRQ high byte
    clc             ; flag ok
    beq LAB_F957    ; if tape write done go clear saved IRQ address and exit

    jsr TSTOP       ; scan stop key and flag abort if pressed
                    ; note if STOP was pressed the return is to the
                    ; routine that called this one and not here
    lda VIA2IFR     ; get VIA 2 IFR
    and #$40        ; mask T1 interrupt
    beq LAB_F92F    ; loop if not T1 interrupt

                    ; else increment jiffy clock
    lda VIA1T1CL    ; get VIA 1 T1C_l, clear T1 flag
    jsr FUDTIM      ; increment the real time clock
    jmp LAB_F92F    ; loop


;***********************************************************************************;
;
; scan stop key and flag abort if pressed

TSTOP:
    jsr STOP        ; scan stop key
    clc             ; flag no stop
    bne LAB_F95C    ; exit if no stop

    jsr TNIF        ; restore everything for STOP
    sec             ; flag stopped
    pla             ; dump return address low byte
    pla             ; dump return address high byte


;***********************************************************************************;
;
; clear saved IRQ address

LAB_F957:
    lda #$00        ; clear .A
    sta IRQTMP+1    ; clear saved IRQ address high byte
LAB_F95C:
    rts


;***********************************************************************************;
;
;## set timing

STT1:
    stx CMP0+1      ; save tape timing constant max byte
    lda CMP0        ; get tape timing constant min byte
    asl             ; *2
    asl             ; *4
    clc             ; clear carry for add
    adc CMP0        ; add tape timing constant min byte *5
    clc             ; clear carry for add
    adc CMP0+1      ; add tape timing constant max byte
    sta CMP0+1      ; save tape timing constant max byte
    lda #$00
    bit CMP0        ; test tape timing constant min byte
    bmi LAB_F972    ; branch if b7 set

    ROL             ; else shift carry into ??
LAB_F972:
    asl CMP0+1      ; shift tape timing constant max byte
    ROL
    asl CMP0+1      ; shift tape timing constant max byte
    ROL
    tax
LAB_F979:
    lda VIA2T2CL    ; get VIA 2 T2C_l
    cmp #$15        ; compare with ??
    bcc LAB_F979    ; loop if less

    adc CMP0+1      ; add tape timing constant max byte
    sta VIA2T1CL    ; set VIA 2 T1C_l
    txa
    adc VIA2T2CH    ; add VIA 2 T2C_h
    sta VIA2T1CH    ; set VIA 2 T1C_h
    cli             ; enable interrupts
    rts


;***********************************************************************************;
;
;;  On Commodore computers, the streams consist of four kinds of symbols
;;  that denote different kinds of low-to-high-to-low transitions on the
;;  read or write signals of the Commodore cassette interface.
;;
;;  A   A break in the communications, or a pulse with very long cycle
;;      time.
;;
;;  B   A short pulse, whose cycle time typically ranges from 296 to 424
;;      microseconds, depending on the computer model.
;;
;;  C   A medium-length pulse, whose cycle time typically ranges from
;;      440 to 576 microseconds, depending on the computer model.
;;
;;  D   A long pulse, whose cycle time typically ranges from 600 to 744
;;      microseconds, depending on the computer model.
;;
;;   The actual interpretation of the serial data takes a little more work to
;; explain. The typical ROM tape loader (and the turbo loaders) will
;; initialise a timer with a specified value and start it counting down. If
;; either the tape data changes or the timer runs out, an IRQ will occur. The
;; loader will determine which condition caused the IRQ. If the tape data
;; changed before the timer ran out, we have a short pulse, or a "0" bit. If
;; the timer ran out first, we have a long pulse, or a "1" bit. Doing this
;; continuously and we decode the entire file.

; read tape bits, IRQ routine

; read T2C which has been counting down from $FFFF. subtract this from $FFFF

READT:
    ldx VIA2T2CH    ; get VIA 2 T2C_h
    ldy #$FF        ; set $FF
    tya             ; .A = $FF
    sbc VIA2T2CL    ; subtract VIA 2 T2C_l
    cpx VIA2T2CH    ; compare VIA 2 T2C_h with previous
    bne READT       ; loop if timer low byte rolled over

    stx CMP0+1      ; save tape timing constant max byte
    tax             ; copy $FF - T2C_l
    sty VIA2T2CL    ; set VIA 2 T2C_l to $FF
    sty VIA2T2CH    ; set VIA 2 T2C_h to $FF
    tya             ; $FF
    sbc CMP0+1      ; subtract tape timing constant max byte
                    ; .A = $FF - T2C_h
    stx CMP0+1      ; save tape timing constant max byte
                    ; CMP0+1 = $FF - T2C_l
    lsr             ; .A = $FF - T2C_h >> 1
    ror CMP0+1      ; shift tape timing constant max byte
                    ; CMP0+1 = $FF - T2C_l >> 1
    lsr             ; .A = $FF - T2C_h >> 1
    ror CMP0+1      ; shift tape timing constant max byte
                    ; CMP0+1 = $FF - T2C_l >> 1
    lda CMP0        ; get tape timing constant min byte
    clc             ; clear carry for add
    adc #$3C
    bit VIA2PA1     ; test VIA 2 DRA, keyboard row
    cmp CMP0+1      ; compare with tape timing constant max byte
                    ; compare with ($FFFF - T2C) >> 2
    bcs LAB_FA06    ; branch if min + $3C >= ($FFFF - T2C) >> 2

                    ; min + $3C < ($FFFF - T2C) >> 2
    ldx DPSW        ; get byte received flag
    beq LAB_F9C3    ; branch if not byte received

    jmp TPSTORE     ; store tape character

LAB_F9C3:
    ldx PCNTR       ; get EOI flag byte
    bmi LAB_F9E2

    ldx #$00
    adc #$30
    adc CMP0        ; add tape timing constant min byte
    cmp CMP0+1      ; compare with tape timing constant max byte
    bcs LAB_F9ED

    inx
    adc #$26
    adc CMP0        ; add tape timing constant min byte
    cmp CMP0+1      ; compare with tape timing constant max byte
    bcs LAB_F9F1

    adc #$2C
    adc CMP0        ; add tape timing constant min byte
    cmp CMP0+1      ; compare with tape timing constant max byte
    bcc LAB_F9E5

LAB_F9E2:
    jmp LAB_FA60

LAB_F9E5:
    lda BITTS       ; get bit count
    beq LAB_FA06    ; branch if zero

    sta BITCI       ; save receiver bit count in
    bne LAB_FA06    ; branch always

LAB_F9ED:
    inc RINONE      ; increment ?? start bit check flag
    bcs LAB_F9F3

LAB_F9F1:
    dec RINONE      ; decrement ?? start bit check flag
LAB_F9F3:
    SEC
    sbc #$13
    sbc CMP0+1      ; subtract tape timing constant max byte
    adc SVXT        ; add timing constant for tape
    sta SVXT        ; save timing constant for tape
    lda FIRT        ; get tape bit cycle phase
    eor #$01
    sta FIRT        ; save tape bit cycle phase
    beq LAB_FA25

    stx ASCII
LAB_FA06:
    lda BITTS       ; get bit count
    beq LAB_FA22    ; exit if zero

    bit VIA2IFR     ; test get 2 IFR
    BVC LAB_FA22    ; exit if no T1 interrupt

    lda #$00
    sta FIRT        ; clear tape bit cycle phase
    lda PCNTR       ; get EOI flag byte
    bpl LAB_FA47

    bmi LAB_F9E2

LAB_FA19:
    ldx #$A6        ; set timing max byte
    jsr STT1        ; set timing
    lda PRTY
    bne LAB_F9E5
LAB_FA22:
    jmp _RTI        ; restore registers and exit interrupt

LAB_FA25:
    lda SVXT        ; get timing constant for tape
    beq LAB_FA30

    bmi LAB_FA2E

    dec CMP0        ; decrement tape timing constant min byte
    .byte $2C       ; makes next line bit $B0E6
LAB_FA2E:
    inc CMP0        ; increment tape timing constant min byte
LAB_FA30:
    lda #$00
    sta SVXT        ; clear timing constant for tape
    cpx ASCII
    bne LAB_FA47

    txa
    bne LAB_F9E5

    lda RINONE      ; get start bit check flag
    bmi LAB_FA06

    cmp #$10
    bcc LAB_FA06

    sta SYNO        ; save cassette block synchronisation number
    bcs LAB_FA06

LAB_FA47:
    txa
    eor PRTY
    sta PRTY
    lda BITTS
    beq LAB_FA22

    dec PCNTR       ; decrement EOI flag byte
    bmi LAB_FA19

    lsr ASCII
    ror MYCH        ; parity count
    ldx #$DA        ; set timing max byte
    jsr STT1        ; set timing
    jmp _RTI        ; restore registers and exit interrupt

LAB_FA60:
    lda SYNO        ; get cassette block synchronisation number
    beq LAB_FA68

    lda BITTS
    beq LAB_FA6C

LAB_FA68:
    lda PCNTR       ; get EOI flag byte
    bpl LAB_F9F1

LAB_FA6C:
    lsr CMP0+1      ; shift tape timing constant max byte
    lda #$93
    SEC
    sbc CMP0+1      ; subtract tape timing constant max byte
    adc CMP0        ; add tape timing constant min byte
    asl
    tax             ; copy timing high byte
    jsr STT1        ; set timing
    inc DPSW
    lda BITTS
    bne LAB_FA91

    lda SYNO        ; get cassette block synchronisation number
    beq LAB_FAAA

    sta BITCI       ; save receiver bit count in
    lda #$00
    sta SYNO        ; clear cassette block synchronisation number
    lda #$C0        ; enable T1 interrupt
    sta VIA2IER     ; set VIA 2 IER
    sta BITTS
LAB_FA91:
    lda SYNO        ; get cassette block synchronisation number
    sta NXTBIT
    beq LAB_FAA0

    lda #$00
    sta BITTS
    lda #$40        ; disable T1 interrupt
    sta VIA2IER     ; set VIA 2 IER
LAB_FAA0:
    lda MYCH        ; parity count
    sta ROPRTY      ; save RS-232 parity byte
    lda BITCI       ; get receiver bit count in
    ora RINONE      ; OR with start bit check flag
    sta RODATA
LAB_FAAA:
    jmp _RTI        ; restore registers and exit interrupt


;***********************************************************************************;
;
;## store character

TPSTORE:
    jsr NEWCH       ; new tape byte setup
    sta DPSW        ; clear byte received flag
    ldx #$DA        ; set timing max byte
    jsr STT1        ; set timing
    lda FSBLK       ; get copies count
    beq LAB_FABD

    sta INBIT       ; save receiver input bit temporary storage
LAB_FABD:
    lda #$0F
    bit RIDATA
    bpl LAB_FADA

    lda NXTBIT
    bne LAB_FAD3

    ldx FSBLK       ; get copies count
    dex
    bne LAB_FAD7    ; if ?? restore registers and exit interrupt

    lda #$08        ; set short block
    jsr ORIOST      ; OR into serial status byte
    bne LAB_FAD7    ; restore registers and exit interrupt, branch always

LAB_FAD3:
    lda #$00
    sta RIDATA
LAB_FAD7:
    jmp _RTI        ; restore registers and exit interrupt

LAB_FADA:
    BVS LAB_FB0D

    bne LAB_FAF6

    lda NXTBIT
    bne LAB_FAD7

    lda RODATA
    bne LAB_FAD7

    lda INBIT       ; get receiver input bit temporary storage
    LSR
    lda ROPRTY      ; get RS-232 parity byte
    bmi LAB_FAF0

    bcc LAB_FB07

    clc
LAB_FAF0:
    bcs LAB_FB07

    and #$0F
    sta RIDATA
LAB_FAF6:
    dec RIDATA
    bne LAB_FAD7

    lda #$40
    sta RIDATA
    jsr RD300       ; copy I/O start address to buffer address
    lda #$00
    sta RIPRTY
    beq LAB_FAD7    ; branch always


;***********************************************************************************;
;
;## reset pointer

LAB_FB07:
    lda #$80
    sta RIDATA
    bne LAB_FAD7    ; restore registers and exit interrupt, branch always

LAB_FB0D:
    lda NXTBIT
    beq LAB_FB1B

    lda #$04
    jsr ORIOST      ; OR into serial status byte
    lda #$00
    jmp LAB_FB97

LAB_FB1B:
    jsr VPRTY       ; check read/write pointer, return Cb = 1 if pointer >= end
    bcc LAB_FB23

    jmp LAB_FB95

LAB_FB23:
    ldx INBIT       ; get receiver input bit temporary storage
    dex
    beq LAB_FB55

    lda VERCK       ; get load/verify flag
    beq LAB_FB38    ; branch if load

    ldy #$00        ; clear index
    lda ROPRTY      ; get RS-232 parity byte
    cmp (SAL),Y
    beq LAB_FB38

    lda #$01
    sta RODATA
LAB_FB38:
    lda RODATA
    beq LAB_FB87

    ldx #$3D
    cpx PTR1
    bcc LAB_FB80

    ldx PTR1
    lda SAL+1
    sta STACK+1,X
    lda SAL
    sta STACK,X
    inx
    inx
    stx PTR1
    jmp LAB_FB87

LAB_FB55:
    ldx PTR2
    cpx PTR1
    beq LAB_FB90

    lda SAL
    cmp STACK,X
    bne LAB_FB90

    lda SAL+1
    cmp STACK+1,X
    bne LAB_FB90

    inc PTR2
    inc PTR2
    lda VERCK       ; get load/verify flag
    beq LAB_FB7C    ; branch if load

    lda ROPRTY      ;nget RS-232 parity byte
    ldy #$00
    cmp (SAL),Y
    beq LAB_FB90

    INY
    sty RODATA
LAB_FB7C:
    lda RODATA
    beq LAB_FB87

LAB_FB80:
    lda #$10
    jsr ORIOST      ; OR into serial status byte
    bne LAB_FB90

LAB_FB87:
    lda VERCK       ; get load/verify flag
    bne LAB_FB90    ; branch if verify

    tay
    lda ROPRTY      ; get RS-232 parity byte
    sta (SAL),Y
LAB_FB90:
    jsr WRT62       ; increment read/write pointer
    bne LAB_FBCF    ; restore registers and exit interrupt, branch always

LAB_FB95:
    lda #$80
LAB_FB97:
    sta RIDATA
    ldx FSBLK       ; get copies count
    dex
    bmi LAB_FBA0

    stx FSBLK       ; save copies count
LAB_FBA0:
    dec INBIT       ; decrement receiver input bit temporary storage
    beq LAB_FBAC

    lda PTR1
    bne LAB_FBCF    ; if ?? restore registers and exit interrupt

    sta FSBLK       ; save copies count
    beq LAB_FBCF    ; restore registers and exit interrupt, branch always

LAB_FBAC:
    jsr TNIF        ; restore everything for STOP
    jsr RD300       ; copy I/O start address to buffer address
    ldy #$00        ; clear index
    sty RIPRTY      ; clear checksum
LAB_FBB6:
    lda (SAL),Y     ; get byte from buffer
    eor RIPRTY      ; XOR with checksum
    sta RIPRTY      ; save new checksum
    jsr WRT62       ; increment read/write pointer
    jsr VPRTY       ; check read/write pointer, return Cb = 1 if pointer >= end
    bcc LAB_FBB6    ; loop if not at end

    lda RIPRTY      ; get computed checksum
    eor ROPRTY      ; compare with stored checksum ??
    beq LAB_FBCF    ; if checksum ok restore registers and exit interrupt

    lda #$20        ; else set checksum error
    jsr ORIOST      ; OR into serial status byte
LAB_FBCF:
    jmp _RTI        ; restore registers and exit interrupt


;***********************************************************************************;
;
; copy I/O start address to buffer address

RD300:
    lda STAL+1      ; get I/O start address high byte
    sta SAL+1       ; set buffer address high byte
    lda STAL        ; get I/O start address low byte
    sta SAL         ; set buffer address low byte
    rts


;***********************************************************************************;
;
; new tape byte setup

NEWCH:
    lda #$08        ; eight bits to do
    sta PCNTR       ; set bit count
    lda #$00        ; clear .A
    sta FIRT        ; clear tape bit cycle phase
    sta BITCI       ; clear start bit first cycle done flag
    sta PRTY        ; clear byte parity
    sta RINONE      ; clear start bit check flag, set no start bit yet
    rts


;***********************************************************************************;
;
; send lsb from tape write byte to tape

; This routine tests the least significant bit in the tape write byte and sets VIA 2 T2
; depending on the state of the bit. If the bit is a 1 a time of $00B0 cycles is set, if
; the bit is a 0 a time of $0060 cycles is set. Note that this routine does not shift the
; bits of the tape write byte but uses a copy of that byte, the byte itself is shifted
; elsewhere.

TPTOGLE:
    lda ROPRTY      ; get tape write byte
    lsr             ; shift lsb into Cb
    lda #$60        ; set time constant low byte for bit = 0
    bcc LAB_FBF3    ; branch if bit was 0

; set time constant for bit = 1 and toggle tape

LAB_FBF1:
    lda #$B0        ; set time constant low byte for bit = 1

; write time constant and toggle tape

LAB_FBF3:
    ldx #$00        ; set time constant high byte

; write time constant and toggle tape

LAB_FBF5:
    sta VIA2T2CL    ; set VIA 2 T2C_l
    stx VIA2T2CH    ; set VIA 2 T2C_h
    lda VIA2PB      ; get VIA 2 DRB, keyboard column
    eor #$08        ; toggle tape out bit
    sta VIA2PB      ; set VIA 2 DRB
    and #$08        ; mask tape out bit
    rts


;***********************************************************************************;
;
; flag block done and exit interrupt

BLKEND:
    sec             ; set carry flag
    ror SAL+1       ; set buffer address high byte negative, flag all sync,
                    ; data and checksum bytes written
    bmi LAB_FC47    ; restore registers and exit interrupt, branch always


;***********************************************************************************;
;
; tape write, IRQ routine.

; This is the routine that writes the bits to the tape. It is called each time VIA 2 T2
; times out and checks if the start bit is done, if so checks if the data bits are done,
; if so it checks if the byte is done, if so it checks if the synchronisation bytes are
; done, if so it checks if the data bytes are done, if so it checks if the checksum byte
; is done, if so it checks if both the load and verify copies have been done, if so it
; stops the tape.

WRITE:
    lda BITCI       ; get start bit first cycle done flag
    bne LAB_FC21    ; if first cycle done go do rest of byte

; each byte sent starts with two half cycles of $0110 system clocks and the whole block
; ends with two more such half cycles

    lda #$10        ; set first start cycle time constant low byte
    ldx #$01        ; set first start cycle time constant high byte
    jsr LAB_FBF5    ; write time constant and toggle tape
    bne LAB_FC47    ; if first half cycle go restore registers and exit
                    ; interrupt

    inc BITCI       ; set start bit first start cycle done flag
    lda SAL+1       ; get buffer address high byte
    bpl LAB_FC47    ; if block not complete go restore registers and exit
                    ; interrupt. the end of a block is indicated by the tape
                    ; buffer high byte b7 being set to 1

    jmp WRTN1       ; else do tape routine, block complete exit

; Continue tape byte write. The first start cycle, both half cycles of it, is complete
; so the routine drops straight through to here.

LAB_FC21:
    lda RINONE      ; get start bit check flag
    bne LAB_FC2E    ; if the start bit is complete go send the byte bits

; After the two half cycles of $0110 system clocks the start bit is completed with two
; half cycles of $00B0 system clocks. This is the same as the first part of a 1 bit.

    jsr LAB_FBF1    ; set time constant for bit = 1 and toggle tape
    bne LAB_FC47    ; if first half cycle go restore registers and exit
                    ; interrupt

    inc RINONE      ; set start bit check flag
    bne LAB_FC47    ; restore registers and exit interrupt, branch always

; Continue tape byte write. The start bit, both cycles of it, is complete so the routine
; drops straight through to here. Now the cycle pairs for each bit, and the parity bit,
; are sent.

LAB_FC2E:
    jsr TPTOGLE     ; send lsb from tape write byte to tape
    bne LAB_FC47    ; if first half cycle go restore registers and exit
                    ; interrupt

                    ; else two half cycles have been done
    lda FIRT        ; get tape bit cycle phase
    eor #$01        ; toggle b0
    sta FIRT        ; save tape bit cycle phase
    beq LAB_FC4A    ; if bit cycle phase complete go setup for next bit

; Each bit is written as two full cycles. A 1 is sent as a full cycle of $0160 system
; clocks then a full cycle of $00C0 system clocks. A 0 is sent as a full cycle of $00C0
; system clocks then a full cycle of $0160 system clocks. To do this each bit from the
; write byte is inverted during the second bit cycle phase. As the bit is inverted it
; is also added to the, one bit, parity count for this byte.

    lda ROPRTY      ; get tape write byte
    eor #$01        ; invert bit being sent
    sta ROPRTY      ; save tape write byte
    and #$01        ; mask b0
    eor PRTY        ; XOR with tape write byte parity bit
    sta PRTY        ; save tape write byte parity bit
LAB_FC47:
    jmp _RTI        ; restore registers and exit interrupt

; the bit cycle phase is complete so shift out the just written bit and test for byte
; end

LAB_FC4A:
    lsr ROPRTY      ; shift bit out of tape write byte
    dec PCNTR       ; decrement tape write bit count
    lda PCNTR       ; get tape write bit count
    beq LAB_FC8C    ; if all the data bits have been written go setup for
                    ; sending the parity bit next and exit the interrupt

    bpl LAB_FC47    ; if all the data bits are not yet sent just restore the
                    ; registers and exit the interrupt

; do next tape byte

; The byte is complete. The start bit, data bits and parity bit have been written to
; the tape so setup for the next byte.

LAB_FC54:
    jsr NEWCH       ; new tape byte setup
    cli             ; enable interrupts
    lda CNTDN       ; get cassette synchronisation character count
    beq LAB_FC6E    ; if synchronisation characters done go do block data

; At the start of each block sent to tape there are a number of synchronisation bytes
; that count down to the actual data. The Commodore tape system saves two copies of all
; the tape data, the first is loaded and is indicated by the synchronisation bytes
; having b7 set, and the second copy is indicated by the synchronisation bytes having b7
; clear. the sequence goes $09, $08, ... $02, $01, data bytes.

    ldx #$00        ; clear .X
    stx ASCII       ; clear checksum byte
    dec CNTDN       ; decrement cassette synchronisation byte count
    ldx FSBLK       ; get cassette copies count
    cpx #$02        ; compare with load block indicator
    bne LAB_FC6A    ; branch if not the load block

    ora #$80        ; this is the load block so make the synchronisation count
                    ; go $89, $88, ... $82, $81
LAB_FC6A:
    sta ROPRTY      ; save the synchronisation byte as the tape write byte
    bne LAB_FC47    ; restore registers and exit interrupt, branch always

; the synchronisation bytes have been done so now check and do the actual block data

LAB_FC6E:
    jsr VPRTY       ; check read/write pointer, return Cb = 1 if pointer >= end
    bcc LAB_FC7D    ; if not all done yet go get the byte to send

    bne BLKEND      ; if pointer > end go flag block done and exit interrupt

                    ; else the block is complete, it only remains to write the
                    ; checksum byte to the tape so setup for that
    inc SAL+1       ; increment buffer pointer high byte, this means the block
                    ; done branch will always be taken next time without having
                    ; to worry about the low byte wrapping to zero
    lda ASCII       ; get checksum byte
    sta ROPRTY      ; save checksum as tape write byte
    bcs LAB_FC47    ; restore registers and exit interrupt, branch always

; the block isn't finished so get the next byte to write to tape

LAB_FC7D:
    ldy #$00        ; clear index
    lda (SAL),Y     ; get byte from buffer
    sta ROPRTY      ; save as tape write byte
    eor ASCII       ; XOR with checksum byte
    sta ASCII       ; save new checksum byte
    jsr WRT62       ; increment read/write pointer
    bne LAB_FC47    ; restore registers and exit interrupt, branch always

; set parity as next bit and exit interrupt

LAB_FC8C:
    lda PRTY        ; get parity bit
    eor #$01        ; toggle it
    sta ROPRTY      ; save as tape write byte
LAB_FC92:
    jmp _RTI        ; restore registers and exit interrupt

; tape routine, block complete exit

WRTN1:
    dec FSBLK       ; decrement copies remaining to read/write
    bne LAB_FC9C    ; branch if more to do

    jsr TNOFF       ; else stop cassette motor
LAB_FC9C:
    lda #$50        ; set tape write leader count
    sta INBIT       ; save tape write leader count
    ldx #$08        ; set index for write tape leader vector
    sei             ; disable interrupts
    jsr LAB_FCFB    ; set tape vector
    bne LAB_FC92    ; restore registers and exit interrupt, branch always


;***********************************************************************************;
;
; write tape leader IRQ routine

WRTZ:
    lda #$78        ; set time constant low byte for bit = leader
    jsr LAB_FBF3    ; write time constant and toggle tape
    bne LAB_FC92    ; if tape bit high restore registers and exit interrupt

    dec INBIT       ; decrement cycle count
    bne LAB_FC92    ; if not all done restore registers and exit interrupt

    jsr NEWCH       ; new tape byte setup
    dec RIPRTY      ; decrement cassette leader count
    bpl LAB_FC92    ; if not all done restore registers and exit interrupt

    ldx #$0A        ; set index for tape write vector
    jsr LAB_FCFB    ; set tape vector
    cli             ; enable interrupts
    inc RIPRTY      ; clear cassette leader counter, was $FF
    lda FSBLK       ; get cassette block count
    beq BSIV        ; if all done restore everything for STOP and exit interrupt

    jsr RD300       ; copy I/O start address to buffer address
    ldx #$09        ; set nine synchronisation bytes
    stx CNTDN       ; save cassette synchronisation byte count
    bne LAB_FC54    ; go do next tape byte, branch always


;***********************************************************************************;
;
; restore everything for STOP

TNIF:
    php             ; save status
    sei             ; disable interrupts
    jsr TNOFF       ; stop cassette motor
    lda #$7F        ; disable all interrupts
    sta VIA2IER     ; set VIA 2 IER
    lda #$F7        ; set keyboard column 3 active
    sta VIA2PB      ; set VIA 2 DRB, keyboard column
    lda #$40        ; set T1 free run, T2 clock n2,
                    ; SR disabled, latches disabled
    sta VIA2ACR     ; set VIA 2 ACR
    jsr LAB_FE39    ; set 60Hz and enable timer
    lda IRQTMP+1    ; get saved IRQ vector high byte
    beq LAB_FCF4    ; branch if null

    sta CINV+1      ; restore IRQ vector high byte
    lda IRQTMP      ; get saved IRQ vector low byte
    sta CINV        ; restore IRQ vector low byte
LAB_FCF4:
    plp             ; restore status
    rts


;***********************************************************************************;
;
; reset vector

BSIV:
    jsr TNIF        ; restore everything for STOP
    beq LAB_FC92    ; restore registers and exit interrupt, branch always


;***********************************************************************************;
;
; set tape vector

LAB_FCFB:
    lda IRQVCTRS-8,X    ; get tape IRQ vector low byte
    sta CINV            ; set IRQ vector low byte
    lda IRQVCTRS-7,X    ; get tape IRQ vector high byte
    sta CINV+1          ; set IRQ vector high byte
    rts


;***********************************************************************************;
;
; stop cassette motor

TNOFF:
    lda VIA1PCR     ; get VIA 1 PCR
    ora #$0E        ; set CA2 high, cassette motor off
    sta VIA1PCR     ; set VIA 1 PCR
    rts


;***********************************************************************************;
;
; check read/write pointer
; return Cb = 1 if pointer >= end

VPRTY:
    sec             ; set carry for subtract
    lda SAL         ; get buffer address low byte
    sbc EAL         ; subtract buffer end low byte
    lda SAL+1       ; get buffer address high byte
    sbc EAL+1       ; subtract buffer end high byte
    rts


;***********************************************************************************;
;
; increment read/write pointer

WRT62:
    inc SAL         ; increment buffer address low byte
    bne LAB_FD21    ; if no overflow skip the high byte increment

    inc SAL+1       ; increment buffer address high byte
LAB_FD21:
    rts


;***********************************************************************************;
;
; RESET, hardware reset starts here

START:
    ldx #$FF        ; set .X for stack
    sei             ; disable interrupts
    txs             ; clear stack
    cld             ; clear decimal mode
    jsr CHKAUTO     ; scan for autostart ROM at $A000
    bne LAB_FD2F    ; if not there continue VIC startup

    jmp (XROMCOLD)  ; call ROM start code

LAB_FD2F:
    jsr INITMEM     ; initialise and test RAM
    jsr FRESTOR     ; restore default I/O vectors
    jsr INITVIA     ; initialise I/O registers
    jsr INITSK      ; initialise hardware
    cli             ; enable interrupts
    jmp (COLDST)    ; execute BASIC


;***********************************************************************************;
;
; scan for autostart ROM at $A000, returns Zb=1 if ROM found

CHKAUTO:
    ldx #$05        ; five characters to test
LAB_FD41:
    lda A0CBM-1,X   ; get test character
    cmp XROMID-1,X  ; compare with byte in ROM space
    bne LAB_FD4C    ; exit if no match

    dex             ; decrement index
    bne LAB_FD41    ; loop if not all done

LAB_FD4C:
    rts


;***********************************************************************************;
;
; autostart ROM signature

A0CBM:
    .byte "A0",$C3,$C2,$CD  ; A0CBM


;***********************************************************************************;
;
; restore default I/O vectors

; This routine restores the default values of all system vectors used in KERNAL and
; BASIC routines and interrupts. The KERNAL VECTOR routine is used to read and alter
; individual system vectors.

FRESTOR:
    ldx #<VECTORS   ; pointer to vector table low byte
    ldy #>VECTORS   ; pointer to vector table high byte
    clc             ; flag set vectors


;***********************************************************************************;
;
; set/read vectored I/O from (.X.Y), Cb = 1 to read, Cb = 0 to set

; This routine manages all system vector jump addresses stored in RAM. Calling this
; routine with the accumulator carry bit set will store the current contents of the
; RAM vectors in a list pointed to by the .X and .Y registers.

; When this routine is called with the carry bit clear, the user list pointed to by
; the .X and .Y registers is transferred to the system RAM vectors.

; NOTE: This routine requires caution in its use. The best way to use it is to first
; read the entire vector contents into the user area, alter the desired vectors, and
; then copy the contents back to the system vectors.

FVECTOR:
    stx MEMUSS      ; save pointer low byte
    sty MEMUSS+1    ; save pointer high byte
    ldy #$1F        ; set byte count
LAB_FD5D:
    lda CINV,Y      ; read vector byte from vectors
    bcs LAB_FD64    ; if read vectors skip the read from .X.Y

    lda (MEMUSS),Y  ; read vector byte from (.X.Y)
LAB_FD64:
    sta (MEMUSS),Y  ; save byte to (.X.Y)
    sta CINV,Y      ; save byte to vector
    dey             ; decrement index
    bpl LAB_FD5D    ; loop if more to do

    rts

;; The above code works but it tries to write to the ROM. While this is usually harmless
;; systems that use flash ROM may suffer. Here is a version that makes the extra write
;; to RAM instead but is otherwise identical in function. ##
;
;; set/read vectored I/O from (.X.Y), Cb = 1 to read, Cb = 0 to set
;
;FVECTOR
;   stx MEMUSS      ; save pointer low byte
;   sty MEMUSS+1    ; save pointer high byte
;   ldy #$1F        ; set byte count
;LAB_FD5D
;   lda (MEMUSS),Y  ; read vector byte from (.X.Y)
;   bcc LAB_FD66    ; if set vectors skip the read from .X.Y
;
;   lda CINV,Y      ; else read vector byte from vectors
;   sta (MEMUSS),Y  ; save byte to (.X.Y)
;LAB_FD66
;   sta CINV,Y      ; save byte to vector
;   dey             ; decrement index
;   bpl LAB_FD5D    ; loop if more to do
;
;   rts


;***********************************************************************************;
;
; KERNAL vectors

VECTORS:
    .word IRQ       ; CINV      IRQ vector
    .word BREAK     ; CBINV     BRK vector
    .word NMI2      ; NMINV     NMI vector
    .word FOPEN     ; IOPEN     open a logical file
    .word FCLOSE    ; ICLOSE    close a specified logical file
    .word FCHKIN    ; ICHKIN    open channel for input
    .word FCHKOUT   ; ICKOUT    open channel for output
    .word FCLRCHN   ; ICLRCN    close input and output channels
    .word FCHRIN    ; IBASIN    input character from channel
    .word FCHROUT   ; IBSOUT    output character to channel
    .word FSTOP     ; ISTOP     scan stop key
    .word FGETIN    ; IGETIN    get character from keyboard queue
    .word FCLALL    ; ICLALL    close all channels and files
    .word BREAK     ; USRCMD    user function

; Vector to user defined command, currently points to BRK.

; This appears to be a holdover from PET days, when the built-in machine language monitor
; would jump through the $032E vector when it encountered a command that it did not
; understand, allowing the user to add new commands to the monitor.

; Although this vector is initialised to point to the routine called by STOP/RESTORE and
; the BRK interrupt, and is updated by the KERNAL vector routine at $FD57, it no longer
; has any function.

    .word   FLOAD2  ; ILOAD     load
    .word   FSAVE2  ; ISAVE     save


;***********************************************************************************;
;
; Initialise and test RAM, the RAM from $000 to $03FF is never tested and is just assumed
; to work. First a search is done from $0401 for the start of memory and this is saved, if
; this start is at or beyond $1100 then the routine dead ends. Once the start of memory is
; found the routine looks for the end of memory, if this end is before $2000 the routine
; again dead ends. Lastly, if the end of memory is at $2000 then the screen is set to
; $1E00, but if the memory extends to or beyond $2100 then the screen is moved to $1000.

INITMEM:
    lda #$00        ; clear .A
    tax             ; clear index
LAB_FD90:
    sta USRPPOK,X   ; clear page 0
    sta BUF,X       ; clear page 2
    sta IERROR,X    ; clear page 3
    inx             ; increment index
    bne LAB_FD90    ; loop if more to do

    ldx #<TBUFFR    ; set cassette buffer pointer low byte
    ldy #>TBUFFR    ; set cassette buffer pointer high byte
    stx TAPE1       ; save tape buffer start pointer low byte
    sty TAPE1+1     ; save tape buffer start pointer high byte

    sta STAL        ; clear RAM test pointer low byte
    sta XSAV        ; clear looking for end flag
    sta MEMSTR      ; clear OS start of memory low byte

    tay             ; clear .Y
    lda #$04        ; set RAM test pointer high byte
    sta STAL+1      ; save RAM test pointer high byte
LAB_FDAF:
    inc STAL        ; increment RAM test pointer low byte
    bne LAB_FDB5    ; if no rollover skip the high byte increment

    inc STAL+1      ; increment RAM test pointer high byte
LAB_FDB5:
    jsr TSTMEM      ; test RAM byte, return Cb=0 if failed
    lda XSAV        ; test looking for end flag
    beq LAB_FDDE    ; branch if not looking for end

                    ; else now looking for the end of memory
    bcs LAB_FDAF    ; loop if byte test passed

    ldy STAL+1      ; get test address high byte
    ldx STAL        ; get test address low byte
    cpy #$20        ; compare with $2000, RAM should always end at or after
                    ; $2000 even with no expansion memory as the built in RAM
                    ; ends at $1FFF. therefore the following test should
                    ; never branch
    bcc LAB_FDEB    ; if end address < $2000 go do dead end loop

    cpy #$21        ; compare with $2100
    bcs LAB_FDD2    ; branch if >= $2100

                    ; else memory ended before $2100
    ldy #$1E        ; set screen memory page to $1E00
    sty HIBASE      ; save screen memory page
LAB_FDCF:
    jmp LAB_FE7B    ; set the top of memory and return

                    ; memory ends beyond $2100
LAB_FDD2:
    lda #$12        ; set OS start of memory high byte
    sta MEMSTR+1    ; save OS start of memory high byte
    lda #$10        ; set screen memory page to $1000
    sta HIBASE      ; save screen memory page
    bne LAB_FDCF    ; set the top of memory and return, branch always

LAB_FDDE:
    bcc LAB_FDAF    ; loop if byte test failed, not found start yet

                    ; else found start of RAM
    lda STAL+1      ; get test address high byte
    sta MEMSTR+1    ; save OS start of memory high byte
    sta XSAV        ; set looking for end flag
    cmp #$11        ; compare start with $1100, RAM should always start before
                    ; $1100 even with no expansion memory as the built in RAM
                    ; starts at $1000. therefore the following test should
                    ; always branch
    bcc LAB_FDAF    ; go find end of RAM, branch always

                    ; if the code drops through here then the RAM has failed
                    ; and there is not much else to be done
LAB_FDEB:
    jsr INITVIC     ; initialise VIC chip
    jmp LAB_FDEB    ; loop forever


;***********************************************************************************;
;
; tape IRQ vectors

IRQVCTRS:
    .word WRTZ      ; $08   write tape leader IRQ routine
    .word WRITE     ; $0A   tape write IRQ routine
    .word IRQ       ; $0C   normal IRQ vector
    .word READT     ; $0E   read tape bits IRQ routine


;***********************************************************************************;
;
; initialise I/O registers

INITVIA:
    lda #$7F        ; disable all interrupts
    sta VIA1IER     ; on VIA 1 IER ..
    sta VIA2IER     ; .. and VIA 2 IER

    lda #$40        ; set T1 free run, T2 clock  2,
                    ; SR disabled, latches disabled
    sta VIA2ACR     ; set VIA 2 ACR

    lda #$40        ; set T1 free run, T2 clock  2,
                    ; SR disabled, latches disabled
    sta VIA1ACR     ; set VIA 1 ACR

    lda #$FE        ; CB2 high, RS-232 Tx
                    ; CB1 +ve edge,
                    ; CA2 high, tape motor off
                    ; CA1 -ve edge
    sta VIA1PCR     ; set VIA 1 PCR

    lda #$DE        ; CB2 low, serial data out high
                    ; CB1 +ve edge,
                    ; CA2 high, serial clock out low
                    ; CA1 -ve edge
    sta VIA2PCR     ; set VIA 2 PCR

    ldx #$00        ; all inputs, RS-232 interface or parallel user port
    stx VIA1DDRB    ; set VIA 1 DDRB

    ldx #$FF        ; all outputs, keyboard column
    stx VIA2DDRB    ; set VIA 2 DDRB

    ldx #$00        ; all inputs, keyboard row
    stx VIA2DDRA    ; set VIA 2 DDRA

    ldx #$80        ; OIII IIII, ATN out, light pen, joystick, serial data
                    ; in, serial clk in
    stx VIA1DDRA    ; set VIA 1 DDRA

    ldx #$00        ; ATN out low, set ATN high
    stx VIA1PA2     ; set VIA 1 DRA, no handshake

    jsr SRCLKHI     ; set serial clock high
    lda #$82        ; enable CA1 interrupt, [RESTORE] key
    sta VIA1IER     ; set VIA 1 IER
    jsr SRCLKLO     ; set serial clock low


;***********************************************************************************;
;
; set 60Hz and enable timer

LAB_FE39:
    lda #$C0        ; enable T1 interrupt
    sta VIA2IER     ; set VIA 2 IER
.ifdef PAL
    lda #$26        ; set timer constant low byte [PAL]
.endif
.ifdef NTSC
    lda #$89        ; set timer constant low byte [NTSC]
.endif
    sta VIA2T1CL    ; set VIA 2 T1C_l
.ifdef PAL
    lda #$48        ; set timer constant high byte [PAL]
.endif
.ifdef NTSC
    lda #$42        ; set timer constant high byte [NTSC]
.endif
    sta VIA2T1CH    ; set VIA 2 T1C_h
    rts


;***********************************************************************************;
;
; set filename

; This routine is used to set up the file name for the OPEN, SAVE, or LOAD routines.
; The accumulator must be loaded with the length of the file and .X.Y with the pointer
; to file name, .X being the low byte. The address can be any valid memory address in
; the system where a string of characters for the file name is stored. If no file
; name desired the accumulator must be set to 0, representing a zero file length,
; in that case .X.Y may be set to any memory address.

FSETNAM:
    sta FNLEN       ; set file name length
    stx FNADR       ; set file name pointer low byte
    sty FNADR+1     ; set file name pointer high byte
    rts


;***********************************************************************************;
;
; set logical file, first and second addresses

; This routine will set the logical file number, device address, and secondary
; address, command number, for other KERNAL routines.

; The logical file number is used by the system as a key to the file table created
; by the OPEN file routine. Device addresses can range from 0 to 30. The following
; codes are used by the computer to stand for the following devices:

; ADDRESS   DEVICE
; =======   ======
;  0        Keyboard
;  1        Cassette
;  2        RS-232
;  3        CRT display
;  4        Serial bus printer
;  8        Serial bus disk drive

; device numbers of four or greater automatically refer to devices on the serial
; bus.

; A command to the device is sent as a secondary address on the serial bus after
; the device number is sent during the serial attention handshaking sequence. If
; no secondary address is to be sent .Y should be set to $FF.

FSETLFS:
    sta LA          ; set logical file
    stx FA          ; set device number
    sty SA          ; set secondary address or command
    rts


;***********************************************************************************;
;
; read I/O status word

; This routine returns the current status of the I/O device in the accumulator. The
; routine is usually called after new communication to an I/O device. The routine
; will give information about device status, or errors that have occurred during the
; I/O operation.

FREADST:
    lda FA          ; get device number
    cmp #$02        ; compare device with RS-232 device
    bne READIOST    ; branch if not RS-232 device

                    ; get RS-232 device status
    lda RSSTAT      ; read RS-232 status word

    lda #$00        ; clear .A
    sta RSSTAT      ; clear RS-232 status

; The above code is wrong. The RS-232 status is in .A but .A is cleared and that is used
; to clear the RS-232 status byte. So whatever the status the result is always $00 and
; the status byte is always cleared. A solution is to use .X to clear the status after
; it is read instead of the above like this ..
;
;   ldx #$00        ; clear .X
;   stx RSSTAT      ; clear RS-232 status ##
    rts


;***********************************************************************************;
;
; control KERNAL messages

; This routine controls the printing of error and control messages by the KERNAL.
; Either print error messages or print control messages can be selected by setting
; the accumulator when the routine is called.

; FILE NOT FOUND is an example of an error message. PRESS plaY ON CASSETTE is an
; example of a control message.

; Bits 6 and 7 of this value determine where the message will come from. If bit 7
; is set one of the error messages from the KERNAL will be printed. If bit 6 is set
; a control message will be printed.

FSETMSG:
    sta MSGFLG      ; set message mode flag
READIOST:
    lda STATUS      ; read serial status byte

; OR into serial status byte

ORIOST:
    ora STATUS      ; OR with serial status byte
    sta STATUS      ; save serial status byte
    rts


;***********************************************************************************;
;
; set timeout on IEEE-488 bus

; This routine sets the timeout flag for the serial bus. When the timeout flag is
; set, the computer will wait for a device on the serial bus for 64 milliseconds.
; If the device does not respond to the computer's DAV signal within that time the
; computer will recognize an error condition and leave the handshake sequence. When
; this routine is called and the accumulator contains a 0 in bit 7, timeouts are
; enabled. A 1 in bit 7 will disable the timeouts.

; NOTE: The timeout feature is used to communicate that a disk file is not found on
; an attempt to OPEN a file.

FSETTMO:
    sta TIMOUT      ; save serial bus timeout flag
    rts


;***********************************************************************************;
;
; read/set the top of memory, Cb = 1 to read, Cb = 0 to set

; This routine is used to read and set the top of RAM. When this routine is called
; with the carry bit set the pointer to the top of RAM will be loaded into .X.Y. When
; this routine is called with the carry bit clear .X.Y will be saved as the top of
; memory pointer changing the top of memory.

FMEMTOP:
    bcc LAB_FE7B    ; if Cb clear go set the top of memory

; read the top of memory

LAB_FE75:
    ldx MEMHIGH     ; get memory top low byte
    ldy MEMHIGH+1   ; get memory top high byte

; set the top of memory

LAB_FE7B:
    stx MEMHIGH     ; set memory top low byte
    sty MEMHIGH+1   ; set memory top high byte
    rts


;***********************************************************************************;
;
; read/set the bottom of memory, Cb = 1 to read, Cb = 0 to set

; This routine is used to read and set the bottom of RAM. When this routine is
; called with the carry bit set the pointer to the bottom of RAM will be loaded
; into .X.Y. When this routine is called with the carry bit clear .X.Y will be saved as
; the bottom of memory pointer changing the bottom of memory.

FMEMBOT:
    bcc LAB_FE8A    ; if Cb clear go set the bottom of memory

; read the bottom of memory

    ldx MEMSTR      ; read OS start of memory low byte
    ldy MEMSTR+1    ; read OS start of memory high byte

; set the bottom of memory

LAB_FE8A:
    stx MEMSTR      ; set OS start of memory low byte
    sty MEMSTR+1    ; set OS start of memory high byte
    rts


;***********************************************************************************;
;
; non-destructive test RAM byte, return Cb=0 if failed

TSTMEM:
    lda (STAL),Y    ; get existing RAM byte
    tax             ; copy to .X
    lda #$55        ; set first test byte
    sta (STAL),Y    ; save to RAM
    cmp (STAL),Y    ; compare with saved
    bne LAB_FEA4    ; branch if fail

    ror             ; make byte $AA, carry is set here
    sta (STAL),Y    ; save to RAM
    cmp (STAL),Y    ; compare with saved
    bne LAB_FEA4    ; branch if fail
    .byte   $A9     ; makes next line lda #$18

LAB_FEA4:
    clc             ; flag test failed
    txa             ; get original byte back
    sta (STAL),Y    ; restore original byte
    rts


;***********************************************************************************;
;
; NMI vector

NMI:
    sei             ; disable interrupts
    jmp (NMINV)     ; do NMI vector


;***********************************************************************************;
;
; NMI handler

NMI2:
    pha             ; save .A
    txa             ; copy .X
    pha             ; save .X
    tya             ; copy .Y
    pha             ; save .Y
    lda VIA1IFR     ; get VIA 1 IFR
    bpl LAB_FEFF    ; if no interrupt restore registers and exit

    and VIA1IER     ; and with VIA 1 IER
    tax             ; copy to .X
    and #$02        ; mask [RESTORE] key
    beq RSNMI       ; branch if not [RESTORE] key

                    ; else was [RESTORE] key ..
    jsr CHKAUTO     ; scan for autostart ROM at $A000
    bne LAB_FEC7    ; branch if no autostart ROM

    jmp (XROMWARM)  ; else do autostart ROM break entry

LAB_FEC7:
    bit VIA1PA1     ; test VIA 1 DRA
    jsr FUDTIM      ; increment the real time clock
    jsr STOP        ; scan stop key
    bne LAB_FEFF    ; if not [STOP] restore registers and exit interrupt


;***********************************************************************************;
;
; BRK handler

BREAK:
    jsr FRESTOR     ; restore default I/O vectors
    jsr INITVIA     ; initialise I/O registers
    jsr INITSK      ; initialise hardware
    jmp (WARMST)    ; do BASIC break entry


;***********************************************************************************;
;
; RS-232 NMI routine

RSNMI:
    lda VIA1IER     ; get VIA 1 IER
    ora #$80        ; set enable bit, this bit should be set according to the
                    ; Rockwell 6522 datasheet but clear acording to the MOS
                    ; datasheet. best to assume it's not in the state required
                    ; and set it so
    pha             ; save to re-enable interrupts
    lda #$7F        ; disable all interrupts
    sta VIA1IER     ; set VIA 1 IER
    txa             ; get active interrupts back
    and #$40        ; mask T1 interrupt
    beq LAB_FF02    ; branch if not T1 interrupt

                    ; was VIA 1 T1 interrupt
    lda #$CE        ; CB2 low, CB1 -ve edge, CA2 high, CA1 -ve edge
    ora NXTBIT      ; OR RS-232 next bit to send, sets CB2 high if set
    sta VIA1PCR     ; set VIA 1 PCR
    lda VIA1T1CL    ; get VIA 1 T1C_l
    pla             ; restore interrupt enable byte to restore previously
                    ; enabled interrupts
    sta VIA1IER     ; set VIA 1 IER
    jsr RSNXTBIT    ; RS-232 Tx NMI routine
LAB_FEFF:
    jmp _RTI        ; restore registers and exit interrupt

                    ; was not VIA 1 T1 interrupt
LAB_FF02:
    txa             ; get active interrupts back
    and #$20        ; mask T2 interrupt
    beq LAB_FF2C    ; branch if not T2 interrupt

                    ; was VIA 1 T2 interrupt
    lda VIA1PB      ; get VIA 1 DRB
    and #$01        ; mask RS-232 data in
    sta INBIT       ; save receiver input bit temp storage
    lda VIA1T2CL    ; get VIA 1 T2C_l
    sbc #$16
    adc BAUDOF      ; add baud rate bit time low byte
    sta VIA1T2CL    ; set VIA 1 T2C_l
    lda VIA1T2CH    ; get VIA 1 T2C_h
    adc BAUDOF+1    ; add baud rate bit time high byte
    sta VIA1T2CH    ; set VIA 1 T2C_h
    pla             ; restore interrupt enable byte to restore previously
                    ; enabled interrupts
    sta VIA1IER     ; set VIA 1 IER, restore interrupts
    jsr RSINBIT     ; RS-232 Rx
    jmp _RTI        ; restore registers and exit interrupt

                    ; was not VIA 1 T2 interrupt
LAB_FF2C:
    txa             ; get active interrupts back
    and #$10        ; mask CB1 interrupt, Rx data bit transition
    beq _RTI        ; if no bit restore registers and exit interrupt

    lda M51CTR      ; get pseudo 6551 control register
    and #$0F        ; clear non baud bits
    bne LAB_FF38    ; short delay. was this to be a branch to code to implement
                    ; the user baud rate ??
LAB_FF38:
    asl             ; *2, 2 bytes per baud count
    tax             ; copy to index
    lda BAUDTBL-2,X ; get baud count low byte
    sta VIA1T2CL    ; set VIA 1 T2C_l
    lda BAUDTBL-1,X ; get baud count high byte
    sta VIA1T2CH    ; set VIA 1 T2C_h
    lda VIA1PB      ; read VIA 1 DRB, clear interrupt flag
    pla             ; restore interrupt enable byte to restore previously
                    ; enabled interrupts
    ora #$20        ; enable T2 interrupt
    and #$EF        ; disable CB1 interrupt
    sta VIA1IER     ; set VIA 1 IER
    ldx BITNUM      ; get number of bits to be sent/received
    stx BITCI       ; save receiver bit count in


;***********************************************************************************;
;
; restore the registers and exit the interrupt
;
; If you write your own interrupt code you should either return from the interrupt
; using code that ends up here or code that replicates this code.

_RTI:
    pla             ; pull .Y
    tay             ; restore .Y
    pla             ; pull .X
    tax             ; restore .X
    pla             ; restore .A
    rti


;***********************************************************************************;
;
; baud rate word is calculated from ..
;
; (system clock / baud rate) / 2 - 100
;
;       system clock
;       ------------
; PAL       1108404 Hz
; NTSC      1022727 Hz

; baud rate tables for PAL VIC 20

BAUDTBL:
.ifdef PAL
    .word $2AE6 ;   50   baud
    .word $1C78 ;   75   baud
    .word $1349 ;  110   baud
    .word $0FB1 ;  134.5 baud
    .word $0E0A ;  150   baud
    .word $06D3 ;  300   baud
    .word $0338 ;  600   baud
    .word $016A ; 1200   baud
    .word $00D0 ; 1800   baud
    .word $0083 ; 2400   baud
    .word $0036 ; 3600   baud
.endif

; baud rate tables for NTSC VIC 20

.ifdef NTSC
    .word $2792 ;   50   baud
    .word $1A40 ;   75   baud
    .word $11C6 ;  110   baud
    .word $0E74 ;  134.5 baud
    .word $0CEE ;  150   baud
    .word $0645 ;  300   baud
    .word $02F1 ;  600   baud
    .word $0146 ; 1200   baud
    .word $00B8 ; 1800   baud
    .word $0071 ; 2400   baud
    .word $002A ; 3600   baud
.endif


;***********************************************************************************;
;
; IRQ vector

IRQROUT:
    pha             ; save .A
    txa             ; copy .X
    pha             ; save .X
    tya             ; copy .Y
    pha             ; save .Y
    tsx             ; copy stack pointer
    lda STACK+4,X   ; get the stacked status register
    and #$10        ; mask the BRK flag bit
    beq LAB_FF82    ; if not BRK go do the hardware IRQ vector

    jmp (CBINV)     ; else do the BRK vector (iBRK)

LAB_FF82:
    jmp (CINV)      ; do IRQ vector (iIRQ)


;***********************************************************************************;
;
; spare bytes, not referenced

    .byte $FF,$FF,$FF,$FF,$FF


;***********************************************************************************;
;
; restore default I/O vectors

; This routine restores the default values of all system vectors used in KERNAL and
; BASIC routines and interrupts. The KERNAL VECTOR routine is used to read and alter
; individual system vectors.


RESTOR:
    jmp FRESTOR     ; restore default I/O vectors


;***********************************************************************************;
;
; read/set vectored I/O

; This routine manages all system vector jump addresses stored in RAM. Calling this
; routine with the accumulator carry bit set will store the current contents of the
; RAM vectors in a list pointed to by the .X and .Y registers.

; When this routine is called with the carry bit clear, the user list pointed to by
; the .X and .Y registers is transferred to the system RAM vectors.

; NOTE: This routine requires caution in its use. The best way to use it is to first
; read the entire vector contents into the user area, alter the desired vectors, and
; then copy the contents back to the system vectors.

VECTOR:
    jmp FVECTOR    ; set/read vectored I/O from (.X.Y)


;***********************************************************************************;
;
; control KERNAL messages

; This routine controls the printing of error and control messages by the KERNAL.
; Either print error messages or print control messages can be selected by setting
; the accumulator when the routine is called.

; FILE NOT FOUND is an example of an error message. PRESS plaY ON CASSETTE is an
; example of a control message.

; Bits 6 and 7 of this value determine where the message will come from. If bit 7
; is set one of the error messages from the KERNAL will be printed. If bit 6 is set
; a control message will be printed.

SETMSG:
    jmp FSETMSG    ; control KERNAL messages


;***********************************************************************************;
;
; send secondary address after LISTEN

; This routine is used to send a secondary address to an I/O device after a call to
; the LISTEN routine is made and the device commanded to LISTEN. The routine cannot
; be used to send a secondary address after a call to the TALK routine.

; A secondary address is usually used to give set-up information to a device before
; I/O operations begin.

; When a secondary address is to be sent to a device on the serial bus the address
; must first be ORed with $60.

SECOND:
    jmp FSECOND    ; send secondary address after LISTEN


;***********************************************************************************;
;
; send secondary address after TALK

; This routine transmits a secondary address on the serial bus for a TALK device.
; This routine must be called with a number between 4 and 31 in the accumulator.
; The routine will send this number as a secondary address command over the serial
; bus. This routine can only be called after a call to the TALK routine. It will
; not work after a LISTEN.

TKSA:
    jmp FTKSA      ; send secondary address after TALK


;***********************************************************************************;
;
; read/set the top of memory

; This routine is used to read and set the top of RAM. When this routine is called
; with the carry bit set the pointer to the top of RAM will be loaded into .X.Y. When
; this routine is called with the carry bit clear .X.Y will be saved as the top of
; memory pointer changing the top of memory.

MEMTOP:
    jmp FMEMTOP    ; read/set the top of memory


;***********************************************************************************;
;
; read/set the bottom of memory

; This routine is used to read and set the bottom of RAM. When this routine is
; called with the carry bit set the pointer to the bottom of RAM will be loaded
; into .X.Y. When this routine is called with the carry bit clear .X.Y will be saved
; as the bottom of memory pointer changing the bottom of memory.

MEMBOT:
    jmp FMEMBOT     ; read/set the bottom of memory


;***********************************************************************************;
;
; scan the keyboard

; This routine will scan the keyboard and check for pressed keys. It is the same
; routine called by the interrupt handler. If a key is down, its ASCII value is
; placed in the keyboard queue.

SCNKEY:
    jmp FSCNKEY    ; scan keyboard


;***********************************************************************************;
;
; set timeout on IEEE-488 bus

; This routine sets the timeout flag for the serial bus. When the timeout flag is
; set, the computer will wait for a device on the serial bus for 64 milliseconds.
; If the device does not respond to the computer's DAV signal within that time the
; computer will recognize an error condition and leave the handshake sequence. When
; this routine is called and the accumulator contains a 0 in bit 7, timeouts are
; enabled. A 1 in bit 7 will disable the timeouts.

; NOTE: The timeout feature is used to communicate that a disk file is not found on
; an attempt to OPEN a file.

SETTMO:
    jmp FSETTMO    ; set timeout on serial bus


;************************************************************************************
;
; input a byte from the serial bus

; This routine reads a byte of data from the serial bus using full handshaking. The
; data is returned in the accumulator. Before using this routine the TALK routine
; must have been called first to command the device on the serial bus to send data on
; the bus. If the input device needs a secondary command it must be sent by using the
; TKSA routine before calling this routine.

; Errors are returned in the status word which can be read by calling the READST
; routine.

ACPTR:
    jmp FACPTR     ; input byte from serial bus


;************************************************************************************
;
; output a byte to the serial bus

; This routine is used to send information to devices on the serial bus. A call to
; this routine will put a data byte onto the serial bus using full handshaking.
; Before this routine is called the LISTEN routine must be used to command a device
; on the serial bus to get ready to receive data.

; The accumulator is loaded with a byte to output as data on the serial bus. A
; device must be listening or the status word will return a timeout. This routine
; always buffers one character. So when a call to the UNLSN routine is made to end
; the data transmission, the buffered character is sent with EOI set. The UNLISTEN
; command is sent to the device.

CIOUT:
    jmp FCIOUT     ; output a byte to the serial bus


;***********************************************************************************;
;
; command the serial bus to UNTALK

; This routine will transmit an UNTALK command on the serial bus. All devices
; previously set to TALK will stop sending data when this command is received.

UNTLK:
    jmp FUNTLK     ; command the serial bus to UNTALK


;***********************************************************************************;
;
; command the serial bus to UNLISTEN

; This routine commands all devices on the serial bus to stop receiving data from
; the computer. Calling this routine results in an UNLISTEN command being transmitted
; on the serial bus. Only devices previously commanded to listen will be affected.

; This routine is normally used after the computer is finished sending data to
; external devices. Sending the UNLISTEN will command the listening devices to get
; off the serial bus so it can be used for other purposes.

UNLSN:
    jmp FUNLSN     ; command the serial bus to UNLISTEN


;***********************************************************************************;
;
; command devices on the serial bus to LISTEN

; This routine will command a device on the serial bus to receive data. The
; accumulator must be loaded with a device number between 4 and 31 before calling
; this routine. LISTEN convert this to a listen address then transmit this data as
; a command on the serial bus. The specified device will then go into listen mode
; and be ready to accept information.

LISTEN:
    jmp FLISTEN    ; command devices on the serial bus to LISTEN


;***********************************************************************************;
;
; command a serial bus device to TALK

; To use this routine the accumulator must first be loaded with a device number
; between 4 and 30. When called this routine converts this device number to a talk
; address. Then this data is transmitted as a command on the serial bus.

TALK:
    jmp FTALK      ; command serial bus device to TALK


;***********************************************************************************;
;
; read I/O status word

; This routine returns the current status of the I/O device in the accumulator. The
; routine is usually called after new communication to an I/O device. The routine
; will give information about device status, or errors that have occurred during the
; I/O operation.

READST:
    jmp FREADST    ; read I/O status word


;***********************************************************************************;
;
; set logical, first and second addresses

; This routine will set the logical file number, device address, and secondary
; address, command number, for other KERNAL routines.

; The logical file number is used by the system as a key to the file table created
; by the OPEN file routine. Device addresses can range from 0 to 30. The following
; codes are used by the computer to stand for the following devices:

; ADDRESS   DEVICE
; =======   ======
;  0        Keyboard
;  1        Cassette
;  2        RS-232
;  3        CRT display
;  4        Serial bus printer
;  8        Serial bus disk drive

; device numbers of four or greater automatically refer to devices on the serial
; bus.

; A command to the device is sent as a secondary address on the serial bus after
; the device number is sent during the serial attention handshaking sequence. If
; no secondary address is to be sent .Y should be set to $FF.

SETLFS:
    jmp FSETLFS    ; set logical, first and second addresses


;***********************************************************************************;
;
; set the filename

; This routine is used to set up the file name for the OPEN, SAVE, or LOAD routines.
; The accumulator must be loaded with the length of the file and .X.Y with the pointer
; to file name, .X being the low byte. The address can be any valid memory address in
; the system where a string of characters for the file name is stored. If no file
; name desired the accumulator must be set to 0, representing a zero file length,
; in that case .X.Y may be set to any memory address.

SETNAM:
    jmp FSETNAM    ; set filename


;***********************************************************************************;
;
; open a logical file

; This routine is used to open a logical file. Once the logical file is set up it
; can be used for input/output operations. Most of the I/O KERNAL routines call on
; this routine to create the logical files to operate on. No arguments need to be
; set up to use this routine, but both the SETLFS and SETNAM KERNAL routines must
; be called before using this routine.

OPEN:
    jmp (IOPEN)    ; do open file vector


;***********************************************************************************;
;
; close a specified logical file

; This routine is used to close a logical file after all I/O operations have been
; completed on that file. This routine is called after the accumulator is loaded
; with the logical file number to be closed, the same number used when the file was
; opened using the OPEN routine.

CLOSE:
    jmp (ICLOSE)   ; do close file vector


;************************************************************************************
;
; open a channel for input

; Any logical file that has already been opened by the OPEN routine can be defined as
; an input channel by this routine. the device on the channel must be an input device
; or an error will occur and the routine will abort.

; If you are getting data from anywhere other than the keyboard, this routine must be
; called before using either the CHRIN routine or the GETIN routine. If you are
; getting data from the keyboard and no other input channels are open then the calls to
; this routine and to the OPEN routine are not needed.

; When used with a device on the serial bus this routine will automatically send the
; listen address specified by the OPEN routine and any secondary address.

; Possible errors are:
;
;   3 : file not open
;   5 : device not present
;   6 : file is not an input file

CHKIN:
    jmp (ICHKIN)   ; do open for input vector


;************************************************************************************
;
; open a channel for output

; Any logical file that has already been opened by the OPEN routine can be defined as
; an output channel by this routine the device on the channel must be an output device
; or an error will occur and the routine will abort.

; If you are sending data to anywhere other than the screen this routine must be
; called before using the CHROUT routine. if you are sending data to the screen and no
; other output channels are open then the calls to this routine and to the OPEN routine
; are not needed.

; When used with a device on the serial bus this routine will automatically send the
; listen address specified by the OPEN routine and any secondary address.

; Possible errors are:
;
;   3 : file not open
;   5 : device not present
;   7 : file is not an output file

CHKOUT:
    jmp (ICKOUT)   ; do open for output vector


;************************************************************************************
;
; close input and output channels

; This routine is called to clear all open channels and restore the I/O channels to
; their original default values. It is usually called after opening other I/O
; channels and using them for input/output operations. The default input device is
; 0, the keyboard. The default output device is 3, the screen.

; If one of the channels to be closed is to the serial bus, an UNTALK signal is sent
; first to clear the input channel or an UNLISTEN is sent to clear the output channel.
; By not calling this routine and leaving listener(s) active on the serial bus,
; several devices can receive the same data from the VIC at the same time. One way to
; take advantage of this would be to command the printer to LISTEN and the disk to
; TALK. This would allow direct printing of a disk file.

CLRCHN:
    jmp (ICLRCN)   ; do close vector


;************************************************************************************
;
; input character from channel

; This routine will get a byte of data from the channel already set up as the input
; channel by the CHKIN routine.

; If CHKIN has not been used to define another input channel the data is expected to
; be from the keyboard. the data byte is returned in the accumulator. The channel
; remains open after the call.

; Input from the keyboard is handled in a special way. first, the cursor is turned on
; and it will blink until a carriage return is typed on the keyboard. All characters
; on the logical line, up to 88 characters, will be stored in the BASIC input buffer.
; then the characters can be returned one at a time by calling this routine once for
; each character. When the carriage return is returned the entire line has been
; processed. the next time this routine is called the whole process begins again.

CHRIN:
    jmp (IBASIN)   ; do input vector


;************************************************************************************
;
; output a character to channel

; This routine will output a character to an already opened channel. Use the OPEN
; routine, OPEN, and the CHKOUT routine to set up the output channel before calling
; this routine. If these calls are omitted, data will be sent to the default output
; device, device 3, the screen. The data byte to be output is loaded into the
; accumulator, and this routine is called. The data is then sent to the specified
; output device. The channel is left open after the call.

; NOTE: Care must be taken when using routine to send data to a serial device since
; data will be sent to all open output channels on the bus. Unless this is desired,
; all open output channels on the serial bus other than the actually intended
; destination channel must be closed by a call to the KERNAL close channel routine.

CHROUT:
    jmp (IBSOUT)   ; do output vector


;***********************************************************************************;
;
; load RAM from a device

; This routine will load data bytes from any input device directly into the memory
; of the computer.  It can also be used for a verify operation comparing data from a
; device with the data already in memory, leaving the data stored in RAM unchanged.

; The accumulator must be set to 0 for a load operation or 1 for a verify.  If the
; input device was OPENed with a secondary address of 0 the header information from
; device will be ignored.  In this case .X.Y must contain the starting address for the
; load. If the device was addressed with a secondary address other than 0 the data will
; load into memory starting at the location specified by the header.  This routine
; returns the address after the last byte loaded.

; Before this routine can be called, the SETLFS and SETNAM routines must be called.

LOAD:
    jmp FLOAD      ; load RAM from a device


;***********************************************************************************;
;
; save RAM to a device

; This routine saves a section of memory. Memory is saved from an indirect address
; on page 0 specified by .A, to the address stored in .X.Y, to a logical file. The
; SETLFS and SETNAM routines must be used before calling this routine. However, a
; file name is not required to SAVE to device 1, the cassette. Any attempt to save to
; other devices without using a file name results in an error.

; NOTE: device 0, the keyboard, and device 3, the screen, cannot be SAVEd to. If
; the attempt is made, an error will occur, and the SAVE stopped.

SAVE:
    jmp FSAVE      ; save RAM to device


;***********************************************************************************;
;
; set the real time clock

; The system clock is maintained by an interrupt routine that updates the clock
; every 1/60th of a second. The clock is three bytes long which gives the capability
; to count from zero up to 5,184,000 jiffies - 24 hours plus one jiffy. At that point
; the clock resets to zero. Before calling this routine to set the clock the new time,
; in jiffies, should be in .Y.X.A, the accumulator containing the most significant byte.

SETTIM:
    jmp FSETTIM    ; set real time clock


;***********************************************************************************;
;
; read the real time clock

; This routine returns the time, in jiffies, in .A.X.Y. The accumulator contains the
; most significant byte.

RDTIM:
    jmp FRDTIM     ; read real time clock


;***********************************************************************************;
;
; scan the stop key

; If the STOP key on the keyboard is pressed when this routine is called the Z flag
; will be set. All other flags remain unchanged. If the STOP key is not pressed then
; the accumulator will contain a byte representing the last row of the keyboard scan.

; The user can also check for certain other keys this way.

STOP:
    jmp (ISTOP)    ; do stop key vector


;***********************************************************************************;
;
; get a character from an input device

; In practice this routine operates identically to the CHRIN routine for all devices
; except for the keyboard. If the keyboard is the current input device this routine
; will get one character from the keyboard buffer. It depends on the IRQ routine to
; read the keyboard and put characters into the buffer.

; If the keyboard buffer is empty the value returned in the accumulator will be zero

GETIN:
    jmp (IGETIN)   ; do get vector


;***********************************************************************************;
;
; close all channels and files

; This routine closes all open files. When this routine is called, the pointers into
; the open file table are reset, closing all files. Also the routine automatically
; resets the I/O channels.

CLALL:
    jmp (ICLALL)   ; do close all vector


;***********************************************************************************;
;
; increment the real time clock

; This routine updates the system clock. Normally this routine is called by the
; normal KERNAL interrupt routine every 1/60th of a second. If the user program
; processes its own interrupts this routine must be called to update the time. Also,
; the STOP key routine must be called if the stop key is to remain functional.

UDTIM:
    jmp FUDTIM     ; increment real time clock


;***********************************************************************************;
;
; return X,Y organisation of screen

; this routine returns the x,y organisation of the screen in .X,.Y

SCREEN:
    jmp FSCREEN    ; return X,Y organisation of screen


;***********************************************************************************;
;
; read/set X,Y cursor position

; This routine, when called with the carry flag set, loads the current position of
; the cursor on the screen into the .X and .Y registers. .X is the column number of
; the cursor location and .Y is the row number of the cursor. A call with the carry
; bit clear moves the cursor to the position determined by the .X and .Y registers.

PLOT:
    jmp FPLOT      ; read/set X,Y cursor position


;***********************************************************************************;
;
; return the base address of the I/O devices

; This routine will set .X.Y to the address of the memory section where the memory
; mapped I/O devices are located. This address can then be used with an offset to
; access the memory mapped I/O devices in the computer.

IOBASE:
    jmp FIOBASE    ; return base address of I/O devices


;***********************************************************************************;
;
; spare bytes, not referenced

    .byte $FF,$FF,$FF,$FF


;***********************************************************************************;
;
; hardware vectors

    .word NMI       ; NMI vector
    .word START     ; RESET vector
    .word IRQROUT   ; IRQ vector

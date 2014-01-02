let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
imap <S-Tab> <Plug>SuperTabBackward
inoremap <silent> <Plug>delimitMateS-Tab =delimitMate#JumpAny()
inoremap <silent> <Plug>delimitMateSpace =delimitMate#ExpandSpace()
inoremap <silent> <Plug>delimitMateCR =delimitMate#ExpandReturn()
inoremap <silent> <expr> <Plug>delimitMateS-BS delimitMate#WithinEmptyPair() ? "\<Del>" : "\<S-BS>"
inoremap <silent> <Plug>delimitMateBS =delimitMate#BS()
inoremap <silent> <Plug>delimitMate` =delimitMate#QuoteDelim("\`")
inoremap <silent> <Plug>delimitMate' =delimitMate#QuoteDelim("\'")
inoremap <silent> <Plug>delimitMate" =delimitMate#QuoteDelim("\"")
inoremap <silent> <Plug>delimitMate] =delimitMate#JumpOut("\]")
inoremap <silent> <Plug>delimitMate} =delimitMate#JumpOut("\}")
inoremap <silent> <Plug>delimitMate) =delimitMate#JumpOut("\)")
inoremap <silent> <Plug>delimitMate[ =delimitMate#ParenDelim("]")
inoremap <silent> <Plug>delimitMate{ =delimitMate#ParenDelim("}")
inoremap <silent> <Plug>delimitMate( =delimitMate#ParenDelim(")")
inoremap <silent> <C-Tab> =UltiSnips_ListSnippets()
inoremap <silent> <F4> =UltiSnips_ExpandSnippet()
lnoremap <F1> 
noremap! <F1> 
map! <S-Insert> *
vmap  <Plug>SequenceAdd
nnoremap  ggVG
onoremap  ggVG
map  "+y
snoremap <silent>  c
nnoremap  h
snoremap <silent> <NL> :call UltiSnips_JumpForwards()
nnoremap <NL> j
snoremap <silent>  :call UltiSnips_JumpBackwards()
nnoremap  k
nnoremap  l
nmap  o
nnoremap <silent>  :call NumberToggle()
nnoremap <silent>  :CtrlP
map  :update
vmap  "+P
nmap  "+gP
omap  "+gP
vmap  <Plug>SequenceSubtract
map  
nnoremap Oc :tabN
nnoremap Od :tabp
nmap ,, :emenu Slimv.	
xmap ,, :emenu Slimv.	
omap ,, :emenu Slimv.	
nnoremap <silent> ,- :call SlimvClearReplBuffer()
xnoremap <silent> ,- :call SlimvClearReplBuffer()
onoremap <silent> ,- :call SlimvClearReplBuffer()
nnoremap <silent> ,y :call SlimvInterrupt()
xnoremap <silent> ,y :call SlimvInterrupt()
onoremap <silent> ,y :call SlimvInterrupt()
nnoremap <silent> ,g :call SlimvSetPackage()
xnoremap <silent> ,g :call SlimvSetPackage()
onoremap <silent> ,g :call SlimvSetPackage()
nnoremap <silent> ,c :call SlimvConnectServer()
xnoremap <silent> ,c :call SlimvConnectServer()
onoremap <silent> ,c :call SlimvConnectServer()
nnoremap <silent> ,] :call SlimvGenerateTags()
xnoremap <silent> ,] :call SlimvGenerateTags()
onoremap <silent> ,] :call SlimvGenerateTags()
nnoremap <silent> ,h :call SlimvHyperspec()
xnoremap <silent> ,h :call SlimvHyperspec()
onoremap <silent> ,h :call SlimvHyperspec()
nnoremap <silent> ,A :call SlimvApropos()
xnoremap <silent> ,A :call SlimvApropos()
onoremap <silent> ,A :call SlimvApropos()
nnoremap <silent> ,s :call SlimvDescribeSymbol()
xnoremap <silent> ,s :call SlimvDescribeSymbol()
onoremap <silent> ,s :call SlimvDescribeSymbol()
nnoremap <silent> ,X :call SlimvProfileReset()
xnoremap <silent> ,X :call SlimvProfileReset()
onoremap <silent> ,X :call SlimvProfileReset()
nnoremap <silent> ,o :call SlimvProfileReport()
xnoremap <silent> ,o :call SlimvProfileReport()
onoremap <silent> ,o :call SlimvProfileReport()
nnoremap <silent> ,? :call SlimvShowProfiled()
xnoremap <silent> ,? :call SlimvShowProfiled()
onoremap <silent> ,? :call SlimvShowProfiled()
nnoremap <silent> ,U :call SlimvUnprofileAll()
xnoremap <silent> ,U :call SlimvUnprofileAll()
onoremap <silent> ,U :call SlimvUnprofileAll()
nnoremap <silent> ,P :call SlimvProfileSubstring()
xnoremap <silent> ,P :call SlimvProfileSubstring()
onoremap <silent> ,P :call SlimvProfileSubstring()
nnoremap <silent> ,p :call SlimvProfile()
xnoremap <silent> ,p :call SlimvProfile()
onoremap <silent> ,p :call SlimvProfile()
nnoremap <silent> ,xe :call SlimvXrefCallees()
xnoremap <silent> ,xe :call SlimvXrefCallees()
onoremap <silent> ,xe :call SlimvXrefCallees()
nnoremap <silent> ,xl :call SlimvXrefCallers()
xnoremap <silent> ,xl :call SlimvXrefCallers()
onoremap <silent> ,xl :call SlimvXrefCallers()
nnoremap <silent> ,xp :call SlimvXrefSpecializes()
xnoremap <silent> ,xp :call SlimvXrefSpecializes()
onoremap <silent> ,xp :call SlimvXrefSpecializes()
nnoremap <silent> ,xm :call SlimvXrefMacroexpands()
xnoremap <silent> ,xm :call SlimvXrefMacroexpands()
onoremap <silent> ,xm :call SlimvXrefMacroexpands()
nnoremap <silent> ,xb :call SlimvXrefBinds()
xnoremap <silent> ,xb :call SlimvXrefBinds()
onoremap <silent> ,xb :call SlimvXrefBinds()
nnoremap <silent> ,xs :call SlimvXrefSets()
xnoremap <silent> ,xs :call SlimvXrefSets()
onoremap <silent> ,xs :call SlimvXrefSets()
nnoremap <silent> ,xr :call SlimvXrefReferences()
xnoremap <silent> ,xr :call SlimvXrefReferences()
onoremap <silent> ,xr :call SlimvXrefReferences()
nnoremap <silent> ,xc :call SlimvXrefCalls()
xnoremap <silent> ,xc :call SlimvXrefCalls()
onoremap <silent> ,xc :call SlimvXrefCalls()
nnoremap <silent> ,R :call SlimvCompileRegion()
xnoremap <silent> ,R :call SlimvCompileRegion()
onoremap <silent> ,R :call SlimvCompileRegion()
nnoremap <silent> ,F :call SlimvCompileFile()
xnoremap <silent> ,F :call SlimvCompileFile()
onoremap <silent> ,F :call SlimvCompileFile()
nnoremap <silent> ,L :call SlimvCompileLoadFile()
xnoremap <silent> ,L :call SlimvCompileLoadFile()
onoremap <silent> ,L :call SlimvCompileLoadFile()
nnoremap <silent> ,G :call SlimvDebugThread()
xnoremap <silent> ,G :call SlimvDebugThread()
onoremap <silent> ,G :call SlimvDebugThread()
nnoremap <silent> ,K :call SlimvKillThread()
xnoremap <silent> ,K :call SlimvKillThread()
onoremap <silent> ,K :call SlimvKillThread()
nnoremap <silent> ,H :call SlimvListThreads()
xnoremap <silent> ,H :call SlimvListThreads()
onoremap <silent> ,H :call SlimvListThreads()
nnoremap <silent> ,N :call SlimvDebugRestartFrame()
xnoremap <silent> ,N :call SlimvDebugRestartFrame()
onoremap <silent> ,N :call SlimvDebugRestartFrame()
nnoremap <silent> ,n :call SlimvDebugContinue()
xnoremap <silent> ,n :call SlimvDebugContinue()
onoremap <silent> ,n :call SlimvDebugContinue()
nnoremap <silent> ,q :call SlimvDebugQuit()
xnoremap <silent> ,q :call SlimvDebugQuit()
onoremap <silent> ,q :call SlimvDebugQuit()
nnoremap <silent> ,a :call SlimvDebugAbort()
xnoremap <silent> ,a :call SlimvDebugAbort()
onoremap <silent> ,a :call SlimvDebugAbort()
nnoremap <silent> ,l :call SlimvDisassemble()
xnoremap <silent> ,l :call SlimvDisassemble()
onoremap <silent> ,l :call SlimvDisassemble()
nnoremap <silent> ,E :call SlimvBreakOnException()
xnoremap <silent> ,E :call SlimvBreakOnException()
onoremap <silent> ,E :call SlimvBreakOnException()
nnoremap <silent> ,B :call SlimvBreak()
xnoremap <silent> ,B :call SlimvBreak()
onoremap <silent> ,B :call SlimvBreak()
nnoremap <silent> ,T :call SlimvUntrace()
xnoremap <silent> ,T :call SlimvUntrace()
onoremap <silent> ,T :call SlimvUntrace()
nnoremap <silent> ,t :call SlimvTrace()
xnoremap <silent> ,t :call SlimvTrace()
onoremap <silent> ,t :call SlimvTrace()
nnoremap <silent> ,m :call SlimvMacroexpandAll()
xnoremap <silent> ,m :call SlimvMacroexpandAll()
onoremap <silent> ,m :call SlimvMacroexpandAll()
nnoremap <silent> ,1 :call SlimvMacroexpand()
xnoremap <silent> ,1 :call SlimvMacroexpand()
onoremap <silent> ,1 :call SlimvMacroexpand()
nnoremap <silent> ,u :call SlimvUndefineFunction()
xnoremap <silent> ,u :call SlimvUndefineFunction()
onoremap <silent> ,u :call SlimvUndefineFunction()
nnoremap <silent> ,v :call SlimvInteractiveEval()
xnoremap <silent> ,v :call SlimvInteractiveEval()
onoremap <silent> ,v :call SlimvInteractiveEval()
nnoremap <silent> ,b :call SlimvEvalBuffer()
xnoremap <silent> ,b :call SlimvEvalBuffer()
onoremap <silent> ,b :call SlimvEvalBuffer()
nnoremap <silent> ,r :call SlimvEvalRegion()
xnoremap <silent> ,r :call SlimvEvalRegion()
onoremap <silent> ,r :call SlimvEvalRegion()
nnoremap <silent> ,e :call SlimvEvalExp()
xnoremap <silent> ,e :call SlimvEvalExp()
onoremap <silent> ,e :call SlimvEvalExp()
nnoremap <silent> ,( :call PareditToggle()
xnoremap <silent> ,( :call PareditToggle()
onoremap <silent> ,( :call PareditToggle()
nnoremap <silent> ,) :call SlimvCloseForm()
xnoremap <silent> ,) :call SlimvCloseForm()
onoremap <silent> ,) :call SlimvCloseForm()
nnoremap <silent> ,D :call SlimvCompileDefun()
xnoremap <silent> ,D :call SlimvCompileDefun()
onoremap <silent> ,D :call SlimvCompileDefun()
xmap ,I <Plug>SequenceAdd
nnoremap <silent> ,d :call SlimvEvalDefun()
xnoremap <silent> ,d :call SlimvEvalDefun()
onoremap <silent> ,d :call SlimvEvalDefun()
nnoremap <silent> ,i :call SlimvInspect()
xnoremap <silent> ,i :call SlimvInspect()
onoremap <silent> ,i :call SlimvInspect()
nmap ,* :call CommentLinePincer('/* ', ' */')+
xmap ,* :call CommentLinePincer('/* ', ' */')+
omap ,* :call CommentLinePincer('/* ', ' */')+
nmap ,/ :call CommentLineToEnd('// ')+
xmap ,/ :call CommentLineToEnd('// ')+
omap ,/ :call CommentLineToEnd('// ')+
nmap ,# :call CommentLineToEnd('#')+
xmap ,# :call CommentLineToEnd('#')+
omap ,# :call CommentLineToEnd('#')+
xmap P p
nnoremap Q 
xmap S <Plug>VSurround
xmap [% [%m'gv``
nmap [q :cprev
xmap [q :cprev
omap [q :cprev
nmap \vl <Plug>VLToggle
xmap <silent> \x <Plug>VisualTraditional
xmap <silent> \c <Plug>VisualTraditionalj
nmap <silent> \x <Plug>Traditional
nmap <silent> \c <Plug>Traditionalj
nmap <silent> \slr :DBListVar
xmap <silent> \sa :DBVarRangeAssign
nmap <silent> \sap :'<,'>DBVarRangeAssign
nmap <silent> \sal :.,.DBVarRangeAssign
nmap <silent> \sas :1,$DBVarRangeAssign
nmap \so <Plug>DBOrientationToggle
nmap \sh <Plug>DBHistory
nmap \slv <Plug>DBListView
nmap \slp <Plug>DBListProcedure
nmap \slt <Plug>DBListTable
xmap <silent> \slc :exec 'DBListColumn "'.DB_getVisualBlock().'"'
nmap \slc <Plug>DBListColumn
nmap \sbp <Plug>DBPromptForBufferParameters
nmap \sdpa <Plug>DBDescribeProcedureAskName
xmap <silent> \sdp :exec 'DBDescribeProcedure "'.DB_getVisualBlock().'"'
nmap \sdp <Plug>DBDescribeProcedure
nmap \sdta <Plug>DBDescribeTableAskName
xmap <silent> \sdt :exec 'DBDescribeTable "'.DB_getVisualBlock().'"'
nmap \sdt <Plug>DBDescribeTable
xmap <silent> \sT :exec 'DBSelectFromTableTopX "'.DB_getVisualBlock().'"'
nmap \sT <Plug>DBSelectFromTopXTable
nmap \sta <Plug>DBSelectFromTableAskName
nmap \stw <Plug>DBSelectFromTableWithWhere
xmap <silent> \st :exec 'DBSelectFromTable "'.DB_getVisualBlock().'"'
nmap \st <Plug>DBSelectFromTable
nmap <silent> \sep :'<,'>DBExecRangeSQL
nmap <silent> \sel :.,.DBExecRangeSQL
nmap <silent> \sea :1,$DBExecRangeSQL
nmap \sE <Plug>DBExecSQLUnderTopXCursor
nmap \se <Plug>DBExecSQLUnderCursor
xmap \sE <Plug>DBExecVisualTopXSQL
xmap \se <Plug>DBExecVisualSQL
nmap \rwp <Plug>RestoreWinPosn
xmap \rwp <Plug>RestoreWinPosn
omap \rwp <Plug>RestoreWinPosn
nmap \swp <Plug>SaveWinPosn
xmap \swp <Plug>SaveWinPosn
omap \swp <Plug>SaveWinPosn
nmap \tt <Plug>AM_tt
xmap \tt <Plug>AM_tt
omap \tt <Plug>AM_tt
nmap \tsq <Plug>AM_tsq
xmap \tsq <Plug>AM_tsq
omap \tsq <Plug>AM_tsq
nmap \tsp <Plug>AM_tsp
xmap \tsp <Plug>AM_tsp
omap \tsp <Plug>AM_tsp
nmap \tml <Plug>AM_tml
xmap \tml <Plug>AM_tml
omap \tml <Plug>AM_tml
nmap \tab <Plug>AM_tab
xmap \tab <Plug>AM_tab
omap \tab <Plug>AM_tab
nmap \m= <Plug>AM_m=
xmap \m= <Plug>AM_m=
omap \m= <Plug>AM_m=
nmap \t@ <Plug>AM_t@
xmap \t@ <Plug>AM_t@
omap \t@ <Plug>AM_t@
nmap \t~ <Plug>AM_t~
xmap \t~ <Plug>AM_t~
omap \t~ <Plug>AM_t~
nmap \t? <Plug>AM_t?
xmap \t? <Plug>AM_t?
omap \t? <Plug>AM_t?
nmap \w= <Plug>AM_w=
xmap \w= <Plug>AM_w=
omap \w= <Plug>AM_w=
nmap \ts= <Plug>AM_ts=
xmap \ts= <Plug>AM_ts=
omap \ts= <Plug>AM_ts=
nmap \ts< <Plug>AM_ts<
xmap \ts< <Plug>AM_ts<
omap \ts< <Plug>AM_ts<
nmap \ts; <Plug>AM_ts;
xmap \ts; <Plug>AM_ts;
omap \ts; <Plug>AM_ts;
nmap \ts: <Plug>AM_ts:
xmap \ts: <Plug>AM_ts:
omap \ts: <Plug>AM_ts:
nmap \ts, <Plug>AM_ts,
xmap \ts, <Plug>AM_ts,
omap \ts, <Plug>AM_ts,
nmap \t= <Plug>AM_t=
xmap \t= <Plug>AM_t=
omap \t= <Plug>AM_t=
nmap \t< <Plug>AM_t<
xmap \t< <Plug>AM_t<
omap \t< <Plug>AM_t<
nmap \t; <Plug>AM_t;
xmap \t; <Plug>AM_t;
omap \t; <Plug>AM_t;
nmap \t: <Plug>AM_t:
xmap \t: <Plug>AM_t:
omap \t: <Plug>AM_t:
nmap \t, <Plug>AM_t,
xmap \t, <Plug>AM_t,
omap \t, <Plug>AM_t,
nmap \t# <Plug>AM_t#
xmap \t# <Plug>AM_t#
omap \t# <Plug>AM_t#
nmap \t| <Plug>AM_t|
xmap \t| <Plug>AM_t|
omap \t| <Plug>AM_t|
nmap \T~ <Plug>AM_T~
xmap \T~ <Plug>AM_T~
omap \T~ <Plug>AM_T~
nmap \Tsp <Plug>AM_Tsp
xmap \Tsp <Plug>AM_Tsp
omap \Tsp <Plug>AM_Tsp
nmap \Tab <Plug>AM_Tab
xmap \Tab <Plug>AM_Tab
omap \Tab <Plug>AM_Tab
nmap \T@ <Plug>AM_T@
xmap \T@ <Plug>AM_T@
omap \T@ <Plug>AM_T@
nmap \T? <Plug>AM_T?
xmap \T? <Plug>AM_T?
omap \T? <Plug>AM_T?
nmap \T= <Plug>AM_T=
xmap \T= <Plug>AM_T=
omap \T= <Plug>AM_T=
nmap \T< <Plug>AM_T<
xmap \T< <Plug>AM_T<
omap \T< <Plug>AM_T<
nmap \T; <Plug>AM_T;
xmap \T; <Plug>AM_T;
omap \T; <Plug>AM_T;
nmap \T: <Plug>AM_T:
xmap \T: <Plug>AM_T:
omap \T: <Plug>AM_T:
nmap \Ts, <Plug>AM_Ts,
xmap \Ts, <Plug>AM_Ts,
omap \Ts, <Plug>AM_Ts,
nmap \T, <Plug>AM_T,o
xmap \T, <Plug>AM_T,o
omap \T, <Plug>AM_T,o
nmap \T# <Plug>AM_T#
xmap \T# <Plug>AM_T#
omap \T# <Plug>AM_T#
nmap \T| <Plug>AM_T|
xmap \T| <Plug>AM_T|
omap \T| <Plug>AM_T|
nmap \Htd <Plug>AM_Htd
xmap \Htd <Plug>AM_Htd
omap \Htd <Plug>AM_Htd
nmap \anum <Plug>AM_aunum
xmap \anum <Plug>AM_aunum
omap \anum <Plug>AM_aunum
nmap \aunum <Plug>AM_aenum
xmap \aunum <Plug>AM_aenum
omap \aunum <Plug>AM_aenum
nmap \afnc <Plug>AM_afnc
xmap \afnc <Plug>AM_afnc
omap \afnc <Plug>AM_afnc
nmap \adef <Plug>AM_adef
xmap \adef <Plug>AM_adef
omap \adef <Plug>AM_adef
nmap \adec <Plug>AM_adec
xmap \adec <Plug>AM_adec
omap \adec <Plug>AM_adec
nmap \ascom <Plug>AM_ascom
xmap \ascom <Plug>AM_ascom
omap \ascom <Plug>AM_ascom
nmap \aocom <Plug>AM_aocom
xmap \aocom <Plug>AM_aocom
omap \aocom <Plug>AM_aocom
nmap \adcom <Plug>AM_adcom
xmap \adcom <Plug>AM_adcom
omap \adcom <Plug>AM_adcom
nmap \acom <Plug>AM_acom
xmap \acom <Plug>AM_acom
omap \acom <Plug>AM_acom
nmap \abox <Plug>AM_abox
xmap \abox <Plug>AM_abox
omap \abox <Plug>AM_abox
nmap \a( <Plug>AM_a(
xmap \a( <Plug>AM_a(
omap \a( <Plug>AM_a(
nmap \a= <Plug>AM_a=
xmap \a= <Plug>AM_a=
omap \a= <Plug>AM_a=
nmap \a< <Plug>AM_a<
xmap \a< <Plug>AM_a<
omap \a< <Plug>AM_a<
nmap \a, <Plug>AM_a,
xmap \a, <Plug>AM_a,
omap \a, <Plug>AM_a,
nmap \a? <Plug>AM_a?
xmap \a? <Plug>AM_a?
omap \a? <Plug>AM_a?
xnoremap <silent> \\w :call EasyMotion#WB(1, 0)
onoremap <silent> \\w :call EasyMotion#WB(0, 0)
nnoremap <silent> \\w :call EasyMotion#WB(0, 0)
xnoremap <silent> \\t :call EasyMotion#T(1, 0)
onoremap <silent> \\t :call EasyMotion#T(0, 0)
nnoremap <silent> \\t :call EasyMotion#T(0, 0)
xnoremap <silent> \\n :call EasyMotion#Search(1, 0)
onoremap <silent> \\n :call EasyMotion#Search(0, 0)
nnoremap <silent> \\n :call EasyMotion#Search(0, 0)
xnoremap <silent> \\k :call EasyMotion#JK(1, 1)
onoremap <silent> \\k :call EasyMotion#JK(0, 1)
nnoremap <silent> \\k :call EasyMotion#JK(0, 1)
xnoremap <silent> \\j :call EasyMotion#JK(1, 0)
onoremap <silent> \\j :call EasyMotion#JK(0, 0)
nnoremap <silent> \\j :call EasyMotion#JK(0, 0)
xnoremap <silent> \\gE :call EasyMotion#EW(1, 1)
onoremap <silent> \\gE :call EasyMotion#EW(0, 1)
nnoremap <silent> \\gE :call EasyMotion#EW(0, 1)
xnoremap <silent> \\f :call EasyMotion#F(1, 0)
onoremap <silent> \\f :call EasyMotion#F(0, 0)
nnoremap <silent> \\f :call EasyMotion#F(0, 0)
xnoremap <silent> \\e :call EasyMotion#E(1, 0)
onoremap <silent> \\e :call EasyMotion#E(0, 0)
nnoremap <silent> \\e :call EasyMotion#E(0, 0)
xnoremap <silent> \\b :call EasyMotion#WB(1, 1)
onoremap <silent> \\b :call EasyMotion#WB(0, 1)
nnoremap <silent> \\b :call EasyMotion#WB(0, 1)
xnoremap <silent> \\W :call EasyMotion#WBW(1, 0)
onoremap <silent> \\W :call EasyMotion#WBW(0, 0)
nnoremap <silent> \\W :call EasyMotion#WBW(0, 0)
xnoremap <silent> \\T :call EasyMotion#T(1, 1)
onoremap <silent> \\T :call EasyMotion#T(0, 1)
nnoremap <silent> \\T :call EasyMotion#T(0, 1)
xnoremap <silent> \\N :call EasyMotion#Search(1, 1)
onoremap <silent> \\N :call EasyMotion#Search(0, 1)
nnoremap <silent> \\N :call EasyMotion#Search(0, 1)
xnoremap <silent> \\ge :call EasyMotion#E(1, 1)
onoremap <silent> \\ge :call EasyMotion#E(0, 1)
nnoremap <silent> \\ge :call EasyMotion#E(0, 1)
xnoremap <silent> \\F :call EasyMotion#F(1, 1)
onoremap <silent> \\F :call EasyMotion#F(0, 1)
nnoremap <silent> \\F :call EasyMotion#F(0, 1)
xnoremap <silent> \\E :call EasyMotion#EW(1, 0)
onoremap <silent> \\E :call EasyMotion#EW(0, 0)
nnoremap <silent> \\E :call EasyMotion#EW(0, 0)
xnoremap <silent> \\B :call EasyMotion#WBW(1, 1)
onoremap <silent> \\B :call EasyMotion#WBW(0, 1)
nnoremap <silent> \\B :call EasyMotion#WBW(0, 1)
nnoremap \  :noh
nnoremap <silent> \q* :execute 'vimgrep '.expand('<cword>').' '.expand('%')  :copen  :cc 
xnoremap <silent> \q* :execute 'vimgrep '.expand('<cword>').' '.expand('%')  :copen  :cc 
onoremap <silent> \q* :execute 'vimgrep '.expand('<cword>').' '.expand('%')  :copen  :cc 
nnoremap <silent> \qq :call QFixToggle(0)
xnoremap <silent> \qq :call QFixToggle(0)
onoremap <silent> \qq :call QFixToggle(0)
nnoremap \G :Rgrep
xnoremap \G :Rgrep
onoremap \G :Rgrep
nnoremap \gg :GrepBuffer 
xnoremap \gg :GrepBuffer 
onoremap \gg :GrepBuffer 
nnoremap \g :GrepBuffer 
xnoremap \g :GrepBuffer 
onoremap \g :GrepBuffer 
nmap <silent> \dif0 :call DiffEndTabs()
xmap <silent> \dif0 :call DiffEndTabs()
omap <silent> \dif0 :call DiffEndTabs()
nmap <silent> \diff :call DiffTabs(2, 1)
xmap <silent> \diff :call DiffTabs(2, 1)
omap <silent> \diff :call DiffTabs(2, 1)
xmap ]% ]%m'gv``
nmap ]q :cnext
xmap ]q :cnext
omap ]q :cnext
xmap a% [%v]%
nmap cs <Plug>Csurround
nmap ds <Plug>Dsurround
nmap gx <Plug>NetrwBrowseX
xmap gS <Plug>VgSurround
nnoremap gk k
xnoremap gk k
onoremap gk k
nnoremap gj j
xnoremap gj j
onoremap gj j
nnoremap j gj
nnoremap k gk
xnoremap p "_dP
xmap s <Plug>Vsurround
nmap ySS <Plug>YSsurround
nmap ySs <Plug>YSsurround
nmap yss <Plug>Yssurround
nmap yS <Plug>YSurround
nmap ys <Plug>Ysurround
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nmap <silent> <Plug>VLToggle :let g:VeryLiteral = !g:VeryLiteral| echo "VeryLiteral " . (g:VeryLiteral ? "On" : "Off")
vmap <kMultiply> *
vnoremap <F2> :norm@q
nnoremap <F2> :call Marvim_search()
vnoremap <F3> y:call Marvim_template_store()
nnoremap <F3> :call Marvim_macro_store()
noremap <Plug>VisualFirstLine :call EnhancedCommentify('', 'first',   line("'<"), line("'>"))
noremap <Plug>VisualTraditional :call EnhancedCommentify('', 'guess',   line("'<"), line("'>"))
noremap <Plug>VisualDeComment :call EnhancedCommentify('', 'decomment',   line("'<"), line("'>"))
noremap <Plug>VisualComment :call EnhancedCommentify('', 'comment',   line("'<"), line("'>"))
noremap <Plug>FirstLine :call EnhancedCommentify('', 'first')
noremap <Plug>Traditional :call EnhancedCommentify('', 'guess')
noremap <Plug>DeComment :call EnhancedCommentify('', 'decomment')
noremap <Plug>Comment :call EnhancedCommentify('', 'comment')
nmap <silent> <Plug>RestoreWinPosn :call RestoreWinPosn()
nmap <silent> <Plug>SaveWinPosn :call SaveWinPosn()
nmap <SNR>26_WE <Plug>AlignMapsWrapperEnd
map <SNR>26_WS <Plug>AlignMapsWrapperStart
snoremap <silent> <Del> c
snoremap <silent> <BS> c
snoremap <silent> <C-Tab> :call UltiSnips_ListSnippets()
xnoremap <F4> :call UltiSnips_SaveLastVisualSelection()gvs
snoremap <silent> <F4> :call UltiSnips_ExpandSnippet()
nmap <F8> :TagbarToggle
noremap <F1> 
nmap <C-Space> i 
nmap <C-S-CR> i>>
nmap <C-CR> i
vmap <C-Del> "*d
vmap <S-Del> "*d
vmap <C-Insert> "*y
vmap <S-Insert> "-d"*P
nmap <S-Insert> "*P
imap  
imap S <Plug>ISurround
imap s <Plug>Isurround
imap 	 <Plug>SuperTabForward
inoremap <silent> <NL> =UltiSnips_JumpForwards()
inoremap <silent>  =UltiSnips_JumpBackwards()
imap  
imap  
imap  "+gP
imap  
imap  u
imap <silent> \x <Plug>Traditional
imap <silent> \c <Plug>Traditionalji
nmap á <Plug>SequenceN_Increment
xmap á <Plug>SequenceV_Increment
nmap ø <Plug>SequenceN_Decrement
xmap ø <Plug>SequenceV_Decrement
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set autoread
set backspace=indent,eol,start
set browsedir=buffer
set completeopt=longest,menuone
set expandtab
set gdefault
set guifont=Consolas:h10:cANSI
set guitablabel=%{GuiTabLabel()}
set helplang=En
set hidden
set ignorecase
set incsearch
set iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,~,#,|,&,.,{,},[,]
set listchars=tab:>-,trail:.,extends:>
set modelines=0
set mouse=nvrh
set omnifunc=SlimvOmniComplete
set operatorfunc=PareditDelete
set ruler
set runtimepath=~\\vimfiles\\bundle\\vundle,~\\vimfiles\\bundle\\vim-easymotion,~\\vimfiles\\bundle\\slimv,~\\vimfiles\\bundle\\ultisnips,~\\vimfiles\\bundle\\delimitMate,~\\vimfiles\\bundle\\supertab,~\\vimfiles\\bundle\\slimv,~/vimfiles,C:\\Program\ Files\ (x86)\\vim/vimfiles,C:\\Program\ Files\ (x86)\\vim\\vim74,C:\\Program\ Files\ (x86)\\vim/vimfiles/after,~/vimfiles/after,~/vimfiles/bundle/vundle/,~\\vimfiles\\bundle\\vundle/after,~\\vimfiles\\bundle\\vim-easymotion/after,~\\vimfiles\\bundle\\slimv/after,~\\vimfiles\\bundle\\ultisnips/after,~\\vimfiles\\bundle\\delimitMate/after,~\\vimfiles\\bundle\\supertab/after,~\\vimfiles\\bundle\\slimv/after
set scrolloff=3
set shiftround
set shiftwidth=2
set showmatch
set smartcase
set smarttab
set softtabstop=2
set tabpagemax=15
set tabstop=2
set textwidth=125
set visualbell
set wildcharm=<Tab>
set wildignore=*.o,*.obj,*.bak,*.exe,*.sw[pno]
set wildmenu
set wildmode=longest:full
set window=47
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd C:\Program\ Files\ (x86)\vim\vim74
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +273 \Users\Jonathan\Lisp\projects\traders-friend\trading-agents\range-projection-mean-reversion.lisp
badd +116 \Users\Jonathan\Lisp\projects\traders-friend\trading-agents\opening-range-breakout.lisp
badd +82 \Users\Jonathan\Lisp\projects\traders-friend\utility-functions.lisp
badd +1 \Users\Jonathan\Lisp\projects\traders-friend\traders-friend.asd
silent! argdel *
edit \Users\Jonathan\Lisp\projects\traders-friend\traders-friend.asd
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
let s:cpo_save=&cpo
set cpo&vim
imap <buffer> <S-BS> <Plug>delimitMateS-BS
inoremap <buffer> <Plug>delimitMateJumpMany =delimitMate#JumpMany()
inoremap <buffer> <expr> <Del> PareditDel()
inoremap <buffer> <expr> <BS> PareditBackspace(0)
inoremap <buffer> <silent> <S-Tab> =pumvisible() ? "\<C-P>" : "\<S-Tab>"
vnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',1)
nnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',0)
vnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',1)
nnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',0)
nnoremap <buffer> <silent> ,S :call PareditSplice()|silent! call repeat#set(",S")
vnoremap <buffer> <silent> ,W :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,W :call PareditWrap("(",")")|silent! call repeat#set(",W")
nnoremap <buffer> <silent> ,J :call PareditJoin()|silent! call repeat#set(",J")
nnoremap <buffer> <silent> ,O :call PareditSplit()|silent! call repeat#set(",O")
nnoremap <buffer> <silent> ,> :call PareditMoveRight()|silent! call repeat#set(",>")
nnoremap <buffer> <silent> ,< :call PareditMoveLeft()|silent! call repeat#set(",\<")
nnoremap <buffer> <silent> ,I :call PareditRaise()|silent! call repeat#set(",I")
nmap <buffer> <silent> ,<Down> d])%,S
nmap <buffer> <silent> ,<Up> d[(,S
vnoremap <buffer> <silent> ,w" :call PareditWrapSelection('"','"')
nnoremap <buffer> <silent> ,w" :call PareditWrap('"','"')|silent! call repeat#set(",w\"")
vnoremap <buffer> <silent> ,w( :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,w( :call PareditWrap("(",")")|silent! call repeat#set(",w(")
nnoremap <buffer> <silent> C v$:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> D v$:call PareditDelete(visualmode(),1)|silent! call repeat#set("D")
nnoremap <buffer> <silent> P :call PareditPut("P")|silent! call repeat#set("P")
nnoremap <buffer> <silent> S V:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> X :call PareditEraseBck()|silent! call repeat#set("X")
nnoremap <buffer> <silent> [[ :call PareditFindDefunBck()
nnoremap <buffer> <silent> ]] :call PareditFindDefunFwd()
nnoremap <buffer> <silent> caw :call PareditChangeSpec('caw',1)
nnoremap <buffer> <silent> ciw :call PareditChangeSpec('ciw',1)
nnoremap <buffer> <silent> cb :call PareditChangeSpec('cb',0)
nnoremap <buffer> <silent> cW :set opfunc=PareditChangeg@E
nnoremap <buffer> <silent> cw :call PareditChangeSpec('cw',1)
nnoremap <buffer> <silent> cc :call PareditChangeLines()
vnoremap <buffer> <silent> c :call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> c :set opfunc=PareditChangeg@
nnoremap <buffer> <silent> dd :call PareditDeleteLines()|silent! call repeat#set("dd")
vnoremap <buffer> <silent> d :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> d :call PareditSetDelete(v:count)g@
nnoremap <buffer> <silent> p :call PareditPut("p")|silent! call repeat#set("p")
nnoremap <buffer> <silent> s :call PareditEraseFwd()i
vnoremap <buffer> <silent> x :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> x :call PareditEraseFwd()|silent! call repeat#set("x")
vnoremap <buffer> <silent> <Del> :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> <Del> :call PareditEraseFwd()
imap <buffer> <silent> g <Plug>delimitMateJumpMany
inoremap <buffer> <silent> 	 =SlimvHandleTab()
inoremap <buffer> <silent>  =pumvisible() ?  "\<C-Y>" : SlimvHandleEnter()=SlimvArglistOnEnter()
inoremap <buffer> <silent> 0 :call SlimvCloseForm()
inoremap <buffer> <silent>    =SlimvArglist()
inoremap <buffer> <expr> " PareditInsertQuotes()
inoremap <buffer> <expr> ( PareditInsertOpening('(',')')
inoremap <buffer> <silent> ) =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('(',')'):let &ve=save_ve
imap <buffer> [ <Plug>delimitMate[
imap <buffer> ] <Plug>delimitMate]
imap <buffer> { <Plug>delimitMate{
imap <buffer> } <Plug>delimitMate}
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=SlimvDescribe(v:beval_text)
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
set colorcolumn=132
setlocal colorcolumn=132
setlocal comments=:;;;,:;;,sr:#|,mb:|,ex:|#,:;
setlocal commentstring=;%s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'lisp'
setlocal filetype=lisp
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=2
setlocal imsearch=2
setlocal include=
setlocal includeexpr=
setlocal indentexpr=SlimvIndent(v:lnum)
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94
setlocal keywordprg=
setlocal nolinebreak
setlocal lisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=SlimvOmniComplete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
set relativenumber
setlocal relativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'lisp'
setlocal syntax=lisp
endif
setlocal tabstop=2
setlocal tags=
setlocal textwidth=125
setlocal thesaurus=
setlocal noundofile
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 25 - ((24 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
25
normal! 0
tabedit \Users\Jonathan\Lisp\projects\traders-friend\trading-agents\opening-range-breakout.lisp
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
let s:cpo_save=&cpo
set cpo&vim
imap <buffer> <S-BS> <Plug>delimitMateS-BS
inoremap <buffer> <Plug>delimitMateJumpMany =delimitMate#JumpMany()
inoremap <buffer> <expr> <Del> PareditDel()
inoremap <buffer> <expr> <BS> PareditBackspace(0)
inoremap <buffer> <silent> <S-Tab> =pumvisible() ? "\<C-P>" : "\<S-Tab>"
xnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',1)
nnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',0)
xnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',1)
nnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',0)
nnoremap <buffer> <silent> ,S :call PareditSplice()|silent! call repeat#set(",S")
xnoremap <buffer> <silent> ,W :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,W :call PareditWrap("(",")")|silent! call repeat#set(",W")
nnoremap <buffer> <silent> ,J :call PareditJoin()|silent! call repeat#set(",J")
nnoremap <buffer> <silent> ,O :call PareditSplit()|silent! call repeat#set(",O")
nnoremap <buffer> <silent> ,> :call PareditMoveRight()|silent! call repeat#set(",>")
nnoremap <buffer> <silent> ,< :call PareditMoveLeft()|silent! call repeat#set(",\<")
nnoremap <buffer> <silent> ,I :call PareditRaise()|silent! call repeat#set(",I")
nmap <buffer> <silent> ,<Down> d])%,S
nmap <buffer> <silent> ,<Up> d[(,S
xnoremap <buffer> <silent> ,w" :call PareditWrapSelection('"','"')
nnoremap <buffer> <silent> ,w" :call PareditWrap('"','"')|silent! call repeat#set(",w\"")
xnoremap <buffer> <silent> ,w( :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,w( :call PareditWrap("(",")")|silent! call repeat#set(",w(")
nnoremap <buffer> <silent> C v$:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> D v$:call PareditDelete(visualmode(),1)|silent! call repeat#set("D")
nnoremap <buffer> <silent> P :call PareditPut("P")|silent! call repeat#set("P")
nnoremap <buffer> <silent> S V:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> X :call PareditEraseBck()|silent! call repeat#set("X")
nnoremap <buffer> <silent> [[ :call PareditFindDefunBck()
nnoremap <buffer> <silent> ]] :call PareditFindDefunFwd()
nnoremap <buffer> <silent> caw :call PareditChangeSpec('caw',1)
nnoremap <buffer> <silent> ciw :call PareditChangeSpec('ciw',1)
nnoremap <buffer> <silent> cb :call PareditChangeSpec('cb',0)
nnoremap <buffer> <silent> cW :set opfunc=PareditChangeg@E
nnoremap <buffer> <silent> cw :call PareditChangeSpec('cw',1)
nnoremap <buffer> <silent> cc :call PareditChangeLines()
xnoremap <buffer> <silent> c :call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> c :set opfunc=PareditChangeg@
nnoremap <buffer> <silent> dd :call PareditDeleteLines()|silent! call repeat#set("dd")
xnoremap <buffer> <silent> d :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> d :call PareditSetDelete(v:count)g@
nnoremap <buffer> <silent> p :call PareditPut("p")|silent! call repeat#set("p")
nnoremap <buffer> <silent> s :call PareditEraseFwd()i
xnoremap <buffer> <silent> x :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> x :call PareditEraseFwd()|silent! call repeat#set("x")
vnoremap <buffer> <silent> <Del> :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> <Del> :call PareditEraseFwd()
imap <buffer> <silent> g <Plug>delimitMateJumpMany
inoremap <buffer> <silent> 	 =SlimvHandleTab()
inoremap <buffer> <silent>  =pumvisible() ?  "\<C-Y>" : SlimvHandleEnter()=SlimvArglistOnEnter()
inoremap <buffer> <silent> 0 :call SlimvCloseForm()
inoremap <buffer> <silent>    =SlimvArglist()
inoremap <buffer> <expr> " PareditInsertQuotes()
inoremap <buffer> <expr> ( PareditInsertOpening('(',')')
inoremap <buffer> <silent> ) =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('(',')'):let &ve=save_ve
imap <buffer> [ <Plug>delimitMate[
imap <buffer> ] <Plug>delimitMate]
imap <buffer> { <Plug>delimitMate{
imap <buffer> } <Plug>delimitMate}
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=SlimvDescribe(v:beval_text)
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
set colorcolumn=132
setlocal colorcolumn=132
setlocal comments=:;;;,:;;,sr:#|,mb:|,ex:|#,:;
setlocal commentstring=;%s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'lisp'
setlocal filetype=lisp
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=2
setlocal imsearch=2
setlocal include=
setlocal includeexpr=
setlocal indentexpr=SlimvIndent(v:lnum)
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,~,#,|,&,.,{,},[,]
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=SlimvOmniComplete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
set relativenumber
setlocal relativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'lisp'
setlocal syntax=lisp
endif
setlocal tabstop=2
setlocal tags=
setlocal textwidth=125
setlocal thesaurus=
setlocal noundofile
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 42 - ((25 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
42
normal! 049|
tabedit \Users\Jonathan\Lisp\projects\traders-friend\utility-functions.lisp
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
let s:cpo_save=&cpo
set cpo&vim
imap <buffer> <S-BS> <Plug>delimitMateS-BS
inoremap <buffer> <Plug>delimitMateJumpMany =delimitMate#JumpMany()
inoremap <buffer> <expr> <Del> PareditDel()
inoremap <buffer> <expr> <BS> PareditBackspace(0)
inoremap <buffer> <silent> <S-Tab> =pumvisible() ? "\<C-P>" : "\<S-Tab>"
vnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',1)
nnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',0)
vnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',1)
nnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',0)
nnoremap <buffer> <silent> ,S :call PareditSplice()|silent! call repeat#set(",S")
vnoremap <buffer> <silent> ,W :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,W :call PareditWrap("(",")")|silent! call repeat#set(",W")
nnoremap <buffer> <silent> ,J :call PareditJoin()|silent! call repeat#set(",J")
nnoremap <buffer> <silent> ,O :call PareditSplit()|silent! call repeat#set(",O")
nnoremap <buffer> <silent> ,> :call PareditMoveRight()|silent! call repeat#set(",>")
nnoremap <buffer> <silent> ,< :call PareditMoveLeft()|silent! call repeat#set(",\<")
nnoremap <buffer> <silent> ,I :call PareditRaise()|silent! call repeat#set(",I")
nmap <buffer> <silent> ,<Down> d])%,S
nmap <buffer> <silent> ,<Up> d[(,S
vnoremap <buffer> <silent> ,w" :call PareditWrapSelection('"','"')
nnoremap <buffer> <silent> ,w" :call PareditWrap('"','"')|silent! call repeat#set(",w\"")
vnoremap <buffer> <silent> ,w( :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,w( :call PareditWrap("(",")")|silent! call repeat#set(",w(")
nnoremap <buffer> <silent> C v$:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> D v$:call PareditDelete(visualmode(),1)|silent! call repeat#set("D")
nnoremap <buffer> <silent> P :call PareditPut("P")|silent! call repeat#set("P")
nnoremap <buffer> <silent> S V:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> X :call PareditEraseBck()|silent! call repeat#set("X")
nnoremap <buffer> <silent> [[ :call PareditFindDefunBck()
nnoremap <buffer> <silent> ]] :call PareditFindDefunFwd()
nnoremap <buffer> <silent> caw :call PareditChangeSpec('caw',1)
nnoremap <buffer> <silent> ciw :call PareditChangeSpec('ciw',1)
nnoremap <buffer> <silent> cb :call PareditChangeSpec('cb',0)
nnoremap <buffer> <silent> cW :set opfunc=PareditChangeg@E
nnoremap <buffer> <silent> cw :call PareditChangeSpec('cw',1)
nnoremap <buffer> <silent> cc :call PareditChangeLines()
vnoremap <buffer> <silent> c :call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> c :set opfunc=PareditChangeg@
nnoremap <buffer> <silent> dd :call PareditDeleteLines()|silent! call repeat#set("dd")
vnoremap <buffer> <silent> d :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> d :call PareditSetDelete(v:count)g@
nnoremap <buffer> <silent> p :call PareditPut("p")|silent! call repeat#set("p")
nnoremap <buffer> <silent> s :call PareditEraseFwd()i
vnoremap <buffer> <silent> x :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> x :call PareditEraseFwd()|silent! call repeat#set("x")
vnoremap <buffer> <silent> <Del> :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> <Del> :call PareditEraseFwd()
imap <buffer> <silent> g <Plug>delimitMateJumpMany
inoremap <buffer> <silent> 	 =SlimvHandleTab()
inoremap <buffer> <silent>  =pumvisible() ?  "\<C-Y>" : SlimvHandleEnter()=SlimvArglistOnEnter()
inoremap <buffer> <silent> 0 :call SlimvCloseForm()
inoremap <buffer> <silent>    =SlimvArglist()
inoremap <buffer> <expr> " PareditInsertQuotes()
inoremap <buffer> <expr> ( PareditInsertOpening('(',')')
inoremap <buffer> <silent> ) =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('(',')'):let &ve=save_ve
imap <buffer> [ <Plug>delimitMate[
imap <buffer> ] <Plug>delimitMate]
imap <buffer> { <Plug>delimitMate{
imap <buffer> } <Plug>delimitMate}
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=SlimvDescribe(v:beval_text)
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
set colorcolumn=132
setlocal colorcolumn=132
setlocal comments=:;;;,:;;,sr:#|,mb:|,ex:|#,:;
setlocal commentstring=;%s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'lisp'
setlocal filetype=lisp
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=2
setlocal imsearch=2
setlocal include=
setlocal includeexpr=
setlocal indentexpr=SlimvIndent(v:lnum)
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94
setlocal keywordprg=
setlocal nolinebreak
setlocal lisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=SlimvOmniComplete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
set relativenumber
setlocal relativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'lisp'
setlocal syntax=lisp
endif
setlocal tabstop=2
setlocal tags=
setlocal textwidth=125
setlocal thesaurus=
setlocal noundofile
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 75 - ((16 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
75
normal! 0
tabedit \Users\Jonathan\Lisp\projects\traders-friend\trading-agents\range-projection-mean-reversion.lisp
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
let s:cpo_save=&cpo
set cpo&vim
imap <buffer> <S-BS> <Plug>delimitMateS-BS
inoremap <buffer> <Plug>delimitMateJumpMany =delimitMate#JumpMany()
inoremap <buffer> <expr> <Del> PareditDel()
inoremap <buffer> <expr> <BS> PareditBackspace(0)
inoremap <buffer> <silent> <S-Tab> =pumvisible() ? "\<C-P>" : "\<S-Tab>"
vnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',1)
nnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',0)
vnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',1)
nnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',0)
nnoremap <buffer> <silent> ,S :call PareditSplice()|silent! call repeat#set(",S")
vnoremap <buffer> <silent> ,W :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,W :call PareditWrap("(",")")|silent! call repeat#set(",W")
nnoremap <buffer> <silent> ,J :call PareditJoin()|silent! call repeat#set(",J")
nnoremap <buffer> <silent> ,O :call PareditSplit()|silent! call repeat#set(",O")
nnoremap <buffer> <silent> ,> :call PareditMoveRight()|silent! call repeat#set(",>")
nnoremap <buffer> <silent> ,< :call PareditMoveLeft()|silent! call repeat#set(",\<")
nnoremap <buffer> <silent> ,I :call PareditRaise()|silent! call repeat#set(",I")
nmap <buffer> <silent> ,<Down> d])%,S
nmap <buffer> <silent> ,<Up> d[(,S
vnoremap <buffer> <silent> ,w" :call PareditWrapSelection('"','"')
nnoremap <buffer> <silent> ,w" :call PareditWrap('"','"')|silent! call repeat#set(",w\"")
vnoremap <buffer> <silent> ,w( :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,w( :call PareditWrap("(",")")|silent! call repeat#set(",w(")
nnoremap <buffer> <silent> C v$:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> D v$:call PareditDelete(visualmode(),1)|silent! call repeat#set("D")
nnoremap <buffer> <silent> P :call PareditPut("P")|silent! call repeat#set("P")
nnoremap <buffer> <silent> S V:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> X :call PareditEraseBck()|silent! call repeat#set("X")
nnoremap <buffer> <silent> [[ :call PareditFindDefunBck()
nnoremap <buffer> <silent> ]] :call PareditFindDefunFwd()
nnoremap <buffer> <silent> caw :call PareditChangeSpec('caw',1)
nnoremap <buffer> <silent> ciw :call PareditChangeSpec('ciw',1)
nnoremap <buffer> <silent> cb :call PareditChangeSpec('cb',0)
nnoremap <buffer> <silent> cW :set opfunc=PareditChangeg@E
nnoremap <buffer> <silent> cw :call PareditChangeSpec('cw',1)
nnoremap <buffer> <silent> cc :call PareditChangeLines()
vnoremap <buffer> <silent> c :call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> c :set opfunc=PareditChangeg@
nnoremap <buffer> <silent> dd :call PareditDeleteLines()|silent! call repeat#set("dd")
vnoremap <buffer> <silent> d :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> d :call PareditSetDelete(v:count)g@
nnoremap <buffer> <silent> p :call PareditPut("p")|silent! call repeat#set("p")
nnoremap <buffer> <silent> s :call PareditEraseFwd()i
vnoremap <buffer> <silent> x :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> x :call PareditEraseFwd()|silent! call repeat#set("x")
vnoremap <buffer> <silent> <Del> :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> <Del> :call PareditEraseFwd()
imap <buffer> <silent> g <Plug>delimitMateJumpMany
inoremap <buffer> <silent> 	 =SlimvHandleTab()
inoremap <buffer> <silent>  =pumvisible() ?  "\<C-Y>" : SlimvHandleEnter()=SlimvArglistOnEnter()
inoremap <buffer> <silent> 0 :call SlimvCloseForm()
inoremap <buffer> <silent>    =SlimvArglist()
inoremap <buffer> <expr> " PareditInsertQuotes()
inoremap <buffer> <expr> ( PareditInsertOpening('(',')')
inoremap <buffer> <silent> ) =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('(',')'):let &ve=save_ve
imap <buffer> [ <Plug>delimitMate[
imap <buffer> ] <Plug>delimitMate]
imap <buffer> { <Plug>delimitMate{
imap <buffer> } <Plug>delimitMate}
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=SlimvDescribe(v:beval_text)
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
set colorcolumn=132
setlocal colorcolumn=132
setlocal comments=:;;;,:;;,sr:#|,mb:|,ex:|#,:;
setlocal commentstring=;%s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'lisp'
setlocal filetype=lisp
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=2
setlocal imsearch=2
setlocal include=
setlocal includeexpr=
setlocal indentexpr=SlimvIndent(v:lnum)
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94
setlocal keywordprg=
setlocal nolinebreak
setlocal lisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=SlimvOmniComplete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
set relativenumber
setlocal relativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'lisp'
setlocal syntax=lisp
endif
setlocal tabstop=2
setlocal tags=
setlocal textwidth=125
setlocal thesaurus=
setlocal noundofile
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 273 - ((21 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
273
normal! 011|
tabnext 2
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :

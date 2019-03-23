/**/
OPTIONS SINGLE_INTERPRETER
Call RxFuncAdd 'DW_LoadFuncs', 'rexxdw', 'DW_LoadFuncs'
Call dw_loadfuncs

  Trace  a 
/*
 * Depending on platform, set up SliderSpinner constants
 */
Parse Version ver
Parse Source os . prog
lp = Lastpos( !REXXDW.!DIRSEP, prog )
icondir = Substr( prog, 1, lp )

Do
   !global.!fixedfont = "fixed"
   !global.!icondir = icondir'gtk/'
   rexxutil = 'rexxutil'
   !global.!root = '/home/mark/RexxDW'
   enable_explorer = 1
   enable_rendering = 1
   !global.!widgetheight = 25
End

!global.!foldericon = dw_icon_load_from_file( !global.!icondir'folder' );
!global.!fileicon = dw_icon_load_from_file( !global.!icondir'file'  );

/*
 * Check if rexxutil already loaded...
 */
If RxFuncQuery( 'sysfiletree' ) \= 0 Then
   Do
      If RxFuncAdd( 'sysloadfuncs', rexxutil, 'sysloadfuncs' ) \= 0 Then
         Do
            If RxFuncAdd( 'SysLoadFuncs', rexxutil, 'SysLoadFuncs' ) \= 0 Then
               Do
                  enable_explorer = 0
               End
            Else enable_explorer = 1
         End
      Else enable_explorer = 1
   End
Else
   enable_explorer = 1

If enable_explorer Then Call SysLoadFuncs

!globalv = '!REXXDW. !global. ind. items. !dialog.'

Call dw_init

!global.!allowcallbacks = 0

!global.!screen_width = dw_screen_width()
!global.!screen_height = dw_screen_height()

!global.!style = dw_or( !REXXDW.!DW_FCF_SYSMENU, !REXXDW.!DW_FCF_TITLEBAR, !REXXDW.!DW_FCF_SHELLPOSITION, !REXXDW.!DW_FCF_TASKLIST, !REXXDW.!DW_FCF_DLGBORDER )

/* create our toplevel window */
!global.!mainwindow = dw_window_new( !REXXDW.!DW_DESKTOP, 'Nome da Janela', dw_or( !global.!style, !REXXDW.!DW_FCF_SIZEBORDER, !REXXDW.!DW_FCF_MINMAX ) )

notebookbox = dw_box_new( !REXXDW.!DW_VERT )
Call dw_box_pack_start !global.!mainwindow, notebookbox, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 0

notebook = dw_notebook_new( 0, !REXXDW.!DW_TAB_TO_BOTTOM )
Call dw_box_pack_start notebookbox, notebook, 100, 100, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 5

Call dw_signal_connect !global.!mainwindow, !REXXDW.!DW_EXPOSE_EVENT, 'MainExposeCallback'
Call dw_signal_connect !global.!mainwindow, !REXXDW.!DW_CONFIGURE_EVENT, 'MainConfigureCallback'

tb = dw_taskbar_insert( !global.!mainwindow, !global.!foldericon, 'Rexx/DW Demo', 'TaskbarMenuCallback', 'arg1', 'arg2' )
/*
 * SliderSpinner:
 *   box, groupbox, button, bitmapbutton, slider/percent, radiobutton/checkbutton
 * Container: (explorer)
 *   container, tree, popup menu, splitbar, mle
 * Render: (display file selected in Container)
 *   render, pixmap
 */
pages = 'ListBoxSelect'

If \enable_rendering Then pages = Delword( pages, 3, 1 )
If \enable_explorer Then pages = Delword( pages, 2, 1 )


Do i = 1 To Words( pages )
   Call CreatePage notebook, Word( pages, i ), i
End

Call dw_signal_connect notebook, !REXXDW.!DW_SWITCH_PAGE_EVENT, 'SwitchPageCallback'
Call dw_notebook_page_set notebook, !global.!notebookpage.1
Call dw_signal_connect !global.!mainwindow, !REXXDW.!DW_DELETE_EVENT, 'QuitCallback', !global.!mainwindow




/* CONFIGURA TAMANHO  JANELA PRINCIPAL */

Call dw_window_set_pos_size !global.!mainwindow, 0, 0, 400, 400
Call dw_window_show !global.!mainwindow


/*
 * The following is the main event loop for this program. We check the
 * variable !REXXDW.!HAVE_REXXCALLBACK to determine which mechanism
 * can be used for callbacks. It is far better to use the mechanism
 * offered by an interpreter that offers RexxCallBack() in its API.
 */

!global.!allowcallbacks = 1

If !REXXDW.!HAVE_REXXCALLBACK Then
   Do
      Call dw_main
   End
Else
   Do Forever
      cmd = dw_main_iteration()
      If cmd \= '' Then
         Do
            Interpret 'Call' cmd
         End
   End
Call dw_taskbar_delete tb, !global.!foldericon
Call dw_window_destroy !global.!mainwindow
Call dw_shutdown
Exit 0

Return 0

Call dw_signal_connect splashwindow, !REXXDW.!DW_DELETE_EVENT, 'GenericCloseCallback', splashwindow

CreatePage: Procedure Expose (!globalv)
Parse Arg notebook, page, idx
func = Translate( page, '', '/' )
!global.!notebookbox.idx = dw_box_new( !REXXDW.!DW_VERT )
!global.!notebookpage.idx = dw_notebook_page_new( notebook, 0, !REXXDW.!DW_PAGE_TO_BACK )
Call dw_notebook_pack notebook, !global.!notebookpage.idx, !global.!notebookbox.idx
Call dw_notebook_page_set_text notebook, !global.!notebookpage.idx, page
Interpret 'Call' func idx
Return

/*
 * Display the splash window
 */
Call dw_window_set_pos_size splashwindow, (!global.!screen_width % 2) - (window_x % 2), (!global.!screen_height % 2) - (window_y % 2), window_x, window_y
Call dw_window_show splashwindow
Return 0


GenericCloseCallback: Procedure Expose (!globalv)
If !global.!allowcallbacks = 0 Then Return 1
Parse Arg ., window, dialog
Call dw_window_destroy window
If dialog \= '' Then Call dw_dialog_dismiss dialog, 'close'
Return 0

QuitCallback:
If !global.!allowcallbacks = 0 Then Return 1
Call dw_main_quit
Return 0


ListBoxSelect: Procedure Expose (!globalv)
Parse Arg idx
mainbox = dw_box_new( !REXXDW.!DW_HORZ )
Call dw_box_pack_start !global.!notebookbox.idx, mainbox, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 5

selectorbox = dw_groupbox_new( !REXXDW.!DW_VERT, 'Listbox Selector' )
Call dw_box_pack_start mainbox, selectorbox, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 2

!global.!ls = dw_listbox_selector_new( 'Selected', 'Not Selected', 100, 200 )
Call dw_box_pack_start selectorbox, !global.!ls, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 0
items.0 = 10
items.1 = 'one'
items.2 = 'two'
items.3 = 'three'
items.4 = 'four'
items.5 = 'five'
items.6 = 'six'
items.7 = 'seven'
items.8 = 'eight'
items.9 = 'nine'
items.10 = 'ten'


ind. = 0
ind.0 = items.0

/* configura valor inicial ind.x = items.x 'valor'  */
/*  ind.x = 1 , quer dizer que está selecionado */
/* ind.1  = 10
   ind.10 = 1
   ind.3  = 1  */

Call dw_listbox_selector_setup !global.!ls, 'items.', 'ind.'


tmpbox = dw_box_new( !REXXDW.!DW_HORZ )
Call dw_box_pack_start mainbox, tmpbox, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 0

Call dw_box_pack_start tmpbox, 0, 0, !global.!widgetheight, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 0
abutton = dw_button_new( 'Say Selected', 0 )
Call dw_box_pack_start tmpbox, abutton, 100, !global.!widgetheight, !REXXDW.!DW_DONT_EXPAND_HORZ, !REXXDW.!DW_DONT_EXPAND_VERT, 0
Call dw_signal_connect abutton, !REXXDW.!DW_CLICKED_EVENT, 'SaySelectedCallback'
Call dw_box_pack_start tmpbox, 0, 0, !global.!widgetheight, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_DONT_EXPAND_VERT, 0


Return

SaySelectedCallback: Procedure Expose (!globalv)
If !global.!allowcallbacks = 0 Then Return 1


Do i = 1 To ind.0
   If ind.i = 1 Then Say 'Value:' items.i 'has been selected'
End


return

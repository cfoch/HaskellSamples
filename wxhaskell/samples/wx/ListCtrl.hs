{--------------------------------------------------------------------------------
   List control demo.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore 


entries 
  = [("BouncingBalls.hs"           ,"2402"  ,"Jul 19 16:50")
    ,("ByeDemo.hs"                 ,"1414"  ,"Jul 13 23:18")
    ,("Camels.hs"                  ,"7633"  ,"Aug 20 11:57")
    ,("Controls.hs"                ,"3862"  ,"Aug 20 11:57")
    ,("HelloWorld.hs"              ,"1028"  ,"Aug 15 10:09")
    ,("ImageViewer.hs"             ,"3756"  ,"Aug 20 11:57")
    ,("Layout.hs"                  ,"1075"  ,"Jul 13 23:18")
    ,("ListCtrl.hs"                 ,"750"  ,"Sep  8 16:22")
    ,("Minimal.hs"                  ,"147"  ,"Jul 13 23:18")
    ,("Paint.hs"                   ,"1024"  ,"Aug 20 11:57")
    ,("Process.hs"                 ,"2261"  ,"Aug 20 11:57")
    ,("TimeFlows.hs"               ,"4929"  ,"Aug 20 11:57")
    ,("TimeFlowsEx.hs"             ,"8648"  ,"Aug 20 11:57")
    ,("desert.bmp"                ,"61302"  ,"Jul 13 23:31")
    ]

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do -- main gui elements: frame, panel, text control, and the notebook
       f       <- frame [text := "List Sample"]
       -- panel: just for the nice grey color
       p       <- panel f []
       textlog <- textCtrl p [enabled := False, wrap := WrapLine]

       -- use text control as logger
       textCtrlMakeLogActiveTarget textlog
       logMessage "logging enabled"              

       -- list control
       l  <- listCtrl p [columns := [("Name", AlignLeft, 120)
                                    ,("Size", AlignRight, -1)
                                    ,("Date", AlignRight, -1)]
                        ,items := [[name,size,date] | (name,size,date) <- entries]
                        ,on listEvent := onListEvent
                        ]

       -- specify layout
       set f [layout     := container p $ margin 10 $ 
                            column 5 [ fill  $ widget l
                                     , hfill $ widget textlog
                                     ]
             ,clientSize := sz 400 300
             ]
       return ()
  where
    onListEvent eventList
      = case eventList of
          ListItemSelected idx    -> logMessage ("item selected: " ++ show idx)
          ListItemDeselected idx  -> logMessage ("item de-selected: " ++ show idx)
          other                   -> logMessage ("list control event.")

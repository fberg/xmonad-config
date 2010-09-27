-- needed for custom GSConfig
{-# LANGUAGE NoMonomorphismRestriction #-}

import XMonad

import qualified XMonad.Actions.ConstrainedResize as ConstR
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.DwmPromote
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseGestures
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WindowMenu

import XMonad.Hooks.DynamicLog hiding (shorten)
import XMonad.Hooks.DynamicLog (dzenColor)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Accordion
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.Monitor
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.Themes

import qualified XMonad.StackSet as W

import Data.Ratio
import Data.List
import qualified Data.Map as M
import System.Exit
import System.IO (Handle)
import System.IO.UTF8
import System.Posix.Unistd

----------------------------------------------------------------------
-- Colors and Fonts

-- yettenet-style color theme
myGlobalBackground      = "#000000"
myGlobalForeground      = "#555555"
myLayoutColor           = "#668800"
--myActiveColor           = "#b88b10"
myActiveColor           = "#807855"
--myActiveTextColor       = "#262729"
myActiveTextColor       = "#000000"
--myActiveBorderColor     = "#b88b10"
myActiveBorderColor     = myActiveColor
--myActiveBorderColor     = "#807855"
myInactiveColor         = "#262729"
myInactiveTextColor     = "#807855"
myInactiveBorderColor   = myInactiveColor
--myUrgentColor           = "#ce5c00"
myUrgentColor           = myGlobalBackground
--myUrgentTextColor       = "#000000"
myUrgentTextColor       = "#CE5C00"
myUrgentBorderColor     = myUrgentTextColor
myTitleColor            = "#FFFFFF"

myFontName              = "-*-profont-*-*-*-*-11-*-*-*-*-*-*-*"
--myXftFontName           = "xft:DejaVuSans Mono:size=7:bold"
myXftFontName           = "xft:HandelGotDlig:size=8"

----------------------------------------------------------------------
-- Settings

myTerminal      = "urxvtc"
myFileManager   = "dolphin"
myScreenLock    = "slock"
myScreenShot    = "scrot '%F_%H-%M-%S_$wx$h_scrot.png' -e 'mv $f ~/screenshots/'"
myScreenShot2   = "sleep 0.2; " ++ myScreenShot ++ " -s" -- take shot of window or area
myBorderWidth   = 1
myModMask       = mod4Mask
myNumlockMask   = mod2Mask
myWorkspaces    = ["web", "math", "term", "fs", "comm", "code", "doc", "net", "misc"]
--myWorkspaces    = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
--mySPRect        = W.RationalRect 0.6 0.5 0.39 0.48 -- Rectangle in lower right corner
mySPRect        = W.RationalRect 0.01 0.02 0.98 0.3 -- Quake-like terminal
myGSApps        = 
    [ myTerminal ++ " -e vim ~/.xmonad/xmonad.hs"
    , "mumble"
    , "gwenview"
    , "systemsettings"
    , "firefox"
    , "konqueror"
    , "chromium-bin"
    , "FBReader"
    , "kile"
    , "lyx"
    , "gimp"
    , "transmission"
    , "tucan"
    , "~/Mathematica/mathematica"
    , "nitrogen ~/wallpapers"
    ]

myStatusXS      = "1" -- Xinerama screen

myIconPath      = "/home/frank/.dzen2/icons/"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

statusbarCmd    =
    "dzen2 " ++
        "-bg '" ++ myGlobalBackground ++ "' " ++
        "-fg '" ++ myGlobalForeground ++ "' " ++ 
 --       "-w "   ++ myStatusWidth ++ " " ++
        "-sa "  ++ "c " ++
        "-fn '" ++ myFontName ++ "' " ++
        "-xs "  ++ myStatusXS ++ " " ++
        "-ta "  ++ "l " ++
        "-e "   ++ "onstart=lower"

launcherCmd     =
    "dmenu_run " ++
        "-fn '" ++ myFontName ++ "' " ++
        "-nb '" ++ myGlobalBackground ++ "' " ++
        "-nf '" ++ myGlobalForeground ++ "' " ++
        "-sb '" ++ myActiveColor ++ "' " ++
        "-sf '" ++ myActiveTextColor ++ "' " ++
        "-b " ++
        "-i"

----------------------------------------------------------------------
-- Themes

-- Prompts
myPromptConfig :: XPConfig
myPromptConfig = defaultXPConfig
    { font              = myFontName
--    , bgColor           = "#111111"
    , bgColor           = myGlobalBackground
--    , fgColor           = "#d5d3a7"
    , fgColor           = myGlobalForeground
    , fgHLight          = myActiveTextColor
    , bgHLight          = myActiveColor
--    , borderColor       = "black"
--    , borderColor       = myGlobalBackground
    , promptBorderWidth = 0
    , position          = Bottom
    , height            = 13
    , defaultText       = []
    -- temporary fix for http://code.google.com/p/xmonad/issues/detail?id=317
    -- remove when upgrading to >xmonad-0.9.1
    , promptKeymap = M.fromList [((controlMask,xK_c), quit)]
        `M.union` promptKeymap defaultXPConfig
    }

-- Decorations
myTheme :: Theme
myTheme = defaultTheme
    { activeColor           = myActiveColor
    , activeTextColor       = myActiveTextColor
    , activeBorderColor     = myActiveBorderColor
    , inactiveColor         = myGlobalBackground
    , inactiveTextColor     = myInactiveTextColor
    , inactiveBorderColor   = myGlobalBackground
    , urgentColor           = myUrgentColor
    , urgentTextColor       = myUrgentTextColor
    , urgentBorderColor     = myUrgentBorderColor
    , fontName              = myFontName
    , decoHeight            = 13
--    , decoWidth             = 300
    }
    
-- GridSelect
myGSConfig = defaultGSConfig
    { gs_font       = myXftFontName
    }
    
----------------------------------------------------------------------
-- Keyboard and Mouse bindings
myKeys = 
    [ ("M-p"              , spawn launcherCmd)
    , ("M-S-p"            , shellPrompt myPromptConfig)
    , ("M-x"              , xmonadPrompt myPromptConfig)
   
    -- Close the focused Window (compatible with WindowCopy)
    , ("M-S-c"            , kill1)
    -- Copy Window to all Workspaces
    , ("M-S-a"            , windows copyToAll)
    -- Kill all the other copies (restore from previous keybinding)
    , ("M-C-a"            , killAllOtherCopies)

    , ("M-/"              , withFocused (sendMessage . maximizeRestore))
    , ("M-y"              , withFocused (\f -> sendMessage (MinimizeWin f)))
    , ("M-S-y"            , sendMessage RestoreNextMinimizedWin)
    , ("M-<Return>"       , dwmpromote)
    , ("M-u"              , focusUrgent)
    
    , ("M-f"              , spawn myFileManager)
    , ("M-<Print>"        , spawn myScreenShot)
    , ("M-S-<Print>"      , spawn myScreenShot2)
    , ("M-S-\\"           , spawn myScreenLock)
    , ("M-s"              , scratchpadSpawnActionTerminal myTerminal)

    -- Reflect
--    , ((myModMask	, xK_r)		, sendMessage $ Toggle REFLECTX)

    -- Resize
    , ("M-i"              , sendMessage Shrink)
    , ("M-S-i"            , sendMessage MirrorShrink)
    , ("M-o"              , sendMessage Expand)
    , ("M-S-o"            , sendMessage MirrorExpand)

    -- GridSelect for windows
    , ("M-g"              , goToSelected myGSConfig)
    , ("M-S-g"            , bringSelected myGSConfig)
    -- GridSelect for a nice application menu
    , ("M-r"              , spawnSelected myGSConfig myGSApps)

    -- Move to next non-empty Workspace
    , ("M-n"              , moveTo Next EmptyWS)
    -- Take the window with me
    , ("M-S-n"            , shiftTo Next EmptyWS)
    
    -- Cycle through recent Workspaces with [Alt] + [Tab]
    , ("M1-<Tab>"         , cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
    
    -- DynamicWorkspaces
    , ("M-;"              , selectWorkspace myPromptConfig)
    , ("M-S-;"            , withWorkspace myPromptConfig (windows . W.shift))
    , ("M-<Backspace>"    , renameWorkspace myPromptConfig)
    , ("M-S-<Backspace>"  , removeWorkspace)
    ]
    ++
    -- GreedyView
    [ ("M" ++ mask ++ ('-':key:[]) , windows $ f ws)
        | (ws, key) <- zip myWorkspaces ['1'..]
        , (f, mask) <- [(W.greedyView, "") , (W.shift, "-S"), (copy, "-C")]
    ]
    ++
    -- XMonad.Actions.PhysicalScreens:
    --   mod-{w,e}, Switch to physical/Xinerama screens 1 or 2
    --   mod-shift-{w,e}, Move client to screen 1 or 2
    [ ("M" ++ mask ++ ('-':key:[]) , f scr)
         | (key, scr)  <- zip "we" [0..]
         , (f, mask) <- [ (viewScreen, "") , (sendToScreen, "-S") ]
    ]
    ++
    -- WindowNavigator
    [ ("M" ++ mask ++ ('-':key) , sendMessage $ f dir)
        | (dir, key) <- zip [R, L, U, D, R, L, U, D] ["<Right>", "<Left>", "<Up>", "<Down>", "l", "h", "k", "j"]
        , (f, mask) <- [ (Go, "") , (Swap, "-S") ]
    ]
     ++
    -- SubLayouts
    [ ("M-C" ++ ('-':key:[]) , sendMessage $ pullGroup dir) 
        | (dir, key) <- zip [L, R, U, D] "hjkl"
    ]
    ++
    [ ("M-C-m"            , withFocused (sendMessage . MergeAll))
    , ("M-C-u"            , withFocused (sendMessage . UnMerge))
    , ("M-C-."            , onGroup W.focusUp')
    , ("M-C-,"            , onGroup W.focusDown')
    ]

myMouse = M.fromList $
    [ ((myModMask               , button3) , \w -> focus w >> Flex.mouseResizeWindow w)
    , ((myModMask .|. shiftMask , button3) , \w -> focus w >> ConstR.mouseResizeWindow w True)
    -- Maximize/Restore
    , ((myModMask               , button2) , \_ -> withFocused (sendMessage . maximizeRestore))
    -- Mouse gestures
    , ((myModMask .|. shiftMask , button1) , mouseGesture gestures)
    ]
    where
        gestures = M.fromList
            [ ([]           , focus)
            , ([U]          , \_ -> sendMessage (Swap U))
            , ([D]          , \_ -> sendMessage (Swap D))
            , ([R]          , \_ -> sendMessage (Swap R))
            , ([L]          , \_ -> sendMessage (Swap L))
            --, ([D, U]       , \_ -> sendMessage SwapWindow)
            , ([R, D, L, U] , \_ -> sendMessage FirstLayout)
            ]

----------------------------------------------------------------------
-- Layouts

-- tabbedLayout = tabbed shrinkText defaultTheme

myLayoutHook =
    avoidStruts $
--    smartBorders $
    maximize $
    minimize $
    layoutHints $
    configurableNavigation (noNavigateBorders) $
--    mkToggle (single REFLECTX) $ -- Tabbed layout (and others) crashes w/ Reflect :(
    onWorkspaces [ "web", "fs" ]
    (   noBorders Full
    ||| tabbed
    ||| Mirror resizableTall
    ) $
--    onWorkspace "im"
--    (   layoutHints $ withIM (1/5) (ClassName "Pidgin" `And` Role "buddy_list") resizableTall
--    ) $
    onWorkspace "doc"
    (   tabbed
    ||| resizableTall
    ||| Mirror resizableTall
    ||| noBorders Full
    ) $
    -- generic Layout
    genericLayout
    where
        genericLayout =
            subLayout [] (Accordion ||| Simplest) $
            (   resizableTall
            ||| threeCol
            ||| Mirror resizableTall
            ||| tabbed
            ||| Grid
            ||| spiral (gRatio)
            ||| noBorders Full
            )
        resizableTall = ResizableTall nmaster delta ratio []
        threeCol = ThreeCol nmaster delta ratio
        tabbed = tabbedBottom shrinkText myTheme
        nmaster = 1
        delta = (3/100)
        ratio = (1/2)
        gRatio = toRational goldenRatio
        goldenRatio = 2/(1+sqrt(5)::Double)

----------------------------------------------------------------------
-- Manage Hooks

myManageHook = composeAll . concat $
    [ [ isDialog                        --> doFloat ]
    , [ isFullscreen                    --> doFloat ]
    , [ className =? c                  --> doShift t | (c, t) <- myClassNameWSShifts ]
    , [ className =? c                  --> doFloat | c <- myClassNameFloats ]
    , [ title =? t                      --> doFloat | t <- myTitleFloats ]
    , [ resource =? r                   --> doFloat | r <- myResourceFloats ]
    , [ fmap (t `isInfixOf`) title      --> doFloat | t <- myMatchTitleAnywhereFloats ]
    , [ fmap (c `isInfixOf`) className  --> doFloat | c <- myMatchClassAnywhereFloats ]
    ]
    where
      myClassNameWSShifts =
          [ ("Okular", "doc")
          , ("Evince", "doc")
          , ("trayer", "comm")
          , ("stalonetray", "comm")
          , ("XMathematica", "math")
          , ("Transmission", "net")
          , ("Firefox", "web")
          --, ("Kmail", "mail")
	      --, ("Pidgin", "im")
          ]
      myClassNameFloats = 
          [ "feh"
          , "kget"
          , "Gimp"
          , "gimp"
          , "MPlayer"
          , "Smplayer"
          , "Kio_uiserver"
          , "Xmessage"
          , "Kcalc"
          , "Toplevel"
          , "Nitrogen"
          , "Dialog"
          , "Gtk-chtheme"
          , "VirtualBox"
          , "Choqok"
          , "Plasma-desktop"          
          , "Kruler"
          , "Skype"
          , "FBReader"
          ]
      myTitleFloats = 
          [ "Downloads"
          , "Iceweasel Preferences"
          , "Save As..."
          , "many more..."
          , "Add-ons"
          , "Progess Dialog"
          , "Element Properties"
          , "Xnest"
          , "Authorization Dialog"
          , "bashrun"
          ]
      myResourceFloats =
          [ "Dialog"
          ]
      myMatchTitleAnywhereFloats =
          [ "VLC"
          , "Copying"
          , "Moving"
          , "Transferring"
          , "Deleting"
          , "Examining"
          , "Gnuplot"
          ]
      myMatchClassAnywhereFloats =
          [ "Gkrellm"
          , "Hz_"
          , "Stylish"
          ]

----------------------------------------------------------------------
-- Status Bar and Logging w/ PrettyPrinter

myLogHook h = dynamicLogWithPP $ myPP h

myPP :: Handle -> PP
myPP h = defaultPP
-- dzenColor doesn't seem to work correcly anymore
--    { ppCurrent = dzenColor myActiveTextColor myActiveColor . wrap pad pad
    { ppCurrent = wrap ("^fg(" ++ myActiveColor ++ ")^r(4x13)^fg()") ("^fg(" ++ myActiveColor ++ ")^r(4x13)^fg()") . dzenColor myActiveTextColor myActiveColor
    , ppSep = " "
    , ppWsSep = ""
--    , ppVisible = dzenColor myInactiveTextColor myInactiveColor . wrap pad pad
    , ppVisible = wrap ("^fg(" ++ myInactiveColor ++ ")^r(4x13)^fg()") ("^fg(" ++ myInactiveColor ++ ")^r(4x13)^fg()") . dzenColor myInactiveTextColor myInactiveColor
    , ppLayout = dzenColor myLayoutColor "" . wrap "" "" . 
        (\x -> case replace x "Maximize Minimize Hinted " "" of
            "Full"                          -> icon("full")             ++ " Full"
            "ResizableTall"                 -> icon("vertical_tiles")   ++ " Tiled |"
            "Mirror ResizableTall"          -> icon("horizontal_tiles") ++ " Tiled -"
            "ThreeCol"                      -> icon("threecol")         ++ " ThreeCol"
            "Tabbed Bottom Simplest"        -> icon("tabbed")           ++ " Tabbed"
            "Spiral"                        -> icon("spiral")           ++ " Spiral"
            "Twopane"                       -> icon("twopane")          ++ " TwoPane"
            "Grid"                          -> icon("grid")             ++ " Grid"
            _                               -> x
        )
    , ppTitle = dzenColor myTitleColor "" . wrap "" ""
    --, ppHiddenNoWindows = dzenColor "" myInactiveColor . wrap pad pad
    , ppHidden = wrap pad pad
    , ppOutput = System.IO.UTF8.hPutStrLn h
    , ppUrgent = dzenColor myUrgentTextColor myUrgentColor
    --, ppUrgent = wrap ("^fg(" ++ myUrgentColor ++ ")^r(4x13)^fg()") ("^fg(" ++ myUrgentColor ++ ")^r(4x13)^fg()") . dzenColor myUrgentTextColor myUrgentColor
    -- Filter out NSP Tag from Scratchpad
    , ppSort = fmap (.scratchpadFilterOutWorkspace) $ ppSort defaultPP
    }
    where
        pad = "^p(4)"

icon :: String -> String
icon x = "^i(" ++ myIconPath ++ x ++ ".xbm)"

-- replace 'find' with 'repl' in 'x'
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

----------------------------------------------------------------------
-- UrgencyHook

myUrgencyHook = withUrgencyHookC NoUrgencyHook urgencyConfig { suppressWhen = Focused }

----------------------------------------------------------------------
-- Run xmonad

main = do
    host <- fmap nodeName getSystemID
    dzen <- spawnPipe statusbarCmd
    xmonad $ myUrgencyHook
           $ defaultConfig
              { terminal           = myTerminal
              , focusFollowsMouse  = myFocusFollowsMouse
              , borderWidth        = myBorderWidth
              , modMask            = myModMask
              , numlockMask        = myNumlockMask
              , workspaces         = myWorkspaces
              , normalBorderColor  = myInactiveBorderColor
              , focusedBorderColor = myActiveBorderColor
--              , keys               = \k -> myOtherKeys `M.union` keys defaultConfig k
              , mouseBindings      = \m -> myMouse `M.union` mouseBindings defaultConfig m
    
              , handleEventHook    = ewmhDesktopsEventHook
              , layoutHook         = myLayoutHook
              , manageHook         = scratchpadManageHook mySPRect
                                 <+> manageDocks 
                                 <+> myManageHook
              , logHook            = myLogHook dzen
                                  >> ewmhDesktopsLogHook
              -- for compatibility with Java apps
              , startupHook = ewmhDesktopsStartup
                           >> setWMName "LG3D"
                           >> return ()
                           >> checkKeymap (defaultConfig) (myKeys)
              }
              `additionalKeysP` myKeys


--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--


import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Util.Run(spawnPipe)
import System.IO
import System.Exit

 
import XMonad.Layout.DragPane (dragPane, DragType(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.Tabbed
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.Minimize
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile

import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn
-- Custom actions
-- import XMonad.Actions.Testing

import XMonad.Util.Paste
import XMonad.Util.EZConfig


import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window (windowPromptGoto)
import XMonad.Prompt.AppendFile

import XMonad.Hooks.ICCCMFocus

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Control.Monad (liftM2)

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--



-- Run xmonad with the settings you specify. No need to modify this.
--

main = do
  xmbar <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"
  -- dzenProc <- spawnPipe myDzenStatus
  -- conky <- spawnPipe myDzenConky
  xmonad $ withUrgencyHook NoUrgencyHook
    $ ewmh defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = False,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
--        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        
	manageHook         = myManageHook <+> manageDocks <+> manageHook defaultConfig <+> doF avoidMaster, 
--        handleEventHook     = fullscreenEventHook,
        logHook            = do  
          takeTopFocus
          dynamicLogWithPP $ xmobarPP 
			     { ppOutput = hPutStrLn xmbar
			     --, ppHidden = const ""
			     , ppLayout = const ""
                             , ppSep = " "
                             , ppCurrent = xmobarColor "yellow" ""
                             , ppHidden = xmobarColor "#aaaaaa" "" 
--                             , ppHiddenNoWindows = xmobarColor "#777799" ""
			     , ppTitle =  xmobarColor "#ffaa00" ""  . shorten 70 
                             , ppUrgent = xmobarColor "red" "" .xmobarStrip
			     }
        -- logHook            = dynamicLogWithPP $ dzenPP 
        --                      {ppOutput = hPutStrLn dzenProc
        --                      , ppLayout = const ""
        --                      },
--        startupHook        = setWMName "LG3D"
                } --  `additionalKeysP` myEmacsStyleKeys





myTerminal = "exec starturxvt -C -sl 10000  -fn '-*-terminus-*-*-*-22-*-*-*-*-*-*-*'"
-- " -fb 'xft:Bitstream Vera Sans Mono:pixelsize=18'"

-- Width of the window border in pixels.
--
myBorderWidth   = 1

myModMask       = mod3Mask
modSongMask     = mod5Mask
modAppMask      = mod4Mask

-- myNumlockMask   = 0

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9", "0"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "orange" -- "#009999"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- myEmacsStyleKeys = [
--  ("M4-t", spawn $ XMonad.terminal conf),
--  ("M3-c", kill),
--  ("M3-<Space>", sendMessage NextLayout),
--  ("M3-S-<Space>", setLayout $ XMonad.layoutHook conf),
--  -- Minimize, maximize
--  ("M3-z", withFocused (\f -> sendMessage (MinimizeWin f))),
--  ("M3-S-z", sendMessage RestoreNextMinimizedWin),
--  -- Quit xmonad
--  ("M3-S-q", io (exitWith ExitSuccess)),
--  -- Restart xmonad
--  ("M3-M4-r", restart "xmonad" True) ]




myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modAppMask,        xK_t), spawn $ XMonad.terminal conf)
    , ((modAppMask,        xK_a), 
       spawn "aterm -C -sl 10000  -fn '-*-terminus-*-*-*-*-24-*-*-*-*-*-*-*'")

    -- close focused window 
    -- Default : modMask .|. shiftMask, xK_c 
    , ((modMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- Minimize, maximize
--    , ((modMask, xK_z), withFocused (\f -> sendMessage (MinimizeWin f)))
    , ((modMask .|. shiftMask, xK_z), sendMessage RestoreNextMinimizedWin)


    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_bracketleft    ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_bracketright     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_s     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap
    , ((modMask              , xK_b     ), sendMessage ToggleStruts) 

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask   .|. modAppMask          , xK_r     ), restart "xmonad" True)

    , ((mod1Mask, xK_F12), spawn "setxkbmap us; xmodmap ~/.xmodmap")
    , ((mod1Mask .|. shiftMask, xK_F12), spawn "setxkbmap us -variant colemak")

    , ((modMask, xK_F8), spawn "dzen-mpc-status")

    -- Custom
--    , ((modMask, xK_t), justso defaultXPConfig)
     
    -- Forcus Urgent
    , ((modMask, xK_u), focusUrgent)
    , ((modMask .|. shiftMask, xK_u), clearUrgents)

    -- Paste
    , ((modMask, xK_y), pasteSelection)

      
    -- LCD brightness control
    , ((modMask              , xK_F5), spawn "xbacklight -dec 1")
    , ((modMask              , xK_F6), spawn "xbacklight -inc 1")

    -- Resizable Tile
    , ((modMask             ,  xK_a), sendMessage MirrorShrink)
    , ((modMask             ,  xK_z), sendMessage MirrorExpand)

    -- Run gmrun
    , ((modMask  ,xK_r),  shellPrompt myXPConfig)
    , ((modMask  ,xK_w),  windowPromptGoto myXPConfig)    
    , ((modMask  ,xK_n),  appendFilePrompt myXPConfig "~/notes")
    -- screen shots

    , ((0, xK_Print), spawn "scrot  '%Y-%m-%d-%H:%M:%S_$wx$h.png' -e 'mv $f ~/.scrots' ")

    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s  -e 'mv $f ~/.scrots' '%Y-%m-%d-%H:%M:%S_$wx$h.png'")


    -- RaiseNext
    , ((modAppMask, xK_p), raiseNext (iconName =? "urxvt"))


    ]
    ++

    [
     ((modMask, xK_t), submap . M.fromList $ 
      [ ((modMask, xK_n), spawn "dzen-mpc-status")])]
    ++


--    mpcBindings modSongMask
    -- [
    --  ((modSongMask, xK_m),  submap . M.fromList $
    --   [ ((0, xK_i), spawn "mpc_status")
    --   , ((0, xK_n), spawn "mpc next")
    --   , ((0, xK_p), spawn "mpc prev")
    --   , ((0, xK_t), spawn "mpc toggle")
    --   , ((0, xK_space), spawn "mpc toggle")
    --   , ((0, xK_Left), spawn "mpc seek -1%")
    --   , ((0, xK_Right), spawn "mpc seek +1%")
    --   , ((0, xK_Up), spawn "mpc seek +10%")
    --   , ((0, xK_Down), spawn "mpc seek -10%")
    --   , ((0, xK_9), spawn "mpc volume -1")
    --   , ((0, xK_0), spawn "mpc volume +1")
    --   , ((0, xK_d), spawn "mpc del 0")
    --   ])
    -- ]


      [ -- ((0, xK_i), spawn "mpc_status")
        ((0, 0x1008ff17), spawn "mpc next")
      , ((0, 0x1008ff16), spawn "mpc prev")
      , ((0, 0x1008ff14), spawn "mpc toggle")
      , ((0, 0x1008ff26), spawn "mpc seek -1%")
      , ((0, 0x1008ff27), spawn "mpc seek +1%")
      , ((shiftMask, 0x1008ff27), spawn "mpc seek +10%")
      , ((shiftMask, 0x1008ff26), spawn "mpc seek -10%")
      , ((0, 0x1008ff11), spawn "mpc volume -1")
      , ((0, 0x1008ff13), spawn "mpc volume +1")
      -- , ((0, xK_d), spawn "mpc del 0")
      ]



    ++

-- mod4 bindings
    [
    ((modAppMask, xK_e), spawn "emacsclient -c -a ''")
    ,  ((modAppMask, xK_c), spawn "conkeror")
    ,  ((modAppMask, xK_w), runOrRaise "firefox" (className =? "Firefox"))
--    ,  ((modAppMask, xK_d), spawn "deluge")
    ,  ((modAppMask, xK_k), runOrRaise "okular" (className =? "Okular"))
--    ,  ((modAppMask, xK_r), spawnHere sp "chromium")
    ,  ((modAppMask, xK_l), spawn "xlock -mode life")
    ]
    ++


    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9]++[xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    --[((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


    -- ++
    [
    ((modMask, xK_p), toggleWS)
    ,((modMask, xK_l), nextWS)
    , ((modMask, xK_h), prevWS)
    , ((modMask, xK_e), moveTo Next EmptyWS)
    ]

iconName = stringProperty "WM_ICON_NAME"


myXPConfig = defaultXPConfig { font = "-*-terminus-*-*-*-*-24-*-*-*-*-*-*-*", 
                                      promptBorderWidth=1, borderColor="gray", height=35, position=Top }


-- dzen2

-- myDzenStyle=" -fn '-*-arial-bold-r-*-*-*-*-*-*-*-*-*-*'" 
-- myDzenStatus= "/usr/bin/dzen2 -w 800 -ta l" ++ myDzenStyle
-- myDzenConky="conky -c ~/.xmonad/conky-dzen | dzen2 -x 900 -w 400 -ta r" ++ myDzenStyle


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask , button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask , button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask , button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ layoutHints . smartBorders $  
           tiled |||  (Mirror tiled) ||| 
            Grid ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = ResizableTall nmaster delta ratio []

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
--     ratio   = 2/(1+(toRational (sqrt (5)::Double)))
     ratio = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     
--     gimp = withIM 0.11 (Role "gimp-toolbox") $ reflectHoriz $ withIM 0.15 (Role "gimp-dock") Full



------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll . concat $
               [
                [appName =? f --> doFloat | f <- floatAppNames ]
                , [className =? f --> doFloat | f <- floatClassNames ]
                , [ 
                className =? "Gimp"           --> (ask >>= doF.W.sink)
                --    , title =? "Extension"          --> doFloat
                --    , className =? "Dialog"         --> doFloat
                , resource  =? "desktop_window" --> doIgnore
                , resource  =? "kdesktop"       --> doIgnore 
                --    , className =? "Emacs"          --> doF avoidMaster
                , title =? "Mcabber"            --> doF (W.shift "9")
                , className =? "Linuxdcpp"      --> doF (W.shift "0")
                , className =? "stalonetray"    --> doIgnore
                , isFullscreen --> doFullFloat
--                , className =? "MPlayer" --> doShiftAndGo "9"
--                , className =? "Okular" --> doShift "8"
                ]
               ]

                where
                  floatAppNames = ["gnome-sudoku"]
                  floatClassNames = ["Realplay.bin"] -- ["MPlayer"]
--                  doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
                                  W.Stack t [] (r:rs) -> W.Stack t [r] rs
                                  otherwise           -> c


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
-- myLogHook = dynamicLogDzen
--
-- myLogHook = return ()

--myLogHook = dynamicLogWithPP $ xmobarPP
	--  { ppCurrent = xmobarColor "yellow" ""
--	  , ppHidden = const ""
--	  , ppLayout = \s -> case fromMaybe s $ stripPrefix "Hinted " s of
--                                               	"Mirror Tall"    -> "Wide"
 --                                              	s                -> s
          --, ppSep = " | "
	  --, ppTitle = xmobarColor "#aaaabb" "" . shorten 40
	  --, ppOutput = hPutStrLn xmproc
	  --, ppExtras = [ do
		--		i <- io $ readIORef floatNextWindows
			--	return $ Just $ if i == 0
			--	       	      	   then "-"
			--			   else show i
                       --]
         -- }



------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
------------------------------------------------------------------------

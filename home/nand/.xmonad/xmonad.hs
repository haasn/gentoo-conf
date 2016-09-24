-- Base imports
import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)

-- Needed for workspace cycling and switching
import XMonad.Actions.CycleWS (findWorkspace, WSType(..))
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.StackSet (shift, greedyView)

-- Layouts and navigation
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Layout.HintedGrid
import qualified XMonad.Layout.BinarySpacePartition as BSP
import qualified XMonad.StackSet as W

-- Misc and utility
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.List (isInfixOf)
import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPD
import System.Exit

main = do
    xmproc <- spawnPipe "exec xmobar"
    xmonad $ withUrgencyHook (NoUrgencyHook)
           $ fullscreenFix
           $ ewmh
           $ def {
        manageHook          = manageFloats <+> manageDocks <+> manageHook def,
        layoutHook          = smartBorders $ avoidStruts $ myLayout,
        handleEventHook     = fullscreenEventHook <+> docksEventHook,
        logHook             = dynamicLogWithPP xmobarPP {
            ppOutput  = hPutStrLn xmproc,
            ppTitle   = xmobarColor "white" "",
            ppCurrent = xmobarColor "#85c600" ""
        },

        modMask             = mod4Mask, -- Capslock

        normalBorderColor   = "#070B0C",
        focusedBorderColor  = "#85919b",

        focusFollowsMouse   = False,
        clickJustFocuses    = False,

        workspaces = workspaceNames 10

    } `additionalKeys` extraKeys

myLayout = nav BSP.emptyBSP ||| Full
    where nav :: LayoutClass l a => l a -> ModifiedLayout WindowNavigation l a
          nav = configurableNavigation noNavigateBorders

extraKeys =
    [ ((0, xK_Print), spawn "import -depth 8 -window root /mem/screengrab.png")
    , ((mod4Mask .|. mod3Mask             , xK_Return),  spawn "newwin")
    , ((mod4Mask .|. mod3Mask .|. mod1Mask, xK_Return),  spawn "newpriv")

    -- General movement stuff
    , ((mod4Mask, xK_h), sendMessage $ Go L)
    , ((mod4Mask, xK_j), sendMessage $ Go D)
    , ((mod4Mask, xK_k), sendMessage $ Go U)
    , ((mod4Mask, xK_l), sendMessage $ Go R)

    , ((mod4Mask .|. mod1Mask, xK_h), sendMessage $ Swap L)
    , ((mod4Mask .|. mod1Mask, xK_j), sendMessage $ Swap D)
    , ((mod4Mask .|. mod1Mask, xK_k), sendMessage $ Swap U)
    , ((mod4Mask .|. mod1Mask, xK_l), sendMessage $ Swap R)

    , ((mod4Mask .|. mod3Mask, xK_h), sendMessage $ BSP.ExpandTowards L)
    , ((mod4Mask .|. mod3Mask, xK_j), sendMessage $ BSP.ExpandTowards D)
    , ((mod4Mask .|. mod3Mask, xK_k), sendMessage $ BSP.ExpandTowards U)
    , ((mod4Mask .|. mod3Mask, xK_l), sendMessage $ BSP.ExpandTowards R)

    , ((mod4Mask .|. controlMask , xK_h), sendMessage $ BSP.ShrinkFrom R)
    , ((mod4Mask .|. controlMask , xK_j), sendMessage $ BSP.ShrinkFrom U)
    , ((mod4Mask .|. controlMask , xK_k), sendMessage $ BSP.ShrinkFrom D)
    , ((mod4Mask .|. controlMask , xK_l), sendMessage $ BSP.ShrinkFrom L)

    , ((mod4Mask .|. mod3Mask .|. mod1Mask, xK_h), sendMessage $ BSP.MoveSplit L)
    , ((mod4Mask .|. mod3Mask .|. mod1Mask, xK_j), sendMessage $ BSP.MoveSplit D)
    , ((mod4Mask .|. mod3Mask .|. mod1Mask, xK_k), sendMessage $ BSP.MoveSplit U)
    , ((mod4Mask .|. mod3Mask .|. mod1Mask, xK_l), sendMessage $ BSP.MoveSplit R)

    , ((mod4Mask                , xK_space), sendMessage BSP.Rotate)
    , ((mod4Mask .|. mod1Mask   , xK_space), sendMessage BSP.Swap)
    , ((mod4Mask .|. controlMask, xK_space), sendMessage BSP.Equalize)

    -- Fullscreen
    , ((mod4Mask, xK_f), sendMessage NextLayout)

    , ((mod4Mask, xK_Tab), nextWS')
    , ((mod4Mask .|. mod1Mask, xK_Tab), prevWS')

    -- Make it harder to kill X
    , ((mod4Mask .|. shiftMask, xK_q), return ())
    , ((mod1Mask .|. mod3Mask .|. mod4Mask, xK_q), io (exitWith ExitSuccess))

    -- Open a new tab (fuzzy search based on history)
    , ((mod4Mask, xK_t), spawn "exec /usr/local/bin/fuzzytab")
    , ((mod4Mask, xK_r), spawn "exec /usr/bin/rofi -show run")

    -- Lock the screen when not in use
    , ((mod4Mask, xK_s), spawn "exec /usr/local/bin/lock")

    -- Reset the mouse cursor
    , ((mod4Mask, xK_Escape), spawn "exec /usr/bin/swarp 0 0")

    , ((controlMask .|. mod3Mask, xK_Home),      io' $ MPD.withMPD MPD.toggle)
    , ((controlMask .|. mod3Mask, xK_Insert),    io' $ MPD.withMPD (MPD.play Nothing))
    , ((controlMask .|. mod3Mask, xK_End),       io' $ MPD.withMPD MPD.stop)
    , ((controlMask .|. mod3Mask, xK_Page_Down), io' $ MPD.withMPD MPD.next)
    , ((controlMask .|. mod3Mask, xK_Page_Up),   io' $ MPD.withMPD MPD.previous)

    -- Default keybindings, remapped to mod1Mask instead of shift
    , ((mod4Mask .|. mod1Mask, xK_Return),  spawn "/usr/bin/urxvtc")
    , ((mod4Mask .|. mod1Mask, xK_c),       kill)
    ]

    ++ [ ((mod4Mask .|. mod1Mask, m), screenWorkspace n >>=
                                      flip whenJust (windows . W.shift))
       | (n,m) <- zip [0..] [xK_w, xK_e]
       ]

    -- Switch workspaces using symbols
    ++ [ ((mod4Mask .|. m, k), windows $ f i)
       | (i, k) <- zip (workspaceNames 10)
         [ xK_exclam, xK_at, xK_numbersign, xK_dollar, xK_percent, xK_asciicircum
         , xK_ampersand, xK_asterisk, xK_bracketleft, xK_bracketright ]
       , (m, f) <- [(0, greedyView), (mod1Mask, shift)]
       ]

workspaceNames n = [ show x ++ ":" ++ case lookup x friendlyNames of
                       Nothing -> "──"
                       Just n  -> n
                   | x <- [1..n] ]

friendlyNames =
    [ (1, "\xF120")
    , (2, "\xF121")
    , (3, "\xF086")
    , (4, "\xF01D")

    , (7, "\xF130")
    , (8, "\xF019")
    , (9, "\xF269")
    , (10, "X")
    ]

-- Float exceptions
manageFloats = composeAll [ fmap (x `isInfixOf`) title --> doFloat
                          | x <- floatTitles ]

floatTitles =
    [ "Firefox Preferences", "About Firefox", "Resize Canvas"
    , "Downloads", "Software Update", "World of Warcraft", "Limbo"
    , "Audiosurf", "Audiosurf 2", "Heroes of the Storm", "scaler_test"
    , "Convert Script", "Remote Viewer"
    ]

switchWorkspace' d = wsBy' d >>= windows . greedyView
wsBy' = findWorkspace getSortByIndex Next HiddenNonEmptyWS

nextWS' = switchWorkspace' 1
prevWS' = switchWorkspace' (-1)

-- Fullscreen fixes. For some reason ewmh doesn't advertise _NET_WM_STATE_FULLSCREEN
fullscreenFix :: XConfig a -> XConfig a
fullscreenFix c = c { startupHook = startupHook c <+> setSupportedWithFullscreen }

setSupportedWithFullscreen :: X ()
setSupportedWithFullscreen = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_FULLSCREEN"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

io' :: MonadIO m => IO a -> m ()
io' = io . void

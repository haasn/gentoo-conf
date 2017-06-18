{-# LANGUAGE ViewPatterns #-}

-- Base imports
import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
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
import Data.Maybe (fromMaybe)
import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPD
import System.Exit

main = do
    xmproc <- spawnPipe "exec xmobar"
    xmonad $ withUrgencyHook (NoUrgencyHook)
           $ fullscreenFix
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
    , ((mod4Mask, xK_p), spawn "exec /usr/bin/rofi-pass")
    , ((mod4Mask, xK_u), withFocused $ windows . W.sink)

    -- Lock the screen when not in use
    , ((mod4Mask, xK_s), spawn "exec /usr/local/bin/lock")

    -- Reset the mouse cursor
    , ((mod4Mask, xK_Escape), spawn "exec /usr/bin/swarp 0 0")

    , ((0, 0x1008FF14), io' $ MPD.withMPD MPD.toggle)
    , ((0, 0x1008FF15), io' $ MPD.withMPD MPD.stop)
    , ((0, 0x1008FF17), io' $ MPD.withMPD MPD.next)
    , ((0, 0x1008FF16), io' $ MPD.withMPD MPD.previous)
    , ((0, 0x1008ff3e), addVolume (-5))
    , ((0, 0x1008ff97), addVolume 5)

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
    , "Audiosurf", "Audiosurf 2", "scaler_test"
    , "Convert Script", "Remote Viewer"
    ]

switchWorkspace' d = wsBy' d >>= windows . greedyView
wsBy' = findWorkspace getSortByIndex Next HiddenNonEmptyWS

nextWS' = switchWorkspace' 1
prevWS' = switchWorkspace' (-1)

addVolume :: MonadIO m => Int -> m ()
addVolume d = io' . MPD.withMPD $ do
    MPD.Status { MPD.stVolume = fromMaybe 70 -> vol } <- MPD.status
    let vol' = max 0 . min 100 $ vol + d
    MPD.setVolume vol'

-- Explicitly advertise _NET_WM_STATE_FULLSCREEN
fullscreenFix :: XConfig a -> XConfig a
fullscreenFix c = c { startupHook = startupHook c <+> setSupportedWithFullscreen }

setSupportedWithFullscreen :: X ()
setSupportedWithFullscreen = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_FULLSCREEN"]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

io' :: MonadIO m => IO a -> m ()
io' = io . void

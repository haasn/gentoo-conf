import Control.Monad (void)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Spacing

import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as W

import System.IO
import System.Posix.Files (touchFile)

import Data.Maybe (fromJust, maybeToList)
import qualified Data.Map as M (toList)
import Data.Ord (comparing)
import qualified Data.List as L

import Control.Monad.Error.Class (MonadError)
import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPD

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook (NoUrgencyHook)
           $ fullscreenFix
           $ defaultConfig {
        manageHook          = manageFloats <+> manageDocks <+> manageHook defaultConfig,
        layoutHook          = smartBorders $ avoidStruts $ myLayout,
        handleEventHook     = fullscreenEventHook <+> docksEventHook,
        logHook             = dynamicLogWithPP xmobarPP {
            ppOutput  = hPutStrLn xmproc,
            ppTitle   = xmobarColor "white" "",
            ppCurrent = xmobarColor "#85c600" ""
        },

        modMask             = mod4Mask, -- Capslock

        terminal            = "urxvtc",

        normalBorderColor   = "#0e1112",
        focusedBorderColor  = "#85919b",

        focusFollowsMouse   = False,
        clickJustFocuses    = False,

        workspaces = workspaceNames 9

    } `additionalKeys` extraKeys

myLayout = s (navi (GridRatio 1)) ||| navi (GridRatio 2) ||| Full
  where navi = configurableNavigation noNavigateBorders
        s = id -- spacing 32

extraKeys =
  [ ((0, xK_Print), spawn "scrot -e 'convert $f -red-primary 0.6881,0.3068 -green-primary 0.2214,0.7160 -blue-primary 0.1468,0.061 -white-point 0.312874,0.329226 +gamma 0.45454545 $f; optipng $f; mv $f ~/scrot/'")

  , ((mod4Mask, xK_h), sendMessage $ Go L)
  , ((mod4Mask, xK_j), sendMessage $ Go D)
  , ((mod4Mask, xK_k), sendMessage $ Go U)
  , ((mod4Mask, xK_l), sendMessage $ Go R)

  , ((mod4Mask .|. shiftMask, xK_h), sendMessage $ Swap L)
  , ((mod4Mask .|. shiftMask, xK_j), sendMessage $ Swap D)
  , ((mod4Mask .|. shiftMask, xK_k), sendMessage $ Swap U)
  , ((mod4Mask .|. shiftMask, xK_l), sendMessage $ Swap R)

--  , ((mod4Mask .|. shiftMask, xK_h), sendMessage Shrink)
--  , ((mod4Mask .|. shiftMask, xK_l), sendMessage Expand)

{-
  , ((mod4Mask, xK_Up), windows moveUp)
  , ((mod4Mask, xK_Down), windows moveDown)
  , ((mod4Mask, xK_Left), windows W.focusMaster)
  , ((mod4Mask, xK_Right), windows moveRight)
-}

  , ((mod4Mask, xK_Tab), nextWS')
  , ((mod4Mask .|. shiftMask, xK_Tab), prevWS')

    -- never terminate X please
  , ((mod4Mask .|. shiftMask, xK_q), return ())

  , ((mod4Mask, xK_r), spawn "$(yeganesh -x -- -fn 'Terminus-10' -i -nf '#daccbb' -nb '#0e1112')")

    -- lock the screen when not in use
  , ((mod4Mask, xK_s), spawn "slock")

    -- reset the mouse cursor
  , ((mod4Mask, xK_Escape), spawn "swarp 0 0")

  , ((controlMask .|. mod1Mask, xK_Home),      io . void $ MPD.withMPD MPD.toggle)
  , ((controlMask .|. mod1Mask, xK_Insert),    io . void $ MPD.withMPD (MPD.play Nothing))
  , ((controlMask .|. mod1Mask, xK_End),       io . void $ MPD.withMPD MPD.stop)
  , ((controlMask .|. mod1Mask, xK_Page_Down), io . void $ MPD.withMPD MPD.next)
  , ((controlMask .|. mod1Mask, xK_Page_Up),   io . void $ MPD.withMPD MPD.previous)
  ]
  -- Switch workspaces using symbols
  ++ [ ((mod4Mask .|. m, k), windows $ f i)
     | (i, k) <- zip (workspaceNames 9)
       [ xK_exclam, xK_at, xK_numbersign, xK_dollar, xK_percent, xK_asciicircum
       , xK_ampersand, xK_asterisk, xK_bracketleft ]
     , (m, f) <- [(0, W.greedyView), (shiftMask, W.shift)]
     ]

friendlyNames = [
        (1, "term"),
        (2, "editor"),
        (3, "irc"),

        (7, "skype"),
        (8, "torrent"),
        (9, "web")
    ]

workspaceNames n = map elem [1..n]
  where
    elem x = show x ++ (concatMap (':':) . maybeToList $ lookup x friendlyNames)

-- Float exceptions
manageFloats = composeAll $ fullF : [ title =? x `to` doFloat | x <- floatTitles ]
    where to = (-->)

floatTitles =
  [ "Firefox Preferences", "About Firefox", "Resize Canvas"
  , "Downloads", "Software Update", "World of Warcraft", "Limbo"
  , "Audiosurf", "Audiosurf 2", "Heroes of the Storm", "scaler_test"
  ]

-- Programs that should start in fullscreen mode. Normally EWMH handles this
-- properly, but eg. mpv does something weird on initial startup so we have to
-- do it manually. (Update: turns out mpv got fixed upstream)
fullTitles = [  ]

fullF = fmap (\t -> any (`L.isPrefixOf` t) fullTitles) title --> doFullFloat

-- Cycle focus inside the current stack
moveUp = W.modify' moveUp'
moveUp' s@(W.Stack _ [] _)          = s -- master is unchanged
moveUp' s@(W.Stack _ [_] _)         = s -- last before master is unchanged
moveUp' (W.Stack f (u:us) ds)       = W.Stack u us (f:ds) -- rest is moved

moveDown = W.modify' moveDown'
moveDown' s@(W.Stack _ [] _)        = s -- master is unchanged
moveDown' s@(W.Stack _ _ [])        = s -- bottom is unchanged
moveDown' (W.Stack f us (d:ds))     = W.Stack d (f:us) ds -- rest is moved

moveRight = W.modify' moveRight'
moveRight' (W.Stack m [] (d:ds))    = W.Stack d [m] ds -- master, move focus to top
moveRight' s@(W.Stack _ _ _)        = s -- only one window or not master

switchWorkspace' d = wsBy' d >>= windows . W.greedyView
wsBy' = findWorkspace getSortByIndex Next HiddenNonEmptyWS

nextWS' = switchWorkspace' 1
prevWS' = switchWorkspace' (-1)

{-
data AllFloat = AllFloat
  deriving (Read, Show)

instance SetsAmbiguous AllFloat where
  hiddens _ ws _ _ = map fst . M.toList $ W.floating ws
-}

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

  --setWMName "xmonad"

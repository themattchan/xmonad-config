Config
  { font		= "xft:Source Code Pro:style=Regular:size=8,FontAwesome:size=9"
  , bgColor		= "#ffffff"
  , fgColor		= "#c5c8c6"
  , border		= BottomB 2
  , borderColor		= "#c5c8c6"
  , position		= Static { xpos = 0 , ypos = 0, width = 1600, height = 20 }
  , lowerOnStart	= True
  , hideOnStart		= False
  , persistent		= True
  , allDesktops		= True
  , sepChar		= "%"
  , alignSep		= "}{"
  , template = " %StdinReader% }{ %vol% %brt% %wifi% %bat% %date%"
  , commands = [ Run Date "\xf073 %a %b %_d \xf017 %H:%M " "date" 10
               , Run StdinReader
               , Run Com "zsh" ["~/.xmonad/scripts/bat"] "bat" 30
               , Run Com "zsh" ["~/.xmonad/scripts/brt"] "brt" 15
               , Run Com "zsh" ["~/.xmonad/scripts/volume"] "vol" 10
               , Run Com "zsh" ["~/.xmonad/scripts/wifi"] "wifi" 15
               ]
  }

{-# LANGUAGE OverloadedStrings     #-}
import Control.Monad
import Control.Arrow ((>>>))
import qualified  Data.Text as T
import Turtle
{-
function touchpadon  { /usr/bin/xinput --enable $(xinput --list | grep -Eo 'TouchPad\s*id\=[0-9]{1,2}' | grep -Eo '[0-9]{1,2}') ; echo "touchpad enabled";}
function touchpadoff  { /usr/bin/xinput --disable $(xinput --list | grep -Eo 'TouchPad\s*id\=[0-9]{1,2}' | grep -Eo '[0-9]{1,2}') ; echo "touchpad disabled";}

device=`xinput --list | grep -Eo 'TouchPad\s*id\=[0-9]{1,2}' | grep -Eo '[0-9]{1,2}'`
state=`xinput list-props "$device" | grep "Device Enabled" | grep -o "[01]$"`

if [ $state == '1' ];then
  xinput --disable $device
else
  xinput --enable $device
fi
-}
toggleTouchpad :: IO ()
toggleTouchpad = sh $ do
  devId:_ <- match (has (text "id=" *> decimal)) <$>
             (grep (has (text "Touchpad")) $
               run "xinput" ["--list"])

  let devIdStr = T.pack (show devId)

  devState:_ <- match (suffix decimal) <$>
              (grep (has (text "Device Enabled")) $
               run "xinput" ["list-props", devIdStr])

  let flag = if devState == 1 then "--disable" else "--enable"
  run "xinput" [flag, devIdStr]
  return ()

run :: Text -> [Text] -> Shell Text
run prog args = inproc prog args (pure "")

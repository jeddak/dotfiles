#!/usr/bin/zsh

# Set up an icon tray

trayer --edge right --align top --SetDockType true --SetPartialStrut true --expand true --width 14 --transparent true --alpha 0 --tint 0x000000 --height 20 --monitor 1 &

# Set the background color<
 
#xsetroot -solid midnightblue

(sleep 4; /usr/local/bin/xflux -z 10522) &

#$HOME/.conky/startup.sh
#(sleep 5; conky -c $HOME/.conky/conkyrc0) &
#(sleep 5; conky -c $HOME/.conky/conkyrc1) &
#(sleep 5; conky -c $HOME/.conky/conkyrc2) &


sudo apt install xcape

# remap caps lock to control
setxkbmap -option ctrl:nocaps

# map caps lock to escape
#   -t Give a timeout in milliseconds.  If you hold a key longer than timeout a key event will not be generated.
xcape -e 'Control_L=Escape' -t 175


#!/bin/bash

# https://superuser.com/a/1695537/1226024
# $1 file basename. will optimize $1.gif to $1.small.gif 
optimize_gif() {
  ffmpeg -y -filter_complex "fps=30,scale=480:-1:flags=lanczos,split[s0][s1];[s0]palettegen=max_colors=32[p];[s1][p]paletteuse=dither=bayer" -i "$1".gif "$1".small.gif
}

# $1 orgname
github_clone_all_in_org() {
  for repo in $(gh repo list "$1" --json name -q .[].name); do gh repo clone "$1"/"$repo"; done
}



setup_base() {
  if [[ -v TERMUX_VERSION ]]; then
    apt_cmd=pkg
    dist_specific="termux-api golang ripgrep"
  else
    apt_cmd=sudo apt
    dist_specific=""
  fi

  $apt_cmd update
  $apt_cmd upgrade
  $apt_cmd dist-upgrade
  $apt_cmd install screen git wget openssl openssh mosh neovim mc tmux man

  [[ -n "$dist_specific" ]] && $apt_cmd install "$dist_specific"

  if [[ ! -f .ssh/id_rsa.pub ]]; then
    ssh-keygen

    echo "import key below to github and bitbucket" 
    echo "========================================"
    cat .ssh/id_rsa.pub
    echo "========================================"

    read -r -p "Press any key to continue"
  fi

  if [[ ! -d .git ]]; then
    git init
    git remote add git@github.com:SebastianZaha/dotfiles
    git pull
  fi
}

setup_arch() {
  pacman -S --needed \
    pkgstats \
    neovim screen git github-cli curl wget openssl openssh mc man sudo htop lsof \
    base-devel ripgrep go cmake \
    firefox \
    noto-fonts-emoji noto-fonts noto-fonts-extra noto-fonts-cjk \
    ttf-jetbrains-mono \
    pulseaudio pavucontrol
    # wayland foot sway waybar swappy grim slurp
}

setup_linux_desktop_x() {
  sudo apt install xcape

  # remap caps lock to control
  setxkbmap -option ctrl:nocaps

  # map caps lock to escape
  #   -t Give a timeout in milliseconds.  If you hold a key longer than timeout a key event will not be generated.
  xcape -e 'Control_L=Escape' -t 175
}



snap_clean() {
  # Removes old revisions of snaps
  # CLOSE ALL SNAPS BEFORE RUNNING THIS

  LANG=C snap list --all | awk '/disabled/{print $1, $3}' |
    while read -r snapname revision; do
      snap remove "$snapname" --revision="$revision"
    done
}



#installGo() {
#    echo "========================================"
#    echo "installing golang"
#    echo "========================================"
#
#    rm -rf ~/.local/go
#    read -p "Paste current golang download uri from https://go.dev/dl/ :" go_uri
#
#    wget $go_uri --output-document=go.gz
#    tar --directory=~/.local/ -xzf go.gz
#    rm go.gz
#}

# vim: ai:ts=2:sw=2:et

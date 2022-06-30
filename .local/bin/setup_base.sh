if [[ -v TERMUX_VERSION ]]; then
    apt_cmd=pkg
else
    apt_cmd=sudo apt
fi

$apt_cmd update
$apt_cmd upgrade
$apt_cmd dist-upgrade
$apt_cmd install screen git wget openssl openssh mosh neovim mc tmux man

ssh-keygen

echo "import key below to github and bitbucket" 
echo "========================================"
cat .ssh/id_rsa.pub
echo "========================================"

read -p "Press any key to continue"

git init
git remote add git@github.com:SebastianZaha/dotfiles
git pull

source .bashrc

echo "========================================"
echo "installing golang"
echo "========================================"

rm -rf ~/.local/go
read -p "Paste current golang download uri from https://go.dev/dl/ :" go_uri

wget go_uri --output-document=go.gz
tar --directory=~/.local/ -xzf go.gz
rm go.gz


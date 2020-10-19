# Roguelike
A roguelike game written in OCaml. Requires an XServer to run the game on MacOS and WSL. 

Installation instructions are as follows:

Since we are using the OCAML graphics library (https://github.com/ocaml/graphics),
a few extra steps will be necessary to get everything going.

Here's what you are installing if you're curious:
pkg-config: https://en.wikipedia.org/wiki/Pkg-config
libx11-dev: https://www.x.org/wiki/Documentation/ 
            https://packages.debian.org/sid/libx11-dev
For MacOS and WSL: You also need an X-server to allow the OCAML library to
interface with the window manager on your respective platform.

Linux:
1) Run 'sudo apt-get install -y pkg-config libx11-dev'
2) Navigate to inside the '.../roguelike' directory
3) Run 'make install'
4) Run 'make play'

MacOS:
NOTE: this part is untested because none of our group members own a Mac, but we
believe it will work.
1) Run 'sudo port -v install xorg-server' or download from https://www.xquartz.org/releases/XQuartz-2.7.11.html
2) Start XQuartz
3) Run 'sudo port install pkg-config libx11-dev'
4) Navigate to inside the '.../roguelike' directory
5) Run 'make install'
6) Run 'make play'

Windows Linux Subsystem:
1) Go to https://sourceforge.net/projects/xming/ and download + install XMing
2) Navigate to the root directory in WSL
3) Run 'nano .bashrc'
4) Scroll all the way to the bottom and add 'export DISPLAY=:0'. Then save and exit.
5) Restart WSL
6) Inside WSL, navigate to '/roguelike'
7) Run 'make install'
9) Run 'make play'

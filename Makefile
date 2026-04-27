all: hs cpp

hs: app/Main.hs
	cabal build

cpp: image.cc
	g++ -o image image.cc

hsimg: hs
	cabal run rayt img | wezterm imgcat

cppimg: cpp
	./image | wezterm imgcat

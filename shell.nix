with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "dev-environment";
  buildInputs = [ pkgconfig SDL2 SDL2_gfx SDL2_ttf ];
}
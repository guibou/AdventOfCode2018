with import (builtins.fetchTarball {
url = https://github.com/NixOS/nixpkgs/archive/3a393eecafb3fcd9db5ff94783ddab0c55d15860.tar.gz;
sha256 = "1r7pycxxjcz3idyl35am4b4rdh4h5srd5r7w8msy2sc1sv830r30";
}) {};
with pkgs.haskell.packages.ghc844;
developPackage {
  root = ./.;
}

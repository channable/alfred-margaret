{ pkgs }:
pkgs.glibcLocales.override {
  allLocales = false;
  locales = [ "C.UTF-8/UTF-8" ];
}

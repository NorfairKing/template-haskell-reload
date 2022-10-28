final: prev:
with final.haskell.lib;
{
  haskellPackages = prev.haskellPackages.override (old: {
    overrides =
      final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super: {
          template-haskell-reload = buildStrictly (self.callPackage ../template-haskell-reload { });
        }
      );
  });
}

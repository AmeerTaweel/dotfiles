# View configuration diff between last and current system builds
{
  coreutils-full,
  nvd,
  writeShellScriptBin,
  ...
}:
writeShellScriptBin "home-manager-change-summary" ''
  BUILDS=$(${coreutils-full}/bin/ls -d1v ''${XDG_STATE_HOME}/nix/profiles/home-manager-*-link | tail -n 2)
  ${nvd}/bin/nvd diff ''${BUILDS}
''

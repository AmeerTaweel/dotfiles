# Run program using Nvidia graphics card on hybrid systems.
# Based on https://nixos.wiki/wiki/Nvidia
{writeShellScriptBin, ...}:
writeShellScriptBin "nvidia-offload" ''
  export __NV_PRIME_RENDER_OFFLOAD=1
  export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
  export __GLX_VENDOR_LIBRARY_NAME=nvidia
  export __VK_LAYER_NV_optimus=NVIDIA_only
  exec "$@"
''

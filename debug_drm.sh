#!/bin/bash
echo 0x19F | sudo tee /sys/module/drm/parameters/debug # Enable verbose DRM logging
sudo dmesg -C # Clear kernel logs
dmesg -w >dmesg.log & # Continuously write DRM logs to a file, in the background
cargo run -p anvil -- --tty-udev &> anvil.log # Reproduce the bug, then exit sway
fg # Kill dmesg with Ctrl+C
echo 0 | sudo tee /sys/module/drm/parameters/debug # Disable DRM logging
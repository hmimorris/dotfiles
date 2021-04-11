#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Launch example
echo "---" | tee -a /tmp/polybar.log
polybar example 2>&1 | tee -a /tmp/polybar.log & disown

echo "Bars launched..."

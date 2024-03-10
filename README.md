
# fpPS4 [![CI](https://github.com/red-prig/fpPS4/actions/workflows/main.yml/badge.svg)](https://github.com/red-prig/fpPS4/actions) [<img src="https://img.shields.io/discord/1047920770225012769?color=5865F2&label=fpPS4&logo=discord&logoColor=white"/>](https://discord.gg/up9qatpX7M)

This emulator is still in the early stages of development and big games like the tripple A game still don't work, see the compatibility list for more details: https://github.com/red-prig/fpps4-game-compatibility/issues

If your game doesn't work don't create a new issue, check the compatibility list first.

If you want to know the details or just chat, welcome to the discord: https://discord.gg/up9qatpX7M

I am currently rewriting the emulator core in the [kern](https://github.com/red-prig/fpPS4/tree/kern) branch and until the work is completed, there will be no support for specific games.

# Donate: [<img src="icons/boosty.svg"/>](https://boosty.to/fpps4)

![Book logo](/icons/logo.png) 

#

  PS4 compatibility layer (emulator) written with Free Pascal
 
This project is currently at the beginning and started for fun.
 
### Building
- Free Pascal compiler: 3.3.1 (use fpcupdeluxe with trunk), x86_64 only. 
- Lazarus: 2.0.0 and higher, x86_64 only. 

### Minimum system requirements 

- OS: Windows 7 SP1 x64 and higher 
- CPU: x64, AVX2 support
- GPU: Vulkan API support

### Game compatibility tracker
https://github.com/red-prig/fpps4-game-compatibility/issues

### Control layout
To switch to borderless full screen mode, press Alt-Enter.

fpPS4 supports XInput-compatible gamepads natively. You can remap buttons by pressing Esc on the keyboard during emulation. 
Regardless, you can use a keyboard as a input.
A DualShock4 touchpad is emulated by the mouse.

# Keyboard layout:
PS4 Gamepad button              | Keyboard button
:------------                   | :------------
Left Stick Up                   |W
Left Stick Left                 |A
Left Stick Down                 |S
Left Stick Right                |D
Right Stick Up                  |I
Right Stick Left                |J
Right Stick Down                |K
Right Stick Right               |L
OPTIONS                   		  |Enter
Dpad Up                   		  |Arrow Up
Dpad Left                   	  |Arrow Left
Dpad Down                   	  |Arrow Down
Dpad Right                   	  |Arrow Right
Triangle                   		  |Numpad 8
Square                   		    |Numpad 4
Cross                   		    |Numpad 2
Circle                   		    |Numpad 6
L1                   			      |Q
L2                   			      |1
L3                   			      |Z
R1                   			      |E
R2                   			      |4
R3                   			      |C
10/03/2024

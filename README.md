# efb - Executing File Browser

## What is this?

A simple file browser, which runs a custom command when selecting a file.

I created it to be used as a file browser when I am coding in neovim, where I use
neovim remote (https://github.com/mhinz/neovim-remote) to spawn a neovim process 
in one terminal, and then I have efb in another terminal, which executes neovim 
remote to open new files in the allready open neovim window. 
By having efb in a seperate window, I can control it with my windowmanager, instead
of having it fixed to the same dimension as my neovim process.


## Features

- Run a custom command on selected file
- Expandable directories (by pressing e)
  - So you can see content of subdirectories
- Vim keybindings

## Todo 

- Implement Colors
- Implement more keybindings
  - 'h' and 'l' to go up and down in the file tree
  - 's' to enter search mode
- Search mode
- Custom command to execute
- Build out command line options
- Write command line help description
- ? Multiple different commands to execute with different key presses?

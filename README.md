# efb - Executing File Browser

*WARNING: This is very early in development*

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
- More keybindings
  - Number quantifiers to move X entries up or down
    - ? Show line numbers
  - PgUp and PgDn for moving further
  - H, M and L to move to beginning, middle and end of screen.
  - G and g(?g) to move to top or bottom of list
- Custom command to execute
- Build out command line options
- Write command line help description
- ? Multiple different commands to execute with different key presses?
- File management
  - Creation of new files
  - Renamimg of files
  - ? Deletion of files
- ? Showing Git status
  - Maybe could be implemented as "run this command, and mark the files it returns"
    This way it could work with more systems. And it would also fit well with custom
    commands, as custom commands would then be able to add files in git, etc.

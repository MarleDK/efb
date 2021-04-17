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

- Build out command line options
- Implement Colors
- More keybindings
  - Number quantifiers to move X entries up or down
    - ? Show line numbers
  - PgUp and PgDn for moving further
  - H, M and L to move to beginning, middle and end of screen.
  - G and g(?g) to move to top or bottom of list
  - Show and hide hidden files/folders
- ? Multiple different commands to execute with different key presses?
- File management
  - Creation of new file
  - Renamimg of file
  - ? Deletion of file
  - Copying of file
- ? Showing Git status
  - Maybe could be implemented as "run this command, and mark the files it returns"
    This way it could work with more systems. And it would also fit well with custom
    commands, as custom commands would then be able to add files in git, etc.
- ? Command mode - if you press ":" you could write a command to run in the shell
  - ? ability to paste the file name of the file on the cursor
  - ? ability to still move while writing a command. Or maybe just remember the command afterwards.
      This way multiple files can easily be used for the command
- Look into minimizing the binary size

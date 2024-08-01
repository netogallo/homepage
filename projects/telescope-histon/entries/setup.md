---
type: entry
name: Initial Setup
project: projects/telescope-histon/index.md
commit: 644bbada6de088751ffe62ee436d04b86e1b7b71
---

This first post gives some background and describes how the repository was initially set up. In Neovim, telescope
plugins are essentially a lua module which relies on the lua telescope module. So as long as the module can be
found by Neovim, it can be invoked in whatever way the user likes, such as a specific key binding.

## Module Loading in Neovim
Even though Neovim uses the lua interpreter, it configures the module search paths differently from the vanilla lua interpreter. In general, Neovim relies on the ['runtimepath' or 'rtp'](https://neovim.io/doc/user/options.html#runtimepath)' or 'rtp' to instruct lua where to find modules. This is exposed in lua as `vim.opt.rtp`. When Neovim starts, it will be filled up with some defaults (as described in the linked documentation). As long as the `lua/telescope` directory of this project is inside of any of the `lua` directories in the `rtp`, the module can be loaded by simply using `require('telescope')`.

A directory often used to install Neovim plugins is `~/.config/nvim/lua/`, so you can simply symlink the `lua/telescope` directory of this project to `~/.config/nvim/lua/telescope` and this should allow your Neovim installation to find this module. Furthermore, you can edit this project and see any updates by restarting Neovim. Feel free to use this approach if you wish to hack this plugin.

## Using Nix to run this plugin
If you don't want to mess with your Neovim configuration and keep things separate, this project supplies a shell.nix file which can be used with [Nix](https://nixos.org/) to run this plugin. If you are not familiar with Nix, I strongly recommend you to check it out. To summarize, Nix is a package manager that, unlike traditional package managers, allows one to have multiple versions of the same package and customize each version down to the build flags used by the compiler to build the package. It runs on many Unix-like systems, including Linux and MacOS, so you should be able to integrate it into your development workflow. Once you have nix installed, you can simply run `nix-shell --run nvim` and it will launch Neovim with telescope and this plugin installed without interfering with your current Neovim configuration.

Lets look at the shell.nix file to understand how this is achieved:
$code-include("shell.nix", "3", "9")$

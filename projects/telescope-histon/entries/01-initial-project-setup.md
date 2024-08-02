---
type: entry
name: 01-project-setup.html
project: projects/telescope-histon/index.md
commit: 21237b29b94047b3fc44aac981a74af06ff26db6
---
This first post gives some background and describes how the repository was initially set up. In Neovim, telescope
plugins are essentially a lua module which relies on the lua telescope module. So as long as the module can be
found by Neovim, it can be invoked in whatever way the user likes, such as a specific key binding.

## Creating the Plugin Module

As stated previously, all we need to do to create a new pluing is to create a lua module. In Lua,
module names follow the directory structure and a special file named `init.lua` is used to indicate
that a directory is a module (which might contain other modules).

For reasons that will become clear in the ["Module Loading in Neovim"](#module-loading-in-neovim)
section, the plugin's module will be placed in the `lua/histon` directory of this project.
There we can find the `init.lua` file which exports a single function called `setup` which
is the entry point for the plugin. The function is quite simple:

$code-include("lua/histon/init.lua", "lua", "35", "38")$

It simply imports the `histon.vimspector` module and passes it as an argument to the 
`vimspector_actions` function.

## The 'histon.vimspector' Module

The `histon.vimspector` module is a very boring module which has the sole purpose of exposing
the functionalities that [vimspector](https://github.com/puremourning/vimspector) whithin
lua. Vimspector is a pure vim plugin, as such it is written in vimscript. However, those
functions need to be called by directly evaluating vimscript using the `vim.cmd` function.
To avoid cluttering our code with vimscript, we will create a dedicated module for that
purpose and have other modules use it.

The `histon.vimspector` module resides in the `lua/histon/vimspector.lua` file. At this point
it is very simple and you can see the full file below:

$code-include("lua/histon/vimspector.lua", "lua", "1", "8")$

As you can see, it simply exports an object that cotains a function called `toggle_breakpoint`
which simply calls the `vimspector#ToggleBreakpoint` vim command.

## Module Loading in Neovim
Even though Neovim uses the lua interpreter, it configures the module search paths differently from the vanilla lua interpreter. In general, Neovim relies on the ['runtimepath' or 'rtp'](https://neovim.io/doc/user/options.html#runtimepath)' or 'rtp' to instruct lua where to find modules. This is exposed in lua as `vim.opt.rtp`. When Neovim starts, it will be filled up with some defaults (as described in the linked documentation). As long as the `lua/telescope` directory of this project is inside of any of the `lua` directories in the `rtp`, the module can be loaded by simply using `require('telescope')`.

A directory often used to install Neovim plugins is `~/.config/nvim/lua/`, so you can simply symlink the `lua/telescope` directory of this project to `~/.config/nvim/lua/telescope` and this should allow your Neovim installation to find this module. Furthermore, you can edit this project and see any updates by restarting Neovim. Feel free to use this approach if you wish to hack this plugin.

## Using Nix to run this plugin
If you don't want to mess with your Neovim configuration and keep things separate, this project supplies a shell.nix file which can be used with [Nix](https://nixos.org/) to run this plugin. If you are not familiar with Nix, I strongly recommend you to check it out.

To summarize, Nix is a package manager that, unlike traditional package managers, allows one to have multiple versions of the same package and customize each version down to the build flags used by the compiler to build the package. It runs on many Unix-like systems, including Linux and MacOS, so you should be able to integrate it into your development workflow.

Once you have nix installed, you can simply run `nix-shell --run nvim` and it will launch Neovim with telescope and this plugin installed without interfering with your current Neovim configuration.

Lets look at the shell.nix file to understand how this is achieved:
$code-include("shell.nix", "nix", "1", "5")$

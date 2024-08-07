---
type: entry
name: 01-project-setup.html
project: projects/telescope-histon/index.md
commit: 6cae813a29b0393107fb1c95fc4b5cb65e16b57
---
This first post gives some background and describes how the repository was initially set up.
In Neovim, telescope plugins are essentially a lua module which relies on the lua telescope
module. So as long as the module can be found by Neovim, it can be invoked in whatever way
the user likes, such as a specific key binding.

## Creating the Plugin Module

As stated previously, all we need to do to create a new pluing is to create a lua module. In Lua,
module names follow the directory structure and a special file named `init.lua` is used to indicate
that a directory is a module (which might contain other modules).

For reasons that will become clear in the ["Module Loading in Neovim"](#module-loading-in-neovim)
section, the plugin's module will be placed in the `lua/histon` directory of this project.
There we can find the `init.lua` file which exports a single function called `setup` which
is the entry point for the plugin. The function is quite simple:

$code-include("lua/histon/init.lua", "lua", "42", "45")$

It simply imports the `histon.vimspector` module and passes it as an argument to the 
`vimspector_actions` function.

## The "histon.vimspector" Module

The `histon.vimspector` module is a very boring module which has the sole purpose of exposing
the functionalities of [vimspector](https://github.com/puremourning/vimspector) whithin
lua. Vimspector is a pure vim plugin, as such, it is written in vimscript. However, those
functions need to be called by directly evaluating vimscript using the `vim.cmd` function.
To avoid cluttering our code with vimscript, we will create a dedicated module for that
purpose and have other modules use it.

The `histon.vimspector` module resides in the `lua/histon/vimspector.lua` file. At this point
it is very simple and you can see the full file below:

$code-include("lua/histon/vimspector.lua", "lua", "1", "8")$

As you can see, it simply exports an object that cotains a function called `toggle_breakpoint`
which simply calls the `vimspector#ToggleBreakpoint` vim command.

## Module Loading in Neovim
Even though Neovim uses the lua interpreter, it configures the module search paths differently
from the vanilla lua interpreter. In general, Neovim relies on the
['runtimepath' or 'rtp'](https://neovim.io/doc/user/options.html#runtimepath)' or 'rtp' to
instruct lua where to find modules. This is exposed in lua as `vim.opt.rtp`. When Neovim starts,
it will be filled up with some defaults (as described in the linked documentation). As long as the
`lua/telescope` directory of this project is inside of any of the `lua` directories of the `rtp`,
the module can be loaded by simply using `require('telescope')`.

A directory often used to install Neovim plugins is `~/.config/nvim/lua/`, so you can simply symlink
the `lua/telescope` directory of this project to `~/.config/nvim/lua/telescope` and this should allow
your Neovim installation to find this module. Furthermore, you can edit this project and see any updates
by restarting Neovim. This is a fine approach for development and testing, however, in the
["Using Nix to run this plugin"](#using-nix-to-run-this-plugin) section, I will show you how to create
a Nix shell to run this plugin automatically and without interfering with your current Neovim configuration.

## Integrating with Telescope

In Telescope, a menu which allows the user to select items is known as a
["picker"](https://github.com/nvim-telescope/telescope.nvim/blob/master/developers.md#first-picker).
The `telescope.pickers` module provides the functionality to create a picker. In addition to that,
we will also be using the `telescope.actions` and `telescope.finders` to build our picker. All
this is imported at the beggining of the `lua/histon/init.lua` file:

$code-include("lua/histon/init.lua", "lua", "1", "4")$

The picker will be created in the `setup` function by calling the `pickers.new` function. This
function takes two arguments: (1) a set of options which is currently unused and (2) an object
with specific fields that define a picker. Lets look at how those fields are set in this plugin:

The first field is the **prompt_tilte** field. This is the string that will be displayed at the
top of the picker once it gets opened. This is trivial:

$code-include("lua/histon/init.lua", "lua", "11", "11")$

The second field is the **finder**. The purpose of this field is to provide the set of items that will
be visible in the prompt. As you can imagine, Telescope is a very versatile plugin so there are lots of
ways to create this field. However, this is a simple plugin so we can rely on the `finders.new_table`
function to do most of the work.

### The "finders.new_table" Function

The `finders.new_table` function takes a single argument which is an object that must contain
two fields:

 1. `resuts`: This is an array of all of the values that constitute entries in the picker. These
    items can be any object (not just strings). In our case, we will use records that contain
    a field called `name` to hold the string to be displayed and a field called `action` which
    holds the function to be called if the item is selected (more oon that [later](#the-attach_mappings-field)). This currently
    looks like this:

$code-include("lua/histon/init.lua", "lua", "13", "18")$

2. `entry_marker`: As `results` can be any object, the picker must know how to convert said
    objects into items that can be displayed by the picker. To accomplish this, one must supply
    a function which takes any object from the list and maps it to an object whith the following
    fields:

    1. `display`: a field holding the `string` that will be displayed to the user.
    2. `ordinal`: a field holding the key that will be used to sort the items by the picker.
    3. `value`: a field holding the value that will be passed to the callback function which
       is invoked once the user makes a selection (more on that [later](#the-attach_mappings-field)).

    In our case, it looks like:

$code-include("lua/histon/init.lua", "lua", "19", "25")$

This completes the declaration of the `finder` field of the picker. The next step is to define
the `attach_mappings` field.

### The "attach_mappings" Field

Our picker now has the items to be displayed and we even defined the function to be called
when an item is selected. However, as things currently are, the picker will do nothing. We need
to tell the picker what to do when an item is selected.

In our case, we wish to call the function stored in the `action` field of the selected item. This
is done by adding some customization to the picker's prompt. We can do so by providing a
"customization function" in the `attach_mappings` field.

This function is very versatile and can be used to provide very advanced functions. However, in
our case, we just want to override what happens when an item is selected. To achieve this, we
will use the `actions` module. This module exports an object with multiple fields. We are
interested in the `select_default` field. This field has a method called `replace` which
overrides the default action performed when an item is selected.

We will call this method with a new function that will do two things:

 1. Close the picker, as this will no longer happen given we are overriding the default action.
 2. Call the function stored in the `action` field. To do this, we need to get the value of the
    selected item (which we defined in the `entry_marker` field). To do this, we will rely on
    the `get_selected_entry` function of the `telescope.actions.state` module.


For the time being, this is all the customization we require. Finally, the function we provided
to the `attach_mappings` field must return `true` as a result. What this indicates is that we
wish to preserve the defaults for all values that are not explicitly overriden. The code
of this section is shown below:

$code-include("lua/histon/init.lua", "lua", "27", "36")$

## Wrapping Up

All that remains is to call the `setup` method of the value we created using the `pickers.new`
function, which happens at the end of the `vimspector_actions` function.



## Using Nix to run this plugin
If you don't want to mess with your Neovim configuration and keep things separate, this project supplies a shell.nix file which can be used with [Nix](https://nixos.org/) to run this plugin. If you are not familiar with Nix, I strongly recommend you to check it out.

To summarize, Nix is a package manager that, unlike traditional package managers, allows one to have multiple versions of the same package and customize each version down to the build flags used by the compiler to build the package. It runs on many Unix-like systems, including Linux and MacOS, so you should be able to integrate it into your development workflow.

Once you have nix installed, you can simply run `nix-shell --run nvim` and it will launch Neovim with telescope and this plugin installed without interfering with your current Neovim configuration.

Lets look at the shell.nix file to understand how this is achieved:
$code-include("shell.nix", "nix", "1", "5")$

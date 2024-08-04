---
type: project
name: Telescope Histon
repository:
    github:
        owner: netogallo
        repo: telescope-histon
---
Telescope Histon is a very simple plugin to ease my transition into
[neovim](https://neovim.io/). Even though neovim is an awesome editor,
its user interface differes greatly from that of a traditional text editor.
In particular, it heavily relies on keyboard shortcuts rather than the
user interface to get things done. Even though I have no problem with
the basics, I have the tendency to forget the key combinations for
lots of features. The goal of this plugin is using telescope to
make life easir in this regard. The use cases being targeted are:

 - Interactions with language servers
 - Interacting with the debuggers

This plugiin aims to assist with this tasks in the following ways:
 - Provide a Telescope menu listing the available operations
 - Assign a hotkey to these operations when the menu is open and
   clearly show the hotkey in the menu.

Hopefully, this will make it easier for me to access these operations
if I forget the shortcut. Furthermore, thanks to the hotkey, all
the operations can be instantly accessed by simply triggering the
telescope menu and pressing the hotkey.



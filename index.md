---
type: page
name: Prelude
---
# Prelude
Welcome to netogallo's homepage. This is my little corner in the internet where I write about
my projects and other interesting things I come across. While working on my personal projects,
I often encounter challenges and learn about novel approaches to solve them. With the intent
of sharing these insights, I document my experiences in this site. The mateiral in this
site strives to be informative and hopeuflly it might help you in your own projects.

## My Programming Jazz

Like any software developer, I have my own "unbiased" and "objective" opinions on
how to develop software. This encompases the tools I use, languages and frameworks
I prefer and my overall workflow. You might probably have a different view on many
of these topics, but ultimately, Computer Science is grand and broad field where
similar domains have been approached with many different yet astonishing ideas.
Ultimately, it is always valuable to step outside of your confort zone.

I have done that many times myself as my general professional experience necesitated me
to use tools and technologies I would have not considered on my own. Ultimately, all these
experiences were inmensely valuable as they broadened and shaped my own views on software
development. I hope you can have a similar experience by reading my posts.

### Programming Languages and Technologies

I have a strong preference for statically typed languages. Generally speaking, I consider
that purely functional languages allow for more thorugh static analysis. This is not just
because functional languages (like Haskell) tend to have very advanced type systems, but
boils down to how these languages aquire their semantics which facilitates static analysis.
For this reason, I have extensively used:

 - Haskell
 - F#
 - Agda

All this being said, I am not a purist or fundamentalist and regularly use many other
programming languages. Some languages, even if not meant to be entirely functional, have
incorporated many ideas from that domain. Personally, I find the following quite outstanding
in that regard:

 - Rust (obviously)
 - C#
 - C++ (the modern standard)

When using languages that differ significantly from the functional paradigm, I still try
to enrich them as much as possible with the fliosophy of functional programming. This
applies to the tooling I use as well as my programming style. For instance, I have significant
experience with:

 - Python
 - Javascript
 - Java

As an example, when I use Python, I will always use it in conjunction with a static type-checker and annotate all functions with the appropiate types. Furthermore, I will not use inheritance
unless I am absolutely required to do so by virtue of a library or language feature. For
instance, I have worked a lot with QT, but that library is designed around inheritance so I
rather be pragmatic and follow the conventions established by the library rather than trying
to avoid inheritance in the name of philosophical purity.

### Design Patterns

My preference for functional programming extends beyond programming languages. I also use
functional programming to guide how to architect software and accomplish tasks which are
common in the software development process.

#### Functional Reactive Programming

When it comes to even handling, I like the ideas proposed by the
["Functional Reactive Programming"](https://wiki.haskell.org/Functional_Reactive_Programming)
paradigm. If possible, I will directly use FRP libraries like 
[dunai](https://github.com/ivanperez-keera/dunai) or [ReactiveX](https://reactivex.io/) to
implement the event handling components of my software. The main value proposition of FRP is
the philosophy it follows in order to handle events. To summarize a bit:

 - Describe event handling in a declarative style.
 - Provide a set of combinators to transform the events.
 - Provide an algebra to combine events and handlers.
 - Allow as much static analysis as possible
 - Permit reusability (this allows standalone testiing of differetn parts of the pipeline)

Even if it is not possible to use an FRP library (like iin the case of
["CBR Tools"](https://github.com/TUM-CBR/pymol-plugins), I will still strive to follow these
design principles. For that particular example, I created a minimal implementation of
[ReactiveX](https://reactivex.io/) which I extend over time as features are needed.

#### Context Managing

This is a broad topic which I discuss in more detail in my post
["Context Managing"](./posts/01-context-managing.html). In short, I prefer the ideas present
in functional languages over the traditional approaches found in OOP. Specifically, I like how
Haskell has tackled the problem with the use of 
[Typeclasses](https://book.realworldhaskell.org/read/using-typeclasses.html) and
[Monads](https://wiki.haskell.org/Monad). There have been many iterations of this apprach
since first introduced: [Monad Transformers](https://wiki.haskell.org/Monad_Transformers),
Free Monads, etc. Most recentlly, most recently my favorite approach is
[Polysemy](https://github.com/polysemy-research/polysemy). Ultimately, all approaches
follow the same general principles which is:

 1. Describe the properties of the context with a Monad
 2. Provide a mechanism to combine these contexts to construct more complex contexts
 3. Leverage on typelevel programming to describe what a function requires from the
    context withouth explicitly prescribing how it should be constructed.

In my humble opinion, these approaches offer a superior way to architect programs
compared to the traditional OOP approach of combining inheritance and composition. Sadly,
these approaches are very Haskell specific and not easily transferrable to other languages.
Nevertheless, I use these ideas as a guide when designing software in other languages.


### System Configuration

Gone are the days in which as software application or system was a single binary. Usually,
a whole ochrestra of programs are required to run alongside the main application. Furthermore,
the main program ofter relies on a specific configuration of these programs. There are many
approaches to solve this problem. In the recent years, the most popular approach has been
to use containers to describe and construct the environment in which an application will run.
Generally speaking, I advocate for this approach and many different technologies exist
to create and manage containers: Docker, Podman, Incus, etc.

Depending on the project and other circumstances, I will use one of these technologies
as they ultimately provide the same functionality. Nevertheless, regardless of which
of those I am using, I have a strong preference of using [Nix](https://nixos.org/) to
describe as much as possible of the system configuration.

In my opinion, what sets Nix apart from other technologies is that it almost entirely
eliminates the distinction between an "official package" and a "user package". In Nix,
you can create and host your own package repository simply by creatiing a git repository.
[Nixpkgs](https://github.com/NixOS/nixpkgs/) itself (which is the official Nix package
repository) is nothing more than a git repository. This is very convenient as it very
easily allows a team to have a private package repository to use across different projects.

The ease of creating repositories is not the only advantage that comes from the
afromentioned distinction. Another valuable feature this provides is the ability
to easily create customized versions of existing packages which can exist alongside
the main packages.

Take for instance the [Open3D](http://www.open3d.org/) library. In this
library, a single function, [TriangleMesh::ComputeUVAtlas](https://github.com/isl-org/Open3D/blob/0f06a149c4fb9406fd3e432a5cb0c024f38e2f0e/cpp/open3d/t/geometry/TriangleMesh.cpp#L706),
will lead to the inclusion of an additional dependency
([UVAtlas](https://github.com/microsoft/UVAtlas/)) in the final binary. If your project
does not need that single function, it is trivial to create a lighter version of Open3D
and use it with the same ease as the original package. You don't even need
to fork the original package or have your own package repository. The Nix expression used
for your project can simply apply overrides to the original package and create a new one.

# Context Managing

One of the hardest challenges in software development is creating a good abstraction to
manage the context of a program. Before delving into the topic, let me breifly describe
what I mean by context. Context refers the current state of a program which can affect
the result of a computation. Among the things encompassed by context are:

 - All variables in a program as well as their values.
 - Any handles available to the program such as open files or sockets
 - Any threads that might be running in the program

 Fundamentally, the outcome of any function in your program can, in principle, look
 at any variable or handle in the memory of the program and use it to compute its result.
 In practice, however, programming languages usually have some concept of scope which
 determines what variables are visible in a function, even if this is an artificial
 construct.

 In general, it is advisable (and desirable) for your functions (in any high-level
 programming language) to rely as little as necessary on the context to perform their
 task. This is mainly for two reasons:

 1. It is easier to reason about the behavior of a function as there are less factors
    which can influence said behavior. A corollary of this is that the functions are
    easier to test.
 2. It makes the function more re-usable, as any program whose context contains
    a subset of what the function needs can use it.

In practice, this requires for programming languages to provide an abstraction
to describe what properties must the context have in order for a function to
perform its task. To my knowledge, the most popular abstraction to manage
contexts is object-oriented programming (OOP).

Most programmers are familiar with OOP as it is usually taught alongside programming,
so there is no need to discuss it in detail. But to briefly summarize, OOP is the idea
of creating an explicit connection between data and functions that operate on said data.
This explicit connection is called a class and instances of this class, called objects,
can be created by giving some initial value to said data. The data (usually called state)
is often encapsulated in such a way that only the functions which are part of the class
have direct access to it.

While OOP remains a popular paradigm, it also comes with problems. A (very opinionated)
overview of its problems is described in this video:
[Object-Oriented Programming is Bad](https://www.youtube.com/watch?v=QM1iUe6IofM).  While
I don't agree with the entirety of the video, I do think there are better ways to solve
the context managing problem. In my opinion, the main problem with OOP is that it is
usually (but not always) coupled with inheritance which leads to rigid class hierarchies
which can become difficult to combine and extend.

Take for instance the [QT](https://www.qt.io/) library. Any object meant to be part of
the user interface must be a subclass of `QWidget`. This means that if you want to create
your own base class to expose the fundamental constituents of your application's context
to provide common functionality (such as logging), it must also be a subclass of `QWidget`
if it were to be used by the user interface code. Obviously, this is absurt and no serious
programmer would do this. This is usually approached by using composition, but that means
that we must provide every object in our application with a reference to the context object.
Furthermore, if we wish to extend the functions available in a particular context, that
functionality must either be added to the respective class (leading to lots of clutter) or
the objects must have a property for to access the context object so it can be passed
to other functions.

There are many solutions to these problems in the context of OOP. For example,
[Dependency Injetion](https://en.wikipedia.org/wiki/Dependency_injection) is often used
to solve the problem of providing objects with a reference to objects containing the
necessary context in such a way that it abstracts how that object is created.


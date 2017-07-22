---
layout: post
title:  Magical Thinking
draft: true
---

The Morgan And Grand Iron Clojure (MAGIC) compiler is a high-performance [Clojure][clojure] compiler, written as a Clojure library, targeting the [Common Language Runtime][clr].
It's goal is to provide tighter integration with its host virtual machine, better interoperation with C# libraries and APIs, and serve as a foundation to use Clojure in places not previously possible.

Today, MAGIC is a regular Clojure library that can run in any [ClojureCLR runtime][clojureclr] environment.
It provides facilities to compile functions and expressions to optimized bytecode while running along side normal Clojure code.
Its primary use is in the [Arcadia Project][arcadia], where MAGIC has been effective at bringing Clojure to the inner loop of video games and interactive graphics applications through better bytecode and reduced memory consumption.

In the long term, MAGIC aspires to become a standalone, potentially self-hosting Clojure implementation on the CLR, providing opportunities for deep control over bytecode from user code, and exploring the relationship between high-level garbage collected dynamically typed functional programming and low-level code statically typed code.

MAGIC is full of ideas I am very excited about, but until now I've only managed to share the thinking behind the compiler in a few [talks][clojurewest] and the occasional sporadic [tweet].
The hope is that this blog will become a repository for the theory behind MAGIC, and a place to document my ongoing progress.

## Audience & Background
In order to remain concise and valuable, certain assumptions are made about the audience of this blog.
Namely, I will avoid explaining the Clojure and [C#][cs] languages, and instead assume a working understanding of both.
Also, as [Arcadia][arcadia] is the primary use case for MAGIC, experience with it is helpful, but just understanding the goals of the project are enough to make sense of the references I will make to it here.

I will not assume a background in compiler theory or familiarity with the intricate details of the CLR, as those are much more specialized bodies of knowledge.
I will try link to papers and documentation whenever I can.

## Rationale
Why a new compiler?
The short answer is: Arcadia is built on the existing ClojureCLR project, and it has served us well, but to get as far as we want we need better control over the bytecode we're generating.

The primary motivator is performance, both in terms of run time and memory allocation.
ClojureCLR is more or less a straight port of Clojure on the JVM, and borrows most of its approach to optimization.
Mismatches between JVM and CLR semantics are a constant source of performance pain for us, particularly due to the demanding nature of game development.
We've contributed spot fixes to ClojureCLR when possible, but addressing the deep misassumptions it makes about its host necessitates a rewrite.
Without one, there will be a fairly low ceiling on how fast we can realistically expect Clojure game code to be.

In addition to performance, literal *control* over bytecode is something that has turned out to be very important for us.
As an example, a lot of ClojureCLR's optimizations around dynamic code are built on the [Dynamic Language Runtime (DLR)][dlr].
On platforms that support it, the DLR can make dynamic code quite fast by generating new call site bytecode at runtime.
However, some platforms *prohibit* the generation of code at runtime, and the DLR is [explicitly listed][nodlr] on Xamarin's documentation of iOS limitations.
Being able to choose which optimizations to apply based on the target platform is not currently possible in ClojureCLR, which is why Arcadia cannot export to iOS or the PlayStation.
MAGIC was designed with this class of problem in mind, and provides mechanisms to deal with it.

A new compiler was not the first thing we tried.
We spent months hacking on the original to fix type flow, introduce value types, and improve the memory usage of generated code.
Underneath the elegance, Clojure's compiler is built on traditional Object Oriented principles in tens of thousands of lines of C#.
The story is all too common, and the kind that drives many people to Clojure in the first place:
Manipulating type inference in one part of the compiler brakes method resolution somewhere else.
Adding logic to the type system means editing a few dozen files in *exactly* the same way.
Testing ideas means recompiling the compiler, which could take over a minute.
Failure means the compiler does not build at all, and you have to start again.
All the familiar charms of mutable object oriented programming are on full display here.
Not only did we miss our technical goals, but hacking the compiler was simply *not fun*.

In the end, using Clojure to build a Clojure compiler has proven to be a winning strategy.
MAGIC is built around functional programming and immutable data, and I develop it live in a REPL like any other Clojure project.
This lends it the simplicity and joy of work that has brought it this far with a single developer.

## A Taste of MAGIC
There is a lot of work left to do, but as of today MAGIC can compile most of Clojure and is already being used to optimize game code in the Arcadia community.
There is much more to say, and I will elaborate the ideas above and more in coming posts, but for now I will end with a taste of where the compiler is at.

The snippet below is an example of the kind of computation you might find in a video game: given an array of three dimensional vectors, it calculates their [centroid][centroid]. A million random [`Vector3`][v3]s are generated and passed to centroid functions written in the stock compiler and MAGIC. They are compared for accuracy, run time, and – in a REPL session with garbage collector tracing turned on – for the amount of garbage they generate.

```clojure
(ns user
  (:require [magic.api :as m]
            magic.intrinsics)
  (:import [UnityEngine Vector3]))

(def vs (into-array Vector3 (repeatedly 1e6 #(Vector3. (rand) (rand) (rand)))))

(defn centroid-stock [^Vector3|[]| vs]
  (Vector3/op_Division (areduce vs i ret (Vector3. 0 0 0)
                                (Vector3/op_Addition ret (aget vs i)))
                       (alength vs)))

(m/defn centroid-magic [^Vector3|[]| vs]
  (Vector3// (areduce vs i ret (Vector3. 0 0 0)
                      (Vector3/+ ret (aget vs i)))
             (alength vs)))

(= (centroid-stock vs) (centroid-magic vs))
;; true

(time (centroid-stock vs))
;; "Elapsed time: 203 msecs"

(time (centroid-magic vs))
;; "Elapsed time: 22 msecs"
```

```
user> (centroid-magic vs)
(0.5, 0.5, 0.5)
user> (centroid-stock vs)
Mono: GC_MINOR: (Nursery full) time 2.87ms, stw 2.91ms promoted 20K major size: 15456K in use: 12342K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 3.32ms, stw 3.36ms promoted 0K major size: 15456K in use: 12343K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.78ms, stw 2.82ms promoted 0K major size: 15456K in use: 12343K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 3.31ms, stw 3.34ms promoted 0K major size: 15456K in use: 12343K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.74ms, stw 2.77ms promoted 0K major size: 15456K in use: 12343K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 4.02ms, stw 4.07ms promoted 0K major size: 15456K in use: 12343K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 4.33ms, stw 4.38ms promoted 0K major size: 15456K in use: 12343K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.84ms, stw 2.88ms promoted 0K major size: 15456K in use: 12343K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.75ms, stw 2.79ms promoted 0K major size: 15456K in use: 12343K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.67ms, stw 2.71ms promoted 0K major size: 15456K in use: 12343K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.89ms, stw 2.93ms promoted 0K major size: 15456K in use: 12344K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.29ms, stw 2.32ms promoted 0K major size: 15456K in use: 12344K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.25ms, stw 2.28ms promoted 0K major size: 15456K in use: 12344K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.63ms, stw 2.67ms promoted 0K major size: 15456K in use: 12344K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.58ms, stw 2.62ms promoted 0K major size: 15456K in use: 12344K los size: 3930716K in use: 3930269K
Mono: GC_MINOR: (Nursery full) time 2.93ms, stw 2.97ms promoted 0K major size: 15456K in use: 12344K los size: 3930716K in use: 3930269K
(0.5, 0.5, 0.5)
```

MAGIC's version produces the same result an order of magnitude faster without generating any garbage, and while even providing nicer syntax for C#'s overloaded operators.

There is more to come.

Stay magical 🎩✨

[clr]: https://docs.microsoft.com/en-us/dotnet/standard/clr
[arcadia]: http://arcadia-unity.github.io/
[clojureclr]: https://clojure.org/about/clojureclr
[cbt]: http://www.braveclojure.com/
[ex]: https://clojuredocs.org/clojure.core/areduce#example-542692cec026201cdc326df8
[areduce]: https://clojuredocs.org/clojure.core/areduce
[cljdocs]: https://clojuredocs.org
[unity]: https://unity3d.com/
[centroid]: https://en.wikipedia.org/wiki/Centroid
[nodlr]: https://developer.xamarin.com/guides/ios/advanced_topics/limitations/#No_Dynamic_Code_Generation
[clojure]: http://clojure.org/
[clojurewest]: https://www.youtube.com/watch?v=eDad1pvwX34
[tweet]: https://twitter.com/ra/status/887399140220706816
[cs]: https://docs.microsoft.com/en-us/dotnet/csharp/csharp
[dlr]: https://docs.microsoft.com/en-us/dotnet/framework/reflection-and-codedom/dynamic-language-runtime-overview
[v3]: https://docs.unity3d.com/ScriptReference/Vector3.html

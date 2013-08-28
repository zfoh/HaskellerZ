ZuriHac 2013
============

## Zurich FP Afternoon:

    When:    Thursday, Aug 29th 2013
    Hours:   13:00 - 18:00
    Where:   ETH, Zurich, Switzerland

## ZuriHac 2013:

    When:    Friday, Aug 30th 2013 - Sunday, Sep 1st 2013
    Hours:   09:00 - 20:00
    Where:   Erudify, Zurich, Switzerland


**Important**: Switzerland has [its own power sockets][sockets]. We
can't provide converters for everybody so make sure to bring one
along. Do note that the Europlug will fit in a Swiss power
socket. There's an electronics shop [Fust Center Eschenmoser][fust] very near
our office where you can buy converters.

[sockets]: http://en.wikipedia.org/wiki/AC_power_plugs_and_sockets#Swiss_SEV_1011_.28Swiss_10.C2.A0A.2F250.C2.A0V.29.28Type_J.29
[fust]: https://maps.google.ch/maps?q=Fust+Center+Eschenmoser&hl=en&cid=6177809817081314569&gl=CH&t=m&z=16&iwloc=A

## Sponsors

  - [Erudify](https://erudify.com/) will host the Hackathon

  - [Google Switzerland](http://www.google.com/about/jobs/locations/zurich/) are providing financial and food support

  - [ETH](http://eth.ch/) will host the Zurich FP Afternoon.


## Location

  * The Zurich FP Afternoon will be hosted at [ETH, building: CAB, room: G61][cabg61], address: [Universitätstrasse 6, Zürich][cabg61a].

  * ZuriHac will be hosted at the Erudify offices, [Grüngasse 19, Zürich][gg19].

[cabg61]: http://www.rauminfo.ethz.ch/Rauminfo/grundrissplan.gif?region=Z&areal=Z&gebaeude=CAB&geschoss=G&raumNr=61
[cabg61a]: https://maps.google.com/maps?q=cab+zurich&hl=en&ll=47.378151,8.548077&spn=0.001041,0.005845&sll=37.0625,-95.677068&sspn=84.226897,191.513672&hq=cab&hnear=Zurich,+Canton+of+Zurich,+Switzerland&t=m&fll=47.378178,8.548114&fspn=0.009212,0.023378&layer=c&cbll=47.378152,8.548077&panoid=p0nIrEwC3UaWPZmRJjLN6g&cbp=11,64.77,,0,0&z=19
[gg19]: https://maps.google.ch/maps?q=Gr%C3%BCngasse+19,+Z%C3%BCrich&hl=en&ie=UTF8&ll=47.372947,8.525573&spn=0.008734,0.009559&sll=47.373386,8.524925&layer=c&cbp=13,222.55,,0,0.06&cbll=47.373504,8.525083&hnear=Gr%C3%BCngasse+19,+Kreis+4,+8004+Z%C3%BCrich&t=h&z=17&iwloc=A&panoid=vqb-ZHsYZ3d-SIhVYQeEoQ


### Getting Around

[Google map with important places](https://maps.google.ch/maps/ms?msa=0&msid=215847268401510829939.0004decda4c3d2e76270f)

Local transport links: 

http://www.zvv.ch/en/

**Getting to the FP Afternoon Talks on the 29th from the Airport**

Follow signs to the Tram 10 from the Airport arrivals. Take the Tram 10 direct to the stop "ETH/Universitätsspital"

**Getting to the Zurich Youth Hostel from the Airport**

Take any train that stops at Zurich Bahnhof Hardbrücke, transfer to the 33 Trolley bus going direction Morgental. Take 33 to Jugendherberge and you will see the Hostel.


**Getting from the Youth Hostel to the FP Afternoon Talks**

Walk to Morgental and take the number 7 Tram direction Bahnhof Stettbach. Change to the Number 10 or Number 6 Tram at the stop "Central" (direction Flughafen/Oerklion and Zoo) and get off at "ETH/Universitätsspital"

**A note on day tickets:**

Day tickets in Zurich run for 24 hours and can be used on all forms of transport, bus, train, tram.


### Accomodation

We have spoken with the local Youth Hostel in Zurich and they will be able to group Haskellers together if you mention the code "ZuriHac2013" when booking and when you check in.

The website to book is here http://www.youthhostel.ch/en/hostels/zurich

**Please be advised we do not have a hold on rooms so you should book sooner rather than later if you will attend! Erudify and the Youth Hostel can't guarantee space**

The Hostel has good public transport links to the Hackathon and downtown Zurich.

Transport between the Hostel and Hackathon is very easy with the Trolley Bus 33.

Other options we recommend are Wimdu or Airbnb.

## Schedule


### 13:00 Registration

Food and drinks provided by Google

### 13:30 Keynote: The Haxl Project at Facebook - Simon Marlow, Facebook

Writing code in an environment as large and complex as Facebook involves talking to many different back-end services, including databases, search engines, caches, and so forth. Each of these data sources has different characteristics and requires different access patterns to make best use of its resources. Typical front-end code will need to access many of these resources concurrently, but the programmer doesn't want to be concerned with the details of how best to access each resource. Moreover, we want to be able to write our code in a modular way and yet have concurrent accesses to the back-end services automatically batched and overlapped for us.

This talk describes a system that we're building using Haskell that allows the front-end programmer writing business logic to access all the back-end services in a concise and consistent way, while the system handles batching and overlapping of requests to multiple data sources behind the scenes. The programming model is one of implicit concurrency: there's no fork or async operation, all external data access is implicitly performed in parallel where possible.

### 14:30 Break

Food and drinks provided by Google


### 15:00 Functional programming at Silk - Erik Hesselink

At Silk we've been using functional programming for about 4 years now. We've used it to build a web application, coding in Haskell and Javascript. I will explain what we do, how we do it, and what we've found to be the strengths and weaknesses of Haskell and Javascript.

### 15:35 Ceh, power-user environments to share - Gergely Risko

It's a common problem to share user environments (editor, compiler, browser, etc.) between GNU/Linux computers: laptop on the move and desktop at home, or different engineering workstations of a product team. This is usually solved by using the GNU/Linux distribution's package manager. That fails if the
different engineers use different distributions or different versions. Ceh, building on Nix provides the same version controlled, reproducible power-user environment on top of any modern GNU/Linux, be it 64-bit or 32-bit. What makes it interesting for Haskellers is the fact that Nix applies the concept of purity and laziness on the filesystem level and package management level, and Ceh also has excellent GHC support with ~80 cabal packages included.

### 16:10 Break

Food and drinks provided by Google

### 16:40 A virtual machine for Haskell - Thomas Schilling

The Glasgow Haskell Compiler is a very good static compiler, but the code it produces is often large and it can be difficult to predict performance.

Furthermore, profiling a program often requires recompiling the program and all libraries it uses. This talk describes Lambdachine, a virtual machine and trace-based just-in-time (JIT) compiler for Haskell, which attempts to address these problems. Lambdachine reuses GHC for type checking and (optional) static optimisations, and complements it with additional optimisations performed at runtime based on the program's behaviour. This talk describes some of the challenges of the design and implementation of Lambdachine.

### 17:15 Concurrent Haskell in the browser with GHCJS - Luite Stegeman

GHCJS is a Haskell to JavaScript compiler that supports many modern Haskell features, including lightweight threads, exceptions, Template Haskell and STM. Since It's based on GHC, we also get all recent type system extensions for free!

In this talk I will explain how GHCJS hooks into the GHC pipeline to translate Haskell to JavaScript and discuss the challenges of compiling to a high level language. We will see how we can interact with JavaScript code from the Haskell world, in particular how GHCJS lets us avoid callback hell for HTTP requests and event handling in the browser.

### 17:50 Prizegiving

Announcement of Google prizes


## ZuriHac

The official hacking hours for ZuriHac are 10:00 to 20:00 on Friday, 09:00 to 20:00 on Saturday and Sunday.

Please come before 11:00 on Friday. At 11:00 there will be projects introduction and discussion.

If the weather is good during ZuriHac we will stop a little earlier to go to Lake Zurich to have a BBQ. Make sure to pack your swimming suit!


## Organizers

The events are organized in name of the Zurich HaskellerZ meetup group by the following people:

  - Ales Pospisil (Erudify)
  - Alexander Bernauer (Google)
  - Bas van Dijk (Erudify)
  - Dominic Small (Erudify)
  - Michela D'Amelio (Erudify)
  - Simon Meier (Erudify)
  - Thomas Schilling (Erudify)

For any questions or emergencies, you can always call Bas at +41 791 285 624 or send an email to zurihac@erudify.com


HaskellerZ Meetup Group
=======================

*Main [HaskellerZ](http://www.meetup.com/HaskellerZ/) meetup page.*

This repository mainly hosts the slides from past meetups.
The README moreover serves as a drafting document for upcoming meetups.

Slides from past meetups
========================

2012-01-19: [Simon Meier](https://github.com/meiersi) - The bytestring library ([handout](https://github.com/meiersi/HaskellerZ/blob/master/meetups/20120119-The_bytestring_library/bytestring-talk.md))

2013-01-24: Alexander Bernauer - Advanced API design ([slides](https://docs.google.com/presentation/d/1wLsW5xzFL1A8JPY3BwXpFCnad8Ri3dFomgySP76c75I))

2013-03-21: 
  [Simon Meier](https://github.com/meiersi) - An Introduction to GHC's Haskell execution model 
  ([slides](http://htmlpreview.github.com/?https://github.com/meiersi/HaskellerZ/blob/master/meetups/20130321-Intro_GHC_RTS/finished_talk/slides.html), 
   [handout](http://htmlpreview.github.com/?https://github.com/meiersi/HaskellerZ/blob/master/meetups/20130321-Intro_GHC_RTS/finished_talk/handout.html))

2013-04-25:
  Gergely Risko - Cmdline libraries, intro to TemplateHaskell
  ([slides](http://htmlpreview.github.com/?https://github.com/meiersi/HaskellerZ/blob/master/meetups/20130425-HFlags_and_Template_Haskell/final/hflags.html),
   [handout](http://htmlpreview.github.com/?https://github.com/meiersi/HaskellerZ/blob/master/meetups/20130425-HFlags_and_Template_Haskell/final/hflags-handout.html))

2013-05-30: Mihaly Barasz - Intro to the [`lens`](http://hackage.haskell.org/package/lens) package.

2013-06-27: 
  [Thomas Schilling](https://github.com/nominolo) and 
  [Simon Meier](https://github.com/meiersi) - A hands-on introduction to stream fusion.
  ([examples](https://github.com/meiersi/HaskellerZ/tree/master/meetups/2013-06-27-Stream_Fusion),
   papers: [ICFP 2007](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.104.7401) and 
          [ICFP 2013](http://research.microsoft.com/en-us/um/people/simonpj/papers/ndp/haskell-beats-C.pdf);
   more [ICFP 2013 papers](https://github.com/gasche/icfp2013-papers)
  )

Upcoming meetups and the main speakers
======================================

2013-07-25: Arvin Moezzi - Theorems for free.

**2013-08-29: Zurich FP Afternoon (http://www.haskell.org/haskellwiki/ZuriHac2013)**

  - Simon Marlow     - Keynote for the Zurich FP Afternoon
  - Thomas Schilling - Lambdachine: a JIT compiler for Haskell 

**2013-08-30 - 2013-09-01: ZuriHac 2013** (Yay, [register here](http://www.haskell.org/haskellwiki/ZuriHac2013))

2013-09-26: [Maximilien Rzepka](https://github.com/maxrzepka), [Bas van Dijk](https://github.com/basvandijk): Clojure / Haskell : how to compute with probabilities. 

2013-10-31: ??

In general, we'll have a HaskellerZ meetup every last thursday of January until October every year.
Note that we welcome guests and will happily adjust our schedule if they would like to give a talk.

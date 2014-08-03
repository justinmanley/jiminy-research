jiminy research 
===============

This repository contains research on the effect of GPS usage on smartphone battery life.  Let's push the limits of what location-aware mobile applications can do!

Research goals
--------------

The core goal of this research project is simple.  I want to know:

> If a user runs my location-aware app on her phone, how long will it take for her battery to run down completely?

Location-aware apps that must run constantly in the background in order to deliver the desired user experience are the largest consumers of smartphone battery life.  Mobile users are sensitive to battery life.  If using a location-aware app consistently runs down the phone's battery before the end of the day, users are likely to drop the app, or to only run it when *absolutely* necessary.

Developers building location-aware apps should therefore be interested in the effect that their app will have on battery life.  In particular: 

> What percentage of smartphone users can use my location-aware app, given its effect on their phone's battery life?

Methodology
-----------

Code
-----------


[Easy JSON Parsing with Haskell](http://blog.raynes.me/blog/2012/11/27/easy-json-parsing-in-haskell-with-aeson/)

[Cabal sandbox](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html#future-work)


Run with:

```
ghci -no-user-package-db -package-db .cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d 
```

Literature
-----------

[Battery Life Estimator Manual: Linear Modeling and Simulation](ftp://mojca.iems.northwestern.edu/Yue%20Geng/Energy/on%20modeling/battery%20life.pdf)

[Exhausting Battery Statistics: Understanding the Energy Demands on Mobile Devices](https://www.cl.cam.ac.uk/~nv240/papers/held03k-vallinarodriguez.pdf)

[A Stochastic Model for Energy Consumption Entailed by Mobile Device Proliferation](http://people.duke.edu/~mat39/MCMPaper.pdf)

[Battery Modeling](http://doc.utwente.nl/64556/1/BatteryRep4.pdf)

[MathWorks: Battery](http://www.mathworks.com/help/physmod/sps/powersys/ref/battery.html)

	* Understand Battery State-of-Charge (SOC)
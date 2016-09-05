Notes from Mat (February, 2015)
===============================

Script
------
The stuff is a mixture of PHP and bash (shell) scripting. The plan is to move this to a database when we look into distributing the load across multiple hosts. Creating a new “app” will involve describing the components to the system, so that each server can recreate the containers and source repositories as required. I am using the name of the “target” for the named applications to determine the specific code base being used, and from that derive which of the containers is required for an application. In theory I could handle numerous different containers, including (say) JVM hosts, python/django platforms, etc - shiny + R is what we use at the moment, but there is no reason why this has to always be the case.

Countdown Mechanism
-------------------
The mechanism provides a safety margin for when things get busy. A new docker instance takes milliseconds to create. Reloading the web configuration should also only take milliseconds, but can be slow when a number of different things happen at the same time. Shiny-server should only take milliseconds to load once the instance is up and running.
When I first built it, I did not know how the server would cope when a number of people used it at the same time, so allowed 10 seconds (user time) for each new instance.

I think there needs to be some kind of pause before throwing the user into the instance -when there was no delay, every now and then (for some reason or other), a client would try to connect while the nginx service was restarting and would be handed a 404 -reloading the page fixed this. A delay avoids this sort of thing.

If my ideas for the clustering are developed, then there may be a delay where hosts sort out who is going to handle a user request (database lookups + web service calls).

I’ll have a think about what I can do with this - adjusting a counter and throwing text at an html element is a fairly low load from the client perspective -nice and easy.

I’ll have a look at resizing a block element to show progression.

The size of the whole thing is determined by the stuff behind, I use the large block to hold vertical size at around the same scale as what you see once the countdown is over.

Docker
------
The web provides an interface, in the engine room I'm creating a virtual computer environment to manage process separation. This is dynamic. Rather than being a load balanced distribution of running instances, each new instance is created, then deployed via a proxy (hence the need to update the web server configuration - the reason I chose nginx for this).

This was originally a prototype, and I had little idea how a live application deployed in this fashion would scale.

The monitoring I have done suggests the loading is not a factor when we're dealing with dozens of instances, so may be we could live with a shorter imposed delay.

# Meal Planner/Prolog Vue.js Integration Proof-of-Concept

This project is an experiment in developing a way to make reactive webapps with Prolog.
It is currently using Vue.js for the client-side.

## install

Query 

   pack_install(css_write).
   pack_install(list_util).


## to run
To run, load `server.pl` and use the `go/1` predicate to start the server, providing the port to run on (e.g. `go(8081).`).

The app uses [Pengines][pengines] to interact with the client.

The goal is to design something that lets one mainly write in Prolog, but have a nice & reactive client-side experience.
Currently, the developer must write a fair amount of Javascript and must include a fair amount of Vue-specific stuff in the HTML to be generated.
The current big question is how best to design a way of hiding this incidental stuff & allow the developer to write mostly Prolog (plus some sort of HTML-generating DCG)?

  [pengines]: http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pengines.html%27)

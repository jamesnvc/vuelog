# Prolog Vue.js Integration Proof-of-Concept

This project is an experiment in developing a way to make reactive webapps with Prolog.
It is currently using Vue.js for the client-side.

## install

Query

```prolog
pack_install(css_write).
pack_install(list_util).
```

## to run

To run, load `example_server.pl` and use the `go/1` predicate to start the server, providing the port to run on (e.g. `go(8081).`).

The app uses [Pengines][pengines] to interact with the client.

The goal is to design something that lets one mainly write in Prolog, but have a nice & reactive client-side experience.
Currently, the developer must write a fair amount of Javascript and must include a fair amount of Vue-specific stuff in the HTML to be generated.
The current big question is how best to design a way of hiding this incidental stuff & allow the developer to write mostly Prolog (plus some sort of HTML-generating DCG)?

  [pengines]: http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pengines.html%27)


## To do:

In no particular order,

 - [ ] Documentation
 - [ ] Integrate the writing of the Pengine api module better with the writing of the front-end (right now they are entirely separate & the author just needs to know to set things up like the example project).
 - [ ] Allow for server-side pre-rendering (perhaps generate template tags as well as the static data passed in, so the page can be generated on the server like normal HTML, but also include enough information for Vue to take over client-side)
 - [ ] Make the HTML DSL look less Vue-specific
 - [ ] Have some story for purely client-side state (that is, have some state without needing to round-trip to the server)
 - [ ] Gracefully handle network failures or remote pengine errors
 - [ ] Compile some of the Prolog to JS & have it run client-side? 😱
 - [ ] Implement more Vue constructs/parameters to components
 - [ ] Expose more [Vue methods](https://vuejs.org/v2/api/#Global-Config)
 - [ ] Integrate with [`pack(identity)`](https://github.com/Anniepoo/identity)

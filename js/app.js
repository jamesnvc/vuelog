function updateState(state) {
  let stateJson = Pengine.stringify(state);
  new Pengine({application: "meals_app",
               ask: `handle_event(${stateJson}, inc_meals, S)`,
               onsuccess: function() {
                 console.log("SUCCESS", this.data);
                 console.log("new info ", this.data[0].S);
                 const newState = this.data[0].S;
                 for (let k in newState) {
                   app[k] = newState[k];
                 }
               }});
}

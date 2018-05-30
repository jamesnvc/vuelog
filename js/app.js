new Pengine({application: "meals_app",
             ask: "handle_event(_{a: 1}, x, S)",
             onsuccess: function() { console.log("SUCCESS", this.data); }});

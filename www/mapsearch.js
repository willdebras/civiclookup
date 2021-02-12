//Shiny.addCustomMessageHandler('mapInit', function(init) {
  
        
//          var map = L.map('map', {
//            layers: MQ.mapLayer()
//            });  

  
//});


Shiny.addCustomMessageHandler('keyUpdate', function(rann) {
  
  Shiny.onInputChange("keyPressed", rann);
  
});



Shiny.addCustomMessageHandler('addressSanitized', function(addy)  {

      if('map' === undefined) {var map = null;}

      //if (map !== undefined) { map.remove(); }
      //if(map !== undefined) {map = null;}
      //document.getElementById('map').innerHTML = "<div id='map'></div>";
      
      
      if (map === undefined) {
        
          map = L.map('map', {
          layers: MQ.mapLayer()
            }); 
        
      }
 
        
      

        
    
    
    var sanaddy = addy.replace(/ /g, "%20");

      MQ.geocode().search(addy)
          .on('success', function(e) {
              var best = e.result.best,
                  latlng = best.latlng;

              map.setView(latlng, 16);

              L.popup({ closeButton: false })
                  .setLatLng(latlng)
                  .setContent('<strong>' + addy + '</strong><br/>' + '<a href=http://www.google.com/search?q=' + sanaddy + ' target=\"_blank\"' + '>Open</a>')
        			//.setContent('http://www.google.com/search?q=' + sanaddy)
        			    //.setContent("Text")
                  .openOn(map);
          });
  });
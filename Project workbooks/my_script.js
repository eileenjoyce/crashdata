// setup the messaging from Shiny to Javascript
// global variable, initially "empty"

var data_from_shiny = [];

var chart = bb.generate({
  data: {
    columns: data_from_shiny, //this is the data you got from shiny
    type: "bar",
    types: {
      Deaths: "bar",
      'Deaths per 100,000 registered motorcycles': "line"
    }
  },
     axis: {
      x: {
      type: "category",
        tick: {
        rotate: 75,
        multiline: false
       },  
        categories: [
          "1997",
          "1998",
          "1999",
          "2000",
          "2001",
          "2002",
          "2003",
          "2004",
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016"  
        ]
      }
  },
    
  bindto: "#CombinationChart",
  tooltip: {grouped: false}
    
});



Shiny.addCustomMessageHandler("get_data_from_shiny",

    function(message){

    // get the data you sent from shiny here
    data_from_shiny = JSON.parse(message);

    // put the data you sent from shiny into the billboard chart columns
    chart.load({columns: data_from_shiny});
 });

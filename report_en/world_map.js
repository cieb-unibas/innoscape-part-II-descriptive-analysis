d3.csv("https://raw.githubusercontent.com/cieb-unibas/innoscape-part-II-descriptive-analysis/master/Data/trad_data.csv", function(err, rows){

function filter_and_unpack(rows, key, year) {
  return rows.filter(row => row['year'] == year && row['variable'] == 'share_in_tot2').map(row => row[key])
  }

function unpack(rows, key) {
        return rows.map(function(row) { return row[key]; });
    }

// Default Country Data
setCtryPlot('world');

function setCtryPlot(chosenCountry) {
  var frames = [];
  var slider_steps = [];
  var n = 9;
  var num = 1992;
  
  for (var i = 0; i <= n; i+=1) {
    var value = filter_and_unpack(rows, 'value', num).map(x => x * 1)
    var locations = filter_and_unpack(rows, 'par.code', num)
    var text = filter_and_unpack(rows, 'Country', num)
    
    frames[i] = {data: [{z: value, locations: locations, text: text, size: value}], name: num}
    slider_steps.push ({
        label: num.toString(),
        method: "animate",
        args: [[num], {
            mode: "immediate",
            transition: {duration: 500},
            frame: {duration: 800}
          }
        ]
      })
    num = num + 3
  }




// Different paramters mobile / desktop
if( /Android|webOS|iPhone|iPad|Mac|Macintosh|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent) ) {
var width_plot = 500;
var heigth_plot = 250;

} else{
  
var width_plot = 900;
var heigth_plot = 450;

}


var  data = [{
     // type: 'scattergeo',
    //  mode: 'markers',
      hovertemplate:  '<b>%{text}</b>' + 
                      //'<br><b>Share of Swiss Pharma-Exports: %{z}</b>' + 
                      '<extra></extra>',
      type: 'choropleth', 
      locationmode: 'europe',
      locations: frames[0].data[0].locations,
      text: frames[0].data[0].text,
      z: frames[0].data[0].z,
      zauto: false,
      zmin: 0,
      zmax: 0.26,
      colorscale: [[0,'rgb(178, 24, 34)'],[1,'rgb(220, 220, 220)']],     autocolorscale: false,
     reversescale: true,
  showscale: false
}];

if(chosenCountry == 'world'){
var geo_var = {
       scope: chosenCountry,
       countrycolor: 'rgb(255, 255, 255)',
       showland: true,
       landcolor: 'rgb(217, 217, 217)',
       showlakes: false,
       lakecolor: 'rgb(255, 255, 255)',
       subunitcolor: 'rgb(255, 255, 255)',
       lonaxis: { range: [-200, 220] },
       lataxis: { range: [-55, 80] },
    showframe: false,
    showcoastlines: false
    };
} else {
var geo_var = {
       scope: chosenCountry,
       countrycolor: 'rgb(255, 255, 255)',
       showland: true,
       landcolor: 'rgb(217, 217, 217)',
       showlakes: false,
       lakecolor: 'rgb(255, 255, 255)',
       subunitcolor: 'rgb(255, 255, 255)',
    showframe: false,
    showcoastlines: false
    };  
}   
   
   

var layout = {
  dragmode: false, 
  xaxis: {fixedrange: true},
  yaxis: {fixedrange: true},
 autosize: true,
 width: width_plot,
 height: heigth_plot, 
    title: '',
    yanchor: 'left',
    xanchor: 'left',
    margin: {r:20, t:0, l:0, b:20, pad: 0},
    geo: geo_var,
    updatemenus: [{
      x: 0.1,
      y: 0,
      yanchor: "top",
      xanchor: "right",
      showactive: false,
      direction: "left",
      type: "buttons",
      pad: {"t": -412, "r": 0, "l": 0},
      buttons: [{
        method: "animate",
        args: [null, {
          fromcurrent: true,
          transition: {
            duration: 10,
          },
          frame: {
            duration: 1000
          }
        }],
        label: "Play"
      }, {
        method: "animate",
        args: [
          [null],
          {
            mode: "immediate",
            transition: {
              duration: 0
            },
            frame: {
              duration: 0
            }
          }
        ],
        label: "Pause"
      }]
    }],
    sliders: [{
      active: 0,
      steps: slider_steps,
      x: 0.15,
      len: 0.9,
      xanchor: "top",
      y: 0,
      yanchor: "top",
      pad: {t: -50, b: 10, r: 50, l:0},
      currentvalue: {
        visible: true,
        prefix: "Year:",
        xanchor: "right",
        font: {
          size: 18,
          color: "#666"
        }
      },
      transition: {
        duration: 200,
        easing: "linear"
      }
    }]
};

const config = {
  displayModeBar: false, // this is the line that hides the bar.
};


Plotly.newPlot('myDiv', data, layout, config).then(function() {
    Plotly.addFrames('myDiv', frames);
  });
};

var chosenctry = document.getElementById("select_1");

function updateCountry(){
        setCtryPlot(chosenctry.value);
    }
    
chosenctry.addEventListener('change', updateCountry, true);
  
  
    
})

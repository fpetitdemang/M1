function onLoad() {
   var eventSource = new Timeline.DefaultEventSource();
   var bandInfos = [
     Timeline.createBandInfo({
         eventSource:    eventSource,
         date:           "Jun 28 2006 00:00:00 GMT",
         width:          "25%", 
         intervalUnit:   Timeline.DateTime.MONTH, 
         intervalPixels: 100,
     }),
     Timeline.createBandInfo({
        overview:       true,
         eventSource:    eventSource,
         date:           "Jun 28 2006 00:00:00 GMT",
         width:          "25%", 
         intervalUnit:   Timeline.DateTime.MONTH, 
         intervalPixels: 100
     }),
     Timeline.createBandInfo({
        overview:       true,
         eventSource:    eventSource,
         date:           "Jun 28 2006 00:00:00 GMT",
         width:          "25%", 
         intervalUnit:   Timeline.DateTime.MONTH, 
         intervalPixels: 100
     }),
     Timeline.createBandInfo({
        overview:       true,
         eventSource:    eventSource,
         date:           "Jun 28 2006 00:00:00 GMT",
         width:          "25%", 
         intervalUnit:   Timeline.DateTime.MONTH, 
         intervalPixels: 100
     })
   ];
   bandInfos[0].syncWith = 1;
   bandInfos[1].syncWith = 2;	
   bandInfos[2].syncWith = 3;
   bandInfos[1].highlight = true;
   
   tl = Timeline.create(document.getElementById("my-timeline"), bandInfos);
   Timeline.loadXML("example1.xml", function(xml, url) { eventSource.loadXML(xml, url); });
 }
 
 /*
function onLoad() {
var eventSource = new Timeline.DefaultEventSource();
  var bandInfos = [
         Timeline.createBandInfo({
            date:           "Jun 28 2006 00:00:00 GMT",
            width:          "35%", 
            intervalUnit:   Timeline.DateTime.MONTH, 
            intervalPixels: 100,
            eventSource:    eventSource,
            zoomIndex:      10,
            zoomSteps:      new Array(
              {pixelsPerInterval: 280,  unit: Timeline.DateTime.HOUR},
              {pixelsPerInterval: 140,  unit: Timeline.DateTime.HOUR},
              {pixelsPerInterval:  70,  unit: Timeline.DateTime.HOUR},
              {pixelsPerInterval:  35,  unit: Timeline.DateTime.HOUR},
              {pixelsPerInterval: 400,  unit: Timeline.DateTime.DAY},
              {pixelsPerInterval: 200,  unit: Timeline.DateTime.DAY},
              {pixelsPerInterval: 100,  unit: Timeline.DateTime.DAY},
              {pixelsPerInterval:  50,  unit: Timeline.DateTime.DAY},
              {pixelsPerInterval: 400,  unit: Timeline.DateTime.MONTH},
              {pixelsPerInterval: 200,  unit: Timeline.DateTime.MONTH},
              {pixelsPerInterval: 100,  unit: Timeline.DateTime.MONTH} // DEFAULT zoomIndex
            )
        }),
	
		Timeline.createBandInfo({
            date:           "Jun 28 2006 00:00:00 GMT",
            width:          "35%", 
            intervalUnit:   Timeline.DateTime.MONTH, 
            intervalPixels: 100,
            showEventText:  false, 
            trackHeight:    0.5,
            trackGap:       0.2,
            eventSource:    eventSource,
            overview:       true
        }),
	

	
	
        Timeline.createBandInfo({
            date:           "Jun 28 2006 00:00:00 GMT",
            width:          "30%", 
            intervalUnit:   Timeline.DateTime.YEAR, 
            intervalPixels: 200,
            showEventText:  false, 
            trackHeight:    0.5,
            trackGap:       0.2,
            eventSource:    eventSource,
            overview:       true
        })
	

  ];
  bandInfos[2].syncWith = 0;
  bandInfos[1].syncWith = 2;
  bandInfos[1].highlight = true;
  
  tl = Timeline.create(document.getElementById("my-timeline"), bandInfos);
  Timeline.loadXML("example1.xml", function(xml, url) { eventSource.loadXML(xml, url); });
  
}*/
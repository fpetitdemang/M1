 function dynTest(){
    document.getElementsByClassName = function(cl, element)
{
    var retnode = [];
    var myclass = new RegExp('\\b'+cl+'\\b');
    var elem = element.getElementsByTagName('*');
    for (var i = 0; i < elem.length; i++)
    {
        var classes = elem[i].className;
        var testRegExp = new RegExp("neutre");
        //if (testRegExp.test(classes)) console.log(classes);
        console.log(classes + " : " +document.compareDocumentPosition(elem[i]));
        if (myclass.test(classes))
            retnode.push(elem[i]);
         
    }
    return retnode;
};

   var tabItem = document.getElementsByClassName("positif", document);
   console.log(tabItem);
}






function onLoad() {
	var eventSource = new Timeline.DefaultEventSource();
	//modification theme
	
	var maFonction = function(){ 
	console.log(this);
	return "images/red-circle.png";
	};
	
	
	var theme = Timeline.ClassicTheme.create(); // create the theme
        theme.event.instant.icon = maFonction();//avec un fonction anomyme pour charger dynamquement le chmein de l'image
	//theme.event.instant.icon = "images/red-circle.png";
	
	
	var dateEvent = new Date();
      	dateEvent.setTime(dateEvent.getTime() + ((Math.floor(Math.random()*41) - 20) * 24 * 60 * 60 * 1000));
	
	var evt = new Timeline.DefaultEventSource.Event(
         dateEvent, //start
         dateEvent, //end
         dateEvent, //latestStart
         dateEvent, //earliestEnd
         true, //instant
         "Event ", //text
         "Description for Event ", //description
         "images/red-circle.png"
      );
      
      eventSource.add(evt);
      eventSource.add(evt);
	
	//console.log(maFonction());
   
   var bandInfos = [
     Timeline.createBandInfo({
         eventSource: eventSource,
         date: "Jun 28 2006 00:00:00 GMT",
         width: "70%",
         theme: theme,
         intervalUnit: Timeline.DateTime.MONTH,
         intervalPixels: 100,
     }),
     Timeline.createBandInfo({
        overview: true,
         eventSource: eventSource,
         date: "Jun 28 2006 00:00:00 GMT",
         width: "30%",
         intervalUnit: Timeline.DateTime.MONTH,
         intervalPixels: 100
     }),
   ];
   bandInfos[0].syncWith = 1;
 
   
   tl = Timeline.create(document.getElementById("my-timeline"), bandInfos);
   Timeline.loadXML("example1.xml", function(xml, url) { eventSource.loadXML(xml, url); });
  
   console.log(eventSource);
   

   //dynTest();

   
   
 }
 /*
 
 function onLoad() {

  var eventSource = new Timeline.DefaultEventSource();
  //Generate 50 random events up to 20 days in the past or the future
  for(var i=0;i<50;i++) {
      var dateEvent = new Date();
      dateEvent.setTime(dateEvent.getTime()+((Math.floor(Math.random()*41) - 20) * 24 * 60 * 60 * 1000));
      var evt = new Timeline.DefaultEventSource.Event(
         dateEvent, //start
         dateEvent, //end
         dateEvent, //latestStart
         dateEvent, //earliestEnd
         true, //instant
         "Event " + i, //text
         "Description for Event " + i, //description
        "images/red-circle.png",
         "images/red-circle.png",
                "images/red-circle.png",
                "red",
                "blue"
      );
      eventSource.add(evt);
      console.log(dateEvent);
  }
  //create the timeline
  var bandInfos = [
    Timeline.createBandInfo({
        trackGap:       0.2,
        width:          "70%",
        intervalUnit:   Timeline.DateTime.DAY,
        intervalPixels: 50,
        eventSource: eventSource
    }),
    Timeline.createBandInfo({
        showEventText:  false,
        trackHeight:    0.5,
        trackGap:       0.2,
        width:          "30%",
        intervalUnit:   Timeline.DateTime.MONTH,
        intervalPixels: 150,
        eventSource: eventSource
    })
  ];
  bandInfos[1].syncWith = 0;
  bandInfos[1].highlight = true;
  tl = Timeline.create(document.getElementById("my-timeline"), bandInfos);

}

var resizeTimerID = null;
function onResize() {
    if (resizeTimerID == null) {
        resizeTimerID = window.setTimeout(function() {
            resizeTimerID = null;
            tl.layout();
        }, 500);
    }
}
 */
 




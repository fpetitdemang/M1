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

	//modification theme
	var theme = Timeline.ClassicTheme.create(); // create the theme
        //theme.event.instant.icon = function(){ return "images/red-circle.png"; };//avec un fonction anomyme pour charger dynamquement le chmein de l'image
	//theme.event.instant.icon = "images/red-circle.png";
	console.log(function(){ return "images/red-circle.png";});
   var eventSource = new Timeline.DefaultEventSource();
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
   
   //dynTest();
   
   
 }
 
 




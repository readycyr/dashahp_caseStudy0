/* =================================================== */
var treeData = [
  {
    "name": "Top Level",
    "parent": "null",
    "children": [
      {
        "name": "Level 2: A",
        "parent": "Top Level",
        "children": [
          {
            "name": "Son of A",
            "parent": "Level 2: A"
          },
          {
            "name": "Daughter of A",
            "parent": "Level 2: A"
          }
        ]
      },
      {
        "name": "Level 2: B",
        "parent": "Top Level",
                "children": [
          {
            "name": "Son of B",
            "parent": "Level 2: B"
          },
          {
            "name": "Daughter of B",
            "parent": "Level 2: B"
          }
        ]
      }
    ]
  }
];
/* =================================================== */
var boxHeight = 60;//
var boxWidth = 130;//
var fontSize = 10;
var lineSpace = 2;
var renderOut = "tree";
var treeOrientation = "vertical";//
var goZoo = true;
var el = "div#myTree";//
var margin = {top: -5, right: -5, bottom: -5, left: -5};//
var width = 500, height = 500;//
/* =================================================== */
/*
Creates a new tree layout with the default settings: 
the default sort order is null; 
the default children accessor assumes each input data is an object with a children array; 
the default separation function uses one node width for siblings, and two node widths for non-siblings;
the default size is 1×1.
*/
var hierarchy = '';
if ( renderOut == "tree") { 
    hierarchy = d3.layout.tree();
} else {
    hierarchy = d3.layout.cluster();
}
hierarchy.nodeSize([boxWidth*1.5, boxHeight*2]);
//hierarchy.size([height, width - 160]);

/** The default comparator is null, which disables sorting and uses tree traversal order. 
 * Note that if no comparator function is specified to the built-in sort method, the default order is lexicographic
* function comparator(a, b) { return a.name < b.name ? -1 : a.name > b.name ? 1 : 0; }
* 
* The current default separation function
* function separation(a, b) { return (a.parent == b.parent ? 1 : 2); }
*//* TREE.sort(comparator).separation(separation); */

var svg = d3.select(el).append("svg")
        .attr("width", width)//+ margin.left + margin.right)
        .attr("height", height)//+ margin.top + margin.bottom)
        .append("g");
/* 
svg.append("rect")
    .attr("width", width)
    .attr("height", height)
    .style("fill", "grey")
    .style("pointer-events", "all");
*/
var zoom = d3.behavior.zoom().scaleExtent([1, 10]).on("zoom", zoomed);
function zoomed() {
        svg.append("g").attr("transform", "translate("+ d3.event.translate +") scale("+ d3.event.scale + ")");
}

if (treeOrientation == "vertical") {
    svg.attr("transform", "translate("+ (0+margin.left)+","+(0+margin.right)+")"+" rotate(90) scale(1)")
    .call(zoom);
} else {
    svg.attr("transform", "translate(1500,50) rotate(0) scale(0.5)")
    .call(zoom);
}

if ( renderOut == "tree" ) {
} else {}

var drag = d3.behavior.drag()
    .origin(function(d) { return d; })
    .on("dragstart", dragstarted)
    .on("drag", dragged)
    .on("dragend", dragended);
    
function dragstarted(d) {
  d3.event.sourceEvent.stopPropagation();
  d3.select(this).classed("dragging", true);
}
function dragged(d) {
  d3.select(this).attr("x", d.x = d3.event.x).attr("y", d.y = d3.event.y);
}
function dragended(d) {
  d3.select(this).classed("dragging", false);
}

d3.json("./flare.json", function(error, json) {
  if (error) throw error;

/* https://github.com/mbostock/d3/wiki/Tree-Layout
Runs the tree layout, returning the array of nodes associated with the specified root node. 
The tree layout is part of D3's family of hierarchical layouts.
*/
    root = treeData[0];
    var nodes =  hierarchy.nodes(root), // nodes(json)
/* Given the specified array of nodes, such as those returned by nodes, 
returns an array of objects representing the links from parent to child for each node.
*/   links =  hierarchy.links(nodes);

    /* https://www.dashingd3js.com/svg-paths-and-d3js */
    var link = svg.selectAll("path.link").data(links).enter().append("path")
    .attr("class", "link");
    
    var diagonal = d3.svg.diagonal();
    
    if ( renderOut == "tree") {
        diagonal = function() {
            var mprojection = function(d) { return [d.y, d.x]; };
            var mpath = function(pathData) { return "M" + pathData[0] + ' ' + pathData[1] + " " + pathData[2]+ " " + pathData[3]; };
            // link.attr("d", function(d) { return  "M" + (d.source.y/2) + "," + (d.source.x) + "H" + (d.target.y/2)+ "V" + (d.target.x);});
            function mdiagonal(diagonalPath, i) {
                    var source = diagonalPath.source,
                    target = diagonalPath.target,
                    turnPointY = target.y/2,
          pathData = [source, {x: source.x, y: turnPointY}, {x: target.x, y: turnPointY}, target];
          pathData = pathData.map(mprojection);
          return mpath(pathData);
        }
        return mdiagonal;
      };
    } else {
          // Straitgh curve!
          diagonal.projection(function(d) { return [d.y, d.x]; });
    }
    link.attr("d",  diagonal());

/* g is a container element 
http://www.w3.org/TR/SVG/shapes.html#InterfaceSVGRectElement
*/
    var node = svg.selectAll("g.node").data(nodes).enter().append("g");
    node.attr("class", "node")
     .attr("transform", function(d) { return "translate("+ d.y +","+ d.x +")"+ "rotate("+ -90 +")"; })
     .on("mouseover", mouseover)
     .on("mouseout", mouseout);
    node.append("rect")
     .attr('class', "recBox")
     .attr("x", -boxWidth/2).attr("y", -boxHeight/2)
     .attr("width", boxWidth).attr("height", boxHeight)
     .attr("rx", 50);
    node.append("text")
     .attr("id", "nodetitle")
     .attr("class", "nodeTitle")
     .attr("y", -boxHeight/2 + fontSize + 2*lineSpace)
     .attr("text-anchor", "middle")
     .text( 'd.name' );
    node.append("text")
      .attr("id", "nodetext")
      .attr("class", "nodeText")
      .attr("y", -boxHeight/2 + 2*fontSize + 4*lineSpace)
      .attr("text-anchor", "middle").text('Score: 00')
     .call(drag);
     //.style("fill", "none").style("stroke", "purple").style("stroke-width", "2.5px");

    // mouseover event handler
    function mouseover() {
      d3.select(this).select("rect").transition().duration(750)
        .attr("width", boxWidth*1.5).attr("height", boxHeight*1.5)
        .style("opacity", 1)
        .style("fill", "#c8e4f8").style("stroke", "orange").style("stroke-width", "5px");
    d3.select(this).select("text#nodetitle").transition().duration(750)
    .attr("y", -boxHeight/2 + fontSize + 2*lineSpace)
        .style("font-weight", "bold")
        .style("font-size", "18px");
//        displayInfoBox(thisNode)
    }
    
    // mouseout event handler
    function mouseout() {
      d3.select(this).select("rect").transition(0.5).duration(750)
        .attr("width", boxWidth).attr("height", boxHeight)
        .style("opacity", 1)
        .style("fill", "white").style("stroke", "purple").style("stroke-width", "2.5px");
        d3.select(this).select("text#nodetitle").transition().duration(750)
        .attr("y", -boxHeight/2 + fontSize + 2*lineSpace)
        .style("font-weight", "normal")
        .style("font-size", "12px");
    }   

/*
    // Display up the info box (for mouse overs)
	function displayInfoBox(node) {
		var nodeName = node.attr("id")
        var infoX = infoBoxWidth/2*0.6
        var infoY = infoBoxHeight/2*1.05
		var infoBox = svg.append("g")
		infoBox
            .attr("class", "popup")
            .attr("transform", function(d) {return "translate(" + infoX + "," + infoY + ")";})

		infoBox
            .append("text")
            .attr("y", -infoBoxHeight/2 + fontSize + 2*lineSpace)
            .attr("text-anchor", "middle")
            .text(nodeName)
            .attr("font-size", fontSize + 8 + "px")

        var imgOffsetX = -infoBoxWidth/2 * 0.95
        var imgOffsetY = -infoBoxHeight/2 + fontSize+8 + 2*lineSpace
		infoBox
            .append("svg:image")
        	.attr("xlink:href", "sample_patches/"+nodeName+".png")
            .attr("width", infoBoxWidth*0.99)
            .attr("height", infoBoxHeight*0.99)
            .attr("transform", function(d) {return "translate(" + imgOffsetX + "," + imgOffsetY + ")";})
	}
*/
});

d3.select(self.frameElement).style("height", height + "px");

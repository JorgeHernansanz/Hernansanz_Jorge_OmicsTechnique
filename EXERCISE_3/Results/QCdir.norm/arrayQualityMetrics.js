// (C) Wolfgang Huber 2010-2011

// Script parameters - these are set up by R in the function 'writeReport' when copying the 
//   template for this script from arrayQualityMetrics/inst/scripts into the report.

var highlightInitial = [ false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false ];
var arrayMetadata    = [ [ "1", "GSM1970196", "EPOC", "UC_patient", "purple", "E_UC " ], [ "2", "GSM1970197", "d-EPOC", "UC_patient", "red", "dE_UC " ], [ "3", "GSM1970198", "EPOC", "UC_patient", "purple", "E_UC " ], [ "4", "GSM1970199", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "5", "GSM1970200", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "6", "GSM1970201", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "7", "GSM1970202", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "8", "GSM1970203", "EPOC", "UC_patient", "purple", "E_UC " ], [ "9", "GSM1970204", "d-EPOC", "UC_patient", "red", "dE_UC " ], [ "10", "GSM1970205", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "11", "GSM1970206", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "12", "GSM1970207", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "13", "GSM1970208", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "14", "GSM1970209", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "15", "GSM1970210", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "16", "GSM1970211", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "17", "GSM1970212", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "18", "GSM1970213", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "19", "GSM1970214", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "20", "GSM1970215", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "21", "GSM1970216", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "22", "GSM1970217", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "23", "GSM1970218", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "24", "GSM1970219", "EPOC", "UC_patient", "purple", "E_UC " ], [ "25", "GSM1970220", "d-EPOC", "UC_patient", "red", "dE_UC " ], [ "26", "GSM1970221", "EPOC", "UC_patient", "purple", "E_UC " ], [ "27", "GSM1970222", "d-EPOC", "UC_patient", "red", "dE_UC " ], [ "28", "GSM1970223", "EPOC", "UC_patient", "purple", "E_UC " ], [ "29", "GSM1970224", "d-EPOC", "UC_patient", "red", "dE_UC " ], [ "30", "GSM1970225", "EPOC", "non-IBD_control", "blue", "E_NC " ], [ "31", "GSM1970226", "d-EPOC", "non-IBD_control", "green", "dE_NC " ], [ "32", "GSM1970227", "EPOC", "UC_patient", "purple", "E_UC " ], [ "33", "GSM1970228", "d-EPOC", "UC_patient", "red", "dE_UC " ], [ "34", "GSM1970229", "EPOC", "UC_patient", "purple", "E_UC " ], [ "35", "GSM1970230", "d-EPOC", "UC_patient", "red", "dE_UC " ], [ "36", "GSM1970231", "EPOC", "non-IBD_control", "blue", "E_NC " ] ];
var svgObjectNames   = [ "pca", "dens" ];

var cssText = ["stroke-width:1; stroke-opacity:0.4",
               "stroke-width:3; stroke-opacity:1" ];

// Global variables - these are set up below by 'reportinit'
var tables;             // array of all the associated ('tooltips') tables on the page
var checkboxes;         // the checkboxes
var ssrules;


function reportinit() 
{
 
    var a, i, status;

    /*--------find checkboxes and set them to start values------*/
    checkboxes = document.getElementsByName("ReportObjectCheckBoxes");
    if(checkboxes.length != highlightInitial.length)
	throw new Error("checkboxes.length=" + checkboxes.length + "  !=  "
                        + " highlightInitial.length="+ highlightInitial.length);
    
    /*--------find associated tables and cache their locations------*/
    tables = new Array(svgObjectNames.length);
    for(i=0; i<tables.length; i++) 
    {
        tables[i] = safeGetElementById("Tab:"+svgObjectNames[i]);
    }

    /*------- style sheet rules ---------*/
    var ss = document.styleSheets[0];
    ssrules = ss.cssRules ? ss.cssRules : ss.rules; 

    /*------- checkboxes[a] is (expected to be) of class HTMLInputElement ---*/
    for(a=0; a<checkboxes.length; a++)
    {
	checkboxes[a].checked = highlightInitial[a];
        status = checkboxes[a].checked; 
        setReportObj(a+1, status, false);
    }

}


function safeGetElementById(id)
{
    res = document.getElementById(id);
    if(res == null)
        throw new Error("Id '"+ id + "' not found.");
    return(res)
}

/*------------------------------------------------------------
   Highlighting of Report Objects 
 ---------------------------------------------------------------*/
function setReportObj(reportObjId, status, doTable)
{
    var i, j, plotObjIds, selector;

    if(doTable) {
	for(i=0; i<svgObjectNames.length; i++) {
	    showTipTable(i, reportObjId);
	} 
    }

    /* This works in Chrome 10, ssrules will be null; we use getElementsByClassName and loop over them */
    if(ssrules == null) {
	elements = document.getElementsByClassName("aqm" + reportObjId); 
	for(i=0; i<elements.length; i++) {
	    elements[i].style.cssText = cssText[0+status];
	}
    } else {
    /* This works in Firefox 4 */
    for(i=0; i<ssrules.length; i++) {
        if (ssrules[i].selectorText == (".aqm" + reportObjId)) {
		ssrules[i].style.cssText = cssText[0+status];
		break;
	    }
	}
    }

}

/*------------------------------------------------------------
   Display of the Metadata Table
  ------------------------------------------------------------*/
function showTipTable(tableIndex, reportObjId)
{
    var rows = tables[tableIndex].rows;
    var a = reportObjId - 1;

    if(rows.length != arrayMetadata[a].length)
	throw new Error("rows.length=" + rows.length+"  !=  arrayMetadata[array].length=" + arrayMetadata[a].length);

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = arrayMetadata[a][i];
}

function hideTipTable(tableIndex)
{
    var rows = tables[tableIndex].rows;

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = "";
}


/*------------------------------------------------------------
  From module 'name' (e.g. 'density'), find numeric index in the 
  'svgObjectNames' array.
  ------------------------------------------------------------*/
function getIndexFromName(name) 
{
    var i;
    for(i=0; i<svgObjectNames.length; i++)
        if(svgObjectNames[i] == name)
	    return i;

    throw new Error("Did not find '" + name + "'.");
}


/*------------------------------------------------------------
  SVG plot object callbacks
  ------------------------------------------------------------*/
function plotObjRespond(what, reportObjId, name)
{

    var a, i, status;

    switch(what) {
    case "show":
	i = getIndexFromName(name);
	showTipTable(i, reportObjId);
	break;
    case "hide":
	i = getIndexFromName(name);
	hideTipTable(i);
	break;
    case "click":
        a = reportObjId - 1;
	status = !checkboxes[a].checked;
	checkboxes[a].checked = status;
	setReportObj(reportObjId, status, true);
	break;
    default:
	throw new Error("Invalid 'what': "+what)
    }
}

/*------------------------------------------------------------
  checkboxes 'onchange' event
------------------------------------------------------------*/
function checkboxEvent(reportObjId)
{
    var a = reportObjId - 1;
    var status = checkboxes[a].checked;
    setReportObj(reportObjId, status, true);
}


/*------------------------------------------------------------
  toggle visibility
------------------------------------------------------------*/
function toggle(id){
  var head = safeGetElementById(id + "-h");
  var body = safeGetElementById(id + "-b");
  var hdtxt = head.innerHTML;
  var dsp;
  switch(body.style.display){
    case 'none':
      dsp = 'block';
      hdtxt = '-' + hdtxt.substr(1);
      break;
    case 'block':
      dsp = 'none';
      hdtxt = '+' + hdtxt.substr(1);
      break;
  }  
  body.style.display = dsp;
  head.innerHTML = hdtxt;
}

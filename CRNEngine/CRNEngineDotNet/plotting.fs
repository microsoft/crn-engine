[<JavaScript>]
module Microsoft.Research.CRNEngine.Plotting

open Microsoft.Research.CRNEngine
open FSharpIDD
open FSharpIDD.Plots
open System;

type html_content = 
  { model : string
  ; comparisons : (string*string) list
  ; posterior : string
  ; correlation : string
  ; traces : string
  ; psummary : string
  ; variations : (string*string) list }

// Bits and pieces of HTML, JS and CSS follow. These are used to compose the final output. Note that the JS contained here should ONLY use InteractiveDataDisplay as a library (e.g. do not use $). There is no guarantee that any library except for InteractiveDataDisplay will be in-scope in an embedded scenario.

/// This script immediately activates IDD for all fsharp-idd plots within any cliplot element.
let activate_idd_script = """
var objects = document.getElementsByClassName("cliplot div.fsharp-idd");
for(var i = 0; i < objects.length; i++)
  InteractiveDataDisplay.asPlot(objects.item(i));
objects = document.getElementsByClassName("cliplot div.idd-subplots");
for(var i = 0; i < objects.length; i++)
  InteractiveDataDisplay.asSubPlots(objects.item(i));
"""

/// This script delays IDD activation until after page load.
let prepare_idd_script = sprintf """
<script type="text/javascript">
    window.addEventListener("load", function(){%s});
</script>"""              activate_idd_script

/// These headers load IDD and prerequisites from CDNs.
let cdn_scripts = """
<link rel="stylesheet" type="text/css" href="https://cdn.jsdelivr.net/npm/interactive-data-display@1.5.30/dist/idd.css" integrity="sha384-c0Bubh+0uCjFanb6BoVtGLQAYmnD27PrMKjIlFp3QZ/ZjpWHo+q86s7zmX1bKAmU" crossorigin="anonymous"/>
<script src="https://cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.3/modernizr.min.js" integrity="sha384-bPV3mA2eo3edoq56VzcPBmG1N1QVUfjYMxVIJPPzyFJyFZ8GFfN7Npt06Zr23qts" crossorigin="anonymous"></script>
<script src="https://code.jquery.com/jquery-2.1.4.min.js" integrity="sha384-R4/ztc4ZlRqWjqIuvf6RX5yb/v90qNGx6fS48N0tRxiGkqveZETq72KgDVJCp2TC" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/rxjs/3.1.2/rx.lite.min.js" integrity="sha384-YYQgxpPVmcgxIB80jXrurb3n9UJCb8MOA+2Om6wfF3Fji0Khse2mz3Pz87kt2wba" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery-mousewheel/3.1.13/jquery.mousewheel.min.js" integrity="sha384-jx+EoRqSnwNqHsZoMJl/OMDriDdzqdPXunQMsJOq6eUISEJxQkBgcR88EFkA2iHw" crossorigin="anonymous"></script>
<script src="https://cdn.jsdelivr.net/npm/interactive-data-display@1.5.30/dist/idd.js" integrity="sha384-Ys962FVdDdKVR+cU47Lb85HIpxUsV8oK/s7lb43z8kD7481L26ecJ4Q3yqUdgRBJ" crossorigin="anonymous"></script>
"""

/// This script implements tabbing. Also implements the workaround to the layout issues that IDD has in tab scenarios.
let footer_scripts_idd = """
<script>
function openTab(evt, group, tabName) {
    var i, tabcontent, tablinks;
    tabcontent = document.getElementsByClassName("tabcontent cliplot "+group);
    for (i = 0; i < tabcontent.length; i++) {
        tabcontent[i].style.display = "none";
    }
    tablinks = document.getElementsByClassName("tablinks cliplot "+group);
    for (i = 0; i < tablinks.length; i++) {
        tablinks[i].className = tablinks[i].className.replace(" active", "");
    }
    document.getElementById(group+tabName).style.display = "block";
    evt.currentTarget.className += " active";
    var objects = document.querySelectorAll("#"+group+tabName+" div.fsharp-idd");
    for(var i = 0; i < objects.length; i++)
        InteractiveDataDisplay.asPlot(objects.item(i)).requestUpdateLayout();
    objects = document.querySelectorAll("#"+group+tabName+" div.idd-subplots");
    for(var i = 0; i < objects.length; i++)
        InteractiveDataDisplay.asSubPlots(objects.item(i));
}
function openComparison(evt, group, compName) {
    var i, compcontent, complinks;
    compcontent = document.getElementsByClassName("compcontent cliplot "+group);
    for (i = 0; i < compcontent.length; i++) {
        compcontent[i].style.display = "none";
    }
    complinks = document.getElementsByClassName("complinks cliplot "+group);
    for (i = 0; i < complinks.length; i++) {
        complinks[i].className = complinks[i].className.replace(" active", "");
    }
    document.getElementById(group+compName).style.display = "block";
    evt.currentTarget.className += "active";
    var objects = document.querySelectorAll("#"+group+compName+" div.fsharp-idd");
    for(var i = 0; i < objects.length; i++)
        InteractiveDataDisplay.asPlot(objects.item(i)).requestUpdateLayout();
    objects = document.querySelectorAll("#"+group+compName+" div.idd-subplots");
    for(var i = 0; i < objects.length; i++)
        InteractiveDataDisplay.asSubPlots(objects.item(i));
}
window.addEventListener("load", function() { document.getElementById("cliplot_defaultOpen").click(); });
</script>"""

/// This string gathers necessary scripts in a way that's suitable for single-file output.
let page_header_scripts = sprintf "%s %s %s" cdn_scripts prepare_idd_script footer_scripts_idd

/// This string gathers necessary scripts in a way that's suitable for embedded output.
let unified_scripts = sprintf "<script>%s</script> %s" activate_idd_script footer_scripts_idd

/// This includes the styles for all elements. All styles are limited to the cliplot class, to prevent leaking.
let local_styles = """<style>
    .cliplot {font-family: "Lato", sans-serif;}
    
    ul.tab.cliplot {
		list-style-type: none;
		margin: 0;
		padding: 0;
		overflow: hidden;
		border: 2px solid #ccc;
		background-color: #f1f1f1;
	}

	/* Float the list items side by side */
	ul.tab.cliplot li {float: left;}

	/* Style the links inside the list items */
	ul.tab.cliplot li a {
		display: inline-block;
		color: black;
		text-align: center;
		padding: 14px 16px;
		text-decoration: none;
		transition: 0.3s;
		font-size: 17px;
	}

	/* Change background color of links on hover */
	ul.tab.cliplot li a:hover {
		background-color: #ddd;
	}

	/* Create an active/current tablink class */
	ul.tab.cliplot li a:focus, .active.cliplot {
		background-color: #ccc;
	}

	/* Style the tab content */
	.tabcontent.cliplot {
		display: none;
		padding: 12px 12px;
		border: 2px solid #ccc;
		border-top: none;
	}

	/* Style the table cells*/
	th.cliplot, td.cliplot { 
		padding: 3px;
		min-width: 50px;
	}	
	th.cliplot {
		border-top: 2px solid #000;
		border-bottom: 1px solid #000;
	}
	table.cliplot#pTable {
		border-collapse: collapse;
		border-bottom: 2px solid #000;
	}</style>"""


let make_page title content =
    sprintf "<!DOCTYPE html><html>%s %s<body><h2 class=\"cliplot\">%s</h2>%s</body></html>" page_header_scripts local_styles title content

let make_embedded content =
    sprintf "<div class=\"cliplot\">%s %s %s</div>" local_styles content unified_scripts

/// Replaces all non-alphanumeric characters (incl. whitespace) with '_' and prepends 'cliplot_'.
let to_identifier n = sprintf "cliplot_%s" (String.map (fun c -> if System.Char.IsLetterOrDigit (c) then c else '_') n)

let arbitrary_tabbed_content group (tabs:(string*string) list)  = 
    let tab_definitions = 
        "<ul class=\"tab cliplot\">" 
        + ( tabs 
          |> List.mapi (fun i (header,_) -> 
              sprintf "\t<li class=\"cliplot\"><a href=\"javascript:void(0)\" class=\"tablinks cliplot %s\" onclick=\"openTab(event, '%s', '%s')\"%s>%s</a></li>" group group (to_identifier header) (if i=0 then " id=\"cliplot_defaultOpen\"" else "") header) 
          |> String.concat "\n" )
        + "\n</ul>" 
    let tab_items = 
        tabs
        |> List.map (fun (header,content) -> sprintf "<div id=\"%s%s\" class=\"tabcontent cliplot %s\">\n\t%s\n</div>" group (to_identifier header) group content) 
        |> String.concat "\n"
    sprintf "%s %s" tab_definitions tab_items
    
let arbitrary_tabbed title (tabs:(string*string) list) =
    let content = arbitrary_tabbed_content "top_tabs" tabs
    make_page title content

let to_tab_items group (content:html_content) : (string*string) list =
    let comp_lis = 
        "<ul class=\"tab tab2 cliplot\">\n\t" 
        + (List.init content.comparisons.Length (fun i -> 
            sprintf "\t\t<li><a href=\"javascript:void(0)\" class=\"complinks cliplot %s\" onclick=\"openComparison(event, '%s', 'cliplot_comparison_%d')\">Comparison %d</a></li>" group group (i+1) (i+1)
        ) |> String.concat "\n")
        + "\n\t</ul>"
    let comp_item = sprintf "\t<div id=\"%scliplot_comparison_%d\" class=\"compcontent cliplot %s\">\n\t\t%s\n</div>" group
    let comp_items = 
        content.comparisons 
        |> List.mapi (fun i (name,str) -> comp_item (i+1) group str)
        |> String.concat "\n"
    let tab_items = 
        content.variations
        |> List.append 
          [ "Model", content.model
          ; "Comparisons", (sprintf "%s\n%s" comp_lis comp_items)
          ; "Posterior marginals", content.posterior
          ; "Correlation", content.correlation
          ; "MCMC traces", content.traces
          ; "Parameter summary", content.psummary
          ]
    tab_items

let structured_tabbed title group (content:html_content) = 
    let tab_items = to_tab_items group content
    arbitrary_tabbed title tab_items

let structured_tabbed_embedded group (content:html_content) = 
    let tab_items = to_tab_items group content
    let content = arbitrary_tabbed_content group tab_items
    make_embedded content

let plot_comparison (tsim:float list) (sim:Column<float>) (tdata:float list) (dat:Column<float>) (simcolor:Colour.Colour) (datcolor:Colour.Colour) tmax =
    let (tdata, ydata), (tsim, ysim) = 
        match tmax with 
        | Some tm -> 
            ( List.zip tdata dat.values |> List.filter (fun (t,x) -> t < tm) |> List.unzip
            , List.zip tsim sim.values |> List.filter (fun (t,x) -> t < tm) |> List.unzip
            )
        | None -> (tdata, dat.values), (tsim, sim.values)
    // https://github.com/predictionmachines/FSharpIDD/issues/16
    let tdata = List.toArray tdata
    let ydata = List.toArray ydata
    let tsim = List.toArray tsim
    let ysim = List.toArray ysim
    ( Markers.createMarkers tdata ydata |> Markers.setFillColour datcolor |> Markers.setSize 3.0
    , Polyline.createPolyline tsim ysim |> Polyline.setStrokeColour simcolor |> Polyline.setThickness 2.0
    )

#if JavaScript
#else
// Regex for extracting keys from strings
let (|SecondRegexGroup|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input,pattern) 
    if (m.Success) then Some m.Groups.[2].Value else None

let extract_instance_key str = match str with SecondRegexGroup "(.*) [(](.*)[)]" key -> key | _ -> "Unknown"
#endif
let condition_map (c:string) = 
    c.Split(';') 
    |> List.ofArray
    |> List.fold (fun a ci -> match ci.Split(' ') with [| name; "="; value|] -> (name,float value) :: a | _ -> a) []
    |> Map.ofList

let darken_colour (f:float) col = 
    let darken_byte x = (float x)*f |> int |> byte
    match col with
    | Colour.Rgb {R=r; G=g; B=b} -> Colour.Rgb {R=darken_byte r; G=darken_byte g; B=darken_byte b}
    | Colour.Default  -> col

let colour_of_string darken_factor (s:String) =
    match Colour.parse s with 
    | Some c -> c, darken_colour darken_factor c
    | None -> Colour.Gray, Colour.Black

let plot_comparisons (sims:float Result list) (data:float Table list) tmax xlabel ylabel =    
    (sims, data)
    ||> List.map2 (fun s d ->
        let colours = 
            s.table.columns 
            |> List.mapi (fun i _ -> 
                if s.instance.settings.plotcolours.Length > i 
                then colour_of_string 0.6 s.instance.settings.plotcolours.[i] 
                else Colour.Grey, Colour.Black
            )
        let title = d.columns.Head.name.Split('(') |> Array.head
        //let title = s.instance.name.Trim('[',']').Substring(0,40)
        List.zip3 s.table.columns d.columns colours
        |> List.fold (fun chart (scol,dcol,(simcolour,datcolour)) ->
            let (markers,polyline) = plot_comparison s.table.times scol d.times dcol simcolour datcolour tmax
            chart |> Chart.addPolyline polyline |> Chart.addMarkers markers
        ) Chart.Empty
        |> Chart.setXlabel xlabel
        |> Chart.setYlabel ylabel 
        |> Chart.setTitle title
        |> Chart.setVisibleRegion (Chart.VisibleRegion.Autofit 0)
    )

let plot_marginal samples title logscale = 
    let x = (if logscale then Array.map log10 samples else samples)
    let nbinsx = 30
    let xLabel = if logscale then "Log10(value)" else "value"
    let histogram = Histogram.createHistogram x |> Histogram.setBinCount nbinsx
    Chart.Empty 
    |> Chart.addHistogram histogram 
    |> Chart.setXlabel xLabel 
    |> Chart.setYlabel "Frequency" 
    |> Chart.setTitle title
    |> Chart.setVisibleRegion (Chart.VisibleRegion.Autofit 0) 


let plot_trace iterations samples title =
    // https://github.com/predictionmachines/FSharpIDD/issues/16
    let iterations = Seq.toArray iterations
    let samples = Seq.toArray samples
    let plot = Polyline.createPolyline iterations samples |> Polyline.setThickness 2.0
    Chart.Empty 
    |> Chart.addPolyline plot 
    |> Chart.setXlabel "Sample" 
    |> Chart.setYlabel "Parameter values" 
    |> Chart.setTitle title 
    |> Chart.setVisibleRegion (Chart.VisibleRegion.Autofit 1)

let fst3 (x,_,_) = x
#if JavaScript
let environment_to_string e = e |> Map.toList |> List.map (fun (k,v) -> k + "=" + sprintf "%1.1f" v) |> String.concat "; " |> sprintf "[%s]"
#else
let environment_to_string e = e |> Map.toList |> List.map (fun (k,v) -> k + "=" + sprintf "%1.1g" v) |> String.concat "; " |> sprintf "[%s]"
#endif

/// Create box-plots that show the marginal posteriors for Multiple parameters 
let plot_variation (posterior:Map<string,float[]>) (keys:(string*string list) list) (ps:string list) = 
    let num_conds = keys |> List.map (fun (_,items) -> items.Length) |> List.max
    let width = max (120+25*num_conds) 400
    
    keys 
    |> List.fold (fun (index,charts) (name,items) -> 
        let n = items.Length
        let xticks, means = 
            ps.[index..index+n-1]
            |> List.map2 (fun item pi -> item, Array.average posterior.[pi]) items
            |> List.sortBy fst
            |> List.unzip
        let xs = Array.init means.Length (fun i -> float (i+1))
        let chart = 
            Chart.Empty
            |> Chart.addMarkers (Markers.createMarkers xs means |> Markers.setShape Markers.Shape.Circle)
            |> Chart.setXaxis (Chart.createTiltedLabelledAxis xs xticks 90.0 |> Chart.setLabelsVisibility true)
            |> Chart.setTitle name
            |> Chart.setYlabel "Parameter value" 
            |> Chart.setVisibleRegion (Chart.VisibleRegion.Autofit 0)
        index + n, chart :: charts
    ) (0,[])
    |> snd, width

let plot_correlation (C:float[][]) (paras:string list) =
    let np = List.length paras
    // Dimensions for plotting
    let left = 50
    let bottom = 50
    let right = 30
    let top = 30
    let scalebar = 60
    let width = np*25+left+right+scalebar
    let height = np*25+top+bottom

    // Remove upper triangle and leading diagonal.
    let z = Array.mapi (fun i col -> Array.mapi (fun j e -> if i <= j then nan else e) col) C
    // Transpose and convert to Array2D.
    let z = Array2D.init np np (fun r c -> z.[np-c-1].[r])
    let colorScale =
        [
            (-1.0, "#0000AA")
            (-0.8, "#2222AA")
            (-0.6, "#4444AA")
            (-0.4, "#6666AA")
            (-0.2, "#8888AA")
            ( 0.0, "#AAAAAA")
            ( 0.2, "#AA8888")
            ( 0.4, "#AA6666")
            ( 0.6, "#AA4444")
            ( 0.8, "#AA2222")
            ( 1.0, "#AA0000")
        ]
    let colorIddString = System.String.Join(",", List.mapi (fun i (a,b) -> if i = 0 then sprintf "%f=%s" a b else if i = (List.length colorScale) - 1 then sprintf "%s=%f" b a else sprintf "%s=%f=%s" b a b) colorScale)
    //let colorIddString = "-1.0=#0000AA,#AAAAAA=0.0=#AAAAAA,#AA0000=1.0"
    let palette = Heatmap.Palette.IddPaletteString colorIddString
    let xVals = Array.init<float> (np+1) (fun i -> float i)
    let corrMap = Heatmap.createHeatmap xVals xVals z |> Heatmap.setPalette palette |> Heatmap.setName null
    let ticks = Array.init (np+1) (fun i -> float i)
    let xAxis = Chart.createTiltedLabelledAxis ticks paras 90.0
    let yAxis = Chart.createLabelledAxis ticks (List.rev paras)
    { Chart.Empty with
       Width = width
       Height = height
       Title = "Correlation in the joint posterior samples"
       VisibleRegion = Chart.VisibleRegion.Autofit 0
       IsLegendEnabled = Chart.Visible
       IsTooltipPlotCoordsEnabled = false
       Xaxis = xAxis
       Yaxis = yAxis
    } |> Chart.addHeatmap corrMap
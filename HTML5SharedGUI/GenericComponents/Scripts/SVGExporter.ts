declare var SVG: any;
import "svg";
import "idd";
declare var InteractiveDataDisplay: any;
import { saveAs } from "file-saver";

function defaultGenerator(container: HTMLElement) {
    var plot = InteractiveDataDisplay.asPlot($("div.idd-plot-master", container)[0]);
    var svg = plot.exportToSvg();

    var finalSvg: any;

    var $legend = $(".idd-legend-cover", container);
    var lsvg = null;
    if ($legend.length > 0) {
        // (FP) this throws inside idd on the CTMC/Summary export.
        try { lsvg = plot.exportLegendToSvg($legend[0]); }
        catch (e) { }
    }
    if (lsvg != null) {
        var svgHost = document.createElementNS('http://www.w3.org/2000/svg', 'svg');

        var exportSvg = SVG(svgHost).size(svg.width() + lsvg.width(), svg.height());
        exportSvg.nested().add(svg);
        exportSvg.nested().add(lsvg).x(svg.width());

        finalSvg = exportSvg.svg();
    } else {
        finalSvg = svg.svg();
    }
    return finalSvg;
}

function GetExporter(name: string, container: HTMLElement, generator?: (container: HTMLElement) => string) {
    if (generator == null)
        generator = defaultGenerator;
    return function () {
        var blobSupport = !!new Blob;
        if (blobSupport) {
            var content = generator(container);
            var blob = new Blob([content], { type: "image/svg+xml" });
            saveAs(blob, name + ".svg");
        }
        else
            console.error("Cannot generate an SVG export for " + name + ": blobs not supported.");
    }
}

export default GetExporter
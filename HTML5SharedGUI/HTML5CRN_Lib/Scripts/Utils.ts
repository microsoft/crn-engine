export function ExponentToConcentrationString(exponent: number): string {
    var prefix = "";
    switch (exponent) {
        case 0: prefix = ""; break;
        case -1: prefix = "d"; break;
        case -2: prefix = "c"; break;
        case -3: prefix = "m"; break;
        case -6: prefix = "u"; break;
        case -9: prefix = "n"; break;
        case -12: prefix = "p"; break;
        case -15: prefix = "f"; break;
        case -18: prefix = "a"; break;
        case -21: prefix = "z"; break;
        case -24: prefix = "y"; break;
        default: prefix = "10^" + exponent + " "; break;
    }
    return prefix + "M";
}

/** Parses a float, which may be in the "(0 - x)" form that comes out of expression parsing. No other forms are allowed. This is only a problem in the context of boundary lines declared in plot_settings. This is a kludge. Not sure what the long-term solution for this should be: implement an expression parser here, or evaluate boundary lines server-side? */
export function parseFloatExp(s: string) {
    if (s.length > 0 && s[0] == '(')
        s = s.substr(1);
    if (s.length > 0 && s[s.length - 1] == ')')
        s = s.substr(0, s.length - 1);
    if (s.length > 0 && s[0] == '0')
        s = s.substr(1);
    s = s.replace(" ", "").replace(" ","");
    return parseFloat(s);
}

function doRemoveLoadingOverlay() {
    var loadScreen = document.getElementById('loading-screen');
    loadScreen.style.display = "none";
    var appCover = document.getElementById('app-cover');
    appCover.style.display = "none";
    var app = document.getElementById('app');
    app.style.filter = "none";
    app.style.webkitFilter = "none";
}

export function RemoveLoadingOverlay() {
    setTimeout(doRemoveLoadingOverlay);
}